namespace SvgPath

open System
open System.Globalization

type PathNewlines =
    | OneLine
    | AtSubpaths
    | AtSegments

[<Struct>]
type PathSerializeOptions =
    { LeftDecimals: LeftDecimalOptions
      RightDecimals: RightDecimalOptions
      Relative: bool
      MinimizeWhitespace: bool
      Commas: bool
      RepeatCommands: bool
      ExplicitInitialLineto: bool
      UseHorizontalVertical: bool
      UseSmoothCurves: bool
      Newlines: PathNewlines }

type private PreviousCurve =
    | NoPreviousCurve
    | PreviousCubic of Point<length>
    | PreviousQuadratic of Point<length>

[<Struct>]
type private SerializationFormat =
    { Options: PathSerializeOptions
      NumberFormat: NumberFormat }

[<Struct>]
type private RelativeParserState =
    { Current: Point<length>
      SubpathStart: Point<length>
      PreviousCurve: PreviousCurve }

type private ChordSimilarity =
    | StableChord of sourceStart: Point<length> * parserStart: Point<length> * scaleCos: float * scaleSin: float
    | UnstableChord

[<RequireQualifiedAccess>]
module Serialize =
    let defaultOptions =
        { LeftDecimals = Succinct
          RightDecimals = AtMost 5
          Relative = false
          MinimizeWhitespace = false
          Commas = false
          RepeatCommands = true
          ExplicitInitialLineto = true
          UseHorizontalVertical = true
          UseSmoothCurves = true
          Newlines = OneLine }

    let decimalOptions decimalPlaces = { defaultOptions with RightDecimals = AtMost decimalPlaces }
    let fixedDecimalOptions decimalPlaces = { defaultOptions with RightDecimals = RightDecimalOptions.Fixed decimalPlaces }
    let relativeOptions = { defaultOptions with Relative = true }
    let relativeDecimalOptions decimalPlaces = { relativeOptions with RightDecimals = AtMost decimalPlaces }
    let relativeFixedDecimalOptions decimalPlaces = { relativeOptions with RightDecimals = RightDecimalOptions.Fixed decimalPlaces }
    let minimizeWhitespace (options: PathSerializeOptions) = { options with MinimizeWhitespace = true }
    let withCommas commas (options: PathSerializeOptions) = { options with Commas = commas }
    let repeatCommands repeat (options: PathSerializeOptions) = { options with RepeatCommands = repeat }
    let explicitInitialLineto explicit (options: PathSerializeOptions) = { options with ExplicitInitialLineto = explicit }
    let useHorizontalVertical useIt (options: PathSerializeOptions) = { options with UseHorizontalVertical = useIt }
    let useSmoothCurves useIt (options: PathSerializeOptions) = { options with UseSmoothCurves = useIt }
    let withNewlines newlines (options: PathSerializeOptions) = { options with Newlines = newlines }
    let withLeftDecimals left (options: PathSerializeOptions) = { options with LeftDecimals = left }
    let withRightDecimals right (options: PathSerializeOptions) = { options with RightDecimals = right }
    let withLeftPadding left options = withLeftDecimals left options

    let minifyingOptions decimalPlaces =
        relativeDecimalOptions decimalPlaces
        |> minimizeWhitespace
        |> repeatCommands false
        |> explicitInitialLineto false

    let private numberOptions (options: PathSerializeOptions) : NumberFormatOptions =
        { LeftDecimals = options.LeftDecimals; RightDecimals = options.RightDecimals }

    let private serializationFormat options numbers =
        { Options = options
          NumberFormat = NumberFormat.prepare (numberOptions options) numbers }

    let private minimizeLeadingZero (value: string) =
        if value.StartsWith("0.", StringComparison.Ordinal) then value.Substring 1
        elif value.StartsWith("-0.", StringComparison.Ordinal) then "-" + value.Substring 2
        else value

    let private number (value: float<'Unit>) (format: SerializationFormat) =
        let formatted = NumberFormat.number (float value) format.NumberFormat
        if format.Options.MinimizeWhitespace then minimizeLeadingZero formatted else formatted

    let private rawNumber (value: float<'Unit>) (options: PathSerializeOptions) = NumberFormat.rawNumber (float value) (numberOptions options)
    let private quantizedNumber (value: float<length>) (format: SerializationFormat) =
        let raw = NumberFormat.codeNumber (float value) format.NumberFormat |> fun value -> value.Trim()
        match Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, parsed -> parsed * 1.0<length>
        | _ -> value

    let private quantizedPoint point format = Point.create (quantizedNumber point.X format) (quantizedNumber point.Y format)
    let private delta origin point = Point.displacement origin point
    let private add left right = Point.add left right
    let private origin = Point.create 0.0<length> 0.0<length>

    let private minimizedSeparator (left: string) (right: string) =
        if right.StartsWith("-", StringComparison.Ordinal) || right.StartsWith("+", StringComparison.Ordinal) then ""
        elif right.StartsWith(".", StringComparison.Ordinal) && left.Contains(".", StringComparison.Ordinal) then ""
        else " "

    let private groupSeparator left right options =
        if options.MinimizeWhitespace then minimizedSeparator left right else " "

    let private pointValue point format =
        let x = number point.X format
        let y = number point.Y format
        let separator =
            if format.Options.MinimizeWhitespace then minimizedSeparator x y
            elif format.Options.Commas then ","
            else " "
        x + separator + y

    let private joinGroups groups format =
        match groups with
        | [] -> ""
        | first :: rest ->
            rest
            |> List.fold (fun (previous, joined) next ->
                next, joined + groupSeparator previous next format.Options + next) (first, first)
            |> snd

    let private command name arguments format =
        name + (if format.Options.MinimizeWhitespace then "" else " ") + arguments

    let private flag value = if value then "1" else "0"

    let private arcArguments radius rotation largeArc sweep endpoint format =
        let before = joinGroups [ pointValue radius format; number rotation format ] format
        let flagSeparator = if format.Options.MinimizeWhitespace then "" else " "
        before + groupSeparator before (flag largeArc) format.Options
        + flag largeArc + flagSeparator + flag sweep + flagSeparator + pointValue endpoint format

    let private reflect point center = Point.create (2.0 * center.X - point.X) (2.0 * center.Y - point.Y)
    let private reflectedQuadratic start = function PreviousQuadratic control -> reflect control start | _ -> start
    let private reflectedCubic start = function PreviousCubic control -> reflect control start | _ -> start
    let private formattedPointsEqual left right format = pointValue left format = pointValue right format

    let private absoluteLine startPoint endPoint format =
        let startX, startY = number startPoint.X format, number startPoint.Y format
        let endX, endY = number endPoint.X format, number endPoint.Y format
        if format.Options.UseHorizontalVertical && startY = endY then command "H" endX format
        elif format.Options.UseHorizontalVertical && startX = endX then command "V" endY format
        else command "L" (pointValue endPoint format) format

    let private relativeLine startPoint endPoint format =
        let difference = delta startPoint endPoint
        let dx, dy, zero = number difference.X format, number difference.Y format, number 0.0<length> format
        if format.Options.UseHorizontalVertical && dy = zero then command "h" dx format
        elif format.Options.UseHorizontalVertical && dx = zero then command "v" dy format
        else command "l" (pointValue difference format) format

    let private absoluteSegment segment previous format =
        match segment with
        | Line(startPoint, endPoint) -> absoluteLine startPoint endPoint format, NoPreviousCurve
        | QuadraticBezier(startPoint, control, endPoint) ->
            let parserStart = quantizedPoint startPoint format
            let parserControl = quantizedPoint control format
            let reflected = reflectedQuadratic parserStart previous
            let smooth = parserControl = reflected
            let serialized =
                if format.Options.UseSmoothCurves && smooth then command "T" (pointValue endPoint format) format
                else command "Q" (joinGroups [ pointValue control format; pointValue endPoint format ] format) format
            serialized, PreviousQuadratic(if smooth then reflected else parserControl)
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let reflected = reflectedCubic (quantizedPoint startPoint format) previous
            let smooth = quantizedPoint control1 format = reflected
            let arguments =
                if format.Options.UseSmoothCurves && smooth then [ pointValue control2 format; pointValue endPoint format ]
                else [ pointValue control1 format; pointValue control2 format; pointValue endPoint format ]
            command (if format.Options.UseSmoothCurves && smooth then "S" else "C") (joinGroups arguments format) format,
            PreviousCubic(quantizedPoint control2 format)
        | Arc arc -> command "A" (arcArguments arc.Radius arc.XAxisRotation arc.LargeArc arc.Sweep arc.End format) format, NoPreviousCurve

    let private relativeSegment segment previous format =
        let startPoint = Segment.start segment
        match segment with
        | Line(_, endPoint) -> relativeLine startPoint endPoint format, NoPreviousCurve
        | QuadraticBezier(_, control, endPoint) ->
            let smooth = format.Options.UseSmoothCurves && formattedPointsEqual control (reflectedQuadratic startPoint previous) format
            let args = if smooth then [ pointValue (delta startPoint endPoint) format ] else [ pointValue (delta startPoint control) format; pointValue (delta startPoint endPoint) format ]
            command (if smooth then "t" else "q") (joinGroups args format) format, PreviousQuadratic control
        | CubicBezier(_, control1, control2, endPoint) ->
            let smooth = format.Options.UseSmoothCurves && formattedPointsEqual control1 (reflectedCubic startPoint previous) format
            let args = if smooth then [ pointValue (delta startPoint control2) format; pointValue (delta startPoint endPoint) format ] else [ pointValue (delta startPoint control1) format; pointValue (delta startPoint control2) format; pointValue (delta startPoint endPoint) format ]
            command (if smooth then "s" else "c") (joinGroups args format) format, PreviousCubic control2
        | Arc arc -> command "a" (arcArguments arc.Radius arc.XAxisRotation arc.LargeArc arc.Sweep (delta startPoint arc.End) format) format, NoPreviousCurve

    let private dropClosingLine (subpath: Subpath) =
        let segments = subpath.Segments
        if not subpath.Closed then segments
        else
            match List.rev segments with
            | Line(lineStart, lineEnd) :: rest when lineEnd = subpath.Start && lineStart <> lineEnd -> List.rev rest
            | _ -> segments

    let private commandName (value: string) = if value.Length = 0 then "" else value.Substring(0, 1)
    let private commandArguments (value: string) (options: PathSerializeOptions) =
        let arguments = value.Substring 1
        if options.MinimizeWhitespace then arguments else arguments.TrimStart()
    let private commandNames = set [ "M"; "m"; "L"; "l"; "H"; "h"; "V"; "v"; "Q"; "q"; "C"; "c"; "S"; "s"; "T"; "t"; "A"; "a"; "Z"; "z" ]
    let private repeatable = set [ "L"; "l"; "H"; "h"; "V"; "v"; "Q"; "q"; "C"; "c"; "S"; "s"; "T"; "t"; "A"; "a" ]

    let private compactCommands commands format =
        let rec loop previous remaining =
            match remaining with
            | [] -> []
            | item :: rest ->
                let current = commandName item
                let compacted = if current = previous && Set.contains current repeatable then commandArguments item format.Options else item
                let effective =
                    if (current = "M" || current = "m") && not format.Options.ExplicitInitialLineto then
                        if current = "M" then "L" else "l"
                    else current
                compacted :: loop effective rest
        loop "" commands

    let private commandChunkSeparator (right: string) (options: PathSerializeOptions) =
        if options.MinimizeWhitespace && (right.StartsWith("-") || right.StartsWith("+")) then "" else " "

    let private joinOneLine commands format =
        let commands = if format.Options.RepeatCommands then commands else compactCommands commands format
        match commands with
        | [] -> ""
        | first :: rest ->
            rest |> List.fold (fun joined next ->
                let separator = if Set.contains (commandName next) commandNames then (if format.Options.MinimizeWhitespace then "" else " ") else commandChunkSeparator next format.Options
                joined + separator + next) first

    let private joinCommands commands format =
        match format.Options.Newlines with
        | OneLine -> joinOneLine commands format
        | AtSegments ->
            if format.Options.RepeatCommands then String.concat "\n" commands
            else
                let commands = compactCommands commands format
                let mutable lines: string list = []
                let mutable afterCommand = false
                for item in commands do
                    let name = commandName item
                    if not (Set.contains name commandNames) then
                        lines <- item :: lines
                        afterCommand <- false
                    else
                        let arguments = commandArguments item format.Options
                        if List.isEmpty lines || afterCommand then lines <- name :: lines
                        elif name = "M" || name = "m" then lines <- name :: lines
                        else
                            match lines with
                            | line :: rest -> lines <- (line + " " + name) :: rest
                            | [] -> lines <- [ name ]
                        if arguments <> "" then lines <- arguments :: lines
                        afterCommand <- arguments = ""
                lines |> List.rev |> String.concat "\n"
        | AtSubpaths ->
            let commands = if format.Options.RepeatCommands then commands else compactCommands commands format
            match commands with
            | [] -> ""
            | first :: rest ->
                rest
                |> List.fold (fun joined next ->
                    let separator =
                        let name = commandName next
                        if name = "M" || name = "m" then "\n"
                        elif format.Options.MinimizeWhitespace then "" else " "
                    joined + separator + next) first

    let private absoluteSubpath (subpath: Subpath) format =
        let segments = dropClosingLine subpath
        let move, remaining =
            match segments with
            | Line(_, endpoint) :: rest when not format.Options.ExplicitInitialLineto ->
                command "M" (joinGroups [ pointValue subpath.Start format; pointValue endpoint format ] format) format, rest
            | _ -> command "M" (pointValue subpath.Start format) format, segments
        let _, reversed =
            remaining |> List.fold (fun (previous, output) segment ->
                let serialized, next = absoluteSegment segment previous format
                next, serialized :: output) (NoPreviousCurve, [])
        let commands = move :: List.rev reversed
        joinCommands (if subpath.Closed then commands @ [ "Z" ] else commands) format

    let private chordSimilarity sourceStart sourceEnd parserStart parserEnd =
        let source = delta sourceStart sourceEnd
        let target = delta parserStart parserEnd
        let denominator = Point.squaredNorm source
        if denominator = 0.0<length^2> || Point.squaredNorm target = 0.0<length^2> then UnstableChord
        else
            let scaleCos = Point.dot source target / denominator
            let scaleSin = Point.cross source target / denominator
            if Double.IsFinite scaleCos && Double.IsFinite scaleSin then
                StableChord(sourceStart, parserStart, scaleCos, scaleSin)
            else
                UnstableChord

    let private similarityPoint (point: Point<length>) = function
        | UnstableChord -> point
        | StableChord(sourceStart, parserStart, scaleCos, scaleSin) ->
            let local = delta sourceStart point
            Point.create
                (parserStart.X + scaleCos * local.X - scaleSin * local.Y)
                (parserStart.Y + scaleSin * local.X + scaleCos * local.Y)

    let private trackedLine (sourceStart: Point<length>) (sourceEnd: Point<length>) (state: RelativeParserState) format =
        let intended = quantizedPoint sourceEnd format
        let target =
            if sourceStart.Y = sourceEnd.Y then Point.create intended.X state.Current.Y
            elif sourceStart.X = sourceEnd.X then Point.create state.Current.X intended.Y
            else intended
        let targetHorizontal = rawNumber target.Y format.Options = rawNumber state.Current.Y format.Options
        let targetVertical = rawNumber target.X format.Options = rawNumber state.Current.X format.Options
        if format.Options.UseHorizontalVertical && targetHorizontal then
            let dx = quantizedNumber (intended.X - state.Current.X) format
            [ command "h" (number dx format) format ], { state with Current = Point.create (state.Current.X + dx) state.Current.Y; PreviousCurve = NoPreviousCurve }
        elif format.Options.UseHorizontalVertical && targetVertical then
            let dy = quantizedNumber (intended.Y - state.Current.Y) format
            [ command "v" (number dy format) format ], { state with Current = Point.create state.Current.X (state.Current.Y + dy); PreviousCurve = NoPreviousCurve }
        else
            let difference = delta state.Current target |> fun point -> quantizedPoint point format
            [ command "l" (pointValue difference format) format ], { state with Current = add state.Current difference; PreviousCurve = NoPreviousCurve }

    let private trackedQuadratic sourceStart control sourceEnd state format =
        let intended = quantizedPoint sourceEnd format
        let corrected =
            match chordSimilarity sourceStart sourceEnd state.Current intended with
            | StableChord _ as similarity -> similarityPoint control similarity
            | UnstableChord -> add control (Point.scale 0.5 (delta sourceStart state.Current))
        let parserControl = quantizedPoint corrected format
        let controlDelta = delta state.Current parserControl |> fun point -> quantizedPoint point format
        let endDelta = delta state.Current intended |> fun point -> quantizedPoint point format
        let reflected = reflectedQuadratic state.Current state.PreviousCurve
        let smooth = format.Options.UseSmoothCurves && formattedPointsEqual parserControl reflected format
        let serialized = if smooth then command "t" (pointValue endDelta format) format else command "q" (joinGroups [ pointValue controlDelta format; pointValue endDelta format ] format) format
        let effective = if smooth then reflected else add state.Current controlDelta
        [ serialized ], { state with Current = add state.Current endDelta; PreviousCurve = PreviousQuadratic effective }

    let private trackedCubic sourceStart control1 control2 sourceEnd state format =
        let intended = quantizedPoint sourceEnd format
        let corrected1, corrected2 =
            match chordSimilarity sourceStart sourceEnd state.Current intended with
            | StableChord _ as similarity -> similarityPoint control1 similarity, similarityPoint control2 similarity
            | UnstableChord ->
                let drift = delta sourceStart state.Current
                add control1 (Point.scale (2.0 / 3.0) drift), add control2 (Point.scale (1.0 / 3.0) drift)
        let parserControl1, parserControl2 = quantizedPoint corrected1 format, quantizedPoint corrected2 format
        let d1 = delta state.Current parserControl1 |> fun p -> quantizedPoint p format
        let d2 = delta state.Current parserControl2 |> fun p -> quantizedPoint p format
        let de = delta state.Current intended |> fun p -> quantizedPoint p format
        let reflected = reflectedCubic state.Current state.PreviousCurve
        let smooth = format.Options.UseSmoothCurves && formattedPointsEqual parserControl1 reflected format
        let args = if smooth then [ pointValue d2 format; pointValue de format ] else [ pointValue d1 format; pointValue d2 format; pointValue de format ]
        [ command (if smooth then "s" else "c") (joinGroups args format) format ], { state with Current = add state.Current de; PreviousCurve = PreviousCubic(add state.Current d2) }

    let rec private trackedSegment segment state format =
        match segment with
        | Line(startPoint, endPoint) -> trackedLine startPoint endPoint state format
        | QuadraticBezier(startPoint, control, endPoint) -> trackedQuadratic startPoint control endPoint state format
        | CubicBezier(startPoint, control1, control2, endPoint) -> trackedCubic startPoint control1 control2 endPoint state format
        | Arc arc when arc.Start = arc.End ->
            // A same-endpoint SVG arc command draws nothing. Match the Gleam
            // serializer by replacing the library's full-arc representation
            // with two ordinary endpoint arcs before writing relative data.
            let radiusDirection =
                Point.create
                    (arc.Radius.X * Trig.cosDegrees arc.XAxisRotation)
                    (arc.Radius.X * Trig.sinDegrees arc.XAxisRotation)
            let midpoint = Point.add arc.Start radiusDirection
            let first = Arc { arc with LargeArc = false; End = midpoint }
            let second = Arc { arc with Start = midpoint; LargeArc = false }
            let firstCommands, next = trackedSegment first state format
            let secondCommands, finish = trackedSegment second next format
            firstCommands @ secondCommands, finish
        | Arc arc -> trackedArc arc state format

    and private trackedArc arc state format =
        let intended = quantizedPoint arc.End format
        let similarity = chordSimilarity arc.Start arc.End state.Current intended
        let radius, rotation =
            match similarity with
            | StableChord(_, _, scaleCos, scaleSin) ->
                let scale = sqrt (scaleCos * scaleCos + scaleSin * scaleSin)
                Point.scale scale arc.Radius, arc.XAxisRotation + Trig.atan2Degrees scaleSin scaleCos
            | UnstableChord -> arc.Radius, arc.XAxisRotation
        let endDelta = delta state.Current intended |> fun point -> quantizedPoint point format
        [ command "a" (arcArguments radius rotation arc.LargeArc arc.Sweep endDelta format) format ], { state with Current = add state.Current endDelta; PreviousCurve = NoPreviousCurve }

    let private trackedSubpath (subpath: Subpath) (state: RelativeParserState) format =
        let sourceStart = subpath.Start
        let moveDelta = quantizedPoint sourceStart format |> fun point -> delta state.Current point |> fun point -> quantizedPoint point format
        let parserStart = add state.Current moveDelta
        let mutable move = command "m" (pointValue moveDelta format) format
        let mutable currentState = { Current = parserStart; SubpathStart = parserStart; PreviousCurve = NoPreviousCurve }
        let mutable segments = dropClosingLine subpath
        if not format.Options.ExplicitInitialLineto then
            match segments with
            | Line(startPoint, endPoint) :: rest ->
                let lineFormat = { format with Options = { format.Options with UseHorizontalVertical = false } }
                let lineCommands, next = trackedLine startPoint endPoint currentState lineFormat
                let arguments = commandArguments (List.exactlyOne lineCommands) format.Options
                move <- move + commandChunkSeparator arguments format.Options + arguments
                currentState <- next
                segments <- rest
            | _ -> ()
        let commands = ResizeArray<string>()
        commands.Add move
        for segment in segments do
            let emitted, next = trackedSegment segment currentState format
            emitted |> List.iter commands.Add
            currentState <- next
        if subpath.Closed then
            commands.Add "z"
            commands |> Seq.toList, { currentState with Current = currentState.SubpathStart; PreviousCurve = NoPreviousCurve }
        else commands |> Seq.toList, currentState

    let private trackedPath (path: Path) format =
        let mutable state = { Current = origin; SubpathStart = origin; PreviousCurve = NoPreviousCurve }
        let chunks = ResizeArray<string>()
        for subpath in path.Subpaths do
            let commands, next = trackedSubpath subpath state format
            chunks.Add(joinCommands commands format)
            state <- next
        joinCommands (chunks |> Seq.toList) format

    let private formatForPath (path: Path) (options: PathSerializeOptions) =
        match options.LeftDecimals with
        | AutoLeftPadding _ ->
            let prepassOptions = { options with LeftDecimals = Succinct; MinimizeWhitespace = false; Commas = false; RepeatCommands = true; Newlines = OneLine }
            let prepassFormat = serializationFormat prepassOptions []
            let prepass =
                if options.Relative then trackedPath path prepassFormat
                else path.Subpaths |> List.map (fun subpath -> absoluteSubpath subpath prepassFormat) |> List.filter ((<>) "") |> fun chunks -> joinCommands chunks prepassFormat
            let commandLetters = "MmLlHhVvQqCcSsTtAaZz"
            let normalized = prepass |> Seq.map (fun c -> if commandLetters.Contains c || c = ',' || c = '\n' then ' ' else c) |> Seq.toArray |> String
            let tokens = normalized.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            { Options = options; NumberFormat = NumberFormat.prepareRaw (numberOptions options) tokens }
        | _ -> serializationFormat options []

    let pathWith (path: Path) options =
        let format = formatForPath path options
        if options.Relative then trackedPath path format
        else path.Subpaths |> List.map (fun subpath -> absoluteSubpath subpath format) |> List.filter ((<>) "") |> fun chunks -> joinCommands chunks format

    let path pathValue = pathWith pathValue defaultOptions
    let subpathWith (subpathValue: Subpath) options = pathWith (Path.ofSubpaths [ subpathValue ]) options
    let subpath subpathValue = subpathWith subpathValue defaultOptions
    let segmentWith segmentValue options = subpathWith (Subpath.ofSegment segmentValue) options
    let segment segmentValue = segmentWith segmentValue defaultOptions
