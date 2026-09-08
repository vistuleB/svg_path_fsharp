namespace SvgPath

type StrokeError =
    | StrokePathError of error: SegmentError
    | StrokeOffsetError of error: Error
    | InvalidStrokeOutlineWidth of width: float<length>
    | InvalidDashLength of length: float<length>
    | InvalidDashOffset of offset: float<length>
    | InvalidDashPatternLength

[<Struct>]
/// Stroke width and technical offset settings; join and cap are operation arguments.
type StrokeOptions =
    { Width: float<length>
      Offset: Options }

[<Struct>]
type DashOptions =
    { Pattern: float<length> list
      Offset: float<length>
      LengthOptions: LengthOptions }

/// Dash-pattern application and stroke-outline construction.
/// Outline operations require explicit styles; pure dash extraction does not.
[<RequireQualifiedAccess>]
module Stroke =
    type Join = SvgPath.Join
    type Cap = SvgPath.Cap

    let defaultOptions =
        { Width = 1.0<length>
          Offset = Offset.defaultOptions }

    let defaultDashOptions pattern offset =
        { Pattern = pattern
          Offset = offset
          LengthOptions = Segment.defaultLengthOptions }

    let private validateOptions options join =
        if options.Width <= 0.0<length> || not (System.Double.IsFinite(float options.Width)) then
            Error(InvalidStrokeOutlineWidth options.Width)
        else
            // Validate even when the path or generated dash list is empty.
            Offset.validateOptions options.Offset
            |> Result.bind (fun () -> Offset.validateJoin join)
            |> Result.mapError (Offset.publicError >> StrokeOffsetError)

    let rec private validateDashPattern = function
        | [] -> Ok()
        | first :: rest ->
            if first < 0.0<length> || not (System.Double.IsFinite(float first)) then
                Error(InvalidDashLength first)
            else validateDashPattern rest

    let private validateDashPatternLength pattern =
        let rec loop total = function
            | [] -> Ok()
            | first :: rest ->
                if float total > System.Double.MaxValue - float first then Error InvalidDashPatternLength
                else loop (total + first) rest
        loop 0.0<length> pattern

    let private normalizeDashPattern pattern =
        validateDashPattern pattern
        |> Result.bind (fun () ->
            if List.isEmpty pattern || List.forall ((=) 0.0<length>) pattern then Ok []
            else
                let normalized = if List.length pattern % 2 = 1 then pattern @ pattern else pattern
                validateDashPatternLength normalized |> Result.map (fun () -> normalized))

    let private validateDashOptions options =
        validateDashPattern options.Pattern
        |> Result.bind (fun () ->
            if not (System.Double.IsFinite(float options.Offset)) then Error(InvalidDashOffset options.Offset)
            else
                Segment.validateLengthOptions options.LengthOptions
                |> Result.mapError StrokePathError)

    let rec private lineSegmentsBetween points =
        match points with
        | []
        | [ _ ] -> []
        | first :: second :: rest ->
            let tail = lineSegmentsBetween (second :: rest)
            if Point.near 1.0e-9<length> first second then tail
            else Line(first, second) :: tail

    let private reverseSegments segments =
        segments |> List.rev |> List.map Segment.reverse

    let private strokeCapSegments center (tangent: Point<1>) radius cap atEnd =
        let normal = Point.rotateCounterclockwise tangent
        let positive = Point.translate (Point.scale radius normal) center
        let negative = Point.translate (Point.scale -radius normal) center
        match cap with
        | Butt ->
            if atEnd then Ok(lineSegmentsBetween [ positive; negative ])
            else Ok(lineSegmentsBetween [ negative; positive ])
        | Square ->
            let extension = Point.scale (if atEnd then radius else -radius) tangent
            let positiveExtended = Point.translate extension positive
            let negativeExtended = Point.translate extension negative
            if atEnd then
                Ok(lineSegmentsBetween [ positive; positiveExtended; negativeExtended; negative ])
            else
                Ok(lineSegmentsBetween [ negative; negativeExtended; positiveExtended; positive ])
        | RoundCap ->
            let startPoint, finish = if atEnd then positive, negative else negative, positive
            Ok [ Arc
                { Start = startPoint
                  Radius = Point.create radius radius
                  XAxisRotation = 0.0<degree>
                  LargeArc = false
                  Sweep = true
                  End = finish } ]

    let private strokeEndCap source radius cap =
        match List.tryLast (Subpath.segments source) with
        | None -> Error(InternalPathError EmptySubpath)
        | Some last ->
            Offset.unitTangent last 1.0<parameter>
            |> Result.bind (fun tangent ->
                strokeCapSegments (Subpath.finish source) tangent radius cap true)

    let private strokeStartCap source radius cap =
        match Subpath.segments source with
        | [] -> Error(InternalPathError EmptySubpath)
        | first :: _ ->
            Offset.unitTangent first 0.0<parameter>
            |> Result.bind (fun tangent ->
                strokeCapSegments (Subpath.start source) tangent radius cap false)

    let private zeroLengthRoundStrokePath center radius =
        let right = Point.translate (Point.create radius 0.0<length>) center
        let left = Point.translate (Point.create -radius 0.0<length>) center
        let radial = Point.create radius radius
        let segments =
            [ Arc { Start = right; Radius = radial; XAxisRotation = 0.0<degree>
                    LargeArc = false; Sweep = true; End = left }
              Arc { Start = left; Radius = radial; XAxisRotation = 0.0<degree>
                    LargeArc = false; Sweep = true; End = right } ]
        Subpath.create segments
        |> Result.mapError InternalPathError
        |> Result.bind (fun outline -> Subpath.setClosed true outline |> Result.mapError InternalPathError)
        |> Result.map Path.singleton

    let private zeroLengthSquareStrokePath center radius =
        let topLeft = Point.translate (Point.create -radius -radius) center
        let topRight = Point.translate (Point.create radius -radius) center
        let bottomRight = Point.translate (Point.create radius radius) center
        let bottomLeft = Point.translate (Point.create -radius radius) center
        lineSegmentsBetween [ topLeft; topRight; bottomRight; bottomLeft; topLeft ]
        |> Subpath.create
        |> Result.mapError InternalPathError
        |> Result.bind (fun outline -> Subpath.setClosed true outline |> Result.mapError InternalPathError)
        |> Result.map Path.singleton

    let private zeroLengthStrokePath subpath radius cap =
        let center = Subpath.start subpath
        match cap with
        | Butt -> Ok Path.empty
        | RoundCap -> zeroLengthRoundStrokePath center radius
        | Square -> zeroLengthSquareStrokePath center radius

    let private closedUntrimmedSideFromNormalizedSource source offset join options =
        Offset.buildSingleOffsetUntrimmed source offset join options
        |> Result.map (fun build -> build.Subpath)
        |> Result.bind (fun side ->
            if Subpath.isClosed side then Ok side
            else
                Subpath.setClosedWith
                    (WiggleWith options.Fitting.Tolerance) true side
                |> Result.mapError InternalPathError)

    let private untrimmedStrokeOutlineFromNormalizedSource source radius join cap options =
        match Offset.buildSingleOffsetUntrimmed source radius join options |> Result.map (fun build -> build.Subpath),
              Offset.buildSingleOffsetUntrimmed source -radius join options |> Result.map (fun build -> build.Subpath),
              strokeEndCap source radius cap,
              strokeStartCap source radius cap with
        | Ok positive, Ok negative, Ok endCap, Ok startCap ->
            let segments =
                Subpath.segments positive
                @ endCap
                @ reverseSegments (Subpath.segments negative)
                @ startCap
            Subpath.createWith Wiggle segments
            |> Result.mapError InternalPathError
            |> Result.bind (fun candidate ->
                Subpath.setClosedWith Wiggle true candidate |> Result.mapError InternalPathError)
        | Error error, _, _, _
        | _, Error error, _, _
        | _, _, Error error, _
        | _, _, _, Error error -> Error error

    let private untrimmedStrokeOutline source radius join cap options =
        Offset.normalizeSourceSubpath source options
        |> Result.bind (fun normalized ->
            untrimmedStrokeOutlineFromNormalizedSource normalized radius join cap options)

    let private untrimmedStrokeBand
        (source: Subpath) (width: float<length>) join cap (options: Options) =
        let radius = width / 2.0
        Offset.normalizeSourceSubpath source options
        |> Result.bind (fun normalized ->
            if Subpath.isClosed source then
                match closedUntrimmedSideFromNormalizedSource normalized -radius join options,
                      closedUntrimmedSideFromNormalizedSource normalized radius join options with
                | Ok interior, Ok exterior -> Ok(ClosedSubpathBand(exterior, interior))
                | Error error, _
                | _, Error error -> Error error
            else
                untrimmedStrokeOutlineFromNormalizedSource normalized radius join cap options
                |> Result.map OpenSubpathBand)

    let private closedStrokePath
        source (radius: float<length>) join cap (options: Options) =
        untrimmedStrokeBand source (radius * 2.0) join cap options
        |> Result.bind (function
            | OpenSubpathBand _ -> Error InternalBandSubpathNotClosed
            | ClosedSubpathBand(exterior, interior) ->
                Offset.topologicalBandPathWithOpinions
                    [ interior; exterior ]
                    [ ClosedSubpathBand(exterior, interior) ]
                    [ { Left = 1; Right = 0 }; { Left = 0; Right = 1 } ]
                    options)

    /// Build and trim the complete outline; band-side cusp settings do not apply.
    let subpathWith subpath join cap (options: StrokeOptions) =
        validateOptions options join
        |> Result.bind (fun () ->
            let result =
                let radius = options.Width / 2.0
                match Subpath.segments subpath with
                | [] -> Ok Path.empty
                | _ ->
                    Subpath.isZeroLength subpath 1.0e-9<length>
                    |> Result.mapError InternalPathError
                    |> Result.bind (fun zeroLength ->
                        if zeroLength then zeroLengthStrokePath subpath radius cap
                        elif Subpath.isClosed subpath then
                            closedStrokePath subpath radius join cap options.Offset
                            |> Result.bind Offset.orientOutlinePath
                        else
                            untrimmedStrokeOutline subpath radius join cap options.Offset
                            |> Result.bind (fun untrimmed ->
                                Offset.topologicalBandPath
                                    [ untrimmed ] [ OpenSubpathBand untrimmed ] options.Offset)
                            |> Result.bind Offset.orientOutlinePath)
            result |> Result.mapError (Offset.publicError >> StrokeOffsetError))

    let rec private strokeSubpaths subpaths join cap options reversedStroked =
        match subpaths with
        | [] -> Ok(List.rev reversedStroked)
        | first :: rest ->
            subpathWith first join cap options
            |> Result.bind (fun path ->
                strokeSubpaths rest join cap options (List.rev path.Subpaths @ reversedStroked))

    let subpath subpath width join cap = subpathWith subpath join cap { defaultOptions with Width = width }

    let segmentWith segment join cap options =
        validateOptions options join
        |> Result.bind (fun () ->
            Subpath.create [ segment ]
            |> Result.mapError StrokePathError
            |> Result.bind (fun subpath -> subpathWith subpath join cap options))

    let segment segment width join cap = segmentWith segment join cap { defaultOptions with Width = width }

    let pathWith (path: Path) join cap options =
        validateOptions options join
        |> Result.bind (fun () -> strokeSubpaths path.Subpaths join cap options [] |> Result.map Path.ofSubpaths)

    let path path width join cap = pathWith path join cap { defaultOptions with Width = width }

    let private positiveRemainder (value: float<length>) (modulus: float<length>) =
        let turns = floor (value / modulus)
        let remainder = value - turns * modulus
        if remainder < 0.0<length> then remainder + modulus
        elif remainder >= modulus then remainder - modulus
        else remainder

    let private dashStart pattern offset =
        let rec loop index remainingOffset = function
            | [] -> 0, 0.0<length>
            | [ last ] -> index, last - remainingOffset
            | first :: rest when remainingOffset < first -> index, first - remainingOffset
            | first :: rest -> loop (index + 1) (remainingOffset - first) rest
        loop 0 offset pattern

    let private dashLengthAt pattern index =
        List.tryItem index pattern |> Option.defaultValue 0.0<length>

    let private nextDashIndex index pattern =
        let next = index + 1
        if next >= List.length pattern then 0 else next

    let private dashIntervals length pattern offset =
        let patternLength = List.sum pattern
        let startIndex, startRemaining = dashStart pattern (positiveRemainder offset patternLength)
        let rec loop position index remaining reversed =
            if position >= length then List.rev reversed
            elif remaining <= 0.0<length> then
                let next = nextDashIndex index pattern
                loop position next (dashLengthAt pattern next) reversed
            else
                let distanceToEnd = length - position
                let step, nextPosition =
                    if remaining >= distanceToEnd then distanceToEnd, length
                    else remaining, position + remaining
                let reversed =
                    if index % 2 = 0 && step > 0.0<length> then (position, nextPosition) :: reversed
                    else reversed
                let next = nextDashIndex index pattern
                loop nextPosition next (dashLengthAt pattern next) reversed
        loop 0.0<length> startIndex startRemaining []

    let private openFullDash (subpath: Subpath) =
        if subpath.Closed then Subpath.openAt subpath { SegmentIndex = 0; T = 0.0<parameter> }
        else Ok subpath

    let private firstSplitPiece (subpath: Subpath) distance options =
        Subpath.betweenLengthsManyWith subpath [ distance ] options
        |> Result.bind (function
            | first :: _ -> Ok first
            | [] -> Subpath.betweenLengthsWith subpath 0.0<length> distance options)

    let private lastSplitPiece (subpath: Subpath) distance options =
        Subpath.betweenLengthsManyWith subpath [ distance ] options
        |> Result.bind (function
            | [] -> Subpath.betweenLengthsWith subpath distance distance options
            | pieces -> Ok(List.last pieces))

    let private dashPiece (subpath: Subpath) fromDistance toDistance length options =
        if fromDistance = 0.0<length> && toDistance = length then openFullDash subpath
        elif subpath.Closed then Subpath.betweenLengthsWith subpath fromDistance toDistance options
        elif fromDistance = 0.0<length> then firstSplitPiece subpath toDistance options
        elif toDistance = length then lastSplitPiece subpath fromDistance options
        else Subpath.betweenLengthsWith subpath fromDistance toDistance options

    let private dashPieces intervals (subpath: Subpath) length options =
        intervals
        |> List.fold (fun state (fromDistance, toDistance) ->
            state
            |> Result.bind (fun reversed ->
                dashPiece subpath fromDistance toDistance length options
                |> Result.map (fun piece -> piece :: reversed))) (Ok [])
        |> Result.map List.rev

    let subpathDashesWith (subpath: Subpath) dashOptions =
        validateDashOptions dashOptions
        |> Result.bind (fun () -> normalizeDashPattern dashOptions.Pattern)
        |> Result.bind (fun pattern ->
            Subpath.lengthWith subpath dashOptions.LengthOptions
            |> Result.mapError StrokePathError
            |> Result.bind (fun length ->
                if length <= 0.0<length> then Ok []
                elif List.isEmpty pattern then Ok [ subpath ]
                else
                    dashIntervals length pattern dashOptions.Offset
                    |> fun intervals -> dashPieces intervals subpath length dashOptions.LengthOptions
                    |> Result.mapError StrokePathError))

    let subpathDashes subpath pattern offset =
        subpathDashesWith subpath (defaultDashOptions pattern offset)

    let pathDashesWith (path: Path) dashOptions =
        validateDashOptions dashOptions
        |> Result.bind (fun () ->
            path.Subpaths
            |> List.fold (fun state subpath ->
                state
                |> Result.bind (fun reversed ->
                    subpathDashesWith subpath dashOptions
                    |> Result.map (fun dashes -> List.rev dashes @ reversed))) (Ok []))
        |> Result.map (List.rev >> Path.ofSubpaths)

    let pathDashes path pattern offset = pathDashesWith path (defaultDashOptions pattern offset)

    let subpathDashedWith subpath join cap options dashOptions =
        validateOptions options join
        |> Result.bind (fun () -> subpathDashesWith subpath dashOptions)
        |> Result.bind (fun dashes -> strokeSubpaths dashes join cap options [] |> Result.map Path.ofSubpaths)

    let subpathDashed subpath width pattern offset join cap =
        subpathDashedWith subpath join cap { defaultOptions with Width = width } (defaultDashOptions pattern offset)

    let pathDashedWith path join cap options dashOptions =
        validateOptions options join
        |> Result.bind (fun () -> pathDashesWith path dashOptions)
        |> Result.bind (fun dashes -> pathWith dashes join cap options)

    let pathDashed path width pattern offset join cap =
        pathDashedWith path join cap { defaultOptions with Width = width } (defaultDashOptions pattern offset)
