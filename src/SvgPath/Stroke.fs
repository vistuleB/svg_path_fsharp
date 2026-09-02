namespace SvgPath

type StrokeError =
    | StrokePathError of SegmentError
    | StrokeOffsetError of Error
    | InvalidStrokeOutlineWidth of float<length>
    | InvalidDashLength of float<length>
    | InvalidDashOffset of float<length>
    | InvalidDashPatternLength

type StrokeCap =
    | StrokeButt
    | StrokeRound
    | StrokeSquare

[<Struct>]
type StrokeOptions =
    { Width: float<length>
      Cap: StrokeCap
      Offset: Options }

[<Struct>]
type DashOptions =
    { Pattern: float<length> list
      Offset: float<length>
      Length: LengthOptions }

[<RequireQualifiedAccess>]
module Stroke =
    let defaultOptions =
        { Width = 1.0<length>
          Cap = StrokeButt
          Offset = Offset.defaultOptions }

    let defaultDashOptions pattern offset =
        { Pattern = pattern
          Offset = offset
          Length = Segment.defaultLengthOptions }

    let private validateOptions options =
        if options.Width <= 0.0<length> || not (System.Double.IsFinite(float options.Width)) then
            Error(InvalidStrokeOutlineWidth options.Width)
        else Ok()

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
                Segment.validateLengthOptions options.Length
                |> Result.mapError StrokePathError)

    let private toOffsetCap = function
        | StrokeButt -> Butt
        | StrokeRound -> RoundCap
        | StrokeSquare -> Square

    let rec private strokeSubpaths subpaths options reversedStroked =
        match subpaths with
        | [] -> Ok(List.rev reversedStroked)
        | first :: rest ->
            Offset.subpathStrokeWith first options.Width (toOffsetCap options.Cap) options.Offset
            |> Result.mapError StrokeOffsetError
            |> Result.bind (fun path ->
                strokeSubpaths rest options (List.rev path.Subpaths @ reversedStroked))

    let subpathWith subpath options =
        validateOptions options
        |> Result.bind (fun () ->
            Offset.subpathStrokeWith subpath options.Width (toOffsetCap options.Cap) options.Offset
            |> Result.mapError StrokeOffsetError)

    let subpath subpath width = subpathWith subpath { defaultOptions with Width = width }

    let segmentWith segment options =
        validateOptions options
        |> Result.bind (fun () ->
            Subpath.create [ segment ]
            |> Result.mapError StrokePathError
            |> Result.bind (fun subpath -> subpathWith subpath options))

    let segment segment width = segmentWith segment { defaultOptions with Width = width }

    let pathWith (path: Path) options =
        validateOptions options
        |> Result.bind (fun () -> strokeSubpaths path.Subpaths options [] |> Result.map Path.ofSubpaths)

    let path path width = pathWith path { defaultOptions with Width = width }

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
            Subpath.lengthWith subpath dashOptions.Length
            |> Result.mapError StrokePathError
            |> Result.bind (fun length ->
                if length <= 0.0<length> then Ok []
                elif List.isEmpty pattern then Ok [ subpath ]
                else
                    dashIntervals length pattern dashOptions.Offset
                    |> fun intervals -> dashPieces intervals subpath length dashOptions.Length
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

    let subpathDashedWith subpath options dashOptions =
        validateOptions options
        |> Result.bind (fun () -> subpathDashesWith subpath dashOptions)
        |> Result.bind (fun dashes -> strokeSubpaths dashes options [] |> Result.map Path.ofSubpaths)

    let subpathDashed subpath width pattern offset =
        subpathDashedWith subpath { defaultOptions with Width = width } (defaultDashOptions pattern offset)

    let pathDashedWith path options dashOptions =
        validateOptions options
        |> Result.bind (fun () -> pathDashesWith path dashOptions)
        |> Result.bind (fun dashes -> pathWith dashes options)

    let pathDashed path width pattern offset =
        pathDashedWith path { defaultOptions with Width = width } (defaultDashOptions pattern offset)
