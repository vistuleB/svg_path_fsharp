namespace SvgPath

open System
open System.Globalization

type PathParseErrorReason =
    | ParsedPathError of SegmentError
    | ExpectedArcFlag
    | ExpectedCommand
    | ExpectedMove
    | ExpectedNumber
    | InvalidNumber of string
    | InvalidSeparator
    | UnsupportedCommand of string

type PathParseError =
    | ParseError of reason: PathParseErrorReason * remaining: string

type private PathToken =
    | Command of char * at: int
    | Number of float * at: int

[<Struct>]
type private ParseState =
    { Subpaths: Subpath list
      Subpath: Subpath
      Current: Point<length>
      HasCurrent: bool
      Active: bool
      LastCubicControl: Point<length> option
      LastQuadraticControl: Point<length> option
      At: int
      EndAt: int }

[<RequireQualifiedAccess>]
module Parse =
    let private supportedCommand character =
        match character with
        | 'M' | 'm' | 'L' | 'l' | 'Q' | 'q' | 'T' | 't'
        | 'C' | 'c' | 'S' | 's' | 'A' | 'a' | 'H' | 'h'
        | 'V' | 'v' | 'Z' | 'z' -> true
        | _ -> false

    let private svgWhitespace character =
        character = ' ' || character = '\n' || character = '\r'
        || character = '\t' || character = '\u000c'

    let private numberStart character =
        Char.IsDigit character || character = '+' || character = '-' || character = '.'

    let private nextArcArgumentPosition = function
        | None -> None
        | Some 6 -> Some 0
        | Some position -> Some(position + 1)

    let private readNumber (input: string) start arcArgumentPosition =
        if (arcArgumentPosition = Some 3 || arcArgumentPosition = Some 4)
           && start < input.Length && (input[start] = '0' || input[start] = '1') then
            input.Substring(start, 1), start + 1
        else
            let mutable index = start
            let mutable previousWasExponent = false
            let mutable hasDecimalPoint = false
            let mutable hasExponent = false
            let mutable reading = true
            while reading && index < input.Length do
                let character = input[index]
                if Char.IsDigit character then
                    previousWasExponent <- false
                    index <- index + 1
                elif character = '.' && not hasDecimalPoint && not hasExponent then
                    previousWasExponent <- false
                    hasDecimalPoint <- true
                    index <- index + 1
                elif (character = 'e' || character = 'E') && not hasExponent then
                    previousWasExponent <- true
                    hasExponent <- true
                    index <- index + 1
                elif (previousWasExponent || index = start) && (character = '+' || character = '-') then
                    previousWasExponent <- false
                    index <- index + 1
                else
                    reading <- false
            input.Substring(start, index - start), index

    let private parseNumber (raw: string) =
        match Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value when Double.IsFinite value -> Ok value
        | _ -> Error(InvalidNumber raw)

    let private tokenize (input: string) =
        let rec loop index reversed arcArgumentPosition =
            if index >= input.Length then Ok(List.rev reversed)
            else
                let character = input[index]
                if svgWhitespace character then loop (index + 1) reversed arcArgumentPosition
                elif character = ',' then
                    let mutable next = index + 1
                    while next < input.Length && svgWhitespace input[next] do next <- next + 1
                    match reversed with
                    | Number _ :: _ when next < input.Length && numberStart input[next] ->
                        loop (index + 1) reversed arcArgumentPosition
                    | _ -> Error(InvalidSeparator, index)
                elif supportedCommand character then
                    let arcPosition = if character = 'A' || character = 'a' then Some 0 else None
                    loop (index + 1) (Command(character, index) :: reversed) arcPosition
                elif numberStart character then
                    let raw, next = readNumber input index arcArgumentPosition
                    parseNumber raw
                    |> Result.mapError (fun reason -> reason, index)
                    |> Result.bind (fun value ->
                        loop next (Number(value, index) :: reversed) (nextArcArgumentPosition arcArgumentPosition))
                else Error(UnsupportedCommand(string character), index)
        loop 0 [] None

    let private tokenAt fallback = function
        | Command(_, at) :: _
        | Number(_, at) :: _ -> at
        | [] -> fallback

    let private expectedNumber state tokens = ExpectedNumber, tokenAt state.EndAt tokens

    let private clearControls state =
        { state with LastCubicControl = None; LastQuadraticControl = None }

    let private appendSegment state segment endpoint =
        Subpath.append segment state.Subpath
        |> Result.mapError (fun error -> ParsedPathError error, state.At)
        |> Result.map (fun subpath ->
            { state with Subpath = subpath; Current = endpoint; Active = true }
            |> clearControls)

    let private appendLine state endpoint = appendSegment state (Line(state.Current, endpoint)) endpoint

    let private targetPoint state x y relative =
        if relative then Point.create (state.Current.X + x) (state.Current.Y + y)
        else Point.create x y

    let private ensureActive state =
        if state.Active && state.HasCurrent then Ok() else Error(ExpectedMove, state.At)

    let private finishActive state =
        if state.Active then
            { state with
                Subpaths = state.Subpath :: state.Subpaths
                Subpath = Subpath.empty state.Current
                Active = false }
        else state

    let private reflect point origin =
        Point.create (2.0 * origin.X - point.X) (2.0 * origin.Y - point.Y)

    let private takeNumbers count state tokens =
        let rec loop remaining values rest =
            if remaining = 0 then Ok(List.rev values, rest)
            else
                match rest with
                | Number(value, _) :: tail -> loop (remaining - 1) (value :: values) tail
                | _ -> Error(expectedNumber state rest)
        loop count [] tokens

    let private takeArc state tokens =
        takeNumbers 7 state tokens
        |> Result.bind (fun (values, rest) ->
            match values with
            | [ radiusX; radiusY; rotation; largeArc; sweep; endX; endY ] ->
                let flag value at =
                    if value = 0.0 then Ok false
                    elif value = 1.0 then Ok true
                    else Error(ExpectedArcFlag, at)
                let largeAt = match List.item 3 tokens with Number(_, at) -> at | _ -> state.EndAt
                let sweepAt = match List.item 4 tokens with Number(_, at) -> at | _ -> state.EndAt
                flag largeArc largeAt
                |> Result.bind (fun large ->
                    flag sweep sweepAt
                    |> Result.map (fun sweepValue -> radiusX, radiusY, rotation, large, sweepValue, endX, endY, rest))
            | _ -> Error(expectedNumber state tokens))

    let rec private parseTokens tokens state : Result<Path, PathParseErrorReason * int> =
        match tokens with
        | [] -> Ok(Path.ofSubpaths (state |> finishActive |> _.Subpaths |> List.rev))
        | Number(_, at) :: _ -> Error(ExpectedCommand, at)
        | Command(command, at) :: rest -> parseCommand command rest { state with At = at }

    and private parseCommand command tokens state =
        let relative = Char.IsLower command
        match Char.ToUpperInvariant command with
        | 'M' -> parseMove tokens state relative
        | 'L' -> parsePairs tokens state relative appendLine
        | 'H' -> parseSingles tokens state (fun current value ->
            let endpoint = if relative then Point.create (current.Current.X + value) current.Current.Y else Point.create value current.Current.Y
            appendLine current endpoint)
        | 'V' -> parseSingles tokens state (fun current value ->
            let endpoint = if relative then Point.create current.Current.X (current.Current.Y + value) else Point.create current.Current.X value
            appendLine current endpoint)
        | 'Q' -> parseQuadratics tokens state relative false
        | 'T' -> parseQuadratics tokens state relative true
        | 'C' -> parseCubics tokens state relative false
        | 'S' -> parseCubics tokens state relative true
        | 'A' -> parseArcs tokens state relative
        | 'Z' -> parseClose tokens state
        | _ -> Error(UnsupportedCommand(string command), state.At)

    and private parseMove tokens state relative =
        takeNumbers 2 state tokens
        |> Result.bind (fun (values, rest) ->
            match values with
            | [ x; y ] ->
                let finished = finishActive state
                let basePoint =
                    if relative && finished.HasCurrent then finished.Current
                    else Point.create 0.0<length> 0.0<length>
                let target = Point.create (basePoint.X + x * 1.0<length>) (basePoint.Y + y * 1.0<length>)
                let moved =
                    { finished with
                        Subpath = Subpath.empty target
                        Current = target
                        HasCurrent = true
                        Active = true
                        LastCubicControl = None
                        LastQuadraticControl = None }
                parseImplicitLines rest moved relative
            | _ -> Error(expectedNumber state tokens))

    and private parseImplicitLines tokens state relative =
        match tokens with
        | Number _ :: _ ->
            takeNumbers 2 state tokens
            |> Result.bind (fun (values, rest) ->
                match values with
                | [ x; y ] ->
                    let endpoint = targetPoint state (x * 1.0<length>) (y * 1.0<length>) relative
                    appendLine state endpoint
                    |> Result.bind (fun next -> parseImplicitLines rest next relative)
                | _ -> Error(expectedNumber state tokens))
        | _ -> parseTokens tokens state

    and private parsePairs tokens state relative append =
        ensureActive state
        |> Result.bind (fun () ->
            let rec loop current rest parsed =
                match rest with
                | Number _ :: _ ->
                    takeNumbers 2 current rest
                    |> Result.bind (fun (values, tail) ->
                        match values with
                        | [ x; y ] ->
                            append current (targetPoint current (x * 1.0<length>) (y * 1.0<length>) relative)
                            |> Result.bind (fun next -> loop next tail true)
                        | _ -> Error(expectedNumber current rest))
                | _ when parsed -> parseTokens rest current
                | _ -> Error(expectedNumber current rest)
            loop state tokens false)

    and private parseSingles tokens state append =
        ensureActive state
        |> Result.bind (fun () ->
            let rec loop current rest parsed =
                match rest with
                | Number(value, _) :: tail -> append current (value * 1.0<length>) |> Result.bind (fun next -> loop next tail true)
                | _ when parsed -> parseTokens rest current
                | _ -> Error(expectedNumber current rest)
            loop state tokens false)

    and private parseQuadratics tokens state relative smooth =
        ensureActive state
        |> Result.bind (fun () ->
            let arity = if smooth then 2 else 4
            let rec loop current rest parsed =
                match rest with
                | Number _ :: _ ->
                    takeNumbers arity current rest
                    |> Result.bind (fun (values, tail) ->
                        let control, endpoint =
                            if smooth then
                                let control = current.LastQuadraticControl |> Option.map (fun value -> reflect value current.Current) |> Option.defaultValue current.Current
                                let endpoint = targetPoint current (values[0] * 1.0<length>) (values[1] * 1.0<length>) relative
                                control, endpoint
                            else
                                targetPoint current (values[0] * 1.0<length>) (values[1] * 1.0<length>) relative,
                                targetPoint current (values[2] * 1.0<length>) (values[3] * 1.0<length>) relative
                        appendSegment current (QuadraticBezier(current.Current, control, endpoint)) endpoint
                        |> Result.map (fun next -> { next with LastQuadraticControl = Some control })
                        |> Result.bind (fun next -> loop next tail true))
                | _ when parsed -> parseTokens rest current
                | _ -> Error(expectedNumber current rest)
            loop state tokens false)

    and private parseCubics tokens state relative smooth =
        ensureActive state
        |> Result.bind (fun () ->
            let arity = if smooth then 4 else 6
            let rec loop current rest parsed =
                match rest with
                | Number _ :: _ ->
                    takeNumbers arity current rest
                    |> Result.bind (fun (values, tail) ->
                        let control1, control2, endpoint =
                            if smooth then
                                let first = current.LastCubicControl |> Option.map (fun value -> reflect value current.Current) |> Option.defaultValue current.Current
                                first,
                                targetPoint current (values[0] * 1.0<length>) (values[1] * 1.0<length>) relative,
                                targetPoint current (values[2] * 1.0<length>) (values[3] * 1.0<length>) relative
                            else
                                targetPoint current (values[0] * 1.0<length>) (values[1] * 1.0<length>) relative,
                                targetPoint current (values[2] * 1.0<length>) (values[3] * 1.0<length>) relative,
                                targetPoint current (values[4] * 1.0<length>) (values[5] * 1.0<length>) relative
                        appendSegment current (CubicBezier(current.Current, control1, control2, endpoint)) endpoint
                        |> Result.map (fun next -> { next with LastCubicControl = Some control2 })
                        |> Result.bind (fun next -> loop next tail true))
                | _ when parsed -> parseTokens rest current
                | _ -> Error(expectedNumber current rest)
            loop state tokens false)

    and private parseArcs tokens state relative =
        ensureActive state
        |> Result.bind (fun () ->
            let rec loop current rest parsed =
                match rest with
                | Number _ :: _ ->
                    takeArc current rest
                    |> Result.bind (fun (radiusX, radiusY, rotation, largeArc, sweep, endX, endY, tail) ->
                        let endpoint = targetPoint current (endX * 1.0<length>) (endY * 1.0<length>) relative
                        let radiusX = abs radiusX * 1.0<length>
                        let radiusY = abs radiusY * 1.0<length>
                        let next =
                            if endpoint = current.Current then Ok(clearControls current)
                            elif radiusX = 0.0<length> || radiusY = 0.0<length> then appendLine current endpoint
                            else
                                appendSegment current
                                    (Arc
                                        { Start = current.Current
                                          Radius = Point.create radiusX radiusY
                                          XAxisRotation = rotation * 1.0<degree>
                                          LargeArc = largeArc
                                          Sweep = sweep
                                          End = endpoint }) endpoint
                        next |> Result.bind (fun value -> loop value tail true))
                | _ when parsed -> parseTokens rest current
                | _ -> Error(expectedNumber current rest)
            loop state tokens false)

    and private parseClose tokens state =
        ensureActive state
        |> Result.bind (fun () ->
            let startPoint = Subpath.start state.Subpath
            Subpath.setClosedWith Bridge true state.Subpath
            |> Result.mapError (fun error -> ParsedPathError error, state.At)
            |> Result.bind (fun subpath ->
                parseTokens tokens
                    { state with
                        Subpaths = subpath :: state.Subpaths
                        Subpath = Subpath.empty startPoint
                        Current = startPoint
                        HasCurrent = true
                        Active = false
                        LastCubicControl = None
                        LastQuadraticControl = None }))

    /// Parse an SVG path-data string. Empty input and `none` produce an empty path.
    let path (input: string) =
        if input.Trim() = "none" then Ok Path.empty
        else
            tokenize input
            |> Result.bind (fun tokens ->
                parseTokens tokens
                    { Subpaths = []
                      Subpath = Subpath.empty (Point.create 0.0<length> 0.0<length>)
                      Current = Point.create 0.0<length> 0.0<length>
                      HasCurrent = false
                      Active = false
                      LastCubicControl = None
                      LastQuadraticControl = None
                      At = 0
                      EndAt = input.Length })
            |> Result.mapError (fun (reason, at) -> ParseError(reason, input.Substring at))
