namespace SvgPath

/// Parsing of SVG path-data strings.
/// Arc arguments are preserved, including signed/zero radii and coincident
/// endpoints. Request SVG interpretation separately with Path.normalizeSvgArcs.
[<RequireQualifiedAccess>]
module Parse =

    open System
    open System.Globalization

    type ErrorReason =
        | ParsedPathError of error: SegmentError
        | ExpectedArcFlag
        | ExpectedCommand
        | ExpectedMove
        | ExpectedNumber
        | InvalidNumber of token: string
        | InvalidSeparator
        | UnsupportedCommand of command: string

    type Error =
        | ParseError of reason: ErrorReason * remaining: string

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
        InternalNumber.parse raw |> Result.mapError (fun () -> InvalidNumber raw)

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
                    match parseNumber raw with
                    | Error reason -> Error(reason,index)
                    | Ok value -> loop next (Number(value,index)::reversed) (nextArcArgumentPosition arcArgumentPosition)
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

    let private ensureCurrent state =
        // Closepath retains its start as the current point for subsequent drawing.
        if state.HasCurrent then Ok() else Error(ExpectedMove, state.At)

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

    // Each handler returns remaining tokens and state. Only this driver advances
    // to another command, avoiding recursive calls through Result.bind closures.
    let private repeated tokens state step =
        let rec loop current rest parsed =
            match rest with
            | Number _ :: _ ->
                match step current rest with
                | Error error -> Error error
                | Ok(next,tail) -> loop next tail true
            | _ when parsed -> Ok(rest,current)
            | _ -> Error(expectedNumber current rest)
        loop state tokens false

    let private pairs tokens state relative =
        repeated tokens state (fun current rest ->
            takeNumbers 2 current rest |> Result.bind (fun (values,tail) ->
                let endpoint = targetPoint current (values[0]*1.0<length>) (values[1]*1.0<length>) relative
                appendLine current endpoint |> Result.map (fun next -> next,tail)))

    let private parseCommand command tokens state =
        let relative = Char.IsLower command
        let code = Char.ToUpperInvariant command
        if code='M' then
            takeNumbers 2 state tokens |> Result.bind (fun (values,rest) ->
                let finished=finishActive state
                let basePoint=if relative && finished.HasCurrent then finished.Current else Point.create 0.0<length> 0.0<length>
                let target=Point.create (basePoint.X+values[0]*1.0<length>) (basePoint.Y+values[1]*1.0<length>)
                let moved={finished with Subpath=Subpath.empty target;Current=target;HasCurrent=true;Active=true;LastCubicControl=None;LastQuadraticControl=None}
                match rest with Number _ :: _ -> pairs rest moved relative | _ -> Ok(rest,moved))
        else
            ensureCurrent state |> Result.bind (fun () ->
                match code with
                | 'Z' ->
                    if not state.Active then Ok(tokens,state)
                    else
                        let startPoint=Subpath.start state.Subpath
                        Subpath.closeWith Bridge state.Subpath
                        |> Result.mapError (fun error -> ParsedPathError error,state.At)
                        |> Result.map (fun subpath ->
                            let next =
                                { state with
                                    Subpaths = subpath :: state.Subpaths
                                    Subpath = Subpath.empty startPoint
                                    Current = startPoint
                                    HasCurrent = true
                                    Active = false
                                    LastCubicControl = None
                                    LastQuadraticControl = None }
                            tokens,next)
                | 'L' -> pairs tokens state relative
                | 'H' | 'V' ->
                    repeated tokens state (fun current rest ->
                        takeNumbers 1 current rest |> Result.bind (fun (values,tail) ->
                            let value=values[0]*1.0<length>
                            let endpoint=
                                if code='H' then Point.create (if relative then current.Current.X+value else value) current.Current.Y
                                else Point.create current.Current.X (if relative then current.Current.Y+value else value)
                            appendLine current endpoint |> Result.map (fun next -> next,tail)))
                | 'Q' | 'T' ->
                    let smooth=code='T'
                    repeated tokens state (fun current rest ->
                        takeNumbers (if smooth then 2 else 4) current rest |> Result.bind (fun (values,tail) ->
                            let target i=targetPoint current (values[i]*1.0<length>) (values[i+1]*1.0<length>) relative
                            let control,endpoint=
                                if smooth then
                                    current.LastQuadraticControl |> Option.map (fun value -> reflect value current.Current) |> Option.defaultValue current.Current, target 0
                                else target 0,target 2
                            appendSegment current (QuadraticBezier(current.Current,control,endpoint)) endpoint
                            |> Result.map (fun next -> {next with LastQuadraticControl=Some control},tail)))
                | 'C' | 'S' ->
                    let smooth=code='S'
                    repeated tokens state (fun current rest ->
                        takeNumbers (if smooth then 4 else 6) current rest |> Result.bind (fun (values,tail) ->
                            let target i=targetPoint current (values[i]*1.0<length>) (values[i+1]*1.0<length>) relative
                            let control1,control2,endpoint=
                                if smooth then
                                    (current.LastCubicControl |> Option.map (fun value -> reflect value current.Current) |> Option.defaultValue current.Current),target 0,target 2
                                else target 0,target 2,target 4
                            appendSegment current (CubicBezier(current.Current,control1,control2,endpoint)) endpoint
                            |> Result.map (fun next -> {next with LastCubicControl=Some control2},tail)))
                | 'A' ->
                    repeated tokens state (fun current rest ->
                        takeArc current rest |> Result.bind (fun (rx,ry,rotation,largeArc,sweep,x,y,tail) ->
                            let endpoint=targetPoint current (x*1.0<length>) (y*1.0<length>) relative
                            let segment=Arc {Start=current.Current;Radius=Point.create (rx*1.0<length>) (ry*1.0<length>)
                                             XAxisRotation=rotation*1.0<degree>;LargeArc=largeArc;Sweep=sweep;End=endpoint}
                            appendSegment current segment endpoint |> Result.map (fun next -> next,tail)))
                | _ -> Error(UnsupportedCommand(string command),state.At))

    let rec private parseTokens tokens state : Result<Path, ErrorReason * int> =
        match tokens with
        | [] -> Ok(Path.ofSubpaths (state |> finishActive |> _.Subpaths |> List.rev))
        | Number(_,at) :: _ -> Error(ExpectedCommand,at)
        | Command(command,at) :: rest ->
            match parseCommand command rest {state with At=at} with
            | Error error -> Error error
            | Ok(remaining,next) -> parseTokens remaining next

    /// Parse an SVG path-data string. Empty input and `none` produce an empty path.
    /// Drawing after closepath begins a new subpath at the retained start point.
    /// Repeated closepath commands do not append empty subpaths.
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
