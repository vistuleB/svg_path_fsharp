namespace SvgPath

open System
open System.Globalization

/// Parse SVG transform attribute values.
[<RequireQualifiedAccess>]
module TransformParse =
    type ErrorReason =
        | ExpectedClose
        | ExpectedOpen
        | ExpectedTransform
        | InvalidArgumentCount of name: string * count: int
        | InvalidNumber of raw: string
        | NonFiniteTransform
        | UnexpectedToken of token: string
        | UnknownTransform of name: string

    type Error = ParseError of reason: ErrorReason * remaining: string

    type private Token =
        | Close of at: int
        | Comma of at: int
        | Name of value: string * at: int
        | Number of value: float * at: int
        | Open of at: int
        | Whitespace of at: int

    type private LocatedError = LocatedError of reason: ErrorReason * at: int

    let private tokenAt fallback tokens =
        match tokens with
        | [] -> fallback
        | Close at :: _
        | Comma at :: _
        | Name(_, at) :: _
        | Number(_, at) :: _
        | Open at :: _
        | Whitespace at :: _ -> at

    let rec private dropWhitespace tokens =
        match tokens with
        | Whitespace _ :: rest -> dropWhitespace rest
        | _ -> tokens

    let private invalidCount name arguments at =
        Error(LocatedError(InvalidArgumentCount(name, List.length arguments), at))

    let private transformFromArguments name arguments at =
        match name, arguments with
        | "matrix", [ a; b; c; d; e; f ] ->
            Ok(Transform.matrix a b c d (Length.fromFloat e) (Length.fromFloat f))
        | "translate", [ x ] -> Ok(Transform.translate (Length.fromFloat x) 0.0<length>)
        | "translate", [ x; y ] -> Ok(Transform.translate (Length.fromFloat x) (Length.fromFloat y))
        | "scale", [ factor ] -> Ok(Transform.scale factor)
        | "scale", [ x; y ] -> Ok(Transform.scaleXY x y)
        | "rotate", [ degrees ] -> Ok(Transform.rotate (Degree.fromFloat degrees))
        | "rotate", [ degrees; centerX; centerY ] ->
            let centerX = Length.fromFloat centerX
            let centerY = Length.fromFloat centerY
            let moveToOrigin = Transform.translate -centerX -centerY
            let rotate = Transform.rotate (Degree.fromFloat degrees)
            let moveBack = Transform.translate centerX centerY
            Ok(
                moveToOrigin
                |> fun first -> Transform.chain first rotate
                |> fun first -> Transform.chain first moveBack)
        | "skewX", [ degrees ] -> Ok(Transform.skewX (Degree.fromFloat degrees))
        | "skewY", [ degrees ] -> Ok(Transform.skewY (Degree.fromFloat degrees))
        | ("matrix" | "translate" | "scale" | "rotate" | "skewX" | "skewY"), _ ->
            invalidCount name arguments at
        | _ -> Error(LocatedError(UnknownTransform name, at))

    let rec private takeArguments tokens arguments endAt =
        match dropWhitespace tokens with
        | [] -> Error(LocatedError(ExpectedClose, endAt))
        | Close _ :: rest -> Ok(List.rev arguments, rest)
        | Number(number, _) :: rest -> takeArgumentsAfterNumber rest (number :: arguments) endAt
        | Comma at :: _ -> Error(LocatedError(UnexpectedToken ",", at))
        | Name(name, at) :: _ -> Error(LocatedError(UnexpectedToken name, at))
        | Open at :: _ -> Error(LocatedError(UnexpectedToken "(", at))
        | Whitespace _ :: _ -> failwith "dropWhitespace left whitespace"

    and private takeArgumentsAfterNumber tokens arguments endAt =
        match tokens with
        | [] -> Error(LocatedError(ExpectedClose, endAt))
        | Close _ :: rest -> Ok(List.rev arguments, rest)
        | Whitespace _ :: _ ->
            match dropWhitespace tokens with
            | [] -> Error(LocatedError(ExpectedClose, endAt))
            | Close _ :: rest -> Ok(List.rev arguments, rest)
            | Comma _ :: rest -> takeArgumentAfterSeparator (dropWhitespace rest) arguments endAt
            | rest -> takeArgumentAfterSeparator rest arguments endAt
        | Comma _ :: rest -> takeArgumentAfterSeparator (dropWhitespace rest) arguments endAt
        | Number(_, at) :: _ -> Error(LocatedError(UnexpectedToken "number", at))
        | Name(name, at) :: _ -> Error(LocatedError(UnexpectedToken name, at))
        | Open at :: _ -> Error(LocatedError(UnexpectedToken "(", at))

    and private takeArgumentAfterSeparator tokens arguments endAt =
        match tokens with
        | [] -> Error(LocatedError(ExpectedClose, endAt))
        | Number(number, _) :: rest -> takeArgumentsAfterNumber rest (number :: arguments) endAt
        | Comma at :: _ -> Error(LocatedError(UnexpectedToken ",", at))
        | Close at :: _ -> Error(LocatedError(UnexpectedToken ")", at))
        | Name(name, at) :: _ -> Error(LocatedError(UnexpectedToken name, at))
        | Open at :: _ -> Error(LocatedError(UnexpectedToken "(", at))
        | Whitespace _ :: _ -> failwith "dropWhitespace left whitespace"

    let rec private parseTransforms tokens accumulated endAt =
        match tokens with
        | [] -> Ok accumulated
        | Name(name, nameAt) :: Open _ :: rest ->
            takeArguments rest [] endAt
            |> Result.bind (fun (arguments, rest) ->
                transformFromArguments name arguments nameAt
                |> Result.bind (fun next ->
                    let accumulated = Transform.chain next accumulated
                    if Affine.isFinite accumulated then
                        continueTransformList rest accumulated endAt
                    else
                        Error(LocatedError(NonFiniteTransform, nameAt))))
        | Name _ :: rest -> Error(LocatedError(ExpectedOpen, tokenAt endAt rest))
        | Close at :: _
        | Comma at :: _
        | Number(_, at) :: _
        | Open at :: _
        | Whitespace at :: _ -> Error(LocatedError(ExpectedTransform, at))

    and private continueTransformList tokens accumulated endAt =
        match tokens with
        | [] -> Ok accumulated
        | Whitespace _ :: _ ->
            match dropWhitespace tokens with
            | [] -> Ok accumulated
            | Comma _ :: rest -> parseTransformAfterSeparator (dropWhitespace rest) accumulated endAt
            | rest -> parseTransforms rest accumulated endAt
        | Comma _ :: rest -> parseTransformAfterSeparator (dropWhitespace rest) accumulated endAt
        | token :: _ -> Error(LocatedError(ExpectedTransform, tokenAt endAt [ token ]))

    and private parseTransformAfterSeparator tokens accumulated endAt =
        match tokens with
        | [] -> Error(LocatedError(ExpectedTransform, endAt))
        | Comma at :: _ -> Error(LocatedError(UnexpectedToken ",", at))
        | _ -> parseTransforms tokens accumulated endAt

    let private isWhitespace character =
        character = ' ' || character = '\n' || character = '\r' || character = '\t'

    let private isAsciiLetter character =
        (character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')

    let private isDigit character = character >= '0' && character <= '9'

    let private isNumberStart character =
        isDigit character || character = '+' || character = '-' || character = '.'

    let private readName (input: string) startAt =
        let mutable at = startAt
        while at < input.Length && isAsciiLetter input[at] do at <- at + 1
        input.Substring(startAt, at - startAt), at

    let private readNumber (input: string) startAt =
        let mutable at = startAt
        let mutable previousWasExponent = false
        let mutable reading = true
        while at < input.Length && reading do
            let character = input[at]
            if isDigit character || character = '.' then
                previousWasExponent <- false
                at <- at + 1
            elif character = 'e' || character = 'E' then
                previousWasExponent <- true
                at <- at + 1
            elif (previousWasExponent || at = startAt) && (character = '+' || character = '-') then
                previousWasExponent <- false
                at <- at + 1
            else
                reading <- false
        input.Substring(startAt, at - startAt), at

    let private parseFiniteNumber (raw: string) =
        match Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value when Double.IsFinite value -> Some value
        | _ -> None

    let private tokenize (input: string) =
        let rec loop at tokens =
            if at >= input.Length then Ok(List.rev tokens)
            else
                let character = input[at]
                if isWhitespace character then loop (at + 1) (Whitespace at :: tokens)
                else
                    match character with
                    | '(' -> loop (at + 1) (Open at :: tokens)
                    | ')' -> loop (at + 1) (Close at :: tokens)
                    | ',' -> loop (at + 1) (Comma at :: tokens)
                    | _ when isAsciiLetter character ->
                        let name, nextAt = readName input at
                        loop nextAt (Name(name, at) :: tokens)
                    | _ when isNumberStart character ->
                        let raw, nextAt = readNumber input at
                        match parseFiniteNumber raw with
                        | Some number -> loop nextAt (Number(number, at) :: tokens)
                        | None -> Error(LocatedError(InvalidNumber raw, at))
                    | _ ->
                        let token = Char.ConvertFromUtf32(Char.ConvertToUtf32(input, at))
                        Error(LocatedError(UnexpectedToken token, at))
        loop 0 []

    /// Parse an SVG transform attribute into an affine matrix.
    /// Empty strings parse as the identity matrix.
    let attribute input =
        tokenize input
        |> Result.bind (fun tokens ->
            parseTransforms (dropWhitespace tokens) (Transform.identity ()) input.Length)
        |> Result.mapError (fun (LocatedError(reason, at)) ->
            ParseError(reason, input.Substring at))
