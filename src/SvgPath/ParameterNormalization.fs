namespace SvgPath

[<RequireQualifiedAccess>]
module internal ParameterNormalization =
    let normalizeSplits points =
        points
        |> List.map InternalNumber.normalizeZero
        |> List.distinct
        |> List.sort
        |> List.skipWhile ((=) (0.0<parameter>))
        |> List.rev
        |> List.skipWhile ((=) (1.0<parameter>))
        |> List.rev
