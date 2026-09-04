namespace SvgPath

[<Struct>]
type CurvatureOptions =
    { Tolerance: float<parameter>
      Samples: int
      MaxDepth: int }

[<Struct>]
type SegmentDerivatives =
    { First: Point<length / parameter>
      Second: Point<length / parameter^2> }

[<Struct>]
type CurvatureBand =
    { From: float<parameter>
      To: float<parameter> }

/// Derivatives, visual-left-normal curvature, and curvature-event parameters.
[<RequireQualifiedAccess>]
module Curvature =
    let defaultOptions =
        { Tolerance = 1.0e-9<parameter>
          Samples = 100
          MaxDepth = 32 }

    let private parameter value = Parameter.fromFloat value

    let private validateOptions options =
        if options.Tolerance <= 0.0<parameter>
           || not (System.Double.IsFinite(float options.Tolerance))
           || options.Samples <= 0
           || options.MaxDepth <= 0 then Error()
        else Ok()

    let segmentDerivatives segment t =
        match Segment.derivative segment t, Segment.secondDerivative segment t with
        | Ok first, Ok second -> Ok { First = first; Second = second }
        | Error error, _
        | _, Error error -> Error error

    let private leftNormalCurvatureFromDerivatives data : Result<float<1 / length>, unit> =
        let speedSquared = Point.dot data.First data.First
        if speedSquared <= 0.0<length^2 / parameter^2>
           || not (System.Double.IsFinite(float speedSquared)) then Error()
        else
            let speed = sqrt (float speedSquared) * 1.0<length / parameter>
            Ok(-Point.cross data.First data.Second / (speedSquared * speed))

    let segmentLeftNormalCurvature segment t =
        segmentDerivatives segment t
        |> Result.mapError ignore
        |> Result.bind leftNormalCurvatureFromDerivatives

    let segmentLeftNormalRadius segment t : Result<float<length>, unit> =
        segmentLeftNormalCurvature segment t
        |> Result.bind (fun curvature -> if curvature = 0.0<1 / length> then Error() else Ok(1.0 / curvature))

    let private cuspResidualFromDerivatives data (offset: float<length>) =
        let speedSquared = Point.dot data.First data.First
        if speedSquared <= 0.0<length^2 / parameter^2>
           || not (System.Double.IsFinite(float speedSquared)) then Error()
        else
            let speed = sqrt (float speedSquared) * 1.0<length / parameter>
            Ok(speedSquared * speed + offset * Point.cross data.First data.Second)

    let segmentLeftNormalCuspResidual segment offset t =
        segmentDerivatives segment t
        |> Result.mapError ignore
        |> Result.bind (fun data -> cuspResidualFromDerivatives data offset)

    let segmentLeftNormalRadiusCloseTo segment offset margin t =
        if margin < 0.0<length> || not (System.Double.IsFinite(float margin)) then Error()
        else
            segmentDerivatives segment t
            |> Result.mapError ignore
            |> Result.bind (fun data ->
                let speedSquared = Point.dot data.First data.First
                let cross = Point.cross data.First data.Second
                if speedSquared <= 0.0<length^2 / parameter^2>
                   || cross = 0.0<length^2 / parameter^3>
                   || not (System.Double.IsFinite(float speedSquared)) then Error()
                else
                    let speed = sqrt (float speedSquared) * 1.0<length / parameter>
                    Ok(abs (speedSquared * speed + offset * cross) < margin * abs cross))

    let inline private signChange a b = (a < 0.0<_> && b > 0.0<_>) || (a > 0.0<_> && b < 0.0<_>)

    let rec private refineRoot
        (f: float<parameter> -> Result<float<'Unit>, unit>)
        (a: float<parameter>)
        (b: float<parameter>)
        (va: float<'Unit>)
        (vb: float<'Unit>)
        options
        depth
        : Result<float<parameter>, unit> =
        if depth >= options.MaxDepth || abs (b - a) <= options.Tolerance then Ok((a + b) / 2.0)
        else
            let midpoint = (a + b) / 2.0
            match f midpoint with
            | Error _ -> Error()
            | Ok vm when vm = 0.0<_> -> Ok midpoint
            | Ok vm when signChange va vm -> refineRoot f a midpoint va vm options (depth + 1)
            | Ok vm when signChange vm vb -> refineRoot f midpoint b vm vb options (depth + 1)
            | Ok _ -> Ok midpoint

    let private uniqueSorted tolerance values =
        values
        |> List.filter (fun value -> value >= parameter 0.0 && value <= parameter 1.0)
        |> List.sort
        |> List.fold (fun kept value ->
            match kept with
            | previous :: _ when abs (value - previous) <= tolerance -> kept
            | _ -> value :: kept) []
        |> List.rev

    let private sampledRoots f options =
        match validateOptions options with
        | Error _ -> Error()
        | Ok _ ->
            [ 0 .. options.Samples - 1 ]
            |> List.fold (fun roots index ->
                let a = parameter (float index / float options.Samples)
                let b = parameter (float (index + 1) / float options.Samples)
                match f a, f b with
                | Ok va, Ok _ when va = 0.0<_> -> a :: roots
                | Ok va, Ok vb when signChange va vb ->
                    match refineRoot f a b va vb options 0 with
                    | Ok root -> root :: roots
                    | Error _ -> roots
                | _, Ok vb when index = options.Samples - 1 && vb = 0.0<_> -> b :: roots
                | _ -> roots) []
            |> uniqueSorted options.Tolerance
            |> Ok

    let segmentLeftNormalCuspParameters segment offset options =
        sampledRoots (segmentLeftNormalCuspResidual segment offset) options

    let segmentInflectionParameters segment options =
        match validateOptions options with
        | Error _ -> Error()
        | Ok _ ->
            match segment with
            | Line _
            | QuadraticBezier _
            | Arc _ -> Ok []
            | CubicBezier(startPoint, control1, control2, endPoint) ->
                CubicBezierData(startPoint, control1, control2, endPoint)
                |> Bezier.cubicInflectionParameters
                |> Ok

    let segmentLeftNormalRadiusCloseBands segment offset margin options =
        if margin < 0.0<length> || not (System.Double.IsFinite(float margin)) then Error()
        else
            match validateOptions options with
            | Error _ -> Error()
            | Ok _ ->
                let samples =
                    [ 0 .. options.Samples ]
                    |> List.map (fun index ->
                        let t = parameter (float index / float options.Samples)
                        let close = segmentLeftNormalRadiusCloseTo segment offset margin t = Ok true
                        t, close)
                let bands, openStart =
                    samples
                    |> List.fold (fun (bands, openStart) (t, close) ->
                        match close, openStart with
                        | true, None -> bands, Some t
                        | true, Some _ -> bands, openStart
                        | false, Some start -> { From = start; To = t } :: bands, None
                        | false, None -> bands, None) ([], None)
                let bands =
                    match openStart with
                    | Some start -> { From = start; To = parameter 1.0 } :: bands
                    | None -> bands
                Ok(List.rev bands)
