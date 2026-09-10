namespace SvgPath

/// Invalid curvature arguments, underlying path failures, and undefined geometry.
type CurvatureError =
    | CurvaturePathError of error: SegmentError
    | InvalidCurvatureTolerance of tolerance: float<parameter>
    | InvalidCurvatureSamples of samples: int
    | InvalidCurvatureMaxDepth of maxDepth: int
    | InvalidCurvatureMargin of margin: float<length>
    | DegenerateCurvatureDerivative
    | InfiniteRadiusOfCurvature

/// Options for sampled cusp/root/band discovery. Discovery functions validate
/// every field. Band discovery uses only Samples; algebraic inflection discovery
/// uses none of the fields after validation.
[<Struct>]
type CurvatureOptions =
    { Tolerance: float<parameter>
      Samples: int
      MaxDepth: int }

/// First and second parameter derivatives at a segment parameter.
[<Struct>]
type SegmentDerivatives =
    { First: Point<length / parameter>
      Second: Point<length / parameter^2> }

/// A sampled parameter interval where signed radius is close to a target offset.
[<Struct>]
type CurvatureBand =
    { From: float<parameter>
      To: float<parameter> }

/// Signed curvature, radius, inflection points, and offset-cusp diagnostics.
/// Signs refer to the visual left normal in SVG coordinates (positive y down).
/// Curvature has inverse-length units; radius, offsets, and margins have length
/// units. Parameters refer to the segment's 0..1 interval.
/// Pointwise queries evaluate derivatives directly. Cusp-parameter and near-radius
/// band discovery use sampling and are not exhaustive root or interval solvers.
[<RequireQualifiedAccess>]
module Curvature =
    /// Default sampling and refinement options.
    let defaultOptions =
        { Tolerance = 1.0e-9<parameter>
          Samples = 100
          MaxDepth = 32 }

    let private parameter value = Parameter.fromFloat value

    let private validateOptions options =
        if options.Tolerance < 0.0<parameter>
           || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidCurvatureTolerance options.Tolerance)
        elif options.Samples <= 0 then Error(InvalidCurvatureSamples options.Samples)
        elif options.MaxDepth <= 0 then Error(InvalidCurvatureMaxDepth options.MaxDepth)
        else Ok()

    /// Return first and second parameter derivatives. Lines have zero second
    /// derivative; arcs use exact ellipse derivatives.
    let segmentDerivatives segment t =
        match Segment.derivative segment t, Segment.secondDerivative segment t with
        | Ok first, Ok second -> Ok { First = first; Second = second }
        | Error error, _
        | _, Error error -> Error error

    let private leftNormalCurvatureFromDerivatives data : Result<float<1 / length>, CurvatureError> =
        let speedSquared = Point.dot data.First data.First
        if speedSquared <= 0.0<length^2 / parameter^2>
           || not (System.Double.IsFinite(float speedSquared)) then Error DegenerateCurvatureDerivative
        else
            let speed = sqrt (float speedSquared) * 1.0<length / parameter>
            Ok(-Point.cross data.First data.Second / (speedSquared * speed))

    /// Signed curvature, positive toward the visual left of the tangent and
    /// negative toward the visual right. Lines return zero; zero-speed parameters
    /// return DegenerateCurvatureDerivative.
    let segmentLeftNormalCurvature segment t =
        segmentDerivatives segment t
        |> Result.mapError CurvaturePathError
        |> Result.bind leftNormalCurvatureFromDerivatives

    /// Signed visual-left radius. Lines and inflection points return
    /// InfiniteRadiusOfCurvature; zero-speed parameters return
    /// DegenerateCurvatureDerivative.
    let segmentLeftNormalRadius segment t : Result<float<length>, CurvatureError> =
        segmentLeftNormalCurvature segment t
        |> Result.bind (fun curvature -> if InternalNumber.isZero curvature then Error InfiniteRadiusOfCurvature else Ok(1.0 / curvature))

    let private cuspResidualFromDerivatives data (offset: float<length>) =
        let speedSquared = Point.dot data.First data.First
        if speedSquared <= 0.0<length^2 / parameter^2>
           || not (System.Double.IsFinite(float speedSquared)) then Error DegenerateCurvatureDerivative
        else
            let speed = sqrt (float speedSquared) * 1.0<length / parameter>
            Ok(speedSquared * speed + offset * Point.cross data.First data.Second)

    /// Return |p'|^3 + offset * cross(p', p''). Zero means the signed visual-left
    /// radius equals offset, assuming finite nonzero curvature. Its magnitude
    /// depends on parameter speed and is not a geometric distance error.
    /// Units are length^3 / parameter^3. Lines return |p'|^3; zero-speed
    /// parameters return DegenerateCurvatureDerivative.
    let segmentLeftNormalCuspResidual segment offset t =
        segmentDerivatives segment t
        |> Result.mapError CurvaturePathError
        |> Result.bind (fun data -> cuspResidualFromDerivatives data offset)

    /// Test abs(R_left(t) - offset) < margin without dividing by curvature.
    /// For finite nonzero curvature, this is equivalent to
    /// abs(|p'|^3 + offset * cross(p', p'')) < margin * abs(cross(p', p'')).
    let segmentLeftNormalRadiusCloseTo segment offset margin t =
        if margin < 0.0<length> || not (System.Double.IsFinite(float margin)) then Error(InvalidCurvatureMargin margin)
        else
            segmentDerivatives segment t
            |> Result.mapError CurvaturePathError
            |> Result.bind (fun data ->
                let speedSquared = Point.dot data.First data.First
                let cross = Point.cross data.First data.Second
                if speedSquared <= 0.0<length^2 / parameter^2>
                   || not (System.Double.IsFinite(float speedSquared)) then Error DegenerateCurvatureDerivative
                elif InternalNumber.isZero cross then Error InfiniteRadiusOfCurvature
                else
                    let speed = sqrt (float speedSquared) * 1.0<length / parameter>
                    Ok(abs (speedSquared * speed + offset * cross) < margin * abs cross))

    let inline private signChange a b = (a < 0.0<_> && b > 0.0<_>) || (a > 0.0<_> && b < 0.0<_>)

    let rec private refineRoot
        (f: float<parameter> -> Result<float<'Unit>, CurvatureError>)
        (a: float<parameter>)
        (b: float<parameter>)
        (va: float<'Unit>)
        (vb: float<'Unit>)
        options
        depth
        : Result<float<parameter>, CurvatureError> =
        if depth >= options.MaxDepth || abs (b - a) <= options.Tolerance then Ok((a + b) / 2.0)
        else
            let midpoint = (a + b) / 2.0
            match f midpoint with
            | Error error -> Error error
            | Ok vm when InternalNumber.isZero vm -> Ok midpoint
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
        | Error error -> Error error
        | Ok _ ->
            [ 0 .. options.Samples - 1 ]
            |> List.fold (fun roots index ->
                let a = parameter (float index / float options.Samples)
                let b = parameter (float (index + 1) / float options.Samples)
                match f a, f b with
                | Ok va, Ok _ when InternalNumber.isZero va -> a :: roots
                | Ok va, Ok vb when signChange va vb ->
                    match refineRoot f a b va vb options 0 with
                    | Ok root -> root :: roots
                    | Error _ -> roots
                | _, Ok vb when index = options.Samples - 1 && InternalNumber.isZero vb -> b :: roots
                | _ -> roots) []
            |> uniqueSorted options.Tolerance
            |> Ok

    /// Sample the cusp residual on a uniform grid and bisect sign-changing
    /// windows. Exact sampled zeros are included. Multiple roots within one
    /// window and non-sign-changing roots between samples may be missed.
    /// Failed evaluations or bisections can cause roots to be omitted.
    /// At MaxDepth, return the current midpoint without an accuracy guarantee.
    /// Results are sorted and merged within Tolerance in parameter space.
    let segmentLeftNormalCuspParameters segment offset options =
        sampledRoots (segmentLeftNormalCuspResidual segment offset) options

    /// Return algebraically computed interior roots of cross(p', p'') = 0.
    /// Cubics use the Bezier inflection solver; lines, quadratics, arcs, and
    /// identically flat pieces return an empty list. Endpoint roots are excluded.
    /// Options are validated but do not affect the algebraic solve.
    let segmentInflectionParameters segment options =
        match validateOptions options with
        | Error error -> Error error
        | Ok _ ->
            match segment with
            | Line _
            | QuadraticBezier _
            | Arc _ -> Ok []
            | CubicBezier(startPoint, control1, control2, endPoint) ->
                CubicBezierData(startPoint, control1, control2, endPoint)
                |> Bezier.cubicInflectionParameters
                |> Ok

    /// Merge adjacent close samples on a uniform grid into parameter bands.
    /// A band starts at its first close sample and ends at the first subsequent
    /// non-close sample (or 1). Evaluation errors count as non-close samples.
    /// Bands are approximate: narrow intervals may be missed, and not every point
    /// inside a returned band is guaranteed to satisfy the predicate.
    let segmentLeftNormalRadiusCloseBands segment offset margin options =
        match validateOptions options with
        | Error error -> Error error
        | Ok _ ->
            if margin < 0.0<length> || not (System.Double.IsFinite(float margin)) then Error(InvalidCurvatureMargin margin)
            else
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
