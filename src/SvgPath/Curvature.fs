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
    | CurvatureRootIsolationFailed
    | CurvatureMaxDepthReached of lower: float<parameter> * upper: float<parameter>

/// Options for cusp/root/band discovery. Discovery functions validate
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
/// Pointwise queries evaluate derivatives directly. Cusp discovery partitions at
/// curvature extrema before bisection; near-radius band discovery stays sampled.
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
        let midpoint = (a + b) / 2.0
        match f midpoint with
        | Error error -> Error error
        | Ok vm when InternalNumber.isZero vm || abs (b-a) <= options.Tolerance -> Ok midpoint
        | Ok _ when depth >= options.MaxDepth -> Error(CurvatureMaxDepthReached(a,b))
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

    let private cuspRelativeTolerance = 1e-12
    let private rootParameterTolerance = 1e-9<parameter>

    let private polynomialRoots coefficients =
        Root.polynomialRootsWith (List.rev coefficients) 0.0<parameter> 1.0<parameter> (Root.defaultPolynomialOptions())
        |> Result.mapError (fun _ -> CurvatureRootIsolationFailed)

    let private coefficientScale values = values |> List.fold (fun sum value -> sum + abs value) 0.0<_>
    let private polynomialDerivative values = values |> List.rev |> Root.polynomialDerivative |> List.rev
    let private polynomialScale (values: float<'a> list) (factor: float<'b>): float<'a * 'b> list =
        values |> List.map (fun value -> value * factor)
    let rec private polynomialAdd (a: float<'a> list) (b: float<'a> list): float<'a> list =
        match a,b with
        | [],_ -> b
        | _,[] -> a
        | x::xs,y::ys -> (x+y)::polynomialAdd xs ys
    let rec private polynomialMultiply<[<Measure>] 'a, [<Measure>] 'b> (a: float<'a> list) (b: float<'b> list): float<'a * 'b> list =
        match a with
        | [] -> []
        | x::xs -> polynomialAdd (polynomialScale b x) (0.0<_>::polynomialMultiply<'a,'b> xs b)

    let private relativeCuspResidual (first: Point<length/parameter>) (second: Point<length/parameter^2>) (offset: float<length>) =
        let speedSquared = Point.dot first first
        let speed = sqrt speedSquared
        let speedCubed = speedSquared * speed
        let term = offset * Point.cross first second
        let scale = speedCubed + abs term
        if InternalNumber.isZero scale then 1.0 else (speedCubed + term) / scale

    let private partitionedCuspRoots f parameters stationary options =
        let parameters = uniqueSorted 0.0<parameter> (0.0<parameter>::1.0<parameter>::parameters)
        let values = parameters |> List.map (fun t ->
            let value = f t
            t,(if not(List.contains t stationary) && abs value <= cuspRelativeTolerance then 0.0 else value))
        let roots = values |> List.choose (fun (t,value) -> if InternalNumber.isZero value then Some t else None)
        let rec crossings values roots =
            match values with
            | (a,va)::((b,vb) as next)::rest ->
                let result =
                    if signChange va vb then refineRoot (f >> Ok) a b va vb options 0 |> Result.map (fun t -> t::roots)
                    else Ok roots
                result |> Result.bind (crossings (next::rest))
            | _ -> Ok roots
        crossings values roots |> Result.map (uniqueSorted options.Tolerance)

    // Coefficients use the dimensionless numeric parameter coordinate, as in
    // root.gleam. Restore derivative units at residual evaluation boundaries.
    let private polynomialCusps (v0: Point<length>) (v1: Point<length>) (v2: Point<length>) offset options =
        let c = [Point.cross v0 v1; 2.0 * Point.cross v0 v2; Point.cross v1 v2]
        if List.forall InternalNumber.isZero c then Ok []
        else
            let q = [Point.dot v0 v0; 2.0 * Point.dot v0 v1; Point.dot v1 v1 + 2.0 * Point.dot v0 v2; 2.0 * Point.dot v1 v2; Point.dot v2 v2]
            let extremaPolynomial =
                polynomialAdd (polynomialScale (polynomialMultiply (polynomialDerivative c) q) 2.0)
                              (polynomialScale (polynomialMultiply c (polynomialDerivative q)) -3.0)
            polynomialRoots extremaPolynomial |> Result.bind (fun extrema ->
                let xs,ys = [v0.X;v1.X;v2.X],[v0.Y;v1.Y;v2.Y]
                polynomialRoots xs |> Result.bind (fun xRoots ->
                    polynomialRoots ys |> Result.bind (fun yRoots ->
                        let velocity t = Point.add v0 (Point.scale (Parameter.ratio t) (Point.add v1 (Point.scale (Parameter.ratio t) v2)))
                        let stationary =
                            (xRoots@yRoots) |> List.filter (fun t ->
                                let v = velocity t
                                abs v.X <= cuspRelativeTolerance * coefficientScale xs && abs v.Y <= cuspRelativeTolerance * coefficientScale ys)
                            |> uniqueSorted rootParameterTolerance
                        let evaluate t =
                            if List.contains t stationary then
                                if offset * Point.cross v1 v2 < 0.0<_> then -1.0 else 1.0
                            else
                                relativeCuspResidual (Point.scale (1.0 / 1.0<parameter>) (velocity t))
                                    (Point.scale (1.0 / 1.0<parameter^2>) (Point.add v1 (Point.scale (2.0 * Parameter.ratio t) v2))) offset
                        let extrema = extrema |> List.filter (fun t -> not(stationary |> List.exists (fun zero -> abs(t-zero) <= rootParameterTolerance)))
                        partitionedCuspRoots evaluate (extrema@stationary) stationary options)))

    /// Partition Beziers at polynomial curvature extrema and zero-speed points;
    /// partition ellipses at their axes. Check touches with relative residual
    /// tolerance 1e-12 and bisect crossings. Samples is unused. Zero-speed points
    /// are excluded; lines/zero offsets return []; matching circles return [0;1].
    /// Completeness depends on isolation and floating-point accuracy. At MaxDepth
    /// return CurvatureMaxDepthReached with the remaining bracket unless an exact
    /// midpoint root or an interval within tolerance succeeds first.
    let segmentLeftNormalCuspParameters segment offset options =
        validateOptions options |> Result.bind (fun () ->
            match segment with
            | _ when InternalNumber.isZero offset -> Ok []
            | Line _ -> Ok []
            | QuadraticBezier(start,control,finish) ->
                let first,last = Point.scale 2.0 (Point.subtract control start),Point.scale 2.0 (Point.subtract finish control)
                polynomialCusps first (Point.subtract last first) (Point.create 0.0<length> 0.0<length>) offset options
            | CubicBezier(start,c1,c2,finish) ->
                let first,middle,last = Point.scale 3.0 (Point.subtract c1 start),Point.scale 3.0 (Point.subtract c2 c1),Point.scale 3.0 (Point.subtract finish c2)
                polynomialCusps first (Point.scale 2.0 (Point.subtract middle first)) (Point.add (Point.subtract last (Point.scale 2.0 middle)) first) offset options
            | Arc _ ->
                Segment.arcCenterData segment |> Result.mapError CurvaturePathError |> Result.bind (fun arc ->
                    let evaluate t = relativeCuspResidual (Ellipse.arcDerivative arc t) (Ellipse.arcSecondDerivative arc t) offset
                    if arc.Radius.X = arc.Radius.Y then
                        if abs(evaluate 0.5<parameter>) <= cuspRelativeTolerance then Ok [0.0<parameter>;1.0<parameter>] else Ok []
                    else
                        let cos,sin = Trig.cosDegrees arc.XAxisRotation,Trig.sinDegrees arc.XAxisRotation
                        let parameters = Ellipse.arcProjectionExtrema arc (Point.create cos sin) @ Ellipse.arcProjectionExtrema arc (Point.create -sin cos)
                        partitionedCuspRoots evaluate parameters [] options))

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
