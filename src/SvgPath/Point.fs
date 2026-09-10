namespace SvgPath

[<Struct>]
type Point<[<Measure>] 'Unit> =
    { X: float<'Unit>
      Y: float<'Unit> }

[<RequireQualifiedAccess>]
module Point =
    let create (x: float<'Unit>) (y: float<'Unit>) : Point<'Unit> = { X = x; Y = y }

    /// The zero vector, usable with any coordinate unit.
    let zero<[<Measure>] 'Unit> : Point<'Unit> = { X = 0.0<_>; Y = 0.0<_> }

    let right: Point<1> = create 1.0 0.0
    let left: Point<1> = create -1.0 0.0

    // SVG's positive Y axis points down on the displayed page.
    let up: Point<1> = create 0.0 -1.0
    let down: Point<1> = create 0.0 1.0

    /// Return the unit coordinate pair pointing at a clockwise SVG angle.
    let direction (degrees: float<degree>) : Point<1> =
        create (Trig.cosDegrees degrees) (Trig.sinDegrees degrees)

    // Adding 360 to a representable negative angle can round to exactly 360,
    // where Float spacing is coarser. Canonicalize after the final arithmetic;
    // no epsilon is used, so representable values below 360 are unchanged.
    let private canonicalTurnEndpoint degrees =
        if degrees >= 360.0<degree> then 0.0<degree> else degrees

    /// Return the clockwise heading in the range [0, 360).
    /// The zero pair has heading zero.
    let heading (point: Point<'Unit>) : float<degree> =
        if InternalNumber.isZero point.X && InternalNumber.isZero point.Y then 0.0<degree>
        else
            let raw = Trig.atan2Degrees point.Y point.X
            let turns = floor (raw / 360.0<degree>)
            let normalized = raw - turns * 360.0<degree>
            (if normalized < 0.0<degree> then normalized + 360.0<degree> else normalized)
            |> canonicalTurnEndpoint

    /// Return the clockwise aperture from one coordinate pair to another in [0, 360).
    /// A zero vector is treated as pointing right when comparing headings.
    let clockwiseAperture (fromPoint: Point<'From>) (toPoint: Point<'To>) : float<degree> =
        let difference = Degree.toFloat (heading toPoint) - Degree.toFloat (heading fromPoint)
        Degree.fromFloat (if difference < 0.0 then difference + 360.0 else difference)
        |> canonicalTurnEndpoint

    let add (left: Point<'Unit>) (right: Point<'Unit>) : Point<'Unit> =
        create (left.X + right.X) (left.Y + right.Y)

    let subtract (left: Point<'Unit>) (right: Point<'Unit>) : Point<'Unit> =
        create (left.X - right.X) (left.Y - right.Y)

    let negate (point: Point<'Unit>) : Point<'Unit> = create -point.X -point.Y

    let scale (factor: float<'Factor>) (point: Point<'Unit>) : Point<'Factor * 'Unit> =
        create (factor * point.X) (factor * point.Y)

    let dot (left: Point<'Left>) (right: Point<'Right>) : float<'Left * 'Right> =
        left.X * right.X + left.Y * right.Y

    let cross (left: Point<'Left>) (right: Point<'Right>) : float<'Left * 'Right> =
        left.X * right.Y - left.Y * right.X

    let squaredNorm (point: Point<'Unit>) : float<'Unit^2> = dot point point

    /// Uses hypot rather than sqrt(x*x + y*y) to avoid intermediate overflow.
    let norm (point: Point<'Unit>) : float<'Unit> =
        InternalNumber.hypot (float point.X) (float point.Y)
        |> LanguagePrimitives.FloatWithMeasure<'Unit>

    /// Normalize finite nonzero vectors after rescaling to avoid overflow and
    /// underflow. A zero vector returns None. Divide coordinates directly:
    /// the reciprocal of a subnormal scale can itself overflow.
    let normalize (point: Point<'Unit>) : Point<1> option =
        let largest = max (abs point.X) (abs point.Y)
        if InternalNumber.isZero largest then None
        else
            let scaled = create (point.X / largest) (point.Y / largest)
            let magnitude = norm scaled
            Some(create (scaled.X / magnitude) (scaled.Y / magnitude))

    /// Project onto a vector; None for zero target or nonfinite output.
    let project (point: Point<'Projected>) (onto: Point<'Onto>) : Point<'Projected> option =
        normalize onto |> Option.bind (fun unit ->
            let xContribution,yContribution = point.X * unit.X, point.Y * unit.Y
            // Distribute before summing: the scalar may overflow even when
            // both vector coordinates are representable. Do not rescale source.
            match InternalNumber.checkedSum (xContribution * unit.X) (yContribution * unit.X),
                  InternalNumber.checkedSum (xContribution * unit.Y) (yContribution * unit.Y) with
            | Ok x,Ok y -> Some(create x y)
            | _ -> None)

    /// Scalar projection; None for zero target or nonfinite output.
    let scalarProjection (point: Point<'Projected>) (onto: Point<'Onto>) : float<'Projected> option =
        normalize onto |> Option.bind (fun unit ->
            InternalNumber.checkedSum (point.X * unit.X) (point.Y * unit.Y) |> Result.toOption)

    /// Rotate by 90 degrees clockwise in displayed SVG coordinates.
    let rotateClockwise (point: Point<'Unit>) : Point<'Unit> = create -point.Y point.X

    /// Rotate by 90 degrees counterclockwise in displayed SVG coordinates.
    let rotateCounterclockwise (point: Point<'Unit>) : Point<'Unit> = create point.Y -point.X

    let displacement (fromPoint: Point<'Unit>) (toPoint: Point<'Unit>) : Point<'Unit> =
        subtract toPoint fromPoint

    let translate (offset: Point<'Unit>) (point: Point<'Unit>) : Point<'Unit> = add point offset

    let squaredDistance (left: Point<'Unit>) (right: Point<'Unit>) : float<'Unit^2> =
        displacement left right |> squaredNorm

    /// Uses the overflow-resistant norm.
    let distance (left: Point<'Unit>) (right: Point<'Unit>) : float<'Unit> =
        displacement left right |> norm

    let private interpolateCoordinate a b t =
        // Opposite-sign endpoints can overflow b-a, while weighted interior
        // terms and their opposite-sign sum remain representable.
        if t > 0.0 && t < 1.0 && ((a < 0.0<_> && b > 0.0<_>) || (a > 0.0<_> && b < 0.0<_>)) then
            a * (1.0 - t) + b * t
        else a + (b - a) * t

    let interpolate (startPoint: Point<'Unit>) (endPoint: Point<'Unit>) (t: float<parameter>) : Point<'Unit> =
        // Preserve supplied endpoints without cancellation or overflowing b-a.
        if InternalNumber.isZero t then startPoint
        elif t = 1.0<parameter> then endPoint
        else
            create (interpolateCoordinate startPoint.X endPoint.X (Parameter.ratio t))
                   (interpolateCoordinate startPoint.Y endPoint.Y (Parameter.ratio t))

    let midpoint (left: Point<'Unit>) (right: Point<'Unit>) : Point<'Unit> =
        interpolate left right (Parameter.fromFloat 0.5)

    /// Test Euclidean nearness. Negative, infinite, and NaN tolerances are rejected.
    let near (tolerance: float<'Unit>) (left: Point<'Unit>) (right: Point<'Unit>) : bool =
        let rawTolerance = float tolerance
        if rawTolerance < 0.0 || not (System.Double.IsFinite rawTolerance) then false
        else
            match InternalNumber.checkedSum left.X -right.X, InternalNumber.checkedSum left.Y -right.Y with
            | Ok dx,Ok dy ->
                if InternalNumber.isZero tolerance then InternalNumber.isZero dx && InternalNumber.isZero dy
                else
                    abs dx <= tolerance && abs dy <= tolerance
                    && (let x,y = dx / tolerance,dy / tolerance
                        x*x + y*y <= 1.0)
            | _ -> false
