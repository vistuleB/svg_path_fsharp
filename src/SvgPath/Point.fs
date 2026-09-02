namespace SvgPath

[<Struct>]
type Point<[<Measure>] 'Unit> =
    { X: float<'Unit>
      Y: float<'Unit> }

[<RequireQualifiedAccess>]
module Point =
    let create (x: float<'Unit>) (y: float<'Unit>) : Point<'Unit> = { X = x; Y = y }

    let right: Point<1> = create 1.0 0.0
    let left: Point<1> = create -1.0 0.0

    // SVG's positive Y axis points down on the displayed page.
    let up: Point<1> = create 0.0 -1.0
    let down: Point<1> = create 0.0 1.0

    /// Return the unit coordinate pair pointing at a clockwise SVG angle.
    let direction (degrees: float<degree>) : Point<1> =
        create (Trig.cosDegrees degrees) (Trig.sinDegrees degrees)

    /// Return the clockwise heading in the range [0, 360).
    /// The zero pair has heading zero.
    let heading (point: Point<'Unit>) : float<degree> =
        let raw = Trig.atan2Degrees point.Y point.X
        if raw < 0.0<degree> then raw + Degree.fromFloat 360.0 else raw

    /// Return the clockwise aperture from one coordinate pair to another in [0, 360).
    let clockwiseAperture (fromPoint: Point<'From>) (toPoint: Point<'To>) : float<degree> =
        let difference = Degree.toFloat (heading toPoint) - Degree.toFloat (heading fromPoint)
        Degree.fromFloat (if difference < 0.0 then difference + 360.0 else difference)

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

    let normalize (point: Point<'Unit>) : Point<1> option =
        let magnitude = norm point
        if float magnitude = 0.0 then None else Some(create (point.X / magnitude) (point.Y / magnitude))

    let project (point: Point<'Projected>) (onto: Point<'Onto>) : Point<'Projected> option =
        let denominator = squaredNorm onto
        if float denominator = 0.0 then None else Some(scale (dot point onto / denominator) onto)

    let scalarProjection (point: Point<'Projected>) (onto: Point<'Onto>) : float<'Projected> option =
        let magnitude = norm onto
        if float magnitude = 0.0 then None else Some(dot point onto / magnitude)

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

    let interpolate (startPoint: Point<'Unit>) (endPoint: Point<'Unit>) (t: float<parameter>) : Point<'Unit> =
        displacement startPoint endPoint
        |> scale (Parameter.ratio t)
        |> fun offset -> translate offset startPoint

    let midpoint (left: Point<'Unit>) (right: Point<'Unit>) : Point<'Unit> =
        interpolate left right (Parameter.fromFloat 0.5)

    /// Test Euclidean nearness. Negative, infinite, and NaN tolerances are rejected.
    let near (tolerance: float<'Unit>) (left: Point<'Unit>) (right: Point<'Unit>) : bool =
        let rawTolerance = float tolerance
        rawTolerance >= 0.0
        && System.Double.IsFinite rawTolerance
        && squaredDistance left right <= tolerance * tolerance
