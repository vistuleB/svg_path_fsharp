namespace SvgPath

[<Struct>]
type Vector<[<Measure>] 'Unit> =
    { X: float<'Unit>
      Y: float<'Unit> }

[<Struct>]
type Point =
    { X: float<length>
      Y: float<length> }

[<RequireQualifiedAccess>]
module Vector =
    let create (x: float<'Unit>) (y: float<'Unit>) : Vector<'Unit> = { X = x; Y = y }

    let right: Vector<1> = create 1.0 0.0
    let left: Vector<1> = create -1.0 0.0

    // SVG's positive Y axis points down on the displayed page.
    let up: Vector<1> = create 0.0 -1.0
    let down: Vector<1> = create 0.0 1.0

    /// Return the unit vector pointing at a clockwise SVG angle.
    let direction (degrees: float<degree>) : Vector<1> =
        create (Trig.cosDegrees degrees) (Trig.sinDegrees degrees)

    /// Return the clockwise SVG heading in the range [0, 360).
    /// The zero vector has heading zero.
    let heading (vector: Vector<'Unit>) : float<degree> =
        let raw = Trig.atan2Degrees vector.Y vector.X
        let normalized = if raw < 0.0<degree> then raw + Degree.fromFloat 360.0 else raw
        normalized

    /// Return the clockwise aperture from one vector to another in [0, 360).
    let clockwiseAperture (fromVector: Vector<'From>) (toVector: Vector<'To>) : float<degree> =
        let difference = Degree.toFloat (heading toVector) - Degree.toFloat (heading fromVector)
        Degree.fromFloat (if difference < 0.0 then difference + 360.0 else difference)

    let add (left: Vector<'Unit>) (right: Vector<'Unit>) : Vector<'Unit> =
        create (left.X + right.X) (left.Y + right.Y)

    let subtract (left: Vector<'Unit>) (right: Vector<'Unit>) : Vector<'Unit> =
        create (left.X - right.X) (left.Y - right.Y)

    let negate (vector: Vector<'Unit>) : Vector<'Unit> = create -vector.X -vector.Y

    let scale (factor: float<'Factor>) (vector: Vector<'Unit>) : Vector<'Factor * 'Unit> =
        create (factor * vector.X) (factor * vector.Y)

    let dot (left: Vector<'Left>) (right: Vector<'Right>) : float<'Left * 'Right> =
        left.X * right.X + left.Y * right.Y

    let cross (left: Vector<'Left>) (right: Vector<'Right>) : float<'Left * 'Right> =
        left.X * right.Y - left.Y * right.X

    let squaredNorm (vector: Vector<'Unit>) : float<'Unit^2> = dot vector vector

    /// Uses hypot rather than sqrt(x*x + y*y) to avoid intermediate overflow.
    let norm (vector: Vector<'Unit>) : float<'Unit> =
        let absoluteX = abs (float vector.X)
        let absoluteY = abs (float vector.Y)
        let larger = max absoluteX absoluteY
        let smaller = min absoluteX absoluteY

        let magnitude =
            if System.Double.IsInfinity larger then
                infinity
            elif larger = 0.0 then
                0.0
            else
                let ratio = smaller / larger
                larger * sqrt (1.0 + ratio * ratio)

        LanguagePrimitives.FloatWithMeasure<'Unit> magnitude

    let normalize (vector: Vector<'Unit>) : Vector<1> option =
        let magnitude = norm vector

        if float magnitude = 0.0 then
            None
        else
            Some(create (vector.X / magnitude) (vector.Y / magnitude))

    let project (vector: Vector<'Projected>) (onto: Vector<'Onto>) : Vector<'Projected> option =
        let denominator = squaredNorm onto

        if float denominator = 0.0 then
            None
        else
            Some(scale (dot vector onto / denominator) onto)

    let scalarProjection
        (vector: Vector<'Projected>)
        (onto: Vector<'Onto>)
        : float<'Projected> option =
        let magnitude = norm onto

        if float magnitude = 0.0 then
            None
        else
            Some(dot vector onto / magnitude)

    /// Rotate by 90 degrees clockwise in displayed SVG coordinates.
    let rotateClockwise (vector: Vector<'Unit>) : Vector<'Unit> =
        create -vector.Y vector.X

    /// Rotate by 90 degrees counterclockwise in displayed SVG coordinates.
    let rotateCounterclockwise (vector: Vector<'Unit>) : Vector<'Unit> =
        create vector.Y -vector.X

[<RequireQualifiedAccess>]
module Point =
    let create (x: float<length>) (y: float<length>) : Point = { X = x; Y = y }

    let displacement (fromPoint: Point) (toPoint: Point) : Vector<length> =
        Vector.create (toPoint.X - fromPoint.X) (toPoint.Y - fromPoint.Y)

    let translate (vector: Vector<length>) (point: Point) : Point =
        { X = point.X + vector.X
          Y = point.Y + vector.Y }

    let squaredDistance (left: Point) (right: Point) : float<length^2> =
        displacement left right |> Vector.squaredNorm

    /// Uses the overflow-resistant vector norm.
    let distance (left: Point) (right: Point) : float<length> =
        displacement left right |> Vector.norm

    let interpolate (startPoint: Point) (endPoint: Point) (t: float<parameter>) : Point =
        displacement startPoint endPoint
        |> Vector.scale (Parameter.ratio t)
        |> fun offset -> translate offset startPoint

    let midpoint (left: Point) (right: Point) : Point =
        interpolate left right (Parameter.fromFloat 0.5)

    /// Test Euclidean nearness. Negative, infinite, and NaN tolerances are rejected.
    let near (tolerance: float<length>) (left: Point) (right: Point) : bool =
        let rawTolerance = Length.toFloat tolerance

        rawTolerance >= 0.0
        && System.Double.IsFinite rawTolerance
        && squaredDistance left right <= tolerance * tolerance
