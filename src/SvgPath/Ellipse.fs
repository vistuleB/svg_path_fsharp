namespace SvgPath

type EllipsePoint = Point<length>

[<Struct>]
type EllipseBoundingBox =
    { Min: EllipsePoint
      Max: EllipsePoint }

[<Struct>]
type EndpointArcData =
    { Start: EllipsePoint
      Radius: EllipsePoint
      XAxisRotation: float<degree>
      LargeArc: bool
      Sweep: bool
      End: EllipsePoint }

[<Struct>]
type CenterArcData =
    { Center: EllipsePoint
      Radius: EllipsePoint
      XAxisRotation: float<degree>
      StartAngle: float<degree>
      DeltaAngle: float<degree> }

[<Struct>]
type EllipseCubic =
    { Start: EllipsePoint
      Control1: EllipsePoint
      Control2: EllipsePoint
      End: EllipsePoint }

type EllipseError =
    | DegenerateInputArc
    | NotCollapsedToLine
    | SplitOutsideArc

[<RequireQualifiedAccess>]
module Ellipse =
    let private scalarTolerance = 1.0e-9
    let private parameterTolerance = 1.0e-9<parameter>
    let private degreeTolerance = 1.0e-9<degree>
    let private lengthTolerance = 1.0e-9<length>
    let private squaredLengthTolerance = 1.0e-18<length^2>
    let private quarterTurn = 90.0<degree>
    let private halfTurn = 180.0<degree>
    let private fullTurn = 360.0<degree>
    let private parameter value = Parameter.fromFloat value

    let private sqrtMeasured (value: float<'Unit^2>) : float<'Unit> =
        LanguagePrimitives.FloatWithMeasure<'Unit> (sqrt (float value))

    let private positiveRemainder (angle: float<degree>) =
        let remainder = angle % fullTurn
        if remainder < 0.0<degree> then remainder + fullTurn else remainder

    let private angleProgress angle startAngle deltaAngle =
        if deltaAngle >= 0.0<degree> then positiveRemainder (angle - startAngle)
        else positiveRemainder (startAngle - angle)

    let private angleInSweep angle startAngle deltaAngle =
        if deltaAngle >= 0.0<degree> then
            positiveRemainder (angle - startAngle) <= deltaAngle + degreeTolerance
        else
            positiveRemainder (startAngle - angle) <= -deltaAngle + degreeTolerance

    let private angleAtRaw arc (t: float<parameter>) =
        arc.StartAngle + Parameter.ratio t * arc.DeltaAngle

    let angleAt arc t = angleAtRaw arc t
    let arcEndAngle arc = arc.StartAngle + arc.DeltaAngle
    let arcLargeArc arc = abs arc.DeltaAngle > halfTurn
    let arcSweep arc = arc.DeltaAngle >= 0.0<degree>

    let private ellipsePoint arc angle =
        let cosPhi = Trig.cosDegrees arc.XAxisRotation
        let sinPhi = Trig.sinDegrees arc.XAxisRotation
        let x = arc.Radius.X * Trig.cosDegrees angle
        let y = arc.Radius.Y * Trig.sinDegrees angle
        Point.create
            (arc.Center.X + cosPhi * x - sinPhi * y)
            (arc.Center.Y + sinPhi * x + cosPhi * y)

    let private ellipseDerivativeRadians arc angle : Point<length / radian> =
        let cosPhi = Trig.cosDegrees arc.XAxisRotation
        let sinPhi = Trig.sinDegrees arc.XAxisRotation
        let x = -arc.Radius.X * Trig.sinDegrees angle
        let y = arc.Radius.Y * Trig.cosDegrees angle
        Point.create
            (LanguagePrimitives.FloatWithMeasure<length / radian> (float (cosPhi * x - sinPhi * y)))
            (LanguagePrimitives.FloatWithMeasure<length / radian> (float (sinPhi * x + cosPhi * y)))

    let private ellipseSecondDerivativeRadians arc angle : Point<length / radian^2> =
        let cosPhi = Trig.cosDegrees arc.XAxisRotation
        let sinPhi = Trig.sinDegrees arc.XAxisRotation
        let x = -arc.Radius.X * Trig.cosDegrees angle
        let y = -arc.Radius.Y * Trig.sinDegrees angle
        Point.create
            (LanguagePrimitives.FloatWithMeasure<length / radian^2> (float (cosPhi * x - sinPhi * y)))
            (LanguagePrimitives.FloatWithMeasure<length / radian^2> (float (sinPhi * x + cosPhi * y)))

    let arcPointAtAngle arc angle = ellipsePoint arc angle

    let arcDerivativeAtAngle arc angle : Point<length / degree> =
        let radiansPerDegree = LanguagePrimitives.FloatWithMeasure<radian / degree> (System.Math.PI / 180.0)
        ellipseDerivativeRadians arc angle |> Point.scale radiansPerDegree

    let arcPoint arc t = ellipsePoint arc (angleAtRaw arc t)

    let arcDerivative arc t : Point<length / parameter> =
        let degreesPerParameter =
            LanguagePrimitives.FloatWithMeasure<degree / parameter> (Degree.toFloat arc.DeltaAngle)
        arcDerivativeAtAngle arc (angleAtRaw arc t) |> Point.scale degreesPerParameter

    let arcSecondDerivative arc t : Point<length / parameter^2> =
        let radiansPerParameter =
            LanguagePrimitives.FloatWithMeasure<radian / parameter>
                (Degree.toFloat arc.DeltaAngle * System.Math.PI / 180.0)
        ellipseSecondDerivativeRadians arc (angleAtRaw arc t)
        |> Point.scale (radiansPerParameter * radiansPerParameter)

    let private vectorAngle (a: Point<'UnitA>) (b: Point<'UnitB>) =
        Trig.atan2Degrees (Point.cross a b) (Point.dot a b)

    let private centerPrime
        (rx: float<length>)
        (ry: float<length>)
        (x1p: float<length>)
        (y1p: float<length>)
        largeArc
        sweep
        : Point<length> =
        let numerator = rx * rx * ry * ry - rx * rx * y1p * y1p - ry * ry * x1p * x1p
        let denominator = rx * rx * y1p * y1p + ry * ry * x1p * x1p
        let sign = if largeArc = sweep then -1.0 else 1.0
        let coefficient = sign * sqrt (max 0.0 (float (numerator / denominator)))
        Point.create (coefficient * rx * y1p / ry) (-coefficient * ry * x1p / rx)

    let private sweptDeltaAngle startVector endVector sweep =
        let delta = vectorAngle startVector endVector
        if sweep && delta < 0.0<degree> then delta + fullTurn
        elif not sweep && delta > 0.0<degree> then delta - fullTurn
        else delta

    let private doEndpointToCenter
        (startPoint: EllipsePoint)
        (radius: EllipsePoint)
        (xAxisRotation: float<degree>)
        largeArc
        sweep
        (endPoint: EllipsePoint)
        : Result<CenterArcData, EllipseError> =
        let rx = abs radius.X
        let ry = abs radius.Y
        if rx <= lengthTolerance || ry <= lengthTolerance then
            Error DegenerateInputArc
        else
            let cosPhi = Trig.cosDegrees xAxisRotation
            let sinPhi = Trig.sinDegrees xAxisRotation
            let midpoint = Point.midpoint startPoint endPoint
            let halfDelta = Point.scale 0.5 (Point.subtract startPoint endPoint)
            let x1p = cosPhi * halfDelta.X + sinPhi * halfDelta.Y
            let y1p = -sinPhi * halfDelta.X + cosPhi * halfDelta.Y
            let radiusScale = max 1.0 (float (x1p * x1p / (rx * rx) + y1p * y1p / (ry * ry)))
            let scale = sqrt radiusScale
            let rx = scale * rx
            let ry = scale * ry
            let centerPrime = centerPrime rx ry x1p y1p largeArc sweep
            let center =
                Point.create
                    (cosPhi * centerPrime.X - sinPhi * centerPrime.Y + midpoint.X)
                    (sinPhi * centerPrime.X + cosPhi * centerPrime.Y + midpoint.Y)
            let startVector = Point.create ((x1p - centerPrime.X) / rx) ((y1p - centerPrime.Y) / ry)
            let endVector = Point.create ((-x1p - centerPrime.X) / rx) ((-y1p - centerPrime.Y) / ry)
            Ok
                { Center = center
                  Radius = Point.create rx ry
                  XAxisRotation = xAxisRotation
                  StartAngle = vectorAngle Point.right startVector
                  DeltaAngle = sweptDeltaAngle startVector endVector sweep }

    let endpointToCenter (data: EndpointArcData) =
        doEndpointToCenter data.Start data.Radius data.XAxisRotation data.LargeArc data.Sweep data.End

    let centerToEndpoint (data: CenterArcData) : EndpointArcData =
        { Start = arcPointAtAngle data data.StartAngle
          Radius = data.Radius
          XAxisRotation = data.XAxisRotation
          LargeArc = arcLargeArc data
          Sweep = arcSweep data
          End = arcPointAtAngle data (arcEndAngle data) }

    let private arcBetween arc fromParameter toParameter =
        { arc with
            StartAngle = angleAtRaw arc fromParameter
            DeltaAngle = Parameter.ratio (toParameter - fromParameter) * arc.DeltaAngle }

    let splitArc arc t = arcBetween arc (parameter 0.0) t, arcBetween arc t (parameter 1.0)

    let splitArcInside arc t =
        if t < parameter 0.0 || t > parameter 1.0 then Error SplitOutsideArc else Ok(splitArc arc t)

    let private normalizedProgresses points =
        points
        |> List.distinct
        |> List.sort
        |> List.skipWhile ((=) (parameter 0.0))
        |> List.rev
        |> List.skipWhile ((=) (parameter 1.0))
        |> List.rev

    let splitArcMany arc points =
        let boundaries = parameter 0.0 :: normalizedProgresses points @ [ parameter 1.0 ]
        boundaries |> List.pairwise |> List.map (fun (fromParameter, toParameter) -> arcBetween arc fromParameter toParameter)

    let splitArcInsideMany arc points =
        let points = normalizedProgresses points
        if points |> List.exists (fun t -> t < parameter 0.0 || t > parameter 1.0) then
            Error SplitOutsideArc
        else
            Ok(splitArcMany arc points)

    let private boundingCandidateAngles arc =
        let xAlpha = arc.Radius.X * Trig.cosDegrees arc.XAxisRotation
        let xBeta = -arc.Radius.Y * Trig.sinDegrees arc.XAxisRotation
        let yAlpha = arc.Radius.X * Trig.sinDegrees arc.XAxisRotation
        let yBeta = arc.Radius.Y * Trig.cosDegrees arc.XAxisRotation
        [ Trig.atan2Degrees xBeta xAlpha
          Trig.atan2Degrees xBeta xAlpha + halfTurn
          Trig.atan2Degrees yBeta yAlpha
          Trig.atan2Degrees yBeta yAlpha + halfTurn ]
        |> List.filter (fun angle -> angleInSweep angle arc.StartAngle arc.DeltaAngle)
        |> fun angles -> arc.StartAngle :: arcEndAngle arc :: angles

    let arcBoundingBox arc =
        let points = boundingCandidateAngles arc |> List.map (arcPointAtAngle arc)
        let first = List.head points
        points
        |> List.tail
        |> List.fold
            (fun box candidate ->
                { Min = Point.create (min box.Min.X candidate.X) (min box.Min.Y candidate.Y)
                  Max = Point.create (max box.Max.X candidate.X) (max box.Max.Y candidate.Y) })
            { Min = first; Max = first }

    let arcProjectionExtrema arc (direction: Point<'Direction>) =
        let xAxisX = arc.Radius.X * Trig.cosDegrees arc.XAxisRotation
        let xAxisY = arc.Radius.X * Trig.sinDegrees arc.XAxisRotation
        let yAxisX = -arc.Radius.Y * Trig.sinDegrees arc.XAxisRotation
        let yAxisY = arc.Radius.Y * Trig.cosDegrees arc.XAxisRotation
        let alpha = direction.X * xAxisX + direction.Y * xAxisY
        let beta = direction.X * yAxisX + direction.Y * yAxisY
        if alpha = 0.0<_> && beta = 0.0<_> then []
        else
            let supportAngle = Trig.atan2Degrees beta alpha
            [ supportAngle; supportAngle + halfTurn ]
            |> List.filter (fun angle -> angleInSweep angle arc.StartAngle arc.DeltaAngle)
            |> List.map (fun angle -> parameter (Degree.toFloat (angleProgress angle arc.StartAngle arc.DeltaAngle) / abs (Degree.toFloat arc.DeltaAngle)))
            |> List.filter (fun t -> t >= parameter 0.0 && t <= parameter 1.0)

    let private cubicSplitProgresses arc =
        let delta = abs (Degree.toFloat arc.DeltaAngle)
        if delta <= 90.0 + scalarTolerance then []
        else
            let step = 90.0 / delta
            Seq.initInfinite (fun index -> float (index + 1) * step)
            |> Seq.takeWhile (fun value -> value < 1.0 - Parameter.ratio parameterTolerance)
            |> Seq.map parameter
            |> Seq.toList

    let private cubicForArc arc =
        let startAngle = arc.StartAngle
        let endAngle = arcEndAngle arc
        let alpha = Radian.fromFloat (4.0 / 3.0 * Trig.tanDegrees (arc.DeltaAngle / 4.0))
        let startPoint = ellipsePoint arc startAngle
        let endPoint = ellipsePoint arc endAngle
        let startTangent = ellipseDerivativeRadians arc startAngle
        let endTangent = ellipseDerivativeRadians arc endAngle
        { Start = startPoint
          Control1 = Point.translate (Point.scale alpha startTangent) startPoint
          Control2 = Point.translate (Point.scale -alpha endTangent) endPoint
          End = endPoint }

    let arcToCubics startPoint radius xAxisRotation largeArc sweep endPoint =
        match doEndpointToCenter startPoint radius xAxisRotation largeArc sweep endPoint with
        | Error error -> Error error
        | Ok arc -> splitArcInsideMany arc (cubicSplitProgresses arc) |> Result.map (List.map cubicForArc)

    let private arcAxes radius xAxisRotation =
        let rx, ry = abs radius.X, abs radius.Y
        if rx <= lengthTolerance || ry <= lengthTolerance then Error DegenerateInputArc
        else
            let cosine, sine = Trig.cosDegrees xAxisRotation, Trig.sinDegrees xAxisRotation
            Ok(Point.create (rx * cosine) (rx * sine), Point.create (-ry * sine) (ry * cosine))

    let private normalizeAxisRotation degrees =
        let normalized = degrees % halfTurn
        if normalized < 0.0<degree> then normalized + halfTurn else normalized

    let private eigenvector
        (sxx: float<length^2>)
        (sxy: float<length^2>)
        (syy: float<length^2>)
        (lambda: float<length^2>)
        : Point<1> =
        let matrixScale = max (abs sxx) (abs syy)
        if abs sxy > scalarTolerance * matrixScale then
            Point.normalize (Point.create sxy (lambda - sxx)) |> Option.defaultValue Point.right
        elif sxx >= syy then Point.right else Point.down

    let private extractAxes (xAxis: Point<length>) (yAxis: Point<length>) =
        let sxx = xAxis.X * xAxis.X + yAxis.X * yAxis.X
        let sxy = xAxis.X * xAxis.Y + yAxis.X * yAxis.Y
        let syy = xAxis.Y * xAxis.Y + yAxis.Y * yAxis.Y
        let discriminant = sqrtMeasured ((sxx - syy) * (sxx - syy) + 4.0 * sxy * sxy)
        let lambda1 = (sxx + syy + discriminant) / 2.0
        let lambda2 = (sxx + syy - discriminant) / 2.0
        if lambda1 <= squaredLengthTolerance || lambda2 <= squaredLengthTolerance then
            Error DegenerateInputArc
        else
            let axis1 = eigenvector sxx sxy syy lambda1
            let axis2 = Point.create -axis1.Y axis1.X
            if abs (Point.dot axis1 xAxis) >= abs (Point.dot axis2 xAxis) then
                Ok(Point.create (sqrtMeasured lambda1) (sqrtMeasured lambda2), normalizeAxisRotation (Point.heading axis1))
            else
                Ok(Point.create (sqrtMeasured lambda2) (sqrtMeasured lambda1), normalizeAxisRotation (Point.heading axis2))

    let transformedAxes radius xAxisRotation transform =
        match arcAxes radius xAxisRotation with
        | Error error -> Error error
        | Ok(xAxis, yAxis) -> extractAxes (Affine.linearPoint transform xAxis) (Affine.linearPoint transform yAxis)

    let private transformedPoint transform point = Affine.point transform point

    let private fullyCollapsed xAxis yAxis =
        Point.norm xAxis <= lengthTolerance && Point.norm yAxis <= lengthTolerance

    let private collapsedAxis xAxis yAxis =
        let xLength, yLength = Point.norm xAxis, Point.norm yAxis
        let crossTolerance = scalarTolerance * xLength * yLength
        if xLength <= lengthTolerance && yLength <= lengthTolerance then Error NotCollapsedToLine
        elif abs (Point.cross xAxis yAxis) > crossTolerance then Error NotCollapsedToLine
        elif xLength >= yLength then Ok(Point.scale (1.0 / xLength) xAxis)
        else Ok(Point.scale (1.0 / yLength) yAxis)

    let private collapsedAngles arc alpha beta interiorOnly =
        let maximumAngle = Trig.atan2Degrees beta alpha
        let candidates = [ maximumAngle + halfTurn; maximumAngle ]
        if interiorOnly then
            candidates
            |> List.filter (fun angle ->
                let progress = angleProgress angle arc.StartAngle arc.DeltaAngle
                progress > degreeTolerance && progress < abs arc.DeltaAngle - degreeTolerance)
            |> List.sortBy (fun angle -> angleProgress angle arc.StartAngle arc.DeltaAngle)
            |> fun angles -> arc.StartAngle :: angles @ [ arcEndAngle arc ]
        else
            candidates
            |> List.filter (fun angle -> angleInSweep angle arc.StartAngle arc.DeltaAngle)
            |> fun angles -> arc.StartAngle :: arcEndAngle arc :: angles

    let private collapsedArcPoints startPoint radius xAxisRotation largeArc sweep endPoint transform =
        match doEndpointToCenter startPoint radius xAxisRotation largeArc sweep endPoint with
        | Error error -> Error error
        | Ok arc ->
            match arcAxes arc.Radius arc.XAxisRotation with
            | Error error -> Error error
            | Ok(rawX, rawY) ->
                let xAxis, yAxis = Affine.linearPoint transform rawX, Affine.linearPoint transform rawY
                if fullyCollapsed xAxis yAxis then Ok [ transformedPoint transform startPoint; transformedPoint transform endPoint ]
                else
                    match collapsedAxis xAxis yAxis with
                    | Error error -> Error error
                    | Ok axis ->
                        let center = transformedPoint transform arc.Center
                        let alpha, beta = Point.dot xAxis axis, Point.dot yAxis axis
                        collapsedAngles arc alpha beta true
                        |> List.map (fun angle ->
                            let scalar = alpha * Trig.cosDegrees angle + beta * Trig.sinDegrees angle
                            Point.translate (Point.scale scalar axis) center)
                        |> Ok

    let collapsedArcSubpath startPoint radius xAxisRotation largeArc sweep endPoint transform =
        collapsedArcPoints startPoint radius xAxisRotation largeArc sweep endPoint transform

    let collapsedArcLine startPoint radius xAxisRotation largeArc sweep endPoint transform =
        match doEndpointToCenter startPoint radius xAxisRotation largeArc sweep endPoint with
        | Error error -> Error error
        | Ok arc ->
            match arcAxes arc.Radius arc.XAxisRotation with
            | Error error -> Error error
            | Ok(rawX, rawY) ->
                let xAxis, yAxis = Affine.linearPoint transform rawX, Affine.linearPoint transform rawY
                if fullyCollapsed xAxis yAxis then Ok(transformedPoint transform startPoint, transformedPoint transform endPoint)
                else
                    match collapsedAxis xAxis yAxis with
                    | Error error -> Error error
                    | Ok axis ->
                        let center = transformedPoint transform arc.Center
                        let alpha, beta = Point.dot xAxis axis, Point.dot yAxis axis
                        let points =
                            collapsedAngles arc alpha beta false
                            |> List.map (fun angle ->
                                let scalar = alpha * Trig.cosDegrees angle + beta * Trig.sinDegrees angle
                                Point.translate (Point.scale scalar axis) center)
                        Ok(List.minBy (Point.dot axis) points, List.maxBy (Point.dot axis) points)
