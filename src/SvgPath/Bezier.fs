namespace SvgPath

type BezierPoint = Point<length>

[<Struct>]
type BezierBoundingBox =
    { Min: BezierPoint
      Max: BezierPoint }

type CubicFitHandleState =
    | UnconstrainedHandle
    | PositiveHandle
    | CollapsedHandle

[<Struct>]
type CubicFitReport =
    { RootSumSquare: float<length>
      RootMeanSquare: float<length>
      Max: float<length>
      StartHandle: CubicFitHandleState
      EndHandle: CubicFitHandleState }

[<Struct>]
type CubicSelfIntersectionOptions =
    { MinimumArcLengthSeparation: float<length>
      DistanceTolerance: float<length> }

[<Struct>]
type CubicSelfIntersection =
    { S: float<parameter>
      T: float<parameter>
      Point: BezierPoint }

type BezierData =
    | LinearBezierData of startPoint: BezierPoint * endPoint: BezierPoint
    | QuadraticBezierData of startPoint: BezierPoint * control: BezierPoint * endPoint: BezierPoint
    | CubicBezierData of
        startPoint: BezierPoint *
        control1: BezierPoint *
        control2: BezierPoint *
        endPoint: BezierPoint

type BezierError =
    | SplitOutsideBezier
    | DegenerateTangent
    | UnderdeterminedCubicFit
    | InvalidCubicSelfIntersectionMinimumArcLengthSeparation of float<length>
    | InvalidCubicSelfIntersectionDistanceTolerance of float<length>

/// Evaluation, subdivision, fitting, bounds, and intersections for Bézier curves.
[<RequireQualifiedAccess>]
module Bezier =
    let private parameter value = Parameter.fromFloat value
    let private ratio value = Parameter.ratio value

    let start curve =
        match curve with
        | LinearBezierData(startPoint, _)
        | QuadraticBezierData(startPoint, _, _)
        | CubicBezierData(startPoint, _, _, _) -> startPoint

    let finish curve =
        match curve with
        | LinearBezierData(_, endPoint)
        | QuadraticBezierData(_, _, endPoint)
        | CubicBezierData(_, _, _, endPoint) -> endPoint

    let point curve (t: float<parameter>) =
        match curve with
        | LinearBezierData(startPoint, endPoint) -> Point.interpolate startPoint endPoint t
        | QuadraticBezierData(startPoint, control, endPoint) ->
            Point.interpolate
                (Point.interpolate startPoint control t)
                (Point.interpolate control endPoint t)
                t
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            let left = Point.interpolate startPoint control1 t
            let middle = Point.interpolate control1 control2 t
            let right = Point.interpolate control2 endPoint t
            Point.interpolate (Point.interpolate left middle t) (Point.interpolate middle right t) t

    let derivative curve (t: float<parameter>) : Point<length / parameter> =
        let perParameter = 1.0<1 / parameter>
        match curve with
        | LinearBezierData(startPoint, endPoint) ->
            Point.displacement startPoint endPoint |> Point.scale perParameter
        | QuadraticBezierData(startPoint, control, endPoint) ->
            let left = Point.displacement startPoint control
            let right = Point.displacement control endPoint
            Point.add (Point.scale (1.0 - ratio t) left) (Point.scale (ratio t) right)
            |> Point.scale (2.0 * perParameter)
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            let left = Point.displacement startPoint control1
            let middle = Point.displacement control1 control2
            let right = Point.displacement control2 endPoint
            let first = Point.add (Point.scale (1.0 - ratio t) left) (Point.scale (ratio t) middle)
            let second = Point.add (Point.scale (1.0 - ratio t) middle) (Point.scale (ratio t) right)
            Point.add (Point.scale (1.0 - ratio t) first) (Point.scale (ratio t) second)
            |> Point.scale (3.0 * perParameter)

    let lineProjectionExtrema (_start: BezierPoint) (_end: BezierPoint) (_direction: Point<1>) = []

    let private tolerantQuadraticRoots a b c =
        Root.quadraticWith
            { CoefficientTolerance = LanguagePrimitives.FloatWithMeasure 1.0e-12
              RepeatedRootPolicy = PreserveRepeatedRoot }
            a
            b
            c

    let quadraticProjectionExtrema
        (startPoint: BezierPoint)
        (control: BezierPoint)
        (endPoint: BezierPoint)
        (direction: Point<1>) =
        let p0 = startPoint.X * direction.X + startPoint.Y * direction.Y
        let p1 = control.X * direction.X + control.Y * direction.Y
        let p2 = endPoint.X * direction.X + endPoint.Y * direction.Y
        let a = p0 - 2.0 * p1 + p2
        let b = -2.0 * p0 + 2.0 * p1
        tolerantQuadraticRoots 0.0<_> (2.0 * a) b
        |> List.filter (fun t -> t >= parameter 0.0 && t <= parameter 1.0)

    let cubicProjectionExtrema
        (startPoint: BezierPoint)
        (control1: BezierPoint)
        (control2: BezierPoint)
        (endPoint: BezierPoint)
        (direction: Point<1>) =
        let p0 = startPoint.X * direction.X + startPoint.Y * direction.Y
        let p1 = control1.X * direction.X + control1.Y * direction.Y
        let p2 = control2.X * direction.X + control2.Y * direction.Y
        let p3 = endPoint.X * direction.X + endPoint.Y * direction.Y
        let a = -p0 + 3.0 * p1 - 3.0 * p2 + p3
        let b = 3.0 * p0 - 6.0 * p1 + 3.0 * p2
        let c = -3.0 * p0 + 3.0 * p1
        tolerantQuadraticRoots (3.0 * a) (2.0 * b) c
        |> List.filter (fun t -> t >= parameter 0.0 && t <= parameter 1.0)

    let projectionExtrema curve direction =
        match curve with
        | LinearBezierData(startPoint, endPoint) -> lineProjectionExtrema startPoint endPoint direction
        | QuadraticBezierData(startPoint, control, endPoint) ->
            quadraticProjectionExtrema startPoint control endPoint direction
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            cubicProjectionExtrema startPoint control1 control2 endPoint direction

    let mapPoints mapping curve =
        match curve with
        | LinearBezierData(startPoint, endPoint) -> LinearBezierData(mapping startPoint, mapping endPoint)
        | QuadraticBezierData(startPoint, control, endPoint) ->
            QuadraticBezierData(mapping startPoint, mapping control, mapping endPoint)
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            CubicBezierData(mapping startPoint, mapping control1, mapping control2, mapping endPoint)

    let split curve (t: float<parameter>) =
        match curve with
        | LinearBezierData(startPoint, endPoint) ->
            let splitPoint = Point.interpolate startPoint endPoint t
            LinearBezierData(startPoint, splitPoint), LinearBezierData(splitPoint, endPoint)
        | QuadraticBezierData(startPoint, control, endPoint) ->
            let startControl = Point.interpolate startPoint control t
            let controlEnd = Point.interpolate control endPoint t
            let splitPoint = Point.interpolate startControl controlEnd t
            QuadraticBezierData(startPoint, startControl, splitPoint),
            QuadraticBezierData(splitPoint, controlEnd, endPoint)
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            let startControl = Point.interpolate startPoint control1 t
            let controls = Point.interpolate control1 control2 t
            let controlEnd = Point.interpolate control2 endPoint t
            let leftControl = Point.interpolate startControl controls t
            let rightControl = Point.interpolate controls controlEnd t
            let splitPoint = Point.interpolate leftControl rightControl t
            CubicBezierData(startPoint, startControl, leftControl, splitPoint),
            CubicBezierData(splitPoint, rightControl, controlEnd, endPoint)

    let splitInside curve t =
        if t < parameter 0.0 || t > parameter 1.0 then Error SplitOutsideBezier else Ok(split curve t)

    let private between curve fromParameter toParameter =
        let startPoint = point curve fromParameter
        let endPoint = point curve toParameter
        match curve with
        | LinearBezierData _ -> LinearBezierData(startPoint, endPoint)
        | QuadraticBezierData _ ->
            let parameterDelta = toParameter - fromParameter
            let control = derivative curve fromParameter |> Point.scale (parameterDelta / 2.0) |> fun v -> Point.translate v startPoint
            QuadraticBezierData(startPoint, control, endPoint)
        | CubicBezierData _ ->
            let parameterDelta = toParameter - fromParameter
            let control1 = derivative curve fromParameter |> Point.scale (parameterDelta / 3.0) |> fun v -> Point.translate v startPoint
            let control2 = derivative curve toParameter |> Point.scale (-parameterDelta / 3.0) |> fun v -> Point.translate v endPoint
            CubicBezierData(startPoint, control1, control2, endPoint)

    let private normalizedProgresses points =
        points
        |> List.distinct
        |> List.sort
        |> List.skipWhile ((=) (parameter 0.0))
        |> List.rev
        |> List.skipWhile ((=) (parameter 1.0))
        |> List.rev

    let splitMany curve points =
        let points = normalizedProgresses points
        let boundaries = parameter 0.0 :: (points @ [ parameter 1.0 ])
        boundaries |> List.pairwise |> List.map (fun (fromParameter, toParameter) -> between curve fromParameter toParameter)

    let splitInsideMany curve points =
        let points = normalizedProgresses points
        if points |> List.exists (fun t -> t < parameter 0.0 || t > parameter 1.0) then
            Error SplitOutsideBezier
        else
            Ok(splitMany curve points)

    let private quadraticExtrema
        (startValue: float<'Value>)
        (control: float<'Value>)
        (endValue: float<'Value>) =
        let denominator = startValue - 2.0 * control + endValue
        if denominator = LanguagePrimitives.FloatWithMeasure<'Value> 0.0 then
            []
        else
            [ parameter (float ((startValue - control) / denominator)) ]

    let private cubicExtrema
        (startValue: float<'Value>)
        (control1: float<'Value>)
        (control2: float<'Value>)
        (endValue: float<'Value>) =
        let a = -startValue + 3.0 * control1 - 3.0 * control2 + endValue
        let b = 3.0 * startValue - 6.0 * control1 + 3.0 * control2
        let c = 3.0 * control1 - 3.0 * startValue
        Root.quadratic (3.0 * a) (2.0 * b) c

    let private axisExtrema curve =
        let roots =
            match curve with
            | LinearBezierData _ -> []
            | QuadraticBezierData(startPoint, control, endPoint) ->
                quadraticExtrema startPoint.X control.X endPoint.X
                @ quadraticExtrema startPoint.Y control.Y endPoint.Y
            | CubicBezierData(startPoint, control1, control2, endPoint) ->
                cubicExtrema startPoint.X control1.X control2.X endPoint.X
                @ cubicExtrema startPoint.Y control1.Y control2.Y endPoint.Y
        roots |> List.filter (fun t -> t >= parameter 0.0 && t <= parameter 1.0)

    let boundingBox curve =
        let points = parameter 0.0 :: parameter 1.0 :: axisExtrema curve |> List.map (point curve)
        let first = List.head points
        points
        |> List.tail
        |> List.fold
            (fun box candidate ->
                { Min = Point.create (min box.Min.X candidate.X) (min box.Min.Y candidate.Y)
                  Max = Point.create (max box.Max.X candidate.X) (max box.Max.Y candidate.Y) })
            { Min = first; Max = first }

    let private closeParameters values =
        values
        |> List.sort
        |> List.fold
            (fun kept value ->
                match kept with
                | previous :: _ when abs (value - previous) <= parameter 1.0e-9 -> kept
                | _ -> value :: kept)
            []
        |> List.rev

    let cubicInflectionParameters curve =
        match curve with
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            let a =
                Point.add
                    (Point.subtract (Point.scale 3.0 (Point.displacement (Point.create 0.0<length> 0.0<length>) control1))
                        (Point.displacement (Point.create 0.0<length> 0.0<length>) startPoint))
                    (Point.subtract (Point.displacement (Point.create 0.0<length> 0.0<length>) endPoint)
                        (Point.scale 3.0 (Point.displacement (Point.create 0.0<length> 0.0<length>) control2)))
            let b =
                Point.add
                    (Point.subtract (Point.scale 3.0 (Point.displacement (Point.create 0.0<length> 0.0<length>) startPoint))
                        (Point.scale 6.0 (Point.displacement (Point.create 0.0<length> 0.0<length>) control1)))
                    (Point.scale 3.0 (Point.displacement (Point.create 0.0<length> 0.0<length>) control2))
            let c = Point.scale 3.0 (Point.displacement startPoint control1)
            let quadratic = -6.0 * Point.cross a b
            let linear = 6.0 * Point.cross c a
            let constant = 2.0 * Point.cross c b
            let scale = max (abs quadratic) (max (abs linear) (abs constant))
            if scale = 0.0<_> then
                []
            else
                Root.quadraticWith
                    { CoefficientTolerance = 1.0e-12
                      RepeatedRootPolicy = PreserveRepeatedRoot }
                    (quadratic / scale)
                    (linear / scale)
                    (constant / scale)
                |> List.filter (fun t -> t > parameter 1.0e-9 && t < parameter (1.0 - 1.0e-9))
                |> closeParameters
        | LinearBezierData _
        | QuadraticBezierData _ -> []

    let private handleState (length: float<length>) =
        if length = 0.0<length> then CollapsedHandle else PositiveHandle

    let private normalEquationsAreSingular (ata00: float) (ata01: float) (ata11: float) =
        let determinant = ata00 * ata11 - ata01 * ata01
        let scale = abs (ata00 * ata11)
        scale <= 0.0 || abs determinant <= 1.0e-12 * scale

    let private unconstrainedCubicFit
        (ata00: float)
        (ata01: float)
        (ata11: float)
        (atb0: float<length>)
        (atb1: float<length>) =
        let determinant = ata00 * ata11 - ata01 * ata01
        if normalEquationsAreSingular ata00 ata01 ata11 then
            Error UnderdeterminedCubicFit
        else
            Ok((atb0 * ata11 - atb1 * ata01) / determinant,
               (ata00 * atb1 - ata01 * atb0) / determinant)

    let private nonnegativeAxisFit (ata: float) (atb: float<length>) =
        if ata <= 0.0 then 0.0<length> else max 0.0<length> (atb / ata)

    let private cubicFitCandidateScore
        (ata00: float)
        (ata01: float)
        (ata11: float)
        (atb0: float<length>)
        (atb1: float<length>)
        (a: float<length>, b: float<length>) =
        ata00 * a * a + 2.0 * ata01 * a * b + ata11 * b * b
        - 2.0 * atb0 * a - 2.0 * atb1 * b

    let private solveNonnegativeCubicFit
        (ata00: float)
        (ata01: float)
        (ata11: float)
        (atb0: float<length>)
        (atb1: float<length>) =
        let unconstrained =
            match unconstrainedCubicFit ata00 ata01 ata11 atb0 atb1 with
            | Ok(a, b) when a >= 0.0<length> && b >= 0.0<length> -> [ a, b ]
            | _ -> []
        let candidates =
            [ 0.0<length>, 0.0<length>
              nonnegativeAxisFit ata00 atb0, 0.0<length>
              0.0<length>, nonnegativeAxisFit ata11 atb1 ] @ unconstrained
        candidates
        |> List.minBy (cubicFitCandidateScore ata00 ata01 ata11 atb0 atb1)
        |> Ok

    let private tangentFitEquations
        (samples: (float<parameter> * BezierPoint) list)
        startPoint
        endPoint
        (startDirection: Point<1>)
        (endDirection: Point<1>) =
        let folder (ata00, ata01, ata11, atb0, atb1, count) (t, samplePoint) =
            let t = ratio t
            let oneMinusT = 1.0 - t
            let startBasis = 3.0 * oneMinusT * oneMinusT * t
            let endBasis = 3.0 * oneMinusT * t * t
            let fixedPoint =
                Point.add
                    (Point.scale (oneMinusT * oneMinusT * oneMinusT + startBasis) startPoint)
                    (Point.scale (endBasis + t * t * t) endPoint)
            let target = Point.subtract samplePoint fixedPoint
            let leftColumn = Point.scale startBasis startDirection
            let rightColumn = Point.scale (-endBasis) endDirection
            ata00 + Point.dot leftColumn leftColumn,
            ata01 + Point.dot leftColumn rightColumn,
            ata11 + Point.dot rightColumn rightColumn,
            atb0 + Point.dot leftColumn target,
            atb1 + Point.dot rightColumn target,
            count + 1
        let ata00, ata01, ata11, atb0, atb1, count =
            List.fold folder (0.0, 0.0, 0.0, 0.0<length>, 0.0<length>, 0) samples
        if count = 0 then Error UnderdeterminedCubicFit
        else solveNonnegativeCubicFit ata00 ata01 ata11 atb0 atb1

    let private fitError samples curve =
        let sumSquared, maxSquared, count =
            samples
            |> List.fold
                (fun (sumSquared, maxSquared, count) (t, samplePoint) ->
                    let errorSquared = Point.squaredDistance samplePoint (point curve t)
                    sumSquared + errorSquared, max maxSquared errorSquared, count + 1)
                (0.0<length^2>, 0.0<length^2>, 0)
        if count = 0 then
            { RootSumSquare = 0.0<length>
              RootMeanSquare = 0.0<length>
              Max = 0.0<length>
              StartHandle = UnconstrainedHandle
              EndHandle = UnconstrainedHandle }
        else
            { RootSumSquare = sqrt sumSquared
              RootMeanSquare = sqrt (sumSquared / float count)
              Max = sqrt maxSquared
              StartHandle = UnconstrainedHandle
              EndHandle = UnconstrainedHandle }

    let fitCubicWithEndpointTangents
        startPoint
        endPoint
        (startTangent: Point<'StartTangent>)
        (endTangent: Point<'EndTangent>)
        samples =
        match Point.normalize startTangent, Point.normalize endTangent with
        | Some startDirection, Some endDirection ->
            match tangentFitEquations samples startPoint endPoint startDirection endDirection with
            | Error error -> Error error
            | Ok(a, b) ->
                let control1 = Point.translate (Point.scale a startDirection) startPoint
                let control2 = Point.translate (Point.scale -b endDirection) endPoint
                let curve = CubicBezierData(startPoint, control1, control2, endPoint)
                let report = fitError samples curve
                Ok(curve,
                   { report with
                       StartHandle = handleState a
                       EndHandle = handleState b })
        | _ -> Error DegenerateTangent

    let private endpointFitEquations samples startPoint endPoint =
        let folder (ata00, ata01, ata11, atb0, atb1, count) (t, samplePoint) =
            let t = ratio t
            let oneMinusT = 1.0 - t
            let startBasis = oneMinusT * oneMinusT * oneMinusT
            let control1Basis = 3.0 * oneMinusT * oneMinusT * t
            let control2Basis = 3.0 * oneMinusT * t * t
            let endBasis = t * t * t
            let fixedPoint =
                Point.add (Point.scale startBasis startPoint) (Point.scale endBasis endPoint)
            let target = Point.subtract samplePoint fixedPoint
            ata00 + control1Basis * control1Basis,
            ata01 + control1Basis * control2Basis,
            ata11 + control2Basis * control2Basis,
            Point.add atb0 (Point.scale control1Basis target),
            Point.add atb1 (Point.scale control2Basis target),
            count + 1
        let ata00, ata01, ata11, atb0, atb1, count =
            List.fold folder (0.0, 0.0, 0.0, Point.create 0.0<length> 0.0<length>, Point.create 0.0<length> 0.0<length>, 0) samples
        if count = 0 || normalEquationsAreSingular ata00 ata01 ata11 then
            Error UnderdeterminedCubicFit
        else
            let determinant = ata00 * ata11 - ata01 * ata01
            Ok(Point.scale (1.0 / determinant) (Point.subtract (Point.scale ata11 atb0) (Point.scale ata01 atb1)),
               Point.scale (1.0 / determinant) (Point.subtract (Point.scale ata00 atb1) (Point.scale ata01 atb0)))

    let fitCubicWithEndpoints startPoint endPoint samples =
        match endpointFitEquations samples startPoint endPoint with
        | Error error -> Error error
        | Ok(control1, control2) ->
            let curve = CubicBezierData(startPoint, control1, control2, endPoint)
            Ok(curve, fitError samples curve)

    let defaultCubicSelfIntersectionOptions () =
        { MinimumArcLengthSeparation = 1.0e-9<length>
          DistanceTolerance = 1.0e-9<length> }

    let rec private approximateLength curve remainingDepth =
        let chord = Point.distance (start curve) (finish curve)
        let polygon =
            match curve with
            | LinearBezierData(startPoint, endPoint) -> Point.distance startPoint endPoint
            | QuadraticBezierData(startPoint, control, endPoint) ->
                Point.distance startPoint control + Point.distance control endPoint
            | CubicBezierData(startPoint, control1, control2, endPoint) ->
                Point.distance startPoint control1 + Point.distance control1 control2 + Point.distance control2 endPoint
        if remainingDepth <= 0 || polygon - chord <= 1.0e-9<length> then
            (polygon + chord) / 2.0
        else
            let left, right = split curve (parameter 0.5)
            approximateLength left (remainingDepth - 1) + approximateLength right (remainingDepth - 1)

    let private selfIntersectionCandidate curve =
        match curve with
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            let p0, p1, p2, p3 = startPoint, control1, control2, endPoint
            let a = Point.add (Point.subtract p3 (Point.scale 3.0 p2)) (Point.subtract (Point.scale 3.0 p1) p0)
            let b = Point.add (Point.subtract (Point.scale 3.0 p0) (Point.scale 6.0 p1)) (Point.scale 3.0 p2)
            let c = Point.subtract (Point.scale 3.0 p1) (Point.scale 3.0 p0)
            let crossAB = Point.cross a b
            let squaredA = Point.squaredNorm a
            if crossAB = 0.0<length^2> || squaredA = 0.0<length^2> then
                None
            else
                let u = -Point.cross a c / crossAB
                let v = u * u + Point.dot a (Point.add (Point.scale u b) c) / squaredA
                let discriminant = u * u - 4.0 * v
                if discriminant < 0.0 then None
                else
                    let root = sqrt discriminant
                    let first = parameter ((u - root) / 2.0)
                    let second = parameter ((u + root) / 2.0)
                    Some(if first <= second then first, second else second, first)
        | _ -> None

    let cubicSelfIntersectionsWith curve options =
        let minimum = Length.toFloat options.MinimumArcLengthSeparation
        let tolerance = Length.toFloat options.DistanceTolerance
        if minimum <= 0.0 || not (System.Double.IsFinite minimum) then
            Error(InvalidCubicSelfIntersectionMinimumArcLengthSeparation options.MinimumArcLengthSeparation)
        elif tolerance <= 0.0 || not (System.Double.IsFinite tolerance) then
            Error(InvalidCubicSelfIntersectionDistanceTolerance options.DistanceTolerance)
        else
            match selfIntersectionCandidate curve with
            | None -> Ok []
            | Some(s, t) when s < parameter 0.0 || t > parameter 1.0 -> Ok []
            | Some(s, t) ->
                let leftPoint = point curve s
                let rightPoint = point curve t
                let arcLength = approximateLength (between curve s t) 16
                if arcLength >= options.MinimumArcLengthSeparation
                   && Point.squaredDistance leftPoint rightPoint <= options.DistanceTolerance * options.DistanceTolerance then
                    Ok [ { S = s; T = t; Point = Point.midpoint leftPoint rightPoint } ]
                else
                    Ok []

    let cubicSelfIntersections curve =
        cubicSelfIntersectionsWith curve (defaultCubicSelfIntersectionOptions ())
