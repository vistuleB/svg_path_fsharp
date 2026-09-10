namespace SvgPath

[<Struct>]
type SegmentIntersection =
    { LeftT: float<parameter>
      RightT: float<parameter>
      Point: Point<length> }

type internal CurveSolverError =
    | CurveSolverPathError of error: SegmentError
    | CurveSolverDepthLimit of leftFrom: float<parameter> * leftTo: float<parameter> * rightFrom: float<parameter> * rightTo: float<parameter>
type internal ElizabethBeamReport =
    { Intersections: SegmentIntersection list; Examined: int
      DiscardedCrossing: int; DiscardedOther: int; PeakRetained: int; DiscardedCandidates: int }

[<Struct>]
type SubpathIntersection =
    { Point: Point<length>
      LeftParameters: SubpathParameter list
      RightParameters: SubpathParameter list }

[<Struct>]
type PathIntersection =
    { Point: Point<length>
      LeftParameters: PathParameter list
      RightParameters: PathParameter list }

[<Struct>]
type SelfIntersectionOptions =
    { MinimumArcLengthSeparation: float<length>
      DistanceTolerance: float<length> }

[<Struct>]
type SegmentSegmentProjection =
    { LeftT: float<parameter>
      RightT: float<parameter>
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type SegmentSubpathProjection =
    { LeftT: float<parameter>
      RightAt: SubpathParameter
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type SegmentPathProjection =
    { LeftT: float<parameter>
      RightAt: PathParameter
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type SubpathSubpathProjection =
    { LeftAt: SubpathParameter
      RightAt: SubpathParameter
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type SubpathPathProjection =
    { LeftAt: SubpathParameter
      RightAt: PathParameter
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type PathPathProjection =
    { LeftAt: PathParameter
      RightAt: PathParameter
      LeftPoint: Point<length>
      RightPoint: Point<length>
      Distance: float<length> }

[<Struct>]
type SubpathSelfIntersection =
    { Point: Point<length>
      Parameters: SubpathParameter * SubpathParameter }

[<Struct>]
type PathSelfIntersection =
    { Point: Point<length>
      Parameters: PathParameter * PathParameter }

type CrossingDirection =
    | Clockwise
    | Counterclockwise

type TouchingDirection =
    | SimilarlyDirected
    | OppositelyDirected

type TouchingOrder =
    | ClockwiseFromFirstToSecond
    | ClockwiseFromSecondToFirst
    | IndeterminateTouchingOrder

type SubpathEndpoint =
    | StartEndpoint
    | EndEndpoint

type EndpointContact =
    | FirstEndpointToSecondInterior of first: SubpathEndpoint
    | FirstInteriorToSecondEndpoint of second: SubpathEndpoint
    | EndpointToEndpoint of first: SubpathEndpoint * second: SubpathEndpoint

[<Struct>]
type IntersectionApertures =
    { FirstIncomingToSecondIncoming: float<degree>
      FirstIncomingToSecondOutgoing: float<degree>
      FirstOutgoingToSecondIncoming: float<degree>
      FirstOutgoingToSecondOutgoing: float<degree> }

type IntersectionClassification =
    | Crossing of direction: CrossingDirection * apertures: IntersectionApertures
    | Touching of
        direction: TouchingDirection *
        incomingOrder: TouchingOrder *
        outgoingOrder: TouchingOrder *
        apertures: IntersectionApertures
    | EndpointContact of EndpointContact
    | Indeterminate

[<Struct>]
type ClassifiedSubpathIntersection =
    { FirstParameter: SubpathParameter
      SecondParameter: SubpathParameter
      Classification: IntersectionClassification }

type ParameterSnap =
    | NoParameterSnap
    | DecimalParameterSnap of exponent: int

[<Struct>]
type ClassificationOptions =
    { DirectionOptions: DirectionOptions
      AngularTolerance: float<degree>
      DistanceTolerance: float<length>
      LengthOptions: LengthOptions
      InitialArcLength: float<length>
      MaximumArcLength: float<length>
      MaxSamplingSteps: int }

[<RequireQualifiedAccess>]
type ClassificationError =
    | PathError of error: SegmentError
    | InvalidAngularTolerance of tolerance: float<degree>
    | InvalidClassificationDistanceTolerance of tolerance: float<length>
    | InvalidClassificationInitialArcLength of initialArcLength: float<length>
    | InvalidClassificationMaximumArcLength of maximumArcLength: float<length>
    | InvalidClassificationMaxSamplingSteps of maxSamplingSteps: int

[<Struct>]
type IntersectionOptions =
    { Tolerance: float<length>
      MaxDepth: int
      ParameterSnap: ParameterSnap }

[<Struct>]
type private IntersectionWindow =
    { LeftFrom: float<parameter>
      LeftTo: float<parameter>
      RightFrom: float<parameter>
      RightTo: float<parameter>
      Depth: int }

type private TraversalBranch = IncomingBranch | OutgoingBranch

[<Struct>]
type private ArcLengthLocation =
    { Subpath: Subpath
      At: float<length>
      Total: float<length>
      Closed: bool }

[<Struct>]
type private IntersectionPiece =
    { Segment: Segment
      From: float<parameter>
      To: float<parameter> }

[<Struct>]
type private DistanceMinimum =
    { LeftT: float<parameter>
      RightT: float<parameter>
      DistanceSquared: float<length^2> }

[<Struct>]
type private RawTerminalWindow =
    { Left: IntersectionPiece
      Right: IntersectionPiece
      StartLeftT: float<parameter>
      StartRightT: float<parameter> }

[<Struct>]
type private ProjectionWindow =
    { Left: IntersectionPiece
      Right: IntersectionPiece
      RemainingDepth: int }

/// Intersections, closest-point pairs, projections, and ray crossings.
[<RequireQualifiedAccess>]
module Intersections =
    let defaultOptions =
        { Tolerance = 1.0e-9<length>
          MaxDepth = 48
          ParameterSnap = NoParameterSnap }

    let defaultSelfIntersectionOptions =
        { MinimumArcLengthSeparation = 1.0e-9<length>
          DistanceTolerance = 1.0e-9<length> }

    let defaultClassificationOptions =
        { DirectionOptions = Segment.defaultDirectionOptions
          AngularTolerance = 1.0e-7<degree>
          DistanceTolerance = 1.0e-12<length>
          LengthOptions = Segment.defaultLengthOptions
          InitialArcLength = 1.0e-6<length>
          MaximumArcLength = 0.25<length>
          MaxSamplingSteps = 18 }

    let private parameterTolerance = 1.0e-9<parameter>
    let private enclosureSlack = 1.0e-12<length>
    let private terminalSubdivisionTolerance = 0.01<length>

    let private parameter value = Parameter.fromFloat value
    let private ratio (value: float<parameter>) = Parameter.ratio value
    let private clamp01 value = max 0.0<parameter> (min 1.0<parameter> value)
    let private interpolate
        (fromValue: float<parameter>)
        (toValue: float<parameter>)
        (portion: float<parameter>)
        : float<parameter> =
        fromValue + ratio portion * (toValue - fromValue)

    let private midpoint (left: Point<length>) (right: Point<length>) =
        Point.create ((left.X + right.X) / 2.0) ((left.Y + right.Y) / 2.0)


    let private pointsNear tolerance left right = Point.squaredDistance left right <= tolerance * tolerance

    let private endpointParameterScore (intersection: SegmentIntersection) =
        min (abs intersection.LeftT) (abs (1.0<parameter> - intersection.LeftT))
        + min (abs intersection.RightT) (abs (1.0<parameter> - intersection.RightT))

    let private insert
        (candidate: SegmentIntersection)
        (existing: SegmentIntersection list) =
        // Intersection identity is an address pair, not a geometric position:
        // a closed or retraced curve can visit that position more than once.
        match existing |> List.tryFindIndex (fun found ->
            abs (found.LeftT - candidate.LeftT) <= parameterTolerance
            && abs (found.RightT - candidate.RightT) <= parameterTolerance) with
        | Some index when endpointParameterScore candidate < endpointParameterScore existing[index] ->
            existing |> List.mapi (fun current value -> if current = index then candidate else value)
        | Some _ -> existing
        | None -> candidate :: existing


    let private cross (left: Point<'Left>) (right: Point<'Right>) =
        left.X * right.Y - left.Y * right.X

    let private chordCrossing p p2 q q2 =
        let r = Point.displacement p p2
        let s = Point.displacement q q2
        let denominator = cross r s
        let rSquared = Point.squaredNorm r
        let sSquared = Point.squaredNorm s
        if rSquared <= 0.0<length^2>
           || sSquared <= 0.0<length^2>
           || denominator * denominator <= 1.0e-18 * rSquared * sSquared then None
        else
            let offset = Point.displacement p q
            let leftT = cross offset s / denominator |> Parameter.fromFloat
            let rightT = cross offset r / denominator |> Parameter.fromFloat
            if leftT >= -1e-12<parameter> && leftT <= 1.0<parameter> + 1e-12<parameter>
               && rightT >= -1e-12<parameter> && rightT <= 1.0<parameter> + 1e-12<parameter> then Some(clamp01 leftT, clamp01 rightT)
            else None

    let private chordClosestParameters p p2 q q2 =
        let u = Point.displacement p p2
        let v = Point.displacement q q2
        let w = Point.displacement q p
        let a = Point.dot u u
        let b = Point.dot u v
        let c = Point.dot v v
        let d = Point.dot u w
        let e = Point.dot v w
        let denominator = a * c - b * b
        let wellConditioned =
            a > 0.0<length^2>
            && c > 0.0<length^2>
            && abs denominator > 1.0e-18 * a * c
        let leftT =
            if a = 0.0<length^2> then 0.0<parameter>
            elif c = 0.0<length^2> then clamp01 (Parameter.fromFloat(float (-d / a)))
            elif wellConditioned then clamp01 (Parameter.fromFloat(float ((b * e - c * d) / denominator)))
            else clamp01 (Parameter.fromFloat(float (-d / a)))
        let rightT =
            if c = 0.0<length^2> then 0.0<parameter>
            else clamp01 (Parameter.fromFloat(float ((b * (Parameter.ratio leftT) + e) / c)))
        let leftT =
            if a = 0.0<length^2> then 0.0<parameter>
            else clamp01 (Parameter.fromFloat(float ((b * (Parameter.ratio rightT) - d) / a)))
        leftT, rightT

    let private directionsIndependent left right =
        let leftSquared = Point.squaredNorm left
        let rightSquared = Point.squaredNorm right
        let determinant = cross left right
        leftSquared > 0.0<_>
        && rightSquared > 0.0<_>
        && determinant * determinant > 1.0e-18 * leftSquared * rightSquared


    let private initialWindows maxDepth =
        [ for leftIndex in 0 .. 7 do
              for rightIndex in 0 .. 7 do
                  let leftFrom = parameter (float leftIndex / 8.0)
                  let leftTo = parameter (float (leftIndex + 1) / 8.0)
                  let rightFrom = parameter (float rightIndex / 8.0)
                  let rightTo = parameter (float (rightIndex + 1) / 8.0)
                  yield
                      { LeftFrom = leftFrom
                        LeftTo = leftTo
                        RightFrom = rightFrom
                        RightTo = rightTo
                        Depth = maxDepth } ]
        |> List.rev

    let private splitNine window =
        let leftA = interpolate window.LeftFrom window.LeftTo (parameter (1.0/3.0))
        let leftB = interpolate window.LeftFrom window.LeftTo (parameter (2.0/3.0))
        let rightA = interpolate window.RightFrom window.RightTo (parameter (1.0/3.0))
        let rightB = interpolate window.RightFrom window.RightTo (parameter (2.0/3.0))
        [ for leftFrom,leftTo in [window.LeftFrom,leftA;leftA,leftB;leftB,window.LeftTo] do
              for rightFrom,rightTo in [window.RightFrom,rightA;rightA,rightB;rightB,window.RightTo] do
                  yield
                      { LeftFrom = leftFrom
                        LeftTo = leftTo
                        RightFrom = rightFrom
                        RightTo = rightTo
                        Depth = window.Depth - 1 } ]
        |> List.filter (fun child ->
            child.LeftFrom < child.LeftTo && child.RightFrom < child.RightTo
            && (child.LeftFrom > window.LeftFrom || child.LeftTo < window.LeftTo
                || child.RightFrom > window.RightFrom || child.RightTo < window.RightTo))


    let rec private arcEnclosingPoints (arc: CenterArcData) (fromT: float<parameter>) (toT: float<parameter>) =
        let middle = fromT + (toT - fromT) / 2.0
        let span = arc.DeltaAngle * float (toT - fromT)
        if abs span > 90.0<degree> then
            arcEnclosingPoints arc fromT middle @ arcEnclosingPoints arc middle toT
        else
            let a = Ellipse.arcPoint arc fromT
            let b = Ellipse.arcPoint arc toT
            let m = Ellipse.arcPoint arc middle
            let divisor = Trig.cosDegrees (span / 2.0)
            [ a; b; Point.create (arc.Center.X + (m.X - arc.Center.X) / divisor)
                                (arc.Center.Y + (m.Y - arc.Center.Y) / divisor) ]

    // The points' convex hull encloses the portion; boundary ordering is unnecessary.
    let private segmentEnclosingPoints segment fromT toT =
        match segment with
        | Arc arc -> Ellipse.endpointToCenter arc |> Result.mapError (fun _ -> DegenerateArc)
                     |> Result.map (fun center -> arcEnclosingPoints center fromT toT)
        | _ -> Segment.between segment fromT toT |> Result.bind (function
                   | Line(a,b) -> Ok [a;b]
                   | QuadraticBezier(a,b,c) -> Ok [a;b;c]
                   | CubicBezier(a,b,c,d) -> Ok [a;b;c;d]
                   | Arc _ -> Error DegenerateArc)

    let rec private enclosingPointAxes (points: Point<length> list) =
        match points with
        | [] -> []
        | a :: rest ->
            List.fold (fun axes b ->
                let dx = b.X - a.X
                let dy = b.Y - a.Y
                Point.create -dy dx :: Point.create dx dy :: axes) (enclosingPointAxes rest) rest

    let private enclosingProjectionInterval (points: Point<length> list) (origin: Point<length>) (axis: Point<length>) =
        let value (p: Point<length>) = (p.X - origin.X) * axis.X + (p.Y - origin.Y) * axis.Y
        let initial = value (List.head points)
        List.fold (fun (a,b) p -> let v = value p in min a v, max b v) (initial,initial) (List.tail points)

    let private enclosingPointsDisjoint (left: Point<length> list) (right: Point<length> list) =
        match left,right with
        | [],_ | _,[] -> false
        | origin :: _, _ ->
            let scale = List.fold (fun scale (p: Point<length>) ->
                max scale (max (max (abs p.X) (abs p.Y)) (max (abs (p.X-origin.X)) (abs (p.Y-origin.Y))))) 0.0<length> (left @ right)
            let axes = Point.create 1.0<length> 0.0<length> :: Point.create 0.0<length> 1.0<length> :: (enclosingPointAxes left @ enclosingPointAxes right)
            axes |> List.exists (fun axis ->
                let a,b = enclosingProjectionInterval left origin axis
                let c,d = enclosingProjectionInterval right origin axis
                let margin = 1e-12 * scale * (abs axis.X + abs axis.Y)
                b + margin < c || d + margin < a)


    let private pieceBoundingBox piece =
        Segment.between piece.Segment piece.From piece.To |> Result.bind Segment.boundingBox

    let private splitPiece piece =
        let middle = (piece.From + piece.To) / 2.0
        { piece with To = middle }, { piece with From = middle }


    let private splitPieceThirds piece =
        let firstTo = interpolate piece.From piece.To (parameter (1.0 / 3.0))
        let secondTo = interpolate piece.From piece.To (parameter (2.0 / 3.0))
        [ { piece with To = firstTo }, 0.0<parameter>
          { piece with From = firstTo; To = secondTo }, 0.5<parameter>
          { piece with From = secondTo }, 1.0<parameter> ]

    let private addTerminalWindowGrid left right windows =
        [ for leftPiece, startLeftT in splitPieceThirds left do
              for rightPiece, startRightT in splitPieceThirds right do
                  yield
                      { Left = leftPiece
                        Right = rightPiece
                        StartLeftT = startLeftT
                        StartRightT = startRightT } ] @ windows

    let private globalDistanceMinimumAt left right leftT rightT =
        Segment.point left.Segment leftT
        |> Result.bind (fun leftPoint ->
            Segment.point right.Segment rightT
            |> Result.map (fun rightPoint ->
                { LeftT = leftT
                  RightT = rightT
                  DistanceSquared = Point.squaredDistance leftPoint rightPoint }))

    let private initialDescentSeed leftT rightT =
        let leftBoundary = leftT = 0.0<parameter> || leftT = 1.0<parameter>
        let rightBoundary = rightT = 0.0<parameter> || rightT = 1.0<parameter>
        leftBoundary && rightBoundary
        || (leftT = 0.5<parameter> && rightT = 0.5<parameter>)

    let private bestProposal first second =
        let firstMinimum, _, _ = first
        let secondMinimum, _, _ = second
        if secondMinimum.DistanceSquared < firstMinimum.DistanceSquared then second else first

    let private distanceProposal left right current step useTangentLine =
        Segment.point left.Segment current.LeftT
        |> Result.bind (fun leftPoint ->
            Segment.point right.Segment current.RightT
            |> Result.bind (fun rightPoint ->
                Segment.derivative left.Segment current.LeftT
                |> Result.bind (fun leftDerivative ->
                    Segment.derivative right.Segment current.RightT
                    |> Result.bind (fun rightDerivative ->
                        let separation = Point.displacement rightPoint leftPoint
                        let leftSpeedSquared = max (Point.squaredNorm leftDerivative) 1.0e-18<_>
                        let rightSpeedSquared = max (Point.squaredNorm rightDerivative) 1.0e-18<_>
                        let leftGradient = 2.0 * Point.dot separation leftDerivative
                        let rightGradient = -2.0 * Point.dot separation rightDerivative
                        let rawGradientLeft = current.LeftT - step * leftGradient / leftSpeedSquared
                        let rawGradientRight = current.RightT - step * rightGradient / rightSpeedSquared
                        globalDistanceMinimumAt left right (clamp01 rawGradientLeft) (clamp01 rawGradientRight)
                        |> Result.bind (fun gradientMinimum ->
                            let gradient = gradientMinimum, rawGradientLeft, rawGradientRight
                            if current.DistanceSquared > 0.0001<length> * 0.0001<length> then Ok gradient
                            elif useTangentLine then
                                let determinant = cross leftDerivative rightDerivative
                                if not (directionsIndependent leftDerivative rightDerivative) then Ok gradient
                                else
                                    let deltaLeft = -cross separation rightDerivative / determinant
                                    let deltaRight = -cross separation leftDerivative / determinant
                                    let rawLeft = current.LeftT + deltaLeft
                                    let rawRight = current.RightT + deltaRight
                                    globalDistanceMinimumAt left right (clamp01 rawLeft) (clamp01 rawRight)
                                    |> Result.map (fun candidate -> bestProposal gradient (candidate, rawLeft, rawRight))
                            else
                                let a = Point.dot leftDerivative leftDerivative
                                let b = -Point.dot leftDerivative rightDerivative
                                let c = Point.dot rightDerivative rightDerivative
                                let g1 = Point.dot leftDerivative separation
                                let g2 = -Point.dot rightDerivative separation
                                let determinant = a * c - b * b
                                if a = 0.0<_> || c = 0.0<_> || determinant <= 1.0e-18 * a * c then Ok gradient
                                else
                                    let deltaLeft = (b * g2 - c * g1) / determinant
                                    let deltaRight = (b * g1 - a * g2) / determinant
                                    let rawLeft = current.LeftT + deltaLeft
                                    let rawRight = current.RightT + deltaRight
                                    globalDistanceMinimumAt left right (clamp01 rawLeft) (clamp01 rawRight)
                                    |> Result.map (fun candidate -> bestProposal gradient (candidate, rawLeft, rawRight)))))))

    let private runDescent left right (tolerance: float<length>) start =
        let rec loop current (step: float) iterations =
            if iterations <= 0 || step <= 1.0e-12 then Ok current
            else
                distanceProposal left right current step (iterations % 2 = 0)
                |> Result.bind (fun (proposal, _, _) ->
                    if proposal.DistanceSquared >= current.DistanceSquared then
                        loop current (step / 2.0) (iterations - 1)
                    else
                        let improvement = current.DistanceSquared - proposal.DistanceSquared
                        if proposal.DistanceSquared = 0.0<length^2>
                           || improvement <= tolerance * tolerance * 1.0e-6 then Ok proposal
                        else loop proposal step (iterations - 1))
        loop start 1.0 45

    let private minimaFromTerminalWindows windows tolerance =
        windows
        |> List.filter (fun window -> initialDescentSeed window.StartLeftT window.StartRightT)
        |> List.fold (fun state window ->
            state
            |> Result.bind (fun minima ->
                let leftT = interpolate window.Left.From window.Left.To window.StartLeftT
                let rightT = interpolate window.Right.From window.Right.To window.StartRightT
                globalDistanceMinimumAt window.Left window.Right leftT rightT
                |> Result.bind (runDescent window.Left window.Right tolerance)
                |> Result.map (fun minimum -> minimum :: minima))) (Ok [])

    let private boundaryMinima left right options =
        let projectionOptions =
            { Segment.defaultDistanceOptions with
                Tolerance = options.Tolerance
                MaxIterations = options.MaxDepth }
        let leftPiece = { Segment = left; From = 0.0<parameter>; To = 1.0<parameter> }
        let rightPiece = { Segment = right; From = 0.0<parameter>; To = 1.0<parameter> }
        let projectLeft leftT =
            Segment.point left leftT
            |> Result.bind (fun pointValue ->
                Segment.projectionWith right pointValue projectionOptions
                |> Result.bind (fun (rightT, _, _) -> globalDistanceMinimumAt leftPiece rightPiece leftT rightT))
        let projectRight rightT =
            Segment.point right rightT
            |> Result.bind (fun pointValue ->
                Segment.projectionWith left pointValue projectionOptions
                |> Result.bind (fun (leftT, _, _) -> globalDistanceMinimumAt leftPiece rightPiece leftT rightT))
        [ projectLeft 0.0<parameter>; projectLeft 1.0<parameter>
          projectRight 0.0<parameter>; projectRight 1.0<parameter> ]
        |> List.fold (fun state candidate ->
            state |> Result.bind (fun minima -> candidate |> Result.map (fun minimum -> minimum :: minima))) (Ok [])


    let private boxDistanceSquared (left: BoundingBox) (right: BoundingBox) =
        let dx =
            if left.Max.X < right.Min.X then right.Min.X - left.Max.X
            elif right.Max.X < left.Min.X then left.Min.X - right.Max.X
            else 0.0<length>
        let dy =
            if left.Max.Y < right.Min.Y then right.Min.Y - left.Max.Y
            elif right.Max.Y < left.Min.Y then left.Min.Y - right.Max.Y
            else 0.0<length>
        dx * dx + dy * dy

    let private closerMinimum best candidate =
        if candidate.DistanceSquared < best.DistanceSquared then candidate else best

    let private collectProjectionTerminalWindows left right best maxDepth =
        let rec generations current next best windows =
            match current with
            | [] when List.isEmpty next -> Ok(windows, best)
            | [] -> generations next [] best windows
            | window :: rest ->
                pieceBoundingBox window.Left
                |> Result.bind (fun leftBox ->
                    pieceBoundingBox window.Right
                    |> Result.bind (fun rightBox ->
                        if boxDistanceSquared leftBox rightBox > best.DistanceSquared then
                            generations rest next best windows
                        elif window.RemainingDepth <= 0
                             || (BoundingBox.diameter leftBox <= terminalSubdivisionTolerance
                                 && BoundingBox.diameter rightBox <= terminalSubdivisionTolerance) then
                            let added = addTerminalWindowGrid window.Left window.Right windows
                            added
                            |> List.take 9
                            |> List.fold (fun state terminal ->
                                state
                                |> Result.bind (fun best ->
                                    let leftT = interpolate terminal.Left.From terminal.Left.To terminal.StartLeftT
                                    let rightT = interpolate terminal.Right.From terminal.Right.To terminal.StartRightT
                                    globalDistanceMinimumAt terminal.Left terminal.Right leftT rightT
                                    |> Result.map (closerMinimum best))) (Ok best)
                            |> Result.bind (fun best -> generations rest next best added)
                        elif BoundingBox.diameter leftBox >= BoundingBox.diameter rightBox then
                            let first, second = splitPiece window.Left
                            let children =
                                [ { window with Left = second; RemainingDepth = window.RemainingDepth - 1 }
                                  { window with Left = first; RemainingDepth = window.RemainingDepth - 1 } ]
                            generations rest (children @ next) best windows
                        else
                            let first, second = splitPiece window.Right
                            let children =
                                [ { window with Right = second; RemainingDepth = window.RemainingDepth - 1 }
                                  { window with Right = first; RemainingDepth = window.RemainingDepth - 1 } ]
                            generations rest (children @ next) best windows))
        generations
            [ { Left = left; Right = right; RemainingDepth = maxDepth } ]
            [] best []

    let private segmentPairProjectionMinima left right options =
        let leftPiece = { Segment = left; From = 0.0<parameter>; To = 1.0<parameter> }
        let rightPiece = { Segment = right; From = 0.0<parameter>; To = 1.0<parameter> }
        boundaryMinima left right options
        |> Result.bind (fun boundaries ->
            let best = boundaries |> List.reduce closerMinimum
            collectProjectionTerminalWindows leftPiece rightPiece best options.MaxDepth
            |> Result.bind (fun (windows, best) ->
                minimaFromTerminalWindows (List.rev windows) options.Tolerance
                |> Result.map (fun terminal -> best :: boundaries @ terminal)))

    let internal validateOptions options =
        if options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidIntersectionTolerance options.Tolerance)
        elif options.MaxDepth <= 0 then Error(InvalidIntersectionMaxDepth options.MaxDepth)
        else
            match options.ParameterSnap with
            | DecimalParameterSnap exponent when exponent < 1 || exponent > 15 ->
                Error(InvalidIntersectionParameterSnapExponent exponent)
            | _ -> Ok()

    let private snapCandidates t exponent =
        let scale = Parameter.fromFloat(0.1 ** float exponent)
        let baseValue = floor (float (t / scale))
        [ 0.0<parameter>, -1
          baseValue * scale, 0
          (baseValue + 1.0) * scale, 0
          (baseValue + 1.0 / 3.0) * scale, 1
          (baseValue + 2.0 / 3.0) * scale, 1
          1.0<parameter>, -1 ]
        |> List.map (fun (candidate, rank) ->
            let candidate = InternalNumber.normalizeZero candidate
            candidate, (if candidate = 0.0<parameter> || candidate = 1.0<parameter> then -1 else rank))
        |> List.filter (fun (candidate, _) ->
            candidate >= 0.0<parameter> && candidate <= 1.0<parameter> && abs (candidate - t) <= scale)

    let private polishIntersection left right exponent (intersection: SegmentIntersection) =
        let originalRank = 100
        [ for leftT, leftRank in snapCandidates intersection.LeftT exponent do
              for rightT, rightRank in snapCandidates intersection.RightT exponent do
                  yield leftT, rightT, leftRank + rightRank ]
        |> List.fold (fun state (leftT, rightT, rank) ->
            state
            |> Result.bind (fun best ->
                Segment.point left leftT
                |> Result.bind (fun leftPoint ->
                    Segment.point right rightT
                    |> Result.map (fun rightPoint ->
                        let candidate =
                            ({ LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint } : SegmentIntersection)
                        let distanceSquared = Point.squaredDistance leftPoint rightPoint
                        match best with
                        | None -> Some(candidate, distanceSquared, rank)
                        | Some(_, bestDistanceSquared, bestRank) ->
                            let bestDistance = sqrt (float bestDistanceSquared)
                            let candidateDistance = sqrt (float distanceSquared)
                            let slack = 1.0e-12
                            let candidateTies = candidateDistance <= bestDistance + slack
                            let bestTies = bestDistance <= candidateDistance + slack
                            if distanceSquared < bestDistanceSquared then
                                if bestTies && rank >= bestRank then best else Some(candidate, distanceSquared, rank)
                            elif candidateTies && rank < bestRank then Some(candidate, distanceSquared, rank)
                            else best))))
            (Segment.point left intersection.LeftT
             |> Result.bind (fun leftPoint ->
                 Segment.point right intersection.RightT
                 |> Result.map (fun rightPoint ->
                     Some(intersection, Point.squaredDistance leftPoint rightPoint, originalRank))))
        |> Result.map (Option.map (fun (candidate, _, _) -> candidate) >> Option.defaultValue intersection)

    let private polishAndCertify left right options intersections =
        let polished =
            match options.ParameterSnap with
            | NoParameterSnap -> Ok intersections
            | DecimalParameterSnap exponent ->
                intersections
                |> List.fold (fun state intersection ->
                    state
                    |> Result.bind (fun found ->
                        polishIntersection left right exponent intersection
                        |> Result.map (fun candidate -> insert candidate found))) (Ok [])
        polished
        |> Result.bind (fun values ->
            values
            |> List.fold (fun state intersection ->
                state
                |> Result.bind (fun certified ->
                    Segment.point left intersection.LeftT
                    |> Result.bind (fun leftPoint ->
                        Segment.point right intersection.RightT
                        |> Result.bind (fun rightPoint ->
                            let leftDistance = Point.distance leftPoint intersection.Point
                            let rightDistance = Point.distance rightPoint intersection.Point
                            if leftDistance <= options.Tolerance && rightDistance <= options.Tolerance then
                                Ok(intersection :: certified)
                            else Error(InternalUncertifiedSegmentIntersection(
                                leftDistance, rightDistance, options.Tolerance)))))) (Ok [])
            |> Result.map (List.sortBy (fun item -> item.LeftT, item.RightT)))

    let private lineProjectionT pointValue startPoint endPoint =
        let direction = Point.displacement startPoint endPoint
        let lengthSquared = Point.squaredNorm direction
        if lengthSquared = 0.0<length^2> then 0.0<parameter>
        else Parameter.fromFloat(float (Point.dot (Point.displacement startPoint pointValue) direction / lengthSquared))

    let private inUnitRange value tolerance =
        value >= -tolerance && value <= 1.0<parameter> + tolerance

    let private parameterToleranceForChord direction tolerance =
        let chord = Point.norm direction
        if chord <= 0.0<length> then parameterTolerance
        else Parameter.fromFloat(float (tolerance / chord))

    let private segmentDefiningPoints = function
        | Line(startPoint, endPoint) -> Some [ startPoint; endPoint ]
        | QuadraticBezier(startPoint, control, endPoint) -> Some [ startPoint; control; endPoint ]
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Some [ startPoint; control1; control2; endPoint ]
        | Arc _ -> None

    let private segmentProjectionOverlapsLine points lineStart lineEnd tolerance =
        match points with
        | [] -> false
        | first :: rest ->
            let firstT = lineProjectionT first lineStart lineEnd
            let minimum, maximum =
                rest
                |> List.fold (fun (minimum, maximum) pointValue ->
                    let t = lineProjectionT pointValue lineStart lineEnd
                    min minimum t, max maximum t) (firstT, firstT)
            min 1.0<parameter> maximum - max 0.0<parameter> minimum
            > Parameter.fromFloat(float tolerance)

    let private segmentLiesOnLine segmentValue lineStart lineEnd tolerance =
        let direction = Point.displacement lineStart lineEnd
        let directionLength = Point.norm direction
        match directionLength <= 0.0<length>, segmentDefiningPoints segmentValue with
        | true, _
        | false, None -> false
        | false, Some points ->
            points
            |> List.forall (fun pointValue ->
                abs (cross direction (Point.displacement lineStart pointValue)) / directionLength <= tolerance)
            && segmentProjectionOverlapsLine points lineStart lineEnd tolerance

    let private lineSegmentsAreCollinear lineStart lineEnd segmentStart segmentEnd tolerance =
        let direction = Point.displacement lineStart lineEnd
        let directionLength = Point.norm direction
        directionLength > 0.0<length>
        && abs (cross direction (Point.displacement lineStart segmentStart)) / directionLength <= tolerance
        && abs (cross direction (Point.displacement lineStart segmentEnd)) / directionLength <= tolerance

    let private collinearLinePointIntersections
        lineStart lineEnd segmentStart segmentEnd lineIsLeft lineParameterTolerance =
        let segmentStartLineT = lineProjectionT segmentStart lineStart lineEnd
        let segmentEndLineT = lineProjectionT segmentEnd lineStart lineEnd
        let overlapStart = max 0.0<parameter> (min segmentStartLineT segmentEndLineT)
        let overlapEnd = min 1.0<parameter> (max segmentStartLineT segmentEndLineT)
        if overlapEnd < overlapStart - lineParameterTolerance then Ok []
        elif overlapEnd - overlapStart <= lineParameterTolerance then
            let lineT = clamp01 ((overlapStart + overlapEnd) / 2.0)
            let pointValue =
                let direction = Point.displacement lineStart lineEnd
                Point.translate lineStart (Point.scale (Parameter.ratio lineT) direction)
            let segmentT = lineProjectionT pointValue segmentStart segmentEnd |> clamp01
            Ok [ if lineIsLeft then
                     { LeftT = lineT; RightT = segmentT; Point = pointValue }
                 else
                     { LeftT = segmentT; RightT = lineT; Point = pointValue } ]
        else Error OverlappingSegments

    let private lineSegmentIntersectionsByRay
        lineStart lineEnd lineIsLeft segmentValue options lineParameterTolerance =
        let lineDirection = Point.displacement lineStart lineEnd
        let chord = Point.norm lineDirection
        if chord <= 0.0<length> then Ok []
        else
            let unitDirection = Point.scale (1.0 / chord) lineDirection
            // Center/angle arithmetic can move an exact stored arc endpoint
            // beyond its sweep near tangency. Seed both stored endpoints too.
            let endpoints =
                [0.0<parameter>, Segment.start segmentValue; 1.0<parameter>, Segment.finish segmentValue]
                |> List.choose (fun (segmentT, pointValue) ->
                    let lineT = lineProjectionT pointValue lineStart lineEnd |> clamp01
                    let projected = Point.interpolate lineStart lineEnd lineT
                    if Point.distance pointValue projected > options.Tolerance then None
                    elif lineIsLeft then Some {LeftT = lineT; RightT = segmentT; Point = pointValue}
                    else Some {LeftT = segmentT; RightT = lineT; Point = pointValue})
            Segment.rayCrossingsWith segmentValue lineStart unitDirection
                { Segment.defaultCrossingOptions with
                    Samples = 1
                    SignedLineDistanceTolerance = options.Tolerance
                    MaxIterations = options.MaxDepth * 4 }
            |> Result.bind (fun crossings ->
                crossings
                |> List.fold (fun found (segmentT, distanceAlongLine) ->
                    let lineT = Parameter.fromFloat(float (distanceAlongLine / chord))
                    if not (inUnitRange lineT lineParameterTolerance) then found
                    else
                        match Segment.point segmentValue segmentT with
                        | Error _ -> found
                        | Ok pointValue ->
                            let intersection =
                                if lineIsLeft then
                                    { LeftT = clamp01 lineT; RightT = clamp01 segmentT; Point = pointValue }
                                else
                                    { LeftT = clamp01 segmentT; RightT = clamp01 lineT; Point = pointValue }
                            insert intersection found) endpoints
                |> List.rev
                |> Ok)

    let private lineSegmentIntersections lineStart lineEnd lineIsLeft segmentValue options =
        let lineDirection = Point.displacement lineStart lineEnd
        let lineParameterTolerance = parameterToleranceForChord lineDirection options.Tolerance
        if segmentLiesOnLine segmentValue lineStart lineEnd options.Tolerance then Error OverlappingSegments
        else
            match segmentValue with
            | Line(segmentStart, segmentEnd)
                when lineSegmentsAreCollinear lineStart lineEnd segmentStart segmentEnd options.Tolerance ->
                collinearLinePointIntersections
                    lineStart lineEnd segmentStart segmentEnd lineIsLeft lineParameterTolerance
            | _ ->
                lineSegmentIntersectionsByRay
                    lineStart lineEnd lineIsLeft segmentValue options lineParameterTolerance


    let private elizabethParameterResolution = 1e-9<parameter>
    let private elizabethDedupeResolution = 1e-7<parameter>

    let private elizabethTryMap f items =
        let rec loop items reversed =
            match items with
            | [] -> Ok(List.rev reversed)
            | first :: rest ->
                match f first with
                | Error error -> Error error
                | Ok value -> loop rest (value :: reversed)
        loop items []

    let private elizabethPointDistance a b = sqrt(Point.squaredDistance a b)

    let private elizabethEndpointRank (candidate: SegmentIntersection) =
        (if candidate.LeftT = 0.0<parameter> || candidate.LeftT = 1.0<parameter> then 0 else 1)
        + (if candidate.RightT = 0.0<parameter> || candidate.RightT = 1.0<parameter> then 0 else 1)

    let private elizabethRankCandidates left right (candidates: SegmentIntersection list) =
        candidates |> elizabethTryMap (fun candidate ->
            Segment.point left candidate.LeftT |> Result.bind (fun a ->
                Segment.point right candidate.RightT |> Result.map (fun b -> candidate, elizabethPointDistance a b))
            |> Result.mapError CurveSolverPathError)
        |> Result.map (List.sortBy (fun (candidate,residual) ->
            elizabethEndpointRank candidate,residual,candidate.LeftT,candidate.RightT) >> List.map fst)

    let private elizabethCandidateDistance (a: SegmentIntersection) (b: SegmentIntersection) = max (abs(a.LeftT-b.LeftT)) (abs(a.RightT-b.RightT))

    let private elizabethFinishCandidates left right candidates =
        elizabethRankCandidates left right candidates |> Result.map (fun ranked ->
            ranked |> List.fold (fun kept candidate ->
                if kept |> List.exists (fun other -> elizabethCandidateDistance candidate other <= elizabethDedupeResolution)
                then kept else candidate :: kept) []
            |> List.sortBy (fun candidate -> candidate.LeftT,candidate.RightT))

    let private selectSpatiallyDiverse ranked location limit minimumSeparation =
        let rec pass remaining selected slots separation =
            let deferred,selected,slots = remaining |> List.fold (fun (deferred,selected,slots) ((_,(a,b)) as candidate) ->
                let blocked = separation > 0.0<parameter> && selected |> List.exists (fun (_, (c,d)) -> max(abs(a-c))(abs(b-d)) < separation)
                if slots <= 0 || blocked then candidate :: deferred,selected,slots
                else deferred,candidate :: selected,slots-1) ([],selected,slots)
            if slots <= 0 || List.isEmpty deferred || separation <= minimumSeparation then List.rev selected |> List.map fst
            else
                let next = if separation <= 2.220446049250313e-16<parameter> then minimumSeparation else max minimumSeparation (separation/2.0)
                pass (List.rev deferred) selected slots next
        pass (List.map (fun item -> item,location item) ranked) [] limit (max 0.5<parameter> minimumSeparation)

    let private elizabethDegree = function Line _ -> 1 | QuadraticBezier _ | Arc _ -> 2 | CubicBezier _ -> 3

    let private elizabethSelectCandidates left right candidates =
        elizabethRankCandidates left right candidates |> Result.map (fun ranked ->
            selectSpatiallyDiverse ranked (fun c -> c.LeftT,c.RightT) (elizabethDegree left * elizabethDegree right) elizabethDedupeResolution
            |> List.sortBy (fun c -> c.LeftT,c.RightT))

    let rec private elizabethTerminalNewton left right window t u tolerance remaining =
        Segment.point left t |> Result.bind (fun p ->
            Segment.point right u |> Result.bind (fun q ->
                if elizabethPointDistance p q <= tolerance then Ok(Some {LeftT=t;RightT=u;Point=midpoint p q})
                elif remaining <= 0 then Ok None
                else Segment.derivative left t |> Result.bind (fun v ->
                    Segment.derivative right u |> Result.bind (fun w ->
                        if not(directionsIndependent v w) then Ok None
                        else
                            let determinant = cross v w
                            let delta = Point.displacement p q
                            let nextT = t + cross delta w / determinant
                            let nextU = u - cross v delta / determinant
                            if nextT >= window.LeftFrom && nextT <= window.LeftTo && nextU >= window.RightFrom && nextU <= window.RightTo && (nextT <> t || nextU <> u)
                            then elizabethTerminalNewton left right window nextT nextU tolerance (remaining-1)
                            else Ok None))))


    let private elizabethTerminalCandidates left right window tolerance =
        let a,b,c,d = window.LeftFrom,window.LeftTo,window.RightFrom,window.RightTo
        Segment.point left a |> Result.bind (fun p ->
            Segment.point left b |> Result.bind (fun q ->
                Segment.point right c |> Result.bind (fun r ->
                    Segment.point right d |> Result.bind (fun s ->
                        let t,u = chordCrossing p q r s |> Option.defaultWith (fun () -> chordClosestParameters p q r s)
                        [a,c;a,d;b,c;b,d;(a+b)/2.0,(c+d)/2.0;interpolate a b t,interpolate c d u]
                        |> elizabethTryMap (fun (t,u) ->
                            elizabethTerminalNewton left right window t u tolerance 8)
                        |> Result.map (List.choose id)))))

    // Enclosing points are constructed once; no curve extrema or axis boxes.
    let private elizabethWindowOverlaps left right window =
        segmentEnclosingPoints left window.LeftFrom window.LeftTo |> Result.bind (fun a ->
            segmentEnclosingPoints right window.RightFrom window.RightTo |> Result.map (fun b ->
                not(enclosingPointsDisjoint a b)))


    type private ElizabethEvaluationCache =
        { Samples: Map<float<parameter>*float<parameter>,Point<length>*Point<length>*float<length>>
          Lookups: int; Hits: int
          LeftPoints: Map<float<parameter>,Point<length>>; RightPoints: Map<float<parameter>,Point<length>> }

    let private elizabethCachedPoint segment t points =
        match Map.tryFind t points with
        | Some p -> Ok(p,points)
        | None -> Segment.point segment t |> Result.map (fun p -> p,Map.add t p points)

    let private elizabethCachedPair left right u v cache =
        let key = InternalNumber.normalizeZero u,InternalNumber.normalizeZero v
        let cache = {cache with Lookups=cache.Lookups+1}
        match Map.tryFind key cache.Samples with
        | Some sample -> Ok(sample,{cache with Hits=cache.Hits+1})
        | None -> elizabethCachedPoint left (fst key) cache.LeftPoints |> Result.bind (fun (p,lp) ->
            elizabethCachedPoint right (snd key) cache.RightPoints |> Result.map (fun (q,rp) ->
                let sample = p,q,elizabethPointDistance p q
                sample,{cache with Samples=Map.add key sample cache.Samples;LeftPoints=lp;RightPoints=rp}))

    let private elizabethWindowScore left right window cache =
        let a,b,c,d = window.LeftFrom,window.LeftTo,window.RightFrom,window.RightTo
        elizabethCachedPair left right a c cache |> Result.bind (fun ((p,r,firstDistance),cache) ->
            elizabethCachedPair left right b d cache |> Result.bind (fun ((q,s,lastDistance),cache) ->
                let crossing = chordCrossing p q r s
                let parameters = [a,d;b,c;(a+b)/2.0,(c+d)/2.0]
                let parameters = match crossing with None -> parameters | Some(u,v) -> parameters @ [interpolate a b u,interpolate c d v]
                let initial = min firstDistance lastDistance,cache,(if firstDistance <= lastDistance then a,c else b,d)
                parameters |> List.fold (fun state (u,v) -> state |> Result.bind (fun (score,cache,best) ->
                    elizabethCachedPair left right u v cache |> Result.map (fun ((_,_,distance),cache) ->
                        if distance < score then distance,cache,(u,v) else score,cache,best))) (Ok initial)
                |> Result.map (fun (score,cache,best) -> Option.isSome crossing,score,cache,best)))

    let private elizabethGenerationBudget generation decayStart =
        let budget = match decayStart with
                     | None -> 500
                     | Some _ when generation >= 12 -> 12
                     | Some start ->
                         let progress = float(max 0 (generation-start)) / float(12-start)
                         // Gleam float.round uses nearest integer (not bankers' rounding).
                         int(System.Math.Round(500.0 * System.Math.Pow(12.0/500.0,progress),System.MidpointRounding.AwayFromZero))
        budget,budget

    let private elizabethBeamSelect left right windows (crossingBudget,otherBudget) cache =
        let total = crossingBudget+otherBudget
        if List.length windows <= total then Ok(windows,0,0,cache)
        else
            windows |> List.fold (fun state window -> state |> Result.bind (fun (scored,cache) ->
                elizabethWindowScore left right window cache |> Result.map (fun (crossing,score,cache,best) ->
                    (window,crossing,score,best)::scored,cache))) (Ok([],cache))
            |> Result.mapError CurveSolverPathError
            |> Result.map (fun (scored,cache) ->
                let ranked = List.rev scored |> List.sortBy (fun (window,_,score,_) -> score,window.LeftFrom,window.RightFrom)
                let crossings,others = ranked |> List.partition (fun (_,crossing,_,_) -> crossing)
                let nc,no = List.length crossings,List.length others
                let keepC = min nc (max crossingBudget (total-no))
                let keepO = min no (total-keepC)
                let location (_,_,_,pair) = pair
                let kept = selectSpatiallyDiverse crossings location keepC 0.0<parameter> @ selectSpatiallyDiverse others location keepO 0.0<parameter>
                List.map (fun (window,_,_,_) -> window) kept,nc-keepC,no-keepO,cache)

    let private elizabethEndpointCandidates left right tolerance =
        [left,right,false;right,left,true] |> elizabethTryMap (fun (source,target,reversed) ->
            [0.0<parameter>;1.0<parameter>] |> elizabethTryMap (fun t ->
                Segment.point source t |> Result.bind (fun p ->
                    OverlapDetection.pointParameters target p tolerance |> Result.bind (fun matches ->
                        matches |> elizabethTryMap (fun u -> Segment.point target u |> Result.map (fun q ->
                            if reversed then {LeftT=u;RightT=t;Point=midpoint p q} else {LeftT=t;RightT=u;Point=midpoint p q})))))
            |> Result.map List.concat)
        |> Result.map List.concat

    let internal elizabethBeamIntersections left right options =
        validateOptions options |> Result.mapError CurveSolverPathError
        |> Result.bind (fun () ->
            let tolerance = min options.Tolerance 1e-13<length>
            elizabethEndpointCandidates left right tolerance |> Result.mapError CurveSolverPathError
            |> Result.bind (fun endpoints ->
                let rec generation pending report index decayStart cache =
                    match pending with
                    | [] -> elizabethFinishCandidates left right report.Intersections |> Result.bind (fun intersections ->
                        elizabethSelectCandidates left right intersections |> Result.map (fun selected ->
                            {report with Intersections=selected;DiscardedCandidates=List.length intersections-List.length selected}))
                    | _ ->
                        pending |> elizabethTryMap (fun window ->
                            elizabethWindowOverlaps left right window |> Result.map (fun overlaps -> window,overlaps))
                        |> Result.mapError CurveSolverPathError
                        |> Result.bind (fun overlapping ->
                            let survivors = overlapping |> List.filter snd |> List.map fst
                            let decayStart = match decayStart with None when List.length survivors > 1000 || index >= 5 -> Some index | _ -> decayStart
                            elizabethBeamSelect left right survivors (elizabethGenerationBudget index decayStart) cache
                            |> Result.bind (fun (kept,crossingLost,otherLost,cache) ->
                                let report = {report with Examined=report.Examined+List.length pending;DiscardedCrossing=report.DiscardedCrossing+crossingLost;DiscardedOther=report.DiscardedOther+otherLost;PeakRetained=max report.PeakRetained (List.length kept)}
                                kept |> List.fold (fun state window -> state |> Result.bind (fun (next,candidates) ->
                                    if window.LeftTo-window.LeftFrom <= elizabethParameterResolution && window.RightTo-window.RightFrom <= elizabethParameterResolution then
                                        elizabethTerminalCandidates left right window tolerance |> Result.mapError CurveSolverPathError
                                        |> Result.map (fun found -> next,found @ candidates)
                                    else
                                        let children = splitNine window
                                        if window.Depth <= 0 || List.isEmpty children then Error(CurveSolverDepthLimit(window.LeftFrom,window.LeftTo,window.RightFrom,window.RightTo))
                                        else Ok(List.fold (fun next child -> child::next) next children,candidates))) (Ok([],report.Intersections))
                                |> Result.bind (fun (next,candidates) -> generation next {report with Intersections=candidates} (index+1) decayStart cache)))
                generation (initialWindows options.MaxDepth)
                    {Intersections=endpoints;Examined=0;DiscardedCrossing=0;DiscardedOther=0;PeakRetained=0;DiscardedCandidates=0}
                    1 None {Samples=Map.empty;Lookups=0;Hits=0;LeftPoints=Map.empty;RightPoints=Map.empty}))

    let private curveCurveIntersections left right options =
        elizabethBeamIntersections left right options |> Result.map (fun report -> report.Intersections)
        |> Result.mapError (function
            | CurveSolverPathError error -> error
            | CurveSolverDepthLimit(a,b,c,d) -> IntersectionDepthLimitReached(a,b,c,d))

    let private segmentIntersectionsValidOptions left right options =
        match left, right with
        | Line(startPoint, endPoint), _ ->
            lineSegmentIntersections startPoint endPoint true right options
        | _, Line(startPoint, endPoint) ->
            lineSegmentIntersections startPoint endPoint false left options
        | _ -> curveCurveIntersections left right options

    let segmentWithoutOverlapPrecheckWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            segmentIntersectionsValidOptions left right options
            |> Result.bind (polishAndCertify left right options))

    let segmentWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            OverlapDetection.detect left right options.Tolerance
            |> Result.bind (function
                | _ :: _ -> Error OverlappingSegments
                | [] ->
                    segmentWithoutOverlapPrecheckWith left right options
                    |> Result.mapError (function
                        | OverlappingSegments -> InternalOverlapClassificationInconsistency
                        | error -> error)))

    let segment left right = segmentWith left right defaultOptions

    let private projectionAt left right leftT rightT =
        Segment.point left leftT
        |> Result.bind (fun leftPoint ->
            Segment.point right rightT
            |> Result.map (fun rightPoint ->
                ({ LeftT = leftT
                   RightT = rightT
                   LeftPoint = leftPoint
                   RightPoint = rightPoint
                   Distance = Point.distance leftPoint rightPoint } : SegmentSegmentProjection)))

    let private boundingBoxDistanceSquared (left: BoundingBox) (right: BoundingBox) =
        let dx =
            if left.Max.X < right.Min.X then right.Min.X - left.Max.X
            elif right.Max.X < left.Min.X then left.Min.X - right.Max.X
            else 0.0<length>
        let dy =
            if left.Max.Y < right.Min.Y then right.Min.Y - left.Max.Y
            elif right.Max.Y < left.Min.Y then left.Min.Y - right.Max.Y
            else 0.0<length>
        dx * dx + dy * dy

    let private projectionFromOverlap left right (overlap: RawOverlap) =
        projectionAt left right overlap.LeftFrom overlap.RightFrom

    let private lineLineProjection left right options =
        match left, right with
        | Line(leftStart, leftEnd), Line(rightStart, rightEnd) ->
            let leftDirection = Point.displacement leftStart leftEnd
            let rightDirection = Point.displacement rightStart rightEnd
            let denominator = cross leftDirection rightDirection
            if directionsIndependent leftDirection rightDirection then
                let betweenStarts = Point.displacement leftStart rightStart
                let leftT = Parameter.fromFloat(float (cross betweenStarts rightDirection / denominator))
                let rightT = Parameter.fromFloat(float (cross betweenStarts leftDirection / denominator))
                if inUnitRange leftT 0.0<parameter> && inUnitRange rightT 0.0<parameter> then
                    projectionAt left right leftT rightT
                else
                    boundaryMinima left right options
                    |> Result.bind (fun minima ->
                        let best = minima |> List.reduce closerMinimum
                        projectionAt left right best.LeftT best.RightT)
            else
                boundaryMinima left right options
                |> Result.bind (fun minima ->
                    let best = minima |> List.reduce closerMinimum
                    projectionAt left right best.LeftT best.RightT)
        | _ -> failwith "expected two line segments"

    let segmentSegmentProjectionWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            OverlapDetection.detect left right options.Tolerance
            |> Result.bind (function
                | overlap :: _ -> projectionFromOverlap left right overlap
                | [] when (match left, right with Line _, Line _ -> true | _ -> false) ->
                    lineLineProjection left right options
                | [] ->
                    segmentPairProjectionMinima left right options
                    |> Result.bind (fun minima ->
                        let best = minima |> List.reduce closerMinimum
                        projectionAt left right best.LeftT best.RightT)))

    let segmentSegmentProjection left right =
        segmentSegmentProjectionWith left right defaultOptions

    let private indexedProjectionSegments segments =
        segments
        |> List.indexed
        |> List.fold (fun state (index, segmentValue) ->
            state
            |> Result.bind (fun indexed ->
                Segment.boundingBox segmentValue
                |> Result.map (fun bounds -> (index, segmentValue, bounds) :: indexed))) (Ok [])
        |> Result.map List.rev

    let private segmentListProjection left right options =
        indexedProjectionSegments left
        |> Result.bind (fun leftSegments ->
            indexedProjectionSegments right
            |> Result.bind (fun rightSegments ->
                [ for leftIndex, leftSegment, leftBounds in leftSegments do
                      for rightIndex, rightSegment, rightBounds in rightSegments do
                          yield leftIndex, leftSegment, leftBounds, rightIndex, rightSegment, rightBounds ]
                |> List.fold (fun state pair ->
                    state
                    |> Result.bind (fun best ->
                        let leftIndex, leftSegment, leftBounds, rightIndex, rightSegment, rightBounds = pair
                        let skip =
                            match best with
                            | Some(_, _, projection: SegmentSegmentProjection) ->
                                boundingBoxDistanceSquared leftBounds rightBounds >= projection.Distance * projection.Distance
                            | None -> false
                        if skip then Ok best
                        else
                            segmentSegmentProjectionWith leftSegment rightSegment options
                            |> Result.map (fun projection ->
                                match best with
                                | None -> Some(leftIndex, rightIndex, projection)
                                | Some(_, _, champion) when projection.Distance < champion.Distance ->
                                    Some(leftIndex, rightIndex, projection)
                                | _ -> best))) (Ok None)
                |> Result.bind (function
                    | Some value -> Ok value
                    | None -> Error(InternalUncertifiedSegmentIntersection(
                        1.0e100<length>, 1.0e100<length>, options.Tolerance)))))

    let segmentSubpathProjectionWith left (right: Subpath) options =
        validateOptions options
        |> Result.bind (fun () ->
            if List.isEmpty right.Segments then Error EmptySubpath
            else
                segmentListProjection [ left ] right.Segments options
                |> Result.map (fun (_, rightIndex, (projection: SegmentSegmentProjection)) ->
                    let rightAt: SubpathParameter = { SegmentIndex = rightIndex; T = projection.RightT }
                    let result: SegmentSubpathProjection =
                        { LeftT = projection.LeftT
                          RightAt = rightAt
                          LeftPoint = projection.LeftPoint
                          RightPoint = projection.RightPoint
                          Distance = projection.Distance }
                    result))

    let segmentSubpathProjection left right =
        segmentSubpathProjectionWith left right defaultOptions

    let private pathProjectionSegments (path: Path) =
        if List.isEmpty path.Subpaths then Error EmptyPath
        else
            let pairs =
                path.Subpaths
                |> List.indexed
                |> List.collect (fun (subpathIndex, subpath) ->
                    subpath.Segments
                    |> List.indexed
                    |> List.map (fun (segmentIndex, segmentValue) ->
                        segmentValue,
                        { SubpathIndex = subpathIndex
                          At = { SegmentIndex = segmentIndex; T = 0.0<parameter> } }))
            if List.isEmpty pairs then Error EmptySubpaths else Ok(List.unzip pairs)

    let private addressWithT (address: PathParameter) (t: float<parameter>) : PathParameter =
        { address with At = { address.At with T = t } }

    let segmentPathProjectionWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            pathProjectionSegments right
            |> Result.bind (fun (segments, addresses) ->
                segmentListProjection [ left ] segments options
                |> Result.map (fun (_, rightIndex, (projection: SegmentSegmentProjection)) ->
                    let result: SegmentPathProjection =
                        { LeftT = projection.LeftT
                          RightAt = addressWithT addresses[rightIndex] projection.RightT
                          LeftPoint = projection.LeftPoint
                          RightPoint = projection.RightPoint
                          Distance = projection.Distance }
                    result)))

    let segmentPathProjection left right =
        segmentPathProjectionWith left right defaultOptions

    let subpathSubpathProjectionWith (left: Subpath) (right: Subpath) options =
        validateOptions options
        |> Result.bind (fun () ->
            if List.isEmpty left.Segments || List.isEmpty right.Segments then Error EmptySubpath
            else
                segmentListProjection left.Segments right.Segments options
                |> Result.map (fun (leftIndex, rightIndex, (projection: SegmentSegmentProjection)) ->
                    let leftAt: SubpathParameter = { SegmentIndex = leftIndex; T = projection.LeftT }
                    let rightAt: SubpathParameter = { SegmentIndex = rightIndex; T = projection.RightT }
                    let result: SubpathSubpathProjection =
                        { LeftAt = leftAt
                          RightAt = rightAt
                          LeftPoint = projection.LeftPoint
                          RightPoint = projection.RightPoint
                          Distance = projection.Distance }
                    result))

    let subpathSubpathProjection left right =
        subpathSubpathProjectionWith left right defaultOptions

    let subpathPathProjectionWith (left: Subpath) right options =
        validateOptions options
        |> Result.bind (fun () ->
            if List.isEmpty left.Segments then Error EmptySubpath
            else
                pathProjectionSegments right
                |> Result.bind (fun (segments, addresses) ->
                    segmentListProjection left.Segments segments options
                    |> Result.map (fun (leftIndex, rightIndex, (projection: SegmentSegmentProjection)) ->
                        let leftAt: SubpathParameter = { SegmentIndex = leftIndex; T = projection.LeftT }
                        let result: SubpathPathProjection =
                            { LeftAt = leftAt
                              RightAt = addressWithT addresses[rightIndex] projection.RightT
                              LeftPoint = projection.LeftPoint
                              RightPoint = projection.RightPoint
                              Distance = projection.Distance }
                        result)))

    let subpathPathProjection left right =
        subpathPathProjectionWith left right defaultOptions

    let pathPathProjectionWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            pathProjectionSegments left
            |> Result.bind (fun (leftSegments, leftAddresses) ->
                pathProjectionSegments right
                |> Result.bind (fun (rightSegments, rightAddresses) ->
                    segmentListProjection leftSegments rightSegments options
                    |> Result.map (fun (leftIndex, rightIndex, (projection: SegmentSegmentProjection)) ->
                        let result: PathPathProjection =
                            { LeftAt = addressWithT leftAddresses[leftIndex] projection.LeftT
                              RightAt = addressWithT rightAddresses[rightIndex] projection.RightT
                              LeftPoint = projection.LeftPoint
                              RightPoint = projection.RightPoint
                              Distance = projection.Distance }
                        result))))

    let pathPathProjection left right =
        pathPathProjectionWith left right defaultOptions

    let private validateSelfIntersectionOptions options =
        if options.MinimumArcLengthSeparation <= 0.0<length>
           || not (System.Double.IsFinite(float options.MinimumArcLengthSeparation)) then
            Error(InvalidSelfIntersectionMinimumArcLengthSeparation options.MinimumArcLengthSeparation)
        elif options.DistanceTolerance <= 0.0<length>
             || not (System.Double.IsFinite(float options.DistanceTolerance)) then
            Error(InvalidSelfIntersectionDistanceTolerance options.DistanceTolerance)
        else Ok()

    let private bezierSelfIntersectionError = function
        | InvalidCubicSelfIntersectionMinimumArcLengthSeparation value ->
            InvalidSelfIntersectionMinimumArcLengthSeparation value
        | InvalidCubicSelfIntersectionDistanceTolerance value ->
            InvalidSelfIntersectionDistanceTolerance value
        | BezierError.SplitOutsideBezier -> SegmentError.SplitOutsideSegment
        | BezierError.DegenerateTangent -> SegmentError.DegenerateCubicFitTangent
        | BezierError.UnderdeterminedCubicFit -> SegmentError.UnderdeterminedCubicFit

    let private segmentSelfValid segmentValue options =
        match segmentValue with
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Bezier.cubicSelfIntersectionsWith
                (CubicBezierData(startPoint, control1, control2, endPoint))
                { MinimumArcLengthSeparation = options.MinimumArcLengthSeparation
                  DistanceTolerance = options.DistanceTolerance }
            |> Result.mapError bezierSelfIntersectionError
            |> Result.map (List.map (fun intersection ->
                ({ LeftT = intersection.S
                   RightT = intersection.T
                   Point = intersection.Point } : SegmentIntersection)))
        | Arc arc when arc.Start = arc.End && not (InternalNumber.isZero arc.Radius.X) && not (InternalNumber.isZero arc.Radius.Y) ->
            Ok [ ({ LeftT = 0.0<parameter>; RightT = 1.0<parameter>; Point = arc.Start } : SegmentIntersection) ]
        | Line _
        | QuadraticBezier _
        | Arc _ -> Ok []

    let segmentSelfWith segmentValue options =
        validateSelfIntersectionOptions options
        |> Result.bind (fun () -> segmentSelfValid segmentValue options)

    let segmentSelf segmentValue =
        segmentSelfWith segmentValue defaultSelfIntersectionOptions

    let private orderedSubpathPair (first: SubpathParameter) (second: SubpathParameter) =
        if Subpath.parametersCompare first second <= 0 then first, second else second, first

    let private insertSubpathSelf
        (tolerance: float<length>)
        (point: Point<length>)
        (first: SubpathParameter)
        (second: SubpathParameter)
        (found: SubpathSelfIntersection list) =
        let first, second = orderedSubpathPair first second
        if found
           |> List.exists (fun existing ->
               let existingFirst, existingSecond = existing.Parameters
               Point.distance existing.Point point <= tolerance
               && existingFirst.SegmentIndex = first.SegmentIndex
               && existingSecond.SegmentIndex = second.SegmentIndex
               && abs (existingFirst.T - first.T) <= Parameter.fromFloat(float tolerance)
               && abs (existingSecond.T - second.T) <= Parameter.fromFloat(float tolerance)) then found
        else
            ({ Point = point; Parameters = first, second } : SubpathSelfIntersection) :: found

    let private segmentLengthToT segmentValue t =
        if t <= 0.0<parameter> then Ok 0.0<length>
        elif t >= 1.0<parameter> then Segment.length segmentValue
        else Segment.between segmentValue 0.0<parameter> t |> Result.bind Segment.length

    let subpathSelfWith (subpathValue: Subpath) options =
        validateSelfIntersectionOptions options
        |> Result.bind (fun () ->
            subpathValue.Segments
            |> List.fold (fun state segmentValue ->
                state
                |> Result.bind (fun (prefix, indexed) ->
                    Segment.length segmentValue
                    |> Result.map (fun segmentLength ->
                        prefix + segmentLength,
                        (List.length indexed, segmentValue, prefix, segmentLength) :: indexed))) (Ok(0.0<length>, []))
            |> Result.bind (fun (totalLength, reversedIndexed) ->
                let indexed = List.rev reversedIndexed
                indexed
                |> List.fold (fun state (leftIndex, leftSegment, leftPrefix, leftLength) ->
                    state
                    |> Result.bind (fun found ->
                        segmentSelfValid leftSegment options
                        |> Result.map (fun own ->
                            own
                            |> List.fold (fun found intersection ->
                                insertSubpathSelf options.DistanceTolerance intersection.Point
                                    { SegmentIndex = leftIndex; T = intersection.LeftT }
                                    { SegmentIndex = leftIndex; T = intersection.RightT }
                                    found) found)
                        |> Result.bind (fun found ->
                            indexed
                            |> List.filter (fun (rightIndex, _, _, _) -> rightIndex > leftIndex)
                            |> List.fold (fun state (rightIndex, rightSegment, rightPrefix, _) ->
                                state
                                |> Result.bind (fun found ->
                                    segmentWith leftSegment rightSegment
                                        { defaultOptions with Tolerance = options.DistanceTolerance }
                                    |> Result.bind (fun (intersections: SegmentIntersection list) ->
                                        intersections
                                        |> List.fold (fun state (intersection: SegmentIntersection) ->
                                            state
                                            |> Result.bind (fun found ->
                                                segmentLengthToT leftSegment intersection.LeftT
                                                |> Result.bind (fun leftWithin ->
                                                    segmentLengthToT rightSegment intersection.RightT
                                                    |> Result.map (fun rightWithin ->
                                                        let firstLength = leftPrefix + leftWithin
                                                        let secondLength = rightPrefix + rightWithin
                                                        let direct = abs (secondLength - firstLength)
                                                        let separation =
                                                            if subpathValue.Closed && totalLength > 0.0<length> then
                                                                min direct (totalLength - direct)
                                                            else direct
                                                        if separation >= options.MinimumArcLengthSeparation then
                                                            insertSubpathSelf options.DistanceTolerance intersection.Point
                                                                { SegmentIndex = leftIndex; T = intersection.LeftT }
                                                                { SegmentIndex = rightIndex; T = intersection.RightT }
                                                                found
                                                        else found)))) (Ok found)))) (Ok found)))) (Ok [])
                |> Result.map (List.sortWith (fun (left: SubpathSelfIntersection) (right: SubpathSelfIntersection) ->
                    let leftFirst, leftSecond = left.Parameters
                    let rightFirst, rightSecond = right.Parameters
                    let firstOrder = Subpath.parametersCompare leftFirst rightFirst
                    if firstOrder <> 0 then firstOrder else Subpath.parametersCompare leftSecond rightSecond))))

    let subpathSelf subpathValue =
        subpathSelfWith subpathValue defaultSelfIntersectionOptions

    let private canonicalSubpathParameterUnchecked (subpath: Subpath) tolerance parameterValue =
        let length = subpath.Segments.Length
        if parameterValue.T <= Parameter.fromFloat(float tolerance) then
            { parameterValue with T = 0.0<parameter> }
        elif 1.0<parameter> - parameterValue.T <= Parameter.fromFloat(float tolerance) then
            if parameterValue.SegmentIndex < length - 1 then
                { SegmentIndex = parameterValue.SegmentIndex + 1; T = 0.0<parameter> }
            elif subpath.Closed then { SegmentIndex = 0; T = 0.0<parameter> }
            else { parameterValue with T = 1.0<parameter> }
        else parameterValue

    let private subpathParameterAddressesNear (subpath: Subpath) tolerance a b =
        let parameterTolerance = Parameter.fromFloat(float tolerance)
        if a.SegmentIndex = b.SegmentIndex then abs (a.T - b.T) <= parameterTolerance
        else
            let adjacent =
                if a.SegmentIndex + 1 = b.SegmentIndex then abs (b.T - a.T + 1.0<parameter>) <= parameterTolerance
                elif b.SegmentIndex + 1 = a.SegmentIndex then abs (a.T - b.T + 1.0<parameter>) <= parameterTolerance
                else false
            let wrap =
                if not subpath.Closed then false
                elif a.SegmentIndex = 0 && b.SegmentIndex = subpath.Segments.Length - 1 then
                    abs (a.T - b.T + 1.0<parameter>) <= parameterTolerance
                elif b.SegmentIndex = 0 && a.SegmentIndex = subpath.Segments.Length - 1 then
                    abs (b.T - a.T + 1.0<parameter>) <= parameterTolerance
                else false
            adjacent || wrap

    let private subpathParametersNear subpath tolerance a b =
        subpathParameterAddressesNear subpath tolerance a b
        && match Subpath.point subpath a, Subpath.point subpath b with
           | Ok aPoint, Ok bPoint -> Point.squaredDistance aPoint bPoint <= tolerance * tolerance
           | _ -> false

    let private sortUniqueSubpathParameters (subpath: Subpath) tolerance (parameters: SubpathParameter list) =
        let sorted =
            parameters
            |> List.map (canonicalSubpathParameterUnchecked subpath tolerance)
            |> List.sortWith Subpath.parametersCompare
        let deduped =
            sorted
            |> List.fold (fun accumulated parameterValue ->
                match accumulated with
                | previous :: _ when subpathParametersNear subpath tolerance parameterValue previous -> accumulated
                | _ -> parameterValue :: accumulated) []
            |> List.rev
        if subpath.Closed && deduped.Length >= 2
           && subpathParametersNear subpath tolerance (List.head deduped) (List.last deduped) then
            deduped |> List.tail
        else deduped

    let private insertSubpathIntersection
        (tolerance: float<length>)
        (point: Point<length>)
        (leftParameter: SubpathParameter)
        (rightParameter: SubpathParameter)
        (found: SubpathIntersection list) =
        let rec insert (accumulated: SubpathIntersection list) (remaining: SubpathIntersection list) =
            match remaining with
            | [] ->
                List.rev accumulated
                @ [ ({ Point = point
                       LeftParameters = [ leftParameter ]
                       RightParameters = [ rightParameter ] } : SubpathIntersection) ]
            | first :: rest when Point.distance first.Point point <= tolerance ->
                List.rev accumulated
                @ ({ first with
                        LeftParameters = leftParameter :: first.LeftParameters
                        RightParameters = rightParameter :: first.RightParameters } :: rest)
            | first :: rest -> insert (first :: accumulated) rest
        insert [] found

    let private collectSubpathIntersections permitOverlappingPairs (left: Subpath) (right: Subpath) options =
        validateOptions options
        |> Result.bind (fun () ->
            let pairs =
                [ for leftIndex, leftSegment in List.indexed left.Segments do
                    for rightIndex, rightSegment in List.indexed right.Segments do
                        yield leftIndex, leftSegment, rightIndex, rightSegment ]
            pairs
            |> List.fold (fun state (leftIndex, leftSegment, rightIndex, rightSegment) ->
                state
                |> Result.bind (fun found ->
                    OverlapDetection.detect leftSegment rightSegment options.Tolerance
                    |> Result.bind (function
                        | _ :: _ when permitOverlappingPairs -> Ok found
                        | _ :: _ -> Error OverlappingSegments
                        | [] ->
                            segmentWithoutOverlapPrecheckWith leftSegment rightSegment options
                            |> Result.map (fun (intersections: SegmentIntersection list) ->
                                intersections
                                |> List.fold (fun grouped intersection ->
                                    insertSubpathIntersection
                                        options.Tolerance
                                        intersection.Point
                                        { SegmentIndex = leftIndex; T = intersection.LeftT }
                                        { SegmentIndex = rightIndex; T = intersection.RightT }
                                        grouped) found)))) (Ok([]: SubpathIntersection list))
            |> Result.bind (fun grouped ->
                grouped
                |> List.fold (fun state (intersection: SubpathIntersection) ->
                    state
                    |> Result.bind (fun normalized ->
                        let leftParameters = sortUniqueSubpathParameters left options.Tolerance intersection.LeftParameters
                        let rightParameters = sortUniqueSubpathParameters right options.Tolerance intersection.RightParameters
                        Ok({ intersection with
                                LeftParameters = leftParameters
                                RightParameters = rightParameters } :: normalized))) (Ok([]: SubpathIntersection list))
                |> Result.map (List.sortBy (fun (intersection: SubpathIntersection) ->
                    intersection.LeftParameters
                    |> List.tryHead
                    |> Option.map (fun parameterValue -> parameterValue.SegmentIndex, parameterValue.T)
                    |> Option.defaultValue (System.Int32.MaxValue, 1.0<parameter>)))))

    let subpathWithoutOverlapPrecheckWith left right options =
        collectSubpathIntersections true left right options

    let subpathWith left right options =
        collectSubpathIntersections false left right options

    let subpath left right = subpathWith left right defaultOptions

    let segmentSubpathWithoutOverlapPrecheckWith segmentValue subpathValue options =
        subpathWithoutOverlapPrecheckWith (Subpath.ofSegment segmentValue) subpathValue options
        |> Result.map (List.map (fun (intersection: SubpathIntersection) ->
            let segmentParameters = intersection.LeftParameters |> List.map (fun value -> value.T)
            let segmentParameter = segmentParameters |> List.tryHead |> Option.defaultValue 0.0<parameter>
            intersection.Point, segmentParameter, intersection.RightParameters))

    let segmentSubpathWith segmentValue subpathValue options =
        subpathWith (Subpath.ofSegment segmentValue) subpathValue options
        |> Result.map (List.map (fun (intersection: SubpathIntersection) ->
            let segmentParameter =
                intersection.LeftParameters
                |> List.tryHead
                |> Option.map (fun value -> value.T)
                |> Option.defaultValue 0.0<parameter>
            intersection.Point, segmentParameter, intersection.RightParameters))

    let segmentSubpath segmentValue subpathValue =
        segmentSubpathWith segmentValue subpathValue defaultOptions

    let private collectPathIntersections permitOverlappingPairs (left: Path) (right: Path) options =
        validateOptions options
        |> Result.bind (fun () ->
            let pairs =
                [ for leftIndex, leftSubpath in List.indexed left.Subpaths do
                    for rightIndex, rightSubpath in List.indexed right.Subpaths do
                        yield leftIndex, leftSubpath, rightIndex, rightSubpath ]
            let insertPath
                (intersection: SubpathIntersection)
                leftIndex
                rightIndex
                (found: PathIntersection list) =
                let lifted: PathIntersection =
                    { Point = intersection.Point
                      LeftParameters =
                        intersection.LeftParameters
                        |> List.map (fun at -> { SubpathIndex = leftIndex; At = at })
                      RightParameters =
                        intersection.RightParameters
                        |> List.map (fun at -> { SubpathIndex = rightIndex; At = at }) }
                match found |> List.tryFindIndex (fun existing -> Point.distance existing.Point lifted.Point <= options.Tolerance) with
                | None -> lifted :: found
                | Some index ->
                    found
                    |> List.mapi (fun current existing ->
                        if current <> index then existing
                        else
                            { existing with
                                LeftParameters = List.distinct (lifted.LeftParameters @ existing.LeftParameters)
                                RightParameters = List.distinct (lifted.RightParameters @ existing.RightParameters) })
            pairs
            |> List.fold (fun state (leftIndex, leftSubpath, rightIndex, rightSubpath) ->
                state
                |> Result.bind (fun found ->
                    (if permitOverlappingPairs then
                         subpathWithoutOverlapPrecheckWith leftSubpath rightSubpath options
                     else
                         subpathWith leftSubpath rightSubpath options)
                    |> Result.map (fun intersections ->
                        intersections
                        |> List.fold (fun grouped (intersection: SubpathIntersection) -> insertPath intersection leftIndex rightIndex grouped) found))) (Ok([]: PathIntersection list))
            |> Result.map (fun intersections ->
                intersections
                |> List.map (fun intersection ->
                    { intersection with
                        LeftParameters = intersection.LeftParameters |> List.distinct |> List.sortWith Path.parametersCompare
                        RightParameters = intersection.RightParameters |> List.distinct |> List.sortWith Path.parametersCompare })
                |> List.sortBy (fun (intersection: PathIntersection) ->
                    intersection.LeftParameters
                    |> List.tryHead
                    |> Option.map (fun parameterValue ->
                        parameterValue.SubpathIndex,
                        parameterValue.At.SegmentIndex,
                        parameterValue.At.T)
                    |> Option.defaultValue (System.Int32.MaxValue, System.Int32.MaxValue, 1.0<parameter>))))

    let pathWithoutOverlapPrecheckWith left right options =
        collectPathIntersections true left right options

    let pathWith left right options =
        collectPathIntersections false left right options

    let path left right = pathWith left right defaultOptions

    let private orderedPathPair first second =
        if Path.parametersCompare first second <= 0 then first, second else second, first

    let private insertPathSelf tolerance point first second found =
        let first, second = orderedPathPair first second
        if found
           |> List.exists (fun existing ->
               let existingFirst, existingSecond = existing.Parameters
               Point.distance existing.Point point <= tolerance
               && Path.parametersCompare first existingFirst = 0
               && Path.parametersCompare second existingSecond = 0) then found
        else ({ Point = point; Parameters = first, second } : PathSelfIntersection) :: found

    let pathSelfWith (pathValue: Path) options =
        validateSelfIntersectionOptions options
        |> Result.bind (fun () ->
            pathValue.Subpaths
            |> List.indexed
            |> List.fold (fun state (leftIndex, leftSubpath) ->
                state
                |> Result.bind (fun found ->
                    subpathSelfWith leftSubpath options
                    |> Result.map (fun own ->
                        own
                        |> List.fold (fun found (intersection: SubpathSelfIntersection) ->
                            let first, second = intersection.Parameters
                            insertPathSelf options.DistanceTolerance intersection.Point
                                { SubpathIndex = leftIndex; At = first }
                                { SubpathIndex = leftIndex; At = second }
                                found) found)
                    |> Result.bind (fun found ->
                        pathValue.Subpaths
                        |> List.indexed
                        |> List.filter (fun (rightIndex, _) -> rightIndex > leftIndex)
                        |> List.fold (fun state (rightIndex, rightSubpath) ->
                            state
                            |> Result.bind (fun found ->
                                subpathWith leftSubpath rightSubpath
                                    { defaultOptions with Tolerance = options.DistanceTolerance }
                                |> Result.map (fun intersections ->
                                    intersections
                                    |> List.fold (fun found intersection ->
                                        [ for leftAt in intersection.LeftParameters do
                                              for rightAt in intersection.RightParameters do
                                                  yield leftAt, rightAt ]
                                        |> List.fold (fun found (leftAt, rightAt) ->
                                            insertPathSelf options.DistanceTolerance intersection.Point
                                                { SubpathIndex = leftIndex; At = leftAt }
                                                { SubpathIndex = rightIndex; At = rightAt }
                                                found) found) found))) (Ok found)))) (Ok [])
            |> Result.map (List.sortWith (fun left right ->
                let leftFirst, leftSecond = left.Parameters
                let rightFirst, rightSecond = right.Parameters
                let firstOrder = Path.parametersCompare leftFirst rightFirst
                if firstOrder <> 0 then firstOrder else Path.parametersCompare leftSecond rightSecond)))

    let pathSelf pathValue = pathSelfWith pathValue defaultSelfIntersectionOptions

    let private validateClassificationOptions options =
        if options.AngularTolerance < 0.0<degree>
           || options.AngularTolerance >= 180.0<degree>
           || not (System.Double.IsFinite(float options.AngularTolerance)) then
            Error(ClassificationError.InvalidAngularTolerance options.AngularTolerance)
        elif options.DistanceTolerance < 0.0<length>
             || not (System.Double.IsFinite(float options.DistanceTolerance)) then
            Error(ClassificationError.InvalidClassificationDistanceTolerance options.DistanceTolerance)
        elif options.LengthOptions.Tolerance <= 0.0<length>
             || not (System.Double.IsFinite(float options.LengthOptions.Tolerance)) then
            Error(ClassificationError.PathError(InvalidLengthTolerance options.LengthOptions.Tolerance))
        elif options.LengthOptions.MaxDepth < 0 then
            Error(ClassificationError.PathError(InvalidLengthMaxDepth options.LengthOptions.MaxDepth))
        elif options.InitialArcLength <= 0.0<length>
             || not (System.Double.IsFinite(float options.InitialArcLength)) then
            Error(ClassificationError.InvalidClassificationInitialArcLength options.InitialArcLength)
        elif options.MaximumArcLength < options.InitialArcLength
             || not (System.Double.IsFinite(float options.MaximumArcLength)) then
            Error(ClassificationError.InvalidClassificationMaximumArcLength options.MaximumArcLength)
        elif options.MaxSamplingSteps <= 0 then
            Error(ClassificationError.InvalidClassificationMaxSamplingSteps options.MaxSamplingSteps)
        else Ok()

    let private subpathEndpoint (subpathValue: Subpath) parameterValue =
        Subpath.parameterCanonicalize subpathValue parameterValue
        |> Result.map (fun parameterValue ->
            if subpathValue.Closed then None
            elif parameterValue.SegmentIndex = 0 && parameterValue.T = 0.0<parameter> then Some StartEndpoint
            elif parameterValue.SegmentIndex = subpathValue.Segments.Length - 1
                 && parameterValue.T = 1.0<parameter> then Some EndEndpoint
            else None)

    let private intersectionApertures leftIncoming leftOutgoing rightIncoming rightOutgoing =
        { FirstIncomingToSecondIncoming = Point.clockwiseAperture leftIncoming rightIncoming
          FirstIncomingToSecondOutgoing = Point.clockwiseAperture leftIncoming rightOutgoing
          FirstOutgoingToSecondIncoming = Point.clockwiseAperture leftOutgoing rightIncoming
          FirstOutgoingToSecondOutgoing = Point.clockwiseAperture leftOutgoing rightOutgoing }

    let private separatedByRays boundaryFrom boundaryTo first second tolerance =
        let aperture = Point.clockwiseAperture boundaryFrom boundaryTo
        if aperture <= tolerance || 360.0<degree> - aperture <= tolerance then false
        else
            let firstAperture = Point.clockwiseAperture boundaryFrom first
            let secondAperture = Point.clockwiseAperture boundaryFrom second
            let firstInside = firstAperture > tolerance && firstAperture < aperture - tolerance
            let secondInside = secondAperture > tolerance && secondAperture < aperture - tolerance
            firstInside <> secondInside

    let private crossingDirection leftOutgoing rightOutgoing =
        if Point.clockwiseAperture leftOutgoing rightOutgoing < 180.0<degree> then Clockwise else Counterclockwise

    let private touchingDirection leftOutgoing rightOutgoing =
        let aperture = Point.clockwiseAperture leftOutgoing rightOutgoing
        if aperture <= 90.0<degree> || aperture >= 270.0<degree> then SimilarlyDirected else OppositelyDirected

    let private subpathArcLengthLocation subpathValue parameterValue lengthOptions =
        Subpath.parameterCanonicalize subpathValue parameterValue
        |> Result.bind (fun canonical ->
            Subpath.lengthWith subpathValue lengthOptions
            |> Result.bind (fun total ->
                let startAt: SubpathParameter = { SegmentIndex = 0; T = 0.0<parameter> }
                (if canonical = startAt then Ok 0.0<length>
                 else
                     Subpath.between subpathValue startAt canonical
                     |> Result.bind (fun portion -> Subpath.lengthWith portion lengthOptions))
                |> Result.map (fun at ->
                    { Subpath = subpathValue; At = at; Total = total; Closed = subpathValue.Closed })))

    let private positiveRemainder (value: float<length>) (modulus: float<length>) =
        value - floor (float (value / modulus)) * modulus

    let private sampleBranch
        (location: ArcLengthLocation)
        branch
        (arcLength: float<length>)
        lengthOptions =
        let raw = if branch = IncomingBranch then location.At - arcLength else location.At + arcLength
        let distance =
            if location.Closed && location.Total > 0.0<length> then positiveRemainder raw location.Total
            else max 0.0<length> (min location.Total raw)
        Subpath.pointAtLengthWith location.Subpath distance lengthOptions

    let private touchingOrderFromRays firstRay secondRay options =
        let minimumSquared = options.DistanceTolerance * options.DistanceTolerance
        if Point.squaredNorm firstRay <= minimumSquared || Point.squaredNorm secondRay <= minimumSquared then
            IndeterminateTouchingOrder
        else
            let signedOrder = Point.clockwiseAperture firstRay secondRay - 180.0<degree>
            let distanceFromCoincidence = 180.0<degree> - abs signedOrder
            if abs signedOrder <= options.AngularTolerance
               || distanceFromCoincidence <= options.AngularTolerance then IndeterminateTouchingOrder
            elif signedOrder < 0.0<degree> then ClockwiseFromFirstToSecond
            else ClockwiseFromSecondToFirst

    let private sampleTouchingOrder first second intersectionPoint firstBranch secondBranch options =
        let rec loop arcLength remaining =
            if remaining <= 0 then Ok IndeterminateTouchingOrder
            else
                sampleBranch first firstBranch arcLength options.LengthOptions
                |> Result.bind (fun firstPoint ->
                    sampleBranch second secondBranch arcLength options.LengthOptions
                    |> Result.bind (fun secondPoint ->
                        let firstRay = Point.displacement intersectionPoint firstPoint
                        let secondRay = Point.displacement intersectionPoint secondPoint
                        match touchingOrderFromRays firstRay secondRay options with
                        | IndeterminateTouchingOrder when arcLength < options.MaximumArcLength ->
                            loop (min options.MaximumArcLength (arcLength * 2.0)) (remaining - 1)
                        | order -> Ok order))
        loop options.InitialArcLength options.MaxSamplingSteps

    let private alignedDirections first second tolerance =
        let angle = Point.clockwiseAperture first second
        angle <= tolerance || 360.0<degree> - angle <= tolerance

    let classifySubpathIntersectionWith first second firstParameter secondParameter options =
        validateClassificationOptions options
        |> Result.bind (fun () ->
            subpathEndpoint first firstParameter
            |> Result.mapError ClassificationError.PathError
            |> Result.bind (fun firstEndpoint ->
                subpathEndpoint second secondParameter
                |> Result.mapError ClassificationError.PathError
                |> Result.bind (fun secondEndpoint ->
                    match firstEndpoint, secondEndpoint with
                    | Some firstEndpoint, Some secondEndpoint ->
                        Ok(EndpointContact(EndpointToEndpoint(firstEndpoint, secondEndpoint)))
                    | Some firstEndpoint, None ->
                        Ok(EndpointContact(FirstEndpointToSecondInterior firstEndpoint))
                    | None, Some secondEndpoint ->
                        Ok(EndpointContact(FirstInteriorToSecondEndpoint secondEndpoint))
                    | None, None ->
                        Subpath.directionsWith first firstParameter options.DirectionOptions
                        |> Result.mapError ClassificationError.PathError
                        |> Result.bind (fun left ->
                            Subpath.directionsWith second secondParameter options.DirectionOptions
                            |> Result.mapError ClassificationError.PathError
                            |> Result.bind (fun right ->
                                match left.Incoming, left.Outgoing, right.Incoming, right.Outgoing with
                                | Some leftIncoming, Some leftOutgoing, Some rightIncoming, Some rightOutgoing ->
                                    let apertures = intersectionApertures leftIncoming leftOutgoing rightIncoming rightOutgoing
                                    let leftBefore = Point.negate leftIncoming
                                    let rightBefore = Point.negate rightIncoming
                                    let alternating =
                                        separatedByRays leftBefore leftOutgoing rightBefore rightOutgoing options.AngularTolerance
                                        && separatedByRays rightBefore rightOutgoing leftBefore leftOutgoing options.AngularTolerance
                                    if alternating then Ok(Crossing(crossingDirection leftOutgoing rightOutgoing, apertures))
                                    else
                                        let direction = touchingDirection leftOutgoing rightOutgoing
                                        Subpath.point first firstParameter
                                        |> Result.bind (fun firstPoint ->
                                            Subpath.point second secondParameter
                                            |> Result.map (Point.midpoint firstPoint))
                                        |> Result.bind (fun intersectionPoint ->
                                            subpathArcLengthLocation first firstParameter options.LengthOptions
                                            |> Result.bind (fun firstLocation ->
                                                subpathArcLengthLocation second secondParameter options.LengthOptions
                                                |> Result.bind (fun secondLocation ->
                                                    let firstIncoming, secondIncoming, firstOutgoing, secondOutgoing =
                                                        match direction with
                                                        | SimilarlyDirected -> IncomingBranch, IncomingBranch, OutgoingBranch, OutgoingBranch
                                                        | OppositelyDirected -> IncomingBranch, OutgoingBranch, OutgoingBranch, IncomingBranch
                                                    sampleTouchingOrder firstLocation secondLocation intersectionPoint firstIncoming secondIncoming options
                                                    |> Result.bind (fun incomingOrder ->
                                                        sampleTouchingOrder firstLocation secondLocation intersectionPoint firstOutgoing secondOutgoing options
                                                        |> Result.map (fun outgoingOrder ->
                                                            // At a common smooth tangent equal outward-ray orders
                                                            // imply alternating branches. Do not apply at corners/cusps.
                                                            let smooth =
                                                                alignedDirections leftIncoming leftOutgoing options.AngularTolerance
                                                                && alignedDirections rightIncoming rightOutgoing options.AngularTolerance
                                                                && alignedDirections leftOutgoing
                                                                     (if direction = SimilarlyDirected then rightOutgoing else Point.negate rightOutgoing)
                                                                     options.AngularTolerance
                                                            match smooth, incomingOrder, outgoingOrder, direction with
                                                            | true, ClockwiseFromFirstToSecond, ClockwiseFromFirstToSecond, SimilarlyDirected
                                                            | true, ClockwiseFromSecondToFirst, ClockwiseFromSecondToFirst, OppositelyDirected -> Crossing(Clockwise, apertures)
                                                            | true, ClockwiseFromFirstToSecond, ClockwiseFromFirstToSecond, OppositelyDirected
                                                            | true, ClockwiseFromSecondToFirst, ClockwiseFromSecondToFirst, SimilarlyDirected -> Crossing(Counterclockwise, apertures)
                                                            | _ -> Touching(direction, incomingOrder, outgoingOrder, apertures))))))
                                        |> Result.mapError ClassificationError.PathError
                                | _ -> Ok Indeterminate)))))

    let classifySubpathIntersection first second firstParameter secondParameter =
        classifySubpathIntersectionWith first second firstParameter secondParameter defaultClassificationOptions

    let classifyGroupedSubpathIntersectionWith
        first
        second
        (intersection: SubpathIntersection)
        options =
        [ for firstParameter in intersection.LeftParameters do
              for secondParameter in intersection.RightParameters do
                  yield firstParameter, secondParameter ]
        |> List.fold (fun state (firstParameter, secondParameter) ->
            state
            |> Result.bind (fun classified ->
                classifySubpathIntersectionWith first second firstParameter secondParameter options
                |> Result.map (fun classification ->
                    let item: ClassifiedSubpathIntersection =
                        { FirstParameter = firstParameter
                          SecondParameter = secondParameter
                          Classification = classification }
                    item :: classified))) (Ok([]: ClassifiedSubpathIntersection list))
        |> Result.map List.rev

    let classifyGroupedSubpathIntersection first second intersection =
        classifyGroupedSubpathIntersectionWith first second intersection defaultClassificationOptions
