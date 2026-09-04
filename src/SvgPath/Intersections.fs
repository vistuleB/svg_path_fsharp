namespace SvgPath

[<Struct>]
type SegmentIntersection =
    { LeftT: float<parameter>
      RightT: float<parameter>
      Point: Point<length> }

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
    | PathError of SegmentError
    | InvalidAngularTolerance of float<degree>
    | InvalidClassificationDistanceTolerance of float<length>
    | InvalidClassificationInitialArcLength of float<length>
    | InvalidClassificationMaximumArcLength of float<length>
    | InvalidClassificationMaxSamplingSteps of int

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

    let private maximumWindows = 1000
    let private parameterTolerance = 1.0e-9<parameter>
    let private enclosureSlack = 1.0e-12<length>
    let private terminalSubdivisionTolerance = 0.01<length>
    let private intersectionDedupeTolerance (tolerance: float<length>) =
        max (tolerance * 1_000_000.0) 1.0e-6<length>

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

    let private boxesOverlap slack left right =
        left.Max.X + slack >= right.Min.X
        && right.Max.X + slack >= left.Min.X
        && left.Max.Y + slack >= right.Min.Y
        && right.Max.Y + slack >= left.Min.Y

    let private pointsNear tolerance left right = Point.squaredDistance left right <= tolerance * tolerance

    let private endpointParameterScore (intersection: SegmentIntersection) =
        min (abs intersection.LeftT) (abs (1.0<parameter> - intersection.LeftT))
        + min (abs intersection.RightT) (abs (1.0<parameter> - intersection.RightT))

    let private insert
        (tolerance: float<length>)
        (candidate: SegmentIntersection)
        (existing: SegmentIntersection list) =
        match existing |> List.tryFindIndex (fun found ->
            abs (found.LeftT - candidate.LeftT) <= parameterTolerance
            && abs (found.RightT - candidate.RightT) <= parameterTolerance
            || pointsNear tolerance found.Point candidate.Point) with
        | Some index when endpointParameterScore candidate < endpointParameterScore existing[index] ->
            existing |> List.mapi (fun current value -> if current = index then candidate else value)
        | Some _ -> existing
        | None -> candidate :: existing

    let private insertWindowCandidate
        (candidate: SegmentIntersection)
        (existing: SegmentIntersection list) =
        if existing
           |> List.exists (fun (found: SegmentIntersection) ->
               abs (found.LeftT - candidate.LeftT) <= 1.0e-7<parameter>
               && abs (found.RightT - candidate.RightT) <= 1.0e-7<parameter>) then existing
        else candidate :: existing

    let private endpointCandidates left right tolerance =
        let samples =
            [ 0.0<parameter>, 0.0<parameter>
              0.0<parameter>, 1.0<parameter>
              1.0<parameter>, 0.0<parameter>
              1.0<parameter>, 1.0<parameter> ]
        samples
        |> List.fold (fun state (leftT, rightT) ->
            state
            |> Result.bind (fun found ->
                match Segment.point left leftT, Segment.point right rightT with
                | Ok leftPoint, Ok rightPoint when pointsNear tolerance leftPoint rightPoint ->
                    let candidate: SegmentIntersection =
                        { LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint }
                    Ok(insert tolerance candidate found)
                | Ok _, Ok _ -> Ok found
                | Error error, _
                | _, Error error -> Error error)) (Ok [])

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
            if leftT >= 0.0<parameter> && leftT <= 1.0<parameter>
               && rightT >= 0.0<parameter> && rightT <= 1.0<parameter> then Some(leftT, rightT)
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

    let private refineCrossing left right tolerance leftT rightT =
        let rec loop leftT rightT remaining =
            Segment.point left leftT
            |> Result.bind (fun leftPoint ->
                Segment.point right rightT
                |> Result.bind (fun rightPoint ->
                    if Point.distance leftPoint rightPoint <= tolerance then
                        Ok(Some({ LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint } : SegmentIntersection))
                    elif remaining <= 0 then Ok None
                    else
                        Segment.derivative left leftT
                        |> Result.bind (fun leftDirection ->
                            Segment.derivative right rightT
                            |> Result.bind (fun rightDirection ->
                                if not (directionsIndependent leftDirection rightDirection) then Ok None
                                else
                                    let delta = Point.displacement leftPoint rightPoint
                                    let denominator = cross leftDirection rightDirection
                                    let leftStep = cross delta rightDirection / denominator
                                    let rightStep = -(cross leftDirection delta / denominator)
                                    let nextLeft = leftT + leftStep
                                    let nextRight = rightT + rightStep
                                    if nextLeft < -1.0e-12<parameter> || nextLeft > 1.0<parameter> + 1.0e-12<parameter>
                                       || nextRight < -1.0e-12<parameter> || nextRight > 1.0<parameter> + 1.0e-12<parameter> then Ok None
                                    else loop (clamp01 nextLeft) (clamp01 nextRight) (remaining - 1)))))
        loop leftT rightT 20

    let private candidateAt left right tolerance leftT rightT =
        refineCrossing left right tolerance leftT rightT

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

    let private splitNine window =
        let leftThird = (window.LeftTo - window.LeftFrom) / 3.0
        let rightThird = (window.RightTo - window.RightFrom) / 3.0
        [ for leftIndex in 0 .. 2 do
              for rightIndex in 0 .. 2 do
                  let leftFrom = window.LeftFrom + float leftIndex * leftThird
                  let rightFrom = window.RightFrom + float rightIndex * rightThird
                  yield
                      { LeftFrom = leftFrom
                        LeftTo = leftFrom + leftThird
                        RightFrom = rightFrom
                        RightTo = rightFrom + rightThird
                        Depth = window.Depth - 1 } ]

    let private inspectWindow left right tolerance window =
        match Segment.betweenInside left window.LeftFrom window.LeftTo,
              Segment.betweenInside right window.RightFrom window.RightTo with
        | Error error, _
        | _, Error error -> Error error
        | Ok leftPiece, Ok rightPiece ->
            match Segment.boundingBox leftPiece, Segment.boundingBox rightPiece with
            | Error error, _
            | _, Error error -> Error error
            | Ok leftBox, Ok rightBox when not (boxesOverlap enclosureSlack leftBox rightBox) -> Ok(None, false)
            | Ok leftBox, Ok rightBox ->
                match Segment.point left window.LeftFrom, Segment.point left window.LeftTo,
                      Segment.point right window.RightFrom, Segment.point right window.RightTo with
                | Ok leftStart, Ok leftFinish, Ok rightStart, Ok rightFinish ->
                    let centerLeftT = (window.LeftFrom + window.LeftTo) / 2.0
                    let centerRightT = (window.RightFrom + window.RightTo) / 2.0
                    Segment.point left centerLeftT
                    |> Result.bind (fun centerLeft ->
                        Segment.point right centerRightT
                        |> Result.map (fun centerRight -> centerLeft, centerRight))
                    |> Result.bind (fun (centerLeft, centerRight) ->
                        if Point.distance centerLeft centerRight <= min tolerance 1.0e-12<length> then
                            Ok(Some({ LeftT = centerLeftT; RightT = centerRightT; Point = midpoint centerLeft centerRight } : SegmentIntersection), false)
                        else
                            let local =
                                chordCrossing leftStart leftFinish rightStart rightFinish
                                |> Option.defaultWith (fun () -> chordClosestParameters leftStart leftFinish rightStart rightFinish)
                            let leftT = interpolate window.LeftFrom window.LeftTo (fst local)
                            let rightT = interpolate window.RightFrom window.RightTo (snd local)
                            Segment.point left leftT
                            |> Result.bind (fun leftPoint ->
                                Segment.point right rightT
                                |> Result.map (fun rightPoint ->
                                    if Point.distance leftPoint rightPoint <= tolerance then
                                        Some({ LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint } : SegmentIntersection), false
                                    else None, true)))
                | Error error, _, _, _
                | _, Error error, _, _
                | _, _, Error error, _
                | _, _, _, Error error -> Error error

    let private windowResolved window intersections =
        let leftWidth = window.LeftTo - window.LeftFrom
        let rightWidth = window.RightTo - window.RightFrom
        leftWidth <= 0.125<parameter>
        && rightWidth <= 0.125<parameter>
        && intersections
           |> List.exists (fun (intersection: SegmentIntersection) ->
               intersection.LeftT >= window.LeftFrom - leftWidth * 2.0
               && intersection.LeftT <= window.LeftTo + leftWidth * 2.0
               && intersection.RightT >= window.RightFrom - rightWidth * 2.0
               && intersection.RightT <= window.RightTo + rightWidth * 2.0)

    let private search left right options initialIntersections initial =
        let rec loop pending (found: SegmentIntersection list) examined =
            match pending with
            | [] -> Ok(found |> List.sortBy (fun item -> item.LeftT, item.RightT))
            | _ when examined >= maximumWindows -> Error(IntersectionTerminalWindowLimitExceeded maximumWindows)
            | window :: rest ->
                if windowResolved window found then loop rest found examined
                else
                    inspectWindow left right options.Tolerance window
                    |> Result.bind (fun (candidate, refine) ->
                        let found =
                            candidate
                            |> Option.map (fun value -> insertWindowCandidate value found)
                            |> Option.defaultValue found
                        if refine && window.Depth > 0 then loop ((splitNine window) @ rest) found (examined + 1)
                        else loop rest found (examined + 1))
        loop initial initialIntersections 0

    let private sampledCrossingCandidates left right tolerance =
        let intervals =
            [ for index in 0 .. 15 ->
                  Parameter.fromFloat(float index / 16.0),
                  Parameter.fromFloat(float (index + 1) / 16.0) ]
        [ for leftFrom, leftTo in intervals do
              for rightFrom, rightTo in intervals do
                  yield leftFrom, leftTo, rightFrom, rightTo ]
        |> List.fold (fun state (leftFrom, leftTo, rightFrom, rightTo) ->
            state
            |> Result.bind (fun found ->
                Segment.point left leftFrom
                |> Result.bind (fun leftStart ->
                    Segment.point left leftTo
                    |> Result.bind (fun leftEnd ->
                        Segment.point right rightFrom
                        |> Result.bind (fun rightStart ->
                            Segment.point right rightTo
                            |> Result.bind (fun rightEnd ->
                                match chordCrossing leftStart leftEnd rightStart rightEnd with
                                | None -> Ok found
                                | Some(leftLocal, rightLocal) ->
                                    let leftT = interpolate leftFrom leftTo leftLocal
                                    let rightT = interpolate rightFrom rightTo rightLocal
                                    refineCrossing left right tolerance leftT rightT
                                    |> Result.map (function
                                        | Some candidate -> insertWindowCandidate candidate found
                                        | None -> found))))))) (Ok [])

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

    let private collectIntersectionTerminalWindows left right options =
        let rec collect left right remainingDepth windows =
            pieceBoundingBox left
            |> Result.bind (fun leftBox ->
                pieceBoundingBox right
                |> Result.bind (fun rightBox ->
                    if not (boxesOverlap options.Tolerance leftBox rightBox) then Ok windows
                    elif remainingDepth <= 0
                         || (BoundingBox.diameter leftBox <= terminalSubdivisionTolerance
                             && BoundingBox.diameter rightBox <= terminalSubdivisionTolerance) then
                        if List.length windows + 9 > maximumWindows then
                            Error(IntersectionTerminalWindowLimitExceeded maximumWindows)
                        else Ok(addTerminalWindowGrid left right windows)
                    elif BoundingBox.diameter leftBox >= BoundingBox.diameter rightBox then
                        let first, second = splitPiece left
                        collect first right (remainingDepth - 1) windows
                        |> Result.bind (collect second right (remainingDepth - 1))
                    else
                        let first, second = splitPiece right
                        collect left first (remainingDepth - 1) windows
                        |> Result.bind (collect left second (remainingDepth - 1))))
        collect left right options.MaxDepth [] |> Result.map List.rev

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

    let private legacySearch left right options =
        let leftPiece = { Segment = left; From = 0.0<parameter>; To = 1.0<parameter> }
        let rightPiece = { Segment = right; From = 0.0<parameter>; To = 1.0<parameter> }
        boundaryMinima left right options
        |> Result.bind (fun boundary ->
            collectIntersectionTerminalWindows leftPiece rightPiece options
            |> Result.bind (fun windows ->
                minimaFromTerminalWindows windows options.Tolerance
                |> Result.bind (fun terminal ->
                    boundary @ terminal
                    |> List.fold (fun state minimum ->
                        state
                        |> Result.bind (fun intersections ->
                            if minimum.DistanceSquared > options.Tolerance * options.Tolerance then Ok intersections
                            else
                                Segment.point left minimum.LeftT
                                |> Result.bind (fun leftPoint ->
                                    Segment.point right minimum.RightT
                                    |> Result.map (fun rightPoint ->
                                        let candidate =
                                            { LeftT = minimum.LeftT
                                              RightT = minimum.RightT
                                              Point = midpoint leftPoint rightPoint }
                                        insert (intersectionDedupeTolerance options.Tolerance) candidate intersections)))) (Ok [])
                    |> Result.map (List.sortBy (fun item -> item.LeftT, item.RightT)))))

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
                        |> Result.map (fun candidate -> insert options.Tolerance candidate found))) (Ok [])
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
                            insert options.Tolerance intersection found) []
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

    let private bezierPoints = function
        | Line(startPoint, endPoint) -> Some [ startPoint; endPoint ]
        | QuadraticBezier(startPoint, control, endPoint) -> Some [ startPoint; control; endPoint ]
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Some [ startPoint; control1; control2; endPoint ]
        | Arc _ -> None

    let private certifiedDisjointTranslation left right =
        match bezierPoints left, bezierPoints right with
        | Some(leftStart :: _ as leftPoints), Some(rightStart :: _ as rightPoints) ->
            let translation = Point.displacement leftStart rightStart
            Point.squaredNorm translation > 1.0e-30<length^2>
            && List.length leftPoints = List.length rightPoints
            && List.forall2 (fun leftPoint rightPoint ->
                abs (rightPoint.X - leftPoint.X - translation.X) <= 1.0e-14<length>
                && abs (rightPoint.Y - leftPoint.Y - translation.Y) <= 1.0e-14<length>) leftPoints rightPoints
            && (let axis = Point.create (-translation.Y) translation.X
                let differences =
                    leftPoints
                    |> List.pairwise
                    |> List.map (fun (first, second) -> Point.dot (Point.displacement first second) axis)
                List.forall (fun value -> value > 1.0e-15<length^2>) differences
                || List.forall (fun value -> value < -1.0e-15<length^2>) differences)
        | _ -> false

    let private circularRadius (radius: Point<length>) =
        abs (abs radius.X - abs radius.Y) <= 1.0e-12<length>

    let private positiveAngleRemainder (angle: float<degree>) =
        let turns = floor (Degree.toFloat angle / 360.0)
        angle - turns * 360.0<degree>

    let private circularArcParameter (pointValue: Point<length>) (arc: CenterArcData) =
        let angle = Trig.atan2Degrees (pointValue.Y - arc.Center.Y) (pointValue.X - arc.Center.X)
        let progress =
            if arc.DeltaAngle >= 0.0<degree> then
                positiveAngleRemainder (angle - arc.StartAngle) / arc.DeltaAngle
            else
                positiveAngleRemainder (arc.StartAngle - angle) / -arc.DeltaAngle
        if progress >= -1.0e-9 && progress <= 1.0 + 1.0e-9 then
            Some(Parameter.fromFloat(max 0.0 (min 1.0 progress)))
        else None

    let private circularArcIntersections left right tolerance =
        let endpointArc = function Arc endpoint -> endpoint | _ -> failwith "expected circular arc"
        Ellipse.endpointToCenter (endpointArc left)
        |> Result.mapError (fun _ -> DegenerateArc)
        |> Result.bind (fun leftArc ->
            Ellipse.endpointToCenter (endpointArc right)
            |> Result.mapError (fun _ -> DegenerateArc)
            |> Result.bind (fun rightArc ->
                let displacement = Point.displacement leftArc.Center rightArc.Center
                let distanceSquared = Point.squaredNorm displacement
                let distance = sqrt (float distanceSquared) * 1.0<length>
                let leftRadius = leftArc.Radius.X
                let rightRadius = rightArc.Radius.X
                let radiusDifference = abs (leftRadius - rightRadius)
                if distance <= 1.0e-15<length> then endpointCandidates left right tolerance
                elif distance > leftRadius + rightRadius + tolerance then Ok []
                elif distance < radiusDifference - tolerance then Ok []
                else
                    let along =
                        (leftRadius * leftRadius - rightRadius * rightRadius + distanceSquared)
                        / (2.0 * distance)
                    let heightSquared = leftRadius * leftRadius - along * along
                    if heightSquared < -tolerance * 1.0<length> then Ok []
                    else
                        let height = if heightSquared <= 0.0<length^2> then 0.0<length> else sqrt (float heightSquared) * 1.0<length>
                        let dx = displacement.X
                        let dy = displacement.Y
                        let basePoint =
                            Point.create
                                (leftArc.Center.X + along * dx / distance)
                                (leftArc.Center.Y + along * dy / distance)
                        let offset = Point.create (-dy * height / distance) (dx * height / distance)
                        let candidates =
                            if height <= tolerance then [ basePoint ]
                            else [ Point.translate basePoint offset; Point.translate basePoint (Point.negate offset) ]
                        candidates
                        |> List.fold (fun found pointValue ->
                            match circularArcParameter pointValue leftArc, circularArcParameter pointValue rightArc with
                            | Some leftT, Some rightT ->
                                insertWindowCandidate
                                    { LeftT = leftT; RightT = rightT; Point = pointValue }
                                    found
                            | _ -> found) []
                        |> Ok))

    let private segmentIntersectionsValidOptions left right options =
        match left, right with
        | Line(startPoint, endPoint), _ ->
            lineSegmentIntersections startPoint endPoint true right options
        | _, Line(startPoint, endPoint) ->
            lineSegmentIntersections startPoint endPoint false left options
        | Arc leftArc, Arc rightArc when circularRadius leftArc.Radius && circularRadius rightArc.Radius ->
            circularArcIntersections left right options.Tolerance
        | _ ->
            if certifiedDisjointTranslation left right then Ok []
            else
                endpointCandidates left right options.Tolerance
                |> Result.bind (fun endpoints ->
                    sampledCrossingCandidates left right options.Tolerance
                    |> Result.bind (fun crossings ->
                        let initial = List.fold (fun found candidate -> insertWindowCandidate candidate found) endpoints crossings
                        match search left right options initial (initialWindows options.MaxDepth) with
                        | Error(IntersectionTerminalWindowLimitExceeded _) -> legacySearch left right options
                        | result -> result))

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
        | _ -> InvalidSelfIntersectionDistanceTolerance 0.0<length>

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
        | Arc arc when arc.Start = arc.End && arc.Radius.X <> 0.0<length> && arc.Radius.Y <> 0.0<length> ->
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
                                                            Touching(direction, incomingOrder, outgoingOrder, apertures))))))
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
