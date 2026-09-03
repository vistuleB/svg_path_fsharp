namespace SvgPath

type ConvexHullError =
    | ConvexHullPathError of SegmentError
    | ConvexHullConstructionFailed

type ConvexHullConstructionError =
    | ConstructionPathError of SegmentError
    | ConsecutiveCurves
    | DuplicateAdjacentTValues
    | RefinementReachedMaxIterations of int
    | PurificationReachedMaxIterations of int
    | LoopUnionCollapsed
    | TangentSearchDegenerateLoop
    | TangentSearchNonConvexVertex of int
    | TangentSearchExpectedTwoTangencies of int
    | SeededWorstDirectionExceededThreshold of direction: float<degree> * threshold: float<degree>

type PointLoopView =
    | TangentPoint
    | OutsidePoint
    | InsidePoint

[<Struct>]
type DirectionalExtent =
    { LowerPoint: Point<length>
      UpperPoint: Point<length>
      Width: float<length> }

[<Struct>]
type WidthSearchOptions =
    { Accuracy: float<length>
      MaxDepth: int }

[<Struct>]
type WidthExtremum =
    { Direction: Point<1>
      LowerPoint: Point<length>
      UpperPoint: Point<length>
      Center: Point<length>
      Width: float<length>
      LowerBound: float<length>
      UpperBound: float<length>
      Converged: bool }

[<Struct>]
type MinimumWidthStrip =
    { Width: float<length>
      Direction: Point<1>
      LowerPoint: Point<length>
      UpperPoint: Point<length>
      LowerSupport: float<length>
      UpperSupport: float<length> }

type MinimumWidthDecision =
    | MinimumWidthFits of MinimumWidthStrip
    | MinimumWidthExceeds of lowerBound: float<length>
    | MinimumWidthUnresolved of lowerBound: float<length> * bestWidth: float<length>

type DirectionalSupport = DirectionalExtent

[<RequireQualifiedAccess>]
module ConvexHull =
    type private SupportSample =
        { T: float<parameter>
          Point: Point<length>
          Value: float<length> }

    type private SupportRun = { Ts: float<parameter> list }

    type private RunEndpoint =
        | PointEndpoint of float<parameter>
        | CurveEndpoint of float<parameter> * float<parameter>

    type private HullPiece =
        | HullCurve of float<parameter> * float<parameter>
        | HullLine of float<parameter> * float<parameter>

    type private WidthSample =
        { Angle: float<degree>
          Support: DirectionalExtent }

    type private WidthInterval =
        { From: WidthSample
          ``To``: WidthSample }

    [<Struct>]
    type private LoopParam =
        { SegmentIndex: int
          T: float<parameter> }

    [<Struct>]
    type private ConvexLoop =
        { Segments: Segment list
          Enclosure: Point<length> list }

    [<Struct>]
    type private LoopSupport =
        { Param: LoopParam
          Point: Point<length>
          Value: float<length> }

    type private LoopWinner = LoopA | LoopB

    [<Struct>]
    type private LoopSample =
        { Angle: float<degree>
          Winner: LoopWinner
          A: LoopSupport
          B: LoopSupport
          Difference: float<length> }

    [<Struct>]
    type private LoopBoundary =
        { Angle: float<degree>
          A: LoopSupport
          B: LoopSupport
          From: LoopWinner
          ``To``: LoopWinner }

    type private UnionPiece =
        | HullLineAB of LoopParam * LoopParam
        | HullLineBA of LoopParam * LoopParam
        | LoopPieceA of LoopParam * LoopParam
        | LoopPieceB of LoopParam * LoopParam

    [<Struct>]
    type private TangentCandidate =
        { VertexIndex: int
          Point: Point<length> }

    [<Struct>]
    type private LoopTangentCandidate =
        { Param: LoopParam
          Point: Point<length> }

    type private TangentSearchOrientation =
        | ExactSearchOrientation of clockwise: bool
        | LineLikeSearchOrientation of clockwise: bool
        | DegenerateSearchOrientation

    type private TangentOrientation =
        | FoundTangentOrientation of clockwise: bool
        | NoTangentOrientation
        | ConflictingTangentOrientation

    let defaultWidthSearchOptions =
        { Accuracy = 1.0e-9<length>
          MaxDepth = 20 }

    let private loopUnionSampleCount = 360
    let private loopUnionTieTolerance = 1.0e-7<length>
    let private loopUnionAngleTolerance = 0.02<degree>
    let private loopUnionPointTolerance = 1.0e-6<length>
    let private sameT = 1.0e-6<parameter>
    let private seededWorstDirectionStep = 0.1<degree>
    let private seededWorstDirectionRefinedStep = 0.01<degree>
    let private loopUnionSeedMaxDrift = 1.0<degree>
    let private pointTolerance = 1.0e-9<length>
    let private widthLowerBoundRoundoffFactor = 1.0e-12

    let private cross origin a b =
        Point.cross (Point.displacement origin a) (Point.displacement origin b)

    let private distinctSorted points =
        points
        |> List.distinct
        |> List.sortWith (fun a b ->
            let byX = compare a.X b.X
            if byX <> 0 then byX else compare a.Y b.Y)

    /// Return convex vertices in boundary order, without repeating the first vertex.
    let private hullVertices points =
        let points = distinctSorted points
        let rec build stack remaining =
            match stack, remaining with
            | _, [] -> List.rev stack
            | b :: a :: rest, point :: tail when cross a b point <= 0.0<length^2> ->
                build (a :: rest) remaining
            | _, point :: tail -> build (point :: stack) tail

        match points with
        | [] | [ _ ] -> points
        | _ ->
            let lower = build [] points
            let upper = build [] (List.rev points)
            (lower |> List.take (List.length lower - 1))
            @ (upper |> List.take (List.length upper - 1))

    let private segmentIsPointLike segment =
        match Segment.boundingBox segment with
        | Error _ -> true
        | Ok bounds -> BoundingBox.diameter bounds <= 1.0e-9<length>

    let private exactSimpleSegmentHull segment =
        let startPoint = Segment.start segment
        let endPoint = Segment.finish segment
        let pieces =
            if segmentIsPointLike segment then [ Line(startPoint, startPoint); Line(startPoint, startPoint) ]
            else
                match segment with
                | Line _ -> [ segment; Segment.reverse segment ]
                | QuadraticBezier _ | Arc _ -> [ segment; Line(endPoint, startPoint) ]
                | CubicBezier _ -> []
        match pieces with
        | [] -> Error LoopUnionCollapsed
        | _ ->
            Subpath.createWith WiggleThenBridge pieces
            |> Result.bind (Subpath.setClosedWith WiggleThenBridge true)
            |> Result.mapError ConstructionPathError

    let private supportCandidates segment direction =
        match segment with
        | Line(startPoint, endPoint) ->
            Bezier.projectionExtrema (LinearBezierData(startPoint, endPoint)) direction
        | QuadraticBezier(startPoint, control, endPoint) ->
            Bezier.projectionExtrema (QuadraticBezierData(startPoint, control, endPoint)) direction
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Bezier.projectionExtrema (CubicBezierData(startPoint, control1, control2, endPoint)) direction
        | Arc endpoint ->
            match Ellipse.endpointToCenter endpoint with
            | Ok arc -> Ellipse.arcProjectionExtrema arc direction
            | Error _ -> []

    let private supportSample segment direction =
        (0.0<parameter> :: 1.0<parameter> :: supportCandidates segment direction)
        |> List.choose (fun t ->
            Segment.point segment t
            |> Result.toOption
            |> Option.map (fun point ->
                { T = t
                  Point = point
                  Value = Point.dot point direction }))
        |> List.maxBy _.Value

    let internalSegmentSupport segment angle =
        let direction = Point.direction angle
        let sample = supportSample segment direction
        Ok(sample.T, sample.Point, sample.Value)

    let private mergeCircularRuns (runs: SupportRun list) =
        match runs with
        | [] | [ _ ] -> runs
        | first :: middle ->
            let last = List.last middle
            match first.Ts, List.tryLast last.Ts with
            | firstT :: _, Some lastT when abs (firstT - lastT) <= 0.08<parameter> ->
                ({ Ts = last.Ts @ first.Ts }: SupportRun) :: (middle |> List.take (List.length middle - 1))
            | _ -> runs

    let private collapseSupportRuns (samples: SupportSample list) =
        let rec loop current reversedRuns (remaining: SupportSample list) =
            match remaining with
            | [] -> List.rev ({ Ts = List.rev current } :: reversedRuns) |> mergeCircularRuns
            | sample :: rest ->
                match current with
                | previous :: _ when abs (sample.T - previous) <= 0.08<parameter> ->
                    loop (sample.T :: current) reversedRuns rest
                | _ -> loop [ sample.T ] ({ Ts = List.rev current } :: reversedRuns) rest
        match samples with
        | [] -> []
        | first :: rest -> loop [ first.T ] [] rest

    let private runEndpoint (run: SupportRun) =
        let minimum = List.min run.Ts
        let maximum = List.max run.Ts
        if maximum - minimum < 1.0e-6<parameter> then
            PointEndpoint(List.average run.Ts)
        else CurveEndpoint(List.head run.Ts, List.last run.Ts)

    let private endpointStart = function PointEndpoint t | CurveEndpoint(t, _) -> t
    let private endpointEnd = function PointEndpoint t | CurveEndpoint(_, t) -> t

    let private piecesFromRuns (runs: SupportRun list) =
        let endpoints = List.map runEndpoint runs
        let curvePiece = function
            | PointEndpoint _ -> []
            | CurveEndpoint(a, b) when abs (a - b) <= 1.0e-6<parameter> -> []
            | CurveEndpoint(a, b) -> [ HullCurve(a, b) ]
        match endpoints with
        | [] -> []
        | _ ->
            List.pairwise (endpoints @ [ List.head endpoints ])
            |> List.collect (fun (current, next) ->
                curvePiece current @ [ HullLine(endpointEnd current, endpointStart next) ])

    let private cubicTangencyCoefficients startPoint control1 control2 endPoint fixedPoint =
        let a =
            Point.add
                (Point.add (Point.scale -1.0 startPoint) (Point.scale 3.0 control1))
                (Point.add (Point.scale -3.0 control2) endPoint)
        let b =
            Point.add
                (Point.add (Point.scale 3.0 startPoint) (Point.scale -6.0 control1))
                (Point.scale 3.0 control2)
        let c = Point.displacement startPoint control1 |> Point.scale 3.0
        let s = Point.displacement fixedPoint startPoint
        let coefficients =
            [ -Point.cross a b
              -2.0 * Point.cross a c
              3.0 * Point.cross s a - Point.cross b c
              2.0 * Point.cross s b
              Point.cross s c ]
        let scale = coefficients |> List.map abs |> List.max
        if scale = 0.0<length^2> then coefficients |> List.map (fun _ -> 0.0)
        else coefficients |> List.map (fun coefficient -> float (coefficient / scale))

    let private refineChordTangent source approximate other =
        if approximate < 1.0e-6<parameter> || approximate > 1.0<parameter> - 1.0e-6<parameter> then approximate
        else
            match source with
            | CubicBezier(startPoint, control1, control2, endPoint) ->
                match Segment.point source other with
                | Error _ -> approximate
                | Ok fixedPoint ->
                    let coefficients = cubicTangencyCoefficients startPoint control1 control2 endPoint fixedPoint
                    let options: PolynomialOptions = { MaxIterations = 100 }
                    match Root.polynomialRootIsolationsWith coefficients
                            (max 0.0<parameter> (approximate - 0.08<parameter>))
                            (min 1.0<parameter> (approximate + 0.08<parameter>)) options with
                    | Error _ -> approximate
                    | Ok isolations ->
                        isolations
                        |> List.filter (fun isolation -> abs (isolation.Estimate - other) > 1.0e-6<parameter>)
                        |> List.sortBy (fun isolation -> abs (isolation.Estimate - approximate))
                        |> List.tryHead
                        |> Option.map (fun isolation ->
                            if isolation.Lower = isolation.Upper then isolation.Estimate
                            else
                                let lowerValue = Root.evaluatePolynomial coefficients isolation.Lower
                                let upperValue = Root.evaluatePolynomial coefficients isolation.Upper
                                let sameSign a b = (a < 0.0 && b < 0.0) || (a > 0.0 && b > 0.0)
                                if sameSign lowerValue upperValue then isolation.Estimate
                                else
                                    Root.bisectIsolationUntil
                                        (Root.evaluatePolynomial coefficients)
                                        isolation.Lower
                                        isolation.Upper
                                        100
                                        (fun lower upper ->
                                            match Segment.betweenInside source lower upper with
                                            | Error _ -> false
                                            | Ok portion ->
                                                match Segment.boundingBox portion with
                                                | Error _ -> false
                                                | Ok bounds -> BoundingBox.diameter bounds <= pointTolerance)
                                    |> Result.map _.Estimate
                                    |> Result.defaultValue isolation.Estimate)
                        |> Option.defaultValue approximate
            | _ -> approximate

    let private refineHullPieces source pieces =
        match pieces with
        | [] | [ _ ] -> pieces
        | _ ->
            let count = List.length pieces
            let at index = pieces[(index + count) % count]
            let refinedCurves =
                pieces
                |> List.mapi (fun index current ->
                    match current with
                    | HullCurve(fromT, toT) ->
                        let fromT =
                            match at (index - 1) with
                            | HullLine(other, _) -> refineChordTangent source fromT other
                            | _ -> fromT
                        let toT =
                            match at (index + 1) with
                            | HullLine(_, other) -> refineChordTangent source toT other
                            | _ -> toT
                        HullCurve(fromT, toT)
                    | line -> line)
            let refinedAt index = refinedCurves[(index + count) % count]
            refinedCurves
            |> List.mapi (fun index current ->
                match current with
                | HullLine(fromT, toT) ->
                    let fromT = match refinedAt (index - 1) with HullCurve(_, value) -> value | _ -> fromT
                    let toT = match refinedAt (index + 1) with HullCurve(value, _) -> value | _ -> toT
                    HullLine(fromT, toT)
                | curve -> curve)

    let private pieceSegment source = function
        | HullCurve(fromT, toT) -> Segment.betweenInside source fromT toT
        | HullLine(fromT, toT) ->
            Segment.point source fromT
            |> Result.bind (fun startPoint ->
                Segment.point source toT |> Result.map (fun endPoint -> Line(startPoint, endPoint)))

    let private sampledCubicHull segment =
        let samples =
            [ 0 .. 3599 ]
            |> List.map (fun index ->
                Point.direction (Degree.fromFloat (float index / 10.0))
                |> supportSample segment)
        let pieces = samples |> collapseSupportRuns |> piecesFromRuns |> refineHullPieces segment
        pieces
        |> List.map (pieceSegment segment)
        |> List.fold
            (fun accumulated next ->
                accumulated
                |> Result.bind (fun segments -> next |> Result.map (fun segment -> segment :: segments)))
            (Ok [])
        |> Result.map List.rev
        |> Result.bind (fun segments ->
            Subpath.createWith WiggleThenBridge segments
            |> Result.bind (Subpath.setClosedWith WiggleThenBridge true))
        |> Result.mapError ConstructionPathError

    let private normalizeAngle (angle: float<degree>) =
        let value = Degree.toFloat angle
        Degree.fromFloat (value - floor (value / 360.0) * 360.0)

    let private loopSupport (loop: ConvexLoop) angle =
        let direction = Point.direction angle
        loop.Segments
        |> List.mapi (fun index segment ->
            let sample = supportSample segment direction
            { Param = { SegmentIndex = index; T = sample.T }
              Point = sample.Point
              Value = sample.Value })
        |> List.maxBy _.Value

    let private loopSample loopA loopB angle =
        let a = loopSupport loopA angle
        let b = loopSupport loopB angle
        let difference = a.Value - b.Value
        { Angle = angle
          Winner = if difference >= 0.0<length> then LoopA else LoopB
          A = a
          B = b
          Difference = difference }

    let private uniqueLoopSampleAngles angles =
        let rec loop previous kept remaining =
            match remaining with
            | [] -> List.rev kept
            | angle :: rest ->
                match previous with
                | Some value when angle - value < loopUnionAngleTolerance -> loop previous kept rest
                | _ -> loop (Some angle) (angle :: kept) rest
        let distinct = loop None [] angles
        match distinct with
        | [] | [ _ ] -> distinct
        | first :: _ ->
            let last = List.last distinct
            if first + 360.0<degree> - last < loopUnionAngleTolerance then
                distinct |> List.take (List.length distinct - 1)
            else distinct

    let private loopInitialSampleAngles sampleCount seedAngles =
        let uniform =
            [ 0 .. sampleCount ]
            |> List.map (fun index -> Degree.fromFloat (float index * 360.0 / float sampleCount))
        uniform @ seedAngles
        |> List.map normalizeAngle
        |> List.sort
        |> uniqueLoopSampleAngles

    let private circularPairs items =
        match items with
        | [] | [ _ ] -> []
        | _ -> List.pairwise (items @ [ List.head items ])

    let private unwrapAngleAfter left right =
        if right < left then right + 360.0<degree> else right

    let private bisectLoopBoundary loopA loopB (left: LoopSample) (right: LoopSample) =
        let rec bisect (left: LoopSample) (right: LoopSample) remaining =
            if remaining <= 0 || abs left.Difference <= loopUnionTieTolerance then left
            else
                let middle = loopSample loopA loopB ((left.Angle + right.Angle) / 2.0)
                if middle.Winner = left.Winner then bisect middle right (remaining - 1)
                else bisect left middle (remaining - 1)
        bisect left right 32

    let private refineLoopBoundary loopA loopB leftAngle rightAngle leftWinner =
        let left = loopSample loopA loopB leftAngle
        let right = loopSample loopA loopB (unwrapAngleAfter leftAngle rightAngle)
        let refined = bisectLoopBoundary loopA loopB left right
        let angle = normalizeAngle refined.Angle
        let boundary = loopSample loopA loopB angle
        { Angle = angle
          A = boundary.A
          B = boundary.B
          From = leftWinner
          ``To`` = if leftWinner = LoopA then LoopB else LoopA }

    let private loopTransitionBoundaries loopA loopB samples =
        samples
        |> circularPairs
        |> List.choose (fun (left, right) ->
            if left.Winner = right.Winner then None
            else Some(refineLoopBoundary loopA loopB left.Angle right.Angle left.Winner))

    let private loopPiecesFromBoundaries boundaries =
        boundaries
        |> circularPairs
        |> List.collect (fun (startBoundary, endBoundary) ->
            let loopPiece =
                match startBoundary.``To`` with
                | LoopA -> LoopPieceA(startBoundary.A.Param, endBoundary.A.Param)
                | LoopB -> LoopPieceB(startBoundary.B.Param, endBoundary.B.Param)
            let linePiece =
                match endBoundary.From, endBoundary.``To`` with
                | LoopA, LoopB -> HullLineAB(endBoundary.A.Param, endBoundary.B.Param)
                | LoopB, LoopA -> HullLineBA(endBoundary.B.Param, endBoundary.A.Param)
                | _ -> loopPiece
            [ loopPiece; linePiece ])

    let private loopPoint (loop: ConvexLoop) parameter =
        Segment.point loop.Segments[parameter.SegmentIndex] parameter.T
        |> Result.defaultWith (failwithf "%A")

    let private loopPointsFar left right =
        Point.squaredDistance left right > loopUnionPointTolerance * loopUnionPointTolerance

    let private compactLoopPieces loopA loopB pieces =
        pieces
        |> List.filter (function
            | LoopPieceA(fromParameter, toParameter) -> loopPointsFar (loopPoint loopA fromParameter) (loopPoint loopA toParameter)
            | LoopPieceB(fromParameter, toParameter) -> loopPointsFar (loopPoint loopB fromParameter) (loopPoint loopB toParameter)
            | HullLineAB(a, b) -> loopPointsFar (loopPoint loopA a) (loopPoint loopB b)
            | HullLineBA(b, a) -> loopPointsFar (loopPoint loopB b) (loopPoint loopA a))

    let private allOneLoop samples =
        match samples with
        | [] -> []
        | first :: _ ->
            match first.Winner with
            | LoopA -> [ LoopPieceA(first.A.Param, first.A.Param) ]
            | LoopB -> [ LoopPieceB(first.B.Param, first.B.Param) ]

    let private loopUnion loopA loopB seedAngles =
        let samples =
            loopInitialSampleAngles loopUnionSampleCount seedAngles
            |> List.map (loopSample loopA loopB)
        match loopTransitionBoundaries loopA loopB samples with
        | [] -> allOneLoop samples
        | boundaries -> boundaries |> loopPiecesFromBoundaries |> compactLoopPieces loopA loopB

    let private nextIndex index count = if index + 1 >= count then 0 else index + 1

    let private walkSegmentIndices fromIndex toIndex count =
        let rec loop current reversed =
            if current = toIndex then List.rev (current :: reversed)
            else loop (nextIndex current count) (current :: reversed)
        loop fromIndex []

    let private partialSegment segment fromT toT =
        Segment.betweenInside segment fromT toT |> Result.defaultWith (failwithf "%A")

    let private loopPieceSegments (loop: ConvexLoop) fromParameter toParameter =
        let segments = loop.Segments
        let fromIndex, toIndex = fromParameter.SegmentIndex, toParameter.SegmentIndex
        let fromT, toT = fromParameter.T, toParameter.T
        if fromIndex = toIndex && abs (fromT - toT) <= sameT then segments
        elif fromIndex = toIndex && fromT <= toT then [ partialSegment segments[fromIndex] fromT toT ]
        elif fromIndex = toIndex then
            let middle =
                walkSegmentIndices (nextIndex fromIndex segments.Length) fromIndex segments.Length
                |> List.takeWhile ((<>) fromIndex)
                |> List.map (fun index -> partialSegment segments[index] 0.0<parameter> 1.0<parameter>)
            partialSegment segments[fromIndex] fromT 1.0<parameter>
            :: (middle @ [ partialSegment segments[fromIndex] 0.0<parameter> toT ])
        else
            walkSegmentIndices fromIndex toIndex segments.Length
            |> List.map (fun index ->
                if index = fromIndex then partialSegment segments[index] fromT 1.0<parameter>
                elif index = toIndex then partialSegment segments[index] 0.0<parameter> toT
                else partialSegment segments[index] 0.0<parameter> 1.0<parameter>)

    let private unionPieceSegments loopA loopB pieces =
        pieces
        |> List.collect (function
            | LoopPieceA(fromParameter, toParameter) -> loopPieceSegments loopA fromParameter toParameter
            | LoopPieceB(fromParameter, toParameter) -> loopPieceSegments loopB fromParameter toParameter
            | HullLineAB(a, b) -> [ Line(loopPoint loopA a, loopPoint loopB b) ]
            | HullLineBA(b, a) -> [ Line(loopPoint loopB b, loopPoint loopA a) ])
        |> List.filter (segmentIsPointLike >> not)

    let private loopSupportDominance loopA loopB =
        [ 0 .. loopUnionSampleCount - 1 ]
        |> List.fold (fun (aContainsB, bContainsA) index ->
            let angle = Degree.fromFloat (float index * 360.0 / float loopUnionSampleCount)
            let difference = (loopSample loopA loopB angle).Difference
            aContainsB && difference >= -loopUnionTieTolerance,
            bContainsA && difference <= loopUnionTieTolerance) (true, true)

    let private unionLoopSegments left right =
        let enclosure segments =
            match segments |> List.map Segment.boundingBox |> List.fold (fun state next ->
                state |> Result.bind (fun boxes -> next |> Result.map (fun bounds -> bounds :: boxes))) (Ok []) with
            | Error _ -> segments |> List.collect (fun segment -> [ Segment.start segment; Segment.finish segment ]) |> hullVertices
            | Ok [] -> []
            | Ok boxes ->
                let bounds = boxes |> List.reduce BoundingBox.union
                [ bounds.Min
                  Point.create bounds.Max.X bounds.Min.Y
                  bounds.Max
                  Point.create bounds.Min.X bounds.Max.Y ]
        let loopA = { Segments = left; Enclosure = enclosure left }
        let loopB = { Segments = right; Enclosure = enclosure right }
        match loopUnion loopA loopB [] |> unionPieceSegments loopA loopB with
        | [] ->
            match loopSupportDominance loopA loopB with
            | true, _ -> Ok left
            | false, true -> Ok right
            | false, false -> Error LoopUnionCollapsed
        | segments -> Ok segments

    let private polygonSignedArea points =
        points
        |> circularPairs
        |> List.sumBy (fun (a, b) -> a.X * b.Y - b.X * a.Y)
        |> fun twiceArea -> twiceArea / 2.0

    let private polygonStrictlyContains points candidate =
        if List.length points < 3 then false
        else
            let orientation = polygonSignedArea points
            if abs orientation <= 1.0e-18<length^2> then false
            else
                points
                |> circularPairs
                |> List.forall (fun (a, b) ->
                    let edge = Point.displacement a b
                    let offset = Point.displacement a candidate
                    let turn = Point.cross edge offset
                    let tolerance = 1.0e-9 * Point.norm edge * Point.norm offset
                    if orientation > 0.0<length^2> then turn > tolerance else turn < -tolerance)

    let private loopVertices segments =
        match segments with
        | [] -> []
        | first :: _ ->
            let points = Segment.start first :: List.map Segment.finish segments
            match List.tryLast points with
            | Some last when last = List.head points -> List.take (List.length points - 1) points
            | _ -> points

    let private closestPointOnLineSegment a b point =
        let ab = Point.displacement a b
        let denominator = Point.dot ab ab
        if denominator <= 0.0<length^2> then a
        else
            let t = max 0.0 (min 1.0 (float (Point.dot (Point.displacement a point) ab / denominator)))
            Point.add a (Point.scale t ab)

    let private closestPointOnPolyline closed points point =
        let edges =
            match points with
            | [] | [ _ ] -> []
            | _ when closed -> circularPairs points
            | _ -> List.pairwise points
        edges
        |> List.map (fun (a, b) -> closestPointOnLineSegment a b point)
        |> function
            | [] -> List.tryHead points |> Option.defaultValue point
            | candidates -> List.minBy (Point.squaredDistance point) candidates

    let private pointChordPolygonLoopSeparation (loop: ConvexLoop) point =
        let vertices = loopVertices loop.Segments
        match vertices with
        | [] -> None
        | [ only ] ->
            if Point.distance only point <= pointTolerance then None
            else Some(Point.heading (Point.displacement only point), only)
        | [ a; b ] ->
            let closest = closestPointOnLineSegment a b point
            if Point.distance closest point <= pointTolerance then None
            else Some(Point.heading (Point.displacement closest point), closest)
        | _ ->
            let area = polygonSignedArea vertices
            let inside =
                if abs area <= pointTolerance * pointTolerance then false
                else
                    vertices
                    |> circularPairs
                    |> List.forall (fun (a, b) ->
                        let turn = Point.cross (Point.displacement a b) (Point.displacement a point)
                        if area > 0.0<length^2> then turn >= 0.0<length^2> else turn <= 0.0<length^2>)
            if inside then None
            else
                let closest = closestPointOnPolyline true vertices point
                if Point.distance closest point <= pointTolerance then None
                else Some(Point.heading (Point.displacement closest point), closest)

    [<Struct>]
    type private SeededWorstDirectionState =
        { Direction: float<degree>
          Advantage: float<length> }

    let private loopBAdvantage loopA loopB angle = -(loopSample loopA loopB angle).Difference

    let private seededWorstDirectionCandidate loopA loopB origin candidateDirection best maxDrift =
        if abs (candidateDirection - origin) > maxDrift + 1.0e-9<degree> then best
        else
            let advantage = loopBAdvantage loopA loopB candidateDirection
            if advantage > best.Advantage then
                { Direction = candidateDirection; Advantage = advantage }
            else best

    let private seededWorstDirectionLocalSearch loopA loopB origin initial step maxDrift =
        let rec search current =
            let upper =
                seededWorstDirectionCandidate loopA loopB origin (current.Direction + step) current maxDrift
            let candidate =
                seededWorstDirectionCandidate loopA loopB origin (current.Direction - step) upper maxDrift
            if candidate.Direction = current.Direction then current else search candidate
        search initial

    let private findSeededWorstDirection loopA loopB direction threshold =
        let initial =
            { Direction = direction
              Advantage = loopBAdvantage loopA loopB direction }
        let coarse =
            seededWorstDirectionLocalSearch loopA loopB direction initial seededWorstDirectionStep threshold
        let refined =
            seededWorstDirectionLocalSearch loopA loopB direction coarse seededWorstDirectionRefinedStep threshold
        Ok(normalizeAngle refined.Direction, normalizeAngle refined.Direction)

    let internalFindSeededWorstDirection loopASegments loopBSegments direction threshold =
        let enclosure segments =
            segments
            |> List.map Segment.boundingBox
            |> List.choose Result.toOption
            |> function
                | [] -> []
                | boxes ->
                    let bounds = List.reduce BoundingBox.union boxes
                    [ bounds.Min; Point.create bounds.Max.X bounds.Min.Y; bounds.Max; Point.create bounds.Min.X bounds.Max.Y ]
        let loopA = { Segments = loopASegments; Enclosure = enclosure loopASegments }
        let loopB = { Segments = loopBSegments; Enclosure = enclosure loopBSegments }
        findSeededWorstDirection loopA loopB direction threshold

    let internalLoopInitialSampleAngles sampleCount seedAngles =
        loopInitialSampleAngles sampleCount seedAngles

    let internalLoopUnionSegmentsWithSeedAngles loopASegments loopBSegments seedAngles =
        let enclosure segments =
            segments
            |> List.map Segment.boundingBox
            |> List.choose Result.toOption
            |> function
                | [] -> []
                | boxes ->
                    let bounds = List.reduce BoundingBox.union boxes
                    [ bounds.Min; Point.create bounds.Max.X bounds.Min.Y; bounds.Max; Point.create bounds.Min.X bounds.Max.Y ]
        let loopA = { Segments = loopASegments; Enclosure = enclosure loopASegments }
        let loopB = { Segments = loopBSegments; Enclosure = enclosure loopBSegments }
        loopUnion loopA loopB seedAngles |> unionPieceSegments loopA loopB

    let private loopEndpoints loop =
        loop.Segments
        |> List.collect (fun segment -> [ Segment.start segment; Segment.finish segment ])
        |> List.fold (fun points point ->
            if List.exists (fun existing -> Point.distance existing point <= pointTolerance) points then points
            else point :: points) []
        |> List.rev

    let private ambitiousRepairSeedAngles current addition =
        addition
        |> loopEndpoints
        |> List.fold (fun state point ->
            state
            |> Result.bind (fun angles ->
                match pointChordPolygonLoopSeparation current point with
                | None -> Ok angles
                | Some(direction, _) ->
                    findSeededWorstDirection current addition direction loopUnionSeedMaxDrift
                    |> Result.map (fun (lower, upper) -> upper :: lower :: angles))) (Ok [])
        |> Result.map List.rev

    let private ambitiousRepairLoopWithLoop current addition =
        ambitiousRepairSeedAngles current addition
        |> Result.map (function
            | [] -> current
            | seedAngles ->
                match loopUnion current addition seedAngles |> unionPieceSegments current addition with
                | [] -> current
                | segments -> { current with Segments = segments })

    let internalAmbitiousRepairLoopWithLoop currentSegments additionSegments =
        let enclosure segments =
            segments
            |> List.map Segment.boundingBox
            |> List.choose Result.toOption
            |> function
                | [] -> []
                | boxes ->
                    let bounds = List.reduce BoundingBox.union boxes
                    [ bounds.Min; Point.create bounds.Max.X bounds.Min.Y; bounds.Max; Point.create bounds.Min.X bounds.Max.Y ]
        ambitiousRepairLoopWithLoop
            { Segments = currentSegments; Enclosure = enclosure currentSegments }
            { Segments = additionSegments; Enclosure = enclosure additionSegments }
        |> Result.map _.Segments

    let internalPointChordPolygonLoopSeparation segments point =
        let enclosure =
            segments
            |> List.map Segment.boundingBox
            |> List.choose Result.toOption
            |> function
                | [] -> []
                | boxes ->
                    let bounds = List.reduce BoundingBox.union boxes
                    [ bounds.Min; Point.create bounds.Max.X bounds.Min.Y; bounds.Max; Point.create bounds.Min.X bounds.Max.Y ]
        pointChordPolygonLoopSeparation { Segments = segments; Enclosure = enclosure } point

    let internalPointLoopView point atPoint arriving leaving clockwise =
        let sight = Point.displacement point atPoint
        let arrivingTurn = Point.cross sight arriving
        let leavingTurn = Point.cross sight leaving
        let oppositeSigns a b = (a < 0.0<_> && b > 0.0<_>) || (a > 0.0<_> && b < 0.0<_>)

        if arrivingTurn = 0.0<_> || leavingTurn = 0.0<_> || oppositeSigns arrivingTurn leavingTurn then
            TangentPoint
        elif clockwise then
            if arrivingTurn < 0.0<_> then OutsidePoint else InsidePoint
        else
            if arrivingTurn > 0.0<_> then OutsidePoint else InsidePoint

    let private curvatureViolation values clockwise =
        let wrongWay value =
            if clockwise then max 0.0 -value else max 0.0 value

        let violation = values |> List.fold (fun worst value -> max worst (wrongWay value)) 0.0
        if violation = 0.0 then Ok() else Error violation

    let internalSegmentTangentMonotone segment clockwise =
        match segment with
        | Line _ -> Ok()
        | QuadraticBezier(startPoint, control, endPoint) ->
            let arriving = Point.displacement startPoint control
            let leaving = Point.displacement control endPoint
            curvatureViolation [ float (Point.cross arriving leaving) ] clockwise
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let a = Point.displacement startPoint control1
            let b = Point.displacement control1 control2
            let c = Point.displacement control2 endPoint
            let u = a
            let v = Point.scale 2.0 (Point.subtract b a)
            let w = Point.add (Point.subtract a (Point.scale 2.0 b)) c
            let qa = float (Point.cross v w)
            let qb = 2.0 * float (Point.cross u w)
            let qc = float (Point.cross u v)
            let candidates =
                if qa = 0.0 then [ 0.0; 1.0 ]
                else
                    let critical = -qb / (2.0 * qa)
                    if critical > 0.0 && critical < 1.0 then [ 0.0; 1.0; critical ] else [ 0.0; 1.0 ]
            candidates
            |> List.map (fun t -> qa * t * t + qb * t + qc)
            |> fun values -> curvatureViolation values clockwise
        | Arc data ->
            if data.Sweep = clockwise then Ok() else Error 1.0

    let private normalizeTangencyCoefficients coefficients =
        let scale = coefficients |> List.fold (fun largest coefficient -> max largest (abs coefficient)) 0.0
        if scale = 0.0 then coefficients else coefficients |> List.map (fun coefficient -> coefficient / scale)

    let private cubicPointTangentCoefficients startPoint control1 control2 endPoint point =
        let a =
            Point.add
                (Point.add (Point.scale -1.0 startPoint) (Point.scale 3.0 control1))
                (Point.add (Point.scale -3.0 control2) endPoint)
        let b =
            Point.add
                (Point.add (Point.scale 3.0 startPoint) (Point.scale -6.0 control1))
                (Point.scale 3.0 control2)
        let c = Point.scale 3.0 (Point.displacement startPoint control1)
        let s = Point.displacement point startPoint
        [ -float (Point.cross a b)
          -2.0 * float (Point.cross a c)
          3.0 * float (Point.cross s a) - float (Point.cross b c)
          2.0 * float (Point.cross s b)
          float (Point.cross s c) ]
        |> normalizeTangencyCoefficients

    let private tangentWindowIsGeometricallySmall segment lower upper =
        match Segment.betweenInside segment lower upper with
        | Error _ -> false
        | Ok portion ->
            match Segment.boundingBox portion with
            | Error _ -> false
            | Ok bounds -> BoundingBox.diameter bounds <= pointTolerance

    let private refinePolynomialTangentIsolation coefficients segment isolation =
        if isolation.Lower = isolation.Upper then isolation.Estimate
        else
            let lowerValue = Root.evaluatePolynomial coefficients isolation.Lower
            let upperValue = Root.evaluatePolynomial coefficients isolation.Upper
            let sameSign a b = (a < 0.0 && b < 0.0) || (a > 0.0 && b > 0.0)
            if sameSign lowerValue upperValue then isolation.Estimate
            else
                Root.bisectIsolationUntil
                    (Root.evaluatePolynomial coefficients)
                    isolation.Lower
                    isolation.Upper
                    100
                    (tangentWindowIsGeometricallySmall segment)
                |> Result.map _.Estimate
                |> Result.defaultValue isolation.Estimate

    let private cubicPointTangentRoots startPoint control1 control2 endPoint point =
        let coefficients = cubicPointTangentCoefficients startPoint control1 control2 endPoint point
        let segment = CubicBezier(startPoint, control1, control2, endPoint)
        Root.polynomialRootIsolationsWith
            coefficients
            (Parameter.fromFloat 0.0)
            (Parameter.fromFloat 1.0)
            { MaxIterations = 100 }
        |> Result.defaultValue []
        |> List.map (refinePolynomialTangentIsolation coefficients segment)

    let internalCubicPointTangentRoots segment point =
        match segment with
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            cubicPointTangentRoots startPoint control1 control2 endPoint point
        | _ -> []

    let internalRefineChordTangent segment approximate other =
        refineChordTangent segment approximate other

    let private orientationClockwise vertices =
        let signed = polygonSignedArea vertices
        if abs signed <= pointTolerance * pointTolerance then None else Some(signed > 0.0<length^2>)

    let private pointLoopView point atPoint arriving leaving clockwise =
        internalPointLoopView point atPoint arriving leaving clockwise

    let private previousIndex index count = if index <= 0 then count - 1 else index - 1

    let private vertexView vertices point index clockwise =
        let count = List.length vertices
        let previous = List.item (previousIndex index count) vertices
        let current = List.item index vertices
        let next = List.item (nextIndex index count) vertices
        pointLoopView
            point
            current
            (Point.displacement previous current)
            (Point.displacement current next)
            clockwise

    let private turnAgainstOrientation turn scale clockwise =
        let tolerance = 1.0e-9 * scale
        if clockwise then max 0.0 (-tolerance - turn) else max 0.0 (turn - tolerance)

    let private validateChordPolygonConvex vertices clockwise =
        let count = List.length vertices
        [ 0 .. count - 1 ]
        |> List.tryFind (fun index ->
            let previous = List.item (previousIndex index count) vertices
            let current = List.item index vertices
            let next = List.item (nextIndex index count) vertices
            let arriving = Point.displacement previous current
            let leaving = Point.displacement current next
            let turn = float (Point.cross arriving leaving)
            let scale = float (Point.norm arriving * Point.norm leaving)
            turnAgainstOrientation turn scale clockwise <> 0.0)
        |> function
            | None -> Ok()
            | Some index -> Error(TangentSearchNonConvexVertex index)

    let private vertexChain vertices fromIndex toIndex =
        let count = List.length vertices
        let rec loop current accumulated =
            let accumulated = List.item current vertices :: accumulated
            if current = toIndex then List.rev accumulated
            else loop (nextIndex current count) accumulated
        loop fromIndex []

    let private subpathFromVertices vertices =
        vertices
        |> List.pairwise
        |> List.map Line
        |> function
            | [] -> Error TangentSearchDegenerateLoop
            | segments ->
                Subpath.createWith WiggleThenBridge segments
                |> Result.mapError ConstructionPathError

    let private orientationFromTurn turn scale =
        let tolerance = 1.0e-9 * scale
        if turn > tolerance then Some true
        elif turn < -tolerance then Some false
        else None

    let private endpointTangentOrientation segments index =
        let count = List.length segments
        let segment = List.item index segments
        let previous = List.item (previousIndex index count) segments
        match Segment.derivative previous 1.0<parameter>, Segment.derivative segment 0.0<parameter> with
        | Ok arriving, Ok leaving ->
            orientationFromTurn
                (float (Point.cross arriving leaving))
                (float (Point.norm arriving * Point.norm leaving))
        | _ -> None

    let private loopTangentOrientation segments =
        [ 0 .. List.length segments - 1 ]
        |> List.choose (endpointTangentOrientation segments)
        |> List.distinct
        |> function
            | [] -> NoTangentOrientation
            | [ orientation ] -> FoundTangentOrientation orientation
            | _ -> ConflictingTangentOrientation

    let private lineLikeOrientation vertices point =
        match vertices with
        | [ a; b ] ->
            let edge = Point.displacement a b
            let offset = Point.displacement a point
            match orientationFromTurn (float (Point.cross edge offset)) (float (Point.norm edge * Point.norm offset)) with
            | Some orientation -> not orientation
            | None -> true
        | _ -> true

    let private tangentSearchOrientation segments point =
        let vertices = loopVertices segments
        match orientationClockwise vertices with
        | Some orientation -> ExactSearchOrientation orientation
        | None ->
            match loopTangentOrientation segments with
            | FoundTangentOrientation orientation -> ExactSearchOrientation orientation
            | NoTangentOrientation when List.length vertices = 2 -> LineLikeSearchOrientation(lineLikeOrientation vertices point)
            | NoTangentOrientation | ConflictingTangentOrientation -> DegenerateSearchOrientation

    let private loopVertexParam segments point =
        match segments |> List.tryFindIndex (fun segment -> Point.distance (Segment.start segment) point <= pointTolerance) with
        | Some index -> Ok { SegmentIndex = index; T = 0.0<parameter> }
        | None -> Error TangentSearchDegenerateLoop

    let private buildOpenSubpathFromSegments segments =
        segments
        |> List.filter (segmentIsPointLike >> not)
        |> function
            | [] -> Error TangentSearchDegenerateLoop
            | segments -> Subpath.createWith WiggleThenBridge segments |> Result.mapError ConstructionPathError

    let private segmentChainIsOutside segments point clockwise =
        match segments with
        | [] -> false
        | segment :: _ ->
            match Segment.point segment 0.5<parameter>, Segment.derivative segment 0.5<parameter> with
            | Ok q, Ok tangent -> pointLoopView point q tangent tangent clockwise = OutsidePoint
            | _ -> false

    let private loopTangentChainsToSubpaths loop first second point clockwise =
        let firstSegments = loopPieceSegments loop first.Param second.Param
        let secondSegments = loopPieceSegments loop second.Param first.Param
        buildOpenSubpathFromSegments firstSegments
        |> Result.bind (fun firstSubpath ->
            buildOpenSubpathFromSegments secondSegments
            |> Result.map (fun secondSubpath ->
                if segmentChainIsOutside firstSegments point clockwise then firstSubpath, secondSubpath
                else secondSubpath, firstSubpath))

    let private lineLikeLoopTangentSubpaths loop point clockwise =
        match loopVertices loop.Segments with
        | [ a; b ] ->
            loopVertexParam loop.Segments a
            |> Result.bind (fun aParam ->
                loopVertexParam loop.Segments b
                |> Result.bind (fun bParam ->
                    loopTangentChainsToSubpaths loop
                        { Param = aParam; Point = a }
                        { Param = bParam; Point = b }
                        point clockwise))
        | _ -> Error TangentSearchDegenerateLoop

    let private segmentPointTangentRoots segment point =
        match segment with
        | Line _ -> Ok []
        | QuadraticBezier(startPoint, control, endPoint) ->
            let s = Point.displacement point startPoint
            let a = Point.displacement startPoint control
            let b = Point.add (Point.subtract startPoint (Point.scale 2.0 control)) endPoint
            Root.quadratic (Point.cross a b) (Point.cross s b) (Point.cross s a)
            |> List.filter (fun t -> t >= 0.0<parameter> && t <= 1.0<parameter>)
            |> Ok
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Ok(cubicPointTangentRoots startPoint control1 control2 endPoint point)
        | Arc data ->
            Ellipse.endpointToCenter data
            |> Result.mapError (fun _ -> ConstructionPathError DegenerateArc)
            |> Result.bind (fun arc ->
                let translated = Point.displacement arc.Center point
                let cosine = Trig.cosDegrees (-arc.XAxisRotation)
                let sine = Trig.sinDegrees (-arc.XAxisRotation)
                let local = Point.create
                                (cosine * translated.X - sine * translated.Y)
                                (sine * translated.X + cosine * translated.Y)
                let a = local.X * arc.Radius.Y
                let b = -local.Y * arc.Radius.X
                let c = arc.Radius.X * arc.Radius.Y
                let magnitude = sqrt (float (a * a + b * b)) * 1.0<length^2>
                if magnitude <= pointTolerance * 1.0<length> || c > magnitude + pointTolerance * 1.0<length> then Ok []
                else
                    let ratio = max -1.0 (min 1.0 (float (c / magnitude)))
                    match Trig.acosDegrees ratio with
                    | None -> Error(ConstructionPathError DegenerateArc)
                    | Some offset ->
                        let baseAngle = Trig.atan2Degrees (float b) (float a)
                        [ baseAngle - offset; baseAngle + offset ]
                        |> List.collect (fun angle ->
                            [ -1; 0; 1 ]
                            |> List.map (fun turn ->
                                Parameter.fromFloat
                                    (float ((angle + float turn * 360.0<degree> - arc.StartAngle) / arc.DeltaAngle))))
                        |> List.filter (fun t -> t >= -sameT && t <= 1.0<parameter> + sameT)
                        |> List.map (fun t -> max 0.0<parameter> (min 1.0<parameter> t))
                        |> Ok)

    let private uniqueSortedParameters values =
        values
        |> List.sort
        |> List.fold (fun kept value ->
            match kept with
            | previous :: _ when abs (value - previous) <= sameT -> kept
            | _ -> value :: kept) []
        |> List.rev

    let private exactLoopTangentCandidates loop point clockwise =
        let segments = loop.Segments
        [ 0 .. List.length segments - 1 ]
        |> List.fold (fun state index ->
            state
            |> Result.bind (fun candidates ->
                let segment = List.item index segments
                let previous = List.item (previousIndex index (List.length segments)) segments
                match Segment.derivative previous 1.0<parameter>, Segment.derivative segment 0.0<parameter> with
                | Ok arriving, Ok leaving ->
                    let endpoint = Segment.start segment
                    let endpointCandidates =
                        if pointLoopView point endpoint arriving leaving clockwise = TangentPoint then
                            [ { Param = { SegmentIndex = index; T = 0.0<parameter> }; Point = endpoint } ]
                        else []
                    segmentPointTangentRoots segment point
                    |> Result.bind (fun roots ->
                        roots
                        |> List.filter (fun t -> t > sameT && t < 1.0<parameter> - sameT)
                        |> uniqueSortedParameters
                        |> List.fold (fun interiorState t ->
                            interiorState
                            |> Result.bind (fun interior ->
                                Segment.point segment t
                                |> Result.mapError ConstructionPathError
                                |> Result.map (fun q ->
                                    { Param = { SegmentIndex = index; T = t }; Point = q } :: interior))) (Ok [])
                        |> Result.map (fun interior -> candidates @ endpointCandidates @ List.rev interior))
                | Error error, _ | _, Error error -> Error(ConstructionPathError error))) (Ok [])

    let private pointExactLoopTangentSubpaths loop point =
        match tangentSearchOrientation loop.Segments point with
        | DegenerateSearchOrientation -> Error TangentSearchDegenerateLoop
        | LineLikeSearchOrientation clockwise -> lineLikeLoopTangentSubpaths loop point clockwise
        | ExactSearchOrientation clockwise ->
            validateChordPolygonConvex (loopVertices loop.Segments) clockwise
            |> Result.bind (fun () ->
                loop.Segments
                |> List.tryFindIndex (fun segment -> Result.isError (internalSegmentTangentMonotone segment clockwise))
                |> function
                    | Some index -> Error(TangentSearchNonConvexVertex index)
                    | None -> exactLoopTangentCandidates loop point clockwise)
            |> Result.bind (function
                | [ first; second ] -> loopTangentChainsToSubpaths loop first second point clockwise
                | tangents -> Error(TangentSearchExpectedTwoTangencies(List.length tangents)))

    let internalPointExactLoopTangentSubpaths segments point =
        pointExactLoopTangentSubpaths { Segments = segments; Enclosure = loopVertices segments } point

    let private loopPlusPointHull loop point =
        pointExactLoopTangentSubpaths loop point
        |> Result.bind (fun (_, kept) ->
            let startPoint = kept.Start
            let endPoint = kept.Segments |> List.last |> Segment.finish
            let segments = kept.Segments @ [ Line(endPoint, point); Line(point, startPoint) ]
            Subpath.createWith WiggleThenBridge segments
            |> Result.bind (Subpath.setClosedWith WiggleThenBridge true)
            |> Result.mapError ConstructionPathError
            |> Result.map (fun subpath -> { loop with Segments = subpath.Segments }))

    let internalLoopPlusPointHull segments point =
        loopPlusPointHull { Segments = segments; Enclosure = loopVertices segments } point
        |> Result.map _.Segments

    let private unionLoopWithPoint loop point =
        match pointChordPolygonLoopSeparation loop point with
        | None -> Ok loop
        | Some(direction, _) ->
            let pointLoop = { Segments = [ Line(point, point) ]; Enclosure = [ point ] }
            findSeededWorstDirection loop pointLoop direction loopUnionSeedMaxDrift
            |> Result.map (fun (lower, upper) ->
                match loopUnion loop pointLoop [ lower; upper ] |> unionPieceSegments loop pointLoop with
                | [] -> loop
                | segments -> { loop with Segments = segments })

    let private dumbRepairLoopWithPoint loop point =
        match loopPlusPointHull loop point with
        | Ok repaired -> Ok repaired
        | Error(TangentSearchExpectedTwoTangencies _)
        | Error TangentSearchDegenerateLoop -> unionLoopWithPoint loop point
        | Error error -> Error error

    let private repairLoopWithPoints loop points =
        points
        |> List.fold (fun state point ->
            state
            |> Result.bind (fun current ->
                match pointChordPolygonLoopSeparation current point with
                | None -> Ok current
                | Some _ -> dumbRepairLoopWithPoint current point)) (Ok loop)

    let private dumbRepairLoopWithPoints loop points =
        repairLoopWithPoints loop (points @ points)

    let internalLoopPlusPointsHull segments points =
        dumbRepairLoopWithPoints { Segments = segments; Enclosure = loopVertices segments } points
        |> Result.map _.Segments

    let private vertexChainIsOutside vertices point clockwise =
        match vertices with
        | [ a; b ] ->
            let direction = Point.displacement a b
            pointLoopView point (Point.midpoint a b) direction direction clockwise = OutsidePoint
        | a :: b :: c :: _ ->
            pointLoopView point b (Point.displacement a b) (Point.displacement b c) clockwise = OutsidePoint
        | _ -> false

    let private chordPolygonTangentSubpaths segments point =
        let vertices = loopVertices segments
        match orientationClockwise vertices with
        | None -> Error TangentSearchDegenerateLoop
        | Some clockwise ->
            validateChordPolygonConvex vertices clockwise
            |> Result.bind (fun () ->
                let tangents =
                    [ 0 .. List.length vertices - 1 ]
                    |> List.choose (fun index ->
                        if vertexView vertices point index clockwise = TangentPoint then
                            Some { VertexIndex = index; Point = List.item index vertices }
                        else None)
                match tangents with
                | [ first; second ] ->
                    let firstChain = vertexChain vertices first.VertexIndex second.VertexIndex
                    let secondChain = vertexChain vertices second.VertexIndex first.VertexIndex
                    subpathFromVertices firstChain
                    |> Result.bind (fun firstSubpath ->
                        subpathFromVertices secondChain
                        |> Result.map (fun secondSubpath ->
                            if vertexChainIsOutside firstChain point clockwise then firstSubpath, secondSubpath
                            else secondSubpath, firstSubpath))
                | _ -> Error(TangentSearchExpectedTwoTangencies(List.length tangents)))

    let internalPointChordPolygonTangentSubpaths segments point =
        chordPolygonTangentSubpaths segments point

    let private finalRepairLoop current sourceLoops repairMode =
        (match repairMode with
        | "ambitious" ->
            sourceLoops
            |> List.fold (fun state addition ->
                state |> Result.bind (fun repaired -> ambitiousRepairLoopWithLoop repaired addition)) (Ok current)
        | "dumb" ->
            sourceLoops
            |> List.collect loopEndpoints
            |> List.fold (fun distinct point ->
                if List.exists (fun existing -> Point.distance existing point <= pointTolerance) distinct then distinct
                else point :: distinct) []
            |> List.rev
            |> dumbRepairLoopWithPoints current
        | _ -> Ok current)

    let private prefilterLoops loops =
        match loops with
        | [] | [ _ ] -> loops
        | _ ->
            let envelope =
                [ 0 .. 35 ]
                |> List.map (fun index -> Degree.fromFloat (float index * 10.0))
                |> List.map (fun angle -> loops |> List.map (fun loop -> loopSupport loop angle) |> List.maxBy _.Value |> _.Point)
                |> hullVertices
            let filtered =
                loops
                |> List.filter (fun loop ->
                    not (loop.Enclosure |> List.forall (polygonStrictlyContains envelope)))
            if List.isEmpty filtered then loops else filtered

    let private constructSegmentHullInternal segment =
        match segment with
        | Line _ | QuadraticBezier _ | Arc _ -> exactSimpleSegmentHull segment
        | CubicBezier _ when segmentIsPointLike segment -> exactSimpleSegmentHull (Line(Segment.start segment, Segment.finish segment))
        | CubicBezier _ -> sampledCubicHull segment

    let private buildClosedSubpath segments =
        Subpath.createWith WiggleThenBridge segments
        |> Result.bind (Subpath.setClosedWith WiggleThenBridge true)
        |> Result.mapError ConstructionPathError

    let private segmentsHullWithRepairMode segments repairMode =
        segments
        |> List.map constructSegmentHullInternal
        |> List.fold (fun state next ->
            state
            |> Result.bind (fun loops -> next |> Result.map (fun subpath -> subpath.Segments :: loops))) (Ok [])
        |> Result.bind (fun reversedLoops ->
            let enclosure segments =
                match segments |> List.map Segment.boundingBox |> List.choose Result.toOption with
                | [] -> []
                | boxes ->
                    let bounds = boxes |> List.reduce BoundingBox.union
                    [ bounds.Min; Point.create bounds.Max.X bounds.Min.Y; bounds.Max; Point.create bounds.Min.X bounds.Max.Y ]
            let loops = List.rev reversedLoops |> List.map (fun segments -> { Segments = segments; Enclosure = enclosure segments }) |> prefilterLoops
            match loops with
            | [] -> Error LoopUnionCollapsed
            | first :: rest ->
                rest
                |> List.fold (fun state addition ->
                    state
                    |> Result.bind (fun current ->
                        unionLoopSegments current.Segments addition.Segments
                        |> Result.map (fun union -> { Segments = union; Enclosure = enclosure union }))) (Ok first)
                |> Result.bind (fun union -> finalRepairLoop union loops repairMode))
        |> Result.bind (fun loop -> buildClosedSubpath loop.Segments)

    let private publicError = function
        | ConstructionPathError error -> ConvexHullPathError error
        | _ -> ConvexHullConstructionFailed

    let private segmentsHullCore segments =
        segmentsHullWithRepairMode segments "ambitious" |> Result.mapError publicError

    let private constructSegmentHull segment =
        constructSegmentHullInternal segment |> Result.mapError publicError

    /// Compute a curve-preserving representation of a segment's convex hull.
    let segmentHull segment = constructSegmentHull segment

    let subpathHull (subpath: Subpath) =
        let segments =
            if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ]
            else subpath.Segments
        segmentsHullCore segments

    let pathHull (path: Path) =
        match path.Subpaths with
        | [] -> Error(ConvexHullPathError EmptyPath)
        | subpaths ->
            subpaths
            |> List.collect (fun subpath ->
                if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ]
                else subpath.Segments)
            |> segmentsHullCore

    let internalPathHullWithRepairMode (path: Path) repairMode =
        match path.Subpaths with
        | [] -> Error(ConstructionPathError EmptyPath)
        | subpaths ->
            subpaths
            |> List.collect (fun subpath ->
                if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ]
                else subpath.Segments)
            |> fun segments -> segmentsHullWithRepairMode segments repairMode

    /// Compute a point collection's hull through the same curve-preserving
    /// loop-union and repair path used by segment, subpath, and path hulls.
    let pointsHull points =
        points
        |> List.map (fun point -> Line(point, point))
        |> function
            | [] -> Error(ConvexHullPathError EmptyPath)
            | segments -> segmentsHullCore segments

    let private projectionExtrema points direction =
        match points with
        | [] -> None
        | first :: rest ->
            let firstProjection = Point.dot first direction
            rest
            |> List.fold
                (fun (lowerPoint, lower, upperPoint, upper) point ->
                    let projection = Point.dot point direction
                    let nextLowerPoint, nextLower =
                        if projection < lower then point, projection else lowerPoint, lower
                    let nextUpperPoint, nextUpper =
                        if projection > upper then point, projection else upperPoint, upper
                    nextLowerPoint, nextLower, nextUpperPoint, nextUpper)
                (first, firstProjection, first, firstProjection)
            |> Some

    let private extent points direction =
        match projectionExtrema points direction with
        | None ->
            let origin = Point.create 0.0<length> 0.0<length>
            { LowerPoint = origin; UpperPoint = origin; Width = 0.0<length> }
        | Some(lowerPoint, lower, upperPoint, upper) ->
            { LowerPoint = lowerPoint
              UpperPoint = upperPoint
              Width = upper - lower }

    let private extremum
        (direction: Point<1>)
        (support: DirectionalExtent)
        (lowerBound: float<length>)
        (upperBound: float<length>)
        converged =
        { Direction = direction
          LowerPoint = support.LowerPoint
          UpperPoint = support.UpperPoint
          Center = Point.midpoint support.LowerPoint support.UpperPoint
          Width = support.Width
          LowerBound = lowerBound
          UpperBound = upperBound
          Converged = converged }

    let private polygonMinimumWidth points =
        let vertices = hullVertices points
        match vertices with
        | [] | [ _ ] ->
            let support = extent vertices Point.right
            extremum Point.right support 0.0<length> 0.0<length> true
        | _ ->
            let edges = List.pairwise (vertices @ [ List.head vertices ])
            edges
            |> List.choose (fun (a, b) ->
                Point.displacement a b
                |> Point.rotateCounterclockwise
                |> Point.normalize
                |> Option.map (fun direction -> direction, extent vertices direction))
            |> List.minBy (fun (_, support) -> support.Width)
            |> fun (direction, support) -> extremum direction support support.Width support.Width true

    let private stripFromExtremum (extremum: WidthExtremum) =
        { Width = extremum.Width
          Direction = extremum.Direction
          LowerPoint = extremum.LowerPoint
          UpperPoint = extremum.UpperPoint
          LowerSupport = Point.dot extremum.LowerPoint extremum.Direction
          UpperSupport = Point.dot extremum.UpperPoint extremum.Direction }

    let internalConvexPolygonMinimumWidthStrip vertices =
        vertices |> polygonMinimumWidth |> stripFromExtremum

    let private polygonDiameter points =
        let vertices = hullVertices points
        match vertices with
        | [] | [ _ ] ->
            let support = extent vertices Point.right
            extremum Point.right support 0.0<length> 0.0<length> true
        | _ ->
            [ for i in 0 .. List.length vertices - 1 do
                  for j in i + 1 .. List.length vertices - 1 do
                      let a, b = vertices[i], vertices[j]
                      yield a, b, Point.distance a b ]
            |> List.maxBy (fun (_, _, distance) -> distance)
            |> fun (a, b, distance) ->
                let direction =
                    Point.displacement a b |> Point.normalize |> Option.defaultValue Point.right
                let support: DirectionalExtent = { LowerPoint = a; UpperPoint = b; Width = distance }
                extremum direction support distance distance true

    let private segmentSupport segment direction =
        (supportSample segment direction).Point

    let private segmentsExtent segments direction =
        match segments with
        | [] -> extent [] direction
        | _ ->
            let upper = segments |> List.map (fun segment -> segmentSupport segment direction) |> List.maxBy (fun point -> Point.dot point direction)
            let opposite = Point.negate direction
            let lower = segments |> List.map (fun segment -> segmentSupport segment opposite) |> List.maxBy (fun point -> Point.dot point opposite)
            { LowerPoint = lower
              UpperPoint = upper
              Width = Point.dot upper direction - Point.dot lower direction }

    let private intervalsFromSamples (samples: WidthSample list) =
        List.pairwise samples |> List.map (fun (fromSample, toSample) -> { From = fromSample; ``To`` = toSample })

    let private directionDistance (interval: WidthInterval) =
        let halfAperture = (interval.``To``.Angle - interval.From.Angle) / 2.0
        2.0 * Trig.sinDegrees (halfAperture / 2.0)

    let private intervalLowerBound (diameter: float<length>) (interval: WidthInterval) =
        (min interval.From.Support.Width interval.``To``.Support.Width)
        - (diameter * directionDistance interval)

    let private intervalUpperBound (diameter: float<length>) (interval: WidthInterval) =
        (max interval.From.Support.Width interval.``To``.Support.Width)
        + (diameter * directionDistance interval)

    let private subdivideInterval support (interval: WidthInterval) =
        let step = (interval.``To``.Angle - interval.From.Angle) / 5.0
        let interior =
            [ 1 .. 4 ]
            |> List.map (fun index ->
                let angle = interval.From.Angle + float index * step
                { Angle = angle; Support = support (Point.direction angle) })
        let samples = interval.From :: (interior @ [ interval.``To`` ])
        intervalsFromSamples samples, interior

    let private subdivideIntervals support intervals =
        intervals
        |> List.fold
            (fun (allIntervals, allSamples) interval ->
                let divided, samples = subdivideInterval support interval
                divided @ allIntervals, samples @ allSamples)
            ([], [])

    let private inventoryLowerBound samples =
        samples
        |> List.collect (fun sample -> [ sample.Support.LowerPoint; sample.Support.UpperPoint ])
        |> polygonMinimumWidth
        |> _.Width

    let private tryMinimum values =
        match values with [] -> None | first :: rest -> Some(List.fold min first rest)

    let private tryMaximum values =
        match values with [] -> None | first :: rest -> Some(List.fold max first rest)

    let private widthLowerBoundRoundoff (diameter: float<length>) =
        max 1.0<length> (abs diameter) * widthLowerBoundRoundoffFactor

    let private adaptiveMinimum support diameter accuracy maxDepth initialSamples =
        let lowerBoundRoundoff = widthLowerBoundRoundoff diameter
        let rec search samples intervals depth discardedLowerBound =
            let best = List.minBy (fun sample -> sample.Support.Width) samples
            let bounds = intervals |> List.map (fun interval -> interval, intervalLowerBound diameter interval)
            let intervalBound =
                bounds
                |> List.map snd
                |> tryMinimum
                |> function
                    | None -> discardedLowerBound
                    | Some value -> Some(match discardedLowerBound with None -> value | Some old -> min old value)
            let rawLowerBound =
                intervalBound
                |> Option.defaultValue best.Support.Width
                |> max (inventoryLowerBound samples)
                |> max 0.0<length>
            let lowerBound = max 0.0<length> (rawLowerBound - lowerBoundRoundoff)
            let converged = best.Support.Width - lowerBound <= accuracy
            if converged || depth >= maxDepth then
                extremum (Point.direction best.Angle) best.Support lowerBound best.Support.Width converged
            else
                let active, discarded =
                    bounds |> List.partition (fun (_, bound) -> bound < best.Support.Width - accuracy)
                match active with
                | [] ->
                    extremum (Point.direction best.Angle) best.Support
                        (max 0.0<length> (best.Support.Width - accuracy)) best.Support.Width true
                | _ ->
                    let newlyDiscarded = discarded |> List.map snd |> tryMinimum
                    let retainedBound =
                        match discardedLowerBound, newlyDiscarded with
                        | None, other | other, None -> other
                        | Some left, Some right -> Some(min left right)
                    let divided, added = active |> List.map fst |> subdivideIntervals support
                    search (samples @ added) divided (depth + 1) retainedBound
        search initialSamples (intervalsFromSamples initialSamples) 0 None

    let private minimumWidthDecision support diameter tolerance maxDepth =
        let initialSamples =
            [ 0.0; 36.0; 72.0; 108.0; 144.0; 180.0 ]
            |> List.map (fun value ->
                let angle = Degree.fromFloat value
                { Angle = angle; Support = support (Point.direction angle) })
        let rec search samples intervals depth =
            let best = List.minBy (fun sample -> sample.Support.Width) samples
            if best.Support.Width <= tolerance then
                let direction = Point.direction best.Angle
                MinimumWidthFits
                    { Width = best.Support.Width
                      Direction = direction
                      LowerPoint = best.Support.LowerPoint
                      UpperPoint = best.Support.UpperPoint
                      LowerSupport = Point.dot best.Support.LowerPoint direction
                      UpperSupport = Point.dot best.Support.UpperPoint direction }
            else
                let inventoryBound = inventoryLowerBound samples
                if inventoryBound > tolerance then MinimumWidthExceeds inventoryBound
                else
                    let intervalBound =
                        intervals
                        |> List.map (intervalLowerBound diameter)
                        |> tryMinimum
                        |> Option.defaultValue inventoryBound
                    let active =
                        intervals
                        |> List.filter (fun interval -> intervalLowerBound diameter interval <= tolerance)
                    let certifiedBound = max inventoryBound intervalBound
                    match active with
                    | [] -> MinimumWidthExceeds certifiedBound
                    | _ when depth >= maxDepth -> MinimumWidthUnresolved(certifiedBound, best.Support.Width)
                    | _ ->
                        let divided, added = subdivideIntervals support active
                        search (samples @ added) divided (depth + 1)
        search initialSamples (intervalsFromSamples initialSamples) 0

    let internalConvexPolygonMinimumWidthDecision vertices tolerance maxDepth =
        minimumWidthDecision (extent vertices) (polygonDiameter vertices).Width tolerance maxDepth

    let private adaptiveMaximum support diameter accuracy maxDepth initialSamples =
        let rec search samples intervals depth discardedUpperBound =
            let best = List.maxBy (fun sample -> sample.Support.Width) samples
            let bounds = intervals |> List.map (fun interval -> interval, intervalUpperBound diameter interval)
            let upperBound =
                bounds
                |> List.map snd
                |> tryMaximum
                |> function
                    | None -> discardedUpperBound |> Option.defaultValue best.Support.Width
                    | Some value -> max value (discardedUpperBound |> Option.defaultValue value)
                |> max best.Support.Width
            let converged = upperBound - best.Support.Width <= accuracy
            if converged || depth >= maxDepth then
                extremum (Point.direction best.Angle) best.Support best.Support.Width upperBound converged
            else
                let active, discarded =
                    bounds |> List.partition (fun (_, bound) -> bound > best.Support.Width + accuracy)
                match active with
                | [] ->
                    extremum (Point.direction best.Angle) best.Support best.Support.Width
                        (best.Support.Width + accuracy) true
                | _ ->
                    let newlyDiscarded = discarded |> List.map snd |> tryMaximum
                    let retainedBound =
                        match discardedUpperBound, newlyDiscarded with
                        | None, other | other, None -> other
                        | Some left, Some right -> Some(max left right)
                    let divided, added = active |> List.map fst |> subdivideIntervals support
                    search (samples @ added) divided (depth + 1) retainedBound
        search initialSamples (intervalsFromSamples initialSamples) 0 None

    let private adaptiveDirectionalExtremum
        findMinimum
        (support: Point<1> -> DirectionalExtent)
        (diameterUpperBound: float<length>)
        (options: WidthSearchOptions) =
        let samples =
            [ 0.0; 36.0; 72.0; 108.0; 144.0; 180.0 ]
            |> List.map (fun value ->
                let angle = Degree.fromFloat value
                { Angle = angle; Support = support (Point.direction angle) })
        let accuracy =
            if System.Double.IsNaN(float options.Accuracy) then 0.0<length>
            else max 0.0<length> options.Accuracy
        let maxDepth = max 0 options.MaxDepth
        if findMinimum then adaptiveMinimum support (abs diameterUpperBound) accuracy maxDepth samples
        else adaptiveMaximum support (abs diameterUpperBound) accuracy maxDepth samples

    let minimumWidthWith support diameterUpperBound options =
        adaptiveDirectionalExtremum true support diameterUpperBound options

    let minimumWidth support diameterUpperBound =
        minimumWidthWith support diameterUpperBound defaultWidthSearchOptions

    let diameterWith support diameterUpperBound options =
        adaptiveDirectionalExtremum false support diameterUpperBound options

    let diameter support diameterUpperBound =
        diameterWith support diameterUpperBound defaultWidthSearchOptions

    let private sourceExtremum findMinimum segments options =
        match segments with
        | [] -> Error(ConvexHullPathError EmptySubpath)
        | _ ->
            segments
            |> List.map Segment.boundingBox
            |> List.fold
                (fun accumulated next ->
                    accumulated
                    |> Result.bind (fun boxes -> next |> Result.map (fun box -> box :: boxes)))
                (Ok [])
            |> Result.mapError ConvexHullPathError
            |> Result.map (fun boxes ->
                let box = boxes |> List.reduce BoundingBox.union
                let bound = BoundingBox.diameter box
                adaptiveDirectionalExtremum findMinimum (segmentsExtent segments) bound options)

    let private lineOnlyExtremum findMinimum segments =
        let points =
            segments
            |> List.fold
                (fun points segment ->
                    match segment with
                    | Line(startPoint, endPoint) -> startPoint :: endPoint :: points
                    | _ -> points)
                []
        if findMinimum then polygonMinimumWidth points else polygonDiameter points

    let private sourceOrPolygonExtremum findMinimum segments options =
        sourceExtremum findMinimum segments options

    let segmentMinimumWidthWith segment options =
        sourceOrPolygonExtremum true [ segment ] options

    let segmentMinimumWidth segment = segmentMinimumWidthWith segment defaultWidthSearchOptions

    let subpathMinimumWidthWith (subpath: Subpath) options =
        let segments = if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ] else subpath.Segments
        sourceOrPolygonExtremum true segments options

    let subpathMinimumWidth subpath = subpathMinimumWidthWith subpath defaultWidthSearchOptions

    let pathMinimumWidthWith (path: Path) options =
        path.Subpaths
        |> List.collect (fun subpath ->
            if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ] else subpath.Segments)
        |> fun segments ->
            if List.isEmpty path.Subpaths then Error(ConvexHullPathError EmptyPath)
            else sourceOrPolygonExtremum true segments options

    let pathMinimumWidth path = pathMinimumWidthWith path defaultWidthSearchOptions

    let internalConvexSubpathMinimumWidthDecision (hull: Subpath) tolerance =
        let segments = hull.Segments
        if segments |> List.forall (function Line _ -> true | _ -> false) then
            let strip = segments |> lineOnlyExtremum true |> stripFromExtremum
            Ok(if strip.Width <= tolerance then MinimumWidthFits strip else MinimumWidthExceeds strip.Width)
        else
            Subpath.boundingBox hull
            |> Result.mapError ConvexHullPathError
            |> Result.map (fun bounds ->
                minimumWidthDecision (segmentsExtent segments) (BoundingBox.diameter bounds) tolerance 20)

    let internalConvexSubpathAddSegmentAndTestWidth (hull: Subpath) segment tolerance =
        constructSegmentHullInternal segment
        |> Result.bind (fun addition -> unionLoopSegments hull.Segments addition.Segments)
        |> Result.bind buildClosedSubpath
        |> Result.mapError publicError
        |> Result.bind (fun combinedHull ->
            internalConvexSubpathMinimumWidthDecision combinedHull tolerance
            |> Result.map (fun decision -> combinedHull, decision))

    let segmentDiameterWith segment options = sourceOrPolygonExtremum false [ segment ] options

    let segmentDiameter segment = segmentDiameterWith segment defaultWidthSearchOptions

    let subpathDiameterWith (subpath: Subpath) options =
        let segments = if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ] else subpath.Segments
        sourceOrPolygonExtremum false segments options

    let subpathDiameter subpath = subpathDiameterWith subpath defaultWidthSearchOptions

    let pathDiameterWith (path: Path) options =
        path.Subpaths
        |> List.collect (fun subpath ->
            if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ] else subpath.Segments)
        |> fun segments ->
            if List.isEmpty path.Subpaths then Error(ConvexHullPathError EmptyPath)
            else sourceOrPolygonExtremum false segments options

    let pathDiameter path = pathDiameterWith path defaultWidthSearchOptions
