namespace SvgPath

type ConvexHullError =
    | ConvexHullPathError of SegmentError
    | ConvexHullConstructionFailed

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

    type private SourceSupportSample =
        { SegmentIndex: int
          Sample: SupportSample }

    type private EnvelopeSample =
        { Angle: float<degree>
          Winner: SourceSupportSample }

    type private EnvelopeBoundary =
        { FromIndex: int
          FromT: float<parameter>
          ToIndex: int
          ToT: float<parameter> }

    type private WidthSample =
        { Angle: float<degree>
          Support: DirectionalExtent }

    type private WidthInterval =
        { From: WidthSample
          ``To``: WidthSample }

    let defaultWidthSearchOptions =
        { Accuracy = 1.0e-9<length>
          MaxDepth = 20 }

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

    let private closedHull vertices =
        match vertices with
        | [] -> Error(ConvexHullPathError EmptyPath)
        | [ point ] -> Subpath.empty point |> Subpath.setClosed true |> Result.mapError ConvexHullPathError
        | [ a; b ] ->
            Subpath.create [ Line(a, b); Line(b, a) ]
            |> Result.bind (Subpath.setClosed true)
            |> Result.mapError ConvexHullPathError
        | points ->
            Subpath.polygon points |> Result.mapError ConvexHullPathError

    /// Compute the exact polygonal hull of a point collection.
    let pointsHull points = points |> hullVertices |> closedHull

    let private linearizedPoints segment =
        Segment.toLinesWith
            { Tolerance = 1.0e-7<length>
              MaxDepth = 24 }
            segment
        |> Result.map (fun lines ->
            match lines with
            | [] -> [ Segment.start segment ]
            | _ -> Segment.start segment :: (lines |> List.map Segment.finish))
        |> Result.mapError ConvexHullPathError

    let private exactSimpleSegmentHull segment =
        let startPoint = Segment.start segment
        let endPoint = Segment.finish segment
        let pieces =
            if startPoint = endPoint then [ Line(startPoint, startPoint); Line(startPoint, startPoint) ]
            else
                match segment with
                | Line _ -> [ segment; Segment.reverse segment ]
                | QuadraticBezier _ | Arc _ -> [ segment; Line(endPoint, startPoint) ]
                | CubicBezier _ -> []
        match pieces with
        | [] -> Error ConvexHullConstructionFailed
        | _ ->
            Subpath.createWith WiggleThenBridge pieces
            |> Result.bind (Subpath.setClosedWith WiggleThenBridge true)
            |> Result.mapError ConvexHullPathError

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
        let rec loop current reversedRuns remaining =
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
                    let options: PolynomialOptions<1> =
                        { CoefficientTolerance = 1.0e-12
                          ParameterTolerance = 1.0e-12<parameter>
                          ValueTolerance = 1.0e-12
                          MaxIterations = 100 }
                    match Root.polynomialRootIsolationsWith coefficients
                            (max 0.0<parameter> (approximate - 0.08<parameter>))
                            (min 1.0<parameter> (approximate + 0.08<parameter>)) options with
                    | Error _ -> approximate
                    | Ok isolations ->
                        isolations
                        |> List.filter (fun isolation -> abs (isolation.Estimate - other) > 1.0e-6<parameter>)
                        |> List.sortBy (fun isolation -> abs (isolation.Estimate - approximate))
                        |> List.tryHead
                        |> Option.map _.Estimate
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
        |> Result.mapError ConvexHullPathError

    let private sourceSupportSample (segments: Segment list) direction : SourceSupportSample =
        segments
        |> List.mapi (fun index segment ->
            { SegmentIndex = index
              Sample = supportSample segment direction })
        |> List.maxBy (fun sample -> sample.Sample.Value)

    let private envelopeSample (segments: Segment list) (angle: float<degree>) : EnvelopeSample =
        { Angle = angle
          Winner = sourceSupportSample segments (Point.direction angle) }

    let private normalizeAngle (angle: float<degree>) =
        let value = Degree.toFloat angle
        Degree.fromFloat (value - floor (value / 360.0) * 360.0)

    let private refineEnvelopeBoundary
        (segments: Segment list)
        (left: EnvelopeSample)
        (right: EnvelopeSample) =
        let departing = left.Winner.SegmentIndex
        let rec bisect (left: EnvelopeSample) (right: EnvelopeSample) remaining =
            let difference (sample: EnvelopeSample) =
                let direction = Point.direction (normalizeAngle sample.Angle)
                let departingValue = (supportSample segments[departing] direction).Value
                let arrivingValue = (supportSample segments[right.Winner.SegmentIndex] direction).Value
                departingValue - arrivingValue
            if remaining = 0 || abs (difference left) <= 1.0e-7<length> then left
            else
                let middle = envelopeSample segments ((left.Angle + right.Angle) / 2.0)
                if middle.Winner.SegmentIndex = departing then bisect middle right (remaining - 1)
                else bisect left middle (remaining - 1)
        let refined = bisect left right 32
        let direction = Point.direction (normalizeAngle refined.Angle)
        let arriving = right.Winner.SegmentIndex
        { FromIndex = departing
          FromT = (supportSample segments[departing] direction).T
          ToIndex = arriving
          ToT = (supportSample segments[arriving] direction).T }

    let private envelopeBoundaries (segments: Segment list) (samples: EnvelopeSample list) =
        let first = List.head samples
        List.pairwise (samples @ [ { first with Angle = first.Angle + 360.0<degree> } ])
        |> List.choose (fun (left, right) ->
            if left.Winner.SegmentIndex = right.Winner.SegmentIndex then None
            else Some(refineEnvelopeBoundary segments left right))

    let private sourceEnvelopeHull (segments: Segment list) =
        let samples: EnvelopeSample list =
            [ 0 .. 3599 ]
            |> List.map (fun index -> envelopeSample segments (Degree.fromFloat (float index / 10.0)))
        let boundaries = envelopeBoundaries segments samples
        match boundaries with
        | [] ->
            let winner = segments[(List.head samples).Winner.SegmentIndex]
            match winner with
            | Line _ | QuadraticBezier _ | Arc _ -> exactSimpleSegmentHull winner
            | CubicBezier _ -> sampledCubicHull winner
        | _ ->
            List.pairwise (boundaries @ [ List.head boundaries ])
            |> List.map (fun (startBoundary, endBoundary) ->
                let index = startBoundary.ToIndex
                Segment.betweenInside segments[index] startBoundary.ToT endBoundary.FromT
                |> Result.bind (fun curve ->
                    Segment.point segments[endBoundary.FromIndex] endBoundary.FromT
                    |> Result.bind (fun chordStart ->
                        Segment.point segments[endBoundary.ToIndex] endBoundary.ToT
                        |> Result.map (fun chordEnd -> [ curve; Line(chordStart, chordEnd) ]))))
            |> List.fold
                (fun accumulated next ->
                    accumulated
                    |> Result.bind (fun pieces -> next |> Result.map (fun more -> pieces @ more)))
                (Ok [])
            |> Result.bind (fun pieces ->
                Subpath.createWith WiggleThenBridge pieces
                |> Result.bind (Subpath.setClosedWith WiggleThenBridge true))
            |> Result.mapError ConvexHullPathError

    /// Compute a curve-preserving representation of a segment's convex hull.
    let segmentHull segment =
        match segment with
        | Line _ | QuadraticBezier _ | Arc _ -> exactSimpleSegmentHull segment
        | CubicBezier _ -> sampledCubicHull segment

    let private subpathPoints (subpath: Subpath) =
        match subpath.Segments with
        | [] -> Ok [ subpath.Start ]
        | segments ->
            segments
            |> List.map linearizedPoints
            |> List.fold
                (fun accumulated next ->
                    accumulated
                    |> Result.bind (fun points -> next |> Result.map (fun more -> points @ more)))
                (Ok [])

    let subpathHull (subpath: Subpath) =
        let segments =
            if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ]
            else subpath.Segments
        sourceEnvelopeHull segments

    let pathHull (path: Path) =
        match path.Subpaths with
        | [] -> Error(ConvexHullPathError EmptyPath)
        | subpaths ->
            subpaths
            |> List.collect (fun subpath ->
                if List.isEmpty subpath.Segments then [ Line(subpath.Start, subpath.Start) ]
                else subpath.Segments)
            |> sourceEnvelopeHull

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

    let private subdivideInterval support interval =
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

    let private adaptiveMinimum support diameter accuracy maxDepth initialSamples =
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
            let lowerBound =
                intervalBound
                |> Option.defaultValue best.Support.Width
                |> max (inventoryLowerBound samples)
                |> max 0.0<length>
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

    let private hullPointsOfSubpath subpath = subpathPoints subpath |> Result.map hullVertices

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
        if segments |> List.forall (function Line _ -> true | _ -> false) then
            Ok(lineOnlyExtremum findMinimum segments)
        else sourceExtremum findMinimum segments options

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
