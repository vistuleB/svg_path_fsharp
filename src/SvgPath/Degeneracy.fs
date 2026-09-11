namespace SvgPath

/// Detection and normalization of geometrically degenerate path segments.
[<RequireQualifiedAccess>]
module Degeneracy =

    type Error =
        /// The tolerance must be finite and non-negative.
        | DegeneracyInvalidTolerance of tolerance: float<length>
        | DegeneracyPathError of error: SegmentError
        | DegeneracyConvexHullError of error: ConvexHull.Error

    /// Replace a line-degenerate segment by its ordered line traversal.
    /// The tolerance must be finite and non-negative; a tolerance of 0.0 collapses
    /// the segment only when it lies exactly on a line strip of width zero.
    let segmentLinearizeIfDegenerate segment tolerance =
        let finiteNonNegative = tolerance >= 0.0<length> && System.Double.IsFinite(float tolerance)
        let farthest points origin =
            points |> List.fold (fun best point -> if Point.squaredDistance point origin > Point.squaredDistance best origin then point else best) origin
        let axisFor points =
            match points with
            | [] -> None
            | origin :: _ ->
                let farthestPoint = farthest points origin
                if Point.squaredDistance farthestPoint origin <= tolerance * tolerance then None
                else Some(origin, Point.displacement origin farthestPoint)
        let inStrip points origin axis =
            let axisLength = Point.norm axis
            points |> List.forall (fun point -> abs (Point.cross (Point.displacement origin point) axis) / axisLength <= tolerance)
        let coordinate point origin axis =
            let denominator = Point.squaredNorm axis
            if denominator = 0.0<length^2> then 0.0
            else float (Point.dot (Point.displacement origin point) axis / denominator)
        let pieces at =
            at
            |> List.pairwise
            |> List.fold (fun state (fromParameter, toParameter) ->
                state
                |> Result.bind (fun lines ->
                    Segment.point segment (Parameter.fromFloat fromParameter) |> Result.mapError DegeneracyPathError
                    |> Result.bind (fun startPoint ->
                        Segment.point segment (Parameter.fromFloat toParameter) |> Result.mapError DegeneracyPathError
                        |> Result.map (fun endPoint -> Line(startPoint, endPoint) :: lines)))) (Ok [])
            |> Result.map (List.rev >> List.filter (fun line -> Segment.start line <> Segment.finish line))
        let bezierResult definingPoints breaks =
            match axisFor definingPoints with
            | Some(origin, axis) when inStrip definingPoints origin axis -> pieces (0.0 :: breaks @ [ 1.0 ]) |> Result.map Some
            | None -> Ok None
            | _ -> Ok None
        if not finiteNonNegative then Error(DegeneracyInvalidTolerance tolerance)
        else
            match segment with
            | Line _ -> Ok None
            | QuadraticBezier(startPoint, control, endPoint) ->
                let points = [ startPoint; control; endPoint ]
                let origin = startPoint
                let axis = Point.displacement origin (farthest points origin)
                let s, c, e = coordinate startPoint origin axis, coordinate control origin axis, coordinate endPoint origin axis
                let denominator = s - 2.0 * c + e
                let breaks = if InternalNumber.isZero denominator then [] else [ (s - c) / denominator ] |> List.filter (fun t -> t > 0.0 && t < 1.0)
                bezierResult points breaks
            | CubicBezier(startPoint, control1, control2, endPoint) ->
                let points = [ startPoint; control1; control2; endPoint ]
                let origin = startPoint
                let axis = Point.displacement origin (farthest points origin)
                let s = coordinate startPoint origin axis
                let c1 = coordinate control1 origin axis
                let c2 = coordinate control2 origin axis
                let e = coordinate endPoint origin axis
                let a = -s + 3.0*c1 - 3.0*c2 + e
                let b = 3.0*s - 6.0*c1 + 3.0*c2
                let c = 3.0*c1 - 3.0*s
                let breaks = Root.strictlyInside (Root.parameterQuadratic (3.0*a) (2.0*b) c) 0.0<parameter> 1.0<parameter> |> List.map Parameter.ratio
                bezierResult points breaks
            | Arc endpoint when InternalNumber.isZero endpoint.Radius.X || InternalNumber.isZero endpoint.Radius.Y ->
                if endpoint.Start = endpoint.End then Ok(Some []) else Ok(Some [ Line(endpoint.Start, endpoint.End) ])
            | Arc _ ->
                Segment.toLinesWith { Tolerance = tolerance; MaxDepth = Segment.defaultLinearizeOptions.MaxDepth } segment
                |> Result.mapError DegeneracyPathError
                |> Result.map (fun lines ->
                    let points = lines |> List.collect (fun line -> [ Segment.start line; Segment.finish line ])
                    match axisFor points with
                    | None -> Some []
                    | Some(origin, axis) when inStrip points origin axis -> Some(List.filter (fun line -> Segment.start line <> Segment.finish line) lines)
                    | _ -> None)

    /// Replace line-degenerate segments in a subpath with an ordered line traversal.
    /// The tolerance must be finite and non-negative; a tolerance of 0.0 collapses
    /// only exactly zero-width strips.
    let subpathLinearizeIfDegenerate subpath tolerance =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(DegeneracyInvalidTolerance tolerance)
        else
            Subpath.segments subpath
            |> List.fold (fun state segment ->
                state
                |> Result.bind (fun accumulated ->
                    match accumulated with
                    | None -> Ok None
                    | Some accumulated ->
                        segmentLinearizeIfDegenerate segment tolerance
                        |> Result.map (fun replacement ->
                            match segment, replacement with
                            | Line _, None -> Some(accumulated @ [ segment ])
                            | _, Some lines -> Some(accumulated @ lines)
                            | _ -> None))) (Ok(Some []))
            |> Result.map (fun replacements ->
                match replacements with
                | None -> None
                | Some lines ->
                    let points = lines |> List.collect (fun line -> [ Segment.start line; Segment.finish line ])
                    match points with
                    | [] -> Some []
                    | origin :: _ ->
                        let farthest = points |> List.maxBy (fun point -> Point.squaredDistance point origin)
                        let axis = Point.displacement origin farthest
                        let axisLength = Point.norm axis
                        if axisLength = 0.0<length> then Some []
                        elif points |> List.forall (fun point -> abs (Point.cross (Point.displacement origin point) axis) / axisLength <= tolerance) then
                            Some(List.filter (fun line -> Segment.start line <> Segment.finish line) lines)
                        else None)

    [<Struct>]
    type internal ThinPrefix =
        { Segments: Segment list
          Remaining: Segment list
          Hull: Subpath option
          Strip: ConvexHull.MinimumWidthStrip option }

    let private traversalLines tolerance points =
        points
        |> List.pairwise
        |> List.choose (fun (startPoint, endPoint) ->
            if Point.distance startPoint endPoint <= tolerance then None
            else Some(Line(startPoint, endPoint)))

    let private makeSubpath segments =
        Subpath.createWith Strict segments
        |> Result.mapError DegeneracyPathError

    let private widthDecision hull tolerance =
        ConvexHull.internalConvexSubpathMinimumWidthDecision hull tolerance
        |> Result.mapError DegeneracyConvexHullError

    let private sourceWidthDecision segments hull tolerance =
        match ConvexHull.internalSourceStripCandidate segments with
        | Ok(Some strip) when strip.Width <= tolerance -> Ok(ConvexHull.MinimumWidthFits strip)
        | _ -> widthDecision hull tolerance

    let rec private longestThinPrefixLoop tolerance accepted hull strip remaining =
        match remaining with
        | [] ->
            Ok { Segments = List.rev accepted
                 Remaining = []
                 Hull = Some hull
                 Strip = Some strip }
        | first :: rest ->
            ConvexHull.internalConvexSubpathAddSegmentAndTestWidth hull first tolerance
            |> Result.mapError DegeneracyConvexHullError
            |> Result.bind (fun (candidateHull, decision) ->
                let decision =
                    match ConvexHull.internalSourceStripCandidate (first :: accepted) with
                    | Ok(Some strip) when strip.Width <= tolerance -> ConvexHull.MinimumWidthFits strip
                    | _ -> decision
                match decision with
                | ConvexHull.MinimumWidthFits candidateStrip ->
                    longestThinPrefixLoop tolerance (first :: accepted) candidateHull candidateStrip rest
                | ConvexHull.MinimumWidthExceeds _
                | ConvexHull.MinimumWidthUnresolved _ ->
                    makeSubpath (List.rev (first :: accepted))
                    |> Result.bind (fun candidate ->
                        ConvexHull.subpath candidate
                        |> Result.mapError DegeneracyConvexHullError
                        |> Result.bind (fun rebuiltHull ->
                            sourceWidthDecision (first :: accepted) rebuiltHull tolerance
                            |> Result.bind (function
                                | ConvexHull.MinimumWidthFits rebuiltStrip ->
                                    longestThinPrefixLoop tolerance (first :: accepted) rebuiltHull rebuiltStrip rest
                                | ConvexHull.MinimumWidthExceeds _
                                | ConvexHull.MinimumWidthUnresolved _ ->
                                    Ok { Segments = List.rev accepted
                                         Remaining = first :: rest
                                         Hull = Some hull
                                         Strip = Some strip }))))

    let internal internalLongestThinPrefix (subpath: Subpath) tolerance =
        match subpath.Segments with
        | [] -> Ok { Segments = []; Remaining = []; Hull = None; Strip = None }
        | first :: rest ->
            ConvexHull.segment first
            |> Result.mapError DegeneracyConvexHullError
            |> Result.bind (fun hull ->
                sourceWidthDecision [first] hull tolerance
                |> Result.bind (function
                    | ConvexHull.MinimumWidthFits strip -> longestThinPrefixLoop tolerance [ first ] hull strip rest
                    | ConvexHull.MinimumWidthExceeds _
                    | ConvexHull.MinimumWidthUnresolved _ ->
                        Ok { Segments = []; Remaining = first :: rest; Hull = None; Strip = None }))

    let private pointAlreadyPresent point points tolerance =
        points |> List.exists (fun candidate -> Point.distance point candidate <= tolerance)

    let private uniquePoints tolerance points =
        points
        |> List.fold (fun unique point ->
            if pointAlreadyPresent point unique tolerance then unique else point :: unique) []
        |> List.rev

    // Query source segments and retain occurrence parameters; ties keep the first.
    let private traversalSupport segments angle =
        let rec loop index best remaining =
            match remaining with
            | [] -> Ok best
            | first :: rest ->
                ConvexHull.internalSegmentSupport first angle
                |> Result.bind (fun (t, supportPoint, value) ->
                    let (_, _, _, bestValue) = best
                    let next = if value > bestValue then (index, t, supportPoint, value) else best
                    loop (index + 1) next rest)
        let first = List.head segments
        ConvexHull.internalSegmentSupport first angle
        |> Result.bind (fun (t, supportPoint, value) ->
            loop 1 (0, t, supportPoint, value) (List.tail segments))

    let private stripPointsInTraversalOrder segments (strip: ConvexHull.MinimumWidthStrip) tolerance =
        let angle = strip.Normal |> Point.rotate90Clockwise |> Point.heading
        traversalSupport segments (angle + 180.0<degree>)
        |> Result.bind (fun (minIndex, minT, minPoint, _) ->
            traversalSupport segments angle
            |> Result.map (fun (maxIndex, maxT, maxPoint, _) ->
            let first = List.head segments
            let startPoint = Segment.start first
            let endPoint = segments |> List.last |> Segment.finish
            let ordered =
                if minIndex < maxIndex || (minIndex = maxIndex && minT <= maxT) then [ minPoint; maxPoint ]
                else [ maxPoint; minPoint ]
            // Both endpoint anchors take priority, even when they coincide.
            let protrusions =
                ordered
                |> List.filter (fun candidate ->
                    Point.distance candidate startPoint > tolerance && Point.distance candidate endPoint > tolerance)
                |> uniquePoints tolerance
            startPoint :: (protrusions @ [ endPoint ])))

    let rec private degenerateTraversal tolerance segments =
        match segments with
        | [] -> Ok []
        | first :: rest ->
            segmentLinearizeIfDegenerate first tolerance
            |> Result.bind (fun replacement ->
                degenerateTraversal tolerance rest
                |> Result.map (fun remaining -> (Option.defaultValue [ first ] replacement) @ remaining))

    let private degenerateWindowTraversal prefix tolerance =
        match prefix.Strip with
        | None -> degenerateTraversal tolerance prefix.Segments
        | Some strip ->
            match stripPointsInTraversalOrder prefix.Segments strip tolerance with
            | Error _ -> degenerateTraversal tolerance prefix.Segments
            // Candidate extrema are already deduplicated against both anchors.
            | Ok points -> Ok(traversalLines 0.0<length> points)

    let rec private normalizeSegments tolerance segments converted =
        match segments with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            makeSubpath segments
            |> Result.bind (fun pending -> internalLongestThinPrefix pending tolerance)
            |> Result.bind (fun prefix ->
                match prefix.Segments with
                | _ :: _ :: _ ->
                    degenerateWindowTraversal prefix tolerance
                    |> Result.bind (fun lines ->
                        normalizeSegments tolerance prefix.Remaining (List.rev lines @ converted))
                | _ ->
                    segmentLinearizeIfDegenerate first tolerance
                    |> Result.bind (fun replacement ->
                        normalizeSegments tolerance rest
                            (List.rev (Option.defaultValue [ first ] replacement) @ converted)))

    /// Replace maximal contiguous line-degenerate windows with ordered line traversals.
    /// Preserve endpoints and both longitudinal support extrema in source order;
    /// intermediate local reversals need not be retained.
    /// A 0.0 tolerance collapses a window only when its strip width is exactly zero.
    let normalizeDegenerateSegments (subpath: Subpath) (tolerance: float<length>) =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(DegeneracyInvalidTolerance tolerance)
        else
            normalizeSegments tolerance subpath.Segments []
            |> Result.bind (fun segments ->
                let openResult =
                    match segments with
                    | [] -> Ok(Subpath.empty subpath.Start)
                    | _ ->
                        Subpath.createWith Strict segments
                        |> Result.mapError DegeneracyPathError
                openResult
                |> Result.bind (fun rebuilt ->
                    if not subpath.Closed then Ok rebuilt
                    else
                        Subpath.setClosedWith Strict true rebuilt
                        |> Result.mapError DegeneracyPathError))
