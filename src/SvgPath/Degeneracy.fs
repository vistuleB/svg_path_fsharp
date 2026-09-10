namespace SvgPath

/// Detection and normalization of geometrically degenerate path segments.
[<RequireQualifiedAccess>]
module Degeneracy =

    type Error =
        /// The tolerance must be finite and non-negative.
        | DegeneracyInvalidTolerance of tolerance: float<length>
        | DegeneracyPathError of error: SegmentError
        | DegeneracyConvexHullError of error: ConvexHull.Error

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
                        ConvexHull.subpathHull candidate
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
            ConvexHull.segmentHull first
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
        let angle = strip.Normal |> Point.rotateClockwise |> Point.heading
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
            Segment.degenerateLines first tolerance
            |> Result.mapError DegeneracyPathError
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
                    Segment.degenerateLines first tolerance
                    |> Result.mapError DegeneracyPathError
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
