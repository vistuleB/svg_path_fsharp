namespace SvgPath

type DegeneracyError =
    | DegeneracyPathError of SegmentError
    | DegeneracyConvexHullError of ConvexHullError

[<Struct>]
type internal ThinPrefix =
    { Segments: Segment list
      Remaining: Segment list
      Hull: Subpath option
      Strip: MinimumWidthStrip option }

/// Detection and normalization of geometrically degenerate path segments.
[<RequireQualifiedAccess>]
module Degeneracy =
    let private uniqueAdjacent tolerance points =
        let rec loop previous kept remaining =
            match remaining with
            | [] -> List.rev kept
            | point :: rest when Point.distance previous point <= tolerance -> loop previous kept rest
            | point :: rest -> loop point (point :: kept) rest
        match points with
        | [] -> []
        | first :: rest -> loop first [ first ] rest

    let private traversalLines tolerance points =
        points
        |> uniqueAdjacent tolerance
        |> List.pairwise
        |> List.choose (fun (startPoint, endPoint) ->
            if Point.distance startPoint endPoint <= tolerance then None
            else Some(Line(startPoint, endPoint)))

    let private axialProtrusionPoints axis points =
        let points = uniqueAdjacent 0.0<length> points
        let rec loop previous current kept remaining =
            match remaining with
            | [] -> List.rev (current :: kept)
            | next :: rest ->
                let previousDelta = Point.dot current axis - Point.dot previous axis
                let nextDelta = Point.dot next axis - Point.dot current axis
                let kept = if previousDelta * nextDelta < 0.0<length^2> then current :: kept else kept
                loop current next kept rest
        match points with
        | first :: second :: rest -> loop first second [ first ] rest
        | _ -> points

    let private leadingLineWindow tolerance segments =
        match segments with
        | Line(startPoint, endPoint) :: rest ->
            match Point.displacement startPoint endPoint |> Point.normalize with
            | None -> None
            | Some axis ->
                let normal = Point.rotateClockwise axis
                let startSupport = Point.dot startPoint normal
                let endSupport = Point.dot endPoint normal
                let rec collect lower upper accepted reversedEndpoints remaining =
                    match remaining with
                    | Line(_, nextEnd) :: tail ->
                        let support = Point.dot nextEnd normal
                        let candidateLower, candidateUpper = min lower support, max upper support
                        if candidateUpper - candidateLower <= tolerance then
                            collect candidateLower candidateUpper (accepted + 1) (nextEnd :: reversedEndpoints) tail
                        else accepted, List.rev reversedEndpoints, remaining
                    | _ -> accepted, List.rev reversedEndpoints, remaining
                let accepted, endpoints, remaining =
                    collect (min startSupport endSupport) (max startSupport endSupport) 1 [ endPoint; startPoint ] rest
                if accepted < 2 then None
                else
                    let replacement = endpoints |> axialProtrusionPoints axis |> traversalLines 0.0<length>
                    Some(replacement, remaining)
        | _ -> None

    let private makeSubpath tolerance segments =
        Subpath.createWith (WiggleThenBridgeWith tolerance) segments
        |> Result.mapError DegeneracyPathError

    let private widthDecision hull tolerance =
        ConvexHull.internalConvexSubpathMinimumWidthDecision hull tolerance
        |> Result.mapError DegeneracyConvexHullError

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
                match decision with
                | MinimumWidthFits candidateStrip ->
                    longestThinPrefixLoop tolerance (first :: accepted) candidateHull candidateStrip rest
                | MinimumWidthExceeds _
                | MinimumWidthUnresolved _ ->
                    makeSubpath tolerance (List.rev (first :: accepted))
                    |> Result.bind (fun candidate ->
                        ConvexHull.subpathHull candidate
                        |> Result.mapError DegeneracyConvexHullError
                        |> Result.bind (fun rebuiltHull ->
                            widthDecision rebuiltHull tolerance
                            |> Result.bind (function
                                | MinimumWidthFits rebuiltStrip ->
                                    longestThinPrefixLoop tolerance (first :: accepted) rebuiltHull rebuiltStrip rest
                                | MinimumWidthExceeds _
                                | MinimumWidthUnresolved _ ->
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
                widthDecision hull tolerance
                |> Result.bind (function
                    | MinimumWidthFits strip -> longestThinPrefixLoop tolerance [ first ] hull strip rest
                    | MinimumWidthExceeds _
                    | MinimumWidthUnresolved _ ->
                        Ok { Segments = []; Remaining = first :: rest; Hull = None; Strip = None }))

    let private pointAlreadyPresent point points tolerance =
        points |> List.exists (fun candidate -> Point.distance point candidate <= tolerance)

    let private uniquePoints tolerance points =
        points
        |> List.fold (fun unique point ->
            if pointAlreadyPresent point unique tolerance then unique else point :: unique) []
        |> List.rev

    let private pointOrderInSegments point segments tolerance =
        let rec loop index remaining =
            match remaining with
            | [] -> float index
            | first :: rest ->
                match Segment.projection first point with
                | Ok(t, _, distance) when distance <= tolerance -> float index + float t
                | _ -> loop (index + 1) rest
        loop 0 segments

    let private stripPointsInTraversalOrder segments (strip: MinimumWidthStrip) tolerance =
        match segments with
        | [] -> Error()
        | first :: _ ->
            let startPoint = Segment.start first
            let endPoint = segments |> List.last |> Segment.finish
            let points = [ strip.LowerPoint; strip.UpperPoint ]
            let protrusions =
                (match points with
                 | [ a; b ] when pointOrderInSegments a segments tolerance > pointOrderInSegments b segments tolerance -> [ b; a ]
                 | _ -> points)
                |> uniquePoints tolerance
            Ok(uniquePoints tolerance (startPoint :: (protrusions @ [ endPoint ])))

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
            | Ok points -> Ok(traversalLines tolerance points)

    let rec private normalizeSegments tolerance segments converted =
        match segments with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            match leadingLineWindow tolerance segments with
            | Some(replacement, remaining) ->
                normalizeSegments tolerance remaining (List.rev replacement @ converted)
            | None ->
                makeSubpath tolerance segments
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
    let normalizeDegenerateSegments (subpath: Subpath) (tolerance: float<length>) =
        if tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(DegeneracyPathError(InvalidLinearizeTolerance tolerance))
        else
            normalizeSegments tolerance subpath.Segments []
            |> Result.bind (fun segments ->
                let openResult =
                    match segments with
                    | [] -> Ok(Subpath.empty subpath.Start)
                    | _ ->
                        Subpath.createWith (WiggleThenBridgeWith tolerance) segments
                        |> Result.mapError DegeneracyPathError
                openResult
                |> Result.bind (fun rebuilt ->
                    if not subpath.Closed then Ok rebuilt
                    else
                        Subpath.setClosedWith (WiggleThenBridgeWith tolerance) true rebuilt
                        |> Result.mapError DegeneracyPathError))
