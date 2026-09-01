namespace SvgPath

type DegeneracyError =
    | DegeneracyPathError of SegmentError
    | DegeneracyConvexHullError of ConvexHullError

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

    let private makeSubpath segments =
        Subpath.createWith WiggleThenBridge segments
        |> Result.mapError DegeneracyPathError

    let private minimumWidth segments =
        makeSubpath segments
        |> Result.bind (fun subpath ->
            ConvexHull.subpathMinimumWidth subpath |> Result.mapError DegeneracyConvexHullError)

    let private fits tolerance segments =
        minimumWidth segments
        |> Result.map (fun extremum -> extremum.Converged && extremum.Width <= tolerance)

    let private longestThinPrefix tolerance segments =
        let rec grow accepted remaining =
            match remaining with
            | [] -> Ok(List.rev accepted, [])
            | segment :: rest ->
                let candidate = List.rev (segment :: accepted)
                fits tolerance candidate
                |> Result.bind (fun acceptedCandidate ->
                    if acceptedCandidate then grow (segment :: accepted) rest
                    else Ok(List.rev accepted, remaining))
        grow [] segments

    let private segmentDegenerateLines tolerance segment =
        match segment with
        | Line _ -> Ok None
        | _ ->
            fits tolerance [ segment ]
            |> Result.bind (fun isDegenerate ->
                if not isDegenerate then Ok None
                else
                    Segment.toLinesWith
                        { Tolerance = tolerance
                          MaxDepth = 20 }
                        segment
                    |> Result.map Some
                    |> Result.mapError DegeneracyPathError)

    let rec private degenerateTraversal tolerance segments =
        match segments with
        | [] -> Ok []
        | first :: rest ->
            segmentDegenerateLines tolerance first
            |> Result.bind (fun replacement ->
                degenerateTraversal tolerance rest
                |> Result.map (fun remaining -> (Option.defaultValue [ first ] replacement) @ remaining))

    let rec private normalizeSegments tolerance segments converted =
        match segments with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            match leadingLineWindow tolerance segments with
            | Some(replacement, remaining) ->
                normalizeSegments tolerance remaining (List.rev replacement @ converted)
            | None ->
                longestThinPrefix tolerance segments
                |> Result.bind (fun (prefix, remaining) ->
                    match prefix with
                    | _ :: _ :: _ ->
                        degenerateTraversal tolerance prefix
                        |> Result.bind (fun lines ->
                            normalizeSegments tolerance remaining (List.rev lines @ converted))
                    | _ ->
                        segmentDegenerateLines tolerance first
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
