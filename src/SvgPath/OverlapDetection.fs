namespace SvgPath

[<Struct>]
type internal RawOverlap =
    { LeftFrom: float<parameter>
      LeftTo: float<parameter>
      RightFrom: float<parameter>
      RightTo: float<parameter>
      Start: Point<length>
      Finish: Point<length> }

type private ProjectionSource = LeftEndpoint | RightEndpoint

[<Struct>]
type private EndpointProjection =
    { Source: ProjectionSource
      SourceT: float<parameter>
      TargetT: float<parameter>
      Distance: float<length> }

type private OverlapMerge = Disjoint | Merged of RawOverlap | Contradiction

type private OverlapCandidates = { Affine: RawOverlap list; NonAffine: RawOverlap list }

[<RequireQualifiedAccess>]
module internal OverlapDetection =
    let private parameterTolerance = 1.0e-12<parameter>

    let private canonical overlap =
        if overlap.LeftFrom <= overlap.LeftTo then overlap
        else
            { LeftFrom = overlap.LeftTo
              LeftTo = overlap.LeftFrom
              RightFrom = overlap.RightTo
              RightTo = overlap.RightFrom
              Start = overlap.Finish
              Finish = overlap.Start }

    let private positiveSpan overlap =
        overlap.LeftTo > overlap.LeftFrom && overlap.RightTo <> overlap.RightFrom

    let private pointsNear tolerance first second = Point.squaredDistance first second <= tolerance * tolerance
    let private intervalsOverlap firstFrom firstTo secondFrom secondTo = firstFrom <= secondTo && secondFrom <= firstTo

    let private parameterOrderCompatible firstLeft secondLeft firstRight secondRight rightIncreases =
        if firstLeft < secondLeft then if rightIncreases then firstRight <= secondRight else firstRight >= secondRight
        elif firstLeft > secondLeft then if rightIncreases then firstRight >= secondRight else firstRight <= secondRight
        else abs (firstRight - secondRight) <= parameterTolerance

    let private boundaryCompatible tolerance firstLeft secondLeft firstRight secondRight firstPoint secondPoint =
        firstLeft <> secondLeft
        || (abs (firstRight - secondRight) <= parameterTolerance && pointsNear tolerance firstPoint secondPoint)

    let private merge tolerance first second =
        let firstIncreases = first.RightTo > first.RightFrom
        let secondIncreases = second.RightTo > second.RightFrom
        if first.LeftTo <= first.LeftFrom || second.LeftTo <= second.LeftFrom
           || first.RightTo = first.RightFrom || second.RightTo = second.RightFrom then Contradiction
        else
            let endpointsTouch = pointsNear tolerance first.Finish second.Start || pointsNear tolerance first.Start second.Finish
            let leftsTouch = endpointsTouch || intervalsOverlap first.LeftFrom first.LeftTo second.LeftFrom second.LeftTo
            let rightsTouch =
                endpointsTouch
                || intervalsOverlap (min first.RightFrom first.RightTo) (max first.RightFrom first.RightTo)
                    (min second.RightFrom second.RightTo) (max second.RightFrom second.RightTo)
            if leftsTouch <> rightsTouch then Contradiction
            elif not leftsTouch then Disjoint
            else
                let compatible =
                    firstIncreases = secondIncreases
                    && parameterOrderCompatible first.LeftFrom second.LeftFrom first.RightFrom second.RightFrom firstIncreases
                    && parameterOrderCompatible first.LeftTo second.LeftTo first.RightTo second.RightTo firstIncreases
                    && boundaryCompatible tolerance first.LeftFrom second.LeftFrom first.RightFrom second.RightFrom first.Start second.Start
                    && boundaryCompatible tolerance first.LeftTo second.LeftTo first.RightTo second.RightTo first.Finish second.Finish
                    && boundaryCompatible tolerance first.LeftTo second.LeftFrom first.RightTo second.RightFrom first.Finish second.Start
                    && boundaryCompatible tolerance first.LeftFrom second.LeftTo first.RightFrom second.RightTo first.Start second.Finish
                if not compatible then Contradiction
                else
                    let leftFrom, rightFrom, startPoint =
                        if first.LeftFrom <= second.LeftFrom then first.LeftFrom, first.RightFrom, first.Start
                        else second.LeftFrom, second.RightFrom, second.Start
                    let leftTo, rightTo, finish =
                        if first.LeftTo >= second.LeftTo then first.LeftTo, first.RightTo, first.Finish
                        else second.LeftTo, second.RightTo, second.Finish
                    Merged { LeftFrom = leftFrom; LeftTo = leftTo; RightFrom = rightFrom; RightTo = rightTo; Start = startPoint; Finish = finish }

    let private mergeAll tolerance overlaps =
        let rec insert overlap existing disjoint =
            match existing with
            | [] -> Ok(overlap :: disjoint)
            | first :: rest ->
                match merge tolerance overlap first with
                | Contradiction -> Error()
                | Disjoint -> insert overlap rest (first :: disjoint)
                | Merged combined -> insert combined (rest @ disjoint) []
        overlaps
        |> List.fold (fun state overlap -> state |> Result.bind (fun merged -> insert overlap merged [])) (Ok [])
        |> Result.map List.rev

    // Deduplicate addresses, not positions; exact endpoints are supplied first.
    let private matchingParameters segment at parameters tolerance =
        parameters |> List.fold (fun state t ->
            state |> Result.bind (fun found ->
                Segment.point segment t |> Result.map (fun candidate ->
                    if Point.distance at candidate <= tolerance
                       && not(List.exists (fun previous -> abs(previous-t) <= 1e-9<parameter>) found)
                    then found @ [t] else found))) (Ok [])

    let rec private coordinateControlDegree values =
        let differences = values |> List.pairwise |> List.map (fun (a,b) -> b-a)
        if differences |> List.exists (fun value -> not(InternalNumber.isZero value))
        then 1 + coordinateControlDegree differences else 0

    let private coordinateDegree segment x =
        let coordinate (p:Point<length>) = if x then p.X else p.Y
        match segment with
        | Line(a,b) -> if InternalNumber.isZero(coordinate b-coordinate a) then 0 else 1
        | QuadraticBezier(a,b,c) -> coordinateControlDegree(List.map coordinate [a;b;c])
        | CubicBezier(a,b,c,d) -> coordinateControlDegree(List.map coordinate [a;b;c;d])
        | Arc _ -> -1 // No polynomial completeness claim for arcs.

    let private coordinateMatches segment at x tolerance =
        let degree = coordinateDegree segment x
        if degree=0 then Ok([],0)
        else
            let options = { Segment.defaultCrossingOptions with SignedLineDistanceTolerance=max (tolerance*0.25) 1e-12<length> }
            let direction = if x then Point.create 0.0 1.0 else Point.create 1.0 0.0
            Segment.rayCrossingsWith segment at direction options
            |> Result.bind (fun roots -> matchingParameters segment at ([0.0<parameter>;1.0<parameter>] @ List.map fst roots) tolerance)
            |> Result.map (fun matches -> matches,degree)

    let private completeCoordinate found =
        // A degree-d coordinate has at most d distinct roots. Count only
        // geometrically verified, deduplicated matches toward this bound.
        match found with
        | Ok(matches,degree) -> degree>0 && List.length matches=degree
        | Error _ -> false

    let private combineCoordinateMatches x y =
        match x,y with
        | Ok(xs,_),Ok(ys,_) -> Ok(xs@ys)
        | Ok(xs,_),Error(CrossingMaxIterationsReached _ as error) -> if completeCoordinate x then Ok xs else Error error
        | Error(CrossingMaxIterationsReached _ as error),Ok(ys,_) -> if completeCoordinate y then Ok ys else Error error
        | Error error,_ | _,Error error -> Error error

    let internal pointParameters target sample tolerance =
        // A complete coordinate inventory can replace failed projection;
        // an arbitrary partial collection of geometric matches cannot.
        let x = coordinateMatches target sample true tolerance
        let y = coordinateMatches target sample false tolerance
        combineCoordinateMatches x y |> Result.bind (fun coordinates ->
            let projected =
                match Segment.projection target sample with
                | Ok(t,_,_) -> Ok[t]
                | Error(DistanceMaxIterationsReached _ as error) ->
                    if completeCoordinate x || completeCoordinate y then Ok[] else Error error
                | Error error -> Error error
            projected |> Result.bind (fun projected ->
                matchingParameters target sample ([0.0<parameter>;1.0<parameter>] @ coordinates @ projected) tolerance))

    let private endpointProjection source sourceT sample target tolerance =
        pointParameters target sample tolerance
        |> Result.bind (fun matches ->
                matches |> List.fold (fun state t -> state |> Result.bind (fun found ->
                    Segment.point target t |> Result.map (fun at ->
                        found @ [{Source=source;SourceT=sourceT;TargetT=t;Distance=Point.distance sample at}]))) (Ok []))

    let private endpointProjections left right tolerance =
        [ endpointProjection LeftEndpoint 0.0<parameter> (Segment.start left) right tolerance
          endpointProjection LeftEndpoint 1.0<parameter> (Segment.finish left) right tolerance
          endpointProjection RightEndpoint 0.0<parameter> (Segment.start right) left tolerance
          endpointProjection RightEndpoint 1.0<parameter> (Segment.finish right) left tolerance ]
        |> List.fold (fun state item ->
            match state, item with
            | Ok items, Ok item -> Ok(item :: items)
            | Error error, _ -> Error error
            | _, Error error -> Error error) (Ok [])
        |> Result.map (List.rev >> List.concat >> List.distinct)

    let private fromProjectionPair first second left =
        let leftFrom, leftTo, rightFrom, rightTo =
            match first.Source, second.Source with
            | LeftEndpoint, LeftEndpoint -> first.SourceT, second.SourceT, first.TargetT, second.TargetT
            | RightEndpoint, RightEndpoint -> first.TargetT, second.TargetT, first.SourceT, second.SourceT
            | LeftEndpoint, RightEndpoint -> first.SourceT, second.TargetT, first.TargetT, second.SourceT
            | RightEndpoint, LeftEndpoint -> first.TargetT, second.SourceT, first.SourceT, second.TargetT
        match Segment.point left leftFrom, Segment.point left leftTo with
        | Ok startPoint, Ok finish ->
            Ok(canonical { LeftFrom = leftFrom; LeftTo = leftTo; RightFrom = rightFrom; RightTo = rightTo; Start = startPoint; Finish = finish })
        | Error error, _
        | _, Error error -> Error error

    let private samplePortions samples = [ 1 .. samples ] |> List.map (fun index -> float index / float (samples + 1))

    let private sampledOverlapValid overlap left right tolerance samples =
        Segment.betweenInside right (min overlap.RightFrom overlap.RightTo) (max overlap.RightFrom overlap.RightTo)
        |> Result.bind (fun rightPiece ->
            samplePortions samples
            |> List.fold (fun state portion ->
                state
                |> Result.bind (fun valid ->
                    if not valid then Ok false
                    else
                        let t = overlap.LeftFrom + portion * (overlap.LeftTo - overlap.LeftFrom)
                        Segment.point left t
                        |> Result.bind (fun sample -> Segment.distance rightPiece sample)
                        |> Result.map (fun distance -> distance <= tolerance))) (Ok true))

    let private affineValid overlap left right tolerance samples =
        samplePortions samples
        |> List.fold (fun state portion ->
            state
            |> Result.bind (fun valid ->
                if not valid then Ok false
                else
                    let leftT = overlap.LeftFrom + portion * (overlap.LeftTo - overlap.LeftFrom)
                    let rightT = overlap.RightFrom + portion * (overlap.RightTo - overlap.RightFrom)
                    match Segment.point left leftT, Segment.point right rightT with
                    | Ok leftPoint, Ok rightPoint -> Ok(pointsNear tolerance leftPoint rightPoint)
                    | Error error, _
                    | _, Error error -> Error error)) (Ok true)

    // Check both endpoint pairs explicitly as well as interior samples.
    let checkParameterCorrespondence left right leftFrom leftTo rightFrom rightTo tolerance samples =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then Error(InvalidOverlapTolerance tolerance)
        elif samples <= 0 then Error(InvalidOverlapSamples samples)
        else
            match Segment.point left leftFrom, Segment.point left leftTo with
            | Error error, _
            | _, Error error -> Error error
            | Ok startPoint, Ok finish ->
                let overlap = canonical { LeftFrom = leftFrom; LeftTo = leftTo; RightFrom = rightFrom; RightTo = rightTo; Start = startPoint; Finish = finish }
                if not (positiveSpan overlap) then Ok None
                else
                    Segment.point right rightFrom |> Result.bind (fun rightStart ->
                        Segment.point right rightTo |> Result.bind (fun rightEnd ->
                            if pointsNear tolerance startPoint rightStart && pointsNear tolerance finish rightEnd then
                                sampledOverlapValid overlap left right tolerance samples
                            else Ok false))
                    |> Result.bind (fun sampled ->
                        if not sampled then Ok None
                        else affineValid overlap left right tolerance samples
                             |> Result.bind (fun affine -> if affine then Ok(Some overlap) else Error NonAffineOverlapCorrespondence))

    let private candidateCovered candidate accepted =
        let rec intervalsCover intervals fromT toT =
            if fromT >= toT then true
            else
                match intervals with
                | [] -> false
                | (start,finish)::rest -> if start > fromT then false else intervalsCover rest (max fromT finish) toT
        let left = accepted |> List.map (fun item -> min item.LeftFrom item.LeftTo,max item.LeftFrom item.LeftTo) |> List.sortBy fst
        let right = accepted |> List.map (fun item -> min item.RightFrom item.RightTo,max item.RightFrom item.RightTo) |> List.sortBy fst
        intervalsCover left (min candidate.LeftFrom candidate.LeftTo) (max candidate.LeftFrom candidate.LeftTo)
        && intervalsCover right (min candidate.RightFrom candidate.RightTo) (max candidate.RightFrom candidate.RightTo)

    // Sampled overlap detection proposes intervals from endpoint matches;
    // every overlap boundary is assumed to be an input segment endpoint.
    let detectWithSamples left right tolerance samples =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then Error(InvalidOverlapTolerance tolerance)
        elif samples <= 0 then Error(InvalidOverlapSamples samples)
        else
            endpointProjections left right tolerance
            |> Result.bind (fun projections ->
                let close = projections |> List.filter (fun projection -> projection.Distance <= tolerance)
                close
                |> List.mapi (fun index first -> close |> List.skip (index + 1) |> List.map (fun second -> first, second))
                |> List.concat
                |> List.fold (fun state (first, second) ->
                    state
                    |> Result.bind (fun candidates ->
                        fromProjectionPair first second left
                        |> Result.bind (fun overlap ->
                            if not (positiveSpan overlap) then Ok candidates
                            else
                                sampledOverlapValid overlap left right tolerance samples
                                |> Result.bind (fun valid ->
                                    if not valid then Ok candidates
                                    else affineValid overlap left right tolerance samples
                                         |> Result.bind (fun affine ->
                                            if affine then Ok { candidates with Affine=overlap::candidates.Affine }
                                            else
                                                // A wrong pairing can contain an extra loop on one side.
                                                // Require reciprocal containment before calling it non-affine.
                                                let opposite = canonical { overlap with LeftFrom=overlap.RightFrom;LeftTo=overlap.RightTo;RightFrom=overlap.LeftFrom;RightTo=overlap.LeftTo }
                                                sampledOverlapValid opposite right left tolerance samples
                                                |> Result.map (fun reciprocal ->
                                                    if reciprocal then { candidates with NonAffine=overlap::candidates.NonAffine }
                                                    else candidates)))))) (Ok { Affine=[]; NonAffine=[] })
                |> Result.bind (fun candidates ->
                    // A rejected orientation is not evidence of non-affinity if
                    // accepted alternatives cover both parameter domains.
                    if candidates.NonAffine |> List.exists (fun rejected -> not(candidateCovered rejected candidates.Affine)) then
                        Error NonAffineOverlapCorrespondence
                    else
                        match mergeAll tolerance candidates.Affine with
                        | Ok merged -> Ok merged
                        | Error _ -> Ok []))

    let detect left right tolerance = detectWithSamples left right tolerance 5
