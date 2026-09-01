namespace SvgPath

[<Struct>]
type RawOverlap =
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

[<RequireQualifiedAccess>]
module OverlapDetection =
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

    let private endpointProjection source sourceT sample target =
        Segment.projection target sample
        |> Result.map (fun (targetT, _, distance) -> { Source = source; SourceT = sourceT; TargetT = targetT; Distance = distance })

    let private endpointProjections left right =
        [ endpointProjection LeftEndpoint 0.0<parameter> (Segment.start left) right
          endpointProjection LeftEndpoint 1.0<parameter> (Segment.finish left) right
          endpointProjection RightEndpoint 0.0<parameter> (Segment.start right) left
          endpointProjection RightEndpoint 1.0<parameter> (Segment.finish right) left ]
        |> List.fold (fun state item ->
            match state, item with
            | Ok items, Ok item -> Ok(item :: items)
            | Error error, _ -> Error error
            | _, Error error -> Error error) (Ok [])
        |> Result.map List.rev

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
                    sampledOverlapValid overlap left right tolerance samples
                    |> Result.bind (fun sampled ->
                        if not sampled then Ok None
                        else affineValid overlap left right tolerance samples
                             |> Result.bind (fun affine -> if affine then Ok(Some overlap) else Error NonAffineOverlapCorrespondence))

    let detectWithSamples left right tolerance samples =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then Error(InvalidOverlapTolerance tolerance)
        elif samples <= 0 then Error(InvalidOverlapSamples samples)
        else
            endpointProjections left right
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
                                            if affine then Ok(overlap :: candidates)
                                            else Error NonAffineOverlapCorrespondence))))) (Ok [])
                |> Result.map (fun candidates ->
                    match mergeAll tolerance candidates with
                    | Ok merged -> merged
                    | Error _ -> []))

    let detect left right tolerance = detectWithSamples left right tolerance 5
