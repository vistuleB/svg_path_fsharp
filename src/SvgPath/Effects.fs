namespace SvgPath

type EffectsError =
    | EffectsPathError of SegmentError
    | InvalidRadius of float<length>
    | InvalidDistanceTolerance of float<length>
    | InvalidAngularTolerance of float<degree>
    | CannotRoundCorner of int
    | CornerTrimsOverlap of int
    | EffectsDegeneracyError of DegeneracyError

type FailureMode =
    | ErrorOnFailure
    | LeaveCorner
    | AdaptRadius

[<Struct>]
type RoundCornerOptions =
    { Failure: FailureMode
      Length: LengthOptions
      DistanceTolerance: float<length>
      AngularTolerance: float<degree> }

[<RequireQualifiedAccess>]
module Effects =
    type private SegmentInfo =
        { Index: int
          Segment: Segment
          Length: float<length> }

    type private CornerSpec =
        { Index: int
          TrimPerRadius: float
          IncomingTangent: Point<1>
          OutgoingTangent: Point<1> }

    type private Corner =
        { Index: int
          Trim: float<length>
          Arc: Segment }

    let defaultRoundCornerOptions =
        { Failure = ErrorOnFailure
          Length = Segment.defaultLengthOptions
          DistanceTolerance = 1.0e-6<length>
          AngularTolerance = 1.0e-6<degree> }

    let normalizeDegenerateSegments subpath tolerance =
        Degeneracy.normalizeDegenerateSegments subpath tolerance
        |> Result.mapError EffectsDegeneracyError

    let private remapEndpoints segment newStart newFinish =
        match segment with
        | Line _ -> Ok(Line(newStart, newFinish))
        | _ ->
            Affine.pointPairSimilarity (Segment.start segment) (Segment.finish segment) newStart newFinish
            |> Result.bind (fun transform -> Transform.segment segment transform |> Result.mapError (fun _ -> ()))
            |> Result.map (Segment.withStart newStart >> Segment.withFinish newFinish)

    let private stretchToJoin previous next closing =
        let join =
            if closing then Segment.start next
            else Point.midpoint (Segment.finish previous) (Segment.start next)
        match remapEndpoints previous (Segment.start previous) join with
        | Error _ -> [ previous; next ]
        | Ok stretchedPrevious when closing -> [ stretchedPrevious ]
        | Ok stretchedPrevious ->
            match remapEndpoints next join (Segment.finish next) with
            | Ok stretchedNext -> [ stretchedPrevious; stretchedNext ]
            | Error _ -> [ previous; next ]

    let stretchToJoinEndpointPolicy () = Custom stretchToJoin

    let private validate radius options =
        if radius <= 0.0<length> || not (System.Double.IsFinite(float radius)) then Error(InvalidRadius radius)
        elif options.DistanceTolerance <= 0.0<length>
             || not (System.Double.IsFinite(float options.DistanceTolerance)) then
            Error(InvalidDistanceTolerance options.DistanceTolerance)
        elif options.AngularTolerance < 0.0<degree>
             || not (System.Double.IsFinite(float options.AngularTolerance)) then
            Error(InvalidAngularTolerance options.AngularTolerance)
        else Segment.validateLengthOptions options.Length |> Result.mapError EffectsPathError

    let private infos (options: RoundCornerOptions) (segments: Segment list) =
        segments
        |> List.indexed
        |> List.fold (fun state (index, segment) ->
            state
            |> Result.bind (fun accumulated ->
                Segment.lengthWith segment options.Length
                |> Result.mapError EffectsPathError
                |> Result.map (fun length -> { Index = index; Segment = segment; Length = length } :: accumulated))) (Ok [])
        |> Result.map List.rev

    let private endpointTangent segment t =
        Segment.derivative segment t
        |> Result.mapError EffectsPathError
        |> Result.map Point.normalize

    let private cornerPairs (infos: SegmentInfo list) closed =
        let adjacent = infos |> List.pairwise |> List.map (fun (incoming, outgoing) -> incoming, outgoing, incoming.Index)
        match infos, closed with
        | [], _ -> []
        | [ only ], true -> [ only, only, only.Index ]
        | _ :: _, true -> adjacent @ [ List.last infos, List.head infos, (List.last infos).Index ]
        | _ -> adjacent

    let private cornerSpec
        (options: RoundCornerOptions)
        ((incoming, outgoing, index): SegmentInfo * SegmentInfo * int) =
        match endpointTangent incoming.Segment 1.0<parameter>, endpointTangent outgoing.Segment 0.0<parameter> with
        | Error error, _
        | _, Error error -> Error error
        | Ok None, _
        | _, Ok None ->
            match options.Failure with
            | ErrorOnFailure -> Error(CannotRoundCorner index)
            | _ -> Ok None
        | Ok(Some incomingTangent), Ok(Some outgoingTangent) ->
            let cosine = max -1.0 (min 1.0 (Point.dot incomingTangent outgoingTangent))
            let angle = System.Math.Acos cosine * 180.0 / System.Math.PI |> Degree.fromFloat
            if angle <= options.AngularTolerance then Ok None
            else
                let trimPerRadius = Trig.tanDegrees (angle / 2.0)
                if trimPerRadius <= 0.0 || not (System.Double.IsFinite trimPerRadius) then Ok None
                else Ok(Some { Index = index; TrimPerRadius = trimPerRadius; IncomingTangent = incomingTangent; OutgoingTangent = outgoingTangent })

    let private buildSpecs
        (options: RoundCornerOptions)
        (infos: SegmentInfo list)
        closed =
        cornerPairs infos closed
        |> List.fold (fun state pair ->
            state
            |> Result.bind (fun accumulated ->
                cornerSpec options pair |> Result.map (Option.map (fun spec -> spec :: accumulated) >> Option.defaultValue accumulated))) (Ok [])
        |> Result.map List.rev

    let private findSpec index (specs: CornerSpec list) = specs |> List.tryFind (fun spec -> spec.Index = index)
    let private radiusFor index radii = radii |> Map.tryFind index |> Option.defaultValue 0.0<length>
    let private previousIndex index count closed = if index > 0 then index - 1 elif closed then count - 1 else -1

    let private adaptRadii
        (options: RoundCornerOptions)
        (infos: SegmentInfo list)
        closed
        (requested: float<length>)
        (specs: CornerSpec list) =
        let initial = specs |> List.map (fun spec -> spec.Index, requested) |> Map.ofList
        let rec loop radii iterations =
            if iterations = 24 then radii
            else
                let scales =
                    infos
                    |> List.fold (fun scales (info: SegmentInfo) ->
                        let beforeIndex = previousIndex info.Index infos.Length closed
                        let before = findSpec beforeIndex specs
                        let after = findSpec info.Index specs
                        let trim (candidate: CornerSpec option) = candidate |> Option.map (fun spec -> radiusFor spec.Index radii * spec.TrimPerRadius) |> Option.defaultValue 0.0<length>
                        let total = trim before + trim after
                        let available = max 0.0<length> (info.Length - 2.0 * options.DistanceTolerance)
                        if total <= available || total = 0.0<length> then scales
                        else
                            let scale = float (available / total)
                            [ beforeIndex; info.Index ]
                            |> List.fold
                                (fun state index ->
                                    let previous = Map.tryFind index state |> Option.defaultValue 1.0
                                    Map.add index (min scale previous) state)
                                scales)
                        Map.empty
                let next = radii |> Map.map (fun index radius -> radius * (Map.tryFind index scales |> Option.defaultValue 1.0))
                let converged = radii |> Map.forall (fun index radius -> abs (radius - radiusFor index next) <= options.DistanceTolerance)
                if converged then next else loop next (iterations + 1)
        loop initial 0

    let private cornersFromSpecs
        (options: RoundCornerOptions)
        (infos: SegmentInfo list)
        radii
        (specs: CornerSpec list) =
        specs
        |> List.fold (fun state spec ->
            state
            |> Result.bind (fun accumulated ->
                let radius = radiusFor spec.Index radii
                let trim = radius * spec.TrimPerRadius
                if radius <= options.DistanceTolerance || trim <= options.DistanceTolerance then Ok accumulated
                elif trim >= infos[spec.Index].Length - options.DistanceTolerance
                     || trim >= infos[(spec.Index + 1) % infos.Length].Length - options.DistanceTolerance then
                    match options.Failure with
                    | ErrorOnFailure -> Error(CannotRoundCorner spec.Index)
                    | LeaveCorner -> Ok accumulated
                    | AdaptRadius -> Error(CornerTrimsOverlap spec.Index)
                else
                    let incoming = infos[spec.Index]
                    let outgoing = infos[(spec.Index + 1) % infos.Length]
                    Segment.pointAtLengthWith incoming.Segment (incoming.Length - trim) options.Length
                    |> Result.mapError EffectsPathError
                    |> Result.bind (fun incomingCut ->
                        Segment.pointAtLengthWith outgoing.Segment trim options.Length
                        |> Result.mapError EffectsPathError
                        |> Result.map (fun outgoingCut ->
                            let sweep = Point.cross spec.IncomingTangent spec.OutgoingTangent >= 0.0
                            { Index = spec.Index
                              Trim = trim
                              Arc = Arc { Start = incomingCut; Radius = Point.create radius radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = sweep; End = outgoingCut } } :: accumulated)))) (Ok [])
        |> Result.map List.rev

    let private cornerFor index (corners: Corner list) = corners |> List.tryFind (fun corner -> corner.Index = index)

    let private resolveLeaveCornerOverlaps
        (options: RoundCornerOptions)
        (infos: SegmentInfo list)
        closed
        (corners: Corner list) =
        let rec loop active =
            let overlap =
                infos
                |> List.tryFind (fun info ->
                    let before = cornerFor (previousIndex info.Index infos.Length closed) active
                    let after = cornerFor info.Index active
                    let trim corner = corner |> Option.map _.Trim |> Option.defaultValue 0.0<length>
                    trim before + trim after >= info.Length - options.DistanceTolerance)
            match overlap with
            | None -> active
            | Some info ->
                let beforeIndex = previousIndex info.Index infos.Length closed
                active
                |> List.filter (fun corner -> corner.Index <> beforeIndex && corner.Index <> info.Index)
                |> loop
        loop corners

    let private buildRounded
        (options: RoundCornerOptions)
        (subpath: Subpath)
        (infos: SegmentInfo list)
        (corners: Corner list) =
        let closed = Subpath.isClosed subpath
        infos
        |> List.fold (fun state (info: SegmentInfo) ->
            state
            |> Result.bind (fun accumulated ->
                let before = previousIndex info.Index infos.Length closed |> fun index -> cornerFor index corners
                let after = cornerFor info.Index corners
                let startTrim = before |> Option.map _.Trim |> Option.defaultValue 0.0<length>
                let endTrim = after |> Option.map _.Trim |> Option.defaultValue 0.0<length>
                if startTrim + endTrim >= info.Length - options.DistanceTolerance then Error(CornerTrimsOverlap info.Index)
                else
                    Segment.betweenLengthsWith info.Segment startTrim (info.Length - endTrim) options.Length
                    |> Result.mapError EffectsPathError
                    |> Result.map (fun shortened ->
                        let next = shortened :: (after |> Option.map (fun corner -> [ corner.Arc ]) |> Option.defaultValue [])
                        accumulated @ next))) (Ok [])
        |> Result.bind (fun segments ->
            Subpath.createWith Wiggle segments
            |> Result.mapError EffectsPathError
            |> Result.bind (fun rounded ->
                if closed then Subpath.setClosedWith Wiggle true rounded |> Result.mapError EffectsPathError
                else Ok rounded))

    let roundSubpathCornersWith subpath radius options =
        validate radius options
        |> Result.bind (fun () ->
            match Subpath.segments subpath with
            | [] -> Ok subpath
            | [ _ ] when not (Subpath.isClosed subpath) -> Ok subpath
            | segments ->
                infos options segments
                |> Result.bind (fun infos ->
                    buildSpecs options infos (Subpath.isClosed subpath)
                    |> Result.bind (fun specs ->
                        let radii =
                            match options.Failure with
                            | AdaptRadius -> adaptRadii options infos (Subpath.isClosed subpath) radius specs
                            | _ -> specs |> List.map (fun spec -> spec.Index, radius) |> Map.ofList
                        cornersFromSpecs options infos radii specs
                        |> Result.bind (fun corners ->
                            let corners =
                                if options.Failure = LeaveCorner then
                                    resolveLeaveCornerOverlaps options infos (Subpath.isClosed subpath) corners
                                else corners
                            buildRounded options subpath infos corners))))

    let roundSubpathCorners subpath radius = roundSubpathCornersWith subpath radius defaultRoundCornerOptions

    let roundCornersWith path radius options =
        validate radius options
        |> Result.bind (fun () ->
            Path.subpaths path
            |> List.fold (fun state subpath ->
                state
                |> Result.bind (fun rounded -> roundSubpathCornersWith subpath radius options |> Result.map (fun next -> next :: rounded))) (Ok [])
            |> Result.map (List.rev >> Path.ofSubpaths))

    let roundCorners path radius = roundCornersWith path radius defaultRoundCornerOptions
