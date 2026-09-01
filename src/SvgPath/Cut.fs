namespace SvgPath

[<RequireQualifiedAccess>]
module Cut =
    let private compareParameters left right =
        compare (left.SegmentIndex, left.T) (right.SegmentIndex, right.T)

    let private canonicalParameters subpathValue parameters =
        parameters
        |> List.fold (fun state parameterValue ->
            state
            |> Result.bind (fun found ->
                Subpath.parameterCanonicalize subpathValue parameterValue
                |> Result.map (fun canonical -> canonical :: found))) (Ok [])
        |> Result.map (List.distinct >> List.sortWith compareParameters)

    let private nonemptySegmentPortion segmentValue fromValue toValue =
        if fromValue = toValue then Ok []
        else Segment.betweenInside segmentValue fromValue toValue |> Result.map List.singleton

    let private forwardSegments (subpathValue: Subpath) fromValue toValue =
        let segments = subpathValue.Segments
        if fromValue.SegmentIndex > toValue.SegmentIndex
           || (fromValue.SegmentIndex = toValue.SegmentIndex && fromValue.T >= toValue.T) then
            Error(InvalidSubpathInterval(fromValue, toValue))
        elif fromValue.SegmentIndex = toValue.SegmentIndex then
            nonemptySegmentPortion segments[fromValue.SegmentIndex] fromValue.T toValue.T
        else
            nonemptySegmentPortion segments[fromValue.SegmentIndex] fromValue.T 1.0<parameter>
            |> Result.bind (fun first ->
                nonemptySegmentPortion segments[toValue.SegmentIndex] 0.0<parameter> toValue.T
                |> Result.map (fun last ->
                    let middle =
                        if toValue.SegmentIndex = fromValue.SegmentIndex + 1 then []
                        else segments[(fromValue.SegmentIndex + 1) .. (toValue.SegmentIndex - 1)]
                    first @ middle @ last))

    let private wrappedSegments (subpathValue: Subpath) fromValue toValue =
        let segments = subpathValue.Segments
        let lastIndex = List.length segments - 1
        nonemptySegmentPortion segments[fromValue.SegmentIndex] fromValue.T 1.0<parameter>
        |> Result.bind (fun first ->
            nonemptySegmentPortion segments[toValue.SegmentIndex] 0.0<parameter> toValue.T
            |> Result.map (fun last ->
                let tail =
                    if fromValue.SegmentIndex = lastIndex then []
                    else segments[(fromValue.SegmentIndex + 1) .. lastIndex]
                let head =
                    if toValue.SegmentIndex = 0 then []
                    else segments[0 .. (toValue.SegmentIndex - 1)]
                first @ tail @ head @ last))

    let private openPiece segments = Subpath.create segments

    let private cutOpen (subpathValue: Subpath) (points: SubpathParameter list) =
        let count = List.length subpathValue.Segments
        let boundaries =
            { SegmentIndex = 0; T = 0.0<parameter> }
            :: points
            @ [ { SegmentIndex = count - 1; T = 1.0<parameter> } ]
        boundaries
        |> List.pairwise
        |> List.fold (fun state (fromValue, toValue) ->
            state
            |> Result.bind (fun pieces ->
                forwardSegments subpathValue fromValue toValue
                |> Result.bind openPiece
                |> Result.map (fun piece -> piece :: pieces))) (Ok [])
        |> Result.map List.rev

    let private cutClosed (subpathValue: Subpath) (points: SubpathParameter list) =
        match points with
        | [] -> Ok []
        | [ point ] ->
            wrappedSegments subpathValue point point
            |> Result.bind openPiece
            |> Result.map List.singleton
        | _ ->
            let pairs = List.pairwise points @ [ List.last points, List.head points ]
            pairs
            |> List.fold (fun state (fromValue, toValue) ->
                state
                |> Result.bind (fun pieces ->
                    let segments =
                        if compareParameters fromValue toValue < 0 then
                            forwardSegments subpathValue fromValue toValue
                        else wrappedSegments subpathValue fromValue toValue
                    segments
                    |> Result.bind openPiece
                    |> Result.map (fun piece -> piece :: pieces))) (Ok [])
            |> Result.map List.rev

    let internal atParameters subpathValue parameters =
        canonicalParameters subpathValue parameters
        |> Result.bind (fun points ->
            if subpathValue.Closed then cutClosed subpathValue points
            else
                let count = List.length subpathValue.Segments
                let usable =
                    points
                    |> List.filter (fun parameterValue ->
                        not (parameterValue.SegmentIndex = 0 && parameterValue.T = 0.0<parameter>)
                        && not (parameterValue.SegmentIndex = count - 1 && parameterValue.T = 1.0<parameter>))
                match usable with
                | [] -> Ok [ subpathValue ]
                | _ -> cutOpen subpathValue usable)

    let subpathWith subject cutter options =
        Encounters.subpathWith subject cutter options
        |> Result.bind (fun found ->
            let pointParameters =
                found.Intersections |> List.collect (fun intersection -> intersection.LeftParameters)
            let overlapParameters =
                found.Overlaps
                |> List.collect (fun overlap ->
                    [ Overlaps.subpathOverlapLeftStart overlap
                      Overlaps.subpathOverlapLeftEnd overlap ]
                    |> List.choose id)
            atParameters subject (pointParameters @ overlapParameters))

    let subpath subject cutter = subpathWith subject cutter Intersections.defaultOptions

    let private subjectCutPoints subject cutter options =
        Encounters.pathWith (Path.singleton subject) cutter options
        |> Result.map (fun found ->
            let pointParameters =
                found.Intersections
                |> List.collect (fun intersection -> intersection.LeftParameters)
                |> List.choose (fun parameterValue ->
                    if parameterValue.SubpathIndex = 0 then Some parameterValue.At else None)
            let overlapParameters =
                found.Overlaps
                |> List.collect (fun overlap ->
                    [ Overlaps.pathOverlapLeftStart overlap
                      Overlaps.pathOverlapLeftEnd overlap ]
                    |> List.choose id)
                |> List.choose (fun parameterValue ->
                    if parameterValue.SubpathIndex = 0 then Some parameterValue.At else None)
            pointParameters @ overlapParameters)

    let pathWith (subject: Path) cutter options =
        Intersections.validateOptions options
        |> Result.bind (fun () ->
            subject.Subpaths
            |> List.fold (fun state subpathValue ->
                state
                |> Result.bind (fun pieces ->
                    subjectCutPoints subpathValue cutter options
                    |> Result.bind (atParameters subpathValue)
                    |> Result.map (fun cutPieces -> List.rev cutPieces @ pieces))) (Ok [])
            |> Result.map (List.rev >> Path.ofSubpaths))

    let path subject cutter = pathWith subject cutter Intersections.defaultOptions
