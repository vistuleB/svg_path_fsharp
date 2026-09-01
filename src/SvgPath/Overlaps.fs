namespace SvgPath

[<Struct>]
type SegmentOverlap =
    { LeftFrom: float<parameter>
      LeftTo: float<parameter>
      RightFrom: float<parameter>
      RightTo: float<parameter>
      Start: Point<length>
      Finish: Point<length> }

[<Struct>]
type SegmentSubpathOverlapPiece =
    { SubpathSegmentIndex: int
      Correspondence: SegmentOverlap }

[<Struct>]
type SegmentSubpathOverlap =
    { Start: Point<length>
      Finish: Point<length>
      Pieces: SegmentSubpathOverlapPiece list }

[<Struct>]
type SubpathOverlapPiece =
    { LeftSegmentIndex: int
      RightSegmentIndex: int
      Correspondence: SegmentOverlap }

[<Struct>]
type SubpathOverlap =
    { Start: Point<length>
      Finish: Point<length>
      Pieces: SubpathOverlapPiece list }

[<Struct>]
type PathOverlap =
    { LeftSubpathIndex: int
      RightSubpathIndex: int
      Correspondence: SubpathOverlap }

[<RequireQualifiedAccess>]
module Overlaps =
    let defaultTolerance = 1.0e-9<length>
    let private parameterTolerance = 1.0e-9<parameter>

    let segmentOverlapRightParameter (overlap: SegmentOverlap) leftParameter =
        let portion = (leftParameter - overlap.LeftFrom) / (overlap.LeftTo - overlap.LeftFrom)
        overlap.RightFrom + portion * (overlap.RightTo - overlap.RightFrom)

    let segmentOverlapLeftParameter (overlap: SegmentOverlap) rightParameter =
        let portion = (rightParameter - overlap.RightFrom) / (overlap.RightTo - overlap.RightFrom)
        overlap.LeftFrom + portion * (overlap.LeftTo - overlap.LeftFrom)

    let private fromRaw (raw: RawOverlap) : SegmentOverlap =
        { LeftFrom = raw.LeftFrom
          LeftTo = raw.LeftTo
          RightFrom = raw.RightFrom
          RightTo = raw.RightTo
          Start = raw.Start
          Finish = raw.Finish }

    let segmentWithSamples left right tolerance samples =
        OverlapDetection.detectWithSamples left right tolerance samples
        |> Result.map (List.map fromRaw)

    let checkParameterCorrespondence left right leftFrom leftTo rightFrom rightTo tolerance samples =
        OverlapDetection.checkParameterCorrespondence left right leftFrom leftTo rightFrom rightTo tolerance samples
        |> Result.map (Option.map fromRaw)

    let segmentWith left right tolerance =
        OverlapDetection.detect left right tolerance |> Result.map (List.map fromRaw)

    let segment left right = segmentWith left right defaultTolerance

    let private comparePieces (first: SubpathOverlapPiece) (second: SubpathOverlapPiece) =
        let byIndex = compare first.LeftSegmentIndex second.LeftSegmentIndex
        if byIndex <> 0 then byIndex
        else compare first.Correspondence.LeftFrom second.Correspondence.LeftFrom

    let private nearParameter first second = abs (first - second) <= parameterTolerance

    let private forwardParametersConnect firstIndex firstT secondIndex secondT =
        if firstIndex = secondIndex then nearParameter firstT secondT
        else secondIndex = firstIndex + 1
             && nearParameter firstT 1.0<parameter>
             && nearParameter secondT 0.0<parameter>

    let private parametersConnectEitherDirection firstIndex firstT secondIndex secondT =
        forwardParametersConnect firstIndex firstT secondIndex secondT
        || forwardParametersConnect secondIndex secondT firstIndex firstT

    let private piecesConnect tolerance (left: SubpathOverlapPiece) (right: SubpathOverlapPiece) =
        Point.distance left.Correspondence.Finish right.Correspondence.Start <= tolerance
        && forwardParametersConnect
            left.LeftSegmentIndex left.Correspondence.LeftTo
            right.LeftSegmentIndex right.Correspondence.LeftFrom
        && parametersConnectEitherDirection
            left.RightSegmentIndex left.Correspondence.RightTo
            right.RightSegmentIndex right.Correspondence.RightFrom

    let private overlapFromPieces (pieces: SubpathOverlapPiece list) : SubpathOverlap =
        let first = List.head pieces
        let last = List.last pieces
        { Start = first.Correspondence.Start
          Finish = last.Correspondence.Finish
          Pieces = pieces }

    let private mergePieces tolerance (pieces: SubpathOverlapPiece list) =
        let rec loop remaining current merged =
            match remaining, current with
            | [], [] -> List.rev merged
            | [], _ -> List.rev (overlapFromPieces (List.rev current) :: merged)
            | piece :: rest, [] -> loop rest [ piece ] merged
            | piece :: rest, previous :: _ when piecesConnect tolerance previous piece ->
                loop rest (piece :: current) merged
            | piece :: rest, _ ->
                loop rest [ piece ] (overlapFromPieces (List.rev current) :: merged)
        pieces |> List.sortWith comparePieces |> fun sorted -> loop sorted [] []

    let subpathWith (left: Subpath) (right: Subpath) tolerance =
        let pairs =
            left.Segments
            |> List.indexed
            |> List.collect (fun (leftIndex, leftSegment) ->
                right.Segments
                |> List.indexed
                |> List.map (fun (rightIndex, rightSegment) -> leftIndex, leftSegment, rightIndex, rightSegment))
        pairs
        |> List.fold (fun state (leftIndex, leftSegment, rightIndex, rightSegment) ->
            state
            |> Result.bind (fun found ->
                segmentWith leftSegment rightSegment tolerance
                |> Result.map (fun overlaps ->
                    overlaps
                    |> List.fold (fun accumulated correspondence ->
                        { LeftSegmentIndex = leftIndex
                          RightSegmentIndex = rightIndex
                          Correspondence = correspondence } :: accumulated) found))) (Ok [])
        |> Result.map (mergePieces tolerance)

    let subpath left right = subpathWith left right defaultTolerance

    let private segmentSubpathFromSubpath (overlap: SubpathOverlap) : SegmentSubpathOverlap =
        { Start = overlap.Start
          Finish = overlap.Finish
          Pieces =
            overlap.Pieces
            |> List.map (fun piece ->
                if piece.LeftSegmentIndex <> 0 then
                    invalidOp "a segment-subpath overlap must originate from segment zero"
                ({ SubpathSegmentIndex = piece.RightSegmentIndex;
                   Correspondence = piece.Correspondence } : SegmentSubpathOverlapPiece)) }

    let segmentSubpathWith segmentValue subpathValue tolerance =
        let segmentSubpath = Subpath.ofSegment segmentValue
        subpathWith segmentSubpath subpathValue tolerance
        |> Result.map (List.map segmentSubpathFromSubpath)

    let segmentSubpath segmentValue subpathValue =
        segmentSubpathWith segmentValue subpathValue defaultTolerance

    let segmentSubpathOverlapSegmentStart (overlap: SegmentSubpathOverlap) =
        overlap.Pieces |> List.tryHead |> Option.map (fun piece -> piece.Correspondence.LeftFrom)

    let segmentSubpathOverlapSegmentEnd (overlap: SegmentSubpathOverlap) =
        overlap.Pieces |> List.tryLast |> Option.map (fun piece -> piece.Correspondence.LeftTo)

    let segmentSubpathOverlapSubpathStart (overlap: SegmentSubpathOverlap) =
        overlap.Pieces
        |> List.tryHead
        |> Option.map (fun piece ->
            { SegmentIndex = piece.SubpathSegmentIndex
              T = piece.Correspondence.RightFrom })

    let segmentSubpathOverlapSubpathEnd (overlap: SegmentSubpathOverlap) =
        overlap.Pieces
        |> List.tryLast
        |> Option.map (fun piece ->
            { SegmentIndex = piece.SubpathSegmentIndex
              T = piece.Correspondence.RightTo })

    let subpathOverlapLeftStart (overlap: SubpathOverlap) =
        overlap.Pieces
        |> List.tryHead
        |> Option.map (fun piece ->
            { SegmentIndex = piece.LeftSegmentIndex
              T = piece.Correspondence.LeftFrom })

    let subpathOverlapLeftEnd (overlap: SubpathOverlap) =
        overlap.Pieces
        |> List.tryLast
        |> Option.map (fun piece ->
            { SegmentIndex = piece.LeftSegmentIndex
              T = piece.Correspondence.LeftTo })

    let subpathOverlapRightStart (overlap: SubpathOverlap) =
        overlap.Pieces
        |> List.tryHead
        |> Option.map (fun piece ->
            { SegmentIndex = piece.RightSegmentIndex
              T = piece.Correspondence.RightFrom })

    let subpathOverlapRightEnd (overlap: SubpathOverlap) =
        overlap.Pieces
        |> List.tryLast
        |> Option.map (fun piece ->
            { SegmentIndex = piece.RightSegmentIndex
              T = piece.Correspondence.RightTo })

    let private exactEndpointAliases (first: SubpathParameter) (second: SubpathParameter) (subpath: Subpath) =
        let count = List.length subpath.Segments
        first = second
        || (first.T = 1.0<parameter>
            && second.T = 0.0<parameter>
            && (second.SegmentIndex = first.SegmentIndex + 1
                || (subpath.Closed && first.SegmentIndex = count - 1 && second.SegmentIndex = 0)))
        || (second.T = 1.0<parameter>
            && first.T = 0.0<parameter>
            && (first.SegmentIndex = second.SegmentIndex + 1
                || (subpath.Closed && second.SegmentIndex = count - 1 && first.SegmentIndex = 0)))

    let private parameterInsidePiece (parameterValue: SubpathParameter) segmentIndex pieceFrom pieceTo sourceSubpath =
        if parameterValue.SegmentIndex = segmentIndex
           && parameterValue.T >= min pieceFrom pieceTo
           && parameterValue.T <= max pieceFrom pieceTo then Some parameterValue.T
        else
            let from = { SegmentIndex = segmentIndex; T = pieceFrom }
            let ``to`` = { SegmentIndex = segmentIndex; T = pieceTo }
            if exactEndpointAliases parameterValue from sourceSubpath then Some pieceFrom
            elif exactEndpointAliases parameterValue ``to`` sourceSubpath then Some pieceTo
            else None

    let private pieceOppositeParameter (piece: SubpathOverlapPiece) parameterValue sourceSubpath sourceIsLeft =
        let sourceIndex, sourceFrom, sourceTo, oppositeIndex =
            if sourceIsLeft then
                piece.LeftSegmentIndex, piece.Correspondence.LeftFrom,
                piece.Correspondence.LeftTo, piece.RightSegmentIndex
            else
                piece.RightSegmentIndex, piece.Correspondence.RightFrom,
                piece.Correspondence.RightTo, piece.LeftSegmentIndex
        parameterInsidePiece parameterValue sourceIndex sourceFrom sourceTo sourceSubpath
        |> Option.map (fun sourceT ->
            let oppositeT =
                if sourceIsLeft then segmentOverlapRightParameter piece.Correspondence sourceT
                else segmentOverlapLeftParameter piece.Correspondence sourceT
            { SegmentIndex = oppositeIndex; T = oppositeT })

    let private oppositeParameter (pieces: SubpathOverlapPiece list) parameterValue sourceSubpath sourceIsLeft =
        pieces
        |> List.tryPick (fun piece -> pieceOppositeParameter piece parameterValue sourceSubpath sourceIsLeft)

    let private canonicalizeOptional parameterValue subpathValue =
        match parameterValue with
        | None -> Ok None
        | Some value -> Subpath.parameterCanonicalize subpathValue value |> Result.map Some

    let subpathOverlapRightParameter (overlap: SubpathOverlap) leftParameter leftSubpath rightSubpath =
        oppositeParameter overlap.Pieces leftParameter leftSubpath true
        |> fun opposite -> canonicalizeOptional opposite rightSubpath

    let subpathOverlapLeftParameter (overlap: SubpathOverlap) rightParameter leftSubpath rightSubpath =
        oppositeParameter overlap.Pieces rightParameter rightSubpath false
        |> fun opposite -> canonicalizeOptional opposite leftSubpath

    let private segmentSubpathAsSubpath (overlap: SegmentSubpathOverlap) : SubpathOverlap =
        { Start = overlap.Start
          Finish = overlap.Finish
          Pieces =
            overlap.Pieces
            |> List.map (fun piece ->
                ({ LeftSegmentIndex = 0;
                   RightSegmentIndex = piece.SubpathSegmentIndex;
                   Correspondence = piece.Correspondence } : SubpathOverlapPiece)) }

    let segmentSubpathOverlapSubpathParameter overlap segmentParameter segmentValue subpathValue =
        let segmentSubpath = Subpath.ofSegment segmentValue
        subpathOverlapRightParameter
            (segmentSubpathAsSubpath overlap)
            { SegmentIndex = 0; T = segmentParameter }
            segmentSubpath
            subpathValue

    let segmentSubpathOverlapSegmentParameter overlap subpathParameter segmentValue subpathValue =
        let segmentSubpath = Subpath.ofSegment segmentValue
        subpathOverlapLeftParameter
            (segmentSubpathAsSubpath overlap)
            subpathParameter
            segmentSubpath
            subpathValue
        |> Result.map (Option.map (fun parameterValue -> parameterValue.T))

    let pathOverlapLeftStart (overlap: PathOverlap) =
        subpathOverlapLeftStart overlap.Correspondence
        |> Option.map (fun at -> { SubpathIndex = overlap.LeftSubpathIndex; At = at })

    let pathOverlapLeftEnd (overlap: PathOverlap) =
        subpathOverlapLeftEnd overlap.Correspondence
        |> Option.map (fun at -> { SubpathIndex = overlap.LeftSubpathIndex; At = at })

    let pathOverlapRightStart (overlap: PathOverlap) =
        subpathOverlapRightStart overlap.Correspondence
        |> Option.map (fun at -> { SubpathIndex = overlap.RightSubpathIndex; At = at })

    let pathOverlapRightEnd (overlap: PathOverlap) =
        subpathOverlapRightEnd overlap.Correspondence
        |> Option.map (fun at -> { SubpathIndex = overlap.RightSubpathIndex; At = at })

    let pathOverlapRightParameter (overlap: PathOverlap) leftParameter (leftPath: Path) (rightPath: Path) =
        match List.tryItem overlap.LeftSubpathIndex leftPath.Subpaths,
              List.tryItem overlap.RightSubpathIndex rightPath.Subpaths with
        | Some leftSubpath, Some rightSubpath when leftParameter.SubpathIndex = overlap.LeftSubpathIndex ->
            subpathOverlapRightParameter overlap.Correspondence leftParameter.At leftSubpath rightSubpath
            |> Result.map (Option.map (fun at -> { SubpathIndex = overlap.RightSubpathIndex; At = at }))
        | _ -> Ok None

    let pathOverlapLeftParameter (overlap: PathOverlap) rightParameter (leftPath: Path) (rightPath: Path) =
        match List.tryItem overlap.LeftSubpathIndex leftPath.Subpaths,
              List.tryItem overlap.RightSubpathIndex rightPath.Subpaths with
        | Some leftSubpath, Some rightSubpath when rightParameter.SubpathIndex = overlap.RightSubpathIndex ->
            subpathOverlapLeftParameter overlap.Correspondence rightParameter.At leftSubpath rightSubpath
            |> Result.map (Option.map (fun at -> { SubpathIndex = overlap.LeftSubpathIndex; At = at }))
        | _ -> Ok None

    let pathWith (left: Path) (right: Path) tolerance =
        let pairs =
            left.Subpaths
            |> List.indexed
            |> List.collect (fun (leftIndex, leftSubpath) ->
                right.Subpaths
                |> List.indexed
                |> List.map (fun (rightIndex, rightSubpath) -> leftIndex, leftSubpath, rightIndex, rightSubpath))
        pairs
        |> List.fold (fun state (leftIndex, leftSubpath, rightIndex, rightSubpath) ->
            state
            |> Result.bind (fun found ->
                subpathWith leftSubpath rightSubpath tolerance
                |> Result.map (fun overlaps ->
                    overlaps
                    |> List.fold (fun accumulated correspondence ->
                        { LeftSubpathIndex = leftIndex
                          RightSubpathIndex = rightIndex
                          Correspondence = correspondence } :: accumulated) found))) (Ok [])
        |> Result.map List.rev

    let path left right = pathWith left right defaultTolerance
