namespace SvgPath

type MarkerError =
    | EmptyMarkerSubpath
    | DegenerateMarkerTangent
    | MarkerPathError of SegmentError
    | InvalidMarkerWidth of float<length>
    | InvalidMarkerHeight of float<length>
    | InvalidMarkerStrokeWidth of float<length>
    | InvalidMarkerViewBox of BoundingBox

type MarkerKind =
    | MarkerStart
    | MarkerMid
    | MarkerEnd

[<Struct>]
type MarkerPose =
    { Kind: MarkerKind
      Point: Point<length>
      Angle: float<degree> }

type MarkerOrient =
    | Auto
    | AutoStartReverse
    | Fixed of float<degree>

type MarkerUnits =
    | StrokeWidth
    | UserSpaceOnUse

type AspectAlign =
    | XMinYMin
    | XMidYMin
    | XMaxYMin
    | XMinYMid
    | XMidYMid
    | XMaxYMid
    | XMinYMax
    | XMidYMax
    | XMaxYMax

type PreserveAspectRatio =
    | Stretch
    | Meet of AspectAlign
    | Slice of AspectAlign

[<Struct>]
type MarkerLayout =
    { Reference: Point<length>
      MarkerWidth: float<length>
      MarkerHeight: float<length>
      MarkerUnits: MarkerUnits
      StrokeWidth: float<length>
      ViewBox: BoundingBox option
      PreserveAspectRatio: PreserveAspectRatio }

/// Placement and transformation of SVG markers along path geometry.
[<RequireQualifiedAccess>]
module Marker =
    let private incomingDirection segments =
        let rec loop remaining =
            match remaining with
            | [] -> Error DegenerateMarkerTangent
            | segment :: rest ->
                Segment.directions segment 1.0<parameter>
                |> Result.mapError MarkerPathError
                |> Result.bind (fun directions ->
                    match directions.Incoming with
                    | Some direction -> Ok direction
                    | None -> loop rest)
        loop segments

    let private outgoingDirection segments =
        let rec loop remaining =
            match remaining with
            | [] -> Error DegenerateMarkerTangent
            | segment :: rest ->
                Segment.directions segment 0.0<parameter>
                |> Result.mapError MarkerPathError
                |> Result.bind (fun directions ->
                    match directions.Outgoing with
                    | Some direction -> Ok direction
                    | None -> loop rest)
        loop segments

    let private angleOf (vector: Point<1>) = Trig.atan2Degrees vector.Y vector.X

    let private joinAngle incomingSegments outgoingSegments =
        incomingDirection incomingSegments
        |> Result.bind (fun incoming ->
            outgoingDirection outgoingSegments
            |> Result.map (fun outgoing ->
                let bisector = Point.add incoming outgoing
                match Point.normalize bisector with
                | Some direction -> angleOf direction
                | None -> angleOf incoming))

    let private startAngle segments orient closed =
        match orient with
        | Fixed angle -> Ok angle
        | Auto ->
            if closed then joinAngle (List.rev segments) segments
            else outgoingDirection segments |> Result.map angleOf
        | AutoStartReverse ->
            (if closed then joinAngle (List.rev segments) segments
             else outgoingDirection segments |> Result.map angleOf)
            |> Result.map (fun angle -> angle + 180.0<degree>)

    let private endAngle segments orient closed =
        match orient with
        | Fixed angle -> Ok angle
        | Auto
        | AutoStartReverse ->
            if closed then joinAngle (List.rev segments) segments
            else incomingDirection (List.rev segments) |> Result.map angleOf

    let private midAngle incomingSegments outgoingSegments orient =
        match orient with
        | Fixed angle -> Ok angle
        | Auto
        | AutoStartReverse -> joinAngle incomingSegments outgoingSegments

    let private midPoses segments orient closed =
        let rec loop incoming remaining poses =
            match remaining with
            | [] -> Ok(List.rev poses)
            | next :: rest ->
                let previous = List.head incoming
                let incomingSearch =
                    if closed then incoming @ List.rev remaining else incoming
                let outgoingSearch =
                    if closed then remaining @ List.rev incoming else remaining
                midAngle incomingSearch outgoingSearch orient
                |> Result.bind (fun angle ->
                    let pose =
                        { Kind = MarkerMid
                          Point = Segment.finish previous
                          Angle = angle }
                    loop (next :: incoming) rest (pose :: poses))
        match segments with
        | [] -> Ok []
        | first :: rest -> loop [ first ] rest []

    /// Return marker poses for one non-empty subpath.
    let subpathPoses subpath orient =
        let segments = Subpath.segments subpath
        match segments with
        | [] -> Error EmptyMarkerSubpath
        | first :: _ ->
            let closed = Subpath.isClosed subpath
            startAngle segments orient closed
            |> Result.bind (fun start ->
                midPoses segments orient closed
                |> Result.bind (fun mids ->
                    endAngle segments orient closed
                    |> Result.map (fun finish ->
                        { Kind = MarkerStart
                          Point = Segment.start first
                          Angle = start }
                        :: (mids @
                            [ { Kind = MarkerEnd
                                Point = Segment.finish (List.last segments)
                                Angle = finish } ]))))

    /// Return marker poses for every drawable subpath of a path.
    let pathPoses (path: Path) orient =
        path.Subpaths
        |> List.filter (fun subpath -> not (List.isEmpty subpath.Segments))
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun poses ->
                subpathPoses subpath orient |> Result.map (fun next -> poses @ next))) (Ok [])

    /// Rotate marker-local coordinates, then place their origin at the pose point.
    let transform (point: Point<length>) (angle: float<degree>) =
        Affine.chain (Affine.rotate angle) (Affine.translate point.X point.Y)

    let poseTransform (pose: MarkerPose) = transform pose.Point pose.Angle

    /// Place the marker-local reference point at the supplied path point.
    let transformWithReference (point: Point<length>) (angle: float<degree>) (reference: Point<length>) =
        Affine.translate -reference.X -reference.Y
        |> fun matrix -> Affine.chain matrix (Affine.rotate angle)
        |> fun matrix -> Affine.chain matrix (Affine.translate point.X point.Y)

    let poseTransformWithReference (pose: MarkerPose) reference =
        transformWithReference pose.Point pose.Angle reference

    let private xAlignFraction = function
        | XMinYMin | XMinYMid | XMinYMax -> 0.0
        | XMidYMin | XMidYMid | XMidYMax -> 0.5
        | XMaxYMin | XMaxYMid | XMaxYMax -> 1.0

    let private yAlignFraction = function
        | XMinYMin | XMidYMin | XMaxYMin -> 0.0
        | XMinYMid | XMidYMid | XMaxYMid -> 0.5
        | XMinYMax | XMidYMax | XMaxYMax -> 1.0

    let private uniformViewBoxTransform box markerWidth markerHeight scale align =
        let xExtra = markerWidth - BoundingBox.width box * scale
        let yExtra = markerHeight - BoundingBox.height box * scale
        let xOffset = xExtra * xAlignFraction align
        let yOffset = yExtra * yAlignFraction align
        Affine.translate -box.Min.X -box.Min.Y
        |> fun matrix -> Affine.chain matrix (Affine.scale scale)
        |> fun matrix -> Affine.chain matrix (Affine.translate xOffset yOffset)

    let private viewBoxToMarkerViewport box layout =
        let xScale = layout.MarkerWidth / BoundingBox.width box
        let yScale = layout.MarkerHeight / BoundingBox.height box
        match layout.PreserveAspectRatio with
        | Stretch ->
            Affine.translate -box.Min.X -box.Min.Y
            |> fun matrix -> Affine.chain matrix (Affine.scaleXY xScale yScale)
        | Meet align -> uniformViewBoxTransform box layout.MarkerWidth layout.MarkerHeight (min xScale yScale) align
        | Slice align -> uniformViewBoxTransform box layout.MarkerWidth layout.MarkerHeight (max xScale yScale) align

    let private validateLayout layout =
        let finite value = System.Double.IsFinite(float value)
        if layout.MarkerWidth <= 0.0<length> || not (finite layout.MarkerWidth) then
            Error(InvalidMarkerWidth layout.MarkerWidth)
        elif layout.MarkerHeight <= 0.0<length> || not (finite layout.MarkerHeight) then
            Error(InvalidMarkerHeight layout.MarkerHeight)
        elif layout.MarkerUnits = StrokeWidth
             && (layout.StrokeWidth <= 0.0<length> || not (finite layout.StrokeWidth)) then
            Error(InvalidMarkerStrokeWidth layout.StrokeWidth)
        else
            match layout.ViewBox with
            | Some box when
                BoundingBox.width box <= 0.0<length>
                || BoundingBox.height box <= 0.0<length>
                || not (finite (BoundingBox.width box))
                || not (finite (BoundingBox.height box)) -> Error(InvalidMarkerViewBox box)
            | _ -> Ok()

    let private markerLocalTransform layout =
        let viewBoxTransform =
            match layout.ViewBox with
            | None -> Affine.identity ()
            | Some box -> viewBoxToMarkerViewport box layout
        let unitScale =
            match layout.MarkerUnits with
            | UserSpaceOnUse -> 1.0
            | StrokeWidth -> float layout.StrokeWidth
        Affine.chain viewBoxTransform (Affine.scale unitScale)

    /// Return the complete transform-only SVG marker layout operation.
    let layoutTransform (point: Point<length>) (angle: float<degree>) (layout: MarkerLayout) =
        validateLayout layout
        |> Result.map (fun () ->
            let local = markerLocalTransform layout
            let mappedReference = Affine.point local layout.Reference
            local
            |> fun matrix -> Affine.chain matrix (Affine.translate -mappedReference.X -mappedReference.Y)
            |> fun matrix -> Affine.chain matrix (Affine.rotate angle)
            |> fun matrix -> Affine.chain matrix (Affine.translate point.X point.Y))

    let poseLayoutTransform (pose: MarkerPose) layout =
        layoutTransform pose.Point pose.Angle layout
