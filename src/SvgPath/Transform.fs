namespace SvgPath

type Anchor =
    | TopLeft
    | TopCenter
    | TopRight
    | CenterLeft
    | Center
    | CenterRight
    | BottomLeft
    | BottomCenter
    | BottomRight

type TransformError =
    | DegenerateArcTransform
    | InvalidMatrix
    | PathError of SegmentError

[<RequireQualifiedAccess>]
module Transform =
    let matrix = Affine.matrix
    let fromTuple = Affine.fromTuple
    let toTuple = Affine.toTuple
    let identity = Affine.identity
    let chain = Affine.chain
    let multiply = Affine.multiply
    let aboutPoint = Affine.aboutPoint
    let translate = Affine.translate
    let scale = Affine.scale
    let scaleXY = Affine.scaleXY
    let rotate = Affine.rotate
    let skewX = Affine.skewX
    let skewY = Affine.skewY
    let point = Affine.point

    let pointPairMap sourceStart sourceEnd targetStart targetEnd tolerance =
        Affine.pointPairSimilarity sourceStart sourceEnd targetStart targetEnd
        |> Result.bind (fun transform ->
            let mappedStart = point transform sourceStart
            let mappedEnd = point transform sourceEnd
            if tolerance >= 0.0<length>
               && Point.distance mappedStart targetStart <= tolerance
               && Point.distance mappedEnd targetEnd <= tolerance then Ok transform
            else Error())

    let pointTripleMap sourceA sourceB sourceC targetA targetB targetC tolerance =
        Affine.pointTripleMap sourceA sourceB sourceC targetA targetB targetC
        |> Result.bind (fun transform ->
            if tolerance >= 0.0<length>
               && Point.distance (point transform sourceA) targetA <= tolerance
               && Point.distance (point transform sourceB) targetB <= tolerance
               && Point.distance (point transform sourceC) targetC <= tolerance then Ok transform
            else Error())

    let translatePoint input x y = point (translate x y) input
    let scalePoint input factor = point (scale factor) input
    let scaleXYPoint input x y = point (scaleXY x y) input
    let rotatePoint input degrees = point (rotate degrees) input
    let skewXPoint input degrees = point (skewX degrees) input
    let skewYPoint input degrees = point (skewY degrees) input

    let private validate transform =
        if Affine.isFinite transform then Ok transform else Error InvalidMatrix

    let private transformedSweep sweep transform =
        if Affine.determinant transform < 0.0 then not sweep else sweep

    let private validSegment transform segment =
        match segment with
        | Line(startPoint, endPoint) -> Ok(Line(point transform startPoint, point transform endPoint))
        | QuadraticBezier(startPoint, control, endPoint) ->
            Ok(QuadraticBezier(point transform startPoint, point transform control, point transform endPoint))
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Ok(CubicBezier(point transform startPoint, point transform control1, point transform control2, point transform endPoint))
        | Arc endpoint ->
            Ellipse.transformedAxes endpoint.Radius endpoint.XAxisRotation transform
            |> Result.mapError (fun _ -> DegenerateArcTransform)
            |> Result.map (fun (radius, xAxisRotation) ->
                Arc
                    { Start = point transform endpoint.Start
                      Radius = radius
                      XAxisRotation = xAxisRotation
                      LargeArc = endpoint.LargeArc
                      Sweep = transformedSweep endpoint.Sweep transform
                      End = point transform endpoint.End })

    let segment input transform =
        validate transform |> Result.bind (fun transform -> validSegment transform input)

    let segmentAboutPoint input transform center = segment input (aboutPoint transform center)

    let private anchorPoint box anchor =
        let center = BoundingBox.center box
        match anchor with
        | TopLeft -> box.Min
        | TopCenter -> Point.create center.X box.Min.Y
        | TopRight -> Point.create box.Max.X box.Min.Y
        | CenterLeft -> Point.create box.Min.X center.Y
        | Center -> center
        | CenterRight -> Point.create box.Max.X center.Y
        | BottomLeft -> Point.create box.Min.X box.Max.Y
        | BottomCenter -> Point.create center.X box.Max.Y
        | BottomRight -> box.Max

    let segmentAboutAnchor input transform anchor =
        Segment.boundingBox input
        |> Result.mapError PathError
        |> Result.bind (fun box -> segment input (aboutPoint transform (anchorPoint box anchor)))

    let translateSegment input x y = segment input (translate x y)
    let scaleSegment input factor = segment input (scale factor)
    let scaleXYSegment input x y = segment input (scaleXY x y)
    let rotateSegment input degrees = segment input (rotate degrees)
    let skewXSegment input degrees = segment input (skewX degrees)
    let skewYSegment input degrees = segment input (skewY degrees)

    let segmentGracefully input transform =
        match segment input transform with
        | Ok transformed -> Ok transformed
        | Error DegenerateArcTransform ->
            match input with
            | Arc endpoint ->
                Ellipse.collapsedArcLine
                    endpoint.Start endpoint.Radius endpoint.XAxisRotation
                    endpoint.LargeArc endpoint.Sweep endpoint.End transform
                |> Result.map (fun (startPoint, endPoint) -> Line(startPoint, endPoint))
                |> Result.mapError (fun _ -> DegenerateArcTransform)
            | _ -> Error DegenerateArcTransform
        | Error error -> Error error

    let private linesBetween points =
        points |> List.pairwise |> List.map Line

    let segmentToSubpathGracefully input transform =
        match segment input transform with
        | Ok transformed ->
            Ok(Subpath.ofSegment transformed)
        | Error DegenerateArcTransform ->
            match input with
            | Arc endpoint ->
                Ellipse.collapsedArcSubpath
                    endpoint.Start endpoint.Radius endpoint.XAxisRotation
                    endpoint.LargeArc endpoint.Sweep endpoint.End transform
                |> Result.mapError (fun _ -> DegenerateArcTransform)
                |> Result.bind (fun points ->
                    let segments = linesBetween points
                    match segments with
                    | [] -> Ok(Subpath.empty (points |> List.tryHead |> Option.defaultValue (point transform endpoint.Start)))
                    | _ -> Subpath.create segments |> Result.mapError (fun _ -> DegenerateArcTransform))
            | _ -> Error DegenerateArcTransform
        | Error error -> Error error

    let private transformSubpathWith transformSegment input transform =
        validate transform
        |> Result.bind (fun _ ->
            Subpath.segments input
            |> List.fold (fun state original ->
                state
                |> Result.bind (fun transformed ->
                    transformSegment original transform
                    |> Result.map (fun next -> transformed @ next))) (Ok []))
        |> Result.bind (fun segments ->
            let transformedStart = point transform (Subpath.start input)
            let rebuilt =
                match segments with
                | [] -> Ok(Subpath.empty transformedStart)
                | _ -> Subpath.create segments

            rebuilt
            |> Result.bind (fun subpath ->
                if Subpath.isClosed input then Subpath.setClosed true subpath
                else Ok subpath)
            |> Result.mapError PathError)

    let subpath input transform =
        transformSubpathWith
            (fun segmentValue transform -> segment segmentValue transform |> Result.map List.singleton)
            input transform

    let subpathGracefully input transform =
        transformSubpathWith
            (fun segmentValue transform ->
                segmentToSubpathGracefully segmentValue transform
                |> Result.map Subpath.segments)
            input transform

    let subpathAboutPoint input transform center = subpath input (aboutPoint transform center)

    let subpathAboutAnchor input transform anchor =
        Subpath.boundingBox input
        |> Result.mapError PathError
        |> Result.bind (fun box -> subpath input (aboutPoint transform (anchorPoint box anchor)))

    let translateSubpath input x y = subpath input (translate x y)
    let scaleSubpath input factor = subpath input (scale factor)
    let scaleXYSubpath input x y = subpath input (scaleXY x y)
    let rotateSubpath input degrees = subpath input (rotate degrees)
    let skewXSubpath input degrees = subpath input (skewX degrees)
    let skewYSubpath input degrees = subpath input (skewY degrees)

    let private transformPathWith transformSubpath input transform =
        validate transform
        |> Result.bind (fun _ ->
            Path.subpaths input
            |> List.fold (fun state original ->
                state
                |> Result.bind (fun transformed ->
                    transformSubpath original transform
                    |> Result.map (fun next -> next :: transformed))) (Ok []))
        |> Result.map (List.rev >> Path.ofSubpaths)

    let path input transform = transformPathWith subpath input transform
    let pathGracefully input transform = transformPathWith subpathGracefully input transform
    let pathAboutPoint input transform center = path input (aboutPoint transform center)

    let pathAboutAnchor input transform anchor =
        Path.boundingBox input
        |> Result.mapError PathError
        |> Result.bind (fun box -> path input (aboutPoint transform (anchorPoint box anchor)))

    let translatePath input x y = path input (translate x y)
    let scalePath input factor = path input (scale factor)
    let scaleXYPath input x y = path input (scaleXY x y)
    let rotatePath input degrees = path input (rotate degrees)
    let skewXPath input degrees = path input (skewX degrees)
    let skewYPath input degrees = path input (skewY degrees)

    let boundingBox (box: BoundingBox) transform : Result<BoundingBox, TransformError> =
        validate transform
        |> Result.map (fun _ ->
            [ box.Min
              Point.create box.Max.X box.Min.Y
              Point.create box.Min.X box.Max.Y
              box.Max ]
            |> List.map (point transform)
            |> List.map BoundingBox.fromPoint
            |> List.reduce BoundingBox.union)
