namespace SvgPath

[<Struct>]
type EdgeAnnotationPose =
    { Point: Point<length>
      Rotation: float<degree> }

[<Struct>]
type AnnotatedDrawingOptions = { Scale: float }

[<RequireQualifiedAccess>]
module ArrangementDrawing =
    let defaultAnnotatedDrawingOptions = { Scale = 1.0 }

    let private singleSegmentPath segment =
        Path.ofSubpaths [ Subpath.create [ segment ] |> Result.defaultWith (failwithf "%A") ]

    let drawing (graph: ArrangementGraph) =
        let edgeThings =
            graph.Edges
            |> List.collect (fun edge ->
                let midpoint = Segment.point edge.Segment 0.5<parameter> |> Result.defaultValue (Segment.start edge.Segment)
                [ StyledPath(singleSegmentPath edge.Segment, "fill: none; stroke: #334155; stroke-width: 1.5")
                  Rectangle(Point.create (midpoint.X - 11.0<length>) (midpoint.Y - 7.0<length>), 22.0<length>, 14.0<length>, "fill: white; stroke: #94a3b8; stroke-width: 0.75")
                  Text($"{edge.ForwardMultiplicity}/{edge.ReverseMultiplicity}", "fill: #0f172a; font-family: monospace; text-anchor: middle; dominant-baseline: central", Point.create midpoint.X (midpoint.Y + 0.5<length>), 8.0<length>) ])
        let vertexThings =
            graph.Vertices
            |> List.collect (fun vertex -> Svg.labeledPoint $"v{vertex.Id}" "#dc2626" vertex.Point 8.0<length>)
        edgeThings @ vertexThings

    let segmentDirectionArrowWith segment color lengthScale widthScale arrivalOffset opacity =
        Segment.point segment 1.0<parameter>
        |> Result.mapError (fun _ -> ())
        |> Result.bind (fun endpoint ->
            Segment.directions segment 1.0<parameter>
            |> Result.mapError (fun _ -> ())
            |> Result.bind (fun directions ->
                match directions.Incoming with
                | None -> Error()
                | Some direction ->
                    let perpendicular = Point.create -direction.Y direction.X
                    let tip = Point.subtract endpoint (Point.scale arrivalOffset direction)
                    let behind = Point.subtract tip (Point.scale (9.0<length> * lengthScale) direction)
                    let left = Point.add behind (Point.scale (3.5<length> * widthScale) perpendicular)
                    let right = Point.subtract behind (Point.scale (3.5<length> * widthScale) perpendicular)
                    Subpath.polygon [ tip; left; right ]
                    |> Result.mapError (fun _ -> ())
                    |> Result.map (fun arrow ->
                        StyledPath(Path.ofSubpaths [ arrow ], $"fill: {color}; fill-opacity: {opacity}; stroke: none"))))

    let segmentDirectionArrow segment color =
        segmentDirectionArrowWith segment color 1.0 1.0 0.0<length> 1.0

    let subpathDirectionArrowsWith (subpath: Subpath) color lengthScale widthScale arrivalOffset opacity =
        subpath.Segments
        |> List.choose (fun segment ->
            segmentDirectionArrowWith segment color lengthScale widthScale arrivalOffset opacity |> Result.toOption)

    let subpathDirectionArrows subpath color =
        subpathDirectionArrowsWith subpath color 1.0 1.0 0.0<length> 1.0

    let pathDirectionArrowsWith (path: Path) color lengthScale widthScale arrivalOffset opacity =
        path.Subpaths
        |> List.collect (fun subpath ->
            subpathDirectionArrowsWith subpath color lengthScale widthScale arrivalOffset opacity)

    let pathDirectionArrows path color =
        pathDirectionArrowsWith path color 1.0 1.0 0.0<length> 1.0

    let edgeAnnotationPose edge =
        Segment.point edge.Segment 0.5<parameter>
        |> Result.bind (fun midpoint ->
            Segment.directions edge.Segment 0.5<parameter>
            |> Result.bind (fun directions ->
                match directions.Incoming, directions.Outgoing with
                | Some direction, _
                | None, Some direction ->
                    Ok { Point = midpoint
                         Rotation = Trig.atan2Degrees direction.Y direction.X + 90.0<degree> }
                | None, None -> Error IndeterminateDirection))

    let private scaledFontSize baseSize scale = max 1.0<length> (baseSize * scale)

    let annotatedDrawingWith
        (graph: ArrangementGraph)
        (source: Path)
        (tolerance: float<length>)
        (options: AnnotatedDrawingOptions) =
        let scale = options.Scale
        let nodeRadius = 5.0<length> * scale
        let rec edgeThings reversed = function
            | [] -> Ok(List.rev reversed |> List.concat)
            | edge :: rest ->
                WindingField.segmentSideNonzeroLevels edge.Segment source (tolerance * 16.0) WindingField.defaultOptions
                |> Result.mapError ArrangementSegmentError
                |> Result.bind (fun (leftWinding, rightWinding) ->
                    edgeAnnotationPose edge
                    |> Result.mapError ArrangementSegmentError
                    |> Result.bind (fun pose ->
                        let arrow =
                            segmentDirectionArrowWith edge.Segment "#dc2626"
                                (2.0 * float nodeRadius / 9.0) (1.6 * float nodeRadius / 3.5) nodeRadius 1.0
                            |> Result.defaultValue (StyledPath(Path.ofSubpaths [], ""))
                        let chord = Point.distance (Segment.start edge.Segment) (Segment.finish edge.Segment)
                        let usableChord = chord - 2.0 * nodeRadius
                        let labelScale =
                            if usableChord <= 0.0<length> then 0.0
                            else min (1.2 * scale) (float (usableChord * 0.8 / 24.0<length>))
                        let labels =
                            if labelScale <= 0.0 then []
                            else
                                let width, height = 34.0<length> * labelScale, 24.0<length> * labelScale
                                [ RotatedRectangle(Point.create (pose.Point.X - width / 2.0) (pose.Point.Y - height / 2.0), width, height, $"fill: #fff; stroke: #94a3b8; stroke-width: {0.75 * scale}", pose.Rotation, pose.Point)
                                  RotatedText($"{leftWinding}/{rightWinding}", "fill: #0f172a; font-family: ui-monospace, monospace; font-weight: 700; text-anchor: middle", Point.create pose.Point.X (pose.Point.Y - 2.0<length> * labelScale), scaledFontSize 9.0<length> labelScale, pose.Rotation, pose.Point)
                                  RotatedText($"↑{edge.ForwardMultiplicity}/{edge.ReverseMultiplicity}↓", "fill: #dc2626; font-family: ui-monospace, monospace; font-weight: 700; text-anchor: middle", Point.create pose.Point.X (pose.Point.Y + 9.0<length> * labelScale), scaledFontSize 8.0<length> labelScale, pose.Rotation, pose.Point) ]
                        let things =
                            StyledPath(singleSegmentPath edge.Segment, $"fill: none; stroke: #334155; stroke-width: {3.25 * scale}")
                            :: arrow :: labels
                        edgeThings (things :: reversed) rest))
        edgeThings [] graph.Edges
        |> Result.map (fun edges ->
            edges @ (graph.Vertices |> List.map (fun vertex -> Circle(vertex.Point, nodeRadius, $"fill: #fff; stroke: #dc2626; stroke-width: {2.25 * scale}"))))

    let annotatedDrawing graph source tolerance =
        annotatedDrawingWith graph source tolerance defaultAnnotatedDrawingOptions
