namespace SvgPath

/// SVG diagnostics for arrangement graphs and source images.
[<RequireQualifiedAccess>]
module ArrangementDrawing =

    [<Struct>]
    type EdgeAnnotationPose =
        { Point: Point<length>
          Rotation: float<degree> }

    [<Struct>]
    type AnnotatedDrawingOptions = { Scale: float }

    let defaultAnnotatedDrawingOptions = { Scale = 1.0 }

    let private singleSegmentPath segment =
        Path.ofSubpaths [ Subpath.create [ segment ] |> Result.defaultWith (failwithf "%A") ]

    let drawing (graph: Arrangement.ArrangementGraph) =
        let edgeThings =
            graph.Edges
            |> List.collect (fun edge ->
                let midpoint = Segment.point edge.Segment 0.5<parameter> |> Result.defaultValue (Segment.start edge.Segment)
                [ Svg.StyledPath(singleSegmentPath edge.Segment, "fill: none; stroke: #334155; stroke-width: 1.5")
                  Svg.Rectangle(Point.create (midpoint.X - 11.0<length>) (midpoint.Y - 7.0<length>), 22.0<length>, 14.0<length>, "fill: white; stroke: #94a3b8; stroke-width: 0.75")
                  Svg.Text($"{edge.ForwardMultiplicity}/{edge.ReverseMultiplicity}", "fill: #0f172a; font-family: monospace; text-anchor: middle; dominant-baseline: central", Point.create midpoint.X (midpoint.Y + 0.5<length>), 8.0<length>) ])
        let vertexThings =
            graph.Vertices
            |> List.collect (fun vertex -> Svg.labeledPoint $"v{vertex.Id}" "#dc2626" vertex.Point 8.0<length>)
        edgeThings @ vertexThings

    let internal edgeAnnotationPose (edge: Arrangement.ArrangementEdge) =
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
        (graph: Arrangement.ArrangementGraph)
        (source: Path)
        (tolerance: float<length>)
        (options: AnnotatedDrawingOptions) =
        let scale = options.Scale
        let nodeRadius = 5.0<length> * scale
        let rec edgeThings reversed (edges: Arrangement.ArrangementEdge list) =
            match edges with
            | [] -> Ok(List.rev reversed |> List.concat)
            | edge :: rest ->
                // The containment boundary tolerance must match the probe scale.
                WindingField.segmentSideNonzeroLevels edge.Segment source (tolerance * 16.0) {WindingField.defaultOptions with Tolerance=tolerance}
                |> Result.mapError Arrangement.ArrangementSegmentError
                |> Result.bind (fun (leftWinding, rightWinding) ->
                    edgeAnnotationPose edge
                    |> Result.mapError Arrangement.ArrangementSegmentError
                    |> Result.bind (fun pose ->
                        let arrow =
                            Svg.segmentDirectionArrowWith edge.Segment "#dc2626"
                                (2.0 * float nodeRadius / 9.0) (1.6 * float nodeRadius / 3.5) nodeRadius 1.0
                            |> Result.defaultValue (Svg.StyledPath(Path.ofSubpaths [], ""))
                        let chord = Point.distance (Segment.start edge.Segment) (Segment.finish edge.Segment)
                        let usableChord = chord - 2.0 * nodeRadius
                        let labelScale =
                            if usableChord <= 0.0<length> then 0.0
                            else min (1.2 * scale) (float (usableChord * 0.8 / 24.0<length>))
                        let labels =
                            if labelScale <= 0.0 then []
                            else
                                let width, height = 34.0<length> * labelScale, 24.0<length> * labelScale
                                [ Svg.RotatedRectangle(Point.create (pose.Point.X - width / 2.0) (pose.Point.Y - height / 2.0), width, height, $"fill: #fff; stroke: #94a3b8; stroke-width: {0.75 * scale}", pose.Rotation, pose.Point)
                                  Svg.RotatedText($"{leftWinding}/{rightWinding}", "fill: #0f172a; font-family: ui-monospace, monospace; font-weight: 700; text-anchor: middle", Point.create pose.Point.X (pose.Point.Y - 2.0<length> * labelScale), scaledFontSize 9.0<length> labelScale, pose.Rotation, pose.Point)
                                  Svg.RotatedText($"↑{edge.ForwardMultiplicity}/{edge.ReverseMultiplicity}↓", "fill: #dc2626; font-family: ui-monospace, monospace; font-weight: 700; text-anchor: middle", Point.create pose.Point.X (pose.Point.Y + 9.0<length> * labelScale), scaledFontSize 8.0<length> labelScale, pose.Rotation, pose.Point) ]
                        let things =
                            Svg.StyledPath(singleSegmentPath edge.Segment, $"fill: none; stroke: #334155; stroke-width: {3.25 * scale}")
                            :: arrow :: labels
                        edgeThings (things :: reversed) rest))
        edgeThings [] graph.Edges
        |> Result.map (fun edges ->
            edges @ (graph.Vertices |> List.map (fun vertex -> Svg.Circle(vertex.Point, nodeRadius, $"fill: #fff; stroke: #dc2626; stroke-width: {2.25 * scale}"))))

    let annotatedDrawing graph source tolerance =
        annotatedDrawingWith graph source tolerance defaultAnnotatedDrawingOptions
