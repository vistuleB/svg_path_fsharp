namespace SvgPath.Tests

open Xunit
open SvgPath

module ArrangementTests =
    let point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
    let line ax ay bx by = Line(point ax ay, point bx by)

    let rectangle x y width height =
        let segments =
            [ line x y (x + width) y
              line (x + width) y (x + width) (y + height)
              line (x + width) (y + height) x (y + height)
              line x (y + height) x y ]
        Subpath.create segments
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (fun error -> failwithf "%A" error)

    let private graphWithEdges segments =
        segments
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun graph -> Arrangement.insertAtomicSegment graph segment 1.0e-9<length> 1.0e-12<length>)) (Ok Arrangement.empty)

    [<Fact>]
    let ``atomic insertion clusters endpoints and consolidates reversal`` () =
        let result =
            graphWithEdges
                [ line 0.0 0.0 1.0 0.0
                  line 1.0 0.0 0.0 0.0 ]
        match result with
        | Error error -> failwithf "%A" error
        | Ok graph ->
            Assert.Equal(2, graph.Vertices.Length)
            Assert.Single(graph.Edges) |> ignore
            Assert.Equal(1, graph.Edges.Head.ForwardMultiplicity)
            Assert.Equal(1, graph.Edges.Head.ReverseMultiplicity)

    [<Fact>]
    let ``forced parity reduces the uniquely largest incident capacity`` () =
        let graph =
            graphWithEdges
                [ line 0.0 0.0 1.0 0.0
                  line 0.0 0.0 1.0 0.0
                  line 0.0 0.0 0.0 1.0 ]
            |> Result.defaultWith (fun error -> failwithf "%A" error)
        let result = Arrangement.forcedParityCapacities graph []
        match result with
        | Error error -> failwithf "%A" error
        | Ok capacities ->
            let horizontal = capacities |> List.find (fun assignment -> assignment.EdgeId = 0)
            Assert.Equal(0, horizontal.Capacity)

    [<Fact>]
    let ``build nodes a transverse crossing symmetrically`` () =
        match Arrangement.buildWith [ line -1.0 0.0 1.0 0.0; line 0.0 -1.0 0.0 1.0 ] 1.0e-8<length> 1.0e-10<length> 0.0<parameter> with
        | Error error -> failwithf "%A" error
        | Ok build ->
            Assert.Equal(5, build.Graph.Vertices.Length)
            Assert.Equal(4, build.Graph.Edges.Length)
            Assert.Equal(2, build.SegmentImages[0].Edges.Length)
            Assert.Equal(2, build.SegmentImages[1].Edges.Length)

    [<Fact>]
    let ``build consolidates coincident reversed lines`` () =
        match Arrangement.buildWith [ line 0.0 0.0 2.0 0.0; line 2.0 0.0 0.0 0.0 ] 1.0e-8<length> 1.0e-10<length> 0.0<parameter> with
        | Error error -> failwithf "%A" error
        | Ok build ->
            Assert.Single(build.Graph.Edges) |> ignore
            Assert.Equal(1, build.Graph.Edges.Head.ForwardMultiplicity)
            Assert.Equal(1, build.Graph.Edges.Head.ReverseMultiplicity)

    [<Fact>]
    let ``build nodes an incoming endpoint on an existing edge`` () =
        match Arrangement.buildWith [ line 0.0 0.0 2.0 0.0; line 1.0 0.0 1.0 1.0 ] 1.0e-8<length> 1.0e-10<length> 0.0<parameter> with
        | Error error -> failwithf "%A" error
        | Ok build ->
            Assert.Equal(4, build.Graph.Vertices.Length)
            Assert.Equal(3, build.Graph.Edges.Length)
            Assert.Equal(2, build.SegmentImages[0].Edges.Length)

    [<Fact>]
    let ``build nodes partial overlap boundaries`` () =
        match Arrangement.buildWith [ line 0.0 0.0 3.0 0.0; line 1.0 0.0 2.0 0.0 ] 1.0e-8<length> 1.0e-10<length> 0.0<parameter> with
        | Error error -> failwithf "%A" error
        | Ok build ->
            Assert.Equal(3, build.Graph.Edges.Length)
            let shared = build.Graph.Edges |> List.find (fun edge -> edge.ForwardMultiplicity + edge.ReverseMultiplicity = 2)
            let owners =
                build.SegmentImages
                |> List.collect _.Edges
                |> List.filter (fun image -> image.EdgeId = shared.Id && image.Own)
            Assert.Single(owners) |> ignore

    [<Fact>]
    let ``required parity rejects an isolated mismatch`` () =
        let graph = graphWithEdges [ line 0.0 0.0 1.0 0.0 ] |> Result.defaultWith (fun error -> failwithf "%A" error)
        let capacities = graph.Edges |> List.map (fun edge -> { EdgeId = edge.Id; Capacity = 0 })
        match Arrangement.forcedParityCapacitiesWith graph capacities [ RequiredVertexParity(0, 1) ] with
        | Error(ForcedParityInfeasible 0) -> ()
        | other -> failwithf "unexpected result: %A" other

    [<Fact>]
    let ``validation enforces closed-boundary parity`` () =
        let graph = graphWithEdges [ line 0.0 0.0 1.0 0.0 ] |> Result.defaultWith (fun error -> failwithf "%A" error)
        match Arrangement.validate graph 1.0e-9<length> 1.0e-12<length> with
        | Error(OddWeightedDegree(0, 1)) -> ()
        | other -> failwithf "unexpected result: %A" other

    [<Fact>]
    let ``dual square has infinite and bounded faces`` () =
        let path = Path.ofSubpaths [ rectangle 0.0 0.0 10.0 10.0 ]
        match Arrangement.build [ path ] 1.0e-6<length> 1.0e-5<length> |> Result.bind (fun build -> Arrangement.dual build.Graph) with
        | Error error -> failwithf "%A" error
        | Ok dual ->
            Assert.Equal(2, dual.Faces.Length)
            Assert.True(dual.Faces[0].Outer)
            Assert.False(dual.Faces[1].Outer)
            Assert.Equal(4, dual.EdgeFaces.Length)

    [<Fact>]
    let ``dual bounded face collects two island walks`` () =
        let path =
            Path.ofSubpaths
                [ rectangle 0.0 0.0 30.0 20.0
                  rectangle 5.0 5.0 5.0 5.0
                  rectangle 20.0 5.0 5.0 5.0 ]
        match Arrangement.build [ path ] 1.0e-6<length> 1.0e-5<length> |> Result.bind (fun build -> Arrangement.dual build.Graph) with
        | Error error -> failwithf "%A" error
        | Ok dual ->
            let face = dual.Faces |> List.find (fun face -> not face.Outer && face.Walks.Length = 3)
            Assert.True(face.Walks.Head.Outer)
            Assert.False(face.Walks[1].Outer)
            Assert.False(face.Walks[2].Outer)

    [<Fact>]
    let ``nested contours reconstruct a single square`` () =
        let square = rectangle 0.0 0.0 10.0 10.0
        let path = Path.ofSubpaths [ square ]
        match Arrangement.build [ path ] 1.0e-6<length> 1.0e-5<length>
              |> Result.bind (fun build -> Arrangement.nestedContoursFromGraph build.Graph path 1.0e-6<length>) with
        | Error error -> failwithf "%A" error
        | Ok contours ->
            Assert.Single(contours) |> ignore
            Assert.True(contours.Head.Closed)
            Assert.Equal(4, contours.Head.Segments.Length)

    [<Fact>]
    let ``nested contours preserve concentric winding layers`` () =
        let outer = rectangle 0.0 0.0 10.0 10.0
        let inner = rectangle 2.0 2.0 6.0 6.0
        let path = Path.ofSubpaths [ outer; inner ]
        match Arrangement.build [ path ] 1.0e-6<length> 1.0e-5<length>
              |> Result.bind (fun build -> Arrangement.nestedContoursFromGraph build.Graph path 1.0e-6<length>) with
        | Error error -> failwithf "%A" error
        | Ok contours ->
            Assert.Equal(2, contours.Length)
            Assert.All(contours, fun contour -> Assert.True(contour.Closed))

    let private drawingEdge segment =
        { Id = 0
          Segment = segment
          Bounds = Segment.boundingBox segment |> Result.defaultWith (failwithf "%A")
          StartVertex = 0
          EndVertex = 1
          ForwardMultiplicity = 1
          ReverseMultiplicity = 0 }

    [<Fact>]
    let ``edge annotation pose follows midpoint tangent`` () =
        let pose = ArrangementDrawing.edgeAnnotationPose (drawingEdge (line 0.0 0.0 10.0 0.0))
        Assert.Equal(Ok { Point = point 5.0 0.0; Rotation = 90.0<degree> }, pose)

    [<Fact>]
    let ``edge annotation pose uses incoming direction at stationary reversal`` () =
        let segment = QuadraticBezier(point 1.0 0.0, point -1.0 0.0, point 1.0 0.0)
        let pose = ArrangementDrawing.edgeAnnotationPose (drawingEdge segment)
        Assert.Equal(Ok { Point = point 0.0 0.0; Rotation = 270.0<degree> }, pose)

    [<Fact>]
    let ``edge annotation pose rejects directionless geometry`` () =
        let p = point 1.0 2.0
        Assert.Equal(Error IndeterminateDirection, ArrangementDrawing.edgeAnnotationPose (drawingEdge (Line(p, p))))

    [<Fact>]
    let ``direction arrow recovers a collapsed cubic endpoint direction`` () =
        let finish = point 10.0 10.0
        let segment = CubicBezier(point 0.0 0.0, point 0.0 10.0, finish, finish)
        Assert.True(ArrangementDrawing.segmentDirectionArrow segment "red" |> Result.isOk)
