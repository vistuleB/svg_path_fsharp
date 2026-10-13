namespace SvgPath.Tests

open Xunit
open SvgPath

module Release301Tests =
    let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
    let private unwrap value = value |> Result.defaultWith (failwithf "%A")
    let private options =
        { Containment.defaultCrossingOptions with
            Samples = 100; SignedLineDistanceTolerance = 1e-12<length>; MaxIterations = 100 }
    let private curves =
        let a, b = point 150.0 160.0, point 150.0 80.0
        [ CubicBezier(a,a,b,b); CubicBezier(b,b,a,a)
          QuadraticBezier(a,b,b); QuadraticBezier(b,b,a)
          CubicBezier(a,point 170.0 160.0,point 170.0 80.0,b) ]
    let private crossings curve y =
        Containment.segmentRayCrossingsWith curve (point 0.0 y) (Point.create 1.0 0.0) options

    [<Fact>]
    let ``near endpoint inside recovers valid root`` () =
        for curve in curves do
            let y = 80.00000000006969
            let t, _ = crossings curve y |> unwrap |> List.exactlyOne
            Assert.True(t > 0.0<parameter> && t < 1.0<parameter>)
            let p = Segment.point curve t |> unwrap
            Assert.True(abs (float p.Y - y) <= 1e-12)

    [<Fact>]
    let ``near endpoint outside discards spurious root`` () =
        for curve in curves do
            Assert.True(crossings curve 79.99999999993031 = Ok [])

    [<Fact>]
    let ``exact endpoint contact is preserved`` () =
        for curve in curves do
            let t, _ = crossings curve 80.0 |> unwrap |> List.exactlyOne
            let p = Segment.point curve t |> unwrap
            Assert.True(abs (float p.Y - 80.0) <= 1e-12)
            Assert.True(float t < 1e-6 || float t > 0.999999)

    let private tangent = QuadraticBezier(point 0.0 1.0,point 1.0 -1.0,point 2.0 1.0)

    [<Fact>]
    let ``interior tangent is preserved`` () =
        let t, _ = crossings tangent 0.0 |> unwrap |> List.exactlyOne
        Assert.True(abs (float t - 0.5) < 1e-9)

    [<Fact>]
    let ``nonmonotone projection retains both crossings`` () =
        let ts = crossings tangent 0.25 |> unwrap |> List.map (fst >> float)
        Assert.Equal<float list>([0.25; 0.75], ts)

    [<Fact>]
    let ``endpoint recovery respects iteration limit`` () =
        let result = Containment.segmentRayCrossingsWith curves.Head (point 0.0 80.00000000006969)
                        (Point.create 1.0 0.0) {options with MaxIterations=1}
        match result with
        | Error(CrossingMaxIterationsReached _) -> ()
        | other -> failwithf "Expected iteration limit, got %A" other

    let private rectangle left right =
        let a,b,c,d = point left 0.0,point right 0.0,point right 10.0,point left 10.0
        [Line(a,b);Line(b,c);Line(c,d);Line(d,a)]
        |> Subpath.create |> Result.bind Subpath.close |> unwrap
        |> fun subpath -> Path.ofSubpaths [subpath]
    let private paths = [rectangle 0.0 10.0; rectangle 10.1875 20.0]
    let private build () = Arrangement.build paths 0.125<length> 1e-5<length> |> unwrap

    [<Fact>]
    let ``arrangement accepts cluster diameter larger than tolerance`` () =
        let graph = (build ()).Graph
        Arrangement.validateRepresentation graph 0.125<length> 1e-5<length> |> unwrap
        for y in [0.0; 10.0] do
            let a,b = point 10.0 y,point 10.1875 y
            let vertex = graph.Vertices |> List.find (fun v -> List.contains a v.EndpointSamples && List.contains b v.EndpointSamples)
            Assert.Equal(point 10.09375 y, vertex.Point)
            Assert.True(Point.distance a vertex.Point < 0.125<length>)
            Assert.True(Point.distance b vertex.Point < 0.125<length>)
            Assert.True(Point.distance a b > 0.125<length>)

    [<Fact>]
    let ``CSG joins endpoints through shared vertex without modifying graph`` () =
        let output = Csg.unionWith paths[0] paths[1] Nonzero {Tolerance=0.125<length>;MinimumLength=1e-5<length>} |> unwrap
        Assert.True(output.Build.Graph = (build ()).Graph)
        let subpath = Path.subpaths output.Path |> List.exactlyOne
        Assert.True(Subpath.isClosed subpath)
        let segments = Subpath.segments subpath
        let vertices = output.Build.Graph.Vertices |> List.map _.Point
        for segment in segments do
            Assert.Contains(Segment.start segment, vertices)
            Assert.Contains(Segment.finish segment, vertices)
        for a,b in List.pairwise (segments @ [segments.Head]) do
            Assert.Equal(Segment.finish a, Segment.start b)
        Assert.True(abs (abs (float (Area.signedPath output.Path)) - 200.0) < 1e-9)
