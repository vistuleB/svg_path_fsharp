module SvgPath.Tests.HullLoopPieceTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private polygon points = Subpath.polygon points |> get
let private polyline points = Subpath.polyline points |> get
let private assertPolygon (hull: Subpath) expected =
    Assert.True(hull.Closed)
    let segments = hull.Segments
    Assert.True(segments.Length >= List.length expected)
    let vertices = List.map Segment.start segments
    for vertex in expected do Assert.Contains(vertex, vertices)
    let next = List.tail segments @ [List.head segments]
    for edge, following in List.zip segments next do
        Assert.Equal(Segment.finish edge, Segment.start following)
        match edge with Line _ -> () | _ -> failwith "Expected line"
    let expectedEdges = (polygon expected).Segments
    for vertex in vertices do
        Assert.True(expectedEdges |> List.exists (fun edge ->
            let a, b = Segment.start edge, Segment.finish edge
            let ab, av = Point.displacement a b, Point.displacement a vertex
            let cross = ab.X * av.Y - ab.Y * av.X
            let projection = Point.dot ab av
            cross = 0.0<length^2> && projection >= 0.0<length^2> && projection <= Point.dot ab ab))
    let perimeter edges = edges |> List.sumBy (fun edge -> Point.distance (Segment.start edge) (Segment.finish edge))
    Assert.True(abs(perimeter segments - perimeter expectedEdges) < 1e-9<length>)
let private triangle = [p 0. 0.; p 3. 0.; p 2. 1.]
[<Fact>]
let ``vertex contribution does not expand into full loop`` () =
    let hull = ConvexHull.subpath (polyline [p 0. 0.; p 1. 0.; p 2. 1.; p 3. 0.]) |> get
    Assert.Equal(3, hull.Segments.Length)
    assertPolygon hull triangle
[<Fact>]
let ``reversed input preserves triangle without extra circuit`` () =
    ConvexHull.subpath (polyline [p 3. 0.; p 2. 1.; p 1. 0.; p 0. 0.]) |> get |> fun hull -> assertPolygon hull triangle
[<Fact>]
let ``closure address aliases preserve triangle`` () =
    let vertices = [p 0. 0.; p 1. 0.; p 2. 1.; p 3. 0.]
    for index in [0;1;2;3] do
        let rotated = List.skip index vertices @ List.take index vertices
        ConvexHull.subpath (polygon rotated) |> get |> fun hull -> assertPolygon hull triangle
[<Fact>]
let ``full hull is retained in either union operand`` () =
    let vertices = [p 0. 0.; p 4. 0.; p 4. 4.; p 0. 4.]
    let square = polygon vertices
    let interior = polygon [p 1. 1.; p 2. 1.; p 1. 2.]
    for subpaths in [[square;interior]; [interior;square]; [square;square]] do
        ConvexHull.path (Path.ofSubpaths subpaths) |> get |> fun hull -> assertPolygon hull vertices
[<Fact>]
let ``narrow triangle vertex contribution preserves extent`` () =
    ConvexHull.subpath (polyline [p 0. 0.; p 1. 0.; p 2. 1e-9; p 3. 0.]) |> get
    |> fun hull -> assertPolygon hull [p 0. 0.; p 3. 0.; p 2. 1e-9]
