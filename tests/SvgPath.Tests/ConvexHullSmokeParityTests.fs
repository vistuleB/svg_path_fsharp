module SvgPath.Tests.ConvexHullSmokeParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private supportValue segments angle =
    segments
    |> List.map (fun segment ->
        ConvexHull.internalSegmentSupport segment (Degree.fromFloat angle)
        |> Result.defaultWith (failwithf "%A")
        |> fun (_, _, value) -> value)
    |> List.max

let private supportValuesMatch original (hull: Subpath) =
    for angle in [ 0.0; 45.0; 90.0; 135.0; 180.0; 225.0; 270.0; 315.0 ] do
        let expected = supportValue original angle
        let actual = supportValue hull.Segments angle
        Assert.True(abs (actual - expected) <= 1.0e-6<length>)

[<Fact>]
let ``segment hull returns closed subpath for line`` () =
    let segment = Line(point 0.0 0.0, point 10.0 0.0)
    let hull = ConvexHull.segmentHull segment |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.Equal(2, hull.Segments.Length)
    supportValuesMatch [ segment ] hull

[<Fact>]
let ``segment hull returns closed hull for quadratic`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 5.0 10.0, point 10.0 0.0)
    let hull = ConvexHull.segmentHull segment |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.Equal(2, hull.Segments.Length)
    supportValuesMatch [ segment ] hull

[<Fact>]
let ``subpath hull returns closed hull for l shaped polyline`` () =
    let segments =
        [ Line(point 0.0 0.0, point 20.0 0.0)
          Line(point 20.0 0.0, point 20.0 15.0) ]
    let source = Subpath.create segments |> Result.defaultWith (failwithf "%A")
    let hull = ConvexHull.subpathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.True(hull.Segments.Length >= 3)
    supportValuesMatch segments hull

[<Fact>]
let ``subpath hull treats empty subpath as single point`` () =
    let at = point 4.0 -3.0
    let hull = ConvexHull.subpathHull (Subpath.empty at) |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.Equal<Segment list>([ Line(at, at); Line(at, at) ], hull.Segments)

[<Fact>]
let ``path hull includes empty subpath start points`` () =
    let a, b, far = point 0.0 0.0, point 2.0 0.0, point 10.0 0.0
    let source = Path.ofSubpaths [ Subpath.ofSegment (Line(a, b)); Subpath.empty far ]
    let hull = ConvexHull.pathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.True(abs (supportValue hull.Segments 0.0 - 10.0<length>) <= 1.0e-6<length>)

[<Fact>]
let ``path hull rejects empty path`` () =
    Assert.Equal(Error(ConvexHullPathError EmptyPath), ConvexHull.pathHull Path.empty)
