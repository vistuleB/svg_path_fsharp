module SvgPath.Tests.WindingFieldTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private polygon points =
    Subpath.polygon points |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``clockwise SVG polygon has positive winding`` () =
    let square = polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
    let path = Path.singleton square
    Assert.Equal(Ok(Winding 1), Path.winding (point 1.0 1.0) path)
    Assert.Equal(Ok(Winding 0), Path.winding (point 3.0 1.0) path)
    Assert.Equal(Ok BoundaryWinding, Path.winding (point 2.0 1.0) path)

[<Fact>]
let ``subpath containment implicitly closes an open subpath`` () =
    let subpath =
        Subpath.create
            [ Line(point 0.0 0.0, point 2.0 0.0)
              Line(point 2.0 0.0, point 2.0 2.0)
              Line(point 2.0 2.0, point 0.0 2.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok Inside, Subpath.containment (point 1.0 1.0) subpath Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 3.0 1.0) subpath Nonzero)
    Assert.Equal(Ok Boundary, Subpath.containment (point 0.0 1.0) subpath Nonzero)

[<Fact>]
let ``subpath containment supports both fill rules`` () =
    let twice =
        Subpath.create
            [ Line(point 0.0 0.0, point 2.0 0.0)
              Line(point 2.0 0.0, point 2.0 2.0)
              Line(point 2.0 2.0, point 0.0 2.0)
              Line(point 0.0 2.0, point 0.0 0.0)
              Line(point 0.0 0.0, point 2.0 0.0)
              Line(point 2.0 0.0, point 2.0 2.0)
              Line(point 2.0 2.0, point 0.0 2.0)
              Line(point 0.0 2.0, point 0.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok Inside, Subpath.containment (point 1.0 1.0) twice Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 1.0 1.0) twice EvenOdd)

[<Fact>]
let ``subpath containment handles a ray through a vertex`` () =
    let triangle = polygon [ point 0.0 0.0; point 10.0 5.0; point 0.0 10.0 ]
    Assert.Equal(Ok Inside, Subpath.containment (point 2.0 5.0) triangle Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 12.0 5.0) triangle Nonzero)

[<Fact>]
let ``subpath containment handles curved boundaries`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let subpath = Subpath.create [ curve ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok Boundary, Subpath.containment (point 10.0 10.0) subpath Nonzero)
    Assert.Equal(Ok Inside, Subpath.containment (point 10.0 5.0) subpath Nonzero)

[<Fact>]
let ``subpath containment honors boundary tolerance`` () =
    let square = polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
    let options = { Path.defaultContainmentOptions with Tolerance = 0.001<length> }
    Assert.Equal(
        Ok Boundary,
        Subpath.containmentWith (point -0.0005 5.0) square Nonzero options)

[<Fact>]
let ``move-only subpath is outside after option validation`` () =
    let empty = Subpath.empty (point 5.0 5.0)
    Assert.Equal(Ok Outside, Subpath.containment (point 5.0 5.0) empty Nonzero)
    let invalid = { Path.defaultContainmentOptions with Samples = 0 }
    Assert.Equal(
        Error(InvalidContainmentSamples 0),
        Subpath.containmentWith (point 5.0 5.0) empty Nonzero invalid)

[<Fact>]
let ``path containment combines winding and parity once per subpath`` () =
    let outer = polygon [ point 0.0 0.0; point 20.0 0.0; point 20.0 20.0; point 0.0 20.0 ]
    let same = polygon [ point 5.0 5.0; point 15.0 5.0; point 15.0 15.0; point 5.0 15.0 ]
    let opposite = polygon [ point 5.0 5.0; point 5.0 15.0; point 15.0 15.0; point 15.0 5.0 ]
    let center = point 10.0 10.0
    Assert.Equal(Ok Inside, Path.containment center (Path.ofSubpaths [ outer; same ]) Nonzero)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; same ]) EvenOdd)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; opposite ]) Nonzero)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; opposite ]) EvenOdd)

[<Fact>]
let ``path winding accumulates oriented subpaths`` () =
    let outer = polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 4.0; point 0.0 4.0 ]
    let inner = polygon [ point 1.0 1.0; point 1.0 3.0; point 3.0 3.0; point 3.0 1.0 ]
    Assert.Equal(Ok(Winding 0), Path.winding (point 2.0 2.0) (Path.ofSubpaths [ outer; inner ]))

[<Fact>]
let ``side levels follow geometric left and right`` () =
    let top = Line(point 0.0 0.0, point 2.0 0.0)
    let square = polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
    Assert.Equal(
        Ok(0, 1),
        WindingField.segmentSideNonzeroLevels top (Path.singleton square) 0.001<length> Path.defaultContainmentOptions)

[<Fact>]
let ``side levels fall back from a midpoint cusp`` () =
    let cusp = CubicBezier(point -1.0 0.0, point 1.0 1.0, point -1.0 1.0, point 1.0 0.0)
    let source =
        Subpath.create [ cusp; Line(point 1.0 0.0, point -1.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Ok(-1, 0),
        WindingField.segmentSideNonzeroLevels cusp (Path.singleton source) 0.0001<length> Path.defaultContainmentOptions)

[<Fact>]
let ``side levels reject nonpositive sampling distance`` () =
    let line = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(
        Error(InvalidContainmentTolerance 0.0<length>),
        WindingField.segmentSideNonzeroLevels line Path.empty 0.0<length> Path.defaultContainmentOptions)
    Assert.Equal(
        Error(InvalidContainmentTolerance -0.001<length>),
        WindingField.segmentSideNonzeroLevels line Path.empty -0.001<length> Path.defaultContainmentOptions)

[<Fact>]
let ``side levels reject a segment without a regular sample`` () =
    let collapsed = Line(point 1.0 2.0, point 1.0 2.0)
    Assert.Equal(
        Error IndeterminateWindingSideLevels,
        WindingField.segmentSideNonzeroLevels collapsed Path.empty 0.0001<length> Path.defaultContainmentOptions)

[<Fact>]
let ``side levels validate options before degenerate fallback`` () =
    let collapsed = Line(point 1.0 2.0, point 1.0 2.0)
    let invalid = { Path.defaultContainmentOptions with Samples = 0 }
    Assert.Equal(
        Error(InvalidContainmentSamples 0),
        WindingField.segmentSideNonzeroLevels collapsed Path.empty 0.0001<length> invalid)
