module SvgPath.Tests.InspectTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private subpath segments = Subpath.create segments |> Result.defaultWith (failwithf "%A")
let private closedSubpath segments = subpath segments |> Subpath.setClosedWith Bridge true |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``point inspects as comma separated coordinates`` () =
    Assert.Equal("10,-2.5", Inspect.point (point 10.0 -2.5))

[<Fact>]
let ``point inspection preserves scientific exponents`` () =
    Assert.Equal("1e20,0", Inspect.point (point 1.0e20 0.0))

[<Fact>]
let ``point padding measures scientific significands`` () =
    let options = Inspect.defaultOptions () |> Inspect.withLeftPadding (LeftPadding(4, Zero))
    Assert.Equal("0001e20,0002", Inspect.pointWith (point 1.0e20 2.0) options)

[<Fact>]
let ``point code emits valid float scientific notation`` () =
    Assert.Equal("Point.create (1.0e20<length>) (-1.0e-20<length>)", Inspect.pointCode (point 1.0e20 -1.0e-20))

[<Fact>]
let ``point inspects with decimal options`` () =
    Assert.Equal("10.23,-2.24", Inspect.pointWith (point 10.234 -2.235) (Inspect.decimalOptions 2))

[<Fact>]
let ``point inspects with fixed decimal options`` () =
    Assert.Equal("10.00,-2.50", Inspect.pointWith (point 10.0 -2.5) (Inspect.fixedDecimalOptions 2))

[<Fact>]
let ``decimal options use scientific notation when scaling is unsafe`` () =
    Assert.Equal("1e20,-1e20", Inspect.pointWith (point 1.0e20 -1.0e20) (Inspect.decimalOptions 5))

[<Fact>]
let ``fixed decimal options fix unsafe scientific significands`` () =
    Assert.Equal("1.00e20,-1.00e15", Inspect.pointWith (point 1.0e20 -1.0e15) (Inspect.fixedDecimalOptions 2))

[<Fact>]
let ``line segment inspects on one line`` () =
    Assert.Equal("Line(start=0,0 end=12,10)", Inspect.segment (Line(point 0.0 0.0, point 12.0 10.0)))

[<Fact>]
let ``segment inspects with decimal options`` () =
    let segment = Line(point 0.0 0.0, point 12.234 10.235)
    Assert.Equal("Line(start=0.0,0.0 end=12.2,10.2)", Inspect.segmentWith segment (Inspect.fixedDecimalOptions 1))

[<Fact>]
let ``segment inspects with auto left padding`` () =
    let options = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (AutoLeftPadding Zero)
    Assert.Equal("Line(start=000.0,-05.0 end=120.0,010.0)", Inspect.segmentWith (Line(point 0.0 -5.0, point 120.0 10.0)) options)

[<Fact>]
let ``point inspects with explicit left padding`` () =
    let options = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (LeftPadding(4, Zero))
    Assert.Equal("0002.0,-003.0", Inspect.pointWith (point 2.0 -3.0) options)

[<Fact>]
let ``point inspects with space left padding`` () =
    let options = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (LeftPadding(4, Space))
    Assert.Equal("   2.0,  -3.0", Inspect.pointWith (point 2.0 -3.0) options)

[<Fact>]
let ``curve and arc segments inspect named fields`` () =
    let quadratic = QuadraticBezier(point 0.0 0.0, point 5.0 10.0, point 12.0 10.0)
    let cubic = CubicBezier(point 0.0 0.0, point 2.0 4.0, point 6.0 8.0, point 10.0 12.0)
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = point 20.0 0.0 }
    Assert.Equal("QuadraticBezier(start=0,0 control=5,10 end=12,10)", Inspect.segment quadratic)
    Assert.Equal("CubicBezier(start=0,0 control1=2,4 control2=6,8 end=10,12)", Inspect.segment cubic)
    Assert.Equal("Arc(start=0,0 radius=5,8 x_axis_rotation=45 large_arc=true sweep=false end=20,0)", Inspect.segment arc)

[<Fact>]
let ``empty path and subpath inspect compactly`` () =
    Assert.Equal("Path([])", Inspect.path Path.empty)
    Assert.Equal("Subpath(open, start=0,0, [])", Inspect.subpath (Subpath.empty (point 0.0 0.0)))

[<Fact>]
let ``path inspects subpaths and segments with indentation`` () =
    let path = closedSubpath [ Line(point 0.0 0.0, point 12.0 10.0); Line(point 12.0 10.0, point 20.0 10.0) ] |> Path.singleton
    Assert.Equal("Path([\n  Subpath(closed, start=0,0, [\n    Line(start=0,0 end=12,10),\n    Line(start=12,10 end=20,10),\n    Line(start=20,10 end=0,0)\n  ])\n])", Inspect.path path)

[<Fact>]
let ``path inspects with decimal options`` () =
    let path = subpath [ Line(point 0.0 0.0, point 12.234 10.235) ] |> Path.singleton
    Assert.Equal("Path([\n  Subpath(open, start=0,0, [\n    Line(start=0,0 end=12.2,10.2)\n  ])\n])", Inspect.pathWith path (Inspect.decimalOptions 1))

[<Fact>]
let ``point code inspects as copy pasteable F sharp`` () =
    Assert.Equal("Point.create (10.0<length>) (-2.5<length>)", Inspect.pointCode (point 10.0 -2.5))

[<Fact>]
let ``segment code inspects as copy pasteable F sharp`` () =
    let segment = CubicBezier(point 0.0 0.0, point 2.0 4.0, point 6.0 8.0, point 10.0 12.0)
    Assert.Equal("CubicBezier(Point.create (0.0<length>) (0.0<length>), Point.create (2.0<length>) (4.0<length>), Point.create (6.0<length>) (8.0<length>), Point.create (10.0<length>) (12.0<length>))", Inspect.segmentCode segment)

[<Fact>]
let ``subpath code inspects as copy pasteable F sharp`` () =
    let value = subpath [ Line(point 0.0 0.0, point 12.0 10.0) ]
    Assert.Equal("Subpath.create [\n  Line(Point.create (0.0<length>) (0.0<length>), Point.create (12.0<length>) (10.0<length>))\n]\n|> Result.defaultWith (failwithf \"%A\")", Inspect.subpathCode value)

[<Fact>]
let ``closed subpath code inspects as copy pasteable F sharp`` () =
    let value = closedSubpath [ Line(point 0.0 0.0, point 12.0 10.0) ]
    Assert.Equal("Subpath.create [\n  Line(Point.create (0.0<length>) (0.0<length>), Point.create (12.0<length>) (10.0<length>));\n  Line(Point.create (12.0<length>) (10.0<length>), Point.create (0.0<length>) (0.0<length>))\n]\n|> Result.defaultWith (failwithf \"%A\")\n|> Subpath.setClosed true\n|> Result.defaultWith (failwithf \"%A\")", Inspect.subpathCode value)

[<Fact>]
let ``path code inspects as copy pasteable F sharp`` () =
    let value = subpath [ Line(point 0.0 0.0, point 12.0 10.0) ] |> Path.singleton
    Assert.Equal("Path.ofSubpaths [\n  Subpath.create [\n    Line(Point.create (0.0<length>) (0.0<length>), Point.create (12.0<length>) (10.0<length>))\n  ]\n  |> Result.defaultWith (failwithf \"%A\")\n]", Inspect.pathCode value)

[<Fact>]
let ``code inspection respects decimal options`` () =
    let segment = Line(point 0.0 0.0, point 12.234 10.235)
    Assert.Equal("Line(Point.create (0.0<length>) (0.0<length>), Point.create (12.2<length>) (10.2<length>))", Inspect.segmentCodeWith segment (Inspect.decimalOptions 1))

[<Fact>]
let ``code inspection respects auto left padding`` () =
    let value = subpath [ Line(point 0.0 -5.0, point 120.0 10.0); Line(point 120.0 10.0, point 2.0 -30.0) ] |> Path.singleton
    let options = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (AutoLeftPadding Zero)
    Assert.Equal("Path.ofSubpaths [\n  Subpath.create [\n    Line(Point.create (000.0<length>) (-05.0<length>), Point.create (120.0<length>) (010.0<length>));\n    Line(Point.create (120.0<length>) (010.0<length>), Point.create (002.0<length>) (-30.0<length>))\n  ]\n  |> Result.defaultWith (failwithf \"%A\")\n]", Inspect.pathCodeWith value options)
