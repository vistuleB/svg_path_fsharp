module SvgPath.Tests.InspectTests

open SvgPath
open Xunit

module InspectTests =
    let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

    [<Fact>]
    let ``point inspection uses comma separated coordinates`` () =
        Assert.Equal("10,-2.5", Inspect.point (point 10.0 -2.5))
        Assert.Equal("1e20,0", Inspect.point (point 1.0e20 0.0))

    [<Fact>]
    let ``point inspection respects decimal options`` () =
        Assert.Equal("10.23,-2.24", Inspect.pointWith (point 10.234 -2.235) (Inspect.decimalOptions 2))
        Assert.Equal("10.00,-2.50", Inspect.pointWith (point 10.0 -2.5) (Inspect.fixedDecimalOptions 2))

    [<Fact>]
    let ``point inspection supports explicit and automatic padding`` () =
        let explicit = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (LeftPadding(4, Zero))
        Assert.Equal("0002.0,-003.0", Inspect.pointWith (point 2.0 -3.0) explicit)
        let automatic = Inspect.fixedDecimalOptions 1 |> Inspect.withLeftPadding (AutoLeftPadding Zero)
        Assert.Equal(
            "Line(start=000.0,-05.0 end=120.0,010.0)",
            Inspect.segmentWith (Line(point 0.0 -5.0, point 120.0 10.0)) automatic)

    [<Fact>]
    let ``segments inspect named fields`` () =
        Assert.Equal("Line(start=0,0 end=12,10)", Inspect.segment (Line(point 0.0 0.0, point 12.0 10.0)))
        Assert.Equal(
            "QuadraticBezier(start=0,0 control=5,10 end=12,10)",
            Inspect.segment (QuadraticBezier(point 0.0 0.0, point 5.0 10.0, point 12.0 10.0)))
        Assert.Equal(
            "CubicBezier(start=0,0 control1=2,4 control2=6,8 end=10,12)",
            Inspect.segment (CubicBezier(point 0.0 0.0, point 2.0 4.0, point 6.0 8.0, point 10.0 12.0)))

    [<Fact>]
    let ``arcs inspect named fields`` () =
        let arc =
            Arc
                { Start = point 0.0 0.0
                  Radius = point 5.0 8.0
                  XAxisRotation = 45.0<degree>
                  LargeArc = true
                  Sweep = false
                  End = point 20.0 0.0 }
        Assert.Equal(
            "Arc(start=0,0 radius=5,8 x_axis_rotation=45 large_arc=true sweep=false end=20,0)",
            Inspect.segment arc)

    [<Fact>]
    let ``empty path and subpath inspect compactly`` () =
        Assert.Equal("Path([])", Inspect.path Path.empty)
        Assert.Equal("Subpath(open, start=0,0, [])", Inspect.subpath (Subpath.empty (point 0.0 0.0)))

    [<Fact>]
    let ``paths inspect nested geometry with indentation`` () =
        let subpath =
            Subpath.create [ Line(point 0.0 0.0, point 12.0 10.0); Line(point 12.0 10.0, point 20.0 10.0) ]
            |> Result.defaultWith (failwithf "%A")
        let path = Path.singleton subpath
        Assert.Equal(
            "Path([\n  Subpath(open, start=0,0, [\n    Line(start=0,0 end=12,10),\n    Line(start=12,10 end=20,10)\n  ])\n])",
            Inspect.path path)

    [<Fact>]
    let ``code inspection emits copy pasteable F sharp expressions`` () =
        Assert.Equal("Point.create (10.0<length>) (-2.5<length>)", Inspect.pointCode (point 10.0 -2.5))
        Assert.Equal(
            "Line(Point.create (0.0<length>) (0.0<length>), Point.create (12.0<length>) (10.0<length>))",
            Inspect.segmentCode (Line(point 0.0 0.0, point 12.0 10.0)))
