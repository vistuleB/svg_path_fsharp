module SvgPath.Tests.DirectionTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private t value = Parameter.fromFloat value
let private at index value = { SegmentIndex = index; T = t value }
let private near expected actual = Assert.True(Point.distance expected actual < 1.0e-7)

[<Fact>]
let ``segment directions normalize ordinary tangent`` () =
    let directions = Segment.directions (Line(point 1.0 2.0, point 4.0 6.0)) (t 0.5) |> Result.defaultWith (failwithf "%A")
    near (Point.create 0.6 0.8) directions.Incoming.Value
    near (Point.create 0.6 0.8) directions.Outgoing.Value

[<Fact>]
let ``segment directions recover collapsed cubic endpoint tangent`` () =
    let segment = CubicBezier(point 0.0 0.0, point 0.0 0.0, point 0.0 10.0, point 10.0 10.0)
    let directions = Segment.directions segment (t 0.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(directions.Incoming.IsNone)
    near (Point.create 0.0 1.0) directions.Outgoing.Value

[<Fact>]
let ``segment directions distinguish stationary reversal sides`` () =
    let quadratic = QuadraticBezier(point 1.0 0.0, point -1.0 0.0, point 1.0 0.0)
    let directions = Segment.directions quadratic (t 0.5) |> Result.defaultWith (failwithf "%A")
    near (Point.create -1.0 0.0) directions.Incoming.Value
    near (Point.create 1.0 0.0) directions.Outgoing.Value

[<Fact>]
let ``cubic directions distinguish stationary reversal sides`` () =
    let cubic = CubicBezier(point 0.25 0.0, point -0.08333333333333333 0.0, point -0.08333333333333333 0.0, point 0.25 0.0)
    let directions = Segment.directions cubic (t 0.5) |> Result.defaultWith (failwithf "%A")
    near (Point.create -1.0 0.0) directions.Incoming.Value
    near (Point.create 1.0 0.0) directions.Outgoing.Value

[<Fact>]
let ``cubic directions recover third order endpoint direction`` () =
    let origin = point 0.0 0.0
    let directions =
        Segment.directions (CubicBezier(origin, origin, origin, point 3.0 4.0)) (t 0.0)
        |> Result.defaultWith (failwithf "%A")
    Assert.True(directions.Incoming.IsNone)
    near (Point.create 0.6 0.8) directions.Outgoing.Value

[<Fact>]
let ``exact direction options keep nonzero first candidate`` () =
    let segment = CubicBezier(point 0.0 0.0, point 1.0e-12 0.0, point 0.0 1.0, point 1.0 1.0)
    let ordinary = Segment.directions segment (t 0.0) |> Result.defaultWith (failwithf "%A")
    let exact = Segment.directionsWith { RelativeTolerance = 0.0 } segment (t 0.0) |> Result.defaultWith (failwithf "%A")
    near (Point.create 0.0 1.0) ordinary.Outgoing.Value
    near (Point.create 1.0 0.0) exact.Outgoing.Value

[<Fact>]
let ``subpath directions use both sides of corner`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 1.0 0.0); Line(point 1.0 0.0, point 1.0 1.0) ] |> Result.defaultWith (failwithf "%A")
    let directions = Subpath.directions subpath (at 0 1.0) |> Result.defaultWith (failwithf "%A")
    near (Point.create 1.0 0.0) directions.Incoming.Value
    near (Point.create 0.0 1.0) directions.Outgoing.Value

[<Fact>]
let ``subpath directions skip directionless segments`` () =
    let a, b, c = point 0.0 0.0, point 1.0 0.0, point 1.0 1.0
    let subpath = Subpath.create [ Line(a, b); Line(b, b); Line(b, c) ] |> Result.defaultWith (failwithf "%A")
    let directions = Subpath.directions subpath (at 1 0.0) |> Result.defaultWith (failwithf "%A")
    near (Point.create 1.0 0.0) directions.Incoming.Value
    near (Point.create 0.0 1.0) directions.Outgoing.Value

[<Fact>]
let ``subpath directions report open ends and closed seam`` () =
    let a, b, c = point 0.0 0.0, point 1.0 0.0, point 1.0 1.0
    let openSubpath = Subpath.create [ Line(a, b); Line(b, c) ] |> Result.defaultWith (failwithf "%A")
    let openStart = Subpath.directions openSubpath (at 0 0.0) |> Result.defaultWith (failwithf "%A")
    let openEnd = Subpath.directions openSubpath (at 1 1.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(openStart.Incoming.IsNone)
    Assert.True(openEnd.Outgoing.IsNone)
    near (Point.create 1.0 0.0) openStart.Outgoing.Value
    near (Point.create 0.0 1.0) openEnd.Incoming.Value
    let closed =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let seam = Subpath.directions closed (at 0 0.0) |> Result.defaultWith (failwithf "%A")
    near (Point.create -0.70710678 -0.70710678) seam.Incoming.Value
    near (Point.create 1.0 0.0) seam.Outgoing.Value

[<Fact>]
let ``path directions delegate to addressed subpath`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 0.0 2.0) ] |> Result.defaultWith (failwithf "%A")
    let directions = Path.directions (Path.ofSubpaths [ subpath ]) { SubpathIndex = 0; At = at 0 1.0 } |> Result.defaultWith (failwithf "%A")
    near (Point.create 0.0 1.0) directions.Incoming.Value
    Assert.True(directions.Outgoing.IsNone)

[<Fact>]
let ``direction options reject negative relative tolerance`` () =
    Assert.Equal(
        Error(InvalidDirectionRelativeTolerance -0.1),
        Segment.directionsWith { RelativeTolerance = -0.1 } (Line(point 0.0 0.0, point 1.0 0.0)) (t 0.5))
