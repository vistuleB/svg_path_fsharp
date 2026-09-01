module SvgPath.Tests.PointTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private coordinatePair x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

[<Fact>]
let ``distance carries the length measure`` () =
    let actual: float<length> = Point.distance (point 0.0 0.0) (point 3.0 4.0)

    Assert.Equal(5.0, Length.toFloat actual, 12)

[<Fact>]
let ``squared distance carries the squared-length measure`` () =
    let actual: float<length^2> =
        Point.squaredDistance (point 0.0 0.0) (point 3.0 4.0)

    Assert.Equal(25.0, float actual, 12)

[<Fact>]
let ``curve interpolation accepts a parameter, not a length`` () =
    let actual =
        Point.interpolate
            (point 2.0 4.0)
            (point 10.0 12.0)
            (Parameter.fromFloat 0.25)

    Assert.Equal(4.0, Length.toFloat actual.X, 12)
    Assert.Equal(6.0, Length.toFloat actual.Y, 12)

[<Fact>]
let ``degree and radian conversion is explicit`` () =
    let radians: float<radian> = Degree.fromFloat 180.0 |> Degree.toRadians
    let degrees: float<degree> = radians |> Radian.toDegrees

    Assert.Equal(System.Math.PI, Radian.toFloat radians, 12)
    Assert.Equal(180.0, float degrees, 12)

[<Fact>]
let ``SVG basis directions and headings use displayed coordinates`` () =
    Assert.Equal(Point.right, Point.direction (degrees 0.0))
    Assert.Equal(Point.down, Point.direction (degrees 90.0))
    Assert.Equal(Point.left, Point.direction (degrees 180.0))
    Assert.Equal(Point.up, Point.direction (degrees 270.0))
    Assert.Equal(0.0, Point.heading Point.right |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.heading Point.down |> Degree.toFloat, 12)
    Assert.Equal(0.0, Point.heading (Point.create 0.0 0.0) |> Degree.toFloat, 12)

[<Fact>]
let ``clockwise apertures are normalized to one turn`` () =
    Assert.Equal(0.0, Point.clockwiseAperture Point.right Point.right |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.clockwiseAperture Point.right Point.down |> Degree.toFloat, 12)
    Assert.Equal(270.0, Point.clockwiseAperture Point.down Point.right |> Degree.toFloat, 12)

[<Fact>]
let ``coordinate-pair arithmetic preserves and combines measures`` () =
    let a = coordinatePair 3.0 4.0
    let b = coordinatePair 1.0 -2.0
    let dot: float<length^2> = Point.dot a b
    let cross: float<length^2> = Point.cross a b

    Assert.Equal(coordinatePair 4.0 2.0, Point.add a b)
    Assert.Equal(coordinatePair 2.0 6.0, Point.subtract a b)
    Assert.Equal(coordinatePair -1.0 2.0, Point.negate b)
    Assert.Equal(coordinatePair 6.0 8.0, Point.scale 2.0 a)
    Assert.Equal(-5.0, float dot, 12)
    Assert.Equal(-10.0, float cross, 12)

[<Fact>]
let ``norm avoids intermediate overflow`` () =
    let large = coordinatePair 1.0e200 0.0
    Assert.Equal(1.0e200, Point.norm large |> Length.toFloat)

[<Fact>]
let ``normalization and projections reject the zero coordinate pair`` () =
    let a = coordinatePair 3.0 4.0
    let horizontal = coordinatePair 2.0 0.0
    let zero = coordinatePair 0.0 0.0

    let unit = Point.normalize a |> Option.get
    let projected = Point.project a horizontal |> Option.get
    let scalar = Point.scalarProjection a horizontal |> Option.get

    Assert.Equal(0.6, unit.X, 12)
    Assert.Equal(0.8, unit.Y, 12)
    Assert.Equal(coordinatePair 3.0 0.0, projected)
    Assert.Equal(3.0, Length.toFloat scalar, 12)
    Assert.Equal(None, Point.normalize zero)
    Assert.Equal(None, Point.project a zero)
    Assert.Equal(None, Point.scalarProjection a zero)

[<Fact>]
let ``rotations have visual SVG semantics`` () =
    let a = coordinatePair 2.0 3.0
    Assert.Equal(coordinatePair -3.0 2.0, Point.rotateClockwise a)
    Assert.Equal(coordinatePair 3.0 -2.0, Point.rotateCounterclockwise a)

[<Fact>]
let ``point midpoint interpolation and nearness match Gleam contracts`` () =
    let a = point 0.0 10.0
    let b = point 10.0 30.0

    Assert.Equal(point 5.0 20.0, Point.midpoint a b)
    Assert.Equal(point 20.0 50.0, Point.interpolate a b (Parameter.fromFloat 2.0))
    Assert.True(Point.near (Length.fromFloat 5.0) (point 0.0 0.0) (point 3.0 4.0))
    Assert.False(Point.near (Length.fromFloat 4.999) (point 0.0 0.0) (point 3.0 4.0))
    Assert.False(Point.near (Length.fromFloat -0.001) a a)
    Assert.False(Point.near (Length.fromFloat infinity) a a)
