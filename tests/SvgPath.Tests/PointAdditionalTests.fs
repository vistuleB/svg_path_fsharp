module SvgPath.Tests.PointAdditionalTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``distance carries the length measure`` () =
    let actual: float<length> = Point.distance (point 0.0 0.0) (point 3.0 4.0)
    Assert.Equal(5.0, Length.toFloat actual, 12)

[<Fact>]
let ``squared distance carries the squared-length measure`` () =
    let actual: float<length^2> = Point.squaredDistance (point 0.0 0.0) (point 3.0 4.0)
    Assert.Equal(25.0, float actual, 12)

[<Fact>]
let ``curve interpolation accepts a parameter, not a length`` () =
    let actual = Point.interpolate (point 2.0 4.0) (point 10.0 12.0) (Parameter.fromFloat 0.25)
    Assert.Equal(4.0, Length.toFloat actual.X, 12)
    Assert.Equal(6.0, Length.toFloat actual.Y, 12)

[<Fact>]
let ``degree and radian conversion is explicit`` () =
    let radians: float<radian> = Degree.fromFloat 180.0 |> Degree.toRadians
    let degrees: float<degree> = radians |> Radian.toDegrees
    Assert.Equal(System.Math.PI, Radian.toFloat radians, 12)
    Assert.Equal(180.0, float degrees, 12)

[<Fact>]
let ``headings use displayed coordinates`` () =
    Assert.Equal(0.0, Point.heading Point.right |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.heading Point.down |> Degree.toFloat, 12)
    Assert.Equal(0.0, Point.heading (Point.create 0.0 0.0) |> Degree.toFloat, 12)

[<Fact>]
let ``nearness rejects a nonfinite tolerance`` () =
    let value = point 2.0 3.0
    Assert.False(Point.near (Length.fromFloat infinity) value value)
