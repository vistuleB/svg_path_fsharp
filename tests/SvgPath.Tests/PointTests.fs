module SvgPath.Tests.PointTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

[<Fact>]
let ``basis vectors and direction`` () =
    Assert.Equal(Point.create 1.0 0.0, Point.right)
    Assert.Equal(Point.create -1.0 0.0, Point.left)
    Assert.Equal(Point.create 0.0 -1.0, Point.up)
    Assert.Equal(Point.create 0.0 1.0, Point.down)
    Assert.Equal(Point.right, Point.direction (degrees 0.0))
    Assert.Equal(Point.down, Point.direction (degrees 90.0))
    Assert.Equal(Point.left, Point.direction (degrees 180.0))
    Assert.Equal(Point.up, Point.direction (degrees 270.0))

[<Fact>]
let ``clockwise aperture`` () =
    Assert.Equal(0.0, Point.clockwiseAperture Point.right Point.right |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.clockwiseAperture Point.right Point.down |> Degree.toFloat, 12)
    Assert.Equal(270.0, Point.clockwiseAperture Point.down Point.right |> Degree.toFloat, 12)
    Assert.Equal(180.0, Point.clockwiseAperture Point.right Point.left |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.clockwiseAperture Point.up Point.right |> Degree.toFloat, 12)
    Assert.Equal(90.0, Point.clockwiseAperture (Point.create 0.0 0.0) Point.down |> Degree.toFloat, 12)

[<Fact>]
let ``vector arithmetic`` () =
    let a = point 3.0 4.0
    let b = point 1.0 -2.0
    Assert.Equal(point 4.0 2.0, Point.add a b)
    Assert.Equal(point 2.0 6.0, Point.subtract a b)
    Assert.Equal(point -1.0 2.0, Point.negate b)
    Assert.Equal(point 6.0 8.0, Point.scale 2.0 a)

[<Fact>]
let ``dot cross norm and distance`` () =
    let a = point 3.0 4.0
    let b = point 6.0 8.0
    Assert.Equal(50.0, float (Point.dot a b), 12)
    Assert.Equal(0.0, float (Point.cross a b), 12)
    Assert.Equal(25.0, float (Point.squaredNorm a), 12)
    Assert.Equal(5.0, Length.toFloat (Point.norm a), 12)
    Assert.Equal(25.0, float (Point.squaredDistance a b), 12)
    Assert.Equal(5.0, Length.toFloat (Point.distance a b), 12)

[<Fact>]
let ``norm and distance avoid intermediate overflow`` () =
    let large = point 1.0e200 0.0
    Assert.Equal(1.0e200, Point.norm large |> Length.toFloat)
    Assert.Equal(1.0e200, Point.distance (point 0.0 0.0) large |> Length.toFloat)

[<Fact>]
let ``normalize`` () =
    let a = point 3.0 4.0
    let zero = point 0.0 0.0

    let unit = Point.normalize a |> Option.get
    Assert.Equal(0.6, unit.X, 12)
    Assert.Equal(0.8, unit.Y, 12)
    Assert.Equal(None, Point.normalize zero)

[<Fact>]
let ``projection`` () =
    let a = point 3.0 4.0
    let horizontal = point 2.0 0.0
    let zero = point 0.0 0.0
    Assert.Equal(Some(point 3.0 0.0), Point.project a horizontal)
    Assert.Equal(Some(3.0<length>), Point.scalarProjection a horizontal)
    Assert.Equal(None, Point.project a zero)
    Assert.Equal(None, Point.scalarProjection a zero)

[<Fact>]
let ``rotations and near`` () =
    let a = point 2.0 3.0
    Assert.Equal(point -3.0 2.0, Point.rotateClockwise a)
    Assert.Equal(point 3.0 -2.0, Point.rotateCounterclockwise a)
    Assert.True(Point.near (Length.fromFloat 5.0) (point 0.0 0.0) (point 3.0 4.0))
    Assert.False(Point.near (Length.fromFloat 4.999) (point 0.0 0.0) (point 3.0 4.0))
    Assert.False(Point.near (Length.fromFloat -0.001) a a)

[<Fact>]
let ``midpoint and lerp`` () =
    let a = point 0.0 10.0
    let b = point 10.0 30.0

    Assert.Equal(point 5.0 20.0, Point.midpoint a b)
    Assert.Equal(point 2.5 15.0, Point.interpolate a b (Parameter.fromFloat 0.25))
    Assert.Equal(point 20.0 50.0, Point.interpolate a b (Parameter.fromFloat 2.0))
