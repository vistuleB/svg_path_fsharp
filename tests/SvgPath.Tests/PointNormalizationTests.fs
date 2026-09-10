module SvgPath.Tests.PointNormalizationTests
open SvgPath
open Xunit
[<Fact>]
let ``normalize extreme cardinal vectors`` () =
    for magnitude in [1e-310;System.Double.MaxValue] do
        Assert.Equal(Some Point.right,Point.normalize (Point.create magnitude 0.))
        Assert.Equal(Some Point.left,Point.normalize (Point.create -magnitude 0.))
        Assert.Equal(Some Point.down,Point.normalize (Point.create 0. magnitude))
        Assert.Equal(Some Point.up,Point.normalize (Point.create 0. -magnitude))
[<Fact>]
let ``normalize extreme noncardinal vectors`` () =
    for magnitude in [1e-310;System.Double.MaxValue] do
        for sx in [1.;-1.] do
            for sy in [1.;-1.] do
                let unit = Point.normalize (Point.create (sx*magnitude) (sy*magnitude)) |> Option.get
                Assert.True(Point.near 1e-15 unit (Point.create (sx * 0.7071067811865475) (sy * 0.7071067811865475)))
    for vector in [Point.create 3e-310 -4e-310;Point.create 3e307 -4e307] do
        let unit = Point.normalize vector |> Option.get
        Assert.True(Point.near 1e-13 unit (Point.create 0.6 -0.8))
[<Fact>]
let ``normalize signed zero vectors`` () =
    for x in [0.;-0.] do
        for y in [0.;-0.] do Assert.Equal(None,Point.normalize (Point.create x y))
