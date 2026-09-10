module SvgPath.Tests.PointArithmeticTests
open SvgPath
open Xunit
let private p x y = Point.create x y
[<Fact>]
let ``interpolation avoids opposite endpoint overflow`` () =
    let a,b = p -1e308 1e308,p 1e308 -1e308
    Assert.Equal(Point.zero,Point.midpoint a b)
    let quarter = Point.interpolate a b 0.25<parameter>
    Assert.True(quarter.X / 1e308 >= -0.500000000000001 && quarter.X / 1e308 <= -0.499999999999999)
    Assert.True(quarter.Y / 1e308 >= 0.499999999999999 && quarter.Y / 1e308 <= 0.500000000000001)
    Assert.Equal(a,Point.interpolate a b 0.0<parameter>)
    Assert.Equal(b,Point.interpolate a b 1.0<parameter>)
[<Fact>]
let ``projection extreme target scale`` () =
    for magnitude in [1e-200;1e200] do
        Assert.Equal(Some(p 3. 0.),Point.project (p 3. 4.) (p magnitude 0.))
        Assert.Equal(Some -3.,Point.scalarProjection (p 3. 4.) (p -magnitude 0.))
        let projected = Point.project (p 5. 0.) (p (3.*magnitude) (4.*magnitude)) |> Option.get
        Assert.True(Point.near 1e-14 projected (p 1.8 2.4))
[<Fact>]
let ``projection preserves mixed scale source`` () =
    let source = p 1e308 1e-300
    Assert.Equal(Some(p 0. 1e-300),Point.project source Point.down)
    Assert.Equal(Some 1e-300,Point.scalarProjection source Point.down)
[<Fact>]
let ``projection avoids unrepresentable scalar intermediate`` () =
    let maximum = System.Double.MaxValue
    let source = p maximum maximum
    let projected = Point.project source (p 1. 1.) |> Option.get
    Assert.True(projected.X / maximum > 0.999999999999999)
    Assert.True(projected.Y / maximum > 0.999999999999999)
    Assert.Equal(None,Point.scalarProjection source (p 1. 1.))
    Assert.Equal(Some 0.,Point.scalarProjection (p maximum -maximum) (p 1. 1.))
[<Fact>]
let ``near extreme scales`` () =
    for magnitude in [1e-200;1e200] do
        Assert.False(Point.near magnitude Point.zero (p (10.*magnitude) 0.))
        Assert.True(Point.near magnitude Point.zero (p (0.3*magnitude) (0.4*magnitude)))
        Assert.False(Point.near magnitude Point.zero (p (0.8*magnitude) (0.8*magnitude)))
        Assert.True(Point.near magnitude Point.zero (p magnitude 0.))
    let maximum = System.Double.MaxValue
    Assert.False(Point.near maximum (p maximum 0.) (p -maximum 0.))
    Assert.False(Point.near maximum Point.zero (p maximum maximum))
    Assert.False(Point.near 0. Point.zero (p 1e-310 0.))
    Assert.True(Point.near -0. (p -0. 0.) Point.zero)
