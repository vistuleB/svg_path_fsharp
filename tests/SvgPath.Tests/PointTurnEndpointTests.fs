module SvgPath.Tests.PointTurnEndpointTests
open SvgPath
open Xunit
[<Fact>]
let ``heading maps rounded full turn to zero`` () =
    Assert.Equal(0.0<degree>, Point.heading (Point.create 1.0 -1e-16))
[<Fact>]
let ``aperture maps independently rounded full turn to zero`` () =
    let source = Point.create 1.0 1e-16
    Assert.True(Point.heading source > 0.0<degree>)
    Assert.Equal(0.0<degree>, Point.clockwiseAperture source Point.right)
[<Fact>]
let ``near full turn remains distinct when representable`` () =
    let heading = Point.heading (Point.create 1.0 -1e-10)
    let aperture = Point.clockwiseAperture (Point.create 1.0 1e-10) Point.right
    Assert.True(heading > 359.0<degree> && heading < 360.0<degree>)
    Assert.True(aperture > 359.0<degree> && aperture < 360.0<degree>)
