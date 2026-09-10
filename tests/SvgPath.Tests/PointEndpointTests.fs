module SvgPath.Tests.PointEndpointTests
open SvgPath
open Xunit
let private p x y = Point.create x y
[<Fact>]
let ``lerp preserves exact end despite cancellation`` () =
    let a, b = p 1. 1e16, p 0.1 1.
    Assert.Equal(b, Point.interpolate a b 1.0<parameter>)
[<Fact>]
let ``lerp endpoints avoid overflowing difference`` () =
    let a, b = p -1e308 1e308, p 1e308 -1e308
    Assert.Equal(a, Point.interpolate a b 0.0<parameter>)
    Assert.Equal(a, Point.interpolate a b -0.0<parameter>)
    Assert.Equal(b, Point.interpolate a b 1.0<parameter>)
[<Fact>]
let ``lerp preserves signed zero endpoint coordinates`` () =
    let a, b = p -0.0 0.0, p 0.0 -0.0
    // Compare bit patterns because .NET numeric equality merges signed zeros.
    let exact expected actual =
        Assert.Equal(System.BitConverter.DoubleToInt64Bits expected.X, System.BitConverter.DoubleToInt64Bits actual.X)
        Assert.Equal(System.BitConverter.DoubleToInt64Bits expected.Y, System.BitConverter.DoubleToInt64Bits actual.Y)
    exact a (Point.interpolate a b 0.0<parameter>)
    exact a (Point.interpolate a b -0.0<parameter>)
    exact b (Point.interpolate a b 1.0<parameter>)
