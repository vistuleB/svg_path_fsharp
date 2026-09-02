module SvgPath.Tests.InternalNumberTests

open System
open SvgPath
open Xunit

[<Fact>]
let ``hypot avoids representable intermediate overflow`` () =
    let result = InternalNumber.hypot 1.0e308 1.0e308
    Assert.True(Double.IsFinite result)
    Assert.True(result > 1.4e308)

[<Theory>]
[<InlineData("1", 1.0)>]
[<InlineData(".5", 0.5)>]
[<InlineData("+2.5e2", 250.0)>]
let ``parse accepts finite SVG number forms`` raw expected =
    Assert.Equal(Ok expected, InternalNumber.parse raw)

[<Fact>]
let ``parse rejects nonfinite results`` () =
    Assert.Equal(Error(), InternalNumber.parse "1e400")

[<Theory>]
[<InlineData("23.")>]
[<InlineData("23.e2")>]
[<InlineData("-0.E-4")>]
let ``parse rejects SVG numbers with a trailing decimal point`` raw =
    Assert.Equal(Error(), InternalNumber.parse raw)

[<Fact>]
let ``checked arithmetic rejects overflow`` () =
    Assert.Equal(Error(), InternalNumber.checkedProduct Double.MaxValue 2.0)
    Assert.Equal(Error(), InternalNumber.checkedSum Double.MaxValue Double.MaxValue)
    Assert.Equal(Ok 6.0, InternalNumber.checkedProduct 2.0 3.0)
    Assert.Equal(Ok 5.0, InternalNumber.checkedSum 2.0 3.0)
