module SvgPath.Tests.FormatParityTests

open SvgPath
open Xunit

[<Fact>]
let ``decimal places are clamped to shared target limit`` () =
    let options =
        { LeftDecimals = Succinct
          RightDecimals = Fixed 101 }
    let format = NumberFormat.prepare options []
    Assert.Equal("1." + String.replicate 100 "0" + "e20", NumberFormat.number 1.0e20 format)
