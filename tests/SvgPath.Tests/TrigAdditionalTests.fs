module SvgPath.Tests.TrigAdditionalTests

open SvgPath
open Xunit

let private degrees value = Degree.fromFloat value

[<Fact>]
let ``degree and radian conversions carry their measures`` () =
    let radians: float<radian> = Degree.toRadians (degrees 180.0)
    let roundTrip: float<degree> = Radian.toDegrees radians

    Assert.Equal(System.Math.PI, Radian.toFloat radians, 12)
    Assert.Equal(180.0, Degree.toFloat roundTrip, 12)

[<Fact>]
let ``non-finite angles propagate as NaN instead of failing normalization`` () =
    Assert.True(Trig.sinDegrees (degrees infinity) |> System.Double.IsNaN)
    Assert.True(Trig.cosDegrees (degrees -infinity) |> System.Double.IsNaN)
    Assert.True(Trig.tanDegrees (degrees nan) |> System.Double.IsNaN)
