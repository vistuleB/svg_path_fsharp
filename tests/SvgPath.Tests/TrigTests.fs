module SvgPath.Tests.TrigTests

open SvgPath
open Xunit

let private degrees value = Degree.fromFloat value
let private asFloat value = Degree.toFloat value

[<Fact>]
let ``sin degrees returns exact values at quarter turns`` () =
    Assert.Equal(0.0, Trig.sinDegrees (degrees 0.0))
    Assert.Equal(1.0, Trig.sinDegrees (degrees 90.0))
    Assert.Equal(0.0, Trig.sinDegrees (degrees 180.0))
    Assert.Equal(-1.0, Trig.sinDegrees (degrees 270.0))
    Assert.Equal(0.0, Trig.sinDegrees (degrees 360.0))
    Assert.Equal(-1.0, Trig.sinDegrees (degrees -90.0))

[<Fact>]
let ``cos degrees returns exact values at quarter turns`` () =
    Assert.Equal(1.0, Trig.cosDegrees (degrees 0.0))
    Assert.Equal(0.0, Trig.cosDegrees (degrees 90.0))
    Assert.Equal(-1.0, Trig.cosDegrees (degrees 180.0))
    Assert.Equal(0.0, Trig.cosDegrees (degrees 270.0))
    Assert.Equal(1.0, Trig.cosDegrees (degrees 360.0))
    Assert.Equal(0.0, Trig.cosDegrees (degrees -90.0))

[<Fact>]
let ``tan degrees returns exact values at safe eighth turns`` () =
    Assert.Equal(0.0, Trig.tanDegrees (degrees 0.0))
    Assert.Equal(1.0, Trig.tanDegrees (degrees 45.0))
    Assert.Equal(-1.0, Trig.tanDegrees (degrees 135.0))
    Assert.Equal(0.0, Trig.tanDegrees (degrees 180.0))
    Assert.Equal(1.0, Trig.tanDegrees (degrees 225.0))
    Assert.Equal(-1.0, Trig.tanDegrees (degrees 315.0))
    Assert.Equal(-1.0, Trig.tanDegrees (degrees -45.0))

[<Fact>]
let ``atan2 degrees returns exact axis angles`` () =
    let angle y x = Trig.atan2Degrees (Length.fromFloat y) (Length.fromFloat x) |> asFloat

    Assert.Equal(0.0, angle 0.0 1.0)
    Assert.Equal(90.0, angle 1.0 0.0)
    Assert.Equal(180.0, angle 0.0 -1.0)
    Assert.Equal(-90.0, angle -1.0 0.0)

[<Fact>]
let ``atan2 degrees returns exact diagonal angles`` () =
    let angle y x = Trig.atan2Degrees (Length.fromFloat y) (Length.fromFloat x) |> asFloat

    Assert.Equal(45.0, angle 1.0 1.0)
    Assert.Equal(135.0, angle 1.0 -1.0)
    Assert.Equal(-135.0, angle -1.0 -1.0)
    Assert.Equal(-45.0, angle -1.0 1.0)

[<Fact>]
let ``trig degrees uses math for other angles`` () =
    Assert.Equal(0.5, Trig.sinDegrees (degrees 30.0), 6)
    Assert.Equal(0.5, Trig.cosDegrees (degrees 60.0), 6)
    Assert.Equal(0.577350269, Trig.tanDegrees (degrees 30.0), 6)
    Assert.Equal(45.0, Trig.atanDegrees 1.0 |> asFloat, 6)
    Assert.Equal(63.434948823, Trig.atan2Degrees 2.0 1.0 |> asFloat, 6)
    Assert.Equal(60.0, Trig.acosDegrees 0.5 |> Option.get |> asFloat, 6)

[<Fact>]
let ``acos degrees rejects values outside its domain`` () =
    Assert.Equal(None, Trig.acosDegrees -1.000001)
    Assert.Equal(None, Trig.acosDegrees 1.000001)

[<Fact>]
let ``degree functions accept large finite angles`` () =
    let sine = Trig.sinDegrees (degrees 1.0e20)
    let cosine = Trig.cosDegrees (degrees -1.0e20)
    let tangent = Trig.tanDegrees (degrees 1.0e20)

    Assert.InRange(sine, -1.0, 1.0)
    Assert.InRange(cosine, -1.0, 1.0)
    Assert.False(System.Double.IsNaN tangent)
