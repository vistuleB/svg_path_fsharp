module SvgPath.Tests.SmallestEnclosingCircleTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private assertNear (tolerance: float<'u>) (expected: float<'u>) (actual: float<'u>) =
    Assert.True(abs (expected - actual) <= tolerance, $"expected {expected}, got {actual}")

let private assertCircle samples expectedCenter expectedRadiusSquared =
    let circle = SmallestEnclosingCircle.points samples |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance circle.Center expectedCenter <= 1.0e-9<length>)
    assertNear 1.0e-9<length^2> expectedRadiusSquared circle.RadiusSquared

[<Fact>]
let ``cocircular trapezoid preserves previous support points`` () =
    assertCircle [point 6.0 8.0;point 0.0 -10.0;point -1.0 -10.0;point -7.0 8.0]
        (point -0.5 (1.0/6.0)) (3730.0<length^2>/36.0)

[<Fact>]
let ``cocircular points preserve radius under rotation and scaling`` () =
    for rotation in [0.0;7.0;43.0;90.0;137.0] do
        for scale in [1e-9;1.0;1000.0] do
            let samples = [0.0;29.0;83.0;145.0;191.0;239.0;301.0] |> List.map (fun angle ->
                let angle = Degree.fromFloat(angle+rotation)
                point (scale*(3.0+7.0*Trig.cosDegrees angle)) (scale*(-2.0+7.0*Trig.sinDegrees angle)))
            let circle = SmallestEnclosingCircle.points samples |> Result.defaultWith (failwithf "%A")
            assertNear 1e-9 3.0 (float circle.Center.X/scale)
            assertNear 1e-9 -2.0 (float circle.Center.Y/scale)
            assertNear 1e-9 49.0 (float circle.RadiusSquared/(scale*scale))
            Assert.True(samples |> List.forall (fun sample -> Point.squaredDistance circle.Center sample <= circle.RadiusSquared))
            Assert.Equal(Ok circle,SmallestEnclosingCircle.points (List.rev samples))

[<Fact>]
let ``one point preserves exact center`` () =
    let sample = point 3.0 -7.0
    let expected = Ok { Center = sample; RadiusSquared = 0.0<length^2> }
    Assert.Equal(expected, SmallestEnclosingCircle.points [ sample ])

[<Fact>]
let ``equal points preserve exact center`` () =
    let sample = point 3.0 -7.0
    let expected = Ok { Center = sample; RadiusSquared = 0.0<length^2> }
    Assert.Equal(expected, SmallestEnclosingCircle.points [ sample; sample; sample ])

[<Fact>]
let ``two points use midpoint`` () =
    assertCircle [ point 2.0 1.0; point 6.0 5.0 ] (point 4.0 3.0) 8.0<length^2>

[<Fact>]
let ``collinear points use farthest pair`` () =
    assertCircle
        [ point 0.0 0.0; point 1.0 0.0; point 4.0 0.0; point 2.0 0.0 ]
        (point 2.0 0.0)
        4.0<length^2>

[<Fact>]
let ``obtuse triangle uses longest side`` () =
    assertCircle [ point 0.0 0.0; point 4.0 0.0; point 1.0 1.0 ] (point 2.0 0.0) 4.0<length^2>

[<Fact>]
let ``acute triangle uses circumcircle`` () =
    assertCircle [ point 0.0 0.0; point 2.0 0.0; point 1.0 2.0 ] (point 1.0 0.75) 1.5625<length^2>

[<Fact>]
let ``point permutations produce same circle`` () =
    let a, b, c = point 0.0 0.0, point 2.0 0.0, point 1.0 2.0
    let expected = SmallestEnclosingCircle.points [ a; b; c ]
    [ [ a; b; c ]; [ a; c; b ]; [ b; a; c ]; [ b; c; a ]; [ c; a; b ]; [ c; b; a ] ]
    |> List.iter (fun samples -> Assert.Equal(expected, SmallestEnclosingCircle.points samples))

[<Fact>]
let ``close collinear points still find the smallest circle`` () =
    assertCircle
        [ point 0.0 0.0; point 1.0e-13 0.0; point 2.0e-13 0.0 ]
        (point 1.0e-13 0.0)
        1.0e-26<length^2>

[<Fact>]
let ``circumcircle is stable after large translation`` () =
    let origin = 1.0e12
    assertCircle
        [ point origin origin; point (origin + 2.0) origin; point (origin + 1.0) (origin + 2.0) ]
        (point (origin + 1.0) (origin + 0.75))
        1.5625<length^2>
