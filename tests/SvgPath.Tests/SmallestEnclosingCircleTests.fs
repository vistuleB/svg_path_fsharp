module SvgPath.Tests.SmallestEnclosingCircleTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private assertNear tolerance expected actual =
    Assert.True(abs (expected - actual) <= tolerance, $"expected {expected}, got {actual}")

let private assertCircle samples expectedCenter expectedRadiusSquared =
    let circle = SmallestEnclosingCircle.points samples |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance circle.Center expectedCenter <= 1.0e-9<length>)
    assertNear 1.0e-9<length^2> expectedRadiusSquared circle.RadiusSquared

[<Fact>]
let ``empty input has no enclosing circle`` () =
    Assert.Equal(Error(), SmallestEnclosingCircle.points [])

[<Fact>]
let ``one point and exact duplicates preserve the point`` () =
    let sample = point 3.0 -7.0
    let expected = Ok { Center = sample; RadiusSquared = 0.0<length^2> }
    Assert.Equal(expected, SmallestEnclosingCircle.points [ sample ])
    Assert.Equal(expected, SmallestEnclosingCircle.points [ sample; sample; sample ])

[<Fact>]
let ``two points use their midpoint`` () =
    assertCircle [ point 2.0 1.0; point 6.0 5.0 ] (point 4.0 3.0) 8.0<length^2>

[<Fact>]
let ``collinear points use their farthest pair`` () =
    assertCircle
        [ point 0.0 0.0; point 1.0 0.0; point 4.0 0.0; point 2.0 0.0 ]
        (point 2.0 0.0)
        4.0<length^2>

[<Fact>]
let ``obtuse and acute triangles select the appropriate support`` () =
    assertCircle [ point 0.0 0.0; point 4.0 0.0; point 1.0 1.0 ] (point 2.0 0.0) 4.0<length^2>
    assertCircle [ point 0.0 0.0; point 2.0 0.0; point 1.0 2.0 ] (point 1.0 0.75) 1.5625<length^2>

[<Fact>]
let ``point permutations produce the same circle`` () =
    let a, b, c = point 0.0 0.0, point 2.0 0.0, point 1.0 2.0
    let expected = SmallestEnclosingCircle.points [ a; b; c ]
    [ [ a; b; c ]; [ a; c; b ]; [ b; a; c ]; [ b; c; a ]; [ c; a; b ]; [ c; b; a ] ]
    |> List.iter (fun samples -> Assert.Equal(expected, SmallestEnclosingCircle.points samples))

[<Fact>]
let ``close collinear points preserve their scale`` () =
    assertCircle
        [ point 0.0 0.0; point 1.0e-13 0.0; point 2.0e-13 0.0 ]
        (point 1.0e-13 0.0)
        1.0e-26<length^2>

[<Fact>]
let ``circumcircle remains stable after a large translation`` () =
    let origin = 1.0e12
    assertCircle
        [ point origin origin; point (origin + 2.0) origin; point (origin + 1.0) (origin + 2.0) ]
        (point (origin + 1.0) (origin + 0.75))
        1.5625<length^2>
