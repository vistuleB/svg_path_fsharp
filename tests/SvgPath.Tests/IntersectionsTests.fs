module SvgPath.Tests.IntersectionsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private assertParameterNear expected actual tolerance =
    Assert.True(abs (actual - expected) <= Parameter.fromFloat tolerance, $"expected {expected}, got {actual}")

let private arcPair () =
    let left =
        Arc
            { Start = point 82.60920101224798 220.34092587189474
              Radius = point 20.01 20.01
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 43.21295323581002 213.39430445023285 }
    let right =
        Arc
            { Start = point 43.190371867436326 213.5338826899446
              Radius = point 210.0 210.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 454.61771360489934 202.76027858778826 }
    left, right

[<Fact>]
let ``arc crossing regression is found in either order`` () =
    let left, right = arcPair ()
    let forward = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let backward = Intersections.segment right left |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear forward.LeftT backward.RightT 1.0e-6
    assertParameterNear forward.RightT backward.LeftT 1.0e-6

[<Fact>]
let ``symmetric kissing quadratics are found`` () =
    let upper = QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0)
    let lower = QuadraticBezier(point -1.0 -1.0, point 0.0 1.0, point 1.0 -1.0)
    let intersection = Intersections.segment upper lower |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``flat cubic crossing is found`` () =
    let rising = CubicBezier(point 0.0 -0.125, point (1.0 / 3.0) 0.125, point (2.0 / 3.0) -0.125, point 1.0 0.125)
    let falling = CubicBezier(point 0.0 0.125, point (1.0 / 3.0) -0.125, point (2.0 / 3.0) 0.125, point 1.0 -0.125)
    let intersection = Intersections.segment rising falling |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``disjoint quadratics return no intersections`` () =
    let upper = QuadraticBezier(point -1.0 2.0, point 0.0 1.0, point 1.0 2.0)
    let lower = QuadraticBezier(point -1.0 -2.0, point 0.0 -1.0, point 1.0 -2.0)
    Assert.Equal(Ok [], Intersections.segment upper lower)

[<Fact>]
let ``off-center kissing quadratics are found`` () =
    let left = QuadraticBezier(point 0.0 0.1369, point 0.5 -0.2331, point 1.0 0.3969)
    let right = QuadraticBezier(point -0.26 -0.3969, point 0.24 0.2331, point 0.74 -0.1369)
    let intersection = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.37<parameter> intersection.LeftT 2.0e-5
    assertParameterNear 0.63<parameter> intersection.RightT 2.0e-5

[<Fact>]
let ``two close quadratic crossings remain distinct`` () =
    let axis = QuadraticBezier(point 0.0 0.0, point 0.5 0.0, point 1.0 0.0)
    let curve = QuadraticBezier(point 0.0 0.24999999, point 0.5 -0.25000001, point 1.0 0.24999999)
    let found = Intersections.segment axis curve |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``adjacent quadratics include endpoint and crossing`` () =
    let previous = QuadraticBezier(point 0.0 0.0, point 0.5 2.0, point 1.0 0.0)
    let next = QuadraticBezier(point 1.0 0.0, point 0.5 -1.0, point 0.0 1.0)
    let found = Intersections.segment previous next |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``close crossing regression is found`` () =
    let previous =
        CubicBezier(
            point 3.7326908112839723 3.0798423879604986,
            point 3.7326908112839723 3.0798423879604986,
            point 3.732979794442744 3.079321742790136,
            point 3.7326907230069644 3.079843411893102)
    let next =
        CubicBezier(
            point 3.7326907230069644 3.079843411893102,
            point 3.7867214554038764 2.9802982971613803,
            point 3.813617440000867 2.8517024128302615,
            point 3.8148384145946803 2.834959380847173)
    let found = Intersections.segment previous next |> Result.defaultWith (failwithf "%A")
    Assert.True(List.length found >= 1)

[<Fact>]
let ``invalid intersection options are rejected`` () =
    let line = Line(point 0.0 0.0, point 1.0 1.0)
    Assert.Equal(Error(InvalidIntersectionTolerance -1.0e-9<length>), Intersections.segmentWith line line { Intersections.defaultOptions with Tolerance = -1.0e-9<length> })
    Assert.Equal(Error(InvalidIntersectionMaxDepth 0), Intersections.segmentWith line line { Intersections.defaultOptions with MaxDepth = 0 })

[<Fact>]
let ``overlapping segments are rejected`` () =
    let line = Line(point 0.0 0.0, point 1.0 1.0)
    Assert.Equal(Error OverlappingSegments, Intersections.segment line line)
