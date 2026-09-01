module SvgPath.Tests.AreaTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private polyline points =
    Subpath.polyline points |> Result.defaultWith (failwithf "%A")

let private polygon points =
    Subpath.polygon points |> Result.defaultWith (failwithf "%A")

let private square x y size =
    [ point x y; point (x + size) y; point (x + size) (y + size); point x (y + size) ]

let private assertAreaNear tolerance expected actual =
    Assert.True(abs (expected - actual) <= tolerance, $"expected {expected}, got {actual}")

[<Fact>]
let ``signed polygon area is squared length and changes with orientation`` () =
    let points = square 0.0 0.0 10.0
    assertAreaNear 1.0e-12<length^2> 100.0<length^2> (Area.signedPoints points)
    assertAreaNear 1.0e-12<length^2> -100.0<length^2> (Area.signedPoints (List.rev points))

[<Fact>]
let ``signed area remains stable after a large translation`` () =
    let origin = 1.0e12
    let points = square origin origin 0.25
    let subpath = polygon points
    assertAreaNear 1.0e-12<length^2> 0.0625<length^2> (Area.signedPoints points)
    assertAreaNear 1.0e-12<length^2> 0.0625<length^2> (Area.signedSubpath subpath)
    let filled = Area.subpath subpath Nonzero |> Result.defaultWith (failwithf "%A")
    assertAreaNear 1.0e-12<length^2> 0.0625<length^2> filled

[<Fact>]
let ``Bezier and arc signed areas use exact integrals`` () =
    let quadratic = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
    let cubic = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 0.0 10.0, point 10.0 10.0, point 10.0 0.0))
    let semicircle =
        Subpath.ofSegment (Arc
            { Start = point -10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 10.0 0.0 })
    assertAreaNear 1.0e-10<length^2> 133.33333333333334<length^2> (abs (Area.signedSubpath quadratic))
    assertAreaNear 1.0e-10<length^2> 60.0<length^2> (abs (Area.signedSubpath cubic))
    assertAreaNear 1.0e-10<length^2> 157.07963267948966<length^2> (abs (Area.signedSubpath semicircle))

[<Fact>]
let ``open subpaths are implicitly closed for fill area`` () =
    let openSquare = polyline (square 0.0 0.0 10.0)
    Assert.Equal(Ok 100.0<length^2>, Area.subpath openSquare Nonzero)
    Assert.Equal(Ok 100.0<length^2>, Area.subpath openSquare EvenOdd)

[<Fact>]
let ``twice traced loop distinguishes fill rules and winding area`` () =
    let loop = square 0.0 0.0 10.0
    let traced = polyline (loop @ [ List.head loop ] @ loop)
    Assert.Equal(Ok 100.0<length^2>, Area.subpath traced Nonzero)
    Assert.Equal(Ok 0.0<length^2>, Area.subpath traced EvenOdd)
    Assert.Equal(Ok 200.0<length^2>, Area.absoluteSubpath traced)
    Assert.Equal(200.0<length^2>, Area.signedSubpath traced)

[<Fact>]
let ``bow tie has fill area but zero signed area`` () =
    let bowTie = polyline [ point 0.0 0.0; point 10.0 10.0; point 0.0 10.0; point 10.0 0.0 ]
    Assert.Equal(0.0<length^2>, Area.signedSubpath bowTie)
    Assert.Equal(Ok 50.0<length^2>, Area.subpath bowTie Nonzero)
    Assert.Equal(Ok 50.0<length^2>, Area.subpath bowTie EvenOdd)
    Assert.Equal(Ok 50.0<length^2>, Area.absoluteSubpath bowTie)

[<Fact>]
let ``path fill rules combine nested subpaths`` () =
    let outer = polyline (square 0.0 0.0 20.0)
    let inner = polyline (square 5.0 5.0 10.0)
    let same = Path.ofSubpaths [ outer; inner ]
    let opposite = Path.ofSubpaths [ outer; polyline (List.rev (square 5.0 5.0 10.0)) ]
    Assert.Equal(Ok 400.0<length^2>, Area.path same Nonzero)
    Assert.Equal(Ok 300.0<length^2>, Area.path same EvenOdd)
    Assert.Equal(Ok 500.0<length^2>, Area.absolutePath same)
    Assert.Equal(Ok 300.0<length^2>, Area.path opposite Nonzero)
    Assert.Equal(Ok 300.0<length^2>, Area.absolutePath opposite)

[<Fact>]
let ``clockwiseness is dimensionless`` () =
    let clockwise = polygon (square 0.0 0.0 10.0)
    let counterclockwise = polygon (List.rev (square 0.0 0.0 10.0))
    Assert.Equal(Ok 1.0, Area.subpathClockwiseness clockwise)
    Assert.Equal(Ok 0.0, Area.subpathClockwiseness counterclockwise)

[<Fact>]
let ``invalid linearization options propagate through area`` () =
    let subpath = polygon (square 0.0 0.0 10.0)
    let options = { Tolerance = 0.0<length>; MaxDepth = 20 }
    Assert.Equal(Error(InvalidLinearizeTolerance 0.0<length>), Area.subpathWith subpath Nonzero options)
