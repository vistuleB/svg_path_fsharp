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
let ``signed points implicitly closes the loop`` () =
    let points = square 0.0 0.0 10.0
    assertAreaNear 1.0e-12<length^2> 100.0<length^2> (Area.signedPoints points)
    assertAreaNear 1.0e-12<length^2> -100.0<length^2> (Area.signedPoints (List.rev points))
    Assert.Equal(0.0<length^2>, Area.signedPoints [])
    Assert.Equal(0.0<length^2>, Area.signedPoints [ point 0.0 0.0 ])

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
let ``fill area detects intersections at small coordinate scale`` () =
    let size = 1.0e-7
    let bowTie = polygon [ point 0.0 0.0; point size size; point 0.0 size; point size 0.0 ]
    let filled = Area.subpath bowTie Nonzero |> Result.defaultWith (failwithf "%A")
    assertAreaNear 1.0e-20<length^2> (size * size / 2.0 * 1.0<length^2>) filled

[<Fact>]
let ``signed subpath ignores the closed field`` () =
    let points = square 0.0 0.0 10.0
    assertAreaNear 1.0e-12<length^2> 100.0<length^2> (Area.signedSubpath (polyline points))
    assertAreaNear 1.0e-12<length^2> 100.0<length^2> (Area.signedSubpath (polygon points))

[<Fact>]
let ``signed Bezier segments use exact line integrals`` () =
    let quadratic = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
    let cubic = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 0.0 10.0, point 10.0 10.0, point 10.0 0.0))
    assertAreaNear 1.0e-10<length^2> 133.33333333333334<length^2> (abs (Area.signedSubpath quadratic))
    assertAreaNear 1.0e-10<length^2> 60.0<length^2> (abs (Area.signedSubpath cubic))

[<Fact>]
let ``signed arc segment uses the ellipse integral`` () =
    let semicircle =
        Subpath.ofSegment (Arc
            { Start = point -10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 10.0 0.0 })
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
let ``path fill area combines subpaths by fill rule`` () =
    let outer = polyline (square 0.0 0.0 20.0)
    let inner = polyline (square 5.0 5.0 10.0)
    let same = Path.ofSubpaths [ outer; inner ]
    let opposite = Path.ofSubpaths [ outer; polyline (List.rev (square 5.0 5.0 10.0)) ]
    Assert.Equal(Ok 400.0<length^2>, Area.path same Nonzero)
    Assert.Equal(Ok 300.0<length^2>, Area.path same EvenOdd)
    Assert.Equal(Ok 500.0<length^2>, Area.absolutePath same)
    Assert.Equal(Ok 300.0<length^2>, Area.path opposite Nonzero)
    Assert.Equal(Ok 300.0<length^2>, Area.path opposite EvenOdd)
    Assert.Equal(Ok 300.0<length^2>, Area.absolutePath opposite)

[<Fact>]
let ``path fill area cancels overlapping opposite loops`` () =
    let forward = polyline (square 0.0 0.0 10.0)
    let backward = polyline (List.rev (square 0.0 0.0 10.0))
    let path = Path.ofSubpaths [ forward; backward ]
    Assert.Equal(Ok 0.0<length^2>, Area.path path Nonzero)
    Assert.Equal(Ok 0.0<length^2>, Area.path path EvenOdd)
    Assert.Equal(0.0<length^2>, Area.signedPath path)
    Assert.Equal(Ok 0.0<length^2>, Area.absolutePath path)

[<Fact>]
let ``absolute path counts overlapping winding magnitude`` () =
    let outer = polyline (square 0.0 0.0 20.0)
    let inner = polyline (square 5.0 5.0 10.0)
    let path = Path.ofSubpaths [ outer; inner ]
    Assert.Equal(Ok 400.0<length^2>, Area.path path Nonzero)
    Assert.Equal(Ok 300.0<length^2>, Area.path path EvenOdd)
    Assert.Equal(500.0<length^2>, Area.signedPath path)
    Assert.Equal(Ok 500.0<length^2>, Area.absolutePath path)

[<Fact>]
let ``subpath clockwiseness reports area orientation`` () =
    let clockwise = polygon (square 0.0 0.0 10.0)
    let counterclockwise = polygon (List.rev (square 0.0 0.0 10.0))
    Assert.Equal(Ok 1.0, Area.subpathClockwiseness clockwise)
    Assert.Equal(Ok 0.0, Area.subpathClockwiseness counterclockwise)

[<Fact>]
let ``subpath clockwiseness uses implicit closing chord`` () =
    let openSquare = polyline (square 0.0 0.0 10.0)
    let line = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Ok 1.0, Area.subpathClockwiseness openSquare)
    Assert.Equal(Ok 0.5, Area.subpathClockwiseness line)

[<Fact>]
let ``subpath clockwiseness can be intermediate`` () =
    let bowTie = polyline [ point 0.0 0.0; point 10.0 10.0; point 0.0 10.0; point 10.0 0.0 ]
    Assert.Equal(Ok 0.5, Area.subpathClockwiseness bowTie)

[<Fact>]
let ``subpath clockwiseness rejects invalid linearization options`` () =
    let subpath = polygon (square 0.0 0.0 10.0)
    let options = { Tolerance = 0.0<length>; MaxDepth = 20 }
    Assert.Equal(Error(InvalidLinearizeTolerance 0.0<length>), Area.subpathClockwisenessWith subpath options)

[<Fact>]
let ``move-only paths have zero area`` () =
    let moveOnly = Subpath.empty (point 3.0 4.0)
    let path = Path.ofSubpaths [ moveOnly ]
    Assert.Equal(0.0<length^2>, Area.signedSubpath moveOnly)
    Assert.Equal(0.0<length^2>, Area.signedPath path)
    Assert.Equal(Ok 0.0<length^2>, Area.path path Nonzero)
    Assert.Equal(Ok 0.0<length^2>, Area.path path EvenOdd)
    Assert.Equal(Ok 0.0<length^2>, Area.absolutePath path)

[<Fact>]
let ``curved fill area uses linearization options`` () =
    let curve = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 0.0 10.0, point 10.0 10.0, point 10.0 0.0))
    let options = { Tolerance = 0.0001<length>; MaxDepth = 20 }
    let filled = Area.subpathWith curve Nonzero options |> Result.defaultWith (failwithf "%A")
    assertAreaNear 0.01<length^2> 60.0<length^2> filled

[<Fact>]
let ``fill area rejects invalid linearization options`` () =
    let subpath = polyline (square 0.0 0.0 10.0)
    let options = { Tolerance = 0.0<length>; MaxDepth = 20 }
    Assert.Equal(Error(InvalidLinearizeTolerance 0.0<length>), Area.subpathWith subpath Nonzero options)
    Assert.Equal(Error(InvalidLinearizeTolerance 0.0<length>), Area.absoluteSubpathWith subpath options)
