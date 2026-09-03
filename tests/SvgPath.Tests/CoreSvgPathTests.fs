module SvgPath.Tests.CoreSvgPathTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``line keeps its endpoints`` () =
    let startPoint = point 0.0 0.0
    let endPoint = point 10.0 20.0
    let segment = Line(startPoint, endPoint)
    Assert.Equal(startPoint, Segment.start segment)
    Assert.Equal(endPoint, Segment.finish segment)

[<Fact>]
let ``reverse segment reverses lines quadratics cubics and arcs`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    Assert.Equal(Line(b, a), Segment.reverse (Line(a, b)))
    Assert.Equal(QuadraticBezier(c, b, a), Segment.reverse (QuadraticBezier(a, b, c)))
    Assert.Equal(CubicBezier(d, c, b, a), Segment.reverse (CubicBezier(a, b, c, d)))
    let arc = Arc { Start = a; Radius = point 4.0 5.0; XAxisRotation = 30.0<degree>; LargeArc = true; Sweep = false; End = b }
    let expected = Arc { Start = b; Radius = point 4.0 5.0; XAxisRotation = 30.0<degree>; LargeArc = true; Sweep = true; End = a }
    Assert.Equal(expected, Segment.reverse arc)

[<Fact>]
let ``reverse segment swaps start and end`` () =
    let segment = CubicBezier(point 0.0 0.0, point 1.0 2.0, point 3.0 4.0, point 5.0 6.0)
    let reversed = Segment.reverse segment
    Assert.Equal(Segment.finish segment, Segment.start reversed)
    Assert.Equal(Segment.start segment, Segment.finish reversed)

[<Fact>]
let ``point pair similarity maps anchor points exactly`` () =
    let p1, p2 = point 1.0 2.0, point 4.0 2.0
    let q1, q2 = point 10.0 -5.0, point 10.0 1.0
    let transform = Affine.pointPairSimilarity p1 p2 q1 q2 |> Result.defaultWith (failwithf "%A")
    Assert.Equal(q1, Affine.point transform p1)
    Assert.Equal(q2, Affine.point transform p2)

[<Fact>]
let ``segment remap endpoints maps endpoints exactly`` () =
    let segment = CubicBezier(point 1.0 2.0, point 2.0 3.0, point 3.0 4.0, point 4.0 2.0)
    let newStart, newEnd = point 10.0 -5.0, point 10.0 1.0
    let remapped = Segment.remapEndpoints segment newStart newEnd |> Result.defaultWith (failwithf "%A")
    Assert.Equal(newStart, Segment.start remapped)
    Assert.Equal(newEnd, Segment.finish remapped)

[<Fact>]
let ``path point pair similarity maps arcs`` () =
    let sourceStart, sourceEnd = point 0.0 0.0, point 2.0 0.0
    let targetStart, targetEnd = point 10.0 20.0, point 10.0 24.0
    let source = Arc { Start = sourceStart; Radius = point 1.0 2.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = sourceEnd } |> Segment.asPath
    let remapped = Path.byPointPairSimilarity source sourceStart sourceEnd targetStart targetEnd |> Result.defaultWith (failwithf "%A")
    match remapped.Subpaths |> List.exactlyOne |> _.Segments |> List.exactlyOne with
    | Arc arc ->
        Assert.Equal(targetStart, arc.Start)
        Assert.Equal(targetEnd, arc.End)
        Assert.Equal(point 2.0 4.0, arc.Radius)
        Assert.Equal(90.0<degree>, arc.XAxisRotation)
        Assert.True(arc.Sweep)
    | segment -> failwithf "expected arc, got %A" segment

let private assertPointNear (expected: Point<'u>) (actual: Point<'u>) =
    Assert.True(Point.distance expected actual < LanguagePrimitives.FloatWithMeasure<'u> 1.0e-6, $"expected {expected}, got {actual}")

[<Fact>]
let ``segment point evaluates lines quadratics cubics and arcs`` () =
    let segmentsAndExpected =
        [ Line(point 0.0 0.0, point 10.0 20.0), point 5.0 10.0
          QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0), point 10.0 10.0
          CubicBezier(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0), point 15.0 22.5
          Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }, point 10.0 -10.0 ]
    for segment, expected in segmentsAndExpected do
        Segment.point segment 0.5<parameter> |> Result.defaultWith (failwithf "%A") |> assertPointNear expected

[<Fact>]
let ``segment derivative evaluates lines quadratics cubics and arcs`` () =
    let derivative segment = Segment.derivative segment 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (Point.create 10.0<length / parameter> 20.0<length / parameter>) (derivative (Line(point 0.0 0.0, point 10.0 20.0)))
    assertPointNear (Point.create 20.0<length / parameter> 0.0<length / parameter>) (derivative (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)))
    assertPointNear (Point.create 45.0<length / parameter> 0.0<length / parameter>) (derivative (CubicBezier(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)))
    let arc = Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    let arcDerivative = derivative arc
    Assert.True(arcDerivative.X > 0.0<length / parameter>)
    Assert.True(abs arcDerivative.Y < 1.0e-6<length / parameter>)

[<Fact>]
let ``segment second derivative evaluates arc analytically`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    let second = Segment.secondDerivative arc 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (Point.create 0.0<length / parameter^2> 98.69604401089359<length / parameter^2>) second

[<Fact>]
let ``segment bounding box handles lines beziers and arcs`` () =
    let check segment expectedMin expectedMax =
        let box = Segment.boundingBox segment |> Result.defaultWith (failwithf "%A")
        assertPointNear expectedMin box.Min
        assertPointNear expectedMax box.Max
    check (Line(point 1.0 2.0, point 5.0 -3.0)) (point 1.0 -3.0) (point 5.0 2.0)
    check (QuadraticBezier(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)) (point 0.0 0.0) (point 20.0 5.0)
    check (CubicBezier(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)) (point 0.0 0.0) (point 30.0 22.5)
    check (Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }) (point 0.0 -10.0) (point 20.0 0.0)

[<Fact>]
let ``bounding box dimensions use extents`` () =
    let box: BoundingBox = { Min = point -2.0 3.0; Max = point 8.0 15.0 }
    Assert.Equal(10.0<length>, BoundingBox.width box)
    Assert.Equal(12.0<length>, BoundingBox.height box)
    Assert.Equal(point 3.0 9.0, BoundingBox.center box)
    Assert.Equal(22.0<length>, BoundingBox.diameter box)

[<Fact>]
let ``bounding box union covers both boxes`` () =
    let first: BoundingBox = { Min = point 2.0 -3.0; Max = point 5.0 4.0 }
    let second: BoundingBox = { Min = point -7.0 6.0; Max = point -2.0 9.0 }
    let expected: BoundingBox = { Min = point -7.0 -3.0; Max = point 5.0 9.0 }
    Assert.Equal(expected, BoundingBox.union first second)

[<Fact>]
let ``bounding box union many covers every box`` () =
    let boxes: BoundingBox list =
        [ { Min = point 2.0 -3.0; Max = point 5.0 4.0 }
          { Min = point -7.0 6.0; Max = point -2.0 9.0 }
          { Min = point 3.0 -8.0; Max = point 4.0 -6.0 } ]
    let expected: BoundingBox = { Min = point -7.0 -8.0; Max = point 5.0 9.0 }
    Assert.Equal(Some expected, BoundingBox.unionMany boxes)

[<Fact>]
let ``bounding box union many returns none for empty lists`` () =
    Assert.Equal(None, BoundingBox.unionMany [])

[<Fact>]
let ``points bounding box covers every point`` () =
    let expected: BoundingBox = { Min = point -7.0 -8.0; Max = point 4.0 6.0 }
    Assert.Equal(
        Some expected,
        BoundingBox.ofPoints [ point 2.0 -3.0; point -7.0 6.0; point 4.0 -8.0 ])

[<Fact>]
let ``points bounding box returns none for empty lists`` () =
    Assert.Equal(None, BoundingBox.ofPoints [])

[<Fact>]
let ``segment bounding box returns degenerate arc errors`` () =
    let segment = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    Assert.Equal(Error DegenerateArc, Segment.boundingBox segment)

let private semicircle =
    Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }

[<Fact>]
let ``arc center data converts arc segments`` () =
    let arc = Segment.arcCenterData semicircle |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 10.0 0.0) arc.Center
    assertPointNear (point 10.0 10.0) arc.Radius
    Assert.Equal(0.0<degree>, arc.XAxisRotation)
    Assert.Equal(180.0<degree>, arc.StartAngle)
    Assert.Equal(180.0<degree>, arc.DeltaAngle)

[<Fact>]
let ``arc center data rejects non arc segments`` () =
    Assert.Equal(Error DegenerateArc, Segment.arcCenterData (Line(point 0.0 0.0, point 1.0 0.0)))

[<Fact>]
let ``arc wrappers use root points`` () =
    let sample = Segment.arcPoint semicircle 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    let derivative = Segment.arcDerivative semicircle 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    let anglePoint = Segment.arcPointAtAngle semicircle 270.0<degree> |> Result.defaultWith (failwithf "%A")
    let angleDerivative = Segment.arcDerivativeAtAngle semicircle 270.0<degree> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 10.0 -10.0) sample
    Assert.True(derivative.X > 0.0<length / parameter>)
    Assert.True(abs derivative.Y < 1.0e-6<length / parameter>)
    assertPointNear (point 10.0 -10.0) anglePoint
    Assert.True(angleDerivative.X > 0.0<length / degree>)
    Assert.True(abs angleDerivative.Y < 1.0e-6<length / degree>)
    Assert.Equal(270.0<degree>, Segment.arcAngleAt semicircle 0.5<parameter> |> Result.defaultWith (failwithf "%A"))
    Assert.Equal(360.0<degree>, Segment.arcEndAngle semicircle |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``arc wrappers reject non arc segments`` () =
    let line = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error DegenerateArc, Segment.arcPoint line 0.5<parameter>)
    Assert.Equal(Error DegenerateArc, Segment.arcDerivative line 0.5<parameter>)
    Assert.Equal(Error DegenerateArc, Segment.arcPointAtAngle line 0.0<degree>)
    Assert.Equal(Error DegenerateArc, Segment.arcDerivativeAtAngle line 0.0<degree>)
    Assert.Equal(Error DegenerateArc, Segment.arcAngleAt line 0.5<parameter>)
    Assert.Equal(Error DegenerateArc, Segment.arcEndAngle line)
