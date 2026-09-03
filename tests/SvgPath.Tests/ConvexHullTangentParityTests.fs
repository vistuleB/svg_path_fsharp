module SvgPath.Tests.ConvexHullTangentParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private nearPoint expected actual = Assert.True(Point.distance expected actual <= 1.0e-6<length>)
let private nearDegree expected actual = Assert.True(abs (actual - expected) <= 1.0e-6<degree>)

let private squareLoop () =
    [ Line(point 0.0 0.0, point 10.0 0.0)
      Line(point 10.0 0.0, point 10.0 10.0)
      Line(point 10.0 10.0, point 0.0 10.0)
      Line(point 0.0 10.0, point 0.0 0.0) ]

let private clockwiseSquareLoop () =
    [ Line(point 0.0 0.0, point 0.0 10.0)
      Line(point 0.0 10.0, point 10.0 10.0)
      Line(point 10.0 10.0, point 10.0 0.0)
      Line(point 10.0 0.0, point 0.0 0.0) ]

let private roundedTriangleLoop () =
    [ Line(point 0.0 0.0, point 10.0 0.0)
      QuadraticBezier(point 10.0 0.0, point 15.0 5.0, point 10.0 10.0)
      Line(point 10.0 10.0, point 0.0 0.0) ]

let private assertSeparation loop candidate expectedAngle expectedPoint =
    let angle, closest =
        ConvexHull.internalPointChordPolygonLoopSeparation loop candidate
        |> Option.defaultWith (fun () -> failwith "expected separation")
    nearDegree expectedAngle angle
    nearPoint expectedPoint closest

let private assertSplit expectedStart expectedFinish outsideCount insideCount loop candidate =
    let outside, inside =
        ConvexHull.internalPointExactLoopTangentSubpaths loop candidate
        |> Result.defaultWith (failwithf "%A")
    nearPoint expectedStart outside.Start
    nearPoint expectedFinish (Subpath.finish outside)
    Assert.Equal(outsideCount, outside.Segments.Length)
    nearPoint expectedFinish inside.Start
    nearPoint expectedStart (Subpath.finish inside)
    Assert.Equal(insideCount, inside.Segments.Length)

[<Fact>]
let ``point chord polygon loop separation returns none for inside polygon point`` () =
    Assert.Equal(None, ConvexHull.internalPointChordPolygonLoopSeparation (squareLoop ()) (point 5.0 5.0))

[<Fact>]
let ``point chord polygon loop separation returns none for boundary polygon point`` () =
    Assert.Equal(None, ConvexHull.internalPointChordPolygonLoopSeparation (squareLoop ()) (point 10.0 5.0))

[<Fact>]
let ``point chord polygon loop separation finds closest point on polygon edge`` () =
    assertSeparation (squareLoop ()) (point 15.0 5.0) 0.0<degree> (point 10.0 5.0)

[<Fact>]
let ``point chord polygon loop separation handles clockwise polygon`` () =
    assertSeparation (clockwiseSquareLoop ()) (point 15.0 5.0) 0.0<degree> (point 10.0 5.0)

[<Fact>]
let ``point chord polygon loop separation finds closest point on polygon vertex`` () =
    assertSeparation (squareLoop ()) (point 15.0 15.0) 45.0<degree> (point 10.0 10.0)

[<Fact>]
let ``point chord polygon loop separation handles point like loop`` () =
    let at = point 2.0 3.0
    assertSeparation [ Line(at, at); Line(at, at) ] (point 7.0 3.0) 0.0<degree> at

[<Fact>]
let ``point chord polygon loop separation handles line like loop`` () =
    let loop = [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 0.0 0.0) ]
    assertSeparation loop (point 5.0 4.0) 90.0<degree> (point 5.0 0.0)

[<Fact>]
let ``point loop view classifies clockwise outside arc point`` () =
    Assert.Equal(OutsidePoint, ConvexHull.internalPointLoopView (point 15.0 5.0) (point 10.0 5.0) (point 0.0 1.0) (point 0.0 1.0) true)

[<Fact>]
let ``point loop view classifies clockwise inside arc point`` () =
    Assert.Equal(InsidePoint, ConvexHull.internalPointLoopView (point 15.0 5.0) (point 0.0 5.0) (point 0.0 -1.0) (point 0.0 -1.0) true)

[<Fact>]
let ``point loop view classifies ccw tangent corner`` () =
    Assert.Equal(TangentPoint, ConvexHull.internalPointLoopView (point 15.0 5.0) (point 10.0 10.0) (point 0.0 1.0) (point -1.0 0.0) false)

[<Fact>]
let ``point loop view classifies counterclockwise outside arc point`` () =
    Assert.Equal(OutsidePoint, ConvexHull.internalPointLoopView (point 15.0 5.0) (point 10.0 5.0) (point 0.0 -1.0) (point 0.0 -1.0) false)

[<Fact>]
let ``point chord polygon tangent subpaths split square`` () =
    let outside, inside = ConvexHull.internalPointChordPolygonTangentSubpaths (squareLoop ()) (point 15.0 5.0) |> Result.defaultWith (failwithf "%A")
    nearPoint (point 10.0 0.0) outside.Start
    nearPoint (point 10.0 10.0) (Subpath.finish outside)
    Assert.Single(outside.Segments) |> ignore
    nearPoint (point 10.0 10.0) inside.Start
    nearPoint (point 10.0 0.0) (Subpath.finish inside)
    Assert.Equal(3, inside.Segments.Length)

[<Fact>]
let ``point chord polygon tangent subpaths reject nonconvex loop`` () =
    let loop =
        [ Line(point 0.0 0.0, point 10.0 0.0)
          Line(point 10.0 0.0, point 5.0 5.0)
          Line(point 5.0 5.0, point 10.0 10.0)
          Line(point 10.0 10.0, point 0.0 10.0)
          Line(point 0.0 10.0, point 0.0 0.0) ]
    Assert.Equal(Error(TangentSearchNonConvexVertex 2), ConvexHull.internalPointChordPolygonTangentSubpaths loop (point 15.0 5.0))

[<Fact>]
let ``point exact loop tangent subpaths split square`` () =
    assertSplit (point 10.0 0.0) (point 10.0 10.0) 1 3 (squareLoop ()) (point 15.0 5.0)

[<Fact>]
let ``point exact loop tangent subpaths finds quadratic interior tangencies`` () =
    let rootOffset = sqrt 15.0
    assertSplit (point 11.0 (5.0 - rootOffset)) (point 11.0 (5.0 + rootOffset)) 1 4 (roundedTriangleLoop ()) (point 14.0 5.0)

let private lineLikeLoop () =
    [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 0.0 0.0) ]

let private conflictingTangentLineLikeLoop () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    [ CubicBezier(a, point (10.0 / 3.0) (10.0 / 3.0), point (20.0 / 3.0) (10.0 / 3.0), b)
      CubicBezier(b, point (20.0 / 3.0) 0.0, point (-10.0 / 3.0) 0.0, a) ]

let private chordTangentFamily root scale =
    CubicBezier(
        point 0.0 0.0,
        point (scale / 3.0) 0.0,
        point (2.0 * scale / 3.0) (-2.0 * root * scale / 3.0),
        point scale ((1.0 - 2.0 * root) * scale))

let private assertMonotone segment clockwise =
    Assert.Equal(Ok(), ConvexHull.internalSegmentTangentMonotone segment clockwise)

let private assertNotMonotone segment clockwise byAtLeast =
    match ConvexHull.internalSegmentTangentMonotone segment clockwise with
    | Error violation -> Assert.True(violation >= byAtLeast)
    | Ok _ -> Assert.Fail "expected tangent-monotonicity violation"

[<Fact>]
let ``loop plus point hull replaces visible square edge`` () =
    let candidate = point 15.0 5.0
    let segments = ConvexHull.internalLoopPlusPointHull (squareLoop ()) candidate |> Result.defaultWith (failwithf "%A")
    Assert.Equal(5, segments.Length)
    let first, connectorToPoint, connectorFromPoint = segments[0], segments[3], segments[4]
    nearPoint (point 10.0 10.0) (Segment.start first)
    nearPoint (point 0.0 10.0) (Segment.finish first)
    nearPoint candidate (Segment.finish connectorToPoint)
    nearPoint candidate (Segment.start connectorFromPoint)
    nearPoint (point 10.0 10.0) (Segment.finish connectorFromPoint)

[<Fact>]
let ``loop plus point hull handles quadratic interior tangencies`` () =
    let candidate = point 14.0 5.0
    let segments = ConvexHull.internalLoopPlusPointHull (roundedTriangleLoop ()) candidate |> Result.defaultWith (failwithf "%A")
    let rootOffset = sqrt 15.0
    let lower, upper = point 11.0 (5.0 - rootOffset), point 11.0 (5.0 + rootOffset)
    Assert.Equal(6, segments.Length)
    nearPoint upper (Segment.start segments[0])
    nearPoint candidate (Segment.start (List.last segments))
    nearPoint upper (Segment.finish (List.last segments))
    Assert.Contains(segments, fun segment -> Point.distance (Segment.start segment) lower <= 1.0e-6<length> && Point.distance (Segment.finish segment) candidate <= 1.0e-6<length>)

[<Fact>]
let ``loop plus points hull absorbs outside points in order`` () =
    let candidates = [ point 5.0 5.0; point 15.0 5.0; point 5.0 15.0 ]
    let segments = ConvexHull.internalLoopPlusPointsHull (squareLoop ()) candidates |> Result.defaultWith (failwithf "%A")
    Assert.Equal(6, segments.Length)
    for candidate in candidates do Assert.Equal(None, ConvexHull.internalPointChordPolygonLoopSeparation segments candidate)

[<Fact>]
let ``loop plus point hull handles line like loop`` () =
    let candidate = point 5.0 4.0
    let segments = ConvexHull.internalLoopPlusPointHull (lineLikeLoop ()) candidate |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, segments.Length)
    Assert.Contains(segments, fun segment -> Segment.start segment = candidate || Segment.finish segment = candidate)

[<Fact>]
let ``loop plus point hull rejects conflicting tangent orientation`` () =
    Assert.Equal(Error TangentSearchDegenerateLoop, ConvexHull.internalLoopPlusPointHull (conflictingTangentLineLikeLoop ()) (point 5.0 4.0))

[<Fact>]
let ``point exact loop tangent subpaths finds cubic interior tangencies`` () =
    let loop =
        [ Line(point 0.0 0.0, point 10.0 0.0)
          CubicBezier(point 10.0 0.0, point 15.0 2.0, point 15.0 8.0, point 10.0 10.0)
          Line(point 10.0 10.0, point 0.0 0.0) ]
    assertSplit
        (point 13.510530985333089 3.4999239353568505)
        (point 13.510530985333087 6.5000760646431495)
        1 4 loop (point 14.0 5.0)

[<Fact>]
let ``cubic point tangent roots preserve non crossing root`` () =
    let segment = CubicBezier(point 0.0 0.0, point (1.0 / 3.0) 0.0, point (2.0 / 3.0) (1.0 / 3.0), point 1.0 1.0)
    let root = ConvexHull.internalCubicPointTangentRoots segment (point 0.37 0.1369) |> List.exactlyOne
    Assert.True(abs (Parameter.ratio root - 0.37) <= 1.0e-6)

[<Fact>]
let ``cubic chord tangent refinement reaches interior tangency`` () =
    let segment = CubicBezier(point 0.0 0.0, point (1.0 / 3.0) 0.0, point (2.0 / 3.0) (-1.0 / 3.0), point 1.0 0.0)
    let refined = ConvexHull.internalRefineChordTangent segment 0.47<parameter> 0.0<parameter>
    Assert.True(abs (Parameter.ratio refined - 0.5) <= 1.0e-6)

[<Fact>]
let ``cubic chord tangent refinement certifies in geometry space`` () =
    let scale = 1.0e12
    let segment = CubicBezier(point 0.0 0.0, point (scale / 3.0) 0.0, point (2.0 * scale / 3.0) (-0.74 * scale / 3.0), point scale (0.26 * scale))
    let refined = ConvexHull.internalRefineChordTangent segment 0.35<parameter> 0.0<parameter>
    Assert.True(abs (Parameter.ratio refined - 0.37) <= 1.0e-15)

[<Fact>]
let ``cubic chord polynomial refinement matches known family`` () =
    for expected in [ 0.12; 0.25; 0.37; 0.5; 0.73; 0.88 ] do
        for scale in [ 1.0e-6; 1.0; 1.0e12 ] do
            for delta in [ -0.05; 0.05 ] do
                let refined = ConvexHull.internalRefineChordTangent (chordTangentFamily expected scale) (Parameter.fromFloat (expected + delta)) 0.0<parameter>
                Assert.True(abs (Parameter.ratio refined - expected) <= 1.0e-9)

[<Fact>]
let ``cubic chord refinement is scale independent`` () =
    let refined = ConvexHull.internalRefineChordTangent (chordTangentFamily 0.37 1.0e-6) 0.32<parameter> 0.0<parameter>
    Assert.True(abs (Parameter.ratio refined - 0.37) <= 1.0e-9)

[<Fact>]
let ``cubic chord refinement ignores trivial endpoint root`` () =
    let refined = ConvexHull.internalRefineChordTangent (chordTangentFamily 0.12 1.0) 0.05<parameter> 0.0<parameter>
    Assert.True(abs (Parameter.ratio refined - 0.12) <= 1.0e-9)

[<Fact>]
let ``point exact loop tangent subpaths finds arc interior tangencies`` () =
    let loop =
        [ Line(point 0.0 0.0, point 10.0 0.0)
          Arc { Start = point 10.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 10.0 10.0 }
          Line(point 10.0 10.0, point 0.0 0.0) ]
    let rootOffset = sqrt 18.75
    assertSplit (point 12.5 (5.0 - rootOffset)) (point 12.5 (5.0 + rootOffset)) 1 4 loop (point 20.0 5.0)

[<Fact>]
let ``segment tangent monotone accepts lines`` () =
    let segment = Line(point 0.0 0.0, point 10.0 0.0)
    assertMonotone segment false
    assertMonotone segment true

[<Fact>]
let ``segment tangent monotone checks quadratic orientation`` () =
    let clockwise = QuadraticBezier(point 0.0 0.0, point 1.0 -1.0, point 2.0 0.0)
    let counterclockwise = QuadraticBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 0.0)
    assertMonotone clockwise true
    assertNotMonotone clockwise false 2.0
    assertMonotone counterclockwise false
    assertNotMonotone counterclockwise true 2.0

[<Fact>]
let ``segment tangent monotone accepts monotone cubic`` () =
    let segment = CubicBezier(point 1.0 0.0, point 1.0 0.5, point 0.5 1.0, point 0.0 1.0)
    assertMonotone segment true
    assertNotMonotone segment false 0.3

[<Fact>]
let ``segment tangent monotone rejects sign changing cubic`` () =
    let segment = CubicBezier(point 0.0 0.0, point 1.0 1.0, point 1.0 -1.0, point 2.0 0.0)
    assertNotMonotone segment false 4.0
    assertNotMonotone segment true 4.0

[<Fact>]
let ``segment tangent monotone checks arc sweep`` () =
    let clockwise = Arc { Start = point 1.0 0.0; Radius = point 1.0 1.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 0.0 1.0 }
    let counterclockwise = Arc { Start = point 1.0 0.0; Radius = point 1.0 1.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = false; End = point 0.0 -1.0 }
    assertMonotone clockwise true
    assertNotMonotone clockwise false 1.0
    assertMonotone counterclockwise false
    assertNotMonotone counterclockwise true 1.0
