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

let private mapPoint (value: Point<length>) = point (float value.X + 1.0) (float value.Y * 2.0)

[<Fact>]
let ``map segment points maps line quadratic and cubic defining points`` () =
    Assert.Equal(Ok(Line(point 1.0 2.0, point 3.0 6.0)), Segment.mapPoints mapPoint (Line(point 0.0 1.0, point 2.0 3.0)))
    Assert.Equal(Ok(QuadraticBezier(point 1.0 2.0, point 3.0 6.0, point 5.0 10.0)), Segment.mapPoints mapPoint (QuadraticBezier(point 0.0 1.0, point 2.0 3.0, point 4.0 5.0)))
    Assert.Equal(Ok(CubicBezier(point 1.0 2.0, point 3.0 6.0, point 5.0 10.0, point 7.0 14.0)), Segment.mapPoints mapPoint (CubicBezier(point 0.0 1.0, point 2.0 3.0, point 4.0 5.0, point 6.0 7.0)))

[<Fact>]
let ``map segment points rejects arcs`` () =
    Assert.Equal(Error CannotMapArcNonlinearly, Segment.mapPoints id semicircle)

[<Fact>]
let ``map subpath points maps segments and preserves closed state`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); QuadraticBezier(point 10.0 0.0, point 15.0 5.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let mapped = Subpath.mapPoints mapPoint source |> Result.defaultWith (failwithf "%A")
    Assert.True(mapped.Closed)
    Assert.Equal<Segment list>([ Line(point 1.0 0.0, point 11.0 0.0); QuadraticBezier(point 11.0 0.0, point 16.0 10.0, point 1.0 0.0) ], mapped.Segments)

[<Fact>]
let ``map subpath points maps empty subpath`` () =
    let mapped = Subpath.mapPoints (fun value -> Point.translate (point 1.0 1.0) value) (Subpath.empty (point 0.0 0.0)) |> Result.defaultWith (failwithf "%A")
    Assert.Empty(mapped.Segments)
    Assert.False(mapped.Closed)
    Assert.Equal(point 1.0 1.0, mapped.Start)

[<Fact>]
let ``map subpath points rejects arcs`` () =
    Assert.Equal(Error CannotMapArcNonlinearly, Subpath.mapPoints id (Subpath.ofSegment semicircle))

[<Fact>]
let ``map path points maps each subpath`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (CubicBezier(point 10.0 0.0, point 15.0 5.0, point 20.0 5.0, point 25.0 0.0))
    let mapped = Path.mapPoints (fun value -> Point.translate (point 1.0 1.0) value) (Path.ofSubpaths [ first; second ]) |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 1.0 1.0, point 11.0 1.0) ], mapped.Subpaths[0].Segments)
    Assert.Equal<Segment list>([ CubicBezier(point 11.0 1.0, point 16.0 6.0, point 21.0 6.0, point 26.0 1.0) ], mapped.Subpaths[1].Segments)

[<Fact>]
let ``map path points rejects arcs`` () =
    let source = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); Subpath.ofSegment semicircle ]
    Assert.Equal(Error CannotMapArcNonlinearly, Path.mapPoints id source)

[<Fact>]
let ``try map segment points maps line quadratic and cubic defining points`` () =
    let mapping value = Ok(mapPoint value)
    Assert.Equal(Ok(Line(point 1.0 2.0, point 3.0 6.0)), Segment.tryMapPoints mapping (Line(point 0.0 1.0, point 2.0 3.0)))
    Assert.Equal(Ok(QuadraticBezier(point 1.0 2.0, point 3.0 6.0, point 5.0 10.0)), Segment.tryMapPoints mapping (QuadraticBezier(point 0.0 1.0, point 2.0 3.0, point 4.0 5.0)))
    Assert.Equal(Ok(CubicBezier(point 1.0 2.0, point 3.0 6.0, point 5.0 10.0, point 7.0 14.0)), Segment.tryMapPoints mapping (CubicBezier(point 0.0 1.0, point 2.0 3.0, point 4.0 5.0, point 6.0 7.0)))

[<Fact>]
let ``try map segment points returns mapper error`` () =
    let mapping (_: Point<length>) = Error "failed"
    Assert.Equal(Error(PointMappingError "failed"), Segment.tryMapPoints mapping (Line(point 0.0 0.0, point 1.0 0.0)))

[<Fact>]
let ``try map segment points rejects arcs`` () =
    Assert.Equal(Error(PointMapSegmentError CannotMapArcNonlinearly), Segment.tryMapPoints Ok semicircle)

[<Fact>]
let ``try map path points maps each subpath`` () =
    let source = Path.ofSubpaths [ Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0)); Subpath.ofSegment (Line(point 1.0 2.0, point 3.0 4.0)) ]
    let mapped = Path.tryMapPoints (fun value -> Ok(Point.translate (point 1.0 1.0) value)) source |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 1.0 1.0, point 11.0 1.0) ], mapped.Subpaths[0].Segments)
    Assert.Equal<Segment list>([ Line(point 2.0 3.0, point 4.0 5.0) ], mapped.Subpaths[1].Segments)

[<Fact>]
let ``reverse subpath reverses segment order and preserves closed state`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let first, second, third = Line(a, b), Line(b, c), Line(c, a)
    let source = Subpath.create [ first; second; third ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    let reversed = Subpath.reverse source
    Assert.True(reversed.Closed)
    Assert.Equal<Segment list>([ Segment.reverse third; Segment.reverse second; Segment.reverse first ], reversed.Segments)

[<Fact>]
let ``reverse subpath preserves empty open subpath`` () =
    let empty = Subpath.empty (point 0.0 0.0)
    Assert.Equal(empty, Subpath.reverse empty)

[<Fact>]
let ``reverse path reverses subpaths and their segments`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let first = Subpath.create [ Line(a, b); Line(b, c) ] |> Result.defaultWith (failwithf "%A")
    let second = Subpath.ofSegment (Line(c, d))
    let reversed = Path.reverse (Path.ofSubpaths [ first; second ])
    Assert.Equal<Subpath list>([ Subpath.reverse second; Subpath.reverse first ], reversed.Subpaths)

[<Fact>]
let ``segment point and split extrapolate outside t`` () =
    let segment = Line(point 0.0 0.0, point 10.0 20.0)
    let sample = Segment.point segment -0.5<parameter> |> Result.defaultWith (failwithf "%A")
    let before, throughEnd = Segment.split segment -0.5<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point -5.0 -10.0) sample
    assertPointNear (point 0.0 0.0) (Segment.start before)
    assertPointNear (point -5.0 -10.0) (Segment.finish before)
    assertPointNear (point -5.0 -10.0) (Segment.start throughEnd)
    assertPointNear (point 10.0 20.0) (Segment.finish throughEnd)

[<Fact>]
let ``split segment divides quadratic`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    match Segment.split segment 0.25<parameter> |> Result.defaultWith (failwithf "%A") with
    | QuadraticBezier(a, b, c), QuadraticBezier(d, e, f) ->
        assertPointNear (point 0.0 0.0) a; assertPointNear (point 2.5 5.0) b
        assertPointNear (point 5.0 7.5) c; assertPointNear c d
        assertPointNear (point 12.5 15.0) e; assertPointNear (point 20.0 0.0) f
    | pair -> failwithf "expected quadratics, got %A" pair

[<Fact>]
let ``split segment divides arc`` () =
    let left, right = Segment.split semicircle 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 0.0 0.0) (Segment.start left)
    assertPointNear (point 10.0 -10.0) (Segment.finish left)
    assertPointNear (point 10.0 -10.0) (Segment.start right)
    assertPointNear (point 20.0 0.0) (Segment.finish right)

[<Fact>]
let ``split segment inside rejects outside t`` () =
    let segment = CubicBezier(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Equal(Error SplitOutsideSegment, Segment.splitInside segment -0.01<parameter>)
    Assert.Equal(Error SplitOutsideSegment, Segment.splitInside segment 1.01<parameter>)
    Assert.True(Segment.splitInside segment 0.0<parameter> |> Result.isOk)
    Assert.True(Segment.splitInside segment 1.0<parameter> |> Result.isOk)

[<Fact>]
let ``segment between returns segment between parameters`` () =
    let piece = Segment.between (Line(point 0.0 0.0, point 10.0 20.0)) 0.25<parameter> 0.75<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 2.5 5.0) (Segment.start piece)
    assertPointNear (point 7.5 15.0) (Segment.finish piece)

[<Fact>]
let ``segment between uses exact segment point endpoints`` () =
    let segment = CubicBezier(point 25.0 40.0, point 155.0 100.0, point 155.0 10.0, point 25.0 70.0)
    let piece = Segment.between segment 0.123<parameter> 0.876<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Segment.point segment 0.123<parameter>, Ok(Segment.start piece))
    Assert.Equal(Segment.point segment 0.876<parameter>, Ok(Segment.finish piece))

[<Fact>]
let ``segment between reverses when from is after to`` () =
    let piece = Segment.between (Line(point 0.0 0.0, point 10.0 20.0)) 0.75<parameter> 0.25<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 7.5 15.0) (Segment.start piece)
    assertPointNear (point 2.5 5.0) (Segment.finish piece)

[<Fact>]
let ``segment between returns degenerate line when parameters are equal`` () =
    let piece = Segment.between (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)) 0.25<parameter> 0.25<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Line(point 5.0 7.5, point 5.0 7.5), piece)

[<Fact>]
let ``segment between inside rejects outside t`` () =
    let segment = Line(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(Error SplitOutsideSegment, Segment.betweenInside segment -0.01<parameter> 0.5<parameter>)
    Assert.Equal(Error SplitOutsideSegment, Segment.betweenInside segment 0.5<parameter> 1.01<parameter>)
    Assert.True(Segment.betweenInside segment 0.0<parameter> 1.0<parameter> |> Result.isOk)
    Assert.True(Segment.betweenInside segment 1.0<parameter> 0.0<parameter> |> Result.isOk)

[<Fact>]
let ``segment between extrapolates outside t`` () =
    let piece = Segment.between (Line(point 0.0 0.0, point 10.0 20.0)) 1.0<parameter> 1.5<parameter> |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 10.0 20.0) (Segment.start piece)
    assertPointNear (point 15.0 30.0) (Segment.finish piece)

[<Fact>]
let ``segments between returns segments between adjacent parameters`` () =
    let pieces = Segment.betweenMany (Line(point 0.0 0.0, point 10.0 20.0)) [ 0.25<parameter>; 0.75<parameter>; 0.5<parameter> ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, pieces.Length)
    assertPointNear (point 2.5 5.0) (Segment.start pieces[0]); assertPointNear (point 7.5 15.0) (Segment.finish pieces[0])
    assertPointNear (point 7.5 15.0) (Segment.start pieces[1]); assertPointNear (point 5.0 10.0) (Segment.finish pieces[1])

[<Fact>]
let ``segments between does not add boundary parameters`` () =
    let pieces = Segment.betweenMany (Line(point 0.0 0.0, point 10.0 20.0)) [ 0.25<parameter>; 0.75<parameter> ] |> Result.defaultWith (failwithf "%A")
    Assert.Single(pieces) |> ignore
    assertPointNear (point 2.5 5.0) (Segment.start pieces[0]); assertPointNear (point 7.5 15.0) (Segment.finish pieces[0])

[<Fact>]
let ``segments between returns empty for too few parameters`` () =
    let segment = Line(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(Ok [], Segment.betweenMany segment [])
    Assert.Equal(Ok [], Segment.betweenMany segment [ 0.5<parameter> ])

[<Fact>]
let ``segments between inside rejects any outside t`` () =
    let segment = Line(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(Error SplitOutsideSegment, Segment.betweenManyInside segment [ 0.0<parameter>; 0.5<parameter>; 1.01<parameter> ])
    Assert.True(Segment.betweenManyInside segment [ 0.0<parameter>; 1.0<parameter> ] |> Result.isOk)

[<Fact>]
let ``segment eval and split return degenerate arc error`` () =
    let segment = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    Assert.Equal(Error DegenerateArc, Segment.point segment 0.5<parameter>)
    Assert.Equal(Error DegenerateArc, Segment.derivative segment 0.5<parameter>)
    Assert.Equal(Error DegenerateArc, Segment.split segment 0.5<parameter>)

[<Fact>]
let ``path can be built from empty`` () =
    let segment = Line(point 0.0 0.0, point 10.0 0.0)
    let subpath = Subpath.empty (point 0.0 0.0) |> Subpath.append segment |> Result.defaultWith (failwithf "%A")
    let path = Path.empty |> Path.append subpath
    Assert.Single(path.Subpaths) |> ignore
    Assert.Equal<Subpath list>([ subpath ], (Path.singleton subpath).Subpaths)

[<Fact>]
let ``widening as conversions preserve the complete geometry`` () =
    let segment = CubicBezier(point 0.0 0.0, point 1.0 2.0, point 2.0 2.0, point 3.0 0.0)
    let subpath = Segment.asSubpath segment
    Assert.Equal<Segment list>([ segment ], subpath.Segments)
    Assert.False(subpath.Closed)
    Assert.Equal(Segment.asPath segment, Path.singleton subpath)
    Assert.Equal(Ok subpath, Path.asSubpath (Path.singleton subpath))

[<Fact>]
let ``combine paths concatenates subpaths`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (Line(point 20.0 0.0, point 30.0 0.0))
    let empty = Subpath.empty (point 0.0 0.0)
    let combined = Path.combine [ Path.singleton first; Path.empty; Path.ofSubpaths [ empty; second ] ]
    Assert.Equal<Subpath list>([ first; empty; second ], combined.Subpaths)

[<Fact>]
let ``path map and filter subpaths compose after combine`` () =
    let first, zero, second = Line(point 0.0 0.0, point 10.0 0.0), Line(point 10.0 0.0, point 10.0 0.0), Line(point 10.0 0.0, point 20.0 0.0)
    let source = Subpath.create [ first; zero; second ] |> Result.defaultWith (failwithf "%A")
    let cleaned =
        Path.combine [ Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); source ]; Path.singleton (Subpath.empty (point 0.0 0.0)) ]
        |> Path.filterSubpaths (fun value -> not value.Segments.IsEmpty)
        |> Path.mapSubpaths Subpath.normalizeZeroLengthLines
    Assert.Equal<Segment list>([ first; second ], cleaned.Subpaths.Head.Segments)

[<Fact>]
let ``path start and end use first and last subpaths`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let path = Path.ofSubpaths [ Subpath.empty a; Subpath.ofSegment (Line(a, b)); Subpath.empty (point 0.0 0.0); Subpath.ofSegment (Line(c, d)); Subpath.empty d ]
    Assert.Equal(Ok a, Path.start path)
    Assert.Equal(Ok d, Path.finish path)

[<Fact>]
let ``subpath bounding box combines segment boxes`` () =
    let source = Subpath.create [ Line(point 1.0 2.0, point 5.0 -3.0); QuadraticBezier(point 5.0 -3.0, point 10.0 10.0, point 20.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let box = Subpath.boundingBox source |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 1.0 -3.0) box.Min
    assertPointNear (point 20.0 4.347826086956522) box.Max

[<Fact>]
let ``path bounding box uses nonempty subpaths`` () =
    let first = Subpath.ofSegment (Line(point 1.0 2.0, point 5.0 -3.0))
    let path = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); first; Subpath.empty (point 0.0 0.0); Subpath.ofSegment semicircle ]
    let box = Path.boundingBox path |> Result.defaultWith (failwithf "%A")
    assertPointNear (point 0.0 -10.0) box.Min
    assertPointNear (point 20.0 2.0) box.Max

[<Fact>]
let ``empty path has no start or end`` () =
    Assert.Equal(Error EmptyPath, Path.start Path.empty)
    Assert.Equal(Error EmptyPath, Path.finish Path.empty)
    Assert.Equal(Error EmptyPath, Path.boundingBox Path.empty)

[<Fact>]
let ``path with only empty subpaths has start and end`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let path = Path.ofSubpaths [ Subpath.empty a; Subpath.empty b ]
    Assert.Equal(Ok a, Path.start path)
    Assert.Equal(Ok b, Path.finish path)
    Assert.Equal(Error EmptySubpaths, Path.boundingBox path)

[<Fact>]
let ``as subpath rejects empty path`` () =
    Assert.Equal(Error EmptySubpaths, Path.asSubpath Path.empty)

[<Fact>]
let ``as subpath ignores empty subpaths`` () =
    let line = Line(point 0.0 0.0, point 10.0 0.0)
    let source = Subpath.ofSegment line
    let path = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); source; Subpath.empty (point 0.0 0.0) ]
    Assert.Equal(Ok source, Path.asSubpath path)

[<Fact>]
let ``as subpath rejects multiple nonempty subpaths`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (Line(point 20.0 0.0, point 30.0 0.0))
    Assert.Equal(Error MultipleNonemptySubpaths, Path.asSubpath (Path.ofSubpaths [ first; second ]))

[<Fact>]
let ``subpath can be built from empty`` () =
    let value = Subpath.empty (point 0.0 0.0) |> Subpath.append (Line(point 0.0 0.0, point 10.0 0.0)) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 0.0 0.0, Subpath.start value)
    Assert.Equal(point 10.0 0.0, Subpath.finish value)

[<Fact>]
let ``subpath rejects empty segment list`` () =
    Assert.Equal(Error EmptySubpath, Subpath.create [])

[<Fact>]
let ``polyline rejects empty and singleton point lists`` () =
    Assert.Equal(Error EmptySubpath, Subpath.polyline [])
    Assert.Equal(Error EmptySubpath, Subpath.polyline [ point 0.0 0.0 ])

[<Fact>]
let ``polyline builds open line subpath`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 20.0
    let value = Subpath.polyline [ a; b; c ] |> Result.defaultWith (failwithf "%A")
    Assert.False(value.Closed)
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c) ], value.Segments)

[<Fact>]
let ``assert polyline builds open line subpath`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let value = Subpath.assertPolyline [ a; b ]
    Assert.False(value.Closed)
    Assert.Equal<Segment list>([ Line(a, b) ], value.Segments)

[<Fact>]
let ``polygon rejects empty and singleton point lists`` () =
    Assert.Equal(Error EmptySubpath, Subpath.polygon [])
    Assert.Equal(Error EmptySubpath, Subpath.polygon [ point 0.0 0.0 ])

[<Fact>]
let ``polygon builds closed line subpath`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 20.0
    let value = Subpath.polygon [ a; b; c ] |> Result.defaultWith (failwithf "%A")
    Assert.True(value.Closed)
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, a) ], value.Segments)

[<Fact>]
let ``assert polygon builds closed line subpath`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let value = Subpath.assertPolygon [ a; b ]
    Assert.True(value.Closed)
    Assert.Equal<Segment list>([ Line(a, b); Line(b, a) ], value.Segments)

[<Fact>]
let ``polygon does not add zero length line when input already closes`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let value = Subpath.polygon [ a; b; a ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, a) ], value.Segments)

[<Fact>]
let ``empty subpath has start and end`` () =
    let at = point 0.0 0.0
    let empty = Subpath.empty at
    Assert.Equal(at, Subpath.start empty)
    Assert.Equal(at, Subpath.finish empty)
    Assert.Equal(Error EmptySubpath, Subpath.boundingBox empty)

[<Fact>]
let ``subpath rejects disconnected segments`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    Assert.Equal(Error(Discontinuous(0, 1, b, c, 10.0<length>)), Subpath.create [ Line(a, b); Line(c, d) ])

[<Fact>]
let ``subpath discontinuous error reports later segment indices`` () =
    let a, b, c, d, e, f = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 40.0 0.0, point 50.0 0.0
    Assert.Equal(Error(Discontinuous(1, 2, c, d, 10.0<length>)), Subpath.create [ Line(a, b); Line(b, c); Line(d, e); Line(e, f) ])

[<Fact>]
let ``assert subpath builds continuous segments`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let segments = [ Line(a, b); Line(b, c) ]
    Assert.Equal<Segment list>(segments, (Subpath.assertCreate segments).Segments)

[<Fact>]
let ``set closed false clears closed state without changing segments`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let segments = [ Line(a, b); Line(b, a) ]
    let closed = Subpath.assertCreate segments |> Subpath.assertSetClosed true
    let opened = Subpath.setClosed false closed |> Result.defaultWith (failwithf "%A")
    Assert.False(opened.Closed)
    Assert.Equal<Segment list>(segments, opened.Segments)

[<Fact>]
let ``set closed false accepts open and empty subpaths`` () =
    let empty = Subpath.empty (point 0.0 0.0)
    let openSubpath = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Ok empty, Subpath.setClosed false empty)
    Assert.Equal(Ok openSubpath, Subpath.setClosed false openSubpath)

[<Fact>]
let ``set closed false opens subpath`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    let opened = Subpath.setClosed false closed |> Result.defaultWith (failwithf "%A")
    Assert.False(opened.Closed)
    Assert.Equal<Segment list>(closed.Segments, opened.Segments)

[<Fact>]
let ``set closed true closes matching subpath`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let value = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.setClosed true |> Result.defaultWith (failwithf "%A")
    Assert.True(value.Closed)

[<Fact>]
let ``set closed true rejects uncloseable subpath`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    Assert.Equal(Error(Discontinuous(1, 0, a, c, sqrt 200.0 * 1.0<length>)), Subpath.setClosed true source)

[<Fact>]
let ``set closed with wiggle true reconciles nearby endpoints`` () =
    let a, b, nearA = point 0.0 0.0, point 10.0 0.0, point 0.0000000001 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, nearA) ]
    let closed = Subpath.setClosedWith Wiggle true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed)
    Assert.Equal(Subpath.start closed, Subpath.finish closed)

[<Fact>]
let ``set closed with wiggle true rejects gaps beyond tolerance`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 0.1 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    Assert.Equal(Error(Discontinuous(1, 0, a, c, 0.1<length>)), Subpath.setClosedWith Wiggle true source)

[<Fact>]
let ``set closed with wiggle false opens subpath`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    let opened = Subpath.setClosedWith Wiggle false closed |> Result.defaultWith (failwithf "%A")
    Assert.False(opened.Closed)

[<Fact>]
let ``append segment rejects closed subpath`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    Assert.Equal(Error AlreadyClosed, Subpath.append (Line(a, c)) closed)

[<Fact>]
let ``append segment with wiggle rejects start gaps beyond tolerance`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    Assert.Equal(Error(Discontinuous(-1, 0, a, b, 10.0<length>)), Subpath.appendWith Wiggle (Line(b, c)) (Subpath.empty a))

[<Fact>]
let ``subpath with wiggle replaces nearby sequential endpoints`` () =
    let a, b, nearB, c = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0, point 20.0 0.0
    let value = Subpath.createWith Wiggle [ Line(a, b); Line(nearB, c) ] |> Result.defaultWith (failwithf "%A")
    let first, second = value.Segments[0], value.Segments[1]
    let meeting = Segment.finish first
    Assert.Equal(a, Segment.start first); Assert.Equal(meeting, Segment.start second); Assert.Equal(c, Segment.finish second)
    Assert.NotEqual(b, meeting); Assert.NotEqual(nearB, meeting)

[<Fact>]
let ``subpath with wiggle rejects empty and accepts single segment inputs`` () =
    let line = Line(point 0.0 0.0, point 10.0 0.0)
    Assert.Equal(Error EmptySubpath, Subpath.createWith Wiggle [])
    Assert.Equal<Segment list>([ line ], (Subpath.createWith Wiggle [ line ] |> Result.defaultWith (failwithf "%A")).Segments)

let private coalesceLines =
    Custom(fun previous next closing ->
        match previous, next, closing with
        | Line(startPoint, _), Line(_, endPoint), false -> [ Line(startPoint, endPoint) ]
        | _, _, true -> [ previous ]
        | _ -> [ previous; next ])

[<Fact>]
let ``subpath rebuild with applies policy to existing segments`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    let rebuilt = Subpath.rebuildWith coalesceLines source |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, c) ], rebuilt.Segments)
    Assert.False(rebuilt.Closed)

[<Fact>]
let ``subpath rebuild with preserves closed state`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ] |> Subpath.assertSetClosed true
    let rebuilt = Subpath.rebuildWith Strict closed |> Result.defaultWith (failwithf "%A")
    Assert.True(rebuilt.Closed); Assert.Equal<Segment list>(closed.Segments, rebuilt.Segments)

[<Fact>]
let ``path rebuild with rebuilds each subpath`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let empty = Subpath.empty a
    let source = Path.ofSubpaths [ empty; Subpath.assertCreate [ Line(a, b); Line(b, c) ]; Subpath.ofSegment (Line(c, d)) ]
    let rebuilt = Path.rebuildWith coalesceLines source |> Result.defaultWith (failwithf "%A")
    Assert.Equal(empty, rebuilt.Subpaths[0]); Assert.Equal<Segment list>([ Line(a, c) ], rebuilt.Subpaths[1].Segments)
    Assert.Equal<Segment list>([ Line(c, d) ], rebuilt.Subpaths[2].Segments)

[<Fact>]
let ``subpath with wiggle then line prefers wiggle`` () =
    let a, b, nearB, c = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0, point 20.0 0.0
    let value = Subpath.createWith WiggleThenBridge [ Line(a, b); Line(nearB, c) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, value.Segments.Length); Assert.Equal(Segment.finish value.Segments[0], Segment.start value.Segments[1])

[<Fact>]
let ``subpath with wiggle then line falls back to bridge line`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let value = Subpath.createWith WiggleThenBridge [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], value.Segments)

[<Fact>]
let ``clean subpath removes zero length lines`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let first, second = Line(a, b), Line(b, c)
    let cleaned = Subpath.assertCreate [ first; Line(b, b); second ] |> Subpath.normalizeZeroLengthLines
    Assert.Equal<Segment list>([ first; second ], cleaned.Segments)

[<Fact>]
let ``clean subpath keeps single zero length line`` () =
    let a = point 0.0 0.0
    let zero = Line(a, a)
    Assert.Equal<Segment list>([ zero ], (Subpath.assertCreate [ zero ] |> Subpath.normalizeZeroLengthLines).Segments)

[<Fact>]
let ``clean subpath reduces multiple zero length lines to one`` () =
    let a = point 0.0 0.0
    let zero = Line(a, a)
    Assert.Equal<Segment list>([ zero ], (Subpath.assertCreate [ zero; zero ] |> Subpath.normalizeZeroLengthLines).Segments)

[<Fact>]
let ``clean subpath preserves closed state`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let cleaned = Subpath.assertCreate [ Line(a, b); Line(b, a); Line(a, a) ] |> Subpath.assertSetClosed true |> Subpath.normalizeZeroLengthLines
    Assert.True(cleaned.Closed); Assert.Equal<Segment list>([ Line(a, b); Line(b, a) ], cleaned.Segments)

[<Fact>]
let ``segment is zero length detects exact zero lines`` () =
    let a = point 0.0 0.0
    Assert.Equal(Ok true, Segment.isZeroLength (Line(a, a)) 0.0<length>)
    Assert.Equal(Ok false, Segment.isZeroLength (Line(a, point 1.0 0.0)) 0.0<length>)

[<Fact>]
let ``segment is zero length uses tolerance`` () =
    let short = Line(point 0.0 0.0, point 0.001 0.0)
    Assert.Equal(Ok false, Segment.isZeroLength short 0.0009<length>); Assert.Equal(Ok true, Segment.isZeroLength short 0.0011<length>)

[<Fact>]
let ``segment is zero length detects collapsed cubic`` () =
    let a = point 2.0 3.0
    Assert.Equal(Ok true, Segment.isZeroLength (CubicBezier(a, a, a, a)) 0.0<length>)

[<Fact>]
let ``subpath is zero length requires non empty subpath`` () =
    Assert.Equal(Ok false, Subpath.isZeroLength (Subpath.empty (point 0.0 0.0)) 0.0<length>)

[<Fact>]
let ``subpath is zero length checks every segment`` () =
    let a, b = point 0.0 0.0, point 1.0 0.0
    let zero, nonzero = Line(a, a), Line(a, b)
    Assert.Equal(Ok true, Subpath.isZeroLength (Subpath.assertCreate [ zero ]) 0.0<length>)
    Assert.Equal(Ok true, Subpath.isZeroLength (Subpath.assertCreate [ zero; zero ]) 0.0<length>)
    let mixed = Subpath.createWith Wiggle [ zero; nonzero ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok false, Subpath.isZeroLength mixed 0.0<length>)

[<Fact>]
let ``zero length predicates reject negative tolerance`` () =
    let a = point 0.0 0.0
    Assert.Equal(Error(InvalidZeroLengthTolerance -0.1<length>), Segment.isZeroLength (Line(a, a)) -0.1<length>)

[<Fact>]
let ``path map subpaths maps each subpath`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let first, zero, second, third = Line(a, b), Line(b, b), Line(b, c), Line(c, d)
    let path = Path.ofSubpaths [ Subpath.empty a; Subpath.assertCreate [ first; zero; second ]; Subpath.empty a; Subpath.ofSegment third ]
    let cleaned = Path.mapSubpaths Subpath.normalizeZeroLengthLines path
    Assert.Equal(Subpath.empty a, cleaned.Subpaths[0]); Assert.Equal<Segment list>([ first; second ], cleaned.Subpaths[1].Segments)
    Assert.Equal(Subpath.empty a, cleaned.Subpaths[2]); Assert.Equal<Segment list>([ third ], cleaned.Subpaths[3].Segments)

[<Fact>]
let ``path filter subpaths keeps matching subpaths`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (Line(point 20.0 0.0, point 30.0 0.0))
    let source = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); first; Subpath.empty (point 0.0 0.0); second ]
    Assert.Equal<Subpath list>([ first; second ], (Path.filterSubpaths (fun value -> not value.Segments.IsEmpty) source).Subpaths)

[<Fact>]
let ``path filter subpaths can return empty path`` () =
    let source = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); Subpath.empty (point 0.0 0.0) ]
    Assert.Equal(Path.empty, Path.filterSubpaths (fun value -> not value.Segments.IsEmpty) source)

[<Fact>]
let ``splice replaces segment range`` () =
    let a, b, c, d, e = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 40.0 0.0
    let first, replacement, last = Line(a, b), Line(b, d), Line(d, e)
    let source = Subpath.assertCreate [ first; Line(b, c); Line(c, d); last ]
    let spliced = Subpath.splice 1 2 [ replacement ] source |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ first; replacement; last ], spliced.Segments)

[<Fact>]
let ``splice inserts without deleting`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let first, inserted = Line(a, b), Line(b, c)
    let spliced = Subpath.ofSegment first |> Subpath.splice 1 0 [ inserted ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ first; inserted ], spliced.Segments)

[<Fact>]
let ``splice deletes through end when delete is too large`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let first = Line(a, b)
    let source = Subpath.assertCreate [ first; Line(b, c) ]
    Assert.Equal<Segment list>([ first ], (Subpath.splice 1 99 [] source |> Result.defaultWith (failwithf "%A")).Segments)

[<Fact>]
let ``splice rejects invalid bounds`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Error(InvalidSplice(-1, 0, 1)), Subpath.splice -1 0 [] source)
    Assert.Equal(Error(InvalidSplice(2, 0, 1)), Subpath.splice 2 0 [] source)
    Assert.Equal(Error(InvalidSplice(0, -1, 1)), Subpath.splice 0 -1 [] source)

[<Fact>]
let ``splice rejects discontinuous result`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    Assert.Equal(Error(Discontinuous(0, 1, b, d, 20.0<length>)), Subpath.splice 1 1 [ Line(d, c) ] source)

[<Fact>]
let ``splice with wiggle reconciles nearby endpoint gaps`` () =
    let a, b, nearB, c = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0, point 20.0 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    match Subpath.splice 1 1 [ Line(nearB, c) ] source with
    | Error(Discontinuous(0, 1, expected, actual, distance)) ->
        Assert.Equal(b, expected); Assert.Equal(nearB, actual); Assert.True(distance < 1.0e-9<length>)
    | result -> failwithf "expected discontinuity, got %A" result
    let spliced = Subpath.spliceWith Wiggle 1 1 [ Line(nearB, c) ] source |> Result.defaultWith (failwithf "%A")
    Assert.Equal(c, Subpath.finish spliced)
    Assert.True(spliced.Segments |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))

[<Fact>]
let ``splice with wiggle preserves closed state with nearby endpoint gap`` () =
    let a, b, c, nearA = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 0.0000000001 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, a) ] |> Subpath.assertSetClosed true
    let spliced = Subpath.spliceWith Wiggle 2 1 [ Line(c, nearA) ] source |> Result.defaultWith (failwithf "%A")
    Assert.True(spliced.Closed); Assert.Equal(Subpath.start spliced, Subpath.finish spliced)

[<Fact>]
let ``splice with wiggle reuses splice bounds errors`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Error(InvalidSplice(2, 0, 1)), Subpath.spliceWith Wiggle 2 0 [] source)

[<Fact>]
let ``splice preserves closed state`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, a) ] |> Subpath.assertSetClosed true
    let spliced = Subpath.splice 1 1 [ Line(b, c) ] source |> Result.defaultWith (failwithf "%A")
    Assert.True(spliced.Closed)

[<Fact>]
let ``splice allows closed empty result`` () =
    let a = point 0.0 0.0
    let source = Subpath.ofSegment (Line(a, a)) |> Subpath.assertSetClosed true
    Assert.Equal(Ok(Subpath.empty a |> Subpath.assertSetClosed true), Subpath.splice 0 1 [] source)

[<Fact>]
let ``segment arcs to cubic beziers preserves lines`` () =
    let line = Line(point 0.0 0.0, point 9.0 0.0)
    Assert.Equal<Segment list>([ line ], Segment.arcsToCubicBeziers line)

[<Fact>]
let ``segment to cubic beziers converts line exactly`` () =
    let a, d = point 0.0 0.0, point 9.0 0.0
    Assert.Equal<Segment list>([ CubicBezier(a, point 3.0 0.0, point 6.0 0.0, d) ], Segment.toCubicBeziers (Line(a, d)))

[<Fact>]
let ``segment arcs to cubic beziers preserves quadratics`` () =
    let a, b, c, d = point 0.0 0.0, point 3.0 6.0, point 9.0 0.0, point 12.0 4.0
    let quadratic = QuadraticBezier(a, b, c)
    Assert.Equal<Segment list>([ quadratic ], Segment.arcsToCubicBeziers quadratic)

[<Fact>]
let ``segment arcs to cubic beziers preserves cubics`` () =
    let a, b, c, d = point 0.0 0.0, point 3.0 6.0, point 9.0 0.0, point 12.0 4.0
    let cubic = CubicBezier(a, b, d, c)
    Assert.Equal<Segment list>([ cubic ], Segment.arcsToCubicBeziers cubic)

[<Fact>]
let ``segment to cubic beziers converts quadratic exactly`` () =
    let a, b, c = point 0.0 0.0, point 3.0 6.0, point 9.0 0.0
    Assert.Equal<Segment list>([ CubicBezier(a, point 2.0 4.0, point 5.0 4.0, c) ], Segment.toCubicBeziers (QuadraticBezier(a, b, c)))

[<Fact>]
let ``segment arcs to cubic beziers splits half turn`` () =
    let a, b = point 0.0 0.0, point 20.0 0.0
    let pieces = Segment.arcsToCubicBeziers (Arc { Start = a; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = b })
    Assert.Equal(2, pieces.Length); Assert.Equal(a, Segment.start pieces.Head); Assert.Equal(b, Segment.finish pieces[pieces.Length - 1])

[<Fact>]
let ``segment arcs to cubic beziers large arc uses more than two cubics`` () =
    let pieces = Segment.arcsToCubicBeziers (Arc { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = true; Sweep = true; End = point 10.0 10.0 })
    Assert.True(pieces.Length > 2)
    Assert.True(pieces |> List.forall (function CubicBezier _ -> true | _ -> false))
    Assert.True(pieces |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))

[<Fact>]
let ``segment arcs to cubic beziers degenerate arc falls back to line cubic`` () =
    let a, d = point 0.0 0.0, point 9.0 0.0
    let arc = Arc { Start = a; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = d }
    Assert.Equal<Segment list>([ CubicBezier(a, point 3.0 0.0, point 6.0 0.0, d) ], Segment.arcsToCubicBeziers arc)

[<Fact>]
let ``subpath arcs to cubic beziers preserves closed state`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    let converted = Subpath.arcsToCubicBeziers source
    Assert.True(converted.Closed); Assert.Equal<Segment list>(source.Segments, converted.Segments)

[<Fact>]
let ``subpath arcs to cubic beziers replaces only arcs`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let line = Line(a, b)
    let arc = Arc { Start = b; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = c }
    let quadratic = QuadraticBezier(c, c, d)
    let converted = Subpath.assertCreate [ line; arc; quadratic ] |> Subpath.arcsToCubicBeziers
    Assert.Equal(a, Segment.start converted.Segments.Head)
    Assert.Equal(d, Segment.finish converted.Segments[converted.Segments.Length - 1])
    Assert.DoesNotContain(converted.Segments, fun segment -> match segment with Arc _ -> true | _ -> false)
    Assert.Contains(line, converted.Segments); Assert.Contains(quadratic, converted.Segments)
    Assert.True(converted.Segments |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))

[<Fact>]
let ``subpath to cubic beziers preserves closed state`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let converted = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true |> Subpath.toCubicBeziers
    Assert.True(converted.Closed)
    Assert.True(converted.Segments |> List.forall (function CubicBezier _ -> true | _ -> false))

[<Fact>]
let ``path arcs to cubic beziers converts each subpath`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let arc = Arc { Start = a; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = b }
    let line = Line(c, d)
    let segments = Path.ofSubpaths [ Subpath.ofSegment arc; Subpath.ofSegment line ] |> Path.arcsToCubicBeziers |> _.Subpaths |> List.collect _.Segments
    Assert.DoesNotContain(segments, fun segment -> match segment with Arc _ -> true | _ -> false)
    Assert.Contains(line, segments)

[<Fact>]
let ``path to cubic beziers converts each subpath`` () =
    let path = Path.ofSubpaths [ Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0)); Subpath.ofSegment (Line(point 20.0 0.0, point 30.0 0.0)) ]
    let segments = path |> Path.toCubicBeziers |> _.Subpaths |> List.collect _.Segments
    Assert.True(segments |> List.forall (function CubicBezier _ -> true | _ -> false))

[<Fact>]
let ``segment to lines preserves lines`` () =
    let line = Line(point 0.0 0.0, point 10.0 5.0)
    Assert.Equal(Ok [ line ], Segment.toLines line)

[<Fact>]
let ``segment to lines approximates beziers within tolerance`` () =
    let options = { Tolerance = 0.05<length>; MaxDepth = 20 }
    let curves =
        [ QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
          CubicBezier(point 0.0 0.0, point 0.0 20.0, point 20.0 20.0, point 20.0 0.0) ]
    for curve in curves do
        let lines = Segment.toLinesWith options curve |> Result.defaultWith (failwithf "%A")
        Assert.True(lines |> List.forall (function Line _ -> true | _ -> false))
        Assert.True(lines |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))
        for index in 0 .. 500 do
            let sample = Segment.point curve (Parameter.fromFloat (float index / 500.0)) |> Result.defaultWith (failwithf "%A")
            let distance = lines |> List.map (fun line -> Segment.distance line sample |> Result.defaultWith (failwithf "%A")) |> List.min
            Assert.True(distance <= options.Tolerance, $"sample {index} was {distance} from the line approximation")

[<Fact>]
let ``segment to lines detects collinear control overshoot`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 20.0 0.0, point 10.0 0.0)
    let lines = Segment.toLinesWith { Tolerance = 0.01<length>; MaxDepth = 20 } curve |> Result.defaultWith (failwithf "%A")
    Assert.True(lines.Length > 1)
    Assert.Contains(lines, fun segment -> (Segment.finish segment).X > 10.0<length>)

[<Fact>]
let ``segment to lines approximates arcs within tolerance`` () =
    let tolerance = 0.05<length>
    let arc = Arc { Start = point 0.0 0.0; Radius = point 10.0 5.0; XAxisRotation = 30.0<degree>; LargeArc = true; Sweep = true; End = point 20.0 0.0 }
    let lines = Segment.toLinesWith { Tolerance = tolerance; MaxDepth = 20 } arc |> Result.defaultWith (failwithf "%A")
    Assert.True(lines |> List.forall (function Line _ -> true | _ -> false))
    Assert.Equal(Segment.start arc, Segment.start lines.Head)
    Assert.Equal(Segment.finish arc, Segment.finish lines[lines.Length - 1])
    for index in 0 .. 500 do
        let sample = Segment.point arc (Parameter.fromFloat (float index / 500.0)) |> Result.defaultWith (failwithf "%A")
        let distance = lines |> List.map (fun line -> Segment.distance line sample |> Result.defaultWith (failwithf "%A")) |> List.min
        Assert.True(distance <= tolerance, $"sample {index} was {distance} from the line approximation")

[<Fact>]
let ``segment to lines degenerate arc falls back to line`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let arc = Arc { Start = a; Radius = point 0.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = b }
    Assert.Equal(Ok [ Line(a, b) ], Segment.toLines arc)

[<Fact>]
let ``segment to lines tighter tolerance does not use fewer lines`` () =
    let curve = CubicBezier(point 0.0 0.0, point 0.0 20.0, point 20.0 20.0, point 20.0 0.0)
    let count tolerance = Segment.toLinesWith { Tolerance = tolerance; MaxDepth = 20 } curve |> Result.defaultWith (failwithf "%A") |> List.length
    Assert.True(count 0.1<length> >= count 1.0<length>)

[<Fact>]
let ``segment to lines rejects invalid options and depth exhaustion`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    Assert.Equal(Error(InvalidLinearizeTolerance 0.0<length>), Segment.toLinesWith { Tolerance = 0.0<length>; MaxDepth = 20 } curve)
    Assert.Equal(Error(InvalidLinearizeMaxDepth 0), Segment.toLinesWith { Tolerance = 0.1<length>; MaxDepth = 0 } curve)
    match Segment.toLinesWith { Tolerance = 1.0e-12<length>; MaxDepth = 1 } curve with
    | Error(LinearizeMaxDepthReached error) -> Assert.True(error > 1.0e-12<length>)
    | result -> failwithf "expected depth exhaustion, got %A" result

[<Fact>]
let ``subpath and path to lines preserve topology`` () =
    let a = point 0.0 0.0
    let closed = Subpath.ofSegment (QuadraticBezier(a, point 10.0 20.0, a)) |> Subpath.assertSetClosed true
    let moveOnly = Subpath.empty (point 30.0 40.0)
    let converted = Path.ofSubpaths [ moveOnly; closed ] |> Path.toLines |> Result.defaultWith (failwithf "%A")
    let convertedMove, convertedClosed = converted.Subpaths[0], converted.Subpaths[1]
    Assert.Empty(convertedMove.Segments); Assert.Equal(Subpath.start moveOnly, Subpath.start convertedMove)
    Assert.True(convertedClosed.Closed)
    Assert.True(convertedClosed.Segments |> List.forall (function Line _ -> true | _ -> false))
    Assert.True(convertedClosed.Segments |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))
    Assert.Equal(Subpath.start convertedClosed, Subpath.finish convertedClosed)

[<Fact>]
let ``subpath with wiggle rejects gaps beyond tolerance`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.1 0.0, point 20.0 0.0
    Assert.Equal(Error(Discontinuous(0, 1, b, c, 0.09999999999999964<length>)), Subpath.createWith Wiggle [ Line(a, b); Line(c, d) ])

[<Fact>]
let ``subpath with custom wiggle tolerance accepts larger gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.1 0.0, point 20.0 0.0
    let subpath = Subpath.createWith (WiggleWith 0.2<length>) [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(d, Subpath.finish subpath)

[<Fact>]
let ``subpath with custom wiggle then bridge tolerance accepts larger gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.1 0.0, point 20.0 0.0
    let subpath = Subpath.createWith (WiggleThenBridgeWith 0.2<length>) [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(d, Subpath.finish subpath)

[<Fact>]
let ``subpath with rejects negative custom wiggle tolerance`` () =
    let segment = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error(InvalidWiggleTolerance -0.1<length>), Subpath.createWith (WiggleWith -0.1<length>) [ segment ])

[<Fact>]
let ``subpath with rejects negative wiggle then bridge tolerance`` () =
    let first, second = Line(point 0.0 0.0, point 1.0 0.0), Line(point 2.0 0.0, point 3.0 0.0)
    Assert.Equal(Error(InvalidWiggleTolerance -0.1<length>), Subpath.createWith (WiggleThenBridgeWith -0.1<length>) [ first; second ])

[<Fact>]
let ``subpath with wiggle bridges misaligned vertical lines`` () =
    let a, b, c, d = point 0.0 0.0, point 0.0 10.0, point 0.0000000001 10.0000000001, point 0.0000000001 20.0
    let subpath = Subpath.createWith Wiggle [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], subpath.Segments)

[<Fact>]
let ``subpath with wiggle bridges misaligned horizontal lines`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0000000001, point 20.0 0.0000000001
    let subpath = Subpath.createWith Wiggle [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], subpath.Segments)

[<Fact>]
let ``append segment discontinuous error reports segment indices`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    Assert.Equal(Error(Discontinuous(0, 1, b, c, 10.0<length>)), Subpath.append (Line(c, d)) (Subpath.ofSegment (Line(a, b))))

[<Fact>]
let ``join combines open subpaths`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let joined = [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(b, c)); Subpath.ofSegment (Line(c, d)) ] |> Subpath.join |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], joined.Segments)

[<Fact>]
let ``join treats empty open subpaths as identity values`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let subpath, emptyStart, emptyEnd = Subpath.ofSegment (Line(a, b)), Subpath.empty a, Subpath.empty b
    Assert.Equal(Error EmptySubpath, Subpath.join [])
    Assert.Equal(Ok subpath, Subpath.join [ emptyStart; subpath ])
    Assert.Equal(Ok subpath, Subpath.join [ subpath; emptyEnd ])
    Assert.Equal(Ok emptyStart, Subpath.join [ emptyStart; emptyEnd ])

[<Fact>]
let ``join treats interleaved empty subpaths as identity values`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0
    let first, second, empty = Subpath.ofSegment (Line(a, b)), Subpath.ofSegment (Line(b, c)), Subpath.empty a
    let joined = Subpath.join [ empty; first; empty; second; empty ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c) ], joined.Segments)

[<Fact>]
let ``join rejects discontinuous subpaths`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    Assert.Equal(Error(Discontinuous(0, 1, b, c, 10.0<length>)), Subpath.join [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(c, d)) ])

[<Fact>]
let ``join discontinuity reports flattened segment indices`` () =
    let a, b, c, d, e = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 40.0 0.0
    let first = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    Assert.Equal(Error(Discontinuous(1, 2, c, d, 10.0<length>)), Subpath.join [ first; Subpath.ofSegment (Line(d, e)) ])

[<Fact>]
let ``join rejects closed inputs`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let opened = Subpath.ofSegment (Line(a, b))
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    Assert.Equal(Error AlreadyClosed, Subpath.join [ closed; opened ])
    Assert.Equal(Error AlreadyClosed, Subpath.join [ opened; closed ])

[<Fact>]
let ``join with wiggle reconciles nearby endpoint gap`` () =
    let a, b, nearB, c = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0, point 20.0 0.0
    let joined = Subpath.joinWith Wiggle [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(nearB, c)) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(a, Subpath.start joined); Assert.Equal(c, Subpath.finish joined)
    Assert.True(joined.Segments |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))

[<Fact>]
let ``join with wiggle rejects closed inputs`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let opened = Subpath.ofSegment (Line(a, b))
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    Assert.Equal(Error AlreadyClosed, Subpath.joinWith Wiggle [ closed; opened ])
    Assert.Equal(Error AlreadyClosed, Subpath.joinWith Wiggle [ opened; closed ])

[<Fact>]
let ``join with bridge bridges a gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let joined = Subpath.joinWith Bridge [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(c, d)) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], joined.Segments)

[<Fact>]
let ``join with bridge rejects closed inputs`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let opened = Subpath.ofSegment (Line(a, b))
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    Assert.Equal(Error AlreadyClosed, Subpath.joinWith Bridge [ closed; opened ])
    Assert.Equal(Error AlreadyClosed, Subpath.joinWith Bridge [ opened; closed ])

[<Fact>]
let ``subpath with custom reconciles a gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let policy = Custom(fun previous next _ -> [ Segment.withFinish c previous; next ])
    let subpath = Subpath.createWith policy [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, c); Line(c, d) ], subpath.Segments)

[<Fact>]
let ``subpath with custom can insert a connector`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let policy = Custom(fun previous next _ -> [ previous; Line(Segment.finish previous, Segment.start next); next ])
    let subpath = Subpath.createWith policy [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, d) ], subpath.Segments)

[<Fact>]
let ``subpath with custom runs on exact adjacent pair`` () =
    let a, b, c, elbow = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 10.0 10.0
    let policy = Custom(fun previous next _ -> [ previous; Line(Segment.finish previous, elbow); Line(elbow, Segment.finish next) ])
    let subpath = Subpath.createWith policy [ Line(a, b); Line(b, c) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, elbow); Line(elbow, c) ], subpath.Segments)

[<Fact>]
let ``subpath with custom can insert multiple connectors`` () =
    let a, b, elbow, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 20.0 10.0, point 30.0 10.0
    let policy = Custom(fun previous next _ -> [ previous; Line(Segment.finish previous, elbow); Line(elbow, Segment.start next); next ])
    let subpath = Subpath.createWith policy [ Line(a, b); Line(c, d) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, elbow); Line(elbow, c); Line(c, d) ], subpath.Segments)

[<Fact>]
let ``subpath with custom rejects invalid results`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let policy = Custom(fun previous next _ -> [ previous; next ])
    Assert.Equal(Error(Discontinuous(0, 1, b, c, 10.0<length>)), Subpath.createWith policy [ Line(a, b); Line(c, d) ])

[<Fact>]
let ``subpath with custom rejects replacement changing previous start`` () =
    let a, b, c, d, changed = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 5.0 5.0
    let policy = Custom(fun _ next _ -> [ Line(changed, c); next ])
    Assert.Equal(Error(Discontinuous(-1, 0, a, changed, 7.0710678118654755<length>)), Subpath.createWith policy [ Line(a, b); Line(c, d) ])

[<Fact>]
let ``subpath with custom empty replacement deletes both segments`` () =
    let a, b, c, d, e, f = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 40.0 0.0, point 50.0 0.0
    let subpath = Subpath.createWith (Custom(fun _ _ _ -> [])) [ Line(a, b); Line(c, d); Line(e, f) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(e, f) ], subpath.Segments); Assert.Equal(e, Subpath.start subpath)

[<Fact>]
let ``append with custom can rewrite incoming segment`` () =
    let a, b, c, d, e = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0, point 40.0 0.0
    let policy = Custom(fun previous _ _ -> [ previous; Line(Segment.finish previous, e) ])
    let appended = Subpath.ofSegment (Line(a, b)) |> Subpath.appendWith policy (Line(c, d)) |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, e) ], appended.Segments)

[<Fact>]
let ``join with custom reconciles a gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let policy = Custom(fun previous next _ -> [ previous; Segment.withStart (Segment.finish previous) next ])
    let joined = Subpath.joinWith policy [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(c, d)) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(a, b); Line(b, d) ], joined.Segments)

[<Fact>]
let ``set closed with bridge appends final line`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, c) ] |> Subpath.setClosedWith Bridge true |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal(3, closed.Segments.Length); Assert.Equal(a, Subpath.finish closed)

[<Fact>]
let ``set closed with custom reconciles closing gap`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    let policy = Custom(fun last first _ -> [ Segment.withFinish (Segment.start first) last ])
    let closed = Subpath.setClosedWith policy true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal<Segment list>([ Line(a, b); Line(b, a) ], closed.Segments)

[<Fact>]
let ``custom policy receives closing flag`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    let policy = Custom(fun last first closing -> if closing then [ Segment.withFinish (Segment.start first) last ] else [ last; first ])
    let closed = Subpath.setClosedWith policy true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal(a, Subpath.finish closed)

[<Fact>]
let ``set closed custom runs on exact closing pair`` () =
    let a, b, c, elbow = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 5.0 5.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, a) ]
    let policy = Custom(fun last _ closing -> if closing then [ Line(Segment.start last, elbow); Line(elbow, a) ] else [ last ])
    let closed = Subpath.setClosedWith policy true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed)
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, elbow); Line(elbow, a) ], closed.Segments)

[<Fact>]
let ``set closed custom rejects invalid results`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    let policy = Custom(fun last first _ -> [ last; first ])
    Assert.Equal(Error(Discontinuous(0, 1, c, a, 14.142135623730951<length>)), Subpath.setClosedWith policy true source)

[<Fact>]
let ``set closed custom rejects replacement changing subpath start`` () =
    let a, b, changed = point 0.0 0.0, point 10.0 0.0, point 5.0 5.0
    let policy = Custom(fun _ _ _ -> [ Line(changed, changed) ])
    Assert.Equal(Error(Discontinuous(-1, 0, a, changed, 7.0710678118654755<length>)), Subpath.ofSegment (Line(a, b)) |> Subpath.setClosedWith policy true)

[<Fact>]
let ``set closed custom empty replacement deletes last segment`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, a); Line(a, c) ]
    let closed = Subpath.setClosedWith (Custom(fun _ _ _ -> [])) true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, a) ], closed.Segments)

[<Fact>]
let ``set closed true closes empty subpath`` () =
    let source = Subpath.empty (point 0.0 0.0)
    Assert.Equal(Ok(Subpath.assertSetClosed true source), Subpath.setClosed true source)

[<Fact>]
let ``set closed discontinuity reports last to first indices`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 0.0 10.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c) ]
    Assert.Equal(Error(Discontinuous(1, 0, a, c, 10.0<length>)), Subpath.setClosed true source)

[<Fact>]
let ``open at rotates closed subpath to segment start`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let ab, bc, cd, da = Line(a, b), Line(b, c), Line(c, d), Line(d, a)
    let source = Subpath.assertCreate [ ab; bc; cd; da ] |> Subpath.assertSetClosed true
    let opened = Subpath.openAt source { SegmentIndex = 1; T = 0.0<parameter> } |> Result.defaultWith (failwithf "%A")
    Assert.False(opened.Closed); Assert.Equal<Segment list>([ bc; cd; da; ab ], opened.Segments)
    Assert.Equal(b, Subpath.start opened); Assert.Equal(b, Subpath.finish opened)

[<Fact>]
let ``open at accepts parameter inside segment`` () =
    let a, b, c, d, middle = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0, point 10.0 5.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ] |> Subpath.assertSetClosed true
    let opened = Subpath.openAt source { SegmentIndex = 1; T = 0.5<parameter> } |> Result.defaultWith (failwithf "%A")
    Assert.False(opened.Closed); Assert.Equal(middle, Subpath.start opened); Assert.Equal(middle, Subpath.finish opened)
    Assert.Equal<Segment list>([ Line(middle, c); Line(c, d); Line(d, a); Line(a, b); Line(b, middle) ], opened.Segments)

[<Fact>]
let ``open at accepts last segment endpoint`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let segments = [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ]
    let source = Subpath.assertCreate segments |> Subpath.assertSetClosed true
    Assert.Equal<Segment list>(segments, (Subpath.openAt source { SegmentIndex = 3; T = 1.0<parameter> } |> Result.defaultWith (failwithf "%A")).Segments)

[<Fact>]
let ``open at rejects open subpaths`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    Assert.Equal(Error NotClosed, Subpath.openAt (Subpath.ofSegment (Line(a, b))) { SegmentIndex = 0; T = 0.0<parameter> })

[<Fact>]
let ``open at rejects invalid parameters`` () =
    let a, b, c = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, a) ] |> Subpath.assertSetClosed true
    Assert.Equal(Error(InvalidSubpathParameter(3, 0.0<parameter>, 3)), Subpath.openAt closed { SegmentIndex = 3; T = 0.0<parameter> })
    Assert.Equal(Error(InvalidSubpathParameter(0, -0.1<parameter>, 3)), Subpath.openAt closed { SegmentIndex = 0; T = -0.1<parameter> })

[<Fact>]
let ``set closed with wiggle replaces nearby endpoints`` () =
    let a, b, nearA = point 0.0 0.0, point 10.0 0.0, point 0.0000000001 0.0
    let source = Subpath.assertCreate [ Line(a, b); Line(b, nearA) ]
    let closed = Subpath.setClosedWith Wiggle true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal(Subpath.start closed, Subpath.finish closed)

[<Fact>]
let ``set closed with wiggle closes misaligned vertical lines`` () =
    let a, b, c, d = point 0.0 0.0, point 0.0 10.0, point 0.0000000001 0.0000000001, point 0.0000000001 0.00000000005
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, d) ]
    let closed = Subpath.setClosedWith Wiggle true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal(4, closed.Segments.Length); Assert.Equal(a, Segment.finish closed.Segments[3])

[<Fact>]
let ``set closed with wiggle closes misaligned horizontal lines`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 0.0000000001 0.0000000001, point 0.00000000005 0.0000000001
    let source = Subpath.assertCreate [ Line(a, b); Line(b, c); Line(c, d) ]
    let closed = Subpath.setClosedWith Wiggle true source |> Result.defaultWith (failwithf "%A")
    Assert.True(closed.Closed); Assert.Equal(4, closed.Segments.Length); Assert.Equal(a, Segment.finish closed.Segments[3])

[<Fact>]
let ``fit cubic with endpoint tangents returns root segment`` () =
    let original = CubicBezierData(point 0.0 0.0, point 35.0 65.0, point 90.0 -35.0, point 130.0 25.0)
    let sample t = Parameter.fromFloat t, Bezier.point original (Parameter.fromFloat t)
    let startTangent = Bezier.derivative original 0.0<parameter>
    let endTangent = Bezier.derivative original 1.0<parameter>
    let fit, report = Bezier.fitCubicWithEndpointTangents (Bezier.start original) (Bezier.finish original) startTangent endTangent [ sample 0.25; sample 0.5; sample 0.75 ] |> Result.defaultWith (failwithf "%A")
    match fit, original with
    | CubicBezierData(a, b, c, d), CubicBezierData(e, f, g, h) ->
        assertPointNear e a; assertPointNear f b; assertPointNear g c; assertPointNear h d
    | _ -> failwith "expected cubics"
    Assert.True(abs report.RootSumSquare <= 1.0e-6<length>)
    Assert.True(abs report.RootMeanSquare <= 1.0e-6<length>)
    Assert.True(abs report.Max <= 1.0e-6<length>)

[<Fact>]
let ``fit cubic with endpoints returns root segment`` () =
    let a, d = point 0.0 0.0, point 10.0 0.0
    let samples = [ 0.25<parameter>, point 2.5 2.0; 0.5<parameter>, point 5.0 3.0; 0.75<parameter>, point 7.5 2.0 ]
    let fit, _ = Bezier.fitCubicWithEndpoints a d samples |> Result.defaultWith (failwithf "%A")
    Assert.Equal(a, Bezier.start fit); Assert.Equal(d, Bezier.finish fit)

[<Fact>]
let ``fit cubic with endpoint tangents reports degenerate tangent`` () =
    let result = Bezier.fitCubicWithEndpointTangents (point 0.0 0.0) (point 10.0 0.0) (point 0.0 0.0) (point 1.0 0.0) [ 0.5<parameter>, point 5.0 1.0 ]
    Assert.Equal(Error DegenerateTangent, result)

[<Fact>]
let ``fit cubic with endpoints reports underdetermined fit`` () =
    let result = Bezier.fitCubicWithEndpoints (point 0.0 0.0) (point 10.0 0.0) [ 0.5<parameter>, point 5.0 1.0 ]
    Assert.Equal(Error UnderdeterminedCubicFit, result)

[<Fact>]
let ``assert join with wiggle reconciles nearby endpoint gap`` () =
    let a, b, nearB, c = point 0.0 0.0, point 10.0 0.0, point 10.0000000001 0.0, point 20.0 0.0
    let joined = Subpath.assertJoinWith Wiggle [ Subpath.ofSegment (Line(a, b)); Subpath.ofSegment (Line(nearB, c)) ]
    Assert.Equal(a, Subpath.start joined); Assert.Equal(c, Subpath.finish joined)
    Assert.True(joined.Segments |> List.pairwise |> List.forall (fun (left, right) -> Segment.finish left = Segment.start right))

[<Fact>]
let ``append segment with bridge bridges a gap`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 20.0 0.0, point 30.0 0.0
    let subpath = Subpath.empty a |> Subpath.append (Line(a, b)) |> Result.bind (Subpath.appendWith Bridge (Line(c, d))) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, subpath.Segments.Length); Assert.Equal(d, Subpath.finish subpath)

[<Fact>]
let ``assert set closed closes matching endpoints`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let closed = Subpath.assertCreate [ Line(a, b); Line(b, a) ] |> Subpath.assertSetClosed true
    Assert.True(closed.Closed)
