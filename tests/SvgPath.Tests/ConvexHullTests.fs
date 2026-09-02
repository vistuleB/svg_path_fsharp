module SvgPath.Tests.ConvexHullTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private near expected actual = Assert.True(abs (actual - expected) <= 1.0e-8<length>, $"expected {expected}, got {actual}")
let private nearDegree expected actual = Assert.True(abs (actual - expected) <= 1.0e-8<degree>, $"expected {expected}, got {actual}")

let private pointLoop p = [ Line(p, p); Line(p, p) ]

let private bigLineLoop () =
    let startPoint = point 1000.0 0.0
    let endPoint = point 999.84769516 17.45240644
    [ Line(startPoint, endPoint); Line(endPoint, startPoint) ]

let private narrowArcLoop () =
    let startPoint = point 999.94340504 7.63106966
    let endPoint = point 999.92428935 9.82151131
    [ Arc
        { Start = startPoint
          Radius = point 30.0 30.0
          XAxisRotation = 0.0<degree>
          LargeArc = false
          Sweep = true
          End = endPoint }
      Line(endPoint, startPoint) ]

let private squareLoop () =
    [ Line(point 0.0 0.0, point 10.0 0.0)
      Line(point 10.0 0.0, point 10.0 10.0)
      Line(point 10.0 10.0, point 0.0 10.0)
      Line(point 0.0 10.0, point 0.0 0.0) ]

[<Fact>]
let ``point hull rejects an empty collection`` () =
    Assert.Equal(Error(ConvexHullPathError EmptyPath), ConvexHull.pointsHull [])

[<Fact>]
let ``point hull removes interior points`` () =
    let hull =
        ConvexHull.pointsHull
            [ point 0.0 0.0; point 4.0 0.0; point 4.0 2.0; point 0.0 2.0; point 2.0 1.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Equal(4, List.length hull.Segments)

[<Fact>]
let ``rectangle minimum width is its short side`` () =
    let rectangle =
        Subpath.polygon [ point 0.0 0.0; point 7.0 0.0; point 7.0 2.0; point 0.0 2.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = ConvexHull.subpathMinimumWidth rectangle |> Result.defaultWith (failwithf "%A")
    near 2.0<length> result.Width
    Assert.True(result.Converged)

[<Fact>]
let ``triangle diameter returns witnesses and midpoint`` () =
    let triangle =
        Subpath.polygon [ point 0.0 0.0; point 3.0 4.0; point 0.0 1.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = ConvexHull.subpathDiameter triangle |> Result.defaultWith (failwithf "%A")
    near 5.0<length> result.Width
    Assert.Equal(Point.midpoint result.LowerPoint result.UpperPoint, result.Center)

[<Fact>]
let ``directional support API keeps width units`` () =
    let support direction =
        let lower = point 0.0 0.0
        let upper = Point.scale (2.0<length>) direction
        { LowerPoint = lower; UpperPoint = upper; Width = 2.0<length> }
    let result =
        ConvexHull.minimumWidthWith support 2.0<length>
            { Accuracy = 0.01<length>; MaxDepth = 4 }
    near 2.0<length> result.Width

[<Fact>]
let ``quadratic hull preserves the curve and closes with its chord`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let hull = ConvexHull.segmentHull curve |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ curve; Line(point 4.0 0.0, point 0.0 0.0) ], hull.Segments)
    Assert.True(hull.Closed)

[<Fact>]
let ``arc hull preserves the arc and closes with its chord`` () =
    let arc =
        Arc
            { Start = point -2.0 0.0
              Radius = point 2.0 2.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 2.0 0.0 }
    let hull = ConvexHull.segmentHull arc |> Result.defaultWith (failwithf "%A")
    Assert.Equal(arc, List.head hull.Segments)
    Assert.Equal(Line(point 2.0 0.0, point -2.0 0.0), List.last hull.Segments)

[<Fact>]
let ``cubic hull retains cubic boundary pieces`` () =
    let cubic = CubicBezier(point 0.0 0.0, point 0.0 4.0, point 4.0 4.0, point 4.0 0.0)
    let hull = ConvexHull.segmentHull cubic |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function CubicBezier _ -> true | _ -> false)

[<Fact>]
let ``subpath hull preserves exposed source curves`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let source = Subpath.ofSegment curve
    let hull = ConvexHull.subpathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function QuadraticBezier _ -> true | _ -> false)

[<Fact>]
let ``adaptive search converges on a rotated rectangle`` () =
    let angle = Degree.fromFloat 31.7
    let along = Point.direction angle
    let across = Point.rotateClockwise along
    let center = point 3.0 -2.0
    let corner alongSign acrossSign =
        center
        |> Point.translate (Point.scale (alongSign * 6.0<length>) along)
        |> Point.translate (Point.scale (acrossSign * 0.2<length>) across)
    let vertices = [ corner -1.0 -1.0; corner 1.0 -1.0; corner 1.0 1.0; corner -1.0 1.0 ]
    let support direction =
        let ordered = vertices |> List.sortBy (fun vertex -> Point.dot vertex direction)
        { LowerPoint = List.head ordered
          UpperPoint = List.last ordered
          Width = Point.dot (List.last ordered) direction - Point.dot (List.head ordered) direction }
    let result =
        ConvexHull.minimumWidthWith support 13.0<length>
            { Accuracy = 1.0e-6<length>; MaxDepth = 12 }
    Assert.True(result.Converged)
    Assert.True(abs (result.Width - 0.4<length>) <= 1.0e-6<length>)
    Assert.True(result.LowerBound <= 0.400001<length>)
    Assert.True(result.UpperBound >= 0.399999<length>)

[<Fact>]
let ``path hull refines transitions between distinct source curves`` () =
    let upper = QuadraticBezier(point -4.0 0.0, point -2.0 -3.0, point 0.0 0.0)
    let lower = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let source = Path.ofSubpaths [ Subpath.ofSegment upper; Subpath.ofSegment lower ]
    let hull = ConvexHull.pathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function QuadraticBezier _ -> true | _ -> false)
    hull.Segments
    |> List.pairwise
    |> List.iter (fun (previous, next) -> Assert.Equal(Segment.finish previous, Segment.start next))

[<Fact>]
let ``seeded worst direction walks to local maximum`` () =
    let result =
        ConvexHull.internalFindSeededWorstDirection
            (pointLoop (point 0.0 0.0)) (pointLoop (point 1.0 0.0)) 5.0<degree> 10.0<degree>
        |> Result.defaultWith (failwithf "%A")
    nearDegree 0.0<degree> (fst result)
    nearDegree 0.0<degree> (snd result)

[<Fact>]
let ``seeded worst direction stays within drift`` () =
    let result =
        ConvexHull.internalFindSeededWorstDirection
            (pointLoop (point 0.0 0.0)) (pointLoop (point 1.0 0.0)) 5.0<degree> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    nearDegree 4.0<degree> (fst result)
    nearDegree 4.0<degree> (snd result)

[<Fact>]
let ``loop initial sample angles merge and normalize seeds`` () =
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 45.0<degree>; 90.0<degree>; 180.0<degree>; 225.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ 45.0<degree>; 225.0<degree> ])
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 45.0<degree>; 90.0<degree>; 180.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ -90.0<degree>; 405.0<degree> ])

[<Fact>]
let ``seeded loop union removes zero length endpoint pieces`` () =
    let segments =
        ConvexHull.internalLoopUnionSegmentsWithSeedAngles
            (bigLineLoop ()) (narrowArcLoop ()) [ 0.49724434278326146<degree>; 0.5027556573338349<degree> ]
    Assert.Equal(4, List.length segments)
    Assert.All(segments, fun segment -> Assert.NotEqual(Segment.start segment, Segment.finish segment))

[<Fact>]
let ``ambitious repair preserves a narrow exposed arc slice`` () =
    let segments =
        ConvexHull.internalAmbitiousRepairLoopWithLoop (bigLineLoop ()) (narrowArcLoop ())
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, List.length segments)
    Assert.Contains(segments, function Arc _ -> true | _ -> false)

[<Fact>]
let ``point loop view follows loop orientation`` () =
    Assert.Equal(
        OutsidePoint,
        ConvexHull.internalPointLoopView
            (point 15.0 5.0) (point 10.0 5.0) (Point.create 0.0<length> 1.0<length>)
            (Point.create 0.0<length> 1.0<length>) true)
    Assert.Equal(
        InsidePoint,
        ConvexHull.internalPointLoopView
            (point 15.0 5.0) (point 0.0 5.0) (Point.create 0.0<length> -1.0<length>)
            (Point.create 0.0<length> -1.0<length>) true)
    Assert.Equal(
        TangentPoint,
        ConvexHull.internalPointLoopView
            (point 15.0 5.0) (point 10.0 10.0) (Point.create 0.0<length> 1.0<length>)
            (Point.create -1.0<length> 0.0<length>) false)
    Assert.Equal(
        OutsidePoint,
        ConvexHull.internalPointLoopView
            (point 15.0 5.0) (point 10.0 5.0) (Point.create 0.0<length> -1.0<length>)
            (Point.create 0.0<length> -1.0<length>) false)

[<Fact>]
let ``segment tangent monotonicity handles every segment kind`` () =
    let line = Line(point 0.0 0.0, point 1.0 0.0)
    let clockwiseQuadratic = QuadraticBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 1.0)
    let clockwiseArc =
        Arc
            { Start = point 0.0 0.0
              Radius = point 1.0 1.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 1.0 1.0 }
    Assert.Equal(Ok(), ConvexHull.internalSegmentTangentMonotone line true)
    Assert.Equal(Ok(), ConvexHull.internalSegmentTangentMonotone clockwiseQuadratic true)
    Assert.True(Result.isError (ConvexHull.internalSegmentTangentMonotone clockwiseQuadratic false))
    Assert.Equal(Ok(), ConvexHull.internalSegmentTangentMonotone clockwiseArc true)
    Assert.True(Result.isError (ConvexHull.internalSegmentTangentMonotone clockwiseArc false))

[<Fact>]
let ``cubic point tangent roots preserve a repeated root`` () =
    let segment =
        CubicBezier(
            point 0.0 0.0,
            point (1.0 / 3.0) 0.0,
            point (2.0 / 3.0) (1.0 / 3.0),
            point 1.0 1.0)
    let roots = ConvexHull.internalCubicPointTangentRoots segment (point 0.37 0.1369)
    Assert.Single roots |> ignore
    Assert.True(abs (Parameter.ratio (List.head roots) - 0.37) <= 1.0e-9)

[<Fact>]
let ``cubic chord tangent refinement is geometric and scale independent`` () =
    let family expected scale =
        CubicBezier(
            point 0.0 0.0,
            point (scale / 3.0) 0.0,
            point (2.0 * scale / 3.0) (-2.0 * expected * scale / 3.0),
            point scale ((1.0 - 2.0 * expected) * scale))

    for expected in [ 0.12; 0.37; 0.5; 0.88 ] do
        for scale in [ 0.000001; 1.0; 1.0e12 ] do
            let refined =
                ConvexHull.internalRefineChordTangent
                    (family expected scale)
                    (Parameter.fromFloat (expected + 0.05))
                    (Parameter.fromFloat 0.0)
            Assert.True(abs (Parameter.ratio refined - expected) <= 1.0e-9)

[<Fact>]
let ``point chord polygon tangent split preserves outside and inside chains`` () =
    let outside, inside =
        ConvexHull.internalPointChordPolygonTangentSubpaths (squareLoop ()) (point 15.0 5.0)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 10.0 0.0, outside.Start)
    Assert.Equal(point 10.0 10.0, Segment.finish (List.last outside.Segments))
    Assert.Single(outside.Segments) |> ignore
    Assert.Equal(point 10.0 10.0, inside.Start)
    Assert.Equal(point 10.0 0.0, Segment.finish (List.last inside.Segments))
    Assert.Equal(3, List.length inside.Segments)
