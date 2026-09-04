module SvgPath.Tests.GeometryParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private line ax ay bx by = Line(point ax ay, point bx by)
let private ratio value = Parameter.ratio value

[<Fact>]
let ``segment segment projection reports crossing line pair`` () =
    let found = Intersections.segmentSegmentProjection (line 0.0 0.0 3.0 3.0) (line 1.0 0.0 1.0 3.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1.0 / 3.0, ratio found.LeftT, 6)
    Assert.Equal(1.0 / 3.0, ratio found.RightT, 6)
    Assert.True(found.Distance < 1.0e-6<length>)
    Assert.True(Point.distance found.LeftPoint (point 1.0 1.0) < 1.0e-6<length>)
    Assert.True(Point.distance found.RightPoint (point 1.0 1.0) < 1.0e-6<length>)

[<Fact>]
let ``segment segment projection reports separated line pair`` () =
    let found = Intersections.segmentSegmentProjection (line 0.0 0.0 1.0 0.0) (line 0.0 2.0 1.0 2.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)
    Assert.Equal(Length.toFloat found.LeftPoint.X, Length.toFloat found.RightPoint.X, 6)
    Assert.Equal(0.0, Length.toFloat found.LeftPoint.Y, 6)
    Assert.Equal(2.0, Length.toFloat found.RightPoint.Y, 6)

[<Fact>]
let ``segment segment projection reports overlapping line pair`` () =
    let found = Intersections.segmentSegmentProjection (line 0.0 0.0 3.0 0.0) (line 1.0 0.0 2.0 0.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(found.Distance < 1.0e-6<length>)
    Assert.True(Point.distance found.LeftPoint found.RightPoint < 1.0e-6<length>)

[<Fact>]
let ``segment subpath projection reports nearest segment`` () =
    let right = Subpath.create [ line 0.0 3.0 1.0 3.0; line 1.0 3.0 1.0 2.0 ] |> Result.defaultWith (failwithf "%A")
    let found = Intersections.segmentSubpathProjection (line 0.0 0.0 1.0 0.0) right |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, found.RightAt.SegmentIndex)
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)

[<Fact>]
let ``segment path projection reports nearest subpath`` () =
    let far = Subpath.ofSegment (line 0.0 5.0 1.0 5.0)
    let near = Subpath.ofSegment (line 0.0 2.0 1.0 2.0)
    let found = Intersections.segmentPathProjection (line 0.0 0.0 1.0 0.0) (Path.ofSubpaths [ far; near ]) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, found.RightAt.SubpathIndex)
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)

[<Fact>]
let ``subpath subpath projection reports nearest segments`` () =
    let left = Subpath.create [ line 0.0 0.0 1.0 0.0; line 1.0 0.0 2.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let right = Subpath.create [ line 0.0 4.0 1.0 4.0; line 1.0 4.0 1.0 2.0 ] |> Result.defaultWith (failwithf "%A")
    let found = Intersections.subpathSubpathProjection left right |> Result.defaultWith (failwithf "%A")
    Assert.True(found.LeftAt.SegmentIndex = 0 || found.LeftAt.SegmentIndex = 1)
    Assert.Equal(1, found.RightAt.SegmentIndex)
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)

[<Fact>]
let ``subpath path projection reports nearest subpath`` () =
    let left = Subpath.ofSegment (line 0.0 0.0 1.0 0.0)
    let far = Subpath.ofSegment (line 0.0 5.0 1.0 5.0)
    let near = Subpath.ofSegment (line 0.0 2.0 1.0 2.0)
    let found = Intersections.subpathPathProjection left (Path.ofSubpaths [ far; near ]) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, found.RightAt.SubpathIndex)
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)

[<Fact>]
let ``path path projection reports nearest subpaths`` () =
    let left = Path.singleton (Subpath.ofSegment (line 0.0 0.0 1.0 0.0))
    let right = Path.ofSubpaths [ Subpath.ofSegment (line 0.0 5.0 1.0 5.0); Subpath.ofSegment (line 0.0 2.0 1.0 2.0) ]
    let found = Intersections.pathPathProjection left right |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, found.RightAt.SubpathIndex)
    Assert.Equal(2.0, Length.toFloat found.Distance, 6)

[<Fact>]
let ``segment degenerate lines preserves quadratic backtracking`` () =
    let found = Segment.degenerateLines (QuadraticBezier(point 0.0 0.0, point 10.0 0.0, point 0.0 0.0)) 0.001<length> |> Result.defaultWith (failwithf "%A") |> Option.get
    Assert.Equal(2, found.Length)
    Assert.All(found, fun segment -> Assert.True(match segment with Line _ -> true | _ -> false))

[<Fact>]
let ``segment degenerate lines preserves cubic backtracking`` () =
    let found = Segment.degenerateLines (CubicBezier(point 0.0 0.0, point 10.0 0.0, point -10.0 0.0, point 0.0 0.0)) 0.001<length> |> Result.defaultWith (failwithf "%A") |> Option.get
    Assert.Equal(3, found.Length)

[<Fact>]
let ``segment degenerate lines converts zero radius arc`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = false; End = point 10.0 0.0 }
    Assert.Equal(Some [ line 0.0 0.0 10.0 0.0 ], Segment.degenerateLines arc 0.001<length> |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``segment degenerate lines rejects wide curve`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 5.0 2.0, point 10.0 0.0)
    Assert.Equal(Ok None, Segment.degenerateLines curve 0.001<length>)

[<Fact>]
let ``subpath degenerate lines uses one strip for all segments`` () =
    let source = Subpath.create [ QuadraticBezier(point 0.0 0.0, point 10.0 0.0, point 0.0 0.0); line 0.0 0.0 -10.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let found = Subpath.degenerateLines source 0.001<length> |> Result.defaultWith (failwithf "%A") |> Option.get
    Assert.Equal(3, found.Length)

[<Fact>]
let ``subpath degenerate lines rejects bent subpath`` () =
    let source = Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok None, Subpath.degenerateLines source 0.001<length>)

[<Fact>]
let ``parametric subpath fits simple parabola`` () =
    let source = Subpath.parametric 0.0 1.0 (fun t -> point t (t * t)) |> Result.defaultWith (failwithf "%A")
    let segment = Assert.Single(source.Segments)
    let sample = Segment.point segment 0.5<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> sample (point 0.5 0.25))

[<Fact>]
let ``parametric subpath uses optional tangents`` () =
    let options = { Subpath.defaultParametricOptions with Tolerance = 1.0e-9<length>; Tangent = Some(fun _ -> point 1.0 1.0) }
    let source = Subpath.parametricWith 2.0 6.0 (fun t -> point t t) options |> Result.defaultWith (failwithf "%A")
    let segment = Assert.Single(source.Segments)
    let sample = Segment.point segment 0.25<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> sample (point 3.0 3.0))

[<Measure>]
type private sourceParameter

[<Fact>]
let ``parametric subpath preserves caller parameter units`` () =
    let options: ParametricOptions<sourceParameter> =
        { Subpath.defaultParametricOptions with
            Tolerance = 1.0e-9<length>
            Tangent = Some(fun _ -> Point.create 1.0<length / sourceParameter> 1.0<length / sourceParameter>) }
    let source =
        Subpath.parametricWith
            2.0<sourceParameter>
            6.0<sourceParameter>
            (fun t -> point (float t) (float t))
            options
        |> Result.defaultWith (failwithf "%A")
    Assert.Single(source.Segments) |> ignore

[<Fact>]
let ``parametric subpath adaptively subdivides`` () =
    let options = { Subpath.defaultParametricOptions with Tolerance = 1.0e-5<length>; MaxDepth = 8 }
    let source = Subpath.parametricWith -1.0 1.0 (fun t -> point t (t ** 4.0)) options |> Result.defaultWith (failwithf "%A")
    Assert.True(source.Segments.Length > 1)

[<Fact>]
let ``parametric subpath rejects invalid options`` () =
    let invalid = { Subpath.defaultParametricOptions with SamplesPerPiece = 1 }
    let pointFunction t = point t t
    Assert.Equal(Error(InvalidParametricSamplesPerPiece 1), Subpath.parametricWith 0.0 1.0 pointFunction invalid)
    Assert.Equal(Error(InvalidParametricInterval(1.0, 1.0)), Subpath.parametric 1.0 1.0 pointFunction)

let private assertNear expected actual =
    Assert.InRange(float actual, expected - 1.0e-6, expected + 1.0e-6)

let private assertLengthNear expected (actual: float<length>) =
    Assert.InRange(float actual, expected - 1.0e-6, expected + 1.0e-6)

let private semicircle () =
    Arc
        { Start = point 0.0 0.0
          Radius = point 10.0 10.0
          XAxisRotation = 0.0<degree>
          LargeArc = false
          Sweep = true
          End = point 20.0 0.0 }

[<Fact>]
let ``segment crossings finds line crossing`` () =
    let crossing =
        Segment.crossings (line 0.0 0.0 10.0 0.0) (fun sample -> sample.X - 5.0<length>)
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertNear 0.5 crossing

[<Fact>]
let ``segment crossings finds multiple quadratic crossings`` () =
    let options = { Samples = 20; SignedLineDistanceTolerance = 1.0e-9<length>; MaxIterations = 100 }
    let crossings =
        Segment.crossingsWith
            (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
            (fun sample -> sample.Y - 5.0<length>)
            options
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, crossings.Length)
    assertNear 0.146446609 crossings[0]
    assertNear 0.853553391 crossings[1]

[<Fact>]
let ``segment crossings finds arc crossing`` () =
    let crossing =
        Segment.crossings (semicircle ()) (fun sample -> sample.X - 10.0<length>)
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertNear 0.5 crossing

[<Fact>]
let ``segment ray crossings finds line crossing`` () =
    let crossing, rayT =
        Segment.rayCrossingsWith (line 10.0 -5.0 10.0 5.0) (point 5.0 0.0) (Point.create 1.0 0.0) Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertNear 0.5 crossing
    assertLengthNear 5.0 rayT

[<Fact>]
let ``segment ray crossings finds quadratic tangent contact`` () =
    let crossing, rayT =
        Segment.rayCrossingsWith
            (QuadraticBezier(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0))
            (point 0.0 5.0)
            (Point.create 1.0 0.0)
            Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertNear 0.5 crossing
    assertLengthNear 10.0 rayT

[<Fact>]
let ``segment ray crossings finds cubic line crossing`` () =
    let crossing, rayT =
        Segment.rayCrossingsWith
            (CubicBezier(point 0.0 0.0, point 0.1 0.1, point 2.5 2.5, point 3.0 3.0))
            (point 1.0 0.0)
            (Point.create 0.0 1.0)
            Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
        |> List.filter (fun (_, rayT) -> rayT > 0.0<length>)
        |> List.exactlyOne
    assertNear 0.411711782 crossing
    assertLengthNear 1.0 rayT

[<Fact>]
let ``segment ray crossings finds arc line crossing`` () =
    let crossing, rayT =
        Segment.rayCrossingsWith (semicircle ()) (point 10.0 -15.0) (Point.create 0.0 1.0) Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
        |> List.filter (fun (_, rayT) -> rayT > 0.0<length>)
        |> List.exactlyOne
    assertNear 0.5 crossing
    assertLengthNear 5.0 rayT

[<Fact>]
let ``segment ray crossings includes wrong side crossing`` () =
    let crossing, rayT =
        Segment.rayCrossingsWith (line 10.0 -5.0 10.0 5.0) (point 5.0 0.0) (Point.create -1.0 0.0) Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertNear 0.5 crossing
    assertLengthNear -5.0 rayT

[<Fact>]
let ``segment ray crossings rejects zero direction`` () =
    Assert.Equal(
        Error IndeterminateDirection,
        Segment.rayCrossingsWith (line 0.0 0.0 10.0 0.0) (point 5.0 0.0) (Point.create 0.0 0.0) Segment.defaultCrossingOptions)

[<Fact>]
let ``segment crossings rejects invalid options`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let measure sample = sample.X - 5.0<length>
    Assert.Equal(Error(InvalidCrossingSamples 0), Segment.crossingsWith segment measure { Segment.defaultCrossingOptions with Samples = 0 })
    Assert.Equal(Error(InvalidCrossingTolerance 0.0<length>), Segment.crossingsWith segment measure { Segment.defaultCrossingOptions with SignedLineDistanceTolerance = 0.0<length> })
    Assert.Equal(Error(InvalidCrossingMaxIterations 0), Segment.crossingsWith segment measure { Segment.defaultCrossingOptions with MaxIterations = 0 })

[<Fact>]
let ``segment crossings returns degenerate arc errors`` () =
    let segment = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    Assert.Equal(Error DegenerateArc, Segment.crossings segment (fun sample -> sample.X))

[<Fact>]
let ``segment minimize finds line minimum`` () =
    let found =
        Segment.minimize (line 0.0 0.0 10.0 0.0) (fun sample -> (float sample.X - 7.0) ** 2.0)
        |> Result.defaultWith (failwithf "%A")
    assertNear 0.7 found

[<Fact>]
let ``segment minimize finds quadratic minimum`` () =
    let found =
        Segment.minimize
            (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
            (fun sample -> (float sample.X - 10.0) ** 2.0 + (float sample.Y - 10.0) ** 2.0)
        |> Result.defaultWith (failwithf "%A")
    assertNear 0.5 found

[<Fact>]
let ``segment minimize finds arc minimum`` () =
    let found =
        Segment.minimize (semicircle ()) (fun sample -> (float sample.X - 10.0) ** 2.0)
        |> Result.defaultWith (failwithf "%A")
    assertNear 0.5 found

[<Fact>]
let ``segment minimize with rejects invalid options`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let measure sample = float sample.X
    Assert.Equal(Error(InvalidMinimizeSamples 0), Segment.minimizeWith segment measure { Segment.defaultMinimizeOptions with Samples = 0 })
    Assert.Equal(Error(InvalidMinimizeTolerance 0.0<parameter>), Segment.minimizeWith segment measure { Segment.defaultMinimizeOptions with ParameterTolerance = 0.0<parameter> })
    Assert.Equal(Error(InvalidMinimizeMaxIterations 0), Segment.minimizeWith segment measure { Segment.defaultMinimizeOptions with MaxIterations = 0 })

[<Fact>]
let ``segment minimize returns degenerate arc errors`` () =
    let segment = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    Assert.Equal(Error DegenerateArc, Segment.minimize segment (fun sample -> float sample.X))

[<Fact>]
let ``segment distance measures line projection`` () =
    let found = Segment.distance (line 0.0 0.0 10.0 0.0) (point 5.0 4.0) |> Result.defaultWith (failwithf "%A")
    assertLengthNear 4.0 found

[<Fact>]
let ``segment distance measures line endpoint`` () =
    let found = Segment.distance (line 0.0 0.0 10.0 0.0) (point 13.0 4.0) |> Result.defaultWith (failwithf "%A")
    assertLengthNear 5.0 found

[<Fact>]
let ``segment distance measures quadratic curve`` () =
    let found =
        Segment.distance
            (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
            (point 10.0 15.0)
        |> Result.defaultWith (failwithf "%A")
    assertLengthNear 5.0 found

[<Fact>]
let ``segment distance measures cubic curve`` () =
    let found =
        Segment.distance
            (CubicBezier(point 0.0 0.0, point 0.0 10.0, point 10.0 10.0, point 10.0 0.0))
            (point 5.0 7.5)
        |> Result.defaultWith (failwithf "%A")
    Assert.True(found < 0.0001<length>)

[<Fact>]
let ``segment distance measures arc`` () =
    let found = Segment.distance (semicircle ()) (point 10.0 -15.0) |> Result.defaultWith (failwithf "%A")
    assertLengthNear 5.0 found

[<Fact>]
let ``segment distance with rejects invalid options`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let sample = point 5.0 4.0
    Assert.Equal(Error(InvalidDistanceSamples 0), Segment.distanceWith segment sample { Segment.defaultDistanceOptions with Samples = 0 })
    Assert.Equal(Error(InvalidDistanceTolerance 0.0<length>), Segment.distanceWith segment sample { Segment.defaultDistanceOptions with Tolerance = 0.0<length> })
    Assert.Equal(Error(InvalidDistanceMaxIterations 0), Segment.distanceWith segment sample { Segment.defaultDistanceOptions with MaxIterations = 0 })

[<Fact>]
let ``segment distance returns degenerate arc errors`` () =
    let segment = Arc { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 0.0 }
    Assert.Equal(Error DegenerateArc, Segment.distance segment (point 10.0 0.0))

[<Fact>]
let ``segment length measures line exactly`` () =
    Segment.length (line 0.0 0.0 3.0 4.0)
    |> Result.defaultWith (failwithf "%A")
    |> assertLengthNear 5.0

[<Fact>]
let ``segment length avoids intermediate overflow`` () =
    Assert.Equal(Ok 1.0e200<length>, Segment.length (line 0.0 0.0 1.0e200 0.0))

[<Fact>]
let ``segment length approximates quadratic curve`` () =
    let found = Segment.length (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)) |> Result.defaultWith (failwithf "%A")
    Assert.InRange(found, 20.0<length>, 40.0<length>)

[<Fact>]
let ``segment length matches sampled curve reference`` () =
    let curve = CubicBezier(point 0.0 0.0, point 0.0 30.0, point 40.0 -10.0, point 40.0 20.0)
    let found = Segment.length curve |> Result.defaultWith (failwithf "%A")
    let samples =
        [ 0 .. 1000 ]
        |> List.map (fun index -> Segment.point curve (Parameter.fromFloat (float index / 1000.0)) |> Result.defaultWith (failwithf "%A"))
    let reference = samples |> List.pairwise |> List.sumBy (fun (left, right) -> Point.distance left right)
    Assert.True(abs (found - reference) < 0.001<length>)

[<Fact>]
let ``segment length approximates arc`` () =
    let found = Segment.length (semicircle ()) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs (found - 31.41592653589793<length>) < 0.01<length>)

[<Fact>]
let ``segment length with rejects invalid options`` () =
    let segment = line 0.0 0.0 10.0 0.0
    Assert.Equal(Error(InvalidLengthTolerance 0.0<length>), Segment.lengthWith segment { Tolerance = 0.0<length>; MaxDepth = 20 })
    Assert.Equal(Error(InvalidLengthMaxDepth 0), Segment.lengthWith segment { Tolerance = 1.0e-9<length>; MaxDepth = 0 })

[<Fact>]
let ``segment length with reports exhausted refinement depth`` () =
    let curve = CubicBezier(point 0.0 0.0, point 0.0 100.0, point 100.0 -100.0, point 100.0 0.0)
    match Segment.lengthWith curve { Tolerance = 1.0e-30<length>; MaxDepth = 1 } with
    | Error(LengthMaxDepthReached(estimate, error)) ->
        Assert.True(estimate > 0.0<length>)
        Assert.True(error > 0.0<length>)
    | result -> failwithf "expected exhausted length refinement depth, got %A" result

[<Fact>]
let ``subpath length sums segment lengths`` () =
    let subpath = Subpath.create [ line 0.0 0.0 3.0 4.0; line 3.0 4.0 8.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    Subpath.length subpath |> Result.defaultWith (failwithf "%A") |> assertLengthNear 18.0

[<Fact>]
let ``subpath length returns zero for empty subpath`` () =
    Assert.Equal(Ok 0.0<length>, Subpath.length (Subpath.empty (point 0.0 0.0)))

[<Fact>]
let ``segment parameter at length measures line exactly`` () =
    Assert.Equal(Ok 0.4<parameter>, Segment.parameterAtLength (line 0.0 0.0 10.0 0.0) 4.0<length>)

[<Fact>]
let ``segment point at length evaluates line`` () =
    let found = Segment.pointAtLength (line 0.0 0.0 10.0 0.0) 4.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> found (point 4.0 0.0))

[<Fact>]
let ``segment parameter at length inverts symmetric curve`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let length = Segment.length curve |> Result.defaultWith (failwithf "%A")
    Segment.parameterAtLength curve (length / 2.0) |> Result.defaultWith (failwithf "%A") |> assertNear 0.5

[<Fact>]
let ``segment point at length evaluates arc`` () =
    let arc = semicircle ()
    let length = Segment.length arc |> Result.defaultWith (failwithf "%A")
    let found = Segment.pointAtLength arc (length / 2.0) |> Result.defaultWith (failwithf "%A")
    let derivative = Segment.derivativeAtLength arc (length / 2.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> found (point 10.0 -10.0))
    Assert.True(derivative.X > 0.0<length / parameter>)
    Assert.True(abs derivative.Y < 1.0e-6<length / parameter>)

[<Fact>]
let ``segment parameter at length rejects invalid distances`` () =
    let segment = line 0.0 0.0 10.0 0.0
    Assert.Equal(Error(InvalidLengthDistance(-1.0<length>, 10.0<length>)), Segment.parameterAtLength segment -1.0<length>)
    Assert.Equal(Error(InvalidLengthDistance(11.0<length>, 10.0<length>)), Segment.parameterAtLength segment 11.0<length>)

[<Fact>]
let ``segment between lengths uses traveled distances`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let forward = Segment.betweenLengths segment 2.0<length> 7.0<length> |> Result.defaultWith (failwithf "%A")
    let reverse = Segment.betweenLengths segment 7.0<length> 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 2.0 0.0, Segment.start forward)
    Assert.Equal(point 7.0 0.0, Segment.finish forward)
    Assert.Equal(point 7.0 0.0, Segment.start reverse)
    Assert.Equal(point 2.0 0.0, Segment.finish reverse)

[<Fact>]
let ``segments between lengths uses adjacent distances`` () =
    let pieces =
        Segment.betweenLengthsMany (line 0.0 0.0 10.0 0.0) [ 2.0<length>; 7.0<length>; 4.0<length> ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, pieces.Length)
    Assert.Equal(point 2.0 0.0, Segment.start pieces[0])
    Assert.Equal(point 7.0 0.0, Segment.finish pieces[0])
    Assert.Equal(point 7.0 0.0, Segment.start pieces[1])
    Assert.Equal(point 4.0 0.0, Segment.finish pieces[1])

[<Fact>]
let ``segment between lengths rejects invalid input`` () =
    let segment = line 0.0 0.0 10.0 0.0
    Assert.Equal(Error(InvalidLengthDistance(11.0<length>, 10.0<length>)), Segment.betweenLengths segment 0.0<length> 11.0<length>)
    Assert.Equal(
        Error(InvalidLengthTolerance 0.0<length>),
        Segment.betweenLengthsWith segment 2.0<length> 7.0<length> { Tolerance = 0.0<length>; MaxDepth = 20 })

[<Fact>]
let ``segment subdivide to max length splits line by arc length`` () =
    let pieces = Segment.subdivideToMaxLength (line 0.0 0.0 10.0 0.0) 3.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, pieces.Length)
    Assert.Equal(point 0.0 0.0, Segment.start pieces[0])
    Assert.Equal(point 2.5 0.0, Segment.start pieces[1])
    Assert.Equal(point 5.0 0.0, Segment.start pieces[2])
    Assert.Equal(point 7.5 0.0, Segment.start pieces[3])
    Assert.Equal(point 10.0 0.0, Segment.finish pieces[3])

[<Fact>]
let ``segment subdivide to max length splits curve by arc length`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 30.0 0.0, point 30.0 30.0)
    let length = Segment.length curve |> Result.defaultWith (failwithf "%A")
    let pieces = Segment.subdivideToMaxLength curve (length / 2.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, pieces.Length)
    Segment.length pieces[0] |> Result.defaultWith (failwithf "%A") |> assertLengthNear (float length / 2.0)
    Segment.length pieces[1] |> Result.defaultWith (failwithf "%A") |> assertLengthNear (float length / 2.0)
    Assert.True(Point.near 1.0e-6<length> (Segment.finish pieces[0]) (Segment.start pieces[1]))

[<Fact>]
let ``segment subdivide to max length keeps zero length segment`` () =
    let segment = line 1.0 2.0 1.0 2.0
    Assert.Equal(Ok [ segment ], Segment.subdivideToMaxLength segment 1.0<length>)

[<Fact>]
let ``segment subdivide to max length rejects invalid max length`` () =
    Assert.Equal(Error(InvalidSubdivisionMaxLength 0.0<length>), Segment.subdivideToMaxLength (line 0.0 0.0 10.0 0.0) 0.0<length>)

[<Fact>]
let ``subpath subdivide to max length preserves boundaries and closed`` () =
    let source =
        Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 4.0; line 10.0 4.0 0.0 0.0 ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let subdivided = Subpath.subdivideToMaxLength source 4.0<length> |> Result.defaultWith (failwithf "%A")
    let segments = Subpath.segments subdivided
    Assert.True(Subpath.isClosed subdivided)
    Assert.Equal(7, segments.Length)
    Assert.Equal(point 10.0 0.0, Segment.finish segments[2])
    Assert.Equal(point 10.0 0.0, Segment.start segments[3])

[<Fact>]
let ``path subdivide to max length preserves subpaths`` () =
    let source = Path.ofSubpaths [ Subpath.ofSegment (line 0.0 0.0 10.0 0.0); Subpath.ofSegment (line 20.0 0.0 20.0 8.0) ]
    let subdivided = Path.subdivideToMaxLength source 4.0<length> |> Result.defaultWith (failwithf "%A") |> Path.subpaths
    Assert.Equal(2, subdivided.Length)
    Assert.Equal(3, Subpath.segments subdivided[0] |> List.length)
    Assert.Equal(2, Subpath.segments subdivided[1] |> List.length)

[<Fact>]
let ``subpath parameter at length returns public parameter`` () =
    let source = Subpath.create [ line 0.0 0.0 3.0 4.0; line 3.0 4.0 3.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok { SegmentIndex = 1; T = 0.5<parameter> }, Subpath.parameterAtLength source 11.0<length>)
    Assert.Equal(Ok { SegmentIndex = 1; T = 1.0<parameter> }, Subpath.parameterAtLength source 17.0<length>)

[<Fact>]
let ``subpath point and derivative at length evaluate parameter`` () =
    let source = Subpath.create [ line 0.0 0.0 3.0 4.0; line 3.0 4.0 3.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    let found = Subpath.pointAtLength source 11.0<length> |> Result.defaultWith (failwithf "%A")
    let derivative = Subpath.derivativeAtLength source 11.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> found (point 3.0 10.0))
    Assert.Equal(Point.create 0.0<length / parameter> 12.0<length / parameter>, derivative)

[<Fact>]
let ``subpath parameter at length rejects empty subpaths`` () =
    Assert.Equal(Error EmptySubpath, Subpath.parameterAtLength (Subpath.empty (point 0.0 0.0)) 0.0<length>)

[<Fact>]
let ``path parameter at length rejects empty paths and empty subpaths`` () =
    Assert.Equal(Error EmptyPath, Path.parameterAtLength Path.empty 0.0<length>)
    let moveOnly = Subpath.empty (point 0.0 0.0)
    Assert.Equal(Error EmptySubpaths, Path.parameterAtLength (Path.singleton moveOnly) 0.0<length>)

[<Fact>]
let ``empty aggregate lengths still validate options`` () =
    let invalid: LengthOptions = { Tolerance = 0.0<length>; MaxDepth = 20 }
    Assert.Equal(
        Error(InvalidLengthTolerance 0.0<length>),
        Subpath.lengthWith (Subpath.empty (point 0.0 0.0)) invalid)
    Assert.Equal(
        Error(InvalidLengthTolerance 0.0<length>),
        Path.lengthWith Path.empty invalid)

[<Fact>]
let ``empty aggregate linearization still validates options`` () =
    let invalid: LinearizeOptions = { Tolerance = 0.0<length>; MaxDepth = 20 }
    Assert.Equal(
        Error(InvalidLinearizeTolerance 0.0<length>),
        Subpath.toLinesWith invalid (Subpath.empty (point 0.0 0.0)))
    Assert.Equal(
        Error(InvalidLinearizeTolerance 0.0<length>),
        Path.toLinesWith invalid Path.empty)

[<Fact>]
let ``subpath directions validate options before the parameter`` () =
    let invalid = { RelativeTolerance = -0.1 }
    Assert.Equal(
        Error(InvalidDirectionRelativeTolerance -0.1),
        Subpath.directionsWith
            (Subpath.empty (point 0.0 0.0))
            { SegmentIndex = 0; T = 0.0<parameter> }
            invalid)

[<Fact>]
let ``subpath between lengths crosses segments`` () =
    let source = Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 20.0 0.0; line 20.0 0.0 30.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let piece = Subpath.betweenLengths source 5.0<length> 25.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Subpath.segments piece = [ line 5.0 0.0 10.0 0.0; line 10.0 0.0 20.0 0.0; line 20.0 0.0 25.0 0.0 ])

[<Fact>]
let ``subpaths between lengths splits open subpath`` () =
    let source = Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 20.0 0.0; line 20.0 0.0 30.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let pieces = Subpath.betweenLengthsMany source [ 5.0<length>; 25.0<length> ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, pieces.Length)
    Assert.True(Subpath.segments pieces[0] = [ line 0.0 0.0 5.0 0.0 ])
    Assert.True(Subpath.segments pieces[1] = [ line 5.0 0.0 10.0 0.0; line 10.0 0.0 20.0 0.0; line 20.0 0.0 25.0 0.0 ])
    Assert.True(Subpath.segments pieces[2] = [ line 25.0 0.0 30.0 0.0 ])

[<Fact>]
let ``subpath between lengths wraps closed subpaths`` () =
    let source =
        Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 10.0; line 10.0 10.0 0.0 10.0; line 0.0 10.0 0.0 0.0 ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let piece = Subpath.betweenLengths source 25.0<length> 15.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Subpath.segments piece = [ line 5.0 10.0 0.0 10.0; line 0.0 10.0 0.0 0.0; line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 5.0 ])

let private lengthPath () =
    Path.ofSubpaths
        [ Subpath.empty (point -1.0 -1.0)
          Subpath.ofSegment (line 0.0 0.0 3.0 4.0)
          Subpath.ofSegment (line 10.0 10.0 10.0 22.0) ]

[<Fact>]
let ``path length sums subpath lengths`` () =
    Path.length (lengthPath ()) |> Result.defaultWith (failwithf "%A") |> assertLengthNear 17.0

[<Fact>]
let ``path length returns zero for empty path`` () =
    Assert.Equal(Ok 0.0<length>, Path.length Path.empty)

[<Fact>]
let ``path parameter at length returns public parameter`` () =
    let source = lengthPath ()
    Assert.Equal(Ok { SubpathIndex = 2; At = { SegmentIndex = 0; T = 0.5<parameter> } }, Path.parameterAtLength source 11.0<length>)
    Assert.Equal(Ok { SubpathIndex = 2; At = { SegmentIndex = 0; T = 1.0<parameter> } }, Path.parameterAtLength source 17.0<length>)

[<Fact>]
let ``path point and derivative at length evaluate parameter`` () =
    let source = Path.ofSubpaths [ Subpath.ofSegment (line 0.0 0.0 3.0 4.0); Subpath.ofSegment (line 10.0 10.0 10.0 22.0) ]
    let found = Path.pointAtLength source 11.0<length> |> Result.defaultWith (failwithf "%A")
    let derivative = Path.derivativeAtLength source 11.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> found (point 10.0 16.0))
    Assert.Equal(Point.create 0.0<length / parameter> 12.0<length / parameter>, derivative)

[<Fact>]
let ``path parameter at length rejects invalid distances`` () =
    let source = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    Assert.Equal(Error(InvalidLengthDistance(-1.0<length>, 10.0<length>)), Path.parameterAtLength source -1.0<length>)
    Assert.Equal(Error(InvalidLengthDistance(11.0<length>, 10.0<length>)), Path.parameterAtLength source 11.0<length>)

[<Fact>]
let ``path point rejects invalid path parameters`` () =
    let source = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    let at = { SubpathIndex = 1; At = { SegmentIndex = 0; T = 0.0<parameter> } }
    Assert.Equal(Error(InvalidPathParameter(1, 1)), Path.point source at)

[<Fact>]
let ``segment projection returns line parameter point and distance`` () =
    let t, found, distance = Segment.projection (line 0.0 0.0 10.0 0.0) (point 4.0 3.0) |> Result.defaultWith (failwithf "%A")
    assertNear 0.4 t
    Assert.True(Point.near 1.0e-6<length> found (point 4.0 0.0))
    assertLengthNear 3.0 distance

[<Fact>]
let ``segment projection clamps to line endpoint`` () =
    let t, found, distance = Segment.projection (line 0.0 0.0 10.0 0.0) (point 13.0 4.0) |> Result.defaultWith (failwithf "%A")
    assertNear 1.0 t
    Assert.True(Point.near 1.0e-6<length> found (point 10.0 0.0))
    assertLengthNear 5.0 distance

[<Fact>]
let ``segment projection returns curve parameter point and distance`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let t, found, distance = Segment.projection curve (point 10.0 15.0) |> Result.defaultWith (failwithf "%A")
    assertNear 0.5 t
    Assert.True(Point.near 1.0e-6<length> found (point 10.0 10.0))
    assertLengthNear 5.0 distance

[<Fact>]
let ``projection returns quadratic parameter point and distance`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let t, found, distance = Segment.projection curve (point 10.0 15.0) |> Result.defaultWith (failwithf "%A")
    assertNear 0.5 t
    Assert.True(Point.near 1.0e-6<length> found (point 10.0 10.0))
    assertLengthNear 5.0 distance

let private projectionComparisonSegments =
    [ QuadraticBezier(point -6.0 -3.0, point 0.0 14.0, point 7.0 -5.0)
      QuadraticBezier(point 5.0 0.0, point -9.0 3.0, point 4.0 6.0)
      CubicBezier(point -7.0 -4.0, point 12.0 16.0, point -13.0 14.0, point 8.0 -6.0)
      CubicBezier(point 0.0 0.0, point 18.0 2.0, point -16.0 5.0, point 2.0 8.0) ]

let private projectionComparisonCoordinates = [ -12.0; -9.0; -6.0; -3.0; 0.0; 3.0; 6.0; 9.0; 12.0 ]

let private projectionTangentialError query segment at =
    let onSegment = Segment.point segment at |> Result.defaultWith (failwithf "%A")
    let derivative = Segment.derivative segment at |> Result.defaultWith (failwithf "%A")
    abs (Point.dot (Point.displacement query onSegment) derivative) / Point.norm derivative

[<Fact>]
let ``polished bezier projections have small tangential error`` () =
    for segment in projectionComparisonSegments do
        for x in projectionComparisonCoordinates do
            for y in projectionComparisonCoordinates do
                let query = point x y
                let t, _, _ = Segment.projection segment query |> Result.defaultWith (failwithf "%A")
                if t > 0.0<parameter> && t < 1.0<parameter> then
                    Assert.True(projectionTangentialError query segment t < 1.0e-7<length>)

[<Fact>]
let ``projection handles unreliable near cusp tangent`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 1.0 1.0, point 0.00000001 0.0)
    let t, _, distance = Segment.projection segment (point 0.5 0.6) |> Result.defaultWith (failwithf "%A")
    Assert.InRange(t, 0.0<parameter>, 1.0<parameter>)
    Assert.True(distance >= 0.0<length>)

[<Fact>]
let ``projection of bezier points respects tolerance`` () =
    let segments =
        [ QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
          CubicBezier(point 0.0 0.0, point 5.0 20.0, point 15.0 -20.0, point 20.0 0.0) ]
    let parameters = [ 1.0 / 6.0; 1.0 / 3.0; 0.5; 2.0 / 3.0; 5.0 / 6.0 ] |> List.map Parameter.fromFloat
    for segment in segments do
        for t in parameters do
            let sample = Segment.point segment t |> Result.defaultWith (failwithf "%A")
            let _, _, distance = Segment.projection segment sample |> Result.defaultWith (failwithf "%A")
            Assert.True(distance <= 1.0e-9<length>)

[<Fact>]
let ``projection of curve points respects geometric tolerance`` () =
    let segments =
        [ QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
          CubicBezier(point 0.0 0.0, point 5.0 20.0, point 15.0 -20.0, point 20.0 0.0)
          Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 10.0 0.0 } ]
    let parameters = [ 1.0 / 6.0; 1.0 / 3.0; 0.5; 2.0 / 3.0; 5.0 / 6.0 ] |> List.map Parameter.fromFloat
    let options = { Samples = 100; Tolerance = 1.0e-9<length>; MaxIterations = 100 }
    for segment in segments do
        for t in parameters do
            let sample = Segment.point segment t |> Result.defaultWith (failwithf "%A")
            let _, _, distance = Segment.projectionWith segment sample options |> Result.defaultWith (failwithf "%A")
            Assert.True(distance <= 1.0e-9<length>)

[<Fact>]
let ``segment projection with rejects invalid options`` () =
    Assert.Equal(
        Error(InvalidDistanceSamples 0),
        Segment.projectionWith
            (line 0.0 0.0 10.0 0.0)
            (point 5.0 4.0)
            { Segment.defaultDistanceOptions with Samples = 0 })

[<Fact>]
let ``subpath projection returns subpath parameter point and distance`` () =
    let source = Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    let projection = Subpath.projection source (point 14.0 8.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal({ SegmentIndex = 1; T = 0.4<parameter> }, projection.At)
    Assert.True(Point.near 1.0e-6<length> projection.Point (point 10.0 8.0))
    assertLengthNear 4.0 projection.Distance

[<Fact>]
let ``subpath projection rejects empty subpaths`` () =
    Assert.Equal(Error EmptySubpath, Subpath.projection (Subpath.empty (point 0.0 0.0)) (point 1.0 1.0))

[<Fact>]
let ``path projection returns path parameter point and distance`` () =
    let source =
        Path.ofSubpaths
            [ Subpath.empty (point -10.0 -10.0)
              Subpath.ofSegment (line 0.0 0.0 10.0 0.0)
              Subpath.ofSegment (line 20.0 0.0 20.0 10.0) ]
    let projection = Path.projection source (point 17.0 6.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal({ SubpathIndex = 2; At = { SegmentIndex = 0; T = 0.6<parameter> } }, projection.At)
    Assert.True(Point.near 1.0e-6<length> projection.Point (point 20.0 6.0))
    assertLengthNear 3.0 projection.Distance

[<Fact>]
let ``path distance returns projection distance`` () =
    let source = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    Path.distance source (point 4.0 3.0) |> Result.defaultWith (failwithf "%A") |> assertLengthNear 3.0

[<Fact>]
let ``subpath distance returns projection distance`` () =
    let source = Subpath.ofSegment (line 0.0 0.0 10.0 0.0)
    Subpath.distance source (point 4.0 3.0) |> Result.defaultWith (failwithf "%A") |> assertLengthNear 3.0

[<Fact>]
let ``path projection rejects empty paths and empty subpaths`` () =
    Assert.Equal(Error EmptyPath, Path.projection Path.empty (point 1.0 1.0))
    Assert.Equal(Error EmptySubpaths, Path.projection (Path.singleton (Subpath.empty (point 0.0 0.0))) (point 1.0 1.0))

[<Fact>]
let ``path projection with rejects invalid options`` () =
    let source = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    Assert.Equal(
        Error(InvalidDistanceSamples 0),
        Path.projectionWith source (point 4.0 3.0) { Segment.defaultDistanceOptions with Samples = 0 })

let private polygon coordinates =
    coordinates |> List.map (fun (x, y) -> point x y) |> Subpath.polygon |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``subpath containment implicitly closes open subpaths`` () =
    let source = Subpath.create [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 10.0; line 10.0 10.0 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.False(Subpath.isClosed source)
    Assert.Equal(Ok Inside, Subpath.containment (point 5.0 5.0) source Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 15.0 5.0) source Nonzero)
    Assert.Equal(Ok Boundary, Subpath.containment (point 0.0 5.0) source Nonzero)
    Assert.Equal(Ok Boundary, Subpath.containment (point 10.0 5.0) source Nonzero)

[<Fact>]
let ``subpath containment supports both fill rules`` () =
    let once = [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 10.0 10.0; line 10.0 10.0 0.0 10.0; line 0.0 10.0 0.0 0.0 ]
    let source = Subpath.create (once @ once) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok Inside, Subpath.containment (point 5.0 5.0) source Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 5.0 5.0) source EvenOdd)

[<Fact>]
let ``subpath containment handles ray through vertex`` () =
    let source = polygon [ 0.0, 0.0; 10.0, 5.0; 0.0, 10.0 ]
    Assert.Equal(Ok Inside, Subpath.containment (point 2.0 5.0) source Nonzero)
    Assert.Equal(Ok Outside, Subpath.containment (point 12.0 5.0) source Nonzero)

[<Fact>]
let ``subpath containment handles curved boundaries`` () =
    let source = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
    Assert.Equal(Ok Boundary, Subpath.containment (point 10.0 10.0) source Nonzero)
    Assert.Equal(Ok Inside, Subpath.containment (point 10.0 5.0) source Nonzero)

[<Fact>]
let ``subpath containment uses boundary tolerance`` () =
    let source = polygon [ 0.0, 0.0; 10.0, 0.0; 10.0, 10.0; 0.0, 10.0 ]
    let options = { Path.defaultContainmentOptions with Tolerance = 0.001<length>; Samples = 100; MaxIterations = 100 }
    Assert.Equal(Ok Boundary, Subpath.containmentWith (point -0.0005 5.0) source Nonzero options)

[<Fact>]
let ``subpath containment move only subpath is outside`` () =
    let sample = point 5.0 5.0
    Assert.Equal(Ok Outside, Subpath.containment sample (Subpath.empty sample) Nonzero)

[<Fact>]
let ``subpath containment rejects invalid options`` () =
    let source = Subpath.empty (point 0.0 0.0)
    let sample = point 1.0 1.0
    Assert.Equal(Error(InvalidContainmentTolerance 0.0<length>), Subpath.containmentWith sample source Nonzero { Path.defaultContainmentOptions with Tolerance = 0.0<length> })
    Assert.Equal(Error(InvalidContainmentSamples 0), Subpath.containmentWith sample source Nonzero { Path.defaultContainmentOptions with Samples = 0 })
    Assert.Equal(Error(InvalidContainmentMaxIterations 0), Subpath.containmentWith sample source Nonzero { Path.defaultContainmentOptions with MaxIterations = 0 })

[<Fact>]
let ``path containment combines subpath winding and parity`` () =
    let outer = polygon [ 0.0, 0.0; 20.0, 0.0; 20.0, 20.0; 0.0, 20.0 ]
    let same = polygon [ 5.0, 5.0; 15.0, 5.0; 15.0, 15.0; 5.0, 15.0 ]
    let opposite = polygon [ 5.0, 5.0; 5.0, 15.0; 15.0, 15.0; 15.0, 5.0 ]
    let center = point 10.0 10.0
    Assert.Equal(Ok Inside, Path.containment center (Path.ofSubpaths [ outer; same ]) Nonzero)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; same ]) EvenOdd)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; opposite ]) Nonzero)
    Assert.Equal(Ok Outside, Path.containment center (Path.ofSubpaths [ outer; opposite ]) EvenOdd)
    Assert.Equal(Ok Inside, Path.containment (point 2.0 2.0) (Path.ofSubpaths [ outer; opposite ]) Nonzero)

[<Fact>]
let ``path containment boundary on any subpath dominates`` () =
    let outer = polygon [ 0.0, 0.0; 20.0, 0.0; 20.0, 20.0; 0.0, 20.0 ]
    let inner = polygon [ 5.0, 5.0; 15.0, 5.0; 15.0, 15.0; 5.0, 15.0 ]
    Assert.Equal(Ok Boundary, Path.containment (point 5.0 10.0) (Path.ofSubpaths [ outer; inner ]) Nonzero)

[<Fact>]
let ``path winding accumulates subpath winding`` () =
    let outer = polygon [ 0.0, 0.0; 20.0, 0.0; 20.0, 20.0; 0.0, 20.0 ]
    let same = polygon [ 5.0, 5.0; 15.0, 5.0; 15.0, 15.0; 5.0, 15.0 ]
    let opposite = polygon [ 5.0, 5.0; 5.0, 15.0; 15.0, 15.0; 15.0, 5.0 ]
    Assert.Equal(Ok(Winding 2), Path.winding (point 10.0 10.0) (Path.ofSubpaths [ outer; same ]))
    Assert.Equal(Ok(Winding 0), Path.winding (point 10.0 10.0) (Path.ofSubpaths [ outer; opposite ]))
    Assert.Equal(Ok BoundaryWinding, Path.winding (point 5.0 10.0) (Path.ofSubpaths [ outer; same ]))

[<Fact>]
let ``clockwise svg circle has positive winding`` () =
    let source =
        Subpath.create
            [ Arc { Start = point 1.0 0.0; Radius = point 1.0 1.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point -1.0 0.0 }
              Arc { Start = point -1.0 0.0; Radius = point 1.0 1.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 1.0 0.0 } ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok(Winding 1), Path.winding (point 0.0 0.0) (Path.singleton source))
    Assert.Equal(Ok(Winding 0), Path.winding (point 2.0 0.0) (Path.singleton source))

[<Fact>]
let ``path containment empty and move only paths are outside`` () =
    let sample = point 5.0 5.0
    Assert.Equal(Ok Outside, Path.containment sample Path.empty Nonzero)
    Assert.Equal(Ok Outside, Path.containment sample (Path.singleton (Subpath.empty sample)) Nonzero)

[<Fact>]
let ``path containment with rejects invalid options`` () =
    Assert.Equal(Error(InvalidContainmentTolerance 0.0<length>), Path.containmentWith (point 0.0 0.0) Path.empty Nonzero { Path.defaultContainmentOptions with Tolerance = 0.0<length> })

[<Fact>]
let ``segment intersections finds line crossing`` () =
    let found = Intersections.segment (line 0.0 0.0 10.0 10.0) (line 0.0 10.0 10.0 0.0) |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertNear 0.5 found.LeftT
    assertNear 0.5 found.RightT
    Assert.True(Point.near 1.0e-6<length> found.Point (point 5.0 5.0))

[<Fact>]
let ``segment intersections finds endpoint touch`` () =
    let found = Intersections.segment (line 0.0 0.0 10.0 0.0) (line 10.0 0.0 10.0 10.0) |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertNear 1.0 found.LeftT
    assertNear 0.0 found.RightT
    Assert.True(Point.near 1.0e-6<length> found.Point (point 10.0 0.0))

[<Fact>]
let ``segment intersections returns empty for disjoint lines`` () =
    Assert.Equal(Ok [], Intersections.segment (line 0.0 0.0 10.0 0.0) (line 0.0 5.0 10.0 5.0))

[<Fact>]
let ``segment intersections rejects overlapping lines`` () =
    Assert.Equal(Error OverlappingSegments, Intersections.segment (line 0.0 0.0 10.0 0.0) (line 5.0 0.0 15.0 0.0))

[<Fact>]
let ``segment intersections finds line curve crossings`` () =
    let found =
        Intersections.segment
            (line 0.0 5.0 20.0 5.0)
            (QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0))
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, found.Length)
    assertNear 0.146446609 found[0].LeftT
    assertNear 0.146446609 found[0].RightT
    assertLengthNear 5.0 found[0].Point.Y
    assertNear 0.853553391 found[1].LeftT
    assertNear 0.853553391 found[1].RightT
    assertLengthNear 5.0 found[1].Point.Y

[<Fact>]
let ``segment intersections finds line like cubic crossing`` () =
    let left = CubicBezier(point 0.0 0.0, point 0.1 0.1, point 2.5 2.5, point 3.0 3.0)
    let right = CubicBezier(point 1.0 0.0, point 1.0 0.5, point 1.0 2.2, point 1.0 3.0)
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertNear 0.411711782 found.LeftT
    assertNear 0.387612779 found.RightT
    Assert.True(Point.near 1.0e-6<length> found.Point (point 1.0 1.0))

[<Fact>]
let ``segment intersections certifies line arc crossing geometrically`` () =
    let arc = Arc { Start = point 70.0 6.0; Radius = point 24.0 24.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = false; End = point 46.0 30.0 }
    let found = Intersections.segment (line 0.0 24.0 120.0 24.0) arc |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(abs (found.Point.Y - 24.0<length>) <= 1.0e-9<length>)

[<Fact>]
let ``segment intersections certifies arc arc crossing geometrically`` () =
    let left = Arc { Start = point 120.0 24.0; Radius = point 24.0 24.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 96.0 0.0 }
    let right = Arc { Start = point 96.0 30.0; Radius = point 24.0 24.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 120.0 6.0 }
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let leftPoint = Segment.point left found.LeftT |> Result.defaultWith (failwithf "%A")
    let rightPoint = Segment.point right found.RightT |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance leftPoint found.Point <= 1.0e-9<length>)
    Assert.True(Point.distance rightPoint found.Point <= 1.0e-9<length>)
    Assert.True(Point.distance leftPoint rightPoint <= 1.0e-9<length>)

[<Fact>]
let ``segment intersections finds curve curve crossing`` () =
    let left = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let right = QuadraticBezier(point 0.0 20.0, point 10.0 0.0, point 20.0 20.0)
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(abs (found.LeftT - 0.5<parameter>) < 1.0e-5<parameter>)
    Assert.True(abs (found.RightT - 0.5<parameter>) < 1.0e-5<parameter>)
    Assert.True(Point.distance found.Point (point 10.0 10.0) < 0.0001<length>)

[<Fact>]
let ``segment intersections prefers shared endpoint over near endpoint minimum`` () =
    let left =
        CubicBezier(
            point 2.8150000000000004 2.8350002,
            point 2.8150000000000004 2.893157324236443,
            point 2.8236048558813205 2.9152343073348637,
            point 2.8236048558813205 2.9152343073348637)
    let right =
        CubicBezier(
            point 2.8236048558813205 2.9152343073348637,
            point 2.823382761239994 2.914671758602366,
            point 2.816706698732441 2.9041808032333547,
            point 2.8162911593757998 2.903741355683332)
    let raw = Intersections.segmentWith left right { Tolerance = 1.0e-9<length>; MaxDepth = 64; ParameterSnap = NoParameterSnap } |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let snapped = Intersections.segmentWith left right { Tolerance = 1.0e-9<length>; MaxDepth = 64; ParameterSnap = DecimalParameterSnap 7 } |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(1.0<parameter>, raw.LeftT)
    Assert.Equal(0.0<parameter>, raw.RightT)
    Assert.Equal(1.0<parameter>, snapped.LeftT)
    Assert.Equal(0.0<parameter>, snapped.RightT)
    Assert.True(Point.distance snapped.Point (point 2.8236048558813205 2.9152343073348637) <= 1.0e-9<length>)

[<Fact>]
let ``segment intersections with rejects invalid options`` () =
    let segment = line 0.0 0.0 10.0 0.0
    Assert.Equal(Error(InvalidIntersectionTolerance 0.0<length>), Intersections.segmentWith segment segment { Tolerance = 0.0<length>; MaxDepth = 32; ParameterSnap = NoParameterSnap })
    Assert.Equal(Error(InvalidIntersectionMaxDepth 0), Intersections.segmentWith segment segment { Tolerance = 1.0e-9<length>; MaxDepth = 0; ParameterSnap = NoParameterSnap })
    Assert.Equal(Error(InvalidIntersectionParameterSnapExponent 0), Intersections.segmentWith segment segment { Tolerance = 1.0e-9<length>; MaxDepth = 32; ParameterSnap = DecimalParameterSnap 0 })

[<Fact>]
let ``translated monotone cubics are certified disjoint`` () =
    let left = CubicBezier(point 0.0 0.0, point 3.0 1.0, point 7.0 1.0, point 10.0 0.0)
    let right = CubicBezier(point 0.0 0.00000001, point 3.0 1.00000001, point 7.0 1.00000001, point 10.0 0.00000001)
    Assert.Equal(Ok [], Intersections.segmentWith left right { Intersections.defaultOptions with Tolerance = 1.0e-9<length> })

[<Fact>]
let ``segment subpath intersections groups and orders results`` () =
    let source =
        Subpath.create
            [ line 5.0 -5.0 5.0 5.0
              line 5.0 5.0 10.0 5.0
              line 10.0 5.0 10.0 -5.0
              line 10.0 -5.0 5.0 -5.0
              line 5.0 -5.0 5.0 5.0 ]
        |> Result.defaultWith (failwithf "%A")
    let found = Intersections.segmentSubpath (line 20.0 0.0 0.0 0.0) source |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, found.Length)
    let firstPoint, firstT, firstParameters = found[0]
    let secondPoint, secondT, secondParameters = found[1]
    Assert.True(Point.near 1.0e-6<length> firstPoint (point 10.0 0.0))
    assertNear 0.5 firstT
    let expectedFirst: SubpathParameter list = [ { SegmentIndex = 2; T = 0.5<parameter> } ]
    Assert.Equal<SubpathParameter list>(expectedFirst, firstParameters)
    Assert.True(Point.near 1.0e-6<length> secondPoint (point 5.0 0.0))
    assertNear 0.75 secondT
    let expectedSecond: SubpathParameter list = [ { SegmentIndex = 0; T = 0.5<parameter> }; { SegmentIndex = 4; T = 0.5<parameter> } ]
    Assert.Equal<SubpathParameter list>(expectedSecond, secondParameters)

[<Fact>]
let ``segment subpath intersections canonicalizes boundary aliases`` () =
    let middle = point 5.0 0.0
    let source = Subpath.create [ Line(point 0.0 -5.0, middle); Line(middle, point 10.0 -5.0) ] |> Result.defaultWith (failwithf "%A")
    let foundPoint, segmentT, parameters = Intersections.segmentSubpath (line 0.0 0.0 10.0 0.0) source |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(Point.near 1.0e-6<length> foundPoint middle)
    assertNear 0.5 segmentT
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 1; T = 0.0<parameter> } ], parameters)

[<Fact>]
let ``segment subpath intersections canonicalizes closed boundary aliases`` () =
    let a = point 5.0 0.0
    let source =
        Subpath.create [ Line(a, point 0.0 -5.0); line 0.0 -5.0 10.0 -5.0; Line(point 10.0 -5.0, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let foundPoint, segmentT, parameters = Intersections.segmentSubpath (line 0.0 0.0 10.0 0.0) source |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(Point.near 1.0e-6<length> foundPoint a)
    assertNear 0.5 segmentT
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 0; T = 0.0<parameter> } ], parameters)

[<Fact>]
let ``segment subpath intersections empty subpath`` () =
    Assert.Equal(Ok [], Intersections.segmentSubpath (line 0.0 0.0 10.0 0.0) (Subpath.empty (point 5.0 0.0)))

[<Fact>]
let ``segment subpath intersections propagates errors`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let source = Subpath.ofSegment segment
    Assert.Equal(Error(InvalidIntersectionTolerance 0.0<length>), Intersections.segmentSubpathWith segment source { Intersections.defaultOptions with Tolerance = 0.0<length> })
    Assert.Equal(Error OverlappingSegments, Intersections.segmentSubpath segment source)

let private semanticallyEqualArcs () =
    let make largeArc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>; LargeArc = largeArc; Sweep = true; End = point 10.0 0.0 }
    make false, make true

[<Fact>]
let ``segment subpath intersections rejects semantic arc overlap`` () =
    let left, right = semanticallyEqualArcs ()
    Assert.Equal(Error OverlappingSegments, Intersections.segmentSubpath left (Subpath.ofSegment right))

[<Fact>]
let ``subpath intersections groups and orders results`` () =
    let left = Subpath.polyline [ point 0.0 0.0; point 20.0 0.0; point 20.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let right = Subpath.polyline [ point 5.0 -5.0; point 5.0 5.0; point 15.0 5.0; point 15.0 -5.0 ] |> Result.defaultWith (failwithf "%A")
    let found = Intersections.subpath left right |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, found.Length)
    Assert.True(Point.near 1.0e-6<length> found[0].Point (point 5.0 0.0))
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 0; T = 0.25<parameter> } ], found[0].LeftParameters)
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 0; T = 0.5<parameter> } ], found[0].RightParameters)
    Assert.True(Point.near 1.0e-6<length> found[1].Point (point 15.0 0.0))
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 0; T = 0.75<parameter> } ], found[1].LeftParameters)
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 2; T = 0.5<parameter> } ], found[1].RightParameters)

[<Fact>]
let ``subpath intersections canonicalizes boundary aliases on both sides`` () =
    let middle = point 5.0 0.0
    let left = Subpath.create [ Line(point 0.0 0.0, middle); Line(middle, point 10.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let right = Subpath.create [ Line(point 5.0 -5.0, middle); Line(middle, point 5.0 5.0) ] |> Result.defaultWith (failwithf "%A")
    let found = Intersections.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(Point.near 1.0e-6<length> found.Point middle)
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 1; T = 0.0<parameter> } ], found.LeftParameters)
    Assert.Equal<SubpathParameter list>([ { SegmentIndex = 1; T = 0.0<parameter> } ], found.RightParameters)

[<Fact>]
let ``subpath intersections empty subpaths`` () =
    let empty = Subpath.empty (point 0.0 0.0)
    let segment = Subpath.ofSegment (line 0.0 0.0 10.0 0.0)
    Assert.Equal(Ok [], Intersections.subpath empty segment)
    Assert.Equal(Ok [], Intersections.subpath segment empty)

[<Fact>]
let ``subpath intersections propagates errors`` () =
    let source = Subpath.ofSegment (line 0.0 0.0 10.0 0.0)
    Assert.Equal(Error(InvalidIntersectionTolerance 0.0<length>), Intersections.subpathWith source source { Intersections.defaultOptions with Tolerance = 0.0<length> })
    Assert.Equal(Error OverlappingSegments, Intersections.subpath source source)

[<Fact>]
let ``subpath intersections reject semantic arc overlap`` () =
    let left, right = semanticallyEqualArcs ()
    Assert.Equal(Error OverlappingSegments, Intersections.subpath (Subpath.ofSegment left) (Subpath.ofSegment right))

let private pathParameter subpathIndex segmentIndex t: PathParameter =
    { SubpathIndex = subpathIndex; At = { SegmentIndex = segmentIndex; T = Parameter.fromFloat t } }

[<Fact>]
let ``path intersections groups and orders results`` () =
    let left = Path.ofSubpaths [ Subpath.ofSegment (line 20.0 0.0 0.0 0.0); Subpath.ofSegment (line 20.0 10.0 0.0 10.0) ]
    let right = Path.ofSubpaths [ Subpath.ofSegment (line 15.0 -5.0 15.0 5.0); Subpath.ofSegment (line 5.0 -5.0 5.0 15.0) ]
    let found = Intersections.path left right |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, found.Length)
    Assert.True(Point.near 1.0e-6<length> found[0].Point (point 15.0 0.0))
    Assert.Equal<PathParameter list>([ pathParameter 0 0 0.25 ], found[0].LeftParameters)
    Assert.Equal<PathParameter list>([ pathParameter 0 0 0.5 ], found[0].RightParameters)
    Assert.True(Point.near 1.0e-6<length> found[1].Point (point 5.0 0.0))
    Assert.Equal<PathParameter list>([ pathParameter 0 0 0.75 ], found[1].LeftParameters)
    Assert.Equal<PathParameter list>([ pathParameter 1 0 0.25 ], found[1].RightParameters)
    Assert.True(Point.near 1.0e-6<length> found[2].Point (point 5.0 10.0))
    Assert.Equal<PathParameter list>([ pathParameter 1 0 0.75 ], found[2].LeftParameters)
    Assert.Equal<PathParameter list>([ pathParameter 1 0 0.75 ], found[2].RightParameters)

[<Fact>]
let ``path intersections canonicalizes aliases on both sides`` () =
    let middle = point 5.0 0.0
    let left = Path.singleton (Subpath.create [ Line(point 0.0 0.0, middle); Line(middle, point 10.0 0.0) ] |> Result.defaultWith (failwithf "%A"))
    let right = Path.ofSubpaths [ Subpath.empty (point 100.0 100.0); Subpath.create [ Line(point 5.0 -5.0, middle); Line(middle, point 5.0 5.0) ] |> Result.defaultWith (failwithf "%A") ]
    let found = Intersections.path left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(Point.near 1.0e-6<length> found.Point middle)
    Assert.Equal<PathParameter list>([ pathParameter 0 1 0.0 ], found.LeftParameters)
    Assert.Equal<PathParameter list>([ pathParameter 1 1 0.0 ], found.RightParameters)

[<Fact>]
let ``path intersections canonicalizes near boundary aliases`` () =
    let middle = point 10.0 0.0
    let left =
        Path.singleton (
            Subpath.create [ Line(point 0.0 0.0, middle); Line(middle, point 20.0 0.0) ]
            |> Result.defaultWith (failwithf "%A"))
    let right =
        Path.ofSubpaths
            [ Subpath.ofSegment (line 9.9999999999 -5.0 9.9999999999 5.0)
              Subpath.ofSegment (line 10.0000000001 -5.0 10.0000000001 5.0) ]
    let options =
        { Tolerance = 0.000001<length>
          MaxDepth = 48
          ParameterSnap = DecimalParameterSnap 7 }
    let found = Intersections.pathWith left right options |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(Point.near 1.0e-6<length> found.Point middle)
    Assert.Equal<PathParameter list>([ pathParameter 0 1 0.0 ], found.LeftParameters)
    Assert.Equal<PathParameter list>([ pathParameter 0 0 0.5; pathParameter 1 0 0.5 ], found.RightParameters)

[<Fact>]
let ``path intersections empty paths`` () =
    let moveOnly = Path.singleton (Subpath.empty (point 0.0 0.0))
    let segment = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    Assert.Equal(Ok [], Intersections.path Path.empty segment)
    Assert.Equal(Ok [], Intersections.path segment Path.empty)
    Assert.Equal(Ok [], Intersections.path moveOnly segment)
    Assert.Equal(Ok [], Intersections.path segment moveOnly)

[<Fact>]
let ``path intersections propagates errors`` () =
    let source = Path.singleton (Subpath.ofSegment (line 0.0 0.0 10.0 0.0))
    Assert.Equal(Error(InvalidIntersectionTolerance 0.0<length>), Intersections.pathWith source source { Intersections.defaultOptions with Tolerance = 0.0<length> })
    Assert.Equal(Error OverlappingSegments, Intersections.path source source)

[<Fact>]
let ``path intersections reject semantic arc overlap`` () =
    let left, right = semanticallyEqualArcs ()
    Assert.Equal(Error OverlappingSegments, Intersections.path (Path.singleton (Subpath.ofSegment left)) (Path.singleton (Subpath.ofSegment right)))

[<Fact>]
let ``segment intersections match returned parameters`` () =
    let consistent left right =
        Intersections.segment left right
        |> Result.defaultWith (failwithf "%A")
        |> List.forall (fun found ->
            found.LeftT >= -1.0e-6<parameter> && found.LeftT <= 1.000001<parameter>
            && found.RightT >= -1.0e-6<parameter> && found.RightT <= 1.000001<parameter>
            && (Segment.point left found.LeftT |> Result.exists (fun actual -> Point.distance actual found.Point <= 0.0001<length>))
            && (Segment.point right found.RightT |> Result.exists (fun actual -> Point.distance actual found.Point <= 0.0001<length>)))
    let quadraticA = QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let quadraticB = QuadraticBezier(point 0.0 20.0, point 10.0 0.0, point 20.0 20.0)
    let cubic = CubicBezier(point 0.0 0.0, point 0.0 20.0, point 20.0 20.0, point 20.0 0.0)
    Assert.True(consistent (line 0.0 0.0 20.0 20.0) (line 0.0 20.0 20.0 0.0))
    Assert.True(consistent (line 0.0 10.0 20.0 10.0) quadraticA)
    Assert.True(consistent (line 0.0 10.0 20.0 10.0) cubic)
    Assert.True(consistent quadraticA quadraticB)
    Assert.True(consistent (line 10.0 -20.0 10.0 5.0) (semicircle ()))
