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
