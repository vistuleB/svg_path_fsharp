module SvgPath.Tests.SignedZeroTests

// One-to-one regression port from Gleam 87e7e75.
open SvgPath
open Xunit

let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private unwrap x = Result.defaultWith (failwithf "%A") x

[<Fact>]
let ``subpath overlap accepts negative zero endpoint alias`` () =
    let left = Subpath.polyline [p 0. 0.;p 1. 0.;p 2. 0.] |> unwrap
    let right = Subpath.polyline [p 0.5 0.;p 1. 0.] |> unwrap
    let correspondence: Overlaps.SegmentOverlap = { LeftFrom=0.5<parameter>; LeftTo=1.0<parameter>; RightFrom=0.0<parameter>; RightTo=1.0<parameter>; Start=p 0.5 0.; Finish=p 1. 0. }
    let overlap: Overlaps.SubpathOverlap = { Start=correspondence.Start; Finish=correspondence.Finish; Pieces=[({ LeftSegmentIndex=0; RightSegmentIndex=0; Correspondence=correspondence }: Overlaps.SubpathOverlapPiece)] }
    Assert.Equal(Ok(Some { SegmentIndex=0; T=1.0<parameter> }),Overlaps.subpathOverlapRightParameter overlap { SegmentIndex=1; T= -0.0<parameter> } left right)

[<Fact>]
let ``signed zero matrix entries do not add operations`` () =
    Assert.Equal("translate(0)",TransformSerialize.toString(Affine.fromTuple (1.0,-0.0,-0.0,1.0,-0.0<length>,-0.0<length>)))
    Assert.Equal("scale(2)",TransformSerialize.toString(Affine.fromTuple (2.0,-0.0,-0.0,2.0,-0.0<length>,-0.0<length>)))
    Assert.Equal("translate(10)",TransformSerialize.toString(Affine.translate 10.0<length> -0.0<length>))

[<Fact>]
let ``reversed line still has infinite radius`` () =
    let line = Line(p 1. 0.,p 0. 0.)
    Assert.Equal(Error Curvature.InfiniteRadiusOfCurvature,Curvature.segmentLeftNormalRadius line 0.5<parameter>)
    Assert.Equal(Error Curvature.InfiniteRadiusOfCurvature,Curvature.segmentLeftNormalRadiusCloseTo line 1.0<length> 0.1<length> 0.5<parameter>)
    Assert.Equal(Error Curvature.InfiniteRadiusOfCurvature,Curvature.segmentLeftNormalRadiusCloseTo (Segment.reverse line) 1.0<length> 0.1<length> 0.5<parameter>)

[<Fact>]
let ``negative zero sizes disable rendering`` () =
    Assert.Equal(Error BasicShapes.DisabledRendering,BasicShapes.circle 0.0<length> 0.0<length> -0.0<length>)
    Assert.Equal(Error BasicShapes.DisabledRendering,BasicShapes.ellipse 0.0<length> 0.0<length> -0.0<length> 1.0<length>)
    Assert.Equal(Error BasicShapes.DisabledRendering,BasicShapes.ellipse 0.0<length> 0.0<length> 1.0<length> -0.0<length>)
    Assert.Equal(Error BasicShapes.DisabledRendering,BasicShapes.rect 0.0<length> 0.0<length> -0.0<length> 1.0<length> None None)
    Assert.Equal(Error BasicShapes.DisabledRendering,BasicShapes.rect 0.0<length> 0.0<length> 1.0<length> -0.0<length> None None)

[<Fact>]
let ``negative zero length returns exact start parameter`` () =
    let curve = QuadraticBezier(p 0. 0.,p 1. 1.,p 2. 0.)
    let subpath = Subpath.create [curve] |> unwrap
    Assert.Equal(Ok 0.0<parameter>,Segment.parameterAtLength curve -0.0<length>)
    Assert.Equal(Ok { SegmentIndex=0; T=0.0<parameter> },Subpath.parameterAtLength subpath -0.0<length>)
[<Fact>]
let ``negative zero radius degenerates to line`` () =
    let start,finish = p 1. 1.,p 2. 2.
    let arc = Arc ({ Start=start; Radius=p -0. 1.; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=finish }: Ellipse.EndpointArcData)
    Assert.Equal(Ok(Some [Line(start,finish)]),Degeneracy.segmentLinearizeIfDegenerate arc 0.0<length>)

[<Fact>]
let ``negative zero endpoint has forward offset unit tangent`` () =
    Assert.Equal(Ok(Point.create 1.0 0.0),Offset.unitTangent (Line(p 0. 0.,p 1. 0.)) -0.0<parameter>)

let private zeroTestArc =
    ({ Center=p 0. 0.; Radius=p 1. 1.; XAxisRotation=0.0<degree>; StartAngle=0.0<degree>; DeltaAngle=360.0<degree> }: Ellipse.CenterArcData)
[<Fact>]
let ``signed zero ellipse split parameters are canonical`` () =
    let arc = zeroTestArc
    Assert.Equal<Ellipse.CenterArcData list>([arc],Ellipse.arcSplitMany arc [-0.0<parameter>;0.0<parameter>])
    Assert.Equal(Ok [arc],Ellipse.arcSplitManyInside arc [-0.0<parameter>;0.0<parameter>])
    let expected = Ellipse.arcSplitMany arc [-0.5<parameter>;0.0<parameter>;0.5<parameter>]
    Assert.Equal(4,List.length expected)
    Assert.Equal<Ellipse.CenterArcData list>(expected,Ellipse.arcSplitMany arc [-0.5<parameter>;-0.0<parameter>;0.0<parameter>;0.5<parameter>])
[<Fact>]
let ``either zero direction has no ellipse projection extrema`` () =
    for x in [0.0;-0.0] do
        for y in [0.0;-0.0] do
            Assert.Empty(Ellipse.arcProjectionExtrema zeroTestArc (Point.create x y))

[<Fact>]
let ``signed zero dash patterns are continuous`` () =
    let subpath = Subpath.create [Line(p 0. 0.,p 1. 0.)] |> unwrap
    for pattern in [[-0.0<length>];[0.0<length>;-0.0<length>];[-0.0<length>;0.0<length>;-0.0<length>]] do
        Assert.Equal(Ok [subpath],Stroke.subpathDashes subpath pattern 0.0<length>)

[<Fact>]
let ``is_zero_accepts_both_signs_without_a_tolerance_test`` () =
    for x in [0.0; -0.0; 0.0 * -1.0] do Assert.True(InternalNumber.isZero x)
    for x in [1.0; -1.0; 1e-310; -1e-310] do Assert.False(InternalNumber.isZero x)

[<Fact>]
let ``normalize_zero_canonicalizes_only_exact_zeros_test`` () =
    for x in [0.0; -0.0; 0.0 * -1.0] do
        Assert.Equal(0L,System.BitConverter.DoubleToInt64Bits(InternalNumber.normalizeZero x))
    for x in [1.0; -1.0; 1e-310; -1e-310] do Assert.Equal(x,InternalNumber.normalizeZero x)

[<Fact>]
let ``signed_zero_vectors_have_zero_heading_and_no_direction_test`` () =
    for x in [0.0; -0.0] do
        for y in [0.0; -0.0] do
            let zero = p x y
            Assert.Equal(0.0<degree>, Point.heading zero)
            Assert.True(Point.normalize zero |> Option.isNone)
            Assert.True(Point.project Point.right zero |> Option.isNone)
            Assert.True(Point.scalarProjection Point.right zero |> Option.isNone)
    Assert.Equal(0.0<degree>, Point.heading (Point.scale -1.0 (p 0.0 0.0)))

[<Fact>]
let ``atan2_handles_signed_zero_axes_test`` () =
    Assert.Equal(90.0<degree>, Trig.atan2Degrees 1.0 -0.0)
    Assert.Equal(-90.0<degree>, Trig.atan2Degrees -1.0 -0.0)
    Assert.Equal(0.0<degree>, Trig.atan2Degrees -0.0 1.0)
    Assert.Equal(180.0<degree>, Trig.atan2Degrees -0.0 -1.0)
    Assert.Equal(-180.0<degree>, Trig.atan2Degrees -0.0 -0.0)

[<Fact>]
let ``affine_rejects_negative_zero_determinants_test`` () =
    Assert.Equal(Error Affine.DegenerateSourceTriple,
        Affine.pointTripleMap (p 0.0 0.0) (p -1.0 0.0) (p 1.0 0.0)
            (p 0.0 0.0) (p 1.0 0.0) (p 0.0 1.0))

[<Fact>]
let ``roots_reduce_degree_for_both_signs_of_zero_test`` () =
    Assert.Empty(Root.linear -0.0 1.0)
    Assert.Empty(Root.linear -0.0 -0.0)
    Assert.Equal<float<parameter> list>([1.0<parameter>], Root.parameterQuadratic -0.0 1.0 -1.0)
    Assert.Empty(Root.parameterQuadratic -0.0 -0.0 1.0)
    let options = { CoefficientTolerance = -0.0; RepeatedRootPolicy = ConsolidateRepeatedRoot }
    Assert.Equal<float<parameter> list>([1.0<parameter>], Root.parameterQuadraticWith options -0.0 1.0 -1.0)

[<Fact>]
let ``bisection_accepts_negative_zero_at_endpoint_test`` () =
    let isolation = Root.bisectIsolationUntil (fun t -> t * -1.0) 0.0<parameter> 1.0<parameter> 2 (fun _ _ -> false) |> unwrap
    Assert.True(InternalNumber.isZero isolation.Estimate)

[<Fact>]
let ``negative_zero_exponents_stop_after_underflow_test`` () =
    Assert.True(InternalNumber.parse "-1e-1000000000000000" |> unwrap |> InternalNumber.isZero)
    Assert.True(InternalNumber.parse "-0e1000000000000000" |> unwrap |> InternalNumber.isZero)

[<Fact>]
let ``negative_zero_line_does_not_erase_polyline_corners_test`` () =
    let path = Parse.path "M0 0 L-0 -0 L1 0 L1 1 L2 1" |> unwrap
    let source = Path.subpaths path |> List.exactlyOne
    let normalized = Degeneracy.normalizeDegenerateSegments source 0.0<length> |> unwrap
    Assert.Equal<Segment list>([
        Line(p 0.0 0.0, p 1.0 0.0)
        Line(p 1.0 0.0, p 1.0 1.0)
        Line(p 1.0 1.0, p 2.0 1.0)
    ], normalized.Segments)
