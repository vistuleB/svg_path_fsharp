module SvgPath.Tests.SignedZeroTests

// One-to-one regression port from Gleam 87e7e75.
open SvgPath
open Xunit

let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private unwrap x = Result.defaultWith (failwithf "%A") x

[<Fact>]
let ``is_zero_accepts_both_signs_without_a_tolerance_test`` () =
    for x in [0.0; -0.0; 0.0 * -1.0] do Assert.True(InternalNumber.isZero x)
    for x in [1.0; -1.0; 1e-310; -1e-310] do Assert.False(InternalNumber.isZero x)

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
    Assert.Equal(Error DegenerateSourceTriple,
        Affine.pointTripleMap (p 0.0 0.0) (p -1.0 0.0) (p 1.0 0.0)
            (p 0.0 0.0) (p 1.0 0.0) (p 0.0 1.0))

[<Fact>]
let ``roots_reduce_degree_for_both_signs_of_zero_test`` () =
    Assert.Empty(Root.linear -0.0 1.0)
    Assert.Empty(Root.linear -0.0 -0.0)
    Assert.Equal<float<parameter> list>([1.0<parameter>], Root.quadratic -0.0 1.0 -1.0)
    Assert.Empty(Root.quadratic -0.0 -0.0 1.0)
    let options = { CoefficientTolerance = -0.0; RepeatedRootPolicy = ConsolidateRepeatedRoot }
    Assert.Equal<float<parameter> list>([1.0<parameter>], Root.quadraticWith options -0.0 1.0 -1.0)

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
