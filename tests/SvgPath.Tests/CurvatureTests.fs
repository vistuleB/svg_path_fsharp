module SvgPath.Tests.CurvatureTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

[<Fact>]
let ``line_cusp_residual_is_speed_cubed_test`` () =
    let line = Line(point 0.0 0.0, point 3.0 4.0)
    Assert.Equal(Ok 125.0<length^3 / parameter^3>,
        Curvature.segmentLeftNormalCuspResidual line 2.0<length> (parameter 0.5))

[<Fact>]
let ``invalid_arc_curvature_preserves_path_error_test`` () =
    let arc = Arc ({ Start = point 0.0 0.0; Radius = point 1.0 1.0
                     XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 0.0 0.0 }: Ellipse.EndpointArcData)
    Assert.Equal(Error(Curvature.CurvaturePathError SegmentError.DegenerateArc),
        Curvature.segmentLeftNormalCurvature arc (parameter 0.5))
let private downwardCubic = CubicBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 0.0, point 1.0 1.0)
let private upwardCubic = CubicBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 0.0, point 1.0 -1.0)

[<Fact>]
let ``left normal radius uses offset normal sign`` () =
    let downward = Curvature.segmentLeftNormalRadius downwardCubic (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    let upward = Curvature.segmentLeftNormalRadius upwardCubic (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(-0.2651650429449553, Length.toFloat downward, 9)
    Assert.Equal(0.2651650429449553, Length.toFloat upward, 9)

[<Fact>]
let ``left normal cusp parameters match positive offset side`` () =
    let options = Curvature.defaultOptions
    Assert.Equal(Ok [], Curvature.segmentLeftNormalCuspParameters downwardCubic 0.27<length> options)
    let parameters = Curvature.segmentLeftNormalCuspParameters upwardCubic 0.27<length> options |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, parameters.Length)
    Assert.Equal(0.4786978280544282, Parameter.ratio parameters[0], 9)
    Assert.Equal(0.5213021719455719, Parameter.ratio parameters[1], 9)

[<Fact>]
let ``arc curvature uses exact ellipse derivatives`` () =
    let arc =
        Arc
            ({ Start = point 4.0 0.0
               Radius = point 4.0 4.0
               XAxisRotation = Degree.fromFloat 0.0
               LargeArc = false
               Sweep = true
               End = point 0.0 4.0 }: Ellipse.EndpointArcData)
    let radius = Curvature.segmentLeftNormalRadius arc (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs (radius + 4.0<length>) < 1.0e-12<length>)

[<Fact>]
let ``segment inflection parameters detect cubic inflection`` () =
    let curve = CubicBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 -1.0, point 3.0 0.0)
    let roots = Curvature.segmentInflectionParameters curve Curvature.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Single(roots) |> ignore
    Assert.True(abs (List.head roots - parameter 0.5) < parameter 1.0e-12)

[<Fact>]
let ``segment inflection parameters ignore flat cubic`` () =
    let curve = CubicBezier(point 0.0 0.0, point (1.0 / 3.0) 0.0, point (2.0 / 3.0) 0.0, point 1.0 0.0)
    Assert.Equal(Ok [], Curvature.segmentInflectionParameters curve Curvature.defaultOptions)

[<Fact>]
let ``cusp parameters retain exact sampled root`` () =
    let parabola = QuadraticBezier(point -1.0 1.0, point 0.0 0.0, point 1.0 1.0)
    let roots = Curvature.segmentLeftNormalCuspParameters parabola -1.0<length> Curvature.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Contains(roots, fun root -> abs (Parameter.ratio root - 0.5) <= 1.0e-9)

[<Fact>]
let ``curvature options report offending values`` () =
    let defaults = Curvature.defaultOptions
    Assert.Equal(Error(Curvature.InvalidCurvatureTolerance -0.5<parameter>),
        Curvature.segmentLeftNormalCuspParameters upwardCubic 0.27<length> { defaults with Tolerance = -0.5<parameter> })
    Assert.Equal(Error(Curvature.InvalidCurvatureMaxDepth 0),
        Curvature.segmentInflectionParameters upwardCubic { defaults with MaxDepth = 0 })

[<Fact>]
let ``zero tolerance reports unconverged curvature bracket`` () =
    let options = { Curvature.defaultOptions with Tolerance = 0.0<parameter> }
    // Valid zero tolerance does not promise an exact root within the budget.
    match Curvature.segmentLeftNormalCuspParameters upwardCubic 0.27<length> options with
    | Error(Curvature.CurvatureMaxDepthReached(lower,upper)) ->
        Assert.True(lower<upper)
        Assert.True(upper-lower<1e-9<parameter>)
        let a = Curvature.segmentLeftNormalCuspResidual upwardCubic 0.27<length> lower |> Result.defaultWith (failwithf "%A")
        let b = Curvature.segmentLeftNormalCuspResidual upwardCubic 0.27<length> upper |> Result.defaultWith (failwithf "%A")
        Assert.True(a*b<0.0<_>)
    | other -> failwithf "Expected unconverged bracket, got %A" other

[<Fact>]
let ``radius proximity validates margin`` () =
    Assert.Equal(Error(Curvature.InvalidCurvatureMargin -1.0<length>),
        Curvature.segmentLeftNormalRadiusCloseTo upwardCubic 0.27<length> -1.0<length> (parameter 0.5))

[<Fact>]
let ``collapsed derivative differs from infinite radius`` () =
    let p = point 1.0 1.0
    let collapsed = CubicBezier(p, p, p, p)
    Assert.Equal(Error Curvature.DegenerateCurvatureDerivative, Curvature.segmentLeftNormalRadius collapsed (parameter 0.5))
    Assert.Equal(Error Curvature.DegenerateCurvatureDerivative, Curvature.segmentLeftNormalCurvature collapsed (parameter 0.5))
    Assert.Equal(Error Curvature.DegenerateCurvatureDerivative, Curvature.segmentLeftNormalCuspResidual collapsed 1.0<length> (parameter 0.5))
    Assert.Equal(Error Curvature.DegenerateCurvatureDerivative, Curvature.segmentLeftNormalRadiusCloseTo collapsed 1.0<length> 0.1<length> (parameter 0.5))
    Assert.Equal(Error Curvature.InfiniteRadiusOfCurvature, Curvature.segmentLeftNormalRadiusCloseTo (Line(p, point 2.0 1.0)) 1.0<length> 0.1<length> (parameter 0.5))
