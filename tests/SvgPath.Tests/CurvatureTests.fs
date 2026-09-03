module SvgPath.Tests.CurvatureTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value
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
            { Start = point 4.0 0.0
              Radius = point 4.0 4.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 0.0 4.0 }
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
