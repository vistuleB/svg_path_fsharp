module SvgPath.Tests.CurvatureTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

[<Fact>]
let ``line has zero curvature and infinite radius`` () =
    let line = Line(point 0.0 0.0, point 4.0 0.0)
    Assert.Equal(Ok 0.0<1 / length>, Curvature.segmentLeftNormalCurvature line (parameter 0.5))
    Assert.Equal(Error(), Curvature.segmentLeftNormalRadius line (parameter 0.5))

[<Fact>]
let ``clockwise visual circle arc has negative left-normal curvature`` () =
    let arc =
        Arc
            { Start = point 4.0 0.0
              Radius = point 4.0 4.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 0.0 4.0 }
    let curvature = Curvature.segmentLeftNormalCurvature arc (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs (curvature + 0.25<1 / length>) < 1.0e-12<1 / length>)
    let radius = Curvature.segmentLeftNormalRadius arc (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs (radius + 4.0<length>) < 1.0e-12<length>)

[<Fact>]
let ``quadratic derivatives retain parameter powers`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 0.0)
    let data = Curvature.segmentDerivatives curve (parameter 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Point.create 2.0<length / parameter> 0.0<length / parameter>, data.First)
    Assert.Equal(Point.create 0.0<length / parameter^2> -4.0<length / parameter^2>, data.Second)

[<Fact>]
let ``cubic inflection is found algebraically`` () =
    let curve = CubicBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 -1.0, point 3.0 0.0)
    let roots = Curvature.segmentInflectionParameters curve Curvature.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Single(roots) |> ignore
    Assert.True(abs (List.head roots - parameter 0.5) < parameter 1.0e-12)

[<Fact>]
let ``circle radius is recognized within a length margin`` () =
    let arc =
        Arc
            { Start = point 4.0 0.0
              Radius = point 4.0 4.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 0.0 4.0 }
    Assert.Equal(
        Ok true,
        Curvature.segmentLeftNormalRadiusCloseTo arc -4.0<length> 1.0e-9<length> (parameter 0.5)
    )
