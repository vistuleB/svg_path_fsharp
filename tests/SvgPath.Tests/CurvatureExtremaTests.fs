module SvgPath.Tests.CurvatureExtremaTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private options tolerance : CurvatureOptions = { Tolerance=Parameter.fromFloat tolerance; MaxDepth=48 }
let private arch = CubicBezier(p 0. 0.,p 1. 0.,p 1. 0.,p 1. -1.)
let private near tolerance expected actual = Assert.True(abs(actual-expected)<Parameter.fromFloat tolerance)
let private parabola = QuadraticBezier(p 0. 0.,p 0.5 0.,p 1. 1.)
[<Fact>]
let ``cusp depth exhaustion reports remaining bracket`` () =
    Assert.Equal(Error(CurvatureMaxDepthReached(0.0<parameter>,0.5<parameter>)),
        Curvature.segmentLeftNormalCuspParameters parabola -1.0<length> { Tolerance=1e-12<parameter>; MaxDepth=1 })
[<Fact>]
let ``cusp exact root at depth limit succeeds`` () =
    let offset = -1.25 * sqrt 1.25 / 2.0 |> Length.fromFloat
    Assert.Equal(Ok [0.25<parameter>],Curvature.segmentLeftNormalCuspParameters parabola offset { Tolerance=0.0<parameter>; MaxDepth=1 })
[<Fact>]
let ``cusp interval converged at depth limit succeeds`` () =
    Assert.Equal(Ok [0.25<parameter>],Curvature.segmentLeftNormalCuspParameters parabola -1.0<length> { Tolerance=0.5<parameter>; MaxDepth=1 })
[<Fact>]
let ``shifted stationary cubics keep both neighboring intervals`` () =
    let offset = -4.09 * sqrt 4.09 * 0.1 / 6.0 |> Length.fromFloat
    for i in 1..98 do
        let r = float i / 100.0
        let start = p (r*r) (-r*r*r)
        let c1 = p (float start.X - 2.0*r/3.0) (float start.Y+r*r)
        let c2 = p (1.0/3.0+2.0*float c1.X-float start.X) (-r+2.0*float c1.Y-float start.Y)
        let u = 1.0-r
        let curve = CubicBezier(start,c1,c2,p (u*u) (u*u*u))
        let actual = Curvature.segmentLeftNormalCuspParameters curve offset (options 1e-10) |> get
        let expected = [r-0.1;r+0.1] |> List.filter (fun t -> t>=0.0 && t<=1.0) |> List.map Parameter.fromFloat
        Assert.Equal(expected.Length,actual.Length)
        List.iter2 (near 1e-6) expected actual
[<Fact>]
let ``touching cusp between sample points`` () =
    let offset = Curvature.segmentLeftNormalRadius arch 0.5<parameter> |> get
    let roots = Curvature.segmentLeftNormalCuspParameters arch offset (options 1e-9) |> get
    near 1e-8 0.5<parameter> (Assert.Single roots)
[<Fact>]
let ``two cusps in one old sample window`` () =
    let radius = Curvature.segmentLeftNormalRadius arch 0.5<parameter> |> get
    let roots = Curvature.segmentLeftNormalCuspParameters arch (radius+0.000001<length>) (options 1e-10) |> get
    Assert.Equal(2,roots.Length)
    Assert.True(roots[0]>Parameter.fromFloat(49.0/99.0) && roots[0]<0.5<parameter>)
    Assert.True(roots[1]>0.5<parameter> && roots[1]<Parameter.fromFloat(50.0/99.0))
[<Fact>]
let ``touching cusp close miss is rejected`` () =
    let radius = Curvature.segmentLeftNormalRadius arch 0.5<parameter> |> get
    Assert.Equal(Ok [],Curvature.segmentLeftNormalCuspParameters arch (radius-0.000001<length>) Curvature.defaultOptions)
[<Fact>]
let ``stationary cubic endpoint does not hide cusp`` () =
    let curve = CubicBezier(p 0. 0.,p 0. 0.,p (1.0/3.0) 0.,p 1. 1.)
    let roots = Curvature.segmentLeftNormalCuspParameters curve (-125.0<length>/96.0) (options 1e-10) |> get
    near 1e-8 0.5<parameter> (Assert.Single roots)
[<Fact>]
let ``stationary interior is not a cusp root`` () =
    let curve = CubicBezier(p 0.25 -0.125,p (-1.0/12.0) 0.125,p (-1.0/12.0) -0.125,p 0.25 0.125)
    let roots = Curvature.segmentLeftNormalCuspParameters curve -1.0<length> (options 1e-10) |> get
    Assert.Equal(2,roots.Length)
    Assert.True(roots[0]<0.5<parameter> && roots[1]>0.5<parameter>)
    near 1e-8 1.0<parameter> (roots[0]+roots[1])
    Assert.Equal(Ok [],Curvature.segmentLeftNormalCuspParameters curve 0.0<length> Curvature.defaultOptions)
let private arc radius finish = Arc { Start=p 4. 0.; Radius=radius; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=finish }
[<Fact>]
let ``circle constant cusp returns interval endpoints`` () =
    let curve = arc (p 4. 4.) (p 0. 4.)
    Assert.Equal(Ok [0.0<parameter>;1.0<parameter>],Curvature.segmentLeftNormalCuspParameters curve -4.0<length> Curvature.defaultOptions)
    Assert.Equal(Ok [],Curvature.segmentLeftNormalCuspParameters curve 4.0<length> Curvature.defaultOptions)
[<Fact>]
let ``elliptical touch between sample points`` () =
    let roots = Curvature.segmentLeftNormalCuspParameters (arc (p 4. 2.) (p -4. 0.)) -8.0<length> (options 1e-9) |> get
    near 1e-8 0.5<parameter> (Assert.Single roots)
