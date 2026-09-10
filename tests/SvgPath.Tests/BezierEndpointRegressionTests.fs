module SvgPath.Tests.BezierEndpointRegressionTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private curves =
    let start,finish = p 1. -0.,p 0.1 0.2
    [LinearBezierData(start,finish)
     QuadraticBezierData(start,p 0.3 1.,finish)
     CubicBezierData(start,p 0.3 1.,p -1. 0.7,finish)]

[<Fact>]
let ``tangent fit rejects endpoint only samples`` () =
    let start,finish = p 0. 0.,p 3. 0.
    Assert.Equal(Error UnderdeterminedCubicFit,
        Bezier.fitCubicWithEndpointTangents start finish (Point.create 1.0 1.0) (Point.create 1.0 -1.0)
            [0.0<parameter>,start; -0.0<parameter>,start; 1.0<parameter>,finish])
[<Fact>]
let ``evaluation preserves exact endpoints`` () =
    for curve in curves do
        Assert.Equal(Bezier.start curve,Bezier.point curve 0.0<parameter>)
        Assert.Equal(Bezier.start curve,Bezier.point curve -0.0<parameter>)
        Assert.Equal(Bezier.finish curve,Bezier.point curve 1.0<parameter>)
[<Fact>]
let ``endpoint splits preserve whole curve and collapse all controls`` () =
    for curve in curves do
        for t in [0.0<parameter>;-0.0<parameter>] do
            let collapsed,whole = Bezier.splitInside curve t |> Result.defaultWith (failwithf "%A")
            Assert.Equal(curve,whole)
            Assert.Equal(Bezier.mapPoints (fun _ -> Bezier.start curve) curve,collapsed)
        let whole,collapsed = Bezier.splitInside curve 1.0<parameter> |> Result.defaultWith (failwithf "%A")
        Assert.Equal(curve,whole)
        Assert.Equal(Bezier.mapPoints (fun _ -> Bezier.finish curve) curve,collapsed)

[<Fact>]
let ``split many trims both signed zero boundaries`` () =
    let curve = LinearBezierData(p 1. 0.,p 0.1 0.2)
    Assert.Equal<BezierData list>([curve],Bezier.splitMany curve [-0.0<parameter>])
    Assert.Equal<BezierData list>([curve],Bezier.splitMany curve [0.0<parameter>;-0.0<parameter>;0.0<parameter>;1.0<parameter>])
    Assert.Equal(Ok [curve],Bezier.splitInsideMany curve [-0.0<parameter>;0.0<parameter>;-0.0<parameter>;1.0<parameter>])

[<Fact>]
let ``split many deduplicates signed zero inside extrapolated range`` () =
    let curve = LinearBezierData(p 0. 0.,p 1. 0.)
    let expected = Bezier.splitMany curve [-0.5<parameter>;0.0<parameter>;0.5<parameter>]
    Assert.Equal(4,List.length expected)
    Assert.Equal<BezierData list>(expected,Bezier.splitMany curve [0.5<parameter>;-0.0<parameter>;-0.5<parameter>;0.0<parameter>;-0.0<parameter>])
