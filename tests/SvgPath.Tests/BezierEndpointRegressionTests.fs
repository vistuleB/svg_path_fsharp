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
