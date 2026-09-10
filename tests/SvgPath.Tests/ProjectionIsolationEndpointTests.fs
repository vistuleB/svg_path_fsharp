module SvgPath.Tests.ProjectionIsolationEndpointTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private original = QuadraticBezier(p 0. 0.,p 5. 1.1076024267822504,p 10. 0.)
let private fromT,toT = 0.24867968684993685<parameter>,0.802967485692352<parameter>
let private reversed () = Segment.between original fromT toT |> get |> Segment.reverse
[<Fact>]
let ``projection keeps better isolation endpoint without sign change`` () =
    let query = Segment.point original (fromT+(toT-fromT)*0.5) |> get
    let t,_,distance = Segment.projection (reversed()) query |> get
    Assert.True(distance<1e-12<length>)
    Assert.True(abs(t-0.5<parameter>)<1e-12<parameter>)
[<Fact>]
let ``reversed quadratic portion is not rejected by projection`` () =
    let other = reversed()
    let overlap = Overlaps.segment original other |> get |> Assert.Single
    for actual,expected in [overlap.LeftFrom,fromT;overlap.LeftTo,toT;overlap.RightFrom,1.0<parameter>;overlap.RightTo,0.0<parameter>] do
        Assert.True(abs(actual-expected)<1e-9<parameter>)
    let correspondence = Overlaps.checkParameterCorrespondence original other fromT toT 1.0<parameter> 0.0<parameter> 1e-9<length> 5 |> get
    Assert.True(correspondence.IsSome)
