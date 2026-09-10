module SvgPath.Tests.CubicBoundaryIntersectionTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private single curve =
    let hits = Bezier.cubicSelfIntersections curve |> Result.defaultWith (failwithf "%A")
    Assert.Single hits
let private near a b = Assert.True(abs(a-b)<1e-9<parameter>)
[<Fact>]
let ``cubic self intersections preserve closed endpoint parameters`` () =
    let hit = single (CubicBezierData(p 0. 0.,p 0.1 1.,p -1. 0.2,p 0. 0.))
    Assert.Equal(0.0<parameter>,hit.S)
    Assert.Equal(1.0<parameter>,hit.T)
    Assert.Equal(p 0. 0.,hit.Point)
[<Fact>]
let ``cubic self intersections find start interior and reverse`` () =
    let a,b,c,d = p 0. 0.,p 1. 0.,p 0. 1.,p -3. -3.
    let hit = single (CubicBezierData(a,b,c,d))
    Assert.Equal(0.0<parameter>,hit.S)
    near hit.T 0.5<parameter>
    let hit = single (CubicBezierData(d,c,b,a))
    near hit.S 0.5<parameter>
    Assert.Equal(1.0<parameter>,hit.T)
[<Fact>]
let ``cubic self intersections preserve rounded endpoint interior parameters`` () =
    let a,b,c,d = p 0. 0.,p 0.1 1.,p -1. 0.2,p 2.7 -3.5999999999999996
    let hit = single (CubicBezierData(a,b,c,d))
    Assert.Equal(0.0<parameter>,hit.S)
    near hit.T 0.5<parameter>
    let hit = single (CubicBezierData(d,c,b,a))
    near hit.S 0.5<parameter>
    Assert.Equal(1.0<parameter>,hit.T)
[<Fact>]
let ``cubic self intersections reject one coordinate endpoint return`` () =
    Assert.Equal(Ok [],Bezier.cubicSelfIntersections (CubicBezierData(p 0. 0.,p 1. 1.,p 0. 2.,p -3. 3.)))
[<Fact>]
let ``segment self intersections preserve closed cubic endpoints`` () =
    let hits = Intersections.segmentSelf (CubicBezier(p 0. 0.,p 0.1 1.,p -1. 0.2,p 0. 0.)) |> Result.defaultWith (failwithf "%A")
    let hit = Assert.Single hits
    Assert.Equal(0.0<parameter>,hit.LeftT)
    Assert.Equal(1.0<parameter>,hit.RightT)
