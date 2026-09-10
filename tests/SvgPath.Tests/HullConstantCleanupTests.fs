module SvgPath.Tests.HullConstantCleanupTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private a,b,c,d,nearA = p 0. 0.,p 1. 0.,p 1. 1.,p 0. 1.,p 0. 5e-10
let private loop final = [Line(a,b);Line(b,c);Line(c,d);Line(d,nearA);final]
let private tangent final = ConvexHull.internalPointExactLoopTangentSubpaths (loop final) (p -1. 0.5) |> Result.defaultWith (failwithf "%A")
[<Fact>]
let ``exact tangent chain preserves short final line`` () =
    let final = Line(nearA,a)
    let outside,inside = tangent final
    Assert.Equal(d,outside.Start)
    Assert.Equal(a,Subpath.finish outside)
    Assert.Equal(a,inside.Start)
    Assert.Equal(d,Subpath.finish inside)
    Assert.Equal<Segment list>([Line(d,nearA);final],outside.Segments)
[<Fact>]
let ``exact tangent chain preserves short final quadratic`` () =
    let final = QuadraticBezier(nearA,p 0. 2.5e-10,a)
    let outside,inside = tangent final
    Assert.Equal<Segment list>([Line(d,nearA);final],outside.Segments)
    Assert.Equal(Subpath.finish outside,inside.Start)
[<Fact>]
let ``loop union removes exactly constant beziers`` () =
    let expected = [Line(a,b);Line(b,c);Line(c,d);Line(d,a)]
    let loop = [Line(a,b);Line(b,c);Line(c,d);QuadraticBezier(d,d,d);CubicBezier(d,d,d,d);Line(d,a)]
    let inside = p 0.5 0.5
    Assert.Equal<Segment list>(expected,ConvexHull.internalLoopUnionSegmentsWithSeedAngles loop [Line(inside,inside)] [])
