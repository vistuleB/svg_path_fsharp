module SvgPath.Tests.FlattenClosestPairTests

open SvgPath
open Xunit

// FL001: mirrored from Gleam d6c7b63, reduced from Flatten's
// Segment.distanceTo(Circle) fixtures. Endpoint-only bounds caused excessive work.
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private ok result = result |> Result.defaultWith (failwithf "%A")
let private quarterCircle () =
    Arc ({ Start = p 3.0 0.0; Radius = p 3.0 3.0; XAxisRotation = 0.0<degree>
           LargeArc = false; Sweep = true; End = p 0.0 3.0 }: Ellipse.EndpointArcData)

[<Fact>]
let ``crossing line arc closest pair preserves witnesses in both orders`` () =
    let line = Line(p 0.0 2.0, p 4.0 2.0)
    let arc = quarterCircle ()
    for left, right in [line, arc; arc, line] do
        let hit = Distance.segmentSegmentClosestPair left right |> ok
        let leftPoint = Segment.point left hit.LeftAt |> ok
        let rightPoint = Segment.point right hit.RightAt |> ok
        Assert.Equal(0.0<length>, Point.distance leftPoint hit.LeftPoint)
        Assert.Equal(0.0<length>, Point.distance rightPoint hit.RightPoint)
        Assert.True(hit.Distance < 1e-8<length>)
        Assert.True(abs(float hit.LeftPoint.X ** 2.0 - 5.0) < 1e-8)
        Assert.True(abs(float hit.LeftPoint.Y - 2.0) < 1e-8)

[<Fact>]
let ``crossing line arc path closest pair`` () =
    let line = Line(p 0.0 2.0, p 4.0 2.0)
    let hit = Distance.pathPathClosestPair (Segment.asPath line) (Segment.asPath (quarterCircle ())) |> ok
    Assert.True(hit.Distance < 1e-8<length>)
    Assert.Equal(hit.Distance, Point.distance hit.LeftPoint hit.RightPoint)
    Assert.Equal(0, hit.LeftAt.SubpathIndex)
    Assert.Equal(0, hit.RightAt.SubpathIndex)

[<Fact>]
let ``near tangent distance keeps actual positive gap`` () =
    let y = 3.0 + 1e-10
    let line = Line(p -1.0 y, p 1.0 y)
    let hit = Distance.segmentSegmentClosestPair line (quarterCircle ()) |> ok
    Assert.True(hit.Distance > 0.0<length>)
    Assert.True(abs(float hit.Distance - (y - 3.0)) < 1e-13)
    Assert.Equal(hit.Distance, Point.distance hit.LeftPoint hit.RightPoint)

[<Fact>]
let ``disjoint line arc distance preserves fallback`` () =
    let line = Line(p -1.0 4.0, p 1.0 4.0)
    let hit = Distance.segmentSegmentClosestPair line (quarterCircle ()) |> ok
    Assert.True(abs(float hit.Distance - 1.0) < 1e-8)

// FL005: bounded line/arc candidates and directional arc/arc lower bounds.
let private ellipse start radius finish =
    Arc {Start=start; Radius=radius; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=finish}
let private verifyPair left right expected =
    let hit=Distance.segmentSegmentClosestPair left right |> ok
    Assert.True(abs(float hit.Distance-expected)<1e-9, sprintf "distance %.17g expected %.17g" (float hit.Distance) expected)
    Assert.Equal(Segment.point left hit.LeftAt |> ok,hit.LeftPoint)
    Assert.Equal(Segment.point right hit.RightAt |> ok,hit.RightPoint)
    hit

[<Fact>]
let ``disjoint oblique line arc interior minimum`` () =
    let line=Line(p 170. 200.,p 120. 350.)
    let arc=ellipse (p 200. 300.) (p 50. 50.) (p 250. 250.)
    for a,b in [line,arc;arc,line] do
        let hit=verifyPair a b (sqrt(102.*102.+34.*34.)-50.)
        Assert.True(hit.LeftAt>0.0<parameter> && hit.LeftAt<1.0<parameter>)
        Assert.True(hit.RightAt>0.0<parameter> && hit.RightAt<1.0<parameter>)

[<Fact>]
let ``line arc minimum clamps to finite line`` () =
    let hit=verifyPair (Line(p -5. 4.,p -1. 4.)) (quarterCircle ()) (sqrt 2.)
    Assert.Equal(1.0<parameter>,hit.LeftAt)
    Assert.Equal(1.0<parameter>,hit.RightAt)

[<Fact>]
let ``zero length line arc minimum`` () =
    verifyPair (Line(p 0. 4.,p 0. 4.)) (quarterCircle ()) 1. |> ignore

[<Fact>]
let ``oblique line ellipse minimum`` () =
    verifyPair (Line(p 0. 10.,p 10. 0.)) (ellipse (p 4. 0.) (p 4. 2.) (p 0. 2.))
        ((10.-sqrt 20.)/sqrt 2.) |> ignore

[<Fact>]
let ``disjoint oblique arc pair minimum`` () =
    let left=ellipse (p 205. 150.) (p 30. 30.) (p 175. 180.)
    let right=ellipse (p 200. 300.) (p 50. 50.) (p 250. 250.)
    for a,b in [left,right;right,left;Segment.reverse left,Segment.reverse right] do
        let hit=verifyPair a b (sqrt(75.*75.+150.*150.)-80.)
        Assert.True(hit.LeftAt>0.0<parameter> && hit.LeftAt<1.0<parameter>)
        Assert.True(hit.RightAt>0.0<parameter> && hit.RightAt<1.0<parameter>)

[<Fact>]
let ``disjoint elliptical arc pair minimum`` () =
    let cx,cy=32./sqrt 20.+10./sqrt 2.,8./sqrt 20.+10./sqrt 2.
    verifyPair (ellipse (p 4. 0.) (p 4. 2.) (p 0. 2.))
        (ellipse (p (cx-4.) cy) (p 4. 2.) (p cx (cy-2.))) 10. |> ignore

[<Fact>]
let ``arc pair bound preserves positive near tangent gap`` () =
    let cx=10.+1e-8
    let hit=verifyPair (ellipse (p 0. -5.) (p 5. 5.) (p 0. 5.))
                (ellipse (p cx 5.) (p 5. 5.) (p cx -5.)) (cx-10.)
    Assert.True(hit.Distance>0.0<length>)
    Assert.True(abs(float hit.Distance-(cx-10.))<1e-12)
