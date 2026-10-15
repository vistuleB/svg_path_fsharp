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
