module SvgPath.Tests.BoundingPolygonTests

open SvgPath
open Xunit

let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private unwrap result = Result.defaultWith (failwithf "%A") result
let private cross (a: Point<length>) b q = (b.X-a.X)*(q.Y-a.Y)-(b.Y-a.Y)*(q.X-a.X)
let private check segment fromT toT =
    let points = Segment.boundingPolygonBetween segment fromT toT |> unwrap
    Assert.True(points.Length>=3)
    Assert.Equal(points.Length, List.distinct points |> List.length)
    for a,b in List.zip points (List.tail points @ [List.head points]) do
        for q in points do Assert.True(cross a b q >= -1e-9<length^2>)
        for i in 0..200 do
            let t = fromT + (toT-fromT)*float i/200.0
            let q = Segment.point segment t |> unwrap
            Assert.True(cross a b q >= -1e-9<length^2>)

[<Fact>]
let ``bounding_polygon_line_and_point_test`` () =
    let a,b = p 4.0 2.0,p -1.0 3.0
    Assert.True(Segment.boundingPolygon (Line(a,b)) = Ok [a;b])
    Assert.True(Segment.boundingPolygon (Line(a,a)) = Ok [a])
    Assert.True(Segment.boundingPolygonBetween (Line(a,b)) 1.0<parameter> 0.0<parameter> = Ok [b;a])

[<Fact>]
let ``bounding_polygon_bezier_order_and_interior_start_test`` () =
    let a = p 0.0 0.0
    let q = QuadraticBezier(a,p 2.0 -3.0,p 4.0 0.0)
    Assert.Equal(a, Segment.boundingPolygon q |> unwrap |> List.head)
    check q 0.0<parameter> 1.0<parameter>
    let c = CubicBezier(a,p -3.0 -2.0,p 3.0 -2.0,p 0.0 4.0)
    Assert.True(Segment.boundingPolygon c = Ok [p -3.0 -2.0;p 3.0 -2.0;p 0.0 4.0])
    check c 0.0<parameter> 1.0<parameter>
    check c 0.8<parameter> 0.2<parameter>

[<Fact>]
let ``bounding_polygon_collinear_controls_test`` () =
    let a = p 0.0 0.0
    let c = CubicBezier(a,p -3.0 0.0,p 5.0 0.0,a)
    Assert.True(Segment.boundingPolygon c = Ok [p -3.0 0.0;p 5.0 0.0])
    Assert.True(Segment.boundingPolygonBetween c 0.0<parameter> 0.0<parameter> = Ok [a])

[<Fact>]
let ``bounding_polygon_arcs_test`` () =
    for sweep in [true;false] do
        for large in [true;false] do
            let arc = Arc ({Start=p 4.0 0.0;Radius=p 5.0 2.0;XAxisRotation=37.0<degree>;LargeArc=large;Sweep=sweep;End=p -1.0 3.0}: Ellipse.EndpointArcData)
            check arc 0.0<parameter> 1.0<parameter>
            check arc 0.9<parameter> 0.15<parameter>
            check arc 0.5<parameter> 0.500001<parameter>
    let corrected = Arc ({Start=p -4.0 0.0;Radius=p 1.0 1.0;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=p 4.0 0.0}: Ellipse.EndpointArcData)
    check corrected 0.0<parameter> 1.0<parameter>

[<Fact>]
let ``bounding_polygon_invalid_interval_and_arc_test`` () =
    let a = p 0.0 0.0
    Assert.True(Segment.boundingPolygonBetween (Line(a,a)) -0.1<parameter> 1.0<parameter> = Error SplitOutsideSegment)
    let arc = Arc ({Start=a;Radius=p 1.0 1.0;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=a}: Ellipse.EndpointArcData)
    Assert.True(Segment.boundingPolygon arc = Error DegenerateArc)
