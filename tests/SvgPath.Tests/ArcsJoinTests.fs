module SvgPath.Tests.ArcsJoinTests
open SvgPath
open Xunit
// One-to-one with Gleam 71b05d1, svg_path_arcs_join_test.gleam.
let private p x y = Point.create (x*1.0<length>) (y*1.0<length>)
let private v x y = Point.create x y
let private ok r = Result.defaultWith (failwithf "%A") r
let private axis = Point.normalize (v 1.0 -1.0) |> Option.get
let private c start tangent radius : ArcsJoin.Continuation = ({Start=start;Tangent=tangent;Radius=Option.map (fun r -> r*1.0<length>) radius}: ArcsJoin.Continuation)
let private pair ra rb limit = ArcsJoin.join (c (p 0.0 -1.0) (v 1.0 0.0) ra) (c (p 1.0 0.0) (v 0.0 1.0) rb) axis (limit*1.0<length>) |> Option.get
let private near a b = Assert.True(float(Point.distance a b)<1e-8)
let private arc = function Arc a -> a | s -> failwithf "Expected Arc: %A" s
let private continuous segments =
    let path=Subpath.createWith Strict segments |> ok
    near (Subpath.start path) (p 0.0 -1.0)
    near (Subpath.finish path) (p 1.0 0.0)
let private corner () = Subpath.assertPolyline [p -10.0 0.0;p 0.0 0.0;p 0.0 10.0]
[<Fact>]
let ``arcs_intersecting_circles_preserve_radii_and_tangents_test`` () =
    let parts=pair (Some -4.0) (Some -4.0) 10.0
    Assert.Equal(2,parts.Length)
    let a,b=arc parts[0],arc parts[1]
    Assert.True(a.Sweep && b.Sweep && a.Radius=p 4.0 4.0 && b.Radius=a.Radius)
    continuous parts
    near (Segment.derivative parts[0] 0.0<parameter> |> ok |> Point.normalize |> Option.get) (v 1.0 0.0)
    near (Segment.derivative parts[1] 1.0<parameter> |> ok |> Point.normalize |> Option.get) (v 0.0 1.0)
[<Fact>]
let ``arcs_disjoint_circles_grow_equally_until_touching_test`` () =
    let parts=pair (Some 0.5) (Some 0.5) 10.0
    Assert.Equal(2,parts.Length)
    let a,b=arc parts[0],arc parts[1]
    Assert.True(not a.Sweep && not b.Sweep && abs(float a.Radius.X-(1.0+sqrt 2.0))<1e-8 && a.Radius=b.Radius)
    let x=(2.0+sqrt 2.0)/2.0
    near a.End (p x -x)
    continuous parts
[<Fact>]
let ``arcs_nested_circles_shrink_large_and_grow_small_equally_test`` () =
    let parts=ArcsJoin.join (c (p 0.0 -1.0) (v 1.0 0.0) (Some -10.0)) (c (p 1.2 0.0) (v 0.0 1.0) (Some -0.5)) axis 10.0<length> |> Option.get
    Assert.Equal(2,parts.Length)
    let a,b=arc parts[0],arc parts[1]
    Assert.True(a.Radius.X<10.0<length> && b.Radius.X>0.5<length>)
    Assert.True(abs((10.0<length> - a.Radius.X)-(b.Radius.X-0.5<length>))<1e-8<length>)
    Subpath.createWith Strict parts |> ok |> ignore
[<Fact>]
let ``arcs_line_circle_adjustment_test`` () =
    let parts=pair (Some 0.5) None 10.0
    Assert.Equal(2,parts.Length)
    Assert.True((arc parts[0]).Radius=p 1.0 1.0)
    match parts[1] with Line _ -> () | _ -> failwith "Expected line"
    near (Segment.finish parts[0]) (p 1.0 -2.0)
    continuous parts
[<Fact>]
let ``arcs_clip_plane_and_low_limit_test`` () =
    let parts=pair (Some 10.0) (Some 10.0) 1.2
    Assert.Equal(3,parts.Length)
    match parts[1] with
    | Line(a,b) -> for q in [a;b] do Assert.True(abs(Point.dot q axis-1.2<length>)<1e-8<length>)
    | _ -> failwith "Expected line"
    continuous parts
    Assert.True(pair (Some 10.0) (Some 10.0) 0.1=[Line(p 0.0 -1.0,p 1.0 0.0)])
[<Fact>]
let ``arcs_asymmetric_clipping_uses_auxiliary_arc_length_test`` () =
    let parts=pair (Some 10.0) (Some 5.0) 10.0
    Assert.Equal(2,parts.Length)
    let tip=Segment.finish parts[0]
    let r=Point.dot tip tip/(2.0*Point.dot tip (Point.rotateCounterclockwise axis))
    let helper=Arc ({Start=p 0.0 0.0;Radius=Point.create (abs r) (abs r);XAxisRotation=0.0<degree>;LargeArc=false;Sweep=r<0.0<length>;End=tip}: Ellipse.EndpointArcData)
    let length=Segment.length helper |> ok
    let t=Parameter.fromFloat (1.2<length>/length)
    let cut=Segment.point helper t |> ok
    let direction=Segment.derivative helper t |> ok |> Point.normalize |> Option.get
    let parts=pair (Some 10.0) (Some 5.0) 1.2
    Assert.Equal(3,parts.Length)
    match parts[1] with
    | Line(a,b) -> for q in [a;b] do Assert.True(abs(Point.dot (Point.subtract q cut) direction)<1e-8<length>)
    | _ -> failwith "Expected line"
[<Fact>]
let ``arcs_reflection_changes_sweep_not_shape_test`` () =
    let original=pair (Some 10.0) (Some 5.0) 1.2
    let mirrored=ArcsJoin.join (c (p 0.0 1.0) (v 1.0 0.0) (Some -10.0)) (c (p 1.0 0.0) (v 0.0 -1.0) (Some -5.0)) (v axis.X -axis.Y) 1.2<length> |> Option.get
    Assert.Equal(original.Length,mirrored.Length)
    for a,b in List.zip original mirrored do
        for t in [0.0<parameter>;0.5<parameter>;1.0<parameter>] do
            let a,b=Segment.point a t |> ok,Segment.point b t |> ok
            near {a with Y= -a.Y} b
[<Fact>]
let ``arcs_circle_line_and_line_circle_are_reversal_symmetric_test`` () =
    let forward=pair (Some 0.5) None 10.0
    let backward=ArcsJoin.join (c (p 1.0 0.0) (v 0.0 -1.0) None) (c (p 0.0 -1.0) (v -1.0 0.0) (Some -0.5)) axis 10.0<length> |> Option.get
    Assert.True((backward = (forward |> List.rev |> List.map Segment.reverse)))
[<Fact>]
let ``arcs_public_straight_join_matches_miter_clip_test`` () =
    for limit in [0.5;1.2;4.0] do Assert.True(Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.Arcs limit)=Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.MiterClip limit))
[<Fact>]
let ``arcs_inner_corner_defaults_to_bevel_test`` () =
    Assert.True(Offset.subpathUntrimmed (corner()) -1.0<length> (Offset.Arcs 4.0)=Offset.subpathUntrimmed (corner()) -1.0<length> Offset.Bevel)
[<Fact>]
let ``arcs_invalid_limits_test`` () =
    for limit in [0.0;-1.0] do
        Assert.True(Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.Arcs limit)=Error(Offset.InvalidMiterLimit limit))
        Assert.True(Stroke.subpath (corner()) 2.0<length> (Offset.Arcs limit) Offset.Butt |> Result.isError)
[<Fact>]
let ``arcs_public_curved_source_and_stroke_test`` () =
    let a start finish=Arc ({Start=start;Radius=p 3.0 3.0;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=finish}: Ellipse.EndpointArcData)
    let source=Subpath.create [a (p -3.0 3.0) (p 0.0 0.0);a (p 0.0 0.0) (p -3.0 3.0)] |> ok
    let outline=Offset.subpathUntrimmed source 1.0<length> (Offset.Arcs 4.0) |> ok
    Assert.Equal(4,outline.Segments.Length)
    for s in outline.Segments do near (arc s).Radius (p 4.0 4.0)
    let band=Stroke.subpath source 2.0<length> (Offset.Arcs 4.0) Offset.Butt |> ok
    Assert.False(band.Subpaths.IsEmpty)
    Assert.True(band.Subpaths |> List.forall (fun s -> s.Closed))
[<Fact>]
let ``arcs_public_bezier_source_preserves_translated_endpoints_test`` () =
    let p x y=p (127.345+x) (-23.567+y)
    let source=Subpath.create [CubicBezier(p -6.0 5.0,p -5.0 3.0,p -1.0 0.0,p 0.0 0.0);CubicBezier(p 0.0 0.0,p 0.0 1.0,p -3.0 5.0,p -5.0 5.0)] |> ok
    let arcs=Offset.subpathUntrimmed source 0.5<length> (Offset.Arcs 4.0) |> ok
    let bevel=Offset.subpathUntrimmed source 0.5<length> Offset.Bevel |> ok
    Assert.True(Subpath.start arcs=Subpath.start bevel && Subpath.finish arcs=Subpath.finish bevel)
    Assert.True(arcs.Segments.Length>bevel.Segments.Length)
