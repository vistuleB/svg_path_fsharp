module SvgPath.Tests.MiterClipTests

open SvgPath
open Xunit

// One-to-one with Gleam 411c34a, svg_path_miter_clip_test.gleam.
let private p x y = Point.create (x*1.0<length>) (y*1.0<length>)
let private ok r = Result.defaultWith (failwithf "%A") r
let private corner () = Subpath.assertPolyline [p -10.0 0.0; p 0.0 0.0; p 0.0 10.0]
let private near a b = Point.distance a b < 1e-9<length>
let private hasEdge (sub: Subpath) a b = sub.Segments |> List.exists (fun s -> near (Segment.start s) a && near (Segment.finish s) b)

[<Fact>]
let ``miter_clip_high_limit_matches_miter_test`` () =
    Assert.True(Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.MiterClip 4.0) = Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.Miter 4.0))

[<Fact>]
let ``miter_clip_acute_corner_test`` () =
    let source = Subpath.assertPolyline [p -10.0 0.0; p 0.0 0.0; p -8.0 6.0]
    let path = Offset.subpathUntrimmed source 1.0<length> (Offset.MiterClip 2.0) |> ok
    Assert.Equal(5,path.Segments.Length)
    let clip = path.Segments[2]
    let axis = Point.normalize (p 3.0 -1.0) |> Option.get
    for q in [Segment.start clip; Segment.finish clip] do Assert.True(abs(Point.dot q axis - 2.0<length>) < 1e-9<length>)
    Assert.True(near (Subpath.start path) (p -10.0 -1.0))
    Assert.True(near (Subpath.finish path) (p -7.4 6.8))

[<Fact>]
let ``miter_clip_plane_is_measured_from_pivot_test`` () =
    let path = Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.MiterClip 1.2) |> ok
    let x = 1.2 * sqrt 2.0 - 1.0
    Assert.True(hasEdge path (p 0.0 -1.0) (p x -1.0))
    Assert.True(hasEdge path (p x -1.0) (p 1.0 -x))
    Assert.True(hasEdge path (p 1.0 -x) (p 1.0 0.0))
    Assert.False(hasEdge path (p 0.0 -1.0) (p 1.0 -1.0))

[<Fact>]
let ``miter_clip_low_limit_does_not_trim_neighbors_test`` () =
    for limit in [0.1;0.5] do
        Assert.True(Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.MiterClip limit) = Offset.subpathUntrimmed (corner()) 1.0<length> Offset.Bevel)

[<Fact>]
let ``miter_clip_diverging_rays_match_miter_fallback_test`` () =
    Assert.True(Offset.subpathUntrimmed (corner()) -1.0<length> (Offset.MiterClip 1.2) = Offset.subpathUntrimmed (corner()) -1.0<length> (Offset.Miter 1.2))

[<Fact>]
let ``miter_clip_mirrored_corner_negative_offset_test`` () =
    let source = Subpath.assertPolyline [p -10.0 0.0; p 0.0 0.0; p 0.0 -10.0]
    let path = Offset.subpathUntrimmed source -1.0<length> (Offset.MiterClip 1.2) |> ok
    let x = 1.2*sqrt 2.0-1.0
    Assert.True(hasEdge path (p x 1.0) (p 1.0 x))

[<Fact>]
let ``miter_clip_translated_scaled_corner_test`` () =
    let source = Subpath.assertPolyline [p -13.0 -3.0; p 7.0 -3.0; p 7.0 17.0]
    let path = Offset.subpathUntrimmed source 2.0<length> (Offset.MiterClip 1.2) |> ok
    let x = 2.0*(1.2*sqrt 2.0-1.0)
    Assert.True(hasEdge path (p (7.0+x) -5.0) (p 9.0 (-3.0-x)))

[<Fact>]
let ``miter_clip_zero_offset_and_straight_source_test`` () =
    Assert.True(Offset.subpathUntrimmed (corner()) 0.0<length> (Offset.MiterClip 1.2) = Offset.subpathUntrimmed (corner()) 0.0<length> (Offset.Miter 1.2))
    let source = Subpath.assertPolyline [p 0.0 0.0;p 5.0 0.0;p 10.0 0.0]
    Assert.True(Offset.subpathUntrimmed source 1.0<length> (Offset.MiterClip 1.2) = Offset.subpathUntrimmed source 1.0<length> (Offset.Miter 1.2))

[<Fact>]
let ``miter_clip_invalid_limit_test`` () =
    for limit in [0.0;-1.0] do
        Assert.True(Offset.subpathUntrimmed (corner()) 1.0<length> (Offset.MiterClip limit) = Error(Offset.InvalidMiterLimit limit))
        Assert.True(Stroke.subpath (corner()) 2.0<length> (Offset.MiterClip limit) Offset.Butt = Error(Stroke.StrokeOffsetError(Offset.InvalidMiterLimit limit)))

[<Fact>]
let ``miter_clip_stroke_produces_closed_outline_test`` () =
    let path = Stroke.subpath (corner()) 2.0<length> (Offset.MiterClip 1.2) Offset.Butt |> ok
    Assert.Equal(1,path.Subpaths.Length)
    let outline = path.Subpaths.Head
    Assert.True(outline.Closed)
    let x = 1.2*sqrt 2.0-1.0
    Assert.True(hasEdge outline (p x -1.0) (p 1.0 -x))
    let bevel = Stroke.subpath (corner()) 2.0<length> Offset.Bevel Offset.Butt |> ok
    Assert.NotEqual<string>(Serialize.path path,Serialize.path bevel)
