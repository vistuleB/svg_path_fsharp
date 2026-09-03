module SvgPath.Tests.OffsetTests

open SvgPath
open Xunit

let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)

[<Fact>]
let ``subpath_offset_map_uses_cumulative_segment_lengths_test`` () =
    let subpath =
        Subpath.create [ Line(point 0.0 0.0, point 3.0 0.0); Line(point 3.0 0.0, point 3.0 4.0) ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let mapping = Offset.subpathOffsetMap subpath |> Result.defaultWith (fun error -> failwithf "%A" error)
    let first = mapping (point 2.0 1.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    let second = mapping (point 5.0 1.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(Point.distance first (point 2.0 -1.0) < 1.0e-12<length>)
    Assert.True(Point.distance second (point 4.0 2.0) < 1.0e-12<length>)

[<Fact>]
let ``subpath_offset_map_wraps_closed_subpath_distances_test`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let mapping = Offset.subpathOffsetMap subpath |> Result.defaultWith (fun error -> failwithf "%A" error)
    let wrapped = mapping (point 9.0 0.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(Point.distance wrapped (point 1.0 0.0) < 1.0e-12<length>)

[<Fact>]
let ``segment_offsets_line_to_visual_left_for_positive_distance_test`` () =
    let result =
        Offset.segment (Line(point 0.0 0.0, point 10.0 0.0)) 2.0<length>
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal<Segment list>([ Line(point 0.0 -2.0, point 10.0 -2.0) ], result.Segments)

[<Fact>]
let ``segment_offsets_quadratic_to_cubic_pieces_within_tolerance_test`` () =
    let source = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    let result = Offset.segment source 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.NotEmpty(result.Segments)
    Assert.True(result.Segments |> List.forall (function CubicBezier _ -> true | _ -> false))

[<Fact>]
let ``segment_rejects_negative_tangent_heal_angle_test`` () =
    let options = { Offset.defaultOptions with TangentHealAngleDegrees = -1.0<degree> }
    match Offset.segmentWith (Line(point 0.0 0.0, point 1.0 0.0)) 1.0<length> options with
    | Error(InvalidTangentHealAngleDegrees angle) -> Assert.Equal(-1.0<degree>, angle)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``subpath_untrimmed_offsets_open_polyline_with_bevel_join_test`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 2.0 0.0); Line(point 2.0 0.0, point 2.0 2.0) ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let options = { Offset.defaultOptions with Join = Bevel }
    let result = Offset.subpathUntrimmedWith source 0.5<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(3, List.length result.Segments)
    Assert.False(result.Closed)
    Assert.Equal(Segment.finish result.Segments[0], Segment.start result.Segments[1])
    Assert.Equal(Segment.finish result.Segments[1], Segment.start result.Segments[2])

[<Fact>]
let ``subpath_untrimmed_offsets_open_polyline_with_miter_join_by_default_test`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let render join =
        Offset.subpathUntrimmedWith source 2.0<length> { Offset.defaultOptions with Join = join }
        |> Result.defaultWith (failwithf "%A")
        |> Serialize.subpath
    Assert.Equal("M 0 -2 H 10 L 12 0 V 10", render Bevel)
    Assert.Equal("M 0 -2 H 10 H 12 V 0 V 10", render (Miter 4.0))
    Assert.Equal("M 0 -2 H 10 A 2 2 0 0 1 12 0 V 10", render Round)

[<Fact>]
let ``subpath_untrimmed_offsets_open_polyline_with_round_join_test`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.subpathUntrimmedWith source 2.0<length> { Offset.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    Assert.Contains(result.Segments, function Arc _ -> true | _ -> false)
    Assert.Equal("M 0 -2 H 10 A 2 2 0 0 1 12 0 V 10", Serialize.subpath result)

[<Fact>]
let ``subpath_untrimmed_offsets_closed_square_and_preserves_closed_state_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 4.0; point 0.0 4.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let options = { Offset.defaultOptions with Join = Round }
    let result = Offset.subpathUntrimmedWith source 0.5<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(result.Closed)
    Assert.Equal(result.Start, result.Segments |> List.last |> Segment.finish)
    Assert.Contains(result.Segments, function Arc _ -> true | _ -> false)

[<Fact>]
let ``segment_offsets_circular_arc_exactly_test`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 0.0 10.0 }
    let result = Offset.segment source 2.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    match result.Segments with
    | [ Arc offsetArc ] ->
        Assert.Equal(12.0<length>, offsetArc.Radius.X)
        Assert.Equal(12.0<length>, offsetArc.Radius.Y)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment_offsets_circular_arc_across_center_test`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = false
              End = point 0.0 -10.0 }
    let result = Offset.segment source 12.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    match result.Segments with
    | [ Arc offsetArc ] ->
        Assert.Equal(point -2.0 0.0, offsetArc.Start)
        Assert.Equal(2.0<length>, offsetArc.Radius.X)
        Assert.Equal(point 0.0 2.0, offsetArc.End)
        Assert.False(offsetArc.Sweep)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment_rejects_collapsed_circular_arc_offset_test`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 0.0 10.0 }
    match Offset.segment source -10.0<length> with
    | Error(SvgPath.Error.DegenerateTangent parameterValue) -> Assert.Equal(0.0<parameter>, parameterValue)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment_rejects_zero_length_line_test`` () =
    match Offset.segment (Line(point 1.0 2.0, point 1.0 2.0)) 1.0<length> with
    | Error(SvgPath.Error.DegenerateTangent parameterValue) -> Assert.Equal(0.0<parameter>, parameterValue)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``path_untrimmed_offsets_every_subpath_test`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let second = Subpath.ofSegment (Line(point 0.0 2.0, point 1.0 2.0))
    let result =
        Offset.pathUntrimmed (Path.ofSubpaths [ first; second ]) 0.5<length>
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``trimmed path normalization drops empty source subpaths`` () =
    let source =
        Path.ofSubpaths
            [ Subpath.empty (point 0.0 0.0)
              Subpath.ofSegment (Line(point 0.0 2.0, point 3.0 2.0)) ]
    let untrimmed = Offset.pathUntrimmed source 0.5<length> |> Result.defaultWith (failwithf "%A")
    let trimmed = Offset.path source 0.5<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, untrimmed.Subpaths.Length)
    Assert.Empty(untrimmed.Subpaths[0].Segments)
    Assert.Single(trimmed.Subpaths) |> ignore

[<Fact>]
let ``synchronized_offsets_share_nonstalled_refinement_leaves_test`` () =
    let source =
        Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 0.0, point 1.0 1.0))
    let correspondences =
        Offset.internalSynchronizedOffsetTrace source 0.0<length> -0.27<length> Offset.defaultOptions
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let paired = correspondences |> List.filter (fun item -> not item.InnerStalled && not item.OuterStalled)
    let spans leaves =
        leaves |> List.map (fun leaf -> leaf.SourceSegmentIndex, leaf.PreparedFrom, leaf.PreparedTo, leaf.Generation)
    Assert.NotEmpty(paired)
    Assert.All(paired, fun item -> Assert.True(spans item.InnerLeaves = spans item.OuterLeaves))

[<Fact>]
let ``synchronized_offsets_accept_reversed_distance_order_test`` () =
    let source =
        Subpath.ofSegment (Line(point 0.0 0.0, point 2.0 1.0))
    let forward =
        Offset.subpathBandUntrimmed source -0.5<length> 1.0<length>
        |> Result.defaultWith (failwithf "%A")
    let reversed =
        Offset.subpathBandUntrimmed source 1.0<length> -0.5<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(forward.Subpaths[0], reversed.Subpaths[1])
    Assert.Equal(forward.Subpaths[1], reversed.Subpaths[0])

[<Fact>]
let ``subpath_band_closed_square_returns_two_closed_sides_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let result = Offset.subpathBand source -1.0<length> 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``subpath_band_open_line_returns_two_capless_sides_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let result = Offset.subpathBand source -1.0<length> 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.False(subpath.Closed))
    let segments = result.Subpaths |> List.collect Subpath.segments
    Assert.Contains(Line(point 0.0 1.0, point 10.0 1.0), segments)
    Assert.Contains(Line(point 0.0 -2.0, point 10.0 -2.0), segments)

[<Fact>]
let ``subpath_offsets_closed_square_inset_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpath source -2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths[0].Closed)
    Assert.Equal<Segment list>(
        [ Line(point 2.0 2.0, point 8.0 2.0)
          Line(point 8.0 2.0, point 8.0 8.0)
          Line(point 8.0 8.0, point 2.0 8.0)
          Line(point 2.0 8.0, point 2.0 2.0) ],
        result.Subpaths[0].Segments)

[<Fact>]
let ``exchanging_band_offsets_reverses_result_orientation_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let forward = Offset.subpathBand source -1.0<length> 1.0<length> |> Result.defaultWith (failwithf "%A")
    let reversed = Offset.subpathBand source 1.0<length> -1.0<length> |> Result.defaultWith (failwithf "%A")
    let forwardArea = Area.signedPath forward
    let reversedArea = Area.signedPath reversed
    Assert.True(
        abs (forwardArea + reversedArea) < 1.0e-9<length^2>,
        sprintf "forward=%A reversed=%A\nforward path=%s\nreversed path=%s"
            forwardArea reversedArea (Serialize.path forward) (Serialize.path reversed))

[<Fact>]
let ``subpath_can_use_round_join_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let options = { Offset.defaultOptions with Join = Round }
    let result = Offset.subpathStrokeWith source 2.0<length> RoundCap options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths[0].Closed)
    Assert.Equal(2, result.Subpaths[0].Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length)

[<Fact>]
let ``subpath_can_use_bevel_join_test`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let options = { Offset.defaultOptions with Join = Bevel }
    let result = Offset.subpathWith source 2.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.NotEmpty(result.Subpaths)

[<Fact>]
let ``path_offsets_every_subpath_test`` () =
    let source =
        Path.ofSubpaths
            [ Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
              Subpath.ofSegment (Line(point 0.0 5.0, point 10.0 5.0)) ]
    let result = Offset.pathStroke source 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``path_offsets_straight_subpaths_test`` () =
    let first =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 -10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let second =
        Subpath.create [ Line(point 0.0 20.0, point 10.0 20.0); Line(point 10.0 20.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.path (Path.ofSubpaths [ first; second ]) 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 -2 H 8 V -10 M 0 18 H 8 V 10", Serialize.path result)

[<Fact>]
let ``path offset orients nested closed contours by depth`` () =
    let square size inset =
        Subpath.polygon
            [ point inset inset
              point (inset + size) inset
              point (inset + size) (inset + size)
              point inset (inset + size) ]
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.path (Path.ofSubpaths [ square 20.0 0.0; square 6.0 7.0 ]) 0.5<length>
        |> Result.defaultWith (failwithf "%A")
    let areas = result.Subpaths |> List.map Area.signedSubpath
    Assert.Equal(2, areas.Length)
    Assert.Equal(1, areas |> List.filter (fun area -> area > 0.0<length^2>) |> List.length)
    Assert.Equal(1, areas |> List.filter (fun area -> area < 0.0<length^2>) |> List.length)

[<Fact>]
let ``path_offsets_closed_subpaths_test`` () =
    let closed =
        Subpath.polygon [ point 0.0 0.0; point 8.0 0.0; point 8.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let openSource = Subpath.ofSegment (Line(point 20.0 0.0, point 28.0 0.0))
    let result =
        Offset.path (Path.ofSubpaths [ closed; openSource ]) 0.5<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)
    Assert.Equal(1, result.Subpaths |> List.filter Subpath.isClosed |> List.length)
    Assert.Equal(1, result.Subpaths |> List.filter (Subpath.isClosed >> not) |> List.length)

[<Fact>]
let ``subpath_offsets_open_polyline_with_default_miter_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 3.0 0.0))
    let options =
        { Offset.defaultOptions with
            SingleOffsetTrimming = { Offside = false; FinalTrimming = NoTrimming } }
    let result = Offset.subpathWith source 1.0<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal<Segment list>([ Line(point 0.0 -1.0, point 3.0 -1.0) ], result.Subpaths[0].Segments)

[<Fact>]
let ``final_cusp_trimming_handles_open_side_umbrella_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 8.0 0.0; point 8.0 6.0; point 0.0 6.0 ]
        |> Result.defaultWith (failwithf "%A")
    let run finalTrimming =
        let options =
            { Offset.defaultOptions with
                SingleOffsetTrimming =
                    { Offside = false
                      FinalTrimming = finalTrimming } }
        Offset.subpathWith source 0.5<length> options
        |> Result.defaultWith (failwithf "%A")
    let untrimmed = run NoTrimming
    let cuspTrimmed = run CuspTrimming
    let inBandTrimmed = run InBandTrimming
    Assert.Single(untrimmed.Subpaths) |> ignore
    Assert.Single(cuspTrimmed.Subpaths) |> ignore
    Assert.Single(inBandTrimmed.Subpaths) |> ignore
    Assert.All(cuspTrimmed.Subpaths, fun subpath -> Assert.True(subpath.Closed))
    Assert.All(inBandTrimmed.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``band cusp switches execute side-local trimming before final trimming`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let run innerCusps outerCusps =
        let options =
            { Offset.defaultOptions with
                BandTrimming =
                    { InnerCusps = innerCusps
                      OuterCusps = outerCusps
                      InBand = false } }
        Offset.subpathBandWith source -1.0<length> 1.0<length> options
        |> Result.defaultWith (failwithf "%A")
    for innerCusps, outerCusps in [ false, false; true, false; false, true; true, true ] do
        let result = run innerCusps outerCusps
        Assert.Equal(2, result.Subpaths.Length)
        Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``subpath_stroke_closed_square_uses_band_test`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpathBand source -2.0<length> 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        "M 2 2 V 8 H 8 V 2 Z M 0 -2 H 10 H 12 V 0 V 10 V 12 H 10 H 0 H -2 V 10 V 0 V -2 Z",
        Serialize.path result)

[<Fact>]
let ``closed square stroke uses the same capless band`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpathStroke source 4.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        "M 2 2 V 8 H 8 V 2 Z M 0 -2 H 10 H 12 V 0 V 10 V 12 H 10 H 0 H -2 V 10 V 0 V -2 Z",
        Serialize.path result)

[<Fact>]
let ``figure_eight_band_joins_reversed_outer_chunks_test`` () =
    let source =
        Subpath.create
            [ CubicBezier(point 0.0 0.0, point -336.0 -234.0, point -336.0 234.0, point 0.0 0.0)
              CubicBezier(point 0.0 0.0, point 336.0 -234.0, point 336.0 234.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.subpathBandWith source 18.0<length> 34.0<length> { Offset.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``subpath_band_untrimmed_returns_two_raw_sides_test`` () =
    let source =
        Subpath.create
            [ CubicBezier(point 0.0 0.0, point -336.0 -234.0, point -336.0 234.0, point 0.0 0.0)
              CubicBezier(point 0.0 0.0, point 336.0 -234.0, point 336.0 234.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.subpathBandUntrimmedWith
            source 18.0<length> 34.0<length>
            { Offset.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``zero-length stroke follows cap semantics`` () =
    let source = Subpath.ofSegment (Line(point 2.0 3.0, point 2.0 3.0))
    let butt = Offset.subpathStrokeWith source 4.0<length> Butt Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    let round = Offset.subpathStrokeWith source 4.0<length> RoundCap Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    let square = Offset.subpathStrokeWith source 4.0<length> Square Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Empty(butt.Subpaths)
    Assert.Equal(2, round.Subpaths[0].Segments.Length)
    Assert.Equal(4, square.Subpaths[0].Segments.Length)

[<Fact>]
let ``subpath_stroke_open_line_with_square_cap_extends_ends_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let render cap =
        Offset.subpathStrokeWith source 2.0<length> cap Offset.defaultOptions
        |> Result.defaultWith (failwithf "%A")
        |> Serialize.path
    Assert.Equal("M 0 -1 H 10 V 1 H 0 Z", render Butt)
    Assert.Equal("M 0 -1 H 10 H 11 V 1 H 10 H 0 H -1 V -1 Z", render Square)

[<Fact>]
let ``subpath_stroke_open_line_with_butt_cap_returns_closed_outline_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let result = Offset.subpathStrokeWith source 2.0<length> Butt Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths[0].Closed)
    Assert.Equal("M 0 -1 H 10 V 1 H 0 Z", Serialize.path result)

[<Fact>]
let ``subpath_stroke_rejects_invalid_width_test`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Error(InvalidStrokeWidth 0.0<length>), Offset.subpathStrokeWith source 0.0<length> Butt Offset.defaultOptions)
    Assert.Equal(Error(InvalidStrokeWidth -1.0<length>), Offset.subpathStrokeWith source -1.0<length> Butt Offset.defaultOptions)

[<Fact>]
let ``path_band_untrimmed_returns_two_raw_sides_per_subpath_test`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (Line(point 0.0 5.0, point 10.0 5.0))
    let result = Offset.pathBandUntrimmed (Path.ofSubpaths [ first; second ]) -1.0<length> 1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, result.Subpaths.Length)
