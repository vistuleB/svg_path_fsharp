module SvgPath.Tests.InnerJoinTests

open SvgPath
open Xunit

// One-to-one with svg_path_inner_join_test.gleam.
let private p x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
let private ok value = value |> Result.defaultWith (failwithf "%A")
let private corner y = Subpath.polyline [p -10.0 0.0; p 0.0 0.0; p 0.0 y] |> ok
let private arcs subpath = Subpath.segments subpath |> List.filter (function Arc _ -> true | _ -> false) |> List.length
let private styles = [Offset.Bevel; Offset.Miter 4.0; Offset.MiterClip 4.0; Offset.Arcs 4.0]

[<Fact>]
let ``inner_join_defaults_depend_on_join_style_test`` () =
    for join in styles do
        Assert.Equal(Offset.subpathUntrimmed (corner 10.0) -1.0<length> Offset.Bevel,
                     Offset.subpathUntrimmed (corner 10.0) -1.0<length> join)
    Assert.Equal(1, Offset.subpathUntrimmed (corner 10.0) -1.0<length> Offset.Round |> ok |> arcs)

[<Fact>]
let ``inner_round_override_applies_to_all_join_styles_and_offset_signs_test`` () =
    let options = {Offset.defaultOptions with InnerJoin = Some Offset.InnerRound}
    for y, distance in [10.0, -1.0<length>; -10.0, 1.0<length>] do
        for join in styles @ [Offset.Round] do
            Assert.Equal(Offset.subpathUntrimmed (corner y) distance Offset.Round,
                         Offset.subpathUntrimmedWith (corner y) distance join options)

[<Fact>]
let ``inner_bevel_override_does_not_change_outer_round_join_test`` () =
    let options = {Offset.defaultOptions with InnerJoin = Some Offset.InnerBevel}
    Assert.Equal(Offset.subpathUntrimmed (corner 10.0) -1.0<length> Offset.Bevel,
                 Offset.subpathUntrimmedWith (corner 10.0) -1.0<length> Offset.Round options)
    Assert.Equal(Offset.subpathUntrimmed (corner 10.0) 1.0<length> Offset.Round,
                 Offset.subpathUntrimmedWith (corner 10.0) 1.0<length> Offset.Round options)

[<Fact>]
let ``band_inner_join_is_local_not_the_named_inner_offset_test`` () =
    let options =
        {Offset.defaultOptions with
            InnerJoin = Some Offset.InnerRound
            BandTrimming = {InnerCusps=false; OuterCusps=false; InBand=false}}
    for inner, outer in [-1.0<length>, 1.0<length>; 1.0<length>, -1.0<length>] do
        let band = Offset.subpathBandWith (corner 10.0) inner outer Offset.Bevel Offset.Butt options |> ok
        Assert.Equal(1, band.Subpaths |> List.exactlyOne |> arcs)
        let beveled = Offset.subpathBandWith (corner 10.0) inner outer Offset.Bevel Offset.Butt
                        {options with InnerJoin = Some Offset.InnerBevel} |> ok
        Assert.Equal(0, beveled.Subpaths |> List.exactlyOne |> arcs)

[<Fact>]
let ``inner_join_override_includes_closed_seam_test`` () =
    let square = Subpath.polygon [p 0.0 0.0; p 10.0 0.0; p 10.0 10.0; p 0.0 10.0] |> ok
    let options = {Offset.defaultOptions with InnerJoin = Some Offset.InnerRound}
    let rounded = Offset.subpathUntrimmedWith square -1.0<length> (Offset.Miter 4.0) options |> ok
    Assert.Equal(4, arcs rounded)
    Assert.True(rounded.Closed)
