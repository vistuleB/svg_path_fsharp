module SvgPath.Tests.DegeneracyProtrusionsTests

open SvgPath
open Xunit

// One Fact per Gleam test in svg_path_degeneracy_protrusions_test.gleam (2d71317).
let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private normalize source tolerance =
    let path = Parse.path source |> Result.defaultWith (failwithf "%A")
    let subpath = List.exactlyOne path.Subpaths
    let normalized = Degeneracy.normalizeDegenerateSegments subpath (Length.fromFloat tolerance) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(subpath.Start, normalized.Start)
    Assert.Equal(Subpath.finish subpath, Subpath.finish normalized)
    Assert.Equal(subpath.Closed, normalized.Closed)
    normalized.Start :: List.map Segment.finish normalized.Segments
let private check source tolerance expected =
    Assert.Equal<Point<length> list>(expected |> List.map (fun (x,y) -> point x y), normalize source tolerance)

[<Fact>]
let ``transverse line step does not hide longitudinal extent`` () =
    match normalize "M0 0 L10 0 L10 0.0001 L1 0" 0.001 with
    | [start;extremum;finish] ->
        Assert.Equal(point 0. 0.,start)
        Assert.Equal(10.0<length>,extremum.X)
        Assert.Equal(point 1. 0.,finish)
    | other -> failwithf "%A" other
[<Fact>]
let ``line run preserves global extrema not every local reversal`` () =
    check "M0 0 L4 0 L2 0 L10 0 L-3 0 L-1 0 L-10 0 L3 0" 0.0 [0.,0.;10.,0.;-10.,0.;3.,0.]
[<Fact>]
let ``line and quadratic encoding use same protrusion policy`` () =
    Assert.Equal<Point<length> list>(normalize "M0 0 L4 0 L2 0 L10 0 L-10 0 L3 0" 0.0,normalize "M0 0 Q2 0 4 0 L2 0 L10 0 L-10 0 L3 0" 0.0)

[<Fact>]
let ``zero line does not introduce a middle stop`` () =
    check "M0 0 L0 0 L1 0 L2 0" 0.0 [0.,0.; 2.,0.]
[<Fact>]
let ``constant beziers do not introduce a middle stop`` () =
    for source in ["M0 0 Q0 0 0 0 L1 0 L2 0"; "M0 0 C0 0 0 0 0 0 L1 0 L2 0"] do
        check source 0.0 [0.,0.; 2.,0.]
[<Fact>]
let ``zero line does not erase longitudinal extents`` () =
    check "M0 0 L0 0 L-10 0 L1 0.0000000001 L2 0.0000000001 L10 0 L3 0" 0.000001 [0.,0.; -10.,0.; 10.,0.; 3.,0.]
[<Fact>]
let ``line encoded as quadratic preserves longitudinal extents`` () =
    check "M0 0 Q-5 0 -10 0 L1 0.0000000001 L2 0.0000000001 L10 0 L3 0" 0.000001 [0.,0.; -10.,0.; 10.,0.; 3.,0.]
[<Fact>]
let ``longitudinal extents follow traversal not coordinate order`` () =
    check "M3 0 Q6.5 0 10 0 L2 0.0000000001 L1 0.0000000001 L-10 0 L0 0" 0.000001 [3.,0.; 10.,0.; -10.,0.; 0.,0.]
[<Fact>]
let ``longitudinal support finds bezier interior extremum`` () =
    check "M0 0 Q-20 0 0 0 L3 0" 0.000001 [0.,0.; -10.,0.; 3.,0.]
[<Fact>]
let ``endpoint anchors take priority over nearby extrema`` () =
    check "M0 0 Q-0.00025 0 -0.0005 0 L10.0005 0 L10 0" 0.001 [0.,0.; 10.,0.]
[<Fact>]
let ``hull union keeps short connectors until reconstruction`` () =
    check "M0 0 Q-0.00000025 0 -0.0000005 0 L10.0000005 0 L10 0" 0.000001 [0.,0.; 10.,0.]
[<Fact>]
let ``coincident endpoint anchors preserve closed traversal`` () =
    check "M0 0 Q-5 0 -10 0 L10 0 L0 0 Z" 0.0 [0.,0.; -10.,0.; 10.,0.; 0.,0.]
[<Fact>]
let ``two extrema inside one cubic follow parameter order`` () =
    match normalize "M0 0 C-9 0 9 0 0 0 L0 0" 0.0 with
    | [start; minimum; maximum; finish] ->
        Assert.Equal(point 0. 0., start)
        Assert.True(minimum.X < -2.5<length>)
        Assert.True(maximum.X > 2.5<length>)
        Assert.Equal(start, finish)
    | other -> failwithf "Expected four vertices, got %A" other
[<Fact>]
let ``nearby but distinct endpoint anchors are not merged`` () =
    check "M0 0 Q0 0 0 0 L0.0000001 0" 0.000001 [0.,0.; 0.0000001,0.]
[<Fact>]
let ``vertical strip uses longitudinal not transverse support`` () =
    check "M0 0 Q0 -5 0 -10 L0.0000000001 1 L0.0000000001 2 L0 10 L0 3" 0.000001 [0.,0.; 0.,-10.; 0.,10.; 0.,3.]
