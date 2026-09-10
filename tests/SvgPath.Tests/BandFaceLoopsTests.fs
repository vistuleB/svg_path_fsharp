module SvgPath.Tests.BandFaceLoopsTests

open SvgPath
open Xunit

let private p (x,y) = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private unwrap result = Result.defaultWith (failwithf "%A") result
let private polygon points = points |> List.map p |> Subpath.polygon |> unwrap
let private square x y size = polygon [x,y;x+size,y;x+size,y+size;x,y+size]
let private path = Path.ofSubpaths
let private count value = value |> Path.subpaths |> List.collect Subpath.segments |> List.length
let private checkFill input output =
    for x in [-1..7] do
        for y in [-1..7] do
            let point = p(float x + 0.37,float y + 0.19)
            match WindingField.pathWinding point input |> unwrap, WindingField.pathWinding point output |> unwrap with
            | Winding a, Winding b ->
                Assert.Equal(a % 2 <> 0,b <> 0)
                Assert.True(b = 0 || b = 1)
            | _ -> ()
let private checkChoices input loops =
    checkFill input loops
    let choices = Path.subpaths loops |> List.fold (fun prefixes loop ->
        prefixes |> List.collect (fun prefix -> [prefix @ [loop]; prefix @ [Subpath.reverse loop]])) [[]]
    for choice in choices do
        Offset.enumerateBandFaceLoops (path choice) |> unwrap |> checkFill input

[<Fact>]
let ``bowtie_splits_into_two_independently_orientable_face_loops_test`` () =
    let bowtie = polygon [0.0,0.0;4.0,4.0;0.0,4.0;4.0,0.0]
    for contour in [bowtie;Subpath.reverse bowtie] do
        let input = path [contour]
        let loops = Offset.enumerateBandFaceLoops input |> unwrap
        Assert.Equal(2,loops.Subpaths.Length)
        Assert.Equal(6,count loops)
        checkChoices input loops

[<Fact>]
let ``nested_same_direction_contours_become_even_odd_boundary_loops_test`` () =
    let input = path [square 2.0 2.0 2.0;square 0.0 0.0 6.0]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    Assert.Equal(2,loops.Subpaths.Length)
    checkChoices input loops

[<Fact>]
let ``touching_vertices_do_not_merge_filled_face_sectors_test`` () =
    let input = path [square 0.0 0.0 2.0;square 2.0 2.0 2.0]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    Assert.Equal(2,loops.Subpaths.Length)
    checkFill input loops
    Offset.enumerateBandFaceLoops loops |> unwrap |> checkFill input

[<Fact>]
let ``multiply_wound_single_contour_can_be_reenumerated_before_orientation_test`` () =
    let input = path [polygon [0.0,0.0;6.0,0.0;6.0,6.0;0.0,6.0;0.0,0.0;1.0,1.0;3.0,1.0;3.0,3.0;1.0,3.0;1.0,1.0]]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    checkFill input loops
    Offset.enumerateBandFaceLoops loops |> unwrap |> checkFill input

[<Fact>]
let ``crossing_contours_follow_even_odd_independent_of_input_directions_test`` () =
    let a,b = square 0.0 0.0 3.0,square 1.0 1.0 3.0
    for a in [a;Subpath.reverse a] do
        for b in [b;Subpath.reverse b] do
            let input = path [a;b]
            let loops = Offset.enumerateBandFaceLoops input |> unwrap
            Assert.Equal(2,loops.Subpaths.Length)
            checkFill input loops
            Offset.enumerateBandFaceLoops loops |> unwrap |> checkFill input

[<Fact>]
let ``kissing_seam_retains_two_opposite_occurrences_test`` () =
    let input = path [square 0.0 0.0 2.0;square 2.0 0.0 2.0]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    Assert.Equal(2,loops.Subpaths.Length)
    Assert.Equal(8,count loops)
    checkFill input loops

[<Fact>]
let ``even_multiplicity_outside_fill_is_preserved_as_retraces_test`` () =
    let a = square 0.0 0.0 2.0
    let input = path [a;a]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    Assert.Equal(8,count loops)
    checkFill input loops
    Offset.enumerateBandFaceLoops loops |> unwrap |> checkFill input

[<Fact>]
let ``triple_multiplicity_is_not_silently_dropped_test`` () =
    let a = square 0.0 0.0 2.0
    let input = path [a;a;a]
    let loops = Offset.enumerateBandFaceLoops input |> unwrap
    Assert.Equal(12,count loops)
    checkFill input loops

[<Fact>]
let ``empty_and_open_inputs_have_explicit_contracts_test`` () =
    Assert.True(Offset.enumerateBandFaceLoops Path.empty = Ok Path.empty)
    let source = [p(0.0,0.0);p(1.0,0.0)] |> Subpath.polyline |> unwrap
    Assert.True(Offset.enumerateBandFaceLoops (path [source]) = Error Offset.InternalBandSubpathNotClosed)
