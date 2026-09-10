module SvgPath.Tests.BandOrientationTests

open SvgPath
open Xunit

let private p (x,y) = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private unwrap result = Result.defaultWith (failwithf "%A") result
let private polygon points = points |> List.map p |> Subpath.polygon |> unwrap
let private square x y size = polygon [x,y; x+size,y; x+size,y+size; x,y+size]
let private path = Path.ofSubpaths

[<Fact>]
let ``nested_contours_alternate_orientation_independent_of_input_order_test`` () =
    let outer, middle, inner = square 0.0 0.0 10.0, square 2.0 2.0 6.0, square 4.0 4.0 2.0
    let source = path [Subpath.reverse inner; Subpath.reverse outer; middle]
    let expected = path [inner; outer; Subpath.reverse middle]
    Assert.True(Offset.orientBandPath source = Ok expected)
    Assert.True(Offset.orientBandPath expected = Ok expected)

[<Fact>]
let ``disconnected_and_vertex_touching_contours_orient_independently_test`` () =
    let a,b,c = square 0.0 0.0 2.0, square 2.0 2.0 2.0, square 8.0 0.0 2.0
    Assert.True(Offset.orientBandPath (path [Subpath.reverse a; b; Subpath.reverse c]) = Ok(path [a;b;c]))

[<Fact>]
let ``fully_retraced_contour_remains_undecided_test`` () =
    let retrace = polygon [0.0,0.0; 3.0,0.0]
    for contour in [retrace; Subpath.reverse retrace] do
        Assert.True(Offset.orientBandPath (path [contour]) = Ok(path [contour]))

[<Fact>]
let ``retraced_spur_does_not_constrain_its_loop_orientation_test`` () =
    let contour = polygon [0.0,0.0;4.0,0.0;4.0,4.0;0.0,4.0;0.0,0.0;-2.0,0.0]
    Assert.True(Offset.orientBandPath (path [Subpath.reverse contour]) = Ok(path [contour]))

[<Fact>]
let ``coincident_different_contours_are_unexpected_not_summed_test`` () =
    let contour = square 0.0 0.0 4.0
    for contours in [[contour;contour];[contour;Subpath.reverse contour];[contour;contour;contour]] do
        match Offset.orientBandPath (path contours) with
        | Error(InternalBandOrientationUnexpectedEdge(_,count)) -> Assert.Equal(List.length contours,count)
        | value -> failwithf "Unexpected %A" value

[<Fact>]
let ``same_loop_same_direction_duplicate_is_unexpected_test`` () =
    let contour = polygon [0.0,0.0;4.0,0.0;4.0,4.0;0.0,4.0;0.0,0.0;4.0,0.0;4.0,4.0;0.0,4.0]
    match Offset.orientBandPath (path [contour]) with
    | Error(InternalBandOrientationUnexpectedEdge(_,2)) -> ()
    | value -> failwithf "Unexpected %A" value

[<Fact>]
let ``opposite_lobe_orientations_allow_negative_face_values_test`` () =
    let bowtie = polygon [0.0,0.0;4.0,4.0;0.0,4.0;4.0,0.0]
    let result = Offset.orientBandPath (path [bowtie]) |> unwrap
    Assert.True(result = path [bowtie] || result = path [Subpath.reverse bowtie])
    Assert.True(Offset.orientBandPath result = Ok result)

[<Fact>]
let ``same_direction_nested_lobe_exceeding_unit_winding_is_rejected_test`` () =
    let contour = polygon [0.0,0.0;6.0,0.0;6.0,6.0;0.0,6.0;0.0,0.0;1.0,1.0;3.0,1.0;3.0,3.0;1.0,3.0;1.0,1.0]
    for contour in [contour;Subpath.reverse contour] do
        match Offset.orientBandPath (path [contour]) with
        | Error(InternalBandOrientationConflict _) -> ()
        | value -> failwithf "Unexpected %A" value

[<Fact>]
let ``open_contours_are_rejected_and_empty_path_is_preserved_test`` () =
    let source = [p(0.0,0.0);p(1.0,0.0)] |> Subpath.polyline |> unwrap
    Assert.True(Offset.orientBandPath (path [source]) = Error InternalBandSubpathNotClosed)
    Assert.True(Offset.orientBandPath (path []) = Ok(path []))
