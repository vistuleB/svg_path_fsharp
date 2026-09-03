module SvgPath.Tests.TransformParityTests

open SvgPath
open Xunit

let private tolerance = 0.000001<length>
let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

let private near (a: float<length>) (b: float<length>) =
    abs (a - b) <= tolerance

let private pointNear a b =
    near a.X b.X && near a.Y b.Y

let private bboxNear (box: BoundingBox) minPoint maxPoint =
    pointNear box.Min minPoint && pointNear box.Max maxPoint

let private boundingBox minPoint maxPoint : BoundingBox =
    { Min = minPoint; Max = maxPoint }

let private setClosed subpath =
    Subpath.setClosed true subpath |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``matrix_transforms_points_test`` () =
    let matrix = Transform.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length>
    Assert.Equal(point 30.0 40.0, Transform.point matrix (point 2.0 3.0))

[<Fact>]
let ``translate_matrix_transforms_points_test`` () =
    Assert.Equal(point 7.0 -4.0, Transform.point (Transform.translate 5.0<length> -7.0<length>) (point 2.0 3.0))
    Assert.Equal(point 7.0 -4.0, Transform.translatePoint (point 2.0 3.0) 5.0<length> -7.0<length>)

[<Fact>]
let ``matrix_transforms_bounding_boxes_test`` () =
    let box = boundingBox (point 1.0 2.0) (point 3.0 5.0)
    Assert.Equal(
        Ok(boundingBox (point 5.0 -1.0) (point 7.0 2.0)),
        Transform.boundingBox box (Transform.translate 4.0<length> -3.0<length>))

[<Fact>]
let ``rotated_matrix_transforms_bounding_box_corners_test`` () =
    let box = boundingBox (point 0.0 0.0) (point 2.0 1.0)
    let transformed = Transform.boundingBox box (Transform.rotate (degrees 90.0)) |> Result.defaultWith (failwithf "%A")
    Assert.True(bboxNear transformed (point -1.0 0.0) (point 0.0 2.0))

[<Fact>]
let ``scale_matrix_transforms_points_test`` () =
    Assert.Equal(point 8.0 12.0, Transform.point (Transform.scale 4.0) (point 2.0 3.0))
    Assert.Equal(point 8.0 12.0, Transform.scalePoint (point 2.0 3.0) 4.0)

[<Fact>]
let ``scale_xy_matrix_transforms_points_test`` () =
    Assert.Equal(point 8.0 -6.0, Transform.point (Transform.scaleXY 4.0 -2.0) (point 2.0 3.0))
    Assert.Equal(point 8.0 -6.0, Transform.scaleXYPoint (point 2.0 3.0) 4.0 -2.0)

[<Fact>]
let ``about_point_matrix_transforms_points_about_point_test`` () =
    let transformed = Transform.point (Transform.aboutPoint (Transform.scaleXY 2.0 3.0) (point 1.0 2.0)) (point 3.0 4.0)
    Assert.Equal(point 5.0 8.0, transformed)

[<Fact>]
let ``point_pair_map_maps_source_points_to_targets_test`` () =
    let sourceStart = point 1.0 2.0
    let sourceEnd = point 4.0 2.0
    let targetStart = point 10.0 -5.0
    let targetEnd = point 10.0 1.0
    let matrix = Transform.pointPairMap sourceStart sourceEnd targetStart targetEnd tolerance |> Result.defaultWith (failwithf "%A")
    Assert.True(pointNear (Transform.point matrix sourceStart) targetStart)
    Assert.True(pointNear (Transform.point matrix sourceEnd) targetEnd)
    Assert.Equal((0.0, 2.0, -2.0, 0.0, 14.0<length>, -7.0<length>), Transform.toTuple matrix)

[<Fact>]
let ``point_pair_map_maps_distinct_source_to_collapsed_target_test`` () =
    let matrix =
        Transform.pointPairMap (point 1.0 2.0) (point 4.0 2.0) (point 10.0 -5.0) (point 10.0 -5.0) tolerance
        |> Result.defaultWith (failwithf "%A")
    Assert.True(pointNear (Transform.point matrix (point 1.0 2.0)) (point 10.0 -5.0))
    Assert.True(pointNear (Transform.point matrix (point 4.0 2.0)) (point 10.0 -5.0))
    Assert.Equal((0.0, 0.0, -0.0, 0.0, 10.0<length>, -5.0<length>), Transform.toTuple matrix)

[<Fact>]
let ``point_pair_map_handles_large_finite_vectors_test`` () =
    let sourceStart = point -1.0e200 0.0
    let sourceEnd = point 1.0e200 0.0
    let matrix = Transform.pointPairMap sourceStart sourceEnd sourceStart sourceEnd tolerance |> Result.defaultWith (failwithf "%A")
    Assert.Equal((1.0, 0.0, -0.0, 1.0, 0.0<length>, 0.0<length>), Transform.toTuple matrix)

[<Fact>]
let ``point_pair_map_rejects_points_outside_tolerance_test`` () =
    Assert.Equal(Error(), Transform.pointPairMap (point 1.0 2.0) (point 1.0 2.0) (point 10.0 -5.0) (point 10.0 1.0) tolerance)

[<Fact>]
let ``point_pair_map_rejects_negative_tolerance_test`` () =
    Assert.Equal(Error(), Transform.pointPairMap (point 0.0 0.0) (point 1.0 0.0) (point 0.0 0.0) (point 1.0 0.0) -0.001<length>)

[<Fact>]
let ``point_triple_map_maps_source_points_to_targets_test`` () =
    let sourceA = point 1.0 2.0
    let sourceB = point 3.0 2.0
    let sourceC = point 1.0 5.0
    let targetA = point 10.0 -5.0
    let targetB = point 14.0 -3.0
    let targetC = point 7.0 1.0
    let matrix = Transform.pointTripleMap sourceA sourceB sourceC targetA targetB targetC tolerance |> Result.defaultWith (failwithf "%A")
    Assert.True(pointNear (Transform.point matrix sourceA) targetA)
    Assert.True(pointNear (Transform.point matrix sourceB) targetB)
    Assert.True(pointNear (Transform.point matrix sourceC) targetC)
    Assert.Equal((2.0, 1.0, -1.0, 2.0, 10.0<length>, -10.0<length>), Transform.toTuple matrix)

[<Fact>]
let ``point_triple_map_rejects_points_outside_tolerance_test`` () =
    Assert.Equal(
        Error(),
        Transform.pointTripleMap (point 1.0 2.0) (point 1.0 2.0) (point 1.0 2.0) (point 10.0 -5.0) (point 14.0 -3.0) (point 7.0 1.0) tolerance)

[<Fact>]
let ``rotate_matrix_uses_degrees_test`` () =
    let line = Line(point 1.0 0.0, point 1.0 2.0)
    let segment = Transform.rotateSegment line (degrees 90.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 1 H -2", Serialize.segment segment)

[<Fact>]
let ``transform_about_rotation_rotates_about_point_test`` () =
    let transformed = Transform.point (Transform.aboutPoint (Transform.rotate (degrees 90.0)) (point 1.0 2.0)) (point 3.0 2.0)
    Assert.True(pointNear transformed (point 1.0 4.0))

[<Fact>]
let ``path_about_point_transforms_path_about_point_test`` () =
    let source = Path.ofSubpaths [ Subpath.create [ Line(point 3.0 2.0, point 3.0 4.0) ] |> Result.defaultWith (failwithf "%A") ]
    let path = Transform.pathAboutPoint source (Transform.rotate (degrees 90.0)) (point 1.0 2.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 1 4 H -1", Serialize.path path)

[<Fact>]
let ``segment_about_anchor_transforms_segment_about_anchor_test`` () =
    let transformed = Transform.segmentAboutAnchor (Line(point 0.0 0.0, point 10.0 0.0)) (Transform.rotate (degrees 90.0)) TopLeft |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 V 10", Serialize.segment transformed)

[<Fact>]
let ``subpath_about_anchor_transforms_subpath_about_anchor_test`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 0.0 10.0) ] |> Result.defaultWith (failwithf "%A")
    let transformed = Transform.subpathAboutAnchor subpath (Transform.scaleXY 1.0 -1.0) Center |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 10 V 0", Serialize.subpath transformed)

[<Fact>]
let ``path_about_anchor_transforms_path_about_anchor_test`` () =
    let path = Path.ofSubpaths [ Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0) ] |> Result.defaultWith (failwithf "%A") ]
    let transformed = Transform.pathAboutAnchor path (Transform.scaleXY -1.0 1.0) Center |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 10 0 H 0", Serialize.path transformed)

[<Fact>]
let ``skew_matrices_use_degrees_test`` () =
    Assert.Equal(point 5.0 3.0, Transform.point (Transform.skewX (degrees 45.0)) (point 2.0 3.0))
    Assert.Equal(point 2.0 5.0, Transform.skewYPoint (point 2.0 3.0) (degrees 45.0))

[<Fact>]
let ``chain_applies_first_then_second_test`` () =
    let transform = Transform.chain (Transform.scale 2.0) (Transform.translate 10.0<length> 20.0<length>)
    Assert.Equal(point 12.0 22.0, Transform.point transform (point 1.0 1.0))

[<Fact>]
let ``multiply_uses_algebraic_left_times_right_order_test`` () =
    let scale = Transform.scale 2.0
    let translate = Transform.translate 10.0<length> 20.0<length>
    Assert.Equal(point 12.0 22.0, Transform.point (Transform.multiply translate scale) (point 1.0 1.0))
    Assert.Equal(point 22.0 42.0, Transform.point (Transform.multiply scale translate) (point 1.0 1.0))

[<Fact>]
let ``direct_subpath_and_path_helpers_delegate_to_matrices_test`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 5.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let path = Subpath.asPath subpath
    let translatedSubpath = Transform.translateSubpath subpath 10.0<length> 20.0<length> |> Result.defaultWith (failwithf "%A")
    let scaledPath = Transform.scalePath path 2.0 |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 10 20 H 15", Serialize.subpath translatedSubpath)
    Assert.Equal("M 0 0 H 10", Serialize.path scaledPath)

[<Fact>]
let ``to_tuple_exposes_svg_matrix_values_test`` () =
    let values = Transform.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length> |> Transform.toTuple
    Assert.Equal((2.0, 3.0, 5.0, 7.0, 11.0<length>, 13.0<length>), values)

[<Fact>]
let ``from_tuple_creates_matrix_from_svg_matrix_values_test`` () =
    let matrix = Transform.fromTuple (2.0, 3.0, 5.0, 7.0, 11.0<length>, 13.0<length>)
    Assert.Equal((2.0, 3.0, 5.0, 7.0, 11.0<length>, 13.0<length>), Transform.toTuple matrix)

[<Fact>]
let ``line_transform_test`` () =
    let matrix = Transform.matrix 1.0 0.0 0.0 1.0 10.0<length> -5.0<length>
    let segment = Transform.segment (Line(point 0.0 0.0, point 5.0 0.0)) matrix |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 10 -5 H 15", Serialize.segment segment)

[<Fact>]
let ``quadratic_and_cubic_bezier_transform_test`` () =
    let matrix = Transform.matrix 2.0 0.0 0.0 3.0 0.0<length> 0.0<length>
    let quadratic = Transform.segment (QuadraticBezier(point 0.0 0.0, point 1.0 2.0, point 3.0 4.0)) matrix |> Result.defaultWith (failwithf "%A")
    let cubic = Transform.segment (CubicBezier(point 0.0 0.0, point 1.0 2.0, point 3.0 4.0, point 5.0 6.0)) matrix |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 2 6 6 12", Serialize.segment quadratic)
    Assert.Equal("M 0 0 C 2 6 6 12 10 18", Serialize.segment cubic)

[<Fact>]
let ``closed_subpath_transform_preserves_semantic_closure_test`` () =
    let matrix = Transform.matrix 1.0 0.0 0.0 1.0 10.0<length> 0.0<length>
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 0.0 0.0) ] |> Result.defaultWith (failwithf "%A") |> setClosed
    let transformed = Transform.subpath subpath matrix |> Result.defaultWith (failwithf "%A")
    Assert.True(Subpath.isClosed transformed)
    Assert.Equal("M 10 0 H 20 Z", Serialize.subpath transformed)

[<Fact>]
let ``path_transform_test`` () =
    let matrix = Transform.matrix 1.0 0.0 0.0 1.0 1.0<length> 2.0<length>
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let path = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); subpath ]
    let transformed = Transform.path path matrix |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 1 2 M 1 2 H 11", Serialize.path transformed)

[<Fact>]
let ``arc_identity_transform_preserves_arc_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = false; End = point 10.0 0.0 }
    let transformed = Transform.segment arc (Transform.identity ()) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 5 5 0 0 0 10 0", Serialize.segment transformed)

[<Fact>]
let ``arc_non_uniform_scale_transform_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 10.0; XAxisRotation = degrees 0.0; LargeArc = true; Sweep = false; End = point 5.0 10.0 }
    let matrix = Transform.matrix 2.0 0.0 0.0 3.0 0.0<length> 0.0<length>
    let transformed = Transform.segment arc matrix |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 10 30 0 1 0 10 30", Serialize.segment transformed)

[<Fact>]
let ``arc_shear_transform_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point 10.0 0.0 }
    let matrix = Transform.matrix 1.0 0.0 1.0 1.0 0.0<length> 0.0<length>
    let transformed = Transform.segment arc matrix |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 8.09 3.09 31.717 0 1 10 0", Serialize.segmentWith transformed (Serialize.decimalOptions 3))

[<Fact>]
let ``arc_reflection_flips_sweep_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point 10.0 0.0 }
    let transformed = Transform.segment arc (Transform.matrix -1.0 0.0 0.0 1.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 5 5 0 0 0 -10 0", Serialize.segment transformed)

[<Fact>]
let ``arc_degenerate_transform_errors_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = false; End = point 10.0 0.0 }
    Assert.Equal(Error DegenerateArcTransform, Transform.segment arc (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>))

[<Fact>]
let ``strict_subpath_transform_errors_on_collapsed_arc_test`` () =
    let subpath = Subpath.create [ Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 } ] |> Result.defaultWith (failwithf "%A")
    let matrix = Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>
    Assert.Equal(Error DegenerateArcTransform, Transform.subpath subpath matrix)

[<Fact>]
let ``graceful_arc_transform_returns_collapsed_line_test`` () =
    let arc = Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
    let segment = Transform.segmentGracefully arc (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M -5 0 H 5", Serialize.segment segment)

[<Fact>]
let ``graceful_arc_transform_follows_full_collapse_to_point_test`` () =
    let arc = Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
    let segment = Transform.segmentGracefully arc (Transform.matrix 0.0 0.0 0.0 0.0 7.0<length> 11.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 7 11 H 7", Serialize.segment segment)

[<Fact>]
let ``graceful2_arc_transform_preserves_transformed_endpoints_test`` () =
    let arc = Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
    let subpath = Transform.segmentToSubpathGracefully arc (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 5 0 H -5", Serialize.subpath subpath)

[<Fact>]
let ``graceful2_line_transform_returns_single_segment_subpath_test`` () =
    let subpath = Transform.segmentToSubpathGracefully (Line(point 1.0 2.0, point 4.0 2.0)) (Transform.matrix 1.0 0.0 0.0 1.0 10.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 11 2 H 14", Serialize.subpath subpath)

[<Fact>]
let ``graceful2_arc_transform_preserves_out_and_back_motion_test`` () =
    let arc = Arc { Start = point 3.5355339059 -3.5355339059; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point 3.5355339059 3.5355339059 }
    let subpath = Transform.segmentToSubpathGracefully arc (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 3.53553 0 H 5 H 3.53553", Serialize.subpath subpath)

[<Fact>]
let ``graceful2_arc_transform_follows_full_collapse_to_point_test`` () =
    let arc = Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
    let subpath = Transform.segmentToSubpathGracefully arc (Transform.matrix 0.0 0.0 0.0 0.0 7.0<length> 11.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 7 11 H 7", Serialize.subpath subpath)

[<Fact>]
let ``graceful_subpath_transform_keeps_surrounding_continuity_test`` () =
    let subpath =
        Subpath.create [
            Line(point -10.0 0.0, point 5.0 0.0)
            Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
            Line(point -5.0 0.0, point -10.0 0.0)
        ] |> Result.defaultWith (failwithf "%A")
    let transformed = Transform.subpathGracefully subpath (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M -10 0 H 5 H -5 H -10", Serialize.subpath transformed)

[<Fact>]
let ``graceful_closed_subpath_transform_preserves_semantic_closure_test`` () =
    let subpath =
        Subpath.create [
            Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 }
            Line(point -5.0 0.0, point 5.0 0.0)
        ] |> Result.defaultWith (failwithf "%A") |> setClosed
    let transformed = Transform.subpathGracefully subpath (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.True(Subpath.isClosed transformed)
    Assert.Equal("M 5 0 H -5 Z", Serialize.subpath transformed)

[<Fact>]
let ``graceful_path_transform_converts_collapsed_arcs_in_each_subpath_test`` () =
    let first = Subpath.create [ Arc { Start = point 5.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = point -5.0 0.0 } ] |> Result.defaultWith (failwithf "%A")
    let second = Subpath.create [ Line(point 0.0 2.0, point 4.0 2.0) ] |> Result.defaultWith (failwithf "%A")
    let transformed = Transform.pathGracefully (Path.ofSubpaths [ first; second ]) (Transform.matrix 1.0 0.0 0.0 0.0 0.0<length> 3.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 5 3 H -5 M 0 3 H 4", Serialize.path transformed)

[<Fact>]
let ``graceful_arc_transform_returns_vertical_collapsed_line_test`` () =
    let arc = Arc { Start = point 0.0 5.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = false; End = point 0.0 -5.0 }
    let segment = Transform.segmentGracefully arc (Transform.matrix 0.0 0.0 0.0 1.0 10.0<length> 0.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 10 -5 V 5", Serialize.segment segment)

[<Fact>]
let ``graceful_non_degenerate_arc_transform_returns_arc_test`` () =
    let arc = Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = false; End = point 10.0 0.0 }
    let segment = Transform.segmentGracefully arc (Transform.identity ()) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 5 5 0 0 0 10 0", Serialize.segment segment)
