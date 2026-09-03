module SvgPath.Tests.ArrangementParityTests

open SvgPath
open Xunit

let private tolerance = 0.000001<length>
let private minimumChord = 0.00001<length>
let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private line ax ay bx by = Line(point ax ay, point bx by)
let private arc start radius largeArc sweep finish =
    Arc
        { Start = start
          Radius = radius
          XAxisRotation = 0.0<degree>
          LargeArc = largeArc
          Sweep = sweep
          End = finish }

let private closedSubpath segments =
    Subpath.create segments
    |> Result.bind (Subpath.setClosed true)
    |> Result.defaultWith (failwithf "%A")

let private square x y size =
    closedSubpath [
        line x y (x + size) y
        line (x + size) y (x + size) (y + size)
        line (x + size) (y + size) x (y + size)
        line x (y + size) x y
    ]

let private rectangle x y width height =
    closedSubpath [
        line x y (x + width) y
        line (x + width) y (x + width) (y + height)
        line (x + width) (y + height) x (y + height)
        line x (y + height) x y
    ]

let private buildGraph subpaths =
    Arrangement.build (subpaths |> List.map Path.singleton) tolerance minimumChord
    |> Result.map _.Graph

let private buildSegments segments =
    Arrangement.buildWith segments tolerance minimumChord 0.0<parameter>
    |> Result.defaultWith (failwithf "%A")

let private edge id segment startVertex endVertex =
    { Id = id
      Segment = segment
      Bounds = Segment.boundingBox segment |> Result.defaultWith (failwithf "%A")
      StartVertex = startVertex
      EndVertex = endVertex
      ForwardMultiplicity = 1
      ReverseMultiplicity = 1 }

let private graphWithClusteredEndpoints endpoints clusterTolerance =
    let rec loop index endpoints graph =
        match endpoints with
        | [] -> Ok graph
        | endpoint :: rest ->
            Arrangement.insertAtomicSegment
                graph
                (Line(point 100.0 (float index * 10.0), endpoint))
                clusterTolerance
                minimumChord
            |> Result.bind (loop (index + 1) rest)
    loop 0 endpoints Arrangement.empty

[<Fact>]
let ``segment_images_share_coincident_edges_with_source_orientation_test`` () =
    let forward = Subpath.assertCreate [ line 0.0 0.0 10.0 0.0 ]
    let reverse = Subpath.assertCreate [ line 10.0 0.0 0.0 0.0 ]
    let build = Arrangement.build [ Path.ofSubpaths [ forward; reverse ] ] tolerance minimumChord |> Result.defaultWith (failwithf "%A")
    let forwardImage, reverseImage = build.SegmentImages[0], build.SegmentImages[1]
    Assert.Single(forwardImage.Edges) |> ignore
    Assert.Single(reverseImage.Edges) |> ignore
    Assert.False(forwardImage.Edges.Head.Reversed)
    Assert.True(reverseImage.Edges.Head.Reversed)
    Assert.Equal(forwardImage.Edges.Head.EdgeId, reverseImage.Edges.Head.EdgeId)

[<Fact>]
let ``segment_images_map_different_source_decompositions_to_shared_edges_test`` () =
    let whole = Subpath.assertCreate [ line 0.0 0.0 10.0 0.0 ]
    let divided = Subpath.assertCreate [ line 0.0 0.0 5.0 0.0; line 5.0 0.0 10.0 0.0 ]
    let build = Arrangement.build [ Path.ofSubpaths [ whole; divided ] ] tolerance minimumChord |> Result.defaultWith (failwithf "%A")
    let wholeImage, dividedFirstImage, dividedSecondImage = build.SegmentImages[0], build.SegmentImages[1], build.SegmentImages[2]
    Assert.Equal(2, wholeImage.Edges.Length)
    Assert.Single(dividedFirstImage.Edges) |> ignore
    Assert.Single(dividedSecondImage.Edges) |> ignore
    Assert.Equal(wholeImage.Edges[0], dividedFirstImage.Edges.Head)
    Assert.Equal(wholeImage.Edges[1], dividedSecondImage.Edges.Head)

[<Fact>]
let ``forced_parity_reports_unresolved_diamond_choice_test`` () =
    let source = point 0.0 0.0
    let upper = point 5.0 -5.0
    let lower = point 5.0 5.0
    let sink = point 10.0 0.0
    let build = buildSegments [ Line(source, upper); Line(upper, sink); Line(source, lower); Line(lower, sink) ]
    let sourceVertex = build.Graph.Vertices |> List.find (fun vertex -> vertex.Point = source)
    let sinkVertex = build.Graph.Vertices |> List.find (fun vertex -> vertex.Point = sink)
    match Arrangement.forcedParityCapacities build.Graph [ RequiredVertexParity(sourceVertex.Id, 1); RequiredVertexParity(sinkVertex.Id, 1) ] with
    | Error(ForcedParityAmbiguous vertices) -> Assert.Equal(2, vertices.Length)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``forced_parity_reduces_unique_edge_at_higher_threshold_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 5.0 10.0; line 5.0 10.0 0.0 0.0 ]
    let first, second, third = build.Graph.Edges[0], build.Graph.Edges[1], build.Graph.Edges[2]
    let reduced =
        Arrangement.forcedParityCapacitiesWith
            build.Graph
            [ { EdgeId = first.Id; Capacity = 2 }; { EdgeId = second.Id; Capacity = 3 }; { EdgeId = third.Id; Capacity = 2 } ]
            []
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<int list>([ 2; 2; 2 ], reduced |> List.map _.Capacity)

[<Fact>]
let ``preferred_parity_guides_reduction_but_allows_isolation_test`` () =
    let line = line 0.0 0.0 10.0 0.0
    let first = buildSegments [ line ]
    let startVertex, endVertex = first.Graph.Vertices[0], first.Graph.Vertices[1]
    let isolated =
        Arrangement.forcedParityCapacities first.Graph [ RequiredVertexParity(startVertex.Id, 0); PreferredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0, isolated.Head.Capacity)

    let doubled = buildSegments [ line; line ]
    let startVertex, endVertex = doubled.Graph.Vertices[0], doubled.Graph.Vertices[1]
    let preserved =
        Arrangement.forcedParityCapacities doubled.Graph [ PreferredVertexParity(startVertex.Id, 1); PreferredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, preserved.Head.Capacity)

[<Fact>]
let ``forced_parity_sums_forward_and_reverse_capacity_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 0.0 0.0 ]
    let assignment = Arrangement.forcedParityCapacities build.Graph [] |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(2, assignment.Capacity)

[<Fact>]
let ``forced_parity_accepts_explicit_initial_capacities_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 0.0 0.0 10.0 0.0 ]
    let edge = build.Graph.Edges.Head
    let zero =
        Arrangement.forcedParityCapacitiesWith build.Graph [ { EdgeId = edge.Id; Capacity = 0 } ] []
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(0, zero.Capacity)

    let startVertex, endVertex = build.Graph.Vertices[0], build.Graph.Vertices[1]
    let reduced =
        Arrangement.forcedParityCapacitiesWith
            build.Graph
            [ { EdgeId = edge.Id; Capacity = 2 } ]
            [ RequiredVertexParity(startVertex.Id, 1); RequiredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(1, reduced.Capacity)

[<Fact>]
let ``forced_parity_rejects_invalid_vertex_parities_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0 ]
    let vertex = build.Graph.Vertices.Head
    Assert.Equal(
        Error(ForcedParityDuplicateVertex vertex.Id),
        Arrangement.forcedParityCapacities build.Graph [ RequiredVertexParity(vertex.Id, 0); RequiredVertexParity(vertex.Id, 1) ])
    Assert.Equal(Error(ForcedParityMissingVertex 999), Arrangement.forcedParityCapacities build.Graph [ RequiredVertexParity(999, 0) ])
    Assert.Equal(Error(ForcedParityInvalidVertexParity(vertex.Id, 2)), Arrangement.forcedParityCapacities build.Graph [ RequiredVertexParity(vertex.Id, 2) ])

[<Fact>]
let ``cyclic_order_uses_clockwise_common_circle_positions_test`` () =
    let center = point 0.0 0.0
    let build =
        buildSegments [
            Line(center, point 10.0 0.0)
            Line(center, point 0.0 10.0)
            Line(center, point -10.0 0.0)
            Line(point 0.0 -10.0, center)
        ]
    let order = Arrangement.vertexCyclicOrderWith build.Graph 0 tolerance 3 |> Result.defaultWith (failwithf "%A")
    let flattened = order |> List.concat |> List.map (fun edge -> edge.EdgeId, edge.Reversed)
    Assert.Equal<(int * bool) list>([ 0, false; 1, false; 2, false; 3, true ], flattened)

[<Fact>]
let ``cyclic_order_separates_equal_endpoint_tangents_on_circle_test`` () =
    let center = point 0.0 0.0
    let build =
        buildSegments [
            QuadraticBezier(center, point 5.0 0.0, point 10.0 3.0)
            QuadraticBezier(center, point 5.0 0.0, point 10.0 -3.0)
            Line(center, point -10.0 0.0)
        ]
    let order = Arrangement.vertexCyclicOrderWith build.Graph 0 tolerance 3 |> Result.defaultWith (failwithf "%A")
    Assert.Equal<int list>([ 0; 2; 1 ], order |> List.concat |> List.map _.EdgeId)

[<Fact>]
let ``cyclic_order_groups_circle_points_below_both_separation_limits_test`` () =
    let center = point 0.0 0.0
    let build =
        Arrangement.buildWith [ Line(center, point 10.0 0.0); Line(center, point 10.0 0.00000001) ] 0.000000001<length> minimumChord 0.0<parameter>
        |> Result.defaultWith (failwithf "%A")
    let groups = Arrangement.vertexCyclicOrderWith build.Graph 0 tolerance 3 |> Result.defaultWith (failwithf "%A")
    Assert.Equal<int list list>([ [ 0; 1 ] ], groups |> List.map (List.map _.EdgeId))

[<Fact>]
let ``cyclic_orders_cover_every_vertex_of_built_square_test`` () =
    let graph = buildGraph [ square 0.0 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, graph.CyclicOrders.Length)
    Assert.True(graph.CyclicOrders |> List.forall (fun (_, order) -> (order |> List.concat).Length = 2))

[<Fact>]
let ``dual_infinite_face_collects_disconnected_islands_test`` () =
    let graph = buildGraph [ square 0.0 0.0 10.0; square 20.0 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let dual = Arrangement.dual graph |> Result.defaultWith (failwithf "%A")
    let outer = dual.Faces.Head
    Assert.True(outer.Outer)
    Assert.Equal(2, outer.Walks.Length)
    Assert.True(outer.Walks |> List.forall (fun walk -> not walk.Outer))

[<Fact>]
let ``dual_bounded_face_orders_outer_walk_before_island_test`` () =
    let graph = buildGraph [ square 0.0 0.0 20.0; square 5.0 5.0 5.0 ] |> Result.defaultWith (failwithf "%A")
    let dual = Arrangement.dual graph |> Result.defaultWith (failwithf "%A")
    let face = dual.Faces |> List.find (fun face -> not face.Outer && face.Walks.Length = 2)
    Assert.True(face.Walks[0].Outer)
    Assert.False(face.Walks[1].Outer)

[<Fact>]
let ``dual_bridge_has_same_face_on_both_sides_test`` () =
    let graph = buildGraph [ Subpath.ofSegment (line 0.0 0.0 10.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let dual = Arrangement.dual graph |> Result.defaultWith (failwithf "%A")
    let edgeFaces = dual.EdgeFaces |> List.exactlyOne
    Assert.Equal(edgeFaces.LeftFace, edgeFaces.RightFace)

[<Fact>]
let ``dual_empty_graph_is_the_infinite_face_test`` () =
    let dual = Arrangement.dual Arrangement.empty |> Result.defaultWith (failwithf "%A")
    let face = dual.Faces |> List.exactlyOne
    Assert.True(face.Outer)
    Assert.Empty(face.Walks)
    Assert.Empty(dual.EdgeFaces)

[<Fact>]
let ``dual_overlapping_squares_partition_every_edge_side_test`` () =
    let graph = buildGraph [ square 0.0 0.0 10.0; square 5.0 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let dual = Arrangement.dual graph |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, dual.Faces.Length)
    Assert.Equal(graph.Edges.Length, dual.EdgeFaces.Length)
    let walkedSides = dual.Faces |> List.collect _.Walks |> List.collect _.Edges |> List.length
    Assert.Equal(graph.Edges.Length * 2, walkedSides)

[<Fact>]
let ``dual_self_crossing_bowtie_has_two_bounded_faces_test`` () =
    let bowtie =
        closedSubpath [
            line 0.0 0.0 10.0 10.0
            line 10.0 10.0 0.0 10.0
            line 0.0 10.0 10.0 0.0
            line 10.0 0.0 0.0 0.0
        ]
    let graph = buildGraph [ bowtie ] |> Result.defaultWith (failwithf "%A")
    let dual = Arrangement.dual graph |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, dual.Faces.Length)
    Assert.Equal(2, dual.Faces |> List.filter (fun face -> not face.Outer) |> List.length)

[<Fact>]
let ``build_preserves_source_path_grouping_test`` () =
    let first = Path.singleton (square 0.0 0.0 10.0)
    let second = Path.ofSubpaths [ square 20.0 0.0 5.0; square 30.0 0.0 5.0 ]
    let build = Arrangement.build [ first; second ] tolerance minimumChord |> Result.defaultWith (failwithf "%A")
    Assert.Equal(12, build.SegmentImages.Length)

[<Fact>]
let ``segment_images_follow_crossing_source_traversals_test`` () =
    let horizontal = Subpath.assertCreate [ line 0.0 0.0 10.0 0.0 ]
    let vertical = Subpath.assertCreate [ line 5.0 -5.0 5.0 5.0 ]
    let build = Arrangement.build [ Path.ofSubpaths [ horizontal; vertical ] ] tolerance minimumChord |> Result.defaultWith (failwithf "%A")
    let horizontalEdges =
        build.SegmentImages[0].Edges
        |> List.map (fun reference -> build.Graph.Edges |> List.find (fun edge -> edge.Id = reference.EdgeId), reference.Reversed)
    let verticalEdges =
        build.SegmentImages[1].Edges
        |> List.map (fun reference -> build.Graph.Edges |> List.find (fun edge -> edge.Id = reference.EdgeId), reference.Reversed)
    let orientedSegment (edge, reversed) =
        if reversed then Segment.reverse edge.Segment else edge.Segment
    Assert.Equal(2, horizontalEdges.Length)
    Assert.Equal(2, verticalEdges.Length)
    Assert.Equal(point 0.0 0.0, Segment.start (orientedSegment horizontalEdges[0]))
    Assert.Equal(point 10.0 0.0, Segment.finish (orientedSegment horizontalEdges[1]))
    Assert.Equal(point 5.0 -5.0, Segment.start (orientedSegment verticalEdges[0]))
    Assert.Equal(point 5.0 5.0, Segment.finish (orientedSegment verticalEdges[1]))

[<Fact>]
let ``progressive_segment_build_maps_crossing_sources_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 5.0 -5.0 5.0 5.0 ]
    Assert.Equal(5, build.Graph.Vertices.Length)
    Assert.Equal(4, build.Graph.Edges.Length)
    Assert.Equal(2, build.SegmentImages[0].Edges.Length)
    Assert.Equal(2, build.SegmentImages[1].Edges.Length)
    let firstSource = build.SegmentImages[0]
    Assert.Equal(0, firstSource.SegmentIndex)
    Assert.Equal(0.0<parameter>, firstSource.Edges.Head.From)
    Assert.Equal(1.0<parameter>, firstSource.Edges[1].To)

[<Fact>]
let ``progressive_segment_build_splits_existing_edges_by_incoming_endpoints_test`` () =
    let vertical = line 1.0 2.0 1.0 0.0
    let left = line 0.0 1.0 (1.0 - 0.4e-9) 1.0
    let right = line (1.0 + 0.4e-9) 1.0 2.0 1.0
    let build segments =
        Arrangement.buildWith segments 1.0e-9<length> 1.0e-12<length> 0.0<parameter>
        |> Result.defaultWith (failwithf "%A")
    let first = build [ vertical; left; right ]
    let second = build [ left; right; vertical ]
    Assert.Equal(first.Graph.Vertices.Length, second.Graph.Vertices.Length)
    Assert.Equal(first.Graph.Edges.Length, second.Graph.Edges.Length)
    Assert.Equal(5, first.Graph.Vertices.Length)
    Assert.Equal(4, first.Graph.Edges.Length)

[<Fact>]
let ``builder_splits_crossing_lines_at_shared_vertex_test`` () =
    let graph =
        buildGraph [
            Subpath.assertCreate [ line -10.0 0.0 10.0 0.0 ]
            Subpath.assertCreate [ line 0.0 -10.0 0.0 10.0 ]
        ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(5, graph.Vertices.Length)
    Assert.Equal(4, graph.Edges.Length)

[<Fact>]
let ``builder_keeps_geometrically_distinct_cuts_on_long_segment_test`` () =
    let graph =
        Arrangement.build
            [ Path.singleton (Subpath.assertCreate [ line 0.0 0.0 10000.0 0.0 ])
              Path.singleton (Subpath.assertCreate [ line 5000.0 -10.0 5000.0 10.0 ])
              Path.singleton (Subpath.assertCreate [ line 5005.0 -10.0 5005.0 10.0 ]) ]
            0.001<length>
            minimumChord
        |> Result.map _.Graph
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(8, graph.Vertices.Length)
    Assert.Equal(7, graph.Edges.Length)

[<Fact>]
let ``builder_refines_partial_line_overlap_and_counts_middle_test`` () =
    let graph =
        buildGraph [
            Subpath.assertCreate [ line 0.0 0.0 10.0 0.0 ]
            Subpath.assertCreate [ line 5.0 0.0 15.0 0.0 ]
        ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, graph.Vertices.Length)
    Assert.Equal(3, graph.Edges.Length)
    Assert.Equal(1, graph.Edges |> List.filter (fun edge -> edge.ForwardMultiplicity = 2) |> List.length)

[<Fact>]
let ``builder_consolidates_phase_shifted_opposite_circle_arcs_test`` () =
    let radius = point 10.0 10.0
    let east = point 10.0 0.0
    let west = point -10.0 0.0
    let southeast = point 7.0710678118654755 7.0710678118654755
    let northwest = point -7.0710678118654755 -7.0710678118654755
    let clockwise =
        closedSubpath [
            arc east radius false true west
            arc west radius false true east
        ]
    let counterclockwise =
        closedSubpath [
            arc southeast radius false false northwest
            arc northwest radius false false southeast
        ]
    let graph = buildGraph [ clockwise; counterclockwise ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, graph.Vertices.Length)
    Assert.Equal(4, graph.Edges.Length)
    Assert.True(graph.Edges |> List.forall (fun edge -> edge.ForwardMultiplicity = 1 && edge.ReverseMultiplicity = 1))
    Assert.Equal(Ok(), Arrangement.validate graph tolerance minimumChord)

[<Fact>]
let ``builder_consolidates_near_equal_circles_inside_tolerance_test`` () =
    let graphTolerance = 0.0001<length>
    let east = point 10.0 0.0
    let west = point -10.0 0.0
    let innerEast = point 9.99996 0.0
    let innerWest = point -9.99996 0.0
    let outer =
        closedSubpath [
            arc east (point 10.0 10.0) false true west
            arc west (point 10.0 10.0) false true east
        ]
    let innerReversed =
        closedSubpath [
            arc innerEast (point 9.99996 9.99996) false false innerWest
            arc innerWest (point 9.99996 9.99996) false false innerEast
        ]
    let graph =
        Arrangement.build [ Path.singleton outer; Path.singleton innerReversed ] graphTolerance minimumChord
        |> Result.map _.Graph
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, graph.Vertices.Length)
    Assert.Equal(2, graph.Edges.Length)
    Assert.True(graph.Edges |> List.forall (fun edge -> edge.ForwardMultiplicity = 1 && edge.ReverseMultiplicity = 1))

[<Fact>]
let ``build_rejects_invalid_tolerance_before_inspecting_sources_test`` () =
    let badSegment = line 0.0 0.0 0.0 0.0
    Assert.Equal(Error(InvalidArrangementTolerance 0.0<length>), Arrangement.buildWith [ badSegment ] 0.0<length> minimumChord 0.0<parameter>)
    Assert.Equal(Error(InvalidArrangementTolerance -1.0<length>), Arrangement.build [ Path.singleton (Subpath.ofSegment badSegment) ] -1.0<length> minimumChord)

[<Fact>]
let ``build_rejects_invalid_minimum_chord_before_inspecting_sources_test`` () =
    let badSegment = line 0.0 0.0 0.0 0.0
    Assert.Equal(Error(InvalidMinimumChord 0.0<length>), Arrangement.buildWith [ badSegment ] tolerance 0.0<length> 0.0<parameter>)
    Assert.Equal(Error(InvalidMinimumChord -1.0<length>), Arrangement.build [ Path.singleton (Subpath.ofSegment badSegment) ] tolerance -1.0<length>)

[<Fact>]
let ``build_rejects_nonfinite_numeric_options_test`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let infiniteLength = LanguagePrimitives.FloatWithMeasure<length> System.Double.PositiveInfinity
    let infiniteParameter = LanguagePrimitives.FloatWithMeasure<parameter> System.Double.PositiveInfinity
    Assert.Equal(Error(InvalidArrangementTolerance infiniteLength), Arrangement.buildWith [ segment ] infiniteLength minimumChord 0.0<parameter>)
    Assert.Equal(Error(InvalidMinimumChord infiniteLength), Arrangement.buildWith [ segment ] tolerance infiniteLength 0.0<parameter>)
    Assert.Equal(Error(InvalidEndpointSliverTolerance infiniteParameter), Arrangement.buildWith [ segment ] tolerance minimumChord infiniteParameter)

[<Fact>]
let ``insertion_reports_tolerance_cluster_collapse_test`` () =
    Assert.Equal(
        Error(SegmentCollapsedToVertex 0),
        Arrangement.insertAtomicSegment Arrangement.empty (line 0.0 0.0 0.5 0.0) 1.0<length> 0.1<length>)

[<Fact>]
let ``two_endpoint_samples_use_enclosing_circle_midpoint_test`` () =
    let a = point 0.0 0.0
    let b1 = point 10.0 0.0
    let b2 = point 10.0000004 0.0
    let c = point 10.0 10.0
    let first =
        Arrangement.insertAtomicSegment Arrangement.empty (Line(a, b1)) tolerance minimumChord
        |> Result.defaultWith (failwithf "%A")
    let graph =
        Arrangement.insertAtomicSegment first (Line(b2, c)) tolerance minimumChord
        |> Result.defaultWith (failwithf "%A")
    let joined = graph.Vertices[1]
    Assert.Equal(2, joined.EndpointSamples.Length)
    Assert.True(Point.near 0.000000001<length> joined.Point (point 10.0000002 0.0))

[<Fact>]
let ``endpoint_cluster_center_is_independent_of_insertion_order_test`` () =
    let a = point 0.0 0.0
    let b = point 2.0 0.0
    let c = point 1.0 2.0
    let first = graphWithClusteredEndpoints [ a; b; c ] 2.0<length> |> Result.defaultWith (failwithf "%A")
    let second = graphWithClusteredEndpoints [ c; a; b ] 2.0<length> |> Result.defaultWith (failwithf "%A")
    let firstCenter = first.Vertices[1].Point
    let secondCenter = second.Vertices[1].Point
    Assert.Equal(point 1.0 0.75, firstCenter)
    Assert.Equal(firstCenter, secondCenter)

[<Fact>]
let ``exactly_equal_endpoint_samples_preserve_exact_vertex_test`` () =
    let endpoint = point 1.25 -3.5
    let graph = graphWithClusteredEndpoints [ endpoint; endpoint; endpoint ] tolerance |> Result.defaultWith (failwithf "%A")
    let joined = graph.Vertices[1]
    Assert.Equal(endpoint, joined.Point)
    Assert.Equal(3, joined.EndpointSamples.Length)

[<Fact>]
let ``build_with_rejects_negative_endpoint_sliver_tolerance_test`` () =
    let segment = line 0.0 0.0 10.0 0.0
    Assert.Equal(Error(InvalidEndpointSliverTolerance -0.001<parameter>), Arrangement.buildWith [ segment ] tolerance minimumChord -0.001<parameter>)

[<Fact>]
let ``validation_rejects_invalid_numeric_options_test`` () =
    Assert.Equal(Error(InvalidArrangementTolerance 0.0<length>), Arrangement.validate Arrangement.empty 0.0<length> minimumChord)
    Assert.Equal(Error(InvalidMinimumChord 0.0<length>), Arrangement.validate Arrangement.empty tolerance 0.0<length>)

[<Fact>]
let ``validation_rejects_vertex_sample_outside_official_tolerance_test`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let graph =
        { Arrangement.empty with
            Vertices =
                [ { Id = 0
                    Point = point 0.0 0.0
                    EndpointSamples = [ point -2.0 0.0; point 2.0 0.0 ] }
                  { Id = 1
                    Point = point 10.0 0.0
                    EndpointSamples = [ point 10.0 0.0 ] } ]
            Edges = [ edge 0 segment 0 1 ] }
    Assert.Equal(
        Error(VertexSampleOutsideTolerance(0, 4.0<length^2>, 1.0<length^2>)),
        Arrangement.validate graph 1.0<length> minimumChord)

[<Fact>]
let ``validation_rejects_noncanonical_vertex_center_test`` () =
    let segment = line 0.1 0.0 10.0 0.0
    let graph =
        { Arrangement.empty with
            Vertices =
                [ { Id = 0
                    Point = point 0.1 0.0
                    EndpointSamples = [ point 0.0 0.0 ] }
                  { Id = 1
                    Point = point 10.0 0.0
                    EndpointSamples = [ point 10.0 0.0 ] } ]
            Edges = [ edge 0 segment 0 1 ] }
    match Arrangement.validate graph 1.0<length> minimumChord with
    | Error(VertexCenterMismatch(0, distanceSquared)) ->
        Assert.True(abs (float distanceSquared - 0.01) < 0.000001)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``validation_rejects_vertex_without_endpoint_samples_test`` () =
    let segment = line 0.0 0.0 10.0 0.0
    let graph =
        { Arrangement.empty with
            Vertices =
                [ { Id = 0; Point = point 0.0 0.0; EndpointSamples = [] }
                  { Id = 1; Point = point 10.0 0.0; EndpointSamples = [ point 10.0 0.0 ] } ]
            Edges = [ edge 0 segment 0 1 ] }
    Assert.Equal(Error(VertexWithoutEndpointSamples 0), Arrangement.validate graph tolerance minimumChord)

[<Fact>]
let ``reversed_duplicate_increments_reverse_multiplicity_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 0.0 0.0 ]
    let edge = build.Graph.Edges |> List.exactlyOne
    Assert.Equal(1, edge.ForwardMultiplicity)
    Assert.Equal(1, edge.ReverseMultiplicity)

[<Fact>]
let ``short_chord_is_rejected_test`` () =
    Assert.Equal(
        Error(SegmentTooShort(0.000001<length>, minimumChord)),
        Arrangement.insertAtomicSegment Arrangement.empty (line 0.0 0.0 0.000001 0.0) tolerance minimumChord)

[<Fact>]
let ``csg_union_removes_interlocking_square_internal_edges_test`` () =
    let left = Path.singleton (square 0.0 0.0 10.0)
    let right = Path.singleton (square 5.0 5.0 10.0)
    let union = Csg.union left right Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.Single(union.Path.Subpaths) |> ignore
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 2.0 2.0) union.Path Nonzero)
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 7.0 7.0) union.Path Nonzero)
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 13.0 13.0) union.Path Nonzero)
    Assert.Equal(Ok Outside, WindingField.pathContainment (point 2.0 13.0) union.Path Nonzero)

[<Fact>]
let ``csg_union_does_not_cancel_opposite_operands_test`` () =
    let clockwise = square 0.0 0.0 10.0
    let counterclockwise = Subpath.reverse clockwise
    let union = Csg.union (Path.singleton clockwise) (Path.singleton counterclockwise) Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.Single(union.Path.Subpaths) |> ignore
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 5.0 5.0) union.Path Nonzero)

[<Fact>]
let ``csg_union_applies_requested_fill_rule_test`` () =
    let contour = square 0.0 0.0 10.0
    let doubled = Path.ofSubpaths [ contour; contour ]
    let empty = Path.empty
    let nonzero = Csg.union doubled empty Nonzero |> Result.defaultWith (failwithf "%A")
    let evenOdd = Csg.union doubled empty EvenOdd |> Result.defaultWith (failwithf "%A")
    Assert.Single(nonzero.Path.Subpaths) |> ignore
    Assert.Empty(evenOdd.Path.Subpaths)

[<Fact>]
let ``csg_union_pairs_filled_sectors_at_corner_pinch_test`` () =
    let first = square 0.0 0.0 10.0
    let second = square 10.0 10.0 10.0
    let union = Csg.union (Path.singleton first) (Path.singleton second) Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, union.Path.Subpaths.Length)
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 5.0 5.0) union.Path Nonzero)
    Assert.Equal(Ok Inside, WindingField.pathContainment (point 15.0 15.0) union.Path Nonzero)

[<Fact>]
let ``drawing_contains_edges_vertices_and_multiplicity_labels_test`` () =
    let graph =
        Arrangement.insertAtomicSegment Arrangement.empty (line 0.0 0.0 10.0 0.0) tolerance minimumChord
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(7, ArrangementDrawing.drawing graph |> List.length)

[<Fact>]
let ``subpath_direction_arrows_draws_one_arrow_per_segment_test`` () =
    let subpath =
        Subpath.assertCreate [
            line 0.0 0.0 10.0 0.0
            line 10.0 0.0 10.0 10.0
        ]
    Assert.Equal(2, ArrangementDrawing.subpathDirectionArrows subpath "red" |> List.length)
