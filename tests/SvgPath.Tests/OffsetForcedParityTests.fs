module SvgPath.Tests.OffsetForcedParityTests

open SvgPath
open Xunit

let private tolerance = 0.000001<length>
let private minimumChord = 0.00001<length>
let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private line ax ay bx by = Line(point ax ay, point bx by)

let private buildSegments segments =
    Arrangement.buildWith segments tolerance minimumChord 0.0<parameter>
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``forced_parity_reduces_unique_edge_without_mutating_graph_test`` () =
    let graph =
        buildSegments
            [ line 0.0 0.0 10.0 0.0
              line 0.0 0.0 10.0 0.0 ]
        |> _.Graph
    let originalEdge = graph.Edges.Head
    Assert.Equal(2, originalEdge.ForwardMultiplicity)
    let startVertex = graph.Vertices |> List.find (fun vertex -> vertex.Point = point 0.0 0.0)
    let endVertex = graph.Vertices |> List.find (fun vertex -> vertex.Point = point 10.0 0.0)
    let capacities =
        Offset.forcedParityCapacities
            graph
            [ RequiredVertexParity(startVertex.Id, 1)
              RequiredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, (Assert.Single capacities).Capacity)
    Assert.Equal(2, originalEdge.ForwardMultiplicity)

[<Fact>]
let ``forced_parity_reports_capacity_infeasibility_test`` () =
    let graph = buildSegments [ line 0.0 0.0 1.0 0.0 ] |> _.Graph
    let capacities = graph.Edges |> List.map (fun edge -> { EdgeId = edge.Id; Capacity = 0 })
    match Offset.forcedParityCapacitiesWith graph capacities [ RequiredVertexParity(0, 1) ] with
    | Error(ForcedParityInfeasible 0) -> ()
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``forced_parity_reports_unresolved_diamond_choice_test`` () =
    let source = point 0.0 0.0
    let upper = point 5.0 -5.0
    let lower = point 5.0 5.0
    let sink = point 10.0 0.0
    let build = buildSegments [ Line(source, upper); Line(upper, sink); Line(source, lower); Line(lower, sink) ]
    let sourceVertex = build.Graph.Vertices |> List.find (fun vertex -> vertex.Point = source)
    let sinkVertex = build.Graph.Vertices |> List.find (fun vertex -> vertex.Point = sink)
    match Offset.forcedParityCapacities build.Graph [ RequiredVertexParity(sourceVertex.Id, 1); RequiredVertexParity(sinkVertex.Id, 1) ] with
    | Error(ForcedParityAmbiguous vertices) -> Assert.Equal(2, vertices.Length)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``forced_parity_reduces_unique_edge_at_higher_threshold_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 5.0 10.0; line 5.0 10.0 0.0 0.0 ]
    let first, second, third = build.Graph.Edges[0], build.Graph.Edges[1], build.Graph.Edges[2]
    let reduced =
        Offset.forcedParityCapacitiesWith
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
        Offset.forcedParityCapacities first.Graph [ RequiredVertexParity(startVertex.Id, 0); PreferredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0, isolated.Head.Capacity)

    let doubled = buildSegments [ line; line ]
    let startVertex, endVertex = doubled.Graph.Vertices[0], doubled.Graph.Vertices[1]
    let preserved =
        Offset.forcedParityCapacities doubled.Graph [ PreferredVertexParity(startVertex.Id, 1); PreferredVertexParity(endVertex.Id, 1) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1, preserved.Head.Capacity)

[<Fact>]
let ``forced_parity_sums_forward_and_reverse_capacity_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 10.0 0.0 0.0 0.0 ]
    let assignment = Offset.forcedParityCapacities build.Graph [] |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(2, assignment.Capacity)

[<Fact>]
let ``forced_parity_accepts_explicit_initial_capacities_test`` () =
    let build = buildSegments [ line 0.0 0.0 10.0 0.0; line 0.0 0.0 10.0 0.0 ]
    let edge = build.Graph.Edges.Head
    let zero =
        Offset.forcedParityCapacitiesWith build.Graph [ { EdgeId = edge.Id; Capacity = 0 } ] []
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(0, zero.Capacity)

    let startVertex, endVertex = build.Graph.Vertices[0], build.Graph.Vertices[1]
    let reduced =
        Offset.forcedParityCapacitiesWith
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
        Offset.forcedParityCapacities build.Graph [ RequiredVertexParity(vertex.Id, 0); RequiredVertexParity(vertex.Id, 1) ])
    Assert.Equal(Error(ForcedParityMissingVertex 999), Offset.forcedParityCapacities build.Graph [ RequiredVertexParity(999, 0) ])
    Assert.Equal(Error(ForcedParityInvalidVertexParity(vertex.Id, 2)), Offset.forcedParityCapacities build.Graph [ RequiredVertexParity(vertex.Id, 2) ])
