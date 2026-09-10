module SvgPath.Tests.CuspSmallLoopTests

open SvgPath
open Xunit

let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private pair =
    [ QuadraticBezier(p 0.0 0.0, p 2.0 3.0, p 4.0 0.0)
      Line(p 4.0 0.0, p 0.0 1.0) ]
let private unwrap = Result.defaultWith (failwithf "%A")
let private build segments = Arrangement.buildWith segments 2e-9<length> 2e-9<length> 0.0001<parameter> |> unwrap

[<Fact>]
let ``embedded_culling_selects_only_opposite_reversed_adjacent_loop_test`` () =
    let b = build pair
    let ids = Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [true; false] false |> unwrap
    Assert.Equal(2, List.length ids)
    Assert.True(Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [false; true] false = Ok ids)
    Assert.True(Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [true; true] false = Ok [])
    Assert.True(Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [false; false] false = Ok [])

[<Fact>]
let ``embedded_culling_follows_all_graph_splits_inside_the_loop_test`` () =
    let b = build (pair @ [Line(p 3.8 -2.0, p 3.8 3.0)])
    let ids = Offset.cuspSmallLoopEdges b.Graph (List.take 2 b.SegmentImages) [true; false] false |> unwrap
    Assert.Equal(4, List.length ids)
    Assert.True((List.last b.SegmentImages).Edges |> List.forall (fun edge -> not (List.contains edge.EdgeId ids)))

[<Fact>]
let ``embedded_culling_checks_wraparound_only_for_closed_input_test`` () =
    let b = build (List.rev pair)
    Assert.True(Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [false; true] false = Ok [])
    let ids = Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [false; true] true |> unwrap
    Assert.Equal(2, List.length ids)

[<Fact>]
let ``embedded_culling_ignores_ordinary_shared_endpoint_test`` () =
    let b = build [Line(p 0.0 0.0, p 2.0 2.0); Line(p 2.0 2.0, p 4.0 0.0)]
    Assert.True(Offset.cuspSmallLoopEdges b.Graph b.SegmentImages [true; false] false = Ok [])
