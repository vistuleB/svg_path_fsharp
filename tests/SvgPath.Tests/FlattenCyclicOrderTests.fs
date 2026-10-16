module SvgPath.Tests.FlattenCyclicOrderTests

open SvgPath
open Xunit

let private unwrap value = Result.defaultWith (failwithf "%A") value
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private check center ends expected =
    for local in [false;true] do
        let center,ends = if local then p 0. 0.,List.map (Point.displacement center) ends else center,ends
        for reversed in [false;true] do
            let vertices : Arrangement.ArrangementVertex list =
                center::ends |> List.mapi (fun id point -> {Id=id; Point=point; EndpointSamples=[point]})
            let edges : Arrangement.ArrangementEdge list =
                ends |> List.mapi (fun i finish ->
                    let segment = Line(center,finish) |> fun s -> if reversed then Segment.reverse s else s
                    {Id=i; Segment=segment; Bounds=Segment.boundingBox segment |> unwrap
                     StartVertex=(if reversed then i+1 else 0); EndVertex=(if reversed then 0 else i+1)
                     ForwardMultiplicity=1; ReverseMultiplicity=0})
            let graph : Arrangement.ArrangementGraph = {Vertices=vertices; Edges=edges; CyclicOrders=[]}
            let actual=Arrangement.vertexCyclicOrderWith graph 0 1e-6<length> 3 |> unwrap
                       |> List.collect id |> List.map (fun edge -> edge.EdgeId)
            Assert.Equal<int list>(expected,actual)

[<Fact>]
let large_coordinate_fan_uses_local_sampling () =
    check (p 1286232330.342853 54685500.32193941)
        [p 1286198103.4762266 54821391.148841284;p 1319996203.9438455 457619.7648851394;p 1319996203.9439175 457619.7647740394] [0;1;2]
[<Fact>]
let fl003_close_rays_keep_order () =
    check (p 976070000. 221360000.)
        [p 976070737.1913013 221360000.;p 966175296.766696 251057267.55705994;p 961030000. 266500000.] [0;1;2]
[<Fact>]
let fl003_wrapped_fan_keeps_order () =
    check (p 975897619.6303852 232028812.02494428)
        [p 975950000. 231876924.60976407;p 967950526.5044882 255073006.67197832;p 963110483.3545867 269107685.0983405] [1;2;0]
