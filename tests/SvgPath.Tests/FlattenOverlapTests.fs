module SvgPath.Tests.FlattenOverlapTests

open SvgPath
open Xunit

let private unwrap value = Result.defaultWith (failwithf "%A") value
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private roundedLines () =
    Line(p 1336017144.7222269 514632.0777871349,p 1336017149.883367 514641.78073088516),
    Line(p 1336017144.722227 514632.077786993,p 1336017149.8833673 514641.780730718)
let private nearEndpointLines () = Line(p 0. 0.,p 1. 1.),Line(p -2e-7 2e-7,p 1. 0.999997)
let private orientations left right =
    [for a in [left;Segment.reverse left] do
        for b in [right;Segment.reverse right] do yield! [a,b;b,a]]
let private options = {Intersections.defaultOptions with Tolerance=5e-7<length>}

[<Fact>]
let large_coordinate_line_overlap_uses_endpoint_bound () =
    let left,right=roundedLines ()
    for a,b in orientations left right do
        let overlap=Overlaps.segmentWith a b 5e-7<length> |> unwrap |> List.exactlyOne
        Assert.Equal(0.0<parameter>,overlap.LeftFrom)
        Assert.Equal(1.0<parameter>,overlap.LeftTo)
        Assert.True((overlap.RightFrom,overlap.RightTo)=(0.0<parameter>,1.0<parameter>)
                    || (overlap.RightFrom,overlap.RightTo)=(1.0<parameter>,0.0<parameter>))
        Assert.Equal(Error OverlappingSegments,Intersections.segmentWith a b options)

[<Fact>]
let large_coordinate_line_overlap_respects_tighter_tolerance () =
    let a,b=roundedLines ()
    for a,b in [a,b;b,a] do
        Assert.Empty(Overlaps.segmentWith a b 1e-7<length> |> unwrap)

[<Fact>]
let crossing_lines_are_not_continuous_overlap () =
    let a,b=Line(p 0. 0.,p 10. 0.),Line(p 0. -1.,p 10. 1.)
    Assert.Empty(Overlaps.segmentWith a b 5e-7<length> |> unwrap)
    let hit=Intersections.segment a b |> unwrap |> List.exactlyOne
    Assert.Equal(0.5<parameter>,hit.LeftT)
    Assert.Equal(0.5<parameter>,hit.RightT)

[<Fact>]
let near_endpoint_reverse_correspondence_preserves_crossing () =
    let a,b=nearEndpointLines ()
    for a,b in orientations a b do
        Assert.Empty(Overlaps.segmentWith a b 5e-7<length> |> unwrap)
        Assert.True(Intersections.segmentWith a b options |> unwrap |> List.exists (fun hit ->
            hit.Point.X>0.11764<length> && hit.Point.X<0.11765<length>
            && hit.Point.Y>0.11764<length> && hit.Point.Y<0.11765<length>))

[<Fact>]
let short_genuine_line_overlap_preserves_reversed_traversal () =
    let a,b=Line(p 0. 0.,p 1. 0.),Line(p 2e-7 0.,p 0. 0.)
    let hit=Overlaps.segmentWith a b 5e-7<length> |> unwrap |> List.exactlyOne
    Assert.True(hit.LeftTo>hit.LeftFrom && hit.RightFrom>hit.RightTo)

[<Fact>]
let near_endpoint_overlap_does_not_hide_arrangement_crossing () =
    let a,b=nearEndpointLines ()
    for segments in [[a;b];[b;a]] do
        let built=Arrangement.buildWith segments 1e-6<length> 1e-9<length> 0.0<parameter> |> unwrap
        Assert.Equal(3,built.Graph.Edges.Length)
        Assert.True(built.Graph.Vertices |> List.exists (fun vertex ->
            vertex.Point.X>0.11764<length> && vertex.Point.X<0.11765<length>
            && vertex.Point.Y>0.11764<length> && vertex.Point.Y<0.11765<length>))
