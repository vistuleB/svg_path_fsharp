module SvgPath.Tests.FlattenArcOverlapTests

open SvgPath
open Xunit

let private unwrap value = Result.defaultWith (failwithf "%A") value
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private arc start radius sweep finish =
    Arc { Start=start; Radius=p radius radius; XAxisRotation=0.0<degree>
          LargeArc=false; Sweep=sweep; End=finish }
let private shortPair () =
    arc (p 59.76247350201103 59.7377048577821) 84.49938828627135 true (p 59.70692262458793 59.79322713698864),
    arc (p 59.76247342962466 59.73770478542024) 85.20624291591454 false (p 59.70695115012889 59.79325542571751)
let private options = { Intersections.defaultOptions with Tolerance=5e-7<length> }

// Flatten Boolean issue #3, call 4295; same reconciled fixtures as Gleam.
[<Fact>]
let fl006_endpoint_arc_overlap_does_not_hide_crossing () =
    let left,right = shortPair ()
    for a in [left;Segment.reverse left] do
        for b in [right;Segment.reverse right] do
            for a,b in [a,b;b,a] do
                Assert.Equal<Overlaps.SegmentOverlap list>([],Overlaps.segmentWith a b 5e-7<length> |> unwrap)
                let hit = Intersections.segmentWith a b options |> unwrap |> List.exactlyOne
                Assert.True(abs(hit.Point.X-59.73753072787354<length>)<1e-9<length>)
                Assert.True(abs(hit.Point.Y-59.76264755924401<length>)<1e-9<length>)

[<Fact>]
let fl006_arc_direction_guard_preserves_partial_reversed_overlap () =
    let left,_ = shortPair ()
    let part = Segment.between left 0.2<parameter> 0.8<parameter> |> unwrap
    for right in [part;Segment.reverse part] do
        for a,b in [left,right;right,left] do
            let overlap = Overlaps.segmentWith a b 5e-7<length> |> unwrap |> List.exactlyOne
            for t in [0.0;0.25;0.5;0.75;1.0] do
                let u=overlap.LeftFrom+t*(overlap.LeftTo-overlap.LeftFrom)
                let v=Overlaps.segmentOverlapRightParameter overlap u
                let p,q=Segment.point a u |> unwrap,Segment.point b v |> unwrap
                Assert.True(abs(p.X-q.X)<5e-7<length>)
                Assert.True(abs(p.Y-q.Y)<5e-7<length>)
            Assert.Equal(Error OverlappingSegments,Intersections.segmentWith a b options)

let private left_4295 () =
    [
      arc (p 59.70692262458793 59.79322713698864) 84.79252389141845 true (p 59.762664600825765 59.84855755939034)
      arc (p 59.762664600825765 59.84855755939034) 84.5779281026111 false (p 59.81821506960193 59.79303528018376)
      arc (p 59.81821506960193 59.79303528018376) 84.8710637077582 false (p 59.76247350201103 59.7377048577821)
      arc (p 59.76247350201103 59.7377048577821) 84.49938828627135 true (p 59.70692262458793 59.79322713698864)
    ] |> Subpath.create |> unwrap |> fun s -> Path.ofSubpaths [s]

let private right_4295 () =
    [
      arc (p 205.20624291591454 120.0) 85.20624291591454 false (p 120.0 34.793757084085456)
      arc (p 120.0 34.793757084085456) 85.20624291591454 false (p 34.793757084085456 119.99999999999999)
      arc (p 34.793757084085456 119.99999999999999) 85.20624291591454 false (p 119.99999999999999 205.20624291591454)
      arc (p 119.99999999999999 205.20624291591454) 85.20624291591454 false (p 205.20624291591454 120.0)
    ] |> Subpath.create |> unwrap |> fun s -> Path.ofSubpaths [s]

[<Fact>]
let fl006_boolean_keeps_interior_arc_crossing () =
    let path=Csg.intersectionPath (left_4295 ()) (right_4295 ()) EvenOdd |> unwrap
    Assert.True(abs(float(Area.signedPath path)-0.006167841542276564)<1e-8)
    Assert.True(Path.subpaths path |> List.exists (fun s ->
        Subpath.segments s |> List.exists (fun segment ->
            let p=Segment.start segment
            abs(p.X-59.73753072787354<length>)<1e-7<length>
            && abs(p.Y-59.76264755924401<length>)<1e-7<length>)))
