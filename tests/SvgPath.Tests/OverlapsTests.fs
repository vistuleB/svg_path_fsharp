module SvgPath.Tests.OverlapsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

let private polyline xs =
    let points = xs |> List.map (fun x -> point x 0.0)
    { Start = List.head points
      Segments = points |> List.pairwise |> List.map Line
      Closed = false }

let private assertParameterNear expected actual =
    Assert.True(abs (expected - actual) <= parameter 1.0e-9, $"expected {expected}, got {actual}")

[<Fact>]
let ``segment overlap maps parameters in both directions`` () =
    let overlap: SegmentOverlap =
        { LeftFrom = parameter 0.2
          LeftTo = parameter 0.8
          RightFrom = parameter 0.9
          RightTo = parameter 0.3
          Start = point 2.0 0.0
          Finish = point 8.0 0.0 }
    assertParameterNear (parameter 0.6) (Overlaps.segmentOverlapRightParameter overlap (parameter 0.5))
    assertParameterNear (parameter 0.5) (Overlaps.segmentOverlapLeftParameter overlap (parameter 0.6))

[<Fact>]
let ``segment-subpath overlap retains piecewise correspondence`` () =
    let segment = Line(point 0.0 0.0, point 10.0 0.0)
    let subpath = polyline [ 0.0; 5.0; 10.0 ]
    let overlap =
        Overlaps.segmentSubpath segment subpath
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(2, List.length overlap.Pieces)
    Assert.True([ 0; 1 ] = (overlap.Pieces |> List.map _.SubpathSegmentIndex))
    Assert.Equal(Some(parameter 0.0), Overlaps.segmentSubpathOverlapSegmentStart overlap)
    Assert.Equal(Some(parameter 1.0), Overlaps.segmentSubpathOverlapSegmentEnd overlap)
    Assert.Equal(
        Ok(Some { SegmentIndex = 1; T = parameter 0.5 }),
        Overlaps.segmentSubpathOverlapSubpathParameter overlap (parameter 0.75) segment subpath)
    Assert.Equal(
        Ok(Some(parameter 0.75)),
        Overlaps.segmentSubpathOverlapSegmentParameter
            overlap
            { SegmentIndex = 1; T = parameter 0.5 }
            segment
            subpath)

[<Fact>]
let ``subpath overlap merges connected pieces and canonicalizes aliases`` () =
    let left = polyline [ 0.0; 5.0; 10.0 ]
    let right = polyline [ 0.0; 2.5; 7.5; 10.0 ]
    let overlap =
        Overlaps.subpath left right
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(4, List.length overlap.Pieces)
    Assert.Equal(Some { SegmentIndex = 0; T = parameter 0.0 }, Overlaps.subpathOverlapLeftStart overlap)
    Assert.Equal(Some { SegmentIndex = 1; T = parameter 1.0 }, Overlaps.subpathOverlapLeftEnd overlap)
    Assert.Equal(
        Ok(Some { SegmentIndex = 2; T = parameter 0.0 }),
        Overlaps.subpathOverlapRightParameter
            overlap
            { SegmentIndex = 1; T = parameter 0.5 }
            left
            right)
    Assert.Equal(
        Ok(Some { SegmentIndex = 0; T = parameter 0.5 }),
        Overlaps.subpathOverlapLeftParameter
            overlap
            { SegmentIndex = 0; T = parameter 1.0 }
            left
            right)

[<Fact>]
let ``disconnected coincident portions remain separate overlaps`` () =
    let left = polyline [ 0.0; 4.0; 4.0; 6.0; 6.0; 10.0 ]
    let lifted =
        { left with
            Segments =
                [ Line(point 0.0 0.0, point 4.0 0.0)
                  Line(point 4.0 0.0, point 4.0 2.0)
                  Line(point 4.0 2.0, point 6.0 2.0)
                  Line(point 6.0 2.0, point 6.0 0.0)
                  Line(point 6.0 0.0, point 10.0 0.0) ] }
    let baseline = polyline [ 0.0; 10.0 ]
    let overlaps = Overlaps.subpath baseline lifted |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length overlaps)

[<Fact>]
let ``path overlap retains source subpath indices and maps addresses`` () =
    let left = { Subpaths = [ polyline [ 20.0; 30.0 ]; polyline [ 0.0; 10.0 ] ] }
    let right = { Subpaths = [ polyline [ 0.0; 5.0; 10.0 ] ] }
    let overlap =
        Overlaps.path left right
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(1, overlap.LeftSubpathIndex)
    Assert.Equal(0, overlap.RightSubpathIndex)
    Assert.Equal(
        Ok(Some
            { SubpathIndex = 0
              At = { SegmentIndex = 1; T = parameter 0.5 } }),
        Overlaps.pathOverlapRightParameter
            overlap
            { SubpathIndex = 1
              At = { SegmentIndex = 0; T = parameter 0.75 } }
            left
            right)
    Assert.Equal(
        Ok None,
        Overlaps.pathOverlapRightParameter
            overlap
            { SubpathIndex = 0
              At = { SegmentIndex = 0; T = parameter 0.5 } }
            left
            right)

[<Fact>]
let ``invalid geometric tolerance propagates through higher-level APIs`` () =
    let subpath = polyline [ 0.0; 10.0 ]
    Assert.Equal(
        Error(InvalidOverlapTolerance -1.0<length>),
        Overlaps.subpathWith subpath subpath -1.0<length>)
    Assert.Equal(
        Error(InvalidOverlapTolerance -1.0<length>),
        Overlaps.pathWith { Subpaths = [ subpath ] } { Subpaths = [ subpath ] } -1.0<length>)
