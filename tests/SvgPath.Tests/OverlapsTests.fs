module SvgPath.Tests.OverlapsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

let private polyline xs =
    let points = xs |> List.map (fun x -> point x 0.0)
    Subpath.polyline points |> Result.defaultWith (failwithf "%A")

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
let ``subpath overlap maps one segment to two segments`` () =
    let left = polyline [ 0.0; 10.0 ]
    let right = polyline [ 0.0; 5.0; 10.0 ]
    let overlap = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = overlap.Pieces[0], overlap.Pieces[1]
    Assert.Equal((0, 0), (first.LeftSegmentIndex, first.RightSegmentIndex))
    Assert.Equal((0, 1), (second.LeftSegmentIndex, second.RightSegmentIndex))
    assertParameterNear (parameter 0.5) (Overlaps.segmentOverlapRightParameter first.Correspondence (parameter 0.25))
    assertParameterNear (parameter 0.5) (Overlaps.segmentOverlapRightParameter second.Correspondence (parameter 0.75))
    Assert.Equal(Some { SegmentIndex = 0; T = parameter 0.0 }, Overlaps.subpathOverlapLeftStart overlap)
    Assert.Equal(Some { SegmentIndex = 0; T = parameter 1.0 }, Overlaps.subpathOverlapLeftEnd overlap)
    Assert.Equal(Some { SegmentIndex = 0; T = parameter 0.0 }, Overlaps.subpathOverlapRightStart overlap)
    Assert.Equal(Some { SegmentIndex = 1; T = parameter 1.0 }, Overlaps.subpathOverlapRightEnd overlap)

[<Fact>]
let ``subpath overlap maps two segments to one segment`` () =
    let left = polyline [ 0.0; 5.0; 10.0 ]
    let right = polyline [ 0.0; 10.0 ]
    let overlap = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = overlap.Pieces[0], overlap.Pieces[1]
    assertParameterNear (parameter 0.25) (Overlaps.segmentOverlapRightParameter first.Correspondence (parameter 0.5))
    assertParameterNear (parameter 0.75) (Overlaps.segmentOverlapRightParameter second.Correspondence (parameter 0.5))

[<Fact>]
let ``subpath overlap exact lookup accepts internal endpoint aliases`` () =
    let left = polyline [ 0.0; 5.0; 10.0 ]
    let right = polyline [ 0.0; 10.0 ]
    let overlap = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let expectedRight = Ok(Some { SegmentIndex = 0; T = parameter 0.5 })
    Assert.Equal(expectedRight, Overlaps.subpathOverlapRightParameter overlap { SegmentIndex = 0; T = parameter 1.0 } left right)
    Assert.Equal(expectedRight, Overlaps.subpathOverlapRightParameter overlap { SegmentIndex = 1; T = parameter 0.0 } left right)
    Assert.Equal(
        Ok(Some { SegmentIndex = 1; T = parameter 0.0 }),
        Overlaps.subpathOverlapLeftParameter overlap { SegmentIndex = 0; T = parameter 0.5 } left right)

[<Fact>]
let ``subpath overlap exact lookup accepts closed seam alias`` () =
    let closed =
        Subpath.polyline [ point 0.0 0.0; point 1.0 0.0; point 1.0 1.0; point 0.0 1.0; point 0.0 0.0 ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let correspondence: SegmentOverlap =
        { LeftFrom = parameter 0.5
          LeftTo = parameter 1.0
          RightFrom = parameter 0.0
          RightTo = parameter 1.0
          Start = point 0.0 0.5
          Finish = point 0.0 0.0 }
    let overlap =
        { Start = correspondence.Start
          Finish = correspondence.Finish
          Pieces = [ { LeftSegmentIndex = 3; RightSegmentIndex = 0; Correspondence = correspondence } ] }
    Assert.Equal(
        Ok(Some { SegmentIndex = 1; T = parameter 0.0 }),
        Overlaps.subpathOverlapRightParameter overlap { SegmentIndex = 0; T = parameter 0.0 } closed closed)

[<Fact>]
let ``subpath overlap exact lookup rejects address outside overlap`` () =
    let left = polyline [ 0.0; 5.0; 10.0 ]
    let right = polyline [ 0.0; 4.0 ]
    let overlap = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(
        Ok None,
        Overlaps.subpathOverlapRightParameter overlap { SegmentIndex = 1; T = parameter 0.0 } left right)

[<Fact>]
let ``subpath overlap preserves reversed piecewise traversal`` () =
    let left = polyline [ 0.0; 10.0 ]
    let right = polyline [ 10.0; 5.0; 0.0 ]
    let overlap = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = overlap.Pieces[0], overlap.Pieces[1]
    Assert.Equal((1, parameter 1.0, parameter 0.0), (first.RightSegmentIndex, first.Correspondence.RightFrom, first.Correspondence.RightTo))
    Assert.Equal((0, parameter 1.0, parameter 0.0), (second.RightSegmentIndex, second.Correspondence.RightFrom, second.Correspondence.RightTo))
    Assert.Equal(Some { SegmentIndex = 1; T = parameter 1.0 }, Overlaps.subpathOverlapRightStart overlap)
    Assert.Equal(Some { SegmentIndex = 0; T = parameter 0.0 }, Overlaps.subpathOverlapRightEnd overlap)

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
        Subpath.create
            [ Line(point 0.0 0.0, point 4.0 0.0)
              Line(point 4.0 0.0, point 4.0 2.0)
              Line(point 4.0 2.0, point 6.0 2.0)
              Line(point 6.0 2.0, point 6.0 0.0)
              Line(point 6.0 0.0, point 10.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let baseline = polyline [ 0.0; 10.0 ]
    let overlaps = Overlaps.subpath baseline lifted |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length overlaps)

[<Fact>]
let ``path overlap retains source subpath indices and maps addresses`` () =
    let left = Path.ofSubpaths [ polyline [ 20.0; 30.0 ]; polyline [ 0.0; 10.0 ] ]
    let right = Path.ofSubpaths [ polyline [ 0.0; 5.0; 10.0 ] ]
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
        Overlaps.pathWith (Path.singleton subpath) (Path.singleton subpath) -1.0<length>)
