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

let private baseLine () = Line(point 0.0 0.0, point 10.0 0.0)

let private baseArc largeArc sweep =
    Arc
        { Start = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = 0.0<degree>
          LargeArc = largeArc
          Sweep = sweep
          End = point 10.0 0.0 }

let private intersectionOptions tolerance =
    { Tolerance = tolerance
      MaxDepth = 48
      ParameterSnap = DecimalParameterSnap 7 }

let private assertOverlapContract left right expectedOverlap =
    let tolerance = 1.0e-6<length>
    let overlaps = Overlaps.segmentWith left right tolerance |> Result.defaultWith (failwithf "%A")
    Assert.Equal(expectedOverlap, not (List.isEmpty overlaps))
    Assert.Equal(
        expectedOverlap,
        Intersections.segmentWith left right (intersectionOptions tolerance) = Error OverlappingSegments)

let private assertFullOverlap left right expectedRightFrom expectedRightTo =
    let overlap = Overlaps.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.0) overlap.LeftFrom
    assertParameterNear (parameter 1.0) overlap.LeftTo
    assertParameterNear (parameter expectedRightFrom) overlap.RightFrom
    assertParameterNear (parameter expectedRightTo) overlap.RightTo
    Assert.True(Point.near 1.0e-9<length> overlap.Start (point 0.0 0.0))
    Assert.True(Point.near 1.0e-9<length> overlap.Finish (point 10.0 0.0))

[<Fact>]
let ``segment overlap and intersection agree on partial line`` () =
    assertOverlapContract (baseLine ()) (Line(point 5.0 0.0, point 15.0 0.0)) true

[<Fact>]
let ``segment overlap and intersection agree on semantic arc`` () =
    assertOverlapContract (baseArc false true) (baseArc true true) true

[<Fact>]
let ``semantic arc overlap survives nine decimal tolerance`` () =
    let tolerance = 1.0e-9<length>
    Assert.Single(Overlaps.segmentWith (baseArc false true) (baseArc true true) tolerance |> Result.defaultWith (failwithf "%A")) |> ignore
    Assert.Equal(
        Error OverlappingSegments,
        Intersections.segmentWith (baseArc false true) (baseArc true true) (intersectionOptions tolerance))

[<Fact>]
let ``near coincident line overlap survives endpoint parameter dust`` () =
    let tolerance = 1.0e-9<length>
    let left = Line(point 17.443943950536976 4.1250002, point 17.044995 4.1250002)
    let right = Line(point 17.443943950536976 4.125000200000001, point 17.044995 4.1250002)
    let overlap = Overlaps.segmentWithSamples left right tolerance 7 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.0) overlap.LeftFrom
    assertParameterNear (parameter 1.0) overlap.LeftTo
    assertParameterNear (parameter 0.0) overlap.RightFrom
    assertParameterNear (parameter 1.0) overlap.RightTo
    Assert.Equal(Error OverlappingSegments, Intersections.segmentWith left right (intersectionOptions tolerance))

[<Fact>]
let ``segment overlap and intersection agree on endpoint touch`` () =
    assertOverlapContract (baseLine ()) (Line(point 10.0 0.0, point 10.0 10.0)) false

[<Fact>]
let ``segment overlap and intersection agree on disjoint segments`` () =
    assertOverlapContract (baseLine ()) (Line(point 0.0 2.0, point 10.0 2.0)) false

[<Fact>]
let ``endpoint projection overlap finds partial line overlap`` () =
    let overlap = Overlaps.segmentWithSamples (baseLine ()) (Line(point 3.0 0.0, point 7.0 0.0)) 1.0e-6<length> 5 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.3) overlap.LeftFrom
    assertParameterNear (parameter 0.7) overlap.LeftTo
    assertParameterNear (parameter 0.0) overlap.RightFrom
    assertParameterNear (parameter 1.0) overlap.RightTo

[<Fact>]
let ``endpoint projection overlap preserves reversed line overlap`` () =
    let overlap = Overlaps.segmentWithSamples (baseLine ()) (Line(point 7.0 0.0, point 3.0 0.0)) 1.0e-6<length> 5 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 1.0) overlap.RightFrom
    assertParameterNear (parameter 0.0) overlap.RightTo

[<Fact>]
let ``endpoint projection overlap finds semantically equal arcs`` () =
    let overlap = Overlaps.segmentWithSamples (baseArc false true) (baseArc true true) 1.0e-6<length> 9 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.0) overlap.LeftFrom
    assertParameterNear (parameter 1.0) overlap.LeftTo
    assertParameterNear (parameter 0.0) overlap.RightFrom
    assertParameterNear (parameter 1.0) overlap.RightTo

[<Fact>]
let ``endpoint projection overlap rejects opposite semicircles`` () =
    Assert.Equal(Ok [], Overlaps.segmentWithSamples (baseArc false true) (baseArc false false) 1.0e-6<length> 9)

[<Fact>]
let ``overlap detection rejects negative tolerance`` () =
    Assert.Equal(Error(InvalidOverlapTolerance -1.0e-6<length>), Overlaps.segmentWith (baseLine ()) (baseLine ()) -1.0e-6<length>)

[<Fact>]
let ``endpoint projection overlap rejects nonpositive samples`` () =
    Assert.Equal(Error(InvalidOverlapSamples 0), Overlaps.segmentWithSamples (baseLine ()) (baseLine ()) 1.0e-6<length> 0)

[<Fact>]
let ``identical line is one full overlap`` () =
    assertFullOverlap (baseLine ()) (baseLine ()) 0.0 1.0

[<Fact>]
let ``geometric tolerance is not used as parameter span`` () =
    let overlap = Overlaps.segmentWithSamples (baseLine ()) (baseLine ()) 2.0<length> 5 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.0) overlap.LeftFrom
    assertParameterNear (parameter 1.0) overlap.LeftTo

[<Fact>]
let ``strict tolerance merges full arc overlap proposals`` () =
    let overlap = Overlaps.segmentWithSamples (baseArc false true) (baseArc false true) 1.0e-9<length> 9 |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear (parameter 0.0) overlap.LeftFrom
    assertParameterNear (parameter 1.0) overlap.LeftTo

[<Fact>]
let ``reversed line is one full overlap`` () =
    assertFullOverlap (baseLine ()) (Segment.reverse (baseLine ())) 1.0 0.0

[<Fact>]
let ``identical quadratic is one full overlap`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    assertFullOverlap segment segment 0.0 1.0

[<Fact>]
let ``reversed quadratic is one full overlap`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    assertFullOverlap segment (Segment.reverse segment) 1.0 0.0

[<Fact>]
let ``identical cubic is one full overlap`` () =
    let segment = CubicBezier(point 0.0 0.0, point 2.0 9.0, point 8.0 -9.0, point 10.0 0.0)
    assertFullOverlap segment segment 0.0 1.0

[<Fact>]
let ``reversed cubic is one full overlap`` () =
    let segment = CubicBezier(point 0.0 0.0, point 2.0 9.0, point 8.0 -9.0, point 10.0 0.0)
    assertFullOverlap segment (Segment.reverse segment) 1.0 0.0

[<Fact>]
let ``identical arc is one full overlap`` () =
    let segment = baseArc false true
    assertFullOverlap segment segment 0.0 1.0

[<Fact>]
let ``reversed arc is one full overlap`` () =
    let segment = baseArc false true
    assertFullOverlap segment (Segment.reverse segment) 1.0 0.0

[<Fact>]
let ``non affinely parameterized line cubics are rejected`` () =
    let linearSpeed = CubicBezier(point 0.0 0.0, point (1.0 / 3.0) 0.0, point (2.0 / 3.0) 0.0, point 1.0 0.0)
    let cubicSpeed = CubicBezier(point 0.0 0.0, point 0.0 0.0, point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error NonAffineOverlapCorrespondence, Overlaps.segment linearSpeed cubicSpeed)
    Assert.Equal(Error NonAffineOverlapCorrespondence, Intersections.segment linearSpeed cubicSpeed)
    Assert.Equal(Error NonAffineOverlapCorrespondence, Encounters.segment linearSpeed cubicSpeed)

[<Fact>]
let ``segment overlap exposes affine parameter correspondence`` () =
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
let ``segment subpath overlap preserves piecewise correspondence`` () =
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
let ``disconnected subpath overlaps remain separate`` () =
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
let ``path overlap preserves subpath correspondence and indices`` () =
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
