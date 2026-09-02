module SvgPath.Tests.OverlapDetectionTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private tolerance = 1.0e-6<length>

let private line () = Line(point 0.0 0.0, point 10.0 0.0)

let private arc () =
    Arc
        { Start = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = Degree.fromFloat 0.0
          LargeArc = false
          Sweep = true
          End = point 10.0 0.0 }

let private assertParameterNear expected actual =
    Assert.True(abs (expected - actual) <= 1.0e-9<parameter>, $"expected {expected}, got {actual}")

let private assertFullOverlap left right expectedRightFrom expectedRightTo =
    let overlap =
        OverlapDetection.detect left right tolerance
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.0<parameter> overlap.LeftFrom
    assertParameterNear 1.0<parameter> overlap.LeftTo
    assertParameterNear expectedRightFrom overlap.RightFrom
    assertParameterNear expectedRightTo overlap.RightTo
    Assert.Equal(Segment.start left, overlap.Start)
    Assert.Equal(Segment.finish left, overlap.Finish)

let private intersectionOptions tolerance =
    { Intersections.defaultOptions with
        Tolerance = tolerance
        MaxDepth = 48
        ParameterSnap = DecimalParameterSnap 7 }

let private assertOverlapContract left right expectedOverlap =
    let overlaps =
        OverlapDetection.detect left right tolerance
        |> Result.defaultWith (failwithf "%A")
    let intersection = Intersections.segmentWith left right (intersectionOptions tolerance)
    Assert.Equal(expectedOverlap, not (List.isEmpty overlaps))
    Assert.Equal(expectedOverlap, (intersection = Error OverlappingSegments))

[<Fact>]
let ``segment overlap and intersection agree on partial line`` () =
    let right = Line(point 5.0 0.0, point 15.0 0.0)
    assertOverlapContract (line ()) right true

[<Fact>]
let ``segment overlap and intersection agree on semantic arc`` () =
    let sameGeometry =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = true
              Sweep = true
              End = point 10.0 0.0 }
    assertOverlapContract (arc ()) sameGeometry true

[<Fact>]
let ``semantic arc overlap survives nine decimal tolerance`` () =
    let strictTolerance = 1.0e-9<length>
    let sameGeometry =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = true
              Sweep = true
              End = point 10.0 0.0 }
    let overlaps =
        OverlapDetection.detect (arc ()) sameGeometry strictTolerance
        |> Result.defaultWith (failwithf "%A")
    Assert.Single(overlaps) |> ignore
    Assert.Equal(
        Error OverlappingSegments,
        Intersections.segmentWith (arc ()) sameGeometry (intersectionOptions strictTolerance))

[<Fact>]
let ``near coincident line overlap survives endpoint parameter dust`` () =
    let strictTolerance = 1.0e-9<length>
    let left = Line(point 17.443943950536976 4.1250002, point 17.044995 4.1250002)
    let right = Line(point 17.443943950536976 4.125000200000001, point 17.044995 4.1250002)
    let overlap =
        OverlapDetection.detectWithSamples left right strictTolerance 7
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.0<parameter> overlap.LeftFrom
    assertParameterNear 1.0<parameter> overlap.LeftTo
    assertParameterNear 0.0<parameter> overlap.RightFrom
    assertParameterNear 1.0<parameter> overlap.RightTo
    Assert.Equal(
        Error OverlappingSegments,
        Intersections.segmentWith left right (intersectionOptions strictTolerance))

[<Fact>]
let ``segment overlap and intersection agree on endpoint touch`` () =
    let right = Line(point 10.0 0.0, point 10.0 10.0)
    assertOverlapContract (line ()) right false

[<Fact>]
let ``segment overlap and intersection agree on disjoint segments`` () =
    let right = Line(point 0.0 2.0, point 10.0 2.0)
    assertOverlapContract (line ()) right false

[<Fact>]
let ``partial line overlap carries affine parameter correspondence`` () =
    let right = Line(point 3.0 0.0, point 7.0 0.0)
    let overlap =
        OverlapDetection.detectWithSamples (line ()) right tolerance 5
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.3<parameter> overlap.LeftFrom
    assertParameterNear 0.7<parameter> overlap.LeftTo
    assertParameterNear 0.0<parameter> overlap.RightFrom
    assertParameterNear 1.0<parameter> overlap.RightTo

[<Fact>]
let ``reversed overlap preserves decreasing right parameters`` () =
    let right = Line(point 7.0 0.0, point 3.0 0.0)
    let overlap =
        OverlapDetection.detectWithSamples (line ()) right tolerance 5
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.3<parameter> overlap.LeftFrom
    assertParameterNear 0.7<parameter> overlap.LeftTo
    assertParameterNear 1.0<parameter> overlap.RightFrom
    assertParameterNear 0.0<parameter> overlap.RightTo

[<Fact>]
let ``identical line is one full overlap`` () =
    assertFullOverlap (line ()) (line ()) 0.0<parameter> 1.0<parameter>

[<Fact>]
let ``geometric tolerance is not used as parameter span`` () =
    let overlap =
        OverlapDetection.detectWithSamples (line ()) (line ()) 2.0<length> 5
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.0<parameter> overlap.LeftFrom
    assertParameterNear 1.0<parameter> overlap.LeftTo
    assertParameterNear 0.0<parameter> overlap.RightFrom
    assertParameterNear 1.0<parameter> overlap.RightTo

[<Fact>]
let ``strict tolerance merges full arc overlap proposals`` () =
    let overlap =
        OverlapDetection.detectWithSamples (arc ()) (arc ()) 1.0e-9<length> 9
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    assertParameterNear 0.0<parameter> overlap.LeftFrom
    assertParameterNear 1.0<parameter> overlap.LeftTo
    assertParameterNear 0.0<parameter> overlap.RightFrom
    assertParameterNear 1.0<parameter> overlap.RightTo

[<Fact>]
let ``reversed line is one full overlap`` () =
    assertFullOverlap (line ()) (Segment.reverse (line ())) 1.0<parameter> 0.0<parameter>

[<Fact>]
let ``identical quadratic is one full overlap`` () =
    let quadratic = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    assertFullOverlap quadratic quadratic 0.0<parameter> 1.0<parameter>

[<Fact>]
let ``reversed quadratic is one full overlap`` () =
    let quadratic = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    assertFullOverlap quadratic (Segment.reverse quadratic) 1.0<parameter> 0.0<parameter>

[<Fact>]
let ``identical cubic is one full overlap`` () =
    let cubic = CubicBezier(point 0.0 0.0, point 2.0 9.0, point 8.0 -9.0, point 10.0 0.0)
    assertFullOverlap cubic cubic 0.0<parameter> 1.0<parameter>

[<Fact>]
let ``reversed cubic is one full overlap`` () =
    let cubic = CubicBezier(point 0.0 0.0, point 2.0 9.0, point 8.0 -9.0, point 10.0 0.0)
    assertFullOverlap cubic (Segment.reverse cubic) 1.0<parameter> 0.0<parameter>

[<Fact>]
let ``identical arc is one full overlap`` () =
    let sameGeometry =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = true
              Sweep = true
              End = point 10.0 0.0 }
    assertFullOverlap (arc ()) sameGeometry 0.0<parameter> 1.0<parameter>

[<Fact>]
let ``reversed arc is one full overlap`` () =
    assertFullOverlap (arc ()) (Segment.reverse (arc ())) 1.0<parameter> 0.0<parameter>

[<Fact>]
let ``opposite semicircles do not overlap`` () =
    let opposite =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = false
              End = point 10.0 0.0 }
    Assert.Equal(Ok [], OverlapDetection.detectWithSamples (arc ()) opposite tolerance 9)

[<Fact>]
let ``non-affine parameter correspondence is rejected`` () =
    let linearSpeed =
        CubicBezier(point 0.0 0.0, point (1.0 / 3.0) 0.0, point (2.0 / 3.0) 0.0, point 1.0 0.0)
    let cubicSpeed = CubicBezier(point 0.0 0.0, point 0.0 0.0, point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error NonAffineOverlapCorrespondence, OverlapDetection.detect linearSpeed cubicSpeed tolerance)

[<Fact>]
let ``known parameter correspondence can be checked directly`` () =
    let overlap =
        OverlapDetection.checkParameterCorrespondence
            (line ())
            (line ())
            0.2<parameter>
            0.8<parameter>
            0.2<parameter>
            0.8<parameter>
            tolerance
            5
        |> Result.defaultWith (failwithf "%A")
        |> Option.get
    assertParameterNear 0.2<parameter> overlap.LeftFrom
    assertParameterNear 0.8<parameter> overlap.LeftTo

[<Fact>]
let ``overlap detection rejects negative tolerance`` () =
    Assert.Equal(Error(InvalidOverlapTolerance -1.0e-6<length>), OverlapDetection.detect (line ()) (line ()) -1.0e-6<length>)

[<Fact>]
let ``endpoint projection overlap rejects nonpositive samples`` () =
    Assert.Equal(Error(InvalidOverlapSamples 0), OverlapDetection.detectWithSamples (line ()) (line ()) tolerance 0)
