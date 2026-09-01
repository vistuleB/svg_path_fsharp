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
let ``identical and reversed Beziers are full overlaps`` () =
    let quadratic = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    let cubic = CubicBezier(point 0.0 0.0, point 2.0 9.0, point 8.0 -9.0, point 10.0 0.0)
    assertFullOverlap quadratic quadratic 0.0<parameter> 1.0<parameter>
    assertFullOverlap quadratic (Segment.reverse quadratic) 1.0<parameter> 0.0<parameter>
    assertFullOverlap cubic cubic 0.0<parameter> 1.0<parameter>
    assertFullOverlap cubic (Segment.reverse cubic) 1.0<parameter> 0.0<parameter>

[<Fact>]
let ``semantic and reversed arcs are full overlaps`` () =
    let sameGeometry =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = true
              Sweep = true
              End = point 10.0 0.0 }
    assertFullOverlap (arc ()) sameGeometry 0.0<parameter> 1.0<parameter>
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
let ``invalid overlap options are rejected`` () =
    Assert.Equal(Error(InvalidOverlapTolerance -1.0e-6<length>), OverlapDetection.detect (line ()) (line ()) -1.0e-6<length>)
    Assert.Equal(Error(InvalidOverlapSamples 0), OverlapDetection.detectWithSamples (line ()) (line ()) tolerance 0)
