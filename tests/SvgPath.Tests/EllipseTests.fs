module SvgPath.Tests.EllipseTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value
let private parameter value = Parameter.fromFloat value

let private quarterEllipse =
    { Center = point 0.0 0.0
      Radius = point 4.0 2.0
      XAxisRotation = degrees 0.0
      StartAngle = degrees 0.0
      DeltaAngle = degrees 90.0 }

let private assertPointNear tolerance (expected: Point<'Unit>) (actual: Point<'Unit>) =
    Assert.True(Point.distance expected actual <= tolerance, $"expected {expected}, got {actual}")

[<Fact>]
let ``arc evaluation carries parameter powers through both derivatives`` () =
    Assert.Equal(point 4.0 0.0, Ellipse.arcPoint quarterEllipse (parameter 0.0))
    let first = Ellipse.arcDerivative quarterEllipse (parameter 0.0)
    let second = Ellipse.arcSecondDerivative quarterEllipse (parameter 0.0)
    let expectedFirst = Point.create 0.0<length / parameter> (System.Math.PI * 1.0<length / parameter>)
    let expectedSecond = Point.create (-System.Math.PI * System.Math.PI * 1.0<length / parameter^2>) 0.0<length / parameter^2>
    assertPointNear 1.0e-12<length / parameter> expectedFirst first
    assertPointNear 1.0e-12<length / parameter^2> expectedSecond second

[<Fact>]
let ``angle derivative is measured per degree`` () =
    let derivative: Point<length / degree> = Ellipse.arcDerivativeAtAngle quarterEllipse (degrees 0.0)
    let expected = Point.create 0.0<length / degree> (System.Math.PI / 90.0 * 1.0<length / degree>)
    assertPointNear 1.0e-12<length / degree> expected derivative

[<Fact>]
let ``endpoint and center forms preserve endpoints and flags`` () =
    let endpoint =
        { Start = point 4.0 0.0
          Radius = point 4.0 2.0
          XAxisRotation = degrees 0.0
          LargeArc = false
          Sweep = true
          End = point 0.0 2.0 }
    let center = Ellipse.endpointToCenter endpoint |> Result.defaultWith (failwithf "%A")
    let roundTrip = Ellipse.centerToEndpoint center
    assertPointNear 1.0e-12<length> endpoint.Start roundTrip.Start
    assertPointNear 1.0e-12<length> endpoint.End roundTrip.End
    Assert.False(roundTrip.LargeArc)
    Assert.True(roundTrip.Sweep)

[<Fact>]
let ``endpoint conversion corrects radii too small to span endpoints`` () =
    let endpoint =
        { Start = point -10.0 0.0
          Radius = point 1.0 1.0
          XAxisRotation = degrees 0.0
          LargeArc = false
          Sweep = true
          End = point 10.0 0.0 }
    let center = Ellipse.endpointToCenter endpoint |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 10.0 10.0, center.Radius)

[<Fact>]
let ``zero radius endpoint arc is rejected`` () =
    let endpoint =
        { Start = point 0.0 0.0
          Radius = point 0.0 2.0
          XAxisRotation = degrees 0.0
          LargeArc = false
          Sweep = true
          End = point 4.0 0.0 }
    Assert.Equal(Error DegenerateInputArc, Ellipse.endpointToCenter endpoint)

[<Fact>]
let ``arc splitting preserves the original angular interval`` () =
    let left, right = Ellipse.splitArc quarterEllipse (parameter 0.25)
    Assert.Equal(degrees 22.5, left.DeltaAngle)
    Assert.Equal(degrees 22.5, right.StartAngle)
    Assert.Equal(degrees 67.5, right.DeltaAngle)
    assertPointNear 1.0e-12<length> (Ellipse.arcPoint left (parameter 1.0)) (Ellipse.arcPoint right (parameter 0.0))

[<Fact>]
let ``arc bounding box includes interior axis extrema`` () =
    let halfEllipse = { quarterEllipse with StartAngle = degrees -45.0; DeltaAngle = degrees 180.0 }
    let box = Ellipse.arcBoundingBox halfEllipse
    assertPointNear 1.0e-12<length> (point (-2.0 * sqrt 2.0) (-sqrt 2.0)) box.Min
    assertPointNear 1.0e-12<length> (point 4.0 2.0) box.Max

[<Fact>]
let ``projection extrema return nominal arc parameters`` () =
    let extrema: float<parameter> list = Ellipse.arcProjectionExtrema quarterEllipse Point.right
    Assert.Equal<float<parameter> list>([ parameter 0.0 ], extrema)

[<Fact>]
let ``a half-turn arc is approximated by two cubics`` () =
    let cubics =
        Ellipse.arcToCubics (point 4.0 0.0) (point 4.0 2.0) (degrees 0.0) false true (point -4.0 0.0)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length cubics)
    let first, second = List.item 0 cubics, List.item 1 cubics
    assertPointNear 1.0e-12<length> (point 4.0 0.0) first.Start
    assertPointNear 1.0e-12<length> first.End second.Start
    assertPointNear 1.0e-12<length> (point -4.0 0.0) second.End

[<Fact>]
let ``transformed axes preserve ellipse geometry under nonuniform scale`` () =
    let radius, rotation =
        Ellipse.transformedAxes (point 4.0 2.0) (degrees 0.0) (Affine.scaleXY 3.0 5.0)
        |> Result.defaultWith (failwithf "%A")
    assertPointNear 1.0e-12<length> (point 12.0 10.0) radius
    Assert.Equal(degrees 0.0, rotation)

[<Fact>]
let ``transformed axes retain valid radii below square root of length tolerance`` () =
    let radius, _rotation =
        Ellipse.transformedAxes (point 1.0e-5 2.0e-5) (degrees 17.0) (Affine.identity ())
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1.0e-5, min radius.X radius.Y |> Length.toFloat, 12)
    Assert.Equal(2.0e-5, max radius.X radius.Y |> Length.toFloat, 12)

[<Fact>]
let ``collapsed arc produces a line on its surviving transformed axis`` () =
    let collapseY = Affine.scaleXY 1.0 0.0
    let startPoint, endPoint =
        Ellipse.collapsedArcLine
            (point 4.0 0.0)
            (point 4.0 2.0)
            (degrees 0.0)
            false
            true
            (point -4.0 0.0)
            collapseY
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point -4.0 0.0, startPoint)
    Assert.Equal(point 4.0 0.0, endPoint)

[<Fact>]
let ``collapsed arc collinearity is relative to the transformed scale`` () =
    let nearlyRankOne =
        Affine.matrix 1.0 0.0 1.0 1.0e-10 0.0<length> 0.0<length>

    for scale in [ 1.0; 1.0e9 ] do
        Ellipse.collapsedArcLine
            (point scale 0.0)
            (point scale scale)
            (degrees 0.0)
            false
            true
            (point -scale 0.0)
            nearlyRankOne
        |> Result.defaultWith (failwithf "%A")
        |> ignore

[<Fact>]
let ``half-circle bounding boxes follow the sweep flag`` () =
    let endpoint sweep =
        { Start = point 0.0 0.0
          Radius = point 10.0 10.0
          XAxisRotation = degrees 0.0
          LargeArc = false
          Sweep = sweep
          End = point 20.0 0.0 }

    let swept = endpoint true |> Ellipse.endpointToCenter |> Result.defaultWith (failwithf "%A") |> Ellipse.arcBoundingBox
    let unswept = endpoint false |> Ellipse.endpointToCenter |> Result.defaultWith (failwithf "%A") |> Ellipse.arcBoundingBox
    assertPointNear 1.0e-10<length> (point 0.0 -10.0) swept.Min
    assertPointNear 1.0e-10<length> (point 20.0 0.0) swept.Max
    assertPointNear 1.0e-10<length> (point 0.0 0.0) unswept.Min
    assertPointNear 1.0e-10<length> (point 20.0 10.0) unswept.Max

[<Fact>]
let ``rotated arc bounding box includes interior extrema`` () =
    let arc =
        { Center = point 2.0 -3.0
          Radius = point 12.0 5.0
          XAxisRotation = degrees 30.0
          StartAngle = degrees -68.75493541569878
          DeltaAngle = degrees 252.1015816987223 }
    let box = Ellipse.arcBoundingBox arc
    assertPointNear 1.0e-5<length> (point -8.688779 -9.242547) box.Min
    assertPointNear 1.0e-5<length> (point 12.688779 4.399324) box.Max

[<Fact>]
let ``arc splitting permits endpoints and extrapolates outside them`` () =
    let arc =
        { Center = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = degrees 0.0
          StartAngle = degrees 1.0
          DeltaAngle = degrees 2.0 }

    let zeroStart, wholeAfter = Ellipse.splitArc arc (parameter 0.0)
    let wholeBefore, zeroEnd = Ellipse.splitArc arc (parameter 1.0)
    Assert.Equal(degrees 0.0, zeroStart.DeltaAngle)
    Assert.Equal(arc.StartAngle, zeroStart.StartAngle)
    Assert.Equal(arc, wholeAfter)
    Assert.Equal(arc, wholeBefore)
    Assert.Equal(Ellipse.arcEndAngle arc, zeroEnd.StartAngle)
    Assert.Equal(degrees 0.0, zeroEnd.DeltaAngle)

    let before, throughEnd = Ellipse.splitArc arc (parameter -0.25)
    let throughPastEnd, backToEnd = Ellipse.splitArc arc (parameter 1.25)
    Assert.Equal(degrees -0.5, before.DeltaAngle)
    Assert.Equal(degrees 0.5, throughEnd.StartAngle)
    Assert.Equal(degrees 2.5, throughEnd.DeltaAngle)
    Assert.Equal(degrees 2.5, throughPastEnd.DeltaAngle)
    Assert.Equal(degrees 3.5, backToEnd.StartAngle)
    Assert.Equal(degrees -0.5, backToEnd.DeltaAngle)

[<Fact>]
let ``inside arc splitting rejects parameters outside the arc`` () =
    let arc =
        { Center = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = degrees 0.0
          StartAngle = degrees 1.0
          DeltaAngle = degrees 2.0 }
    Assert.Equal(Error SplitOutsideArc, Ellipse.splitArcInside arc (parameter -0.01))
    Assert.Equal(Error SplitOutsideArc, Ellipse.splitArcInside arc (parameter 1.01))
    Assert.True(Ellipse.splitArcInside arc (parameter 0.0) |> Result.isOk)
    Assert.True(Ellipse.splitArcInside arc (parameter 1.0) |> Result.isOk)

[<Fact>]
let ``multi-split sorts parameters and removes duplicates`` () =
    let arc =
        { Center = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = degrees 0.0
          StartAngle = degrees 1.0
          DeltaAngle = degrees 4.0 }
    let pieces = Ellipse.splitArcMany arc [ parameter 0.75; parameter -0.25; parameter 0.25; parameter 0.25 ]
    Assert.Equal(4, List.length pieces)
    let expected =
        [ degrees 1.0, degrees -1.0
          degrees 0.0, degrees 2.0
          degrees 2.0, degrees 2.0
          degrees 4.0, degrees 1.0 ]
    Assert.Equal<(float<degree> * float<degree>) list>(expected, pieces |> List.map (fun piece -> piece.StartAngle, piece.DeltaAngle))

[<Fact>]
let ``empty multi-split returns the original arc`` () =
    Assert.Equal<CenterArcData list>([ quarterEllipse ], Ellipse.splitArcMany quarterEllipse [])

[<Fact>]
let ``inside multi-split rejects any outside parameter`` () =
    let arc = { quarterEllipse with DeltaAngle = degrees 180.0 }
    Assert.Equal(Error SplitOutsideArc, Ellipse.splitArcInsideMany arc [ parameter 0.25; parameter 1.01 ])
    Assert.Equal(Error SplitOutsideArc, Ellipse.splitArcInsideMany arc [ parameter -0.01; parameter 0.75 ])

[<Fact>]
let ``inside multi-split accepts endpoints and removes duplicates`` () =
    let arc =
        { Center = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = degrees 0.0
          StartAngle = degrees 1.0
          DeltaAngle = degrees 4.0 }
    let pieces =
        Ellipse.splitArcInsideMany arc [ parameter 1.0; parameter 0.0; parameter 0.5; parameter 0.5 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length pieces)
    Assert.Equal(degrees 1.0, pieces[0].StartAngle)
    Assert.Equal(degrees 2.0, pieces[0].DeltaAngle)
    Assert.Equal(degrees 3.0, pieces[1].StartAngle)
    Assert.Equal(degrees 2.0, pieces[1].DeltaAngle)

[<Fact>]
let ``unrestricted multi-split preserves boundary parameters inside a wider split range`` () =
    let arc =
        { Center = point 0.0 0.0
          Radius = point 5.0 5.0
          XAxisRotation = degrees 0.0
          StartAngle = degrees 1.0
          DeltaAngle = degrees 4.0 }
    let pieces = Ellipse.splitArcMany arc [ parameter 1.25; parameter 1.0; parameter 0.0; parameter -0.25 ]
    let expected =
        [ degrees 1.0, degrees -1.0
          degrees 0.0, degrees 1.0
          degrees 1.0, degrees 4.0
          degrees 5.0, degrees 1.0
          degrees 6.0, degrees -1.0 ]
    Assert.Equal<(float<degree> * float<degree>) list>(expected, pieces |> List.map (fun piece -> piece.StartAngle, piece.DeltaAngle))

[<Fact>]
let ``large-arc and sweep flags are derived from delta angle`` () =
    let endpoint largeArc sweep =
        { Start = point 0.0 0.0
          Radius = point 10.0 10.0
          XAxisRotation = degrees 0.0
          LargeArc = largeArc
          Sweep = sweep
          End = point 10.0 0.0 }
    let largeSweep = endpoint true true |> Ellipse.endpointToCenter |> Result.defaultWith (failwithf "%A")
    let smallUnswept = endpoint false false |> Ellipse.endpointToCenter |> Result.defaultWith (failwithf "%A")
    Assert.True(Ellipse.arcLargeArc largeSweep)
    Assert.True(Ellipse.arcSweep largeSweep)
    Assert.True(largeSweep.DeltaAngle > degrees 180.0)
    Assert.False(Ellipse.arcLargeArc smallUnswept)
    Assert.False(Ellipse.arcSweep smallUnswept)
    Assert.True(smallUnswept.DeltaAngle < degrees 0.0)
    Assert.True(abs smallUnswept.DeltaAngle < degrees 180.0)
