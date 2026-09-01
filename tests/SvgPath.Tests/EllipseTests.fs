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
