module SvgPath.Tests.VectorSupportTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private support segment x y = ConvexHull.internalSegmentSupportInDirection segment (Point.create x y) |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``nonunit line support returns raw dot product`` () =
    let line = Line(p -2. 3., p 4. 1.)
    Assert.Equal((1.0<parameter>, p 4. 1., 16.0<length>), support line 3. 4.)
    Assert.Equal((0.0<parameter>, p -2. 3., -6.0<length>), support line -3. -4.)
[<Fact>]
let ``zero direction selects start`` () =
    Assert.Equal((0.0<parameter>, p 1. 2., 0.0<length>), support (QuadraticBezier(p 1. 2., p 3. 4., p 5. 6.)) -0. 0.)
[<Fact>]
let ``quadratic support handles extreme direction scales`` () =
    for scale in [1e-20; 7.; 1e20] do
        Assert.Equal((0.5<parameter>, p 1. 1., Length.fromFloat scale), support (QuadraticBezier(p 0. 0., p 1. 2., p 2. 0.)) 0. scale)
[<Fact>]
let ``cubic nonunit support finds interior maximum`` () =
    Assert.Equal((0.5<parameter>, p 1.5 3., 15.0<length>), support (CubicBezier(p 0. 0., p 1. 4., p 2. 4., p 3. 0.)) 0. 5.)
[<Fact>]
let ``arc nonunit support finds interior maximum`` () =
    let arc = Arc { Start = p 1. 0.; Radius = p 1. 1.; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = p -1. 0. }
    let t, point, value = support arc 0. 3.
    Assert.True(abs (t - 0.5<parameter>) < 1e-9<parameter>)
    Assert.True(abs (point.Y - 1.0<length>) < 1e-9<length>)
    Assert.True(abs (value - 3.0<length>) < 1e-9<length>)
[<Fact>]
let ``opposite supports give width after one norm division`` () =
    let line = Line(p 0. 0., p 10. 0.)
    let _, _, upper = support line 3. 4.
    let _, _, opposite = support line -3. -4.
    Assert.Equal(6.0<length>, (upper + opposite) / 5.)
[<Fact>]
let ``collinear bezier width is not certified positive`` () =
    let hull = ConvexHull.segmentHull (QuadraticBezier(p 0. 0., p -20. 0., p 0. 0.)) |> Result.defaultWith (failwithf "%A")
    match ConvexHull.internalConvexSubpathMinimumWidthDecision hull 0.0<length> |> Result.defaultWith (failwithf "%A") with
    | MinimumWidthFits strip -> Assert.Equal(0.0<length>, strip.Width)
    | MinimumWidthUnresolved(lower, _) -> Assert.Equal(0.0<length>, lower)
    | other -> failwithf "Collinear curve has zero width: %A" other
[<Fact>]
let ``width threshold roundoff remains unresolved not exceeds`` () =
    let support angle =
        let normal = Point.direction angle
        { LowerPoint = Point.scale -0.5000000000001<length> normal
          UpperPoint = Point.scale 0.5000000000001<length> normal
          Width = 1.0000000000002<length> }
    match ConvexHull.internalMinimumWidthSearch support 1.0000000000002<length> 1.0<length> 20 with
    | MinimumWidthUnresolved(lower, best) ->
        Assert.True(lower <= 1.0<length>)
        Assert.True(best > 1.0<length>)
    | other -> failwithf "Expected unresolved: %A" other
