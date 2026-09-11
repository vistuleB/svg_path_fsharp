module SvgPath.Tests.BezierTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

[<Fact>]
let ``bezier point evaluates linear quadratic and cubic`` () =
    let linear = Bezier.LinearBezierData(point 0.0 0.0, point 10.0 20.0)
    let quadratic = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let cubic = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Equal(point 5.0 10.0, Bezier.point linear (parameter 0.5))
    Assert.Equal(point 10.0 10.0, Bezier.point quadratic (parameter 0.5))
    Assert.Equal(point 15.0 22.5, Bezier.point cubic (parameter 0.5))

[<Fact>]
let ``bezier point extrapolates outside t`` () =
    let linear = Bezier.LinearBezierData(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(point -5.0 -10.0, Bezier.point linear (parameter -0.5))
    Assert.Equal(point 15.0 30.0, Bezier.point linear (parameter 1.5))

[<Fact>]
let ``bezier derivative uses parameter t`` () =
    let quadratic = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let cubic = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Equal(Point.create 20.0<length / parameter> 40.0<length / parameter>, Bezier.derivative quadratic (parameter 0.0))
    Assert.Equal(Point.create 20.0<length / parameter> 0.0<length / parameter>, Bezier.derivative quadratic (parameter 0.5))
    Assert.Equal(Point.create 45.0<length / parameter> 0.0<length / parameter>, Bezier.derivative cubic (parameter 0.5))

[<Fact>]
let ``split divides quadratic at t`` () =
    let curve = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    let left, right = Bezier.split curve (parameter 0.25)
    Assert.Equal(Bezier.QuadraticBezierData(point 0.0 0.0, point 2.5 5.0, point 5.0 7.5), left)
    Assert.Equal(Bezier.QuadraticBezierData(point 5.0 7.5, point 12.5 15.0, point 20.0 0.0), right)

[<Fact>]
let ``bezier bounding box of quadratic includes interior extremum`` () =
    let curve = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    let box = Bezier.boundingBox curve
    Assert.Equal(point 0.0 0.0, box.Min)
    Assert.Equal(point 20.0 5.0, box.Max)

[<Fact>]
let ``bezier bounding box of line uses endpoint extents`` () =
    let line = Bezier.LinearBezierData(point 1.0 2.0, point 5.0 -3.0) |> Bezier.boundingBox
    Assert.Equal(point 1.0 -3.0, line.Min)
    Assert.Equal(point 5.0 2.0, line.Max)

[<Fact>]
let ``bezier bounding box of cubic includes interior extrema`` () =
    let cubic = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0) |> Bezier.boundingBox
    Assert.Equal(point 0.0 0.0, cubic.Min)
    Assert.Equal(point 30.0 22.5, cubic.Max)

[<Fact>]
let ``bezier bounding box matches generated fixtures`` () =
    for curve, expectedMin, expectedMax in BezierBoundingBoxFixtures.fixtures do
        let actual = Bezier.boundingBox curve
        Assert.True(Point.distance actual.Min expectedMin <= 1.0e-6<length>)
        Assert.True(Point.distance actual.Max expectedMax <= 1.0e-6<length>)

[<Fact>]
let ``map points maps bezier defining points`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let mapped = Bezier.mapPoints (fun value -> point (Length.toFloat value.X + 1.0) (Length.toFloat value.Y * 2.0)) curve
    Assert.Equal(
        Bezier.CubicBezierData(point 1.0 0.0, point 1.0 60.0, point 31.0 60.0, point 31.0 0.0),
        mapped)

[<Fact>]
let ``cubic inflection parameters finds an s curve inflection`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 100.0, point 100.0 -100.0, point 100.0 0.0)
    let roots = Bezier.cubicInflectionParameters curve
    Assert.Single roots |> ignore
    Assert.Equal(0.5, roots |> List.head |> Parameter.ratio, 12)
    Assert.Equal(point 50.0 0.0, Bezier.point curve (roots |> List.head))

[<Fact>]
let ``fit cubic with endpoint tangents recovers exact cubic`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 35.0 65.0, point 90.0 -35.0, point 130.0 25.0)
    let samples =
        [ parameter 0.25, Bezier.point original (parameter 0.25)
          parameter 0.5, Bezier.point original (parameter 0.5)
          parameter 0.75, Bezier.point original (parameter 0.75) ]
    let fit, report =
        Bezier.fitCubicWithEndpointTangents
            (Bezier.start original)
            (Bezier.finish original)
            (Bezier.derivative original (parameter 0.0))
            (Bezier.derivative original (parameter 1.0))
            samples
        |> Result.defaultWith (failwithf "%A")
    let control1, control2 =
        match fit with
        | Bezier.CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic fit"
    let expected1, expected2 =
        match original with
        | Bezier.CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic source"
    Assert.True(Point.distance control1 expected1 < 1.0e-9<length>)
    Assert.True(Point.distance control2 expected2 < 1.0e-9<length>)
    Assert.True(report.Max < 1.0e-9<length>)
    Assert.Equal(Bezier.PositiveHandle, report.StartHandle)
    Assert.Equal(Bezier.PositiveHandle, report.EndHandle)

[<Fact>]
let ``fit cubic with endpoint tangents uses forward end tangent`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 10.0 20.0, point 80.0 40.0, point 100.0 0.0)
    let samples =
        [ parameter 0.2, Bezier.point original (parameter 0.2)
          parameter 0.6, Bezier.point original (parameter 0.6) ]
    let fit, _ =
        Bezier.fitCubicWithEndpointTangents
            (Bezier.start original)
            (Bezier.finish original)
            (Bezier.derivative original (parameter 0.0))
            (Bezier.derivative original (parameter 1.0))
            samples
        |> Result.defaultWith (failwithf "%A")
    let endDerivativeDistance =
        Point.distance
            (Bezier.derivative fit (parameter 1.0))
            (Bezier.derivative original (parameter 1.0))
    Assert.True(endDerivativeDistance < 1.0e-6<length / parameter>)

[<Fact>]
let ``fit cubic with endpoint tangents accepts small well conditioned equations`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 2.0 3.0, point 7.0 -2.0, point 10.0 1.0)
    let samples =
        [ parameter 0.0001, Bezier.point original (parameter 0.0001)
          parameter 0.0002, Bezier.point original (parameter 0.0002) ]
    let fit, report =
        Bezier.fitCubicWithEndpointTangents
            (Bezier.start original)
            (Bezier.finish original)
            (Bezier.derivative original (parameter 0.0))
            (Bezier.derivative original (parameter 1.0))
            samples
        |> Result.defaultWith (failwithf "%A")
    match original, fit with
    | Bezier.CubicBezierData(_, expected1, expected2, _), Bezier.CubicBezierData(_, actual1, actual2, _) ->
        Assert.True(Point.distance expected1 actual1 < 1.0e-6<length>)
        Assert.True(Point.distance expected2 actual2 < 1.0e-6<length>)
    | _ -> Assert.Fail "expected cubic curves"
    Assert.Equal(Bezier.PositiveHandle, report.StartHandle)
    Assert.Equal(Bezier.PositiveHandle, report.EndHandle)

[<Fact>]
let ``fit cubic with endpoint tangents clamps negative handles`` () =
    let startPoint, endPoint = point 0.0 0.0, point 1.0 0.0
    let fit, report =
        Bezier.fitCubicWithEndpointTangents
            startPoint endPoint
            (point 1.0 0.0) (point 1.0 0.0)
            [ parameter 0.25, point -1.0 0.0
              parameter 0.5, point -1.0 0.0
              parameter 0.75, point -1.0 0.0 ]
        |> Result.defaultWith (failwithf "%A")
    match fit with
    | Bezier.CubicBezierData(_, control1, control2, _) ->
        Assert.True(control1.X >= startPoint.X)
        Assert.True(control2.X <= endPoint.X)
    | _ -> Assert.Fail "expected cubic fit"
    Assert.Equal(Bezier.CollapsedHandle, report.StartHandle)

[<Fact>]
let ``fit cubic with endpoint tangents rejects degenerate tangent`` () =
    Assert.Equal(
        Error Bezier.DegenerateTangent,
        Bezier.fitCubicWithEndpointTangents
            (point 0.0 0.0) (point 10.0 0.0)
            (point 0.0 0.0) (point 1.0 0.0)
            [ parameter 0.5, point 5.0 1.0 ])

[<Fact>]
let ``fit cubic with endpoint tangents rejects underdetermined samples`` () =
    Assert.Equal(
        Error Bezier.UnderdeterminedCubicFit,
        Bezier.fitCubicWithEndpointTangents
            (point 0.0 0.0) (point 10.0 0.0)
            (point 1.0 0.0) (point 1.0 0.0)
            [])

[<Fact>]
let ``fit cubic with endpoints recovers exact cubic`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 35.0 65.0, point 90.0 -35.0, point 130.0 25.0)
    let samples =
        [ parameter 0.25, Bezier.point original (parameter 0.25)
          parameter 0.5, Bezier.point original (parameter 0.5)
          parameter 0.75, Bezier.point original (parameter 0.75) ]
    let fit, report =
        Bezier.fitCubicWithEndpoints (Bezier.start original) (Bezier.finish original) samples
        |> Result.defaultWith (failwithf "%A")
    let control1, control2 =
        match fit with
        | Bezier.CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic fit"
    let expected1, expected2 =
        match original with
        | Bezier.CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic source"
    Assert.True(Point.distance control1 expected1 < 1.0e-9<length>)
    Assert.True(Point.distance control2 expected2 < 1.0e-9<length>)
    Assert.True(report.Max < 1.0e-9<length>)

[<Fact>]
let ``fit cubic with endpoints fits noisy samples`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 10.0 30.0, point 80.0 -10.0, point 100.0 0.0)
    let perturb dx dy value = Point.create (value.X + Length.fromFloat dx) (value.Y + Length.fromFloat dy)
    let samples =
        [ parameter 0.2, Bezier.point original (parameter 0.2) |> perturb 1.0 -2.0
          parameter 0.4, Bezier.point original (parameter 0.4) |> perturb -1.0 1.0
          parameter 0.7, Bezier.point original (parameter 0.7) |> perturb 2.0 1.0
          parameter 0.9, Bezier.point original (parameter 0.9) |> perturb -1.0 -1.0 ]
    let fit, report =
        Bezier.fitCubicWithEndpoints (Bezier.start original) (Bezier.finish original) samples
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Bezier.start original, Bezier.start fit)
    Assert.Equal(Bezier.finish original, Bezier.finish fit)
    Assert.True(report.RootSumSquare > 0.0<length>)
    Assert.True(report.RootMeanSquare > 0.0<length>)
    Assert.True(report.Max > 0.0<length>)

[<Fact>]
let ``fit cubic with endpoints accepts small well conditioned equations`` () =
    let original = Bezier.CubicBezierData(point 0.0 0.0, point 2.0 3.0, point 7.0 -2.0, point 10.0 1.0)
    let samples =
        [ parameter 0.0001, Bezier.point original (parameter 0.0001)
          parameter 0.0002, Bezier.point original (parameter 0.0002) ]
    let fit, report =
        Bezier.fitCubicWithEndpoints (Bezier.start original) (Bezier.finish original) samples
        |> Result.defaultWith (failwithf "%A")
    match original, fit with
    | Bezier.CubicBezierData(_, expected1, expected2, _), Bezier.CubicBezierData(_, actual1, actual2, _) ->
        Assert.True(Point.distance expected1 actual1 < 1.0e-6<length>)
        Assert.True(Point.distance expected2 actual2 < 1.0e-6<length>)
    | _ -> Assert.Fail "expected cubic curves"
    Assert.Equal(Bezier.UnconstrainedHandle, report.StartHandle)
    Assert.Equal(Bezier.UnconstrainedHandle, report.EndHandle)

[<Fact>]
let ``fit cubic with endpoints rejects underdetermined samples`` () =
    Assert.Equal(
        Error Bezier.UnderdeterminedCubicFit,
        Bezier.fitCubicWithEndpoints
            (point 0.0 0.0) (point 10.0 0.0)
            [ parameter 0.5, point 5.0 1.0 ])

[<Fact>]
let ``cubic self intersections finds interior crossing`` () =
    let curve =
        Bezier.CubicBezierData(
            point 0.0 0.0,
            point -0.2708333333333333 -0.3333333333333333,
            point -0.5416666666666666 -0.3333333333333333,
            point 0.1875 0.0)
    let intersection =
        Bezier.cubicSelfIntersections curve
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(0.25, Parameter.ratio intersection.S, 12)
    Assert.Equal(0.75, Parameter.ratio intersection.T, 12)

[<Fact>]
let ``cubic self intersections respects minimum arc length separation`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    let options: Bezier.CubicSelfIntersectionOptions =
        { MinimumArcLengthSeparation = 301.0<length>
          DistanceTolerance = 1.0e-6<length> }
    Assert.Empty(Bezier.cubicSelfIntersectionsWith curve options |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``split allows endpoint splits`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let zeroStart, wholeAfter = Bezier.split curve (parameter 0.0)
    let wholeBefore, zeroEnd = Bezier.split curve (parameter 1.0)
    Assert.Equal(point 0.0 0.0, Bezier.start zeroStart)
    Assert.Equal(point 0.0 0.0, Bezier.finish zeroStart)
    Assert.Equal(curve, wholeAfter)
    Assert.Equal(curve, wholeBefore)
    Assert.Equal(point 30.0 0.0, Bezier.start zeroEnd)
    Assert.Equal(point 30.0 0.0, Bezier.finish zeroEnd)

[<Fact>]
let ``split inside rejects outside t`` () =
    let curve = Bezier.LinearBezierData(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(Error Bezier.SplitOutsideBezier, Bezier.splitInside curve (parameter -0.01))
    Assert.Equal(Error Bezier.SplitOutsideBezier, Bezier.splitInside curve (parameter 1.01))
    Assert.True(Bezier.splitInside curve (parameter 0.0) |> Result.isOk)
    Assert.True(Bezier.splitInside curve (parameter 1.0) |> Result.isOk)

[<Fact>]
let ``split many keeps boundary points when they are interior`` () =
    let curve = Bezier.LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    let pieces = Bezier.splitMany curve [ parameter 1.25; parameter 1.0; parameter 0.0; parameter -0.25 ]
    let endpoints = pieces |> List.map (fun piece -> Bezier.start piece, Bezier.finish piece)
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point -10.0 0.0
          point -10.0 0.0, point 0.0 0.0
          point 0.0 0.0, point 40.0 0.0
          point 40.0 0.0, point 50.0 0.0
          point 50.0 0.0, point 40.0 0.0 ],
        endpoints)

[<Fact>]
let ``split inside many rejects any outside point`` () =
    let curve = Bezier.LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    Assert.Equal(Error Bezier.SplitOutsideBezier, Bezier.splitManyInside curve [ parameter 0.25; parameter 1.01 ])
    Assert.Equal(Error Bezier.SplitOutsideBezier, Bezier.splitManyInside curve [ parameter -0.01; parameter 0.75 ])

[<Fact>]
let ``split inside many trims boundary points`` () =
    let curve = Bezier.LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    let pieces =
        Bezier.splitManyInside curve [ parameter 1.0; parameter 0.0; parameter 0.5; parameter 0.5 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length pieces)
    Assert.Equal(point 0.0 0.0, Bezier.start pieces[0])
    Assert.Equal(point 20.0 0.0, Bezier.finish pieces[0])
    Assert.Equal(point 20.0 0.0, Bezier.start pieces[1])
    Assert.Equal(point 40.0 0.0, Bezier.finish pieces[1])

[<Fact>]
let ``split many preserves cubic degree`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let pieces = Bezier.splitMany curve [ parameter 0.25; parameter 0.75 ]
    Assert.Equal(3, List.length pieces)
    Assert.All(pieces, fun piece ->
        match piece with
        | Bezier.CubicBezierData _ -> ()
        | _ -> Assert.Fail "expected cubic piece")

[<Fact>]
let ``cubic inflection parameters are independent of coordinate scale`` () =
    let scale = 1.0e-9
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 (100.0 * scale), point (100.0 * scale) (-100.0 * scale), point (100.0 * scale) 0.0)
    let root = Bezier.cubicInflectionParameters curve |> List.exactlyOne
    Assert.Equal(0.5, Parameter.ratio root, 12)

[<Fact>]
let ``cubic inflection parameters ignores non inflecting curves`` () =
    let cubic = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let quadratic = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    Assert.Empty(Bezier.cubicInflectionParameters cubic)
    Assert.Empty(Bezier.cubicInflectionParameters quadratic)

[<Fact>]
let ``split many sorts and removes duplicate points`` () =
    let curve = Bezier.LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    let pieces = Bezier.splitMany curve [ parameter 0.75; parameter -0.25; parameter 0.25; parameter 0.25 ]
    let endpoints = pieces |> List.map (fun piece -> Bezier.start piece, Bezier.finish piece)
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point -10.0 0.0
          point -10.0 0.0, point 10.0 0.0
          point 10.0 0.0, point 30.0 0.0
          point 30.0 0.0, point 40.0 0.0 ],
        endpoints)

[<Fact>]
let ``cubic self intersections finds loop`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    let intersection = Bezier.cubicSelfIntersections curve |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(0.0, Parameter.ratio intersection.S, 12)
    Assert.Equal(1.0, Parameter.ratio intersection.T, 12)
    Assert.Equal(point 0.0 0.0, intersection.Point)

[<Fact>]
let ``cubic self intersections are independent of coordinate scale`` () =
    let scale = 1.0e-12
    let curve =
        Bezier.CubicBezierData(
            point 0.0 0.0,
            point (-0.2708333333333333 * scale) (-0.3333333333333333 * scale),
            point (-0.5416666666666666 * scale) (-0.3333333333333333 * scale),
            point (0.1875 * scale) 0.0)
    let options: Bezier.CubicSelfIntersectionOptions = { MinimumArcLengthSeparation = 1.0e-15<length>; DistanceTolerance = 1.0e-15<length> }
    let intersection = Bezier.cubicSelfIntersectionsWith curve options |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(0.25, Parameter.ratio intersection.S, 10)
    Assert.Equal(0.75, Parameter.ratio intersection.T, 10)

[<Fact>]
let ``cubic self intersections ignores non looping cubic`` () =
    let cubic = Bezier.CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Empty(Bezier.cubicSelfIntersections cubic |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``cubic self intersections ignores non cubics`` () =
    let line = Bezier.LinearBezierData(point 0.0 0.0, point 10.0 0.0)
    let quadratic = Bezier.QuadraticBezierData(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    Assert.Empty(Bezier.cubicSelfIntersections line |> Result.defaultWith (failwithf "%A"))
    Assert.Empty(Bezier.cubicSelfIntersections quadratic |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``cubic self intersections rejects invalid options`` () =
    let curve = Bezier.CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    Assert.Equal(
        Error(Bezier.InvalidCubicSelfIntersectionMinimumArcLengthSeparation 0.0<length>),
        Bezier.cubicSelfIntersectionsWith curve { MinimumArcLengthSeparation = 0.0<length>; DistanceTolerance = 1.0e-6<length> })
    Assert.Equal(
        Error(Bezier.InvalidCubicSelfIntersectionDistanceTolerance 0.0<length>),
        Bezier.cubicSelfIntersectionsWith curve { MinimumArcLengthSeparation = 1.0e-6<length>; DistanceTolerance = 0.0<length> })
