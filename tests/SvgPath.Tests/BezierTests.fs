module SvgPath.Tests.BezierTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parameter value = Parameter.fromFloat value

[<Fact>]
let ``cubic evaluation and derivative preserve coordinate units`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 0.0 2.0, point 2.0 2.0, point 2.0 0.0)
    let midpoint = Bezier.point curve (parameter 0.5)
    let derivative = Bezier.derivative curve (parameter 0.5)
    Assert.Equal(point 1.0 1.5, midpoint)
    Assert.Equal(Point.create 3.0<length / parameter> 0.0<length / parameter>, derivative)

[<Fact>]
let ``linear and quadratic evaluation and extrapolation match De Casteljau construction`` () =
    let linear = LinearBezierData(point 0.0 0.0, point 10.0 20.0)
    let quadratic = QuadraticBezierData(point 0.0 0.0, point 10.0 20.0, point 20.0 0.0)
    Assert.Equal(point 5.0 10.0, Bezier.point linear (parameter 0.5))
    Assert.Equal(point 10.0 10.0, Bezier.point quadratic (parameter 0.5))
    Assert.Equal(point -5.0 -10.0, Bezier.point linear (parameter -0.5))
    Assert.Equal(point 15.0 30.0, Bezier.point linear (parameter 1.5))

[<Fact>]
let ``derivative times a parameter interval is a geometric coordinate pair`` () =
    let curve = LinearBezierData(point 2.0 3.0, point 10.0 7.0)
    let derivative = Bezier.derivative curve (parameter 0.25)
    let displacement: Point<length> = Point.scale (parameter 0.5) derivative
    Assert.Equal(point 4.0 2.0, displacement)

[<Fact>]
let ``split pieces meet at the evaluated point`` () =
    let curve = QuadraticBezierData(point 0.0 0.0, point 1.0 2.0, point 2.0 0.0)
    let left, right = Bezier.split curve (parameter 0.25)
    let expected = Bezier.point curve (parameter 0.25)
    Assert.Equal(expected, Bezier.finish left)
    Assert.Equal(expected, Bezier.start right)

[<Fact>]
let ``split many sorts deduplicates and trims boundary parameters`` () =
    let curve = LinearBezierData(point 0.0 0.0, point 10.0 0.0)
    let pieces =
        Bezier.splitMany curve [ parameter 1.0; parameter 0.5; parameter 0.0; parameter 0.5 ]
    Assert.Equal(2, List.length pieces)
    Assert.Equal(point 5.0 0.0, pieces |> List.head |> Bezier.finish)

[<Fact>]
let ``quadratic bounding box includes its interior extremum`` () =
    let curve = QuadraticBezierData(point 0.0 0.0, point 1.0 2.0, point 2.0 0.0)
    let box = Bezier.boundingBox curve
    Assert.Equal(point 0.0 0.0, box.Min)
    Assert.Equal(point 2.0 1.0, box.Max)

[<Fact>]
let ``line bounding box uses endpoint extents`` () =
    let line = LinearBezierData(point 1.0 2.0, point 5.0 -3.0) |> Bezier.boundingBox
    Assert.Equal(point 1.0 -3.0, line.Min)
    Assert.Equal(point 5.0 2.0, line.Max)

[<Fact>]
let ``cubic bounding box includes interior extrema`` () =
    let cubic = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0) |> Bezier.boundingBox
    Assert.Equal(point 0.0 0.0, cubic.Min)
    Assert.Equal(point 30.0 22.5, cubic.Max)

[<Fact>]
let ``mapping transforms every defining point`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let mapped = Bezier.mapPoints (fun value -> point (Length.toFloat value.X + 1.0) (Length.toFloat value.Y * 2.0)) curve
    Assert.Equal(
        CubicBezierData(point 1.0 0.0, point 1.0 60.0, point 31.0 60.0, point 31.0 0.0),
        mapped)

[<Fact>]
let ``cubic inflection parameters exclude endpoints`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 1.0 1.0, point 2.0 -1.0, point 3.0 0.0)
    let roots = Bezier.cubicInflectionParameters curve
    Assert.Single roots |> ignore
    Assert.Equal(0.5, roots |> List.head |> Parameter.ratio, 12)

[<Fact>]
let ``endpoint tangent fitting recovers an exact cubic`` () =
    let original = CubicBezierData(point 0.0 0.0, point 35.0 65.0, point 90.0 -35.0, point 130.0 25.0)
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
        | CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic fit"
    let expected1, expected2 =
        match original with
        | CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic source"
    Assert.True(Point.distance control1 expected1 < 1.0e-9<length>)
    Assert.True(Point.distance control2 expected2 < 1.0e-9<length>)
    Assert.True(report.Max < 1.0e-9<length>)
    Assert.Equal(PositiveHandle, report.StartHandle)
    Assert.Equal(PositiveHandle, report.EndHandle)

[<Fact>]
let ``endpoint tangent fitting uses the forward end tangent`` () =
    let original = CubicBezierData(point 0.0 0.0, point 10.0 20.0, point 80.0 40.0, point 100.0 0.0)
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
let ``endpoint tangent fitting accepts small well-conditioned equations`` () =
    let original = CubicBezierData(point 0.0 0.0, point 2.0 3.0, point 7.0 -2.0, point 10.0 1.0)
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
    | CubicBezierData(_, expected1, expected2, _), CubicBezierData(_, actual1, actual2, _) ->
        Assert.True(Point.distance expected1 actual1 < 1.0e-6<length>)
        Assert.True(Point.distance expected2 actual2 < 1.0e-6<length>)
    | _ -> Assert.Fail "expected cubic curves"
    Assert.Equal(PositiveHandle, report.StartHandle)
    Assert.Equal(PositiveHandle, report.EndHandle)

[<Fact>]
let ``endpoint tangent fitting clamps negative handles`` () =
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
    | CubicBezierData(_, control1, control2, _) ->
        Assert.True(control1.X >= startPoint.X)
        Assert.True(control2.X <= endPoint.X)
    | _ -> Assert.Fail "expected cubic fit"
    Assert.Equal(CollapsedHandle, report.StartHandle)

[<Fact>]
let ``endpoint tangent fitting rejects degenerate tangent`` () =
    Assert.Equal(
        Error DegenerateTangent,
        Bezier.fitCubicWithEndpointTangents
            (point 0.0 0.0) (point 10.0 0.0)
            (point 0.0 0.0) (point 1.0 0.0)
            [ parameter 0.5, point 5.0 1.0 ])

[<Fact>]
let ``endpoint tangent fitting rejects underdetermined samples`` () =
    Assert.Equal(
        Error UnderdeterminedCubicFit,
        Bezier.fitCubicWithEndpointTangents
            (point 0.0 0.0) (point 10.0 0.0)
            (point 1.0 0.0) (point 1.0 0.0)
            [])

[<Fact>]
let ``endpoint-only fitting recovers an exact cubic`` () =
    let original = CubicBezierData(point 0.0 0.0, point 35.0 65.0, point 90.0 -35.0, point 130.0 25.0)
    let samples =
        [ parameter 0.25, Bezier.point original (parameter 0.25)
          parameter 0.5, Bezier.point original (parameter 0.5)
          parameter 0.75, Bezier.point original (parameter 0.75) ]
    let fit, report =
        Bezier.fitCubicWithEndpoints (Bezier.start original) (Bezier.finish original) samples
        |> Result.defaultWith (failwithf "%A")
    let control1, control2 =
        match fit with
        | CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic fit"
    let expected1, expected2 =
        match original with
        | CubicBezierData(_, control1, control2, _) -> control1, control2
        | _ -> failwith "expected cubic source"
    Assert.True(Point.distance control1 expected1 < 1.0e-9<length>)
    Assert.True(Point.distance control2 expected2 < 1.0e-9<length>)
    Assert.True(report.Max < 1.0e-9<length>)

[<Fact>]
let ``endpoint-only fitting reports noisy samples`` () =
    let original = CubicBezierData(point 0.0 0.0, point 10.0 30.0, point 80.0 -10.0, point 100.0 0.0)
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
let ``endpoint-only fitting accepts small well-conditioned equations`` () =
    let original = CubicBezierData(point 0.0 0.0, point 2.0 3.0, point 7.0 -2.0, point 10.0 1.0)
    let samples =
        [ parameter 0.0001, Bezier.point original (parameter 0.0001)
          parameter 0.0002, Bezier.point original (parameter 0.0002) ]
    let fit, report =
        Bezier.fitCubicWithEndpoints (Bezier.start original) (Bezier.finish original) samples
        |> Result.defaultWith (failwithf "%A")
    match original, fit with
    | CubicBezierData(_, expected1, expected2, _), CubicBezierData(_, actual1, actual2, _) ->
        Assert.True(Point.distance expected1 actual1 < 1.0e-6<length>)
        Assert.True(Point.distance expected2 actual2 < 1.0e-6<length>)
    | _ -> Assert.Fail "expected cubic curves"
    Assert.Equal(UnconstrainedHandle, report.StartHandle)
    Assert.Equal(UnconstrainedHandle, report.EndHandle)

[<Fact>]
let ``endpoint-only fitting rejects underdetermined samples`` () =
    Assert.Equal(
        Error UnderdeterminedCubicFit,
        Bezier.fitCubicWithEndpoints
            (point 0.0 0.0) (point 10.0 0.0)
            [ parameter 0.5, point 5.0 1.0 ])

[<Fact>]
let ``cubic self intersection finds an interior crossing`` () =
    let curve =
        CubicBezierData(
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
let ``cubic self intersection options carry length units`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    let options =
        { MinimumArcLengthSeparation = 301.0<length>
          DistanceTolerance = 1.0e-6<length> }
    Assert.Empty(Bezier.cubicSelfIntersectionsWith curve options |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``split permits endpoint parameters`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let zeroStart, wholeAfter = Bezier.split curve (parameter 0.0)
    let wholeBefore, zeroEnd = Bezier.split curve (parameter 1.0)
    Assert.Equal(point 0.0 0.0, Bezier.start zeroStart)
    Assert.Equal(point 0.0 0.0, Bezier.finish zeroStart)
    Assert.Equal(curve, wholeAfter)
    Assert.Equal(curve, wholeBefore)
    Assert.Equal(point 30.0 0.0, Bezier.start zeroEnd)
    Assert.Equal(point 30.0 0.0, Bezier.finish zeroEnd)

[<Fact>]
let ``inside split rejects parameters outside the curve`` () =
    let curve = LinearBezierData(point 0.0 0.0, point 10.0 20.0)
    Assert.Equal(Error SplitOutsideBezier, Bezier.splitInside curve (parameter -0.01))
    Assert.Equal(Error SplitOutsideBezier, Bezier.splitInside curve (parameter 1.01))
    Assert.True(Bezier.splitInside curve (parameter 0.0) |> Result.isOk)
    Assert.True(Bezier.splitInside curve (parameter 1.0) |> Result.isOk)

[<Fact>]
let ``unrestricted multi-split extrapolates and retains boundary parameters`` () =
    let curve = LinearBezierData(point 0.0 0.0, point 40.0 0.0)
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
let ``inside multi-split rejects any outside parameter`` () =
    let curve = LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    Assert.Equal(Error SplitOutsideBezier, Bezier.splitInsideMany curve [ parameter 0.25; parameter 1.01 ])
    Assert.Equal(Error SplitOutsideBezier, Bezier.splitInsideMany curve [ parameter -0.01; parameter 0.75 ])

[<Fact>]
let ``inside multi-split trims endpoints and duplicate parameters`` () =
    let curve = LinearBezierData(point 0.0 0.0, point 40.0 0.0)
    let pieces =
        Bezier.splitInsideMany curve [ parameter 1.0; parameter 0.0; parameter 0.5; parameter 0.5 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length pieces)
    Assert.Equal(point 0.0 0.0, Bezier.start pieces[0])
    Assert.Equal(point 20.0 0.0, Bezier.finish pieces[0])
    Assert.Equal(point 20.0 0.0, Bezier.start pieces[1])
    Assert.Equal(point 40.0 0.0, Bezier.finish pieces[1])

[<Fact>]
let ``multi-split preserves cubic degree`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    let pieces = Bezier.splitMany curve [ parameter 0.25; parameter 0.75 ]
    Assert.Equal(3, List.length pieces)
    Assert.All(pieces, fun piece ->
        match piece with
        | CubicBezierData _ -> ()
        | _ -> Assert.Fail "expected cubic piece")

[<Fact>]
let ``inflection parameters are independent of coordinate scale`` () =
    let scale = 1.0e-9
    let curve = CubicBezierData(point 0.0 0.0, point 0.0 (100.0 * scale), point (100.0 * scale) (-100.0 * scale), point (100.0 * scale) 0.0)
    let root = Bezier.cubicInflectionParameters curve |> List.exactlyOne
    Assert.Equal(0.5, Parameter.ratio root, 12)

[<Fact>]
let ``inflection search ignores non-inflecting curves`` () =
    let cubic = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Empty(Bezier.cubicInflectionParameters cubic)

[<Fact>]
let ``inflection search ignores non-cubic curves`` () =
    let quadratic = QuadraticBezierData(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    Assert.Empty(Bezier.cubicInflectionParameters quadratic)

[<Fact>]
let ``self intersection detects a closed cubic loop`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    let intersection = Bezier.cubicSelfIntersections curve |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(0.0, Parameter.ratio intersection.S, 12)
    Assert.Equal(1.0, Parameter.ratio intersection.T, 12)
    Assert.Equal(point 0.0 0.0, intersection.Point)

[<Fact>]
let ``self intersection is independent of coordinate scale`` () =
    let scale = 1.0e-12
    let curve =
        CubicBezierData(
            point 0.0 0.0,
            point (-0.2708333333333333 * scale) (-0.3333333333333333 * scale),
            point (-0.5416666666666666 * scale) (-0.3333333333333333 * scale),
            point (0.1875 * scale) 0.0)
    let options = { MinimumArcLengthSeparation = 1.0e-15<length>; DistanceTolerance = 1.0e-15<length> }
    let intersection = Bezier.cubicSelfIntersectionsWith curve options |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(0.25, Parameter.ratio intersection.S, 10)
    Assert.Equal(0.75, Parameter.ratio intersection.T, 10)

[<Fact>]
let ``self intersection ignores non-looping cubic`` () =
    let cubic = CubicBezierData(point 0.0 0.0, point 0.0 30.0, point 30.0 30.0, point 30.0 0.0)
    Assert.Empty(Bezier.cubicSelfIntersections cubic |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``self intersection ignores non-cubic curves`` () =
    let line = LinearBezierData(point 0.0 0.0, point 10.0 0.0)
    let quadratic = QuadraticBezierData(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    Assert.Empty(Bezier.cubicSelfIntersections line |> Result.defaultWith (failwithf "%A"))
    Assert.Empty(Bezier.cubicSelfIntersections quadratic |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``self intersection rejects invalid options`` () =
    let curve = CubicBezierData(point 0.0 0.0, point 100.0 100.0, point -100.0 100.0, point 0.0 0.0)
    Assert.Equal(
        Error(InvalidCubicSelfIntersectionMinimumArcLengthSeparation 0.0<length>),
        Bezier.cubicSelfIntersectionsWith curve { MinimumArcLengthSeparation = 0.0<length>; DistanceTolerance = 1.0e-6<length> })
    Assert.Equal(
        Error(InvalidCubicSelfIntersectionDistanceTolerance 0.0<length>),
        Bezier.cubicSelfIntersectionsWith curve { MinimumArcLengthSeparation = 1.0e-6<length>; DistanceTolerance = 0.0<length> })
