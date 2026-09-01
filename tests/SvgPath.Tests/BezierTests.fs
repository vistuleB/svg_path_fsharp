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
