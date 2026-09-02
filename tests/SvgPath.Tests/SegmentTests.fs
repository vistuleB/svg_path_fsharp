module SvgPath.Tests.SegmentTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``strict subpath construction rejects a discontinuity`` () =
    let a, b, c, d = point 0.0 0.0, point 1.0 0.0, point 2.0 0.0, point 3.0 0.0

    match Subpath.create [ Line(a, b); Line(c, d) ] with
    | Error(Discontinuous(0, 1, expected, actual, distance)) ->
        Assert.Equal(b, expected)
        Assert.Equal(c, actual)
        Assert.Equal(1.0<length>, distance)
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``custom replacement cannot change previous start`` () =
    let a, b, c, d = point 0.0 0.0, point 1.0 0.0, point 2.0 0.0, point 3.0 0.0
    let changedStart = point 0.0 1.0
    let policy = Custom(fun _ next _ -> [ Line(changedStart, c); next ])

    match Subpath.createWith policy [ Line(a, b); Line(c, d) ] with
    | Error(Discontinuous(-1, 0, expected, actual, _)) ->
        Assert.Equal(a, expected)
        Assert.Equal(changedStart, actual)
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``custom replacement may reconcile adjacent segments`` () =
    let a, b, c, d = point 0.0 0.0, point 1.0 0.0, point 2.0 0.0, point 3.0 0.0
    let policy = Custom(fun previous next _ -> [ Line(Segment.start previous, c); next ])

    match Subpath.createWith policy [ Line(a, b); Line(c, d) ] with
    | Ok subpath ->
        Assert.Equal(a, subpath.Start)
        Assert.Equal<Segment list>([ Line(a, c); Line(c, d) ], subpath.Segments)
        Assert.False(subpath.Closed)
    | Error error -> failwithf "%A" error

[<Fact>]
let ``closing custom replacement cannot change original subpath start`` () =
    let a, b = point 0.0 0.0, point 1.0 0.0
    let changedStart = point 0.0 1.0
    let source = Subpath.create [ Line(a, b) ] |> Result.defaultWith (failwithf "%A")
    let policy = Custom(fun _ _ _ -> [ Line(changedStart, changedStart) ])

    match Subpath.setClosedWith policy true source with
    | Error(Discontinuous(-1, 0, expected, actual, _)) ->
        Assert.Equal(a, expected)
        Assert.Equal(changedStart, actual)
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``strict closure requires the final endpoint to equal the start`` () =
    let a, b = point 0.0 0.0, point 1.0 0.0
    let source = Subpath.create [ Line(a, b) ] |> Result.defaultWith (failwithf "%A")

    match Subpath.setClosed true source with
    | Error(Discontinuous(0, 0, expected, actual, distance)) ->
        Assert.Equal(a, expected)
        Assert.Equal(b, actual)
        Assert.Equal(1.0<length>, distance)
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``bridge policy inserts a connector without moving endpoints`` () =
    let a, b, c, d = point 0.0 0.0, point 1.0 0.0, point 2.0 1.0, point 3.0 1.0
    let expected = [ Line(a, b); Line(b, c); Line(c, d) ]

    match Subpath.createWith Bridge [ Line(a, b); Line(c, d) ] with
    | Ok subpath -> Assert.Equal<Segment list>(expected, subpath.Segments)
    | Error error -> failwithf "%A" error

[<Fact>]
let ``wiggle preserves parallel horizontal lines by bridging`` () =
    let a, b, c, d = point 0.0 0.0, point 1.0 0.0, point 1.0 0.0000000005, point 2.0 0.0000000005
    let expected = [ Line(a, b); Line(b, c); Line(c, d) ]

    match Subpath.createWith Wiggle [ Line(a, b); Line(c, d) ] with
    | Ok subpath -> Assert.Equal<Segment list>(expected, subpath.Segments)
    | Error error -> failwithf "%A" error

[<Fact>]
let ``custom wiggle tolerance must be finite and nonnegative`` () =
    let segment = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error(InvalidWiggleTolerance -1.0<length>), Subpath.createWith (WiggleWith -1.0<length>) [ segment ])

[<Fact>]
let ``segment length and inversion use path-coordinate units`` () =
    let segment = Line(point 0.0 0.0, point 3.0 4.0)
    Assert.Equal(Ok 5.0<length>, Segment.length segment)
    Assert.Equal(Ok 0.4<parameter>, Segment.parameterAtLength segment 2.0<length>)
    let sample = Segment.pointAtLength segment 2.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(Point.distance sample (point 1.2 1.6) < 1.0e-12<length>)

[<Fact>]
let ``curved segment length inversion returns the requested prefix length`` () =
    let segment = QuadraticBezier(point 0.0 0.0, point 1.0 2.0, point 2.0 0.0)
    let options = { Segment.defaultLengthOptions with Tolerance = 1.0e-8<length> }
    let total = Segment.lengthWith segment options |> Result.defaultWith (fun error -> failwithf "%A" error)
    let t = Segment.parameterAtLengthWith segment (total / 2.0) options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.InRange(float t, 0.499999, 0.500001)

[<Fact>]
let ``subpath parameter at length crosses segment boundaries`` () =
    let subpath =
        Subpath.create [ Line(point 0.0 0.0, point 2.0 0.0); Line(point 2.0 0.0, point 2.0 3.0) ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(Ok 5.0<length>, Subpath.length subpath)
    Assert.Equal(
        Ok { SegmentIndex = 1; T = Parameter.fromFloat (1.0 / 3.0) },
        Subpath.parameterAtLength subpath 3.0<length>)

[<Fact>]
let ``segment ray crossings find line quadratic cubic and arc roots`` () =
    let direction x y = Point.create x y
    let crossing segment origin ray =
        Segment.rayCrossingsWith segment origin ray Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")

    let line = Line(point 10.0 -5.0, point 10.0 5.0)
    let lineT, lineRayT = crossing line (point 5.0 0.0) (direction 1.0 0.0) |> List.exactlyOne
    Assert.InRange(float lineT, 0.499999999, 0.500000001)
    Assert.InRange(float lineRayT, 4.999999999, 5.000000001)

    let quadratic = QuadraticBezier(point 0.0 0.0, point 10.0 10.0, point 20.0 0.0)
    let quadraticT, quadraticRayT = crossing quadratic (point 0.0 5.0) (direction 1.0 0.0) |> List.exactlyOne
    Assert.InRange(float quadraticT, 0.499999999, 0.500000001)
    Assert.InRange(float quadraticRayT, 9.999999999, 10.000000001)

    let cubic = CubicBezier(point 0.0 0.0, point 0.1 0.1, point 2.5 2.5, point 3.0 3.0)
    let cubicT, cubicRayT =
        crossing cubic (point 1.0 0.0) (direction 0.0 1.0)
        |> List.filter (fun (_, rayT) -> rayT > 0.0<length>)
        |> List.exactlyOne
    Assert.InRange(float cubicT, 0.411711, 0.411713)
    Assert.InRange(float cubicRayT, 0.999999, 1.000001)

    let arc =
        Arc
            { Start = point 0.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 20.0 0.0 }
    let arcT, arcRayT =
        crossing arc (point 10.0 -15.0) (direction 0.0 1.0)
        |> List.filter (fun (_, rayT) -> rayT > 0.0<length>)
        |> List.exactlyOne
    Assert.InRange(float arcT, 0.499999999, 0.500000001)
    Assert.InRange(float arcRayT, 4.999999999, 5.000000001)

[<Fact>]
let ``segment ray crossings retain negative ray parameters and reject zero direction`` () =
    let line = Line(point 10.0 -5.0, point 10.0 5.0)
    let crossings =
        Segment.rayCrossingsWith line (point 5.0 0.0) (Point.create -1.0 0.0) Segment.defaultCrossingOptions
        |> Result.defaultWith (failwithf "%A")
    let _, rayT = List.exactlyOne crossings
    Assert.InRange(float rayT, -5.000000001, -4.999999999)
    Assert.Equal(
        Error IndeterminateDirection,
        Segment.rayCrossingsWith line (point 5.0 0.0) (Point.create 0.0 0.0) Segment.defaultCrossingOptions)

[<Fact>]
let ``segment split and between extrapolate while inside variants reject`` () =
    let segment = Line(point 0.0 0.0, point 2.0 0.0)
    let left, right = Segment.split segment 1.5<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 3.0 0.0, Segment.finish left)
    Assert.Equal(point 3.0 0.0, Segment.start right)
    Assert.Equal(Error SplitOutsideSegment, Segment.splitInside segment 1.5<parameter>)
    let extrapolated = Segment.between segment -0.5<parameter> 1.5<parameter> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point -1.0 0.0, Segment.start extrapolated)
    Assert.Equal(point 3.0 0.0, Segment.finish extrapolated)
    Assert.Equal(Error SplitOutsideSegment, Segment.betweenInside segment -0.5<parameter> 1.0<parameter>)

[<Fact>]
let ``parametric subpath fits a straight interval`` () =
    let curve t = point t (2.0 * t)
    let subpath = Subpath.parametric 0.0 1.0 curve |> Result.defaultWith (failwithf "%A")
    Assert.False(Subpath.isClosed subpath)
    Assert.Equal(point 0.0 0.0, Subpath.start subpath)
    Assert.Equal(point 1.0 2.0, Subpath.finish subpath)
    Assert.All(Subpath.segments subpath, fun segment ->
        match segment with
        | CubicBezier _ -> ()
        | _ -> failwith "parametric pieces must be cubic Beziers")

[<Fact>]
let ``maximum-length subdivision preserves exact endpoints`` () =
    let segment = Line(point 0.0 0.0, point 5.0 0.0)
    let pieces = Segment.subdivideToMaxLength segment 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, List.length pieces)
    Assert.Equal(Segment.start segment, Segment.start (List.head pieces))
    Assert.Equal(Segment.finish segment, Segment.finish (List.last pieces))

[<Fact>]
let ``segment crossings and minimization match scalar sampling contracts`` () =
    let segment = Line(point 0.0 0.0, point 10.0 0.0)
    let crossings = Segment.crossings segment (fun sample -> sample.X - 4.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Single(crossings) |> ignore
    Assert.InRange(float crossings.Head, 0.399999999, 0.400000001)
    let minimum = Segment.minimize segment (fun sample -> (float sample.X - 7.0) ** 2.0) |> Result.defaultWith (failwithf "%A")
    Assert.InRange(float minimum, 0.699999999, 0.700000001)

[<Fact>]
let ``degenerate cubic preserves collinear backtracking`` () =
    let segment = CubicBezier(point 0.0 0.0, point 3.0 0.0, point -2.0 0.0, point 1.0 0.0)
    let replacement = Segment.degenerateLines segment 1.0e-9<length> |> Result.defaultWith (failwithf "%A")
    match replacement with
    | Some lines ->
        Assert.True(List.length lines >= 2)
        Assert.Equal(Segment.start segment, Segment.start lines.Head)
        Assert.Equal(Segment.finish segment, Segment.finish (List.last lines))
    | None -> failwith "collinear cubic should be detected as line-degenerate"

[<Fact>]
let ``setting an already closed subpath closed is idempotent`` () =
    let polygon = Subpath.polygon [ point 0.0 0.0; point 1.0 0.0; point 0.0 1.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok polygon, Subpath.setClosed true polygon)
