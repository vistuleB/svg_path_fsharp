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
