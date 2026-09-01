module SvgPath.Tests.TransformTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

let private assertPointNear expected actual =
    Assert.True(Point.distance expected actual <= 1.0e-8<length>, $"expected {expected}, got {actual}")

[<Fact>]
let ``matrix coefficients transform points with measured translations`` () =
    let transform = Transform.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length>
    Assert.Equal(point 30.0 40.0, Transform.point transform (point 2.0 3.0))

[<Fact>]
let ``point convenience transforms retain scalar roles`` () =
    Assert.Equal(point 7.0 -4.0, Transform.translatePoint (point 2.0 3.0) 5.0<length> -7.0<length>)
    Assert.Equal(point 8.0 12.0, Transform.scalePoint (point 2.0 3.0) 4.0)
    Assert.Equal(point 4.0 -6.0, Transform.scaleXYPoint (point 2.0 3.0) 2.0 -2.0)

[<Fact>]
let ``bounding box uses all transformed corners`` () =
    let box: BoundingBox = { Min = point 0.0 0.0; Max = point 2.0 1.0 }
    let transformed =
        Transform.boundingBox box (Transform.rotate (degrees 90.0))
        |> Result.defaultWith (failwithf "%A")
    assertPointNear (point -1.0 0.0) transformed.Min
    assertPointNear (point 0.0 2.0) transformed.Max

[<Fact>]
let ``line and Bezier segments transform control geometry`` () =
    let transform = Transform.chain (Transform.scale 2.0) (Transform.translate 3.0<length> 4.0<length>)
    let line = Line(point 0.0 0.0, point 1.0 2.0)
    let cubic = CubicBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 1.0, point 2.0 1.0)
    Assert.Equal(Ok(Line(point 3.0 4.0, point 5.0 8.0)), Transform.segment line transform)
    Assert.Equal(
        Ok(CubicBezier(point 3.0 4.0, point 5.0 4.0, point 5.0 6.0, point 7.0 6.0)),
        Transform.segment cubic transform)

[<Fact>]
let ``arc reflection reverses sweep`` () =
    let arc =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 3.0
              XAxisRotation = degrees 20.0
              LargeArc = false
              Sweep = true
              End = point 8.0 2.0 }
    let transformed =
        Transform.segment arc (Transform.scaleXY -1.0 1.0)
        |> Result.defaultWith (failwithf "%A")
    match transformed with
    | Arc endpoint ->
        Assert.False(endpoint.Sweep)
        Assert.Equal(point 0.0 0.0, endpoint.Start)
        Assert.Equal(point -8.0 2.0, endpoint.End)
    | _ -> failwith "expected arc"

[<Fact>]
let ``collapsed arc can be handled strictly or gracefully`` () =
    let arc =
        Arc
            { Start = point 0.0 0.0
              Radius = point 5.0 5.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 10.0 0.0 }
    let collapse = Transform.scaleXY 1.0 0.0
    Assert.Equal(Error DegenerateArcTransform, Transform.segment arc collapse)
    match Transform.segmentToSubpathGracefully arc collapse with
    | Ok subpath ->
        Assert.False(List.isEmpty subpath.Segments)
        Assert.Equal(point 0.0 0.0, subpath.Start)
        Assert.Equal(Some(point 10.0 0.0), Subpath.finish subpath)
    | Error error -> failwithf "%A" error

[<Fact>]
let ``subpath and path transforms preserve closure and ordering`` () =
    let first = Line(point 0.0 0.0, point 1.0 0.0)
    let second = Line(point 1.0 0.0, point 0.0 0.0)
    let closed = { Start = point 0.0 0.0; Segments = [ first; second ]; Closed = true }
    let path = { Subpaths = [ closed; { closed with Closed = false } ] }
    let transformed =
        Transform.translatePath path 4.0<length> 7.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length transformed.Subpaths)
    Assert.True(transformed.Subpaths.Head.Closed)
    Assert.False(transformed.Subpaths.Tail.Head.Closed)
    Assert.Equal(point 4.0 7.0, transformed.Subpaths.Head.Start)

[<Fact>]
let ``anchor transforms use geometry bounding boxes`` () =
    let line = Line(point 2.0 3.0, point 6.0 7.0)
    let rotated =
        Transform.segmentAboutAnchor line (Transform.rotate (degrees 180.0)) Center
        |> Result.defaultWith (failwithf "%A")
    match rotated with
    | Line(startPoint, endPoint) ->
        assertPointNear (point 6.0 7.0) startPoint
        assertPointNear (point 2.0 3.0) endPoint
    | _ -> failwith "expected line"

[<Fact>]
let ``invalid matrices are rejected before geometry transformation`` () =
    let invalid = Transform.scale infinity
    let line = Line(point 0.0 0.0, point 1.0 1.0)
    Assert.Equal(Error InvalidMatrix, Transform.segment line invalid)
