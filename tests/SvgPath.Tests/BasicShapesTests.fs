module SvgPath.Tests.BasicShapesTests

open SvgPath
open Xunit

let private length value = Length.fromFloat value
let private point x y = Point.create (length x) (length y)

[<Fact>]
let ``rect converts to svg equivalent path`` () =
    let rectangle = BasicShapes.rect (length 10.0) (length 20.0) (length 100.0) (length 50.0) None None |> Result.defaultWith (failwithf "%A")
    Assert.True rectangle.Closed
    Assert.Equal(point 10.0 20.0, rectangle.Start)
    Assert.Equal<Segment list>(
        [ Line(point 10.0 20.0, point 110.0 20.0)
          Line(point 110.0 20.0, point 110.0 70.0)
          Line(point 110.0 70.0, point 10.0 70.0)
          Line(point 10.0 70.0, point 10.0 20.0) ],
        rectangle.Segments)

[<Fact>]
let ``rounded rect converts to svg equivalent path`` () =
    let rectangle = BasicShapes.rect (length 10.0) (length 20.0) (length 100.0) (length 50.0) (Some(length 10.0)) (Some(length 5.0)) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 20.0 20.0, rectangle.Start)
    Assert.Equal(8, List.length rectangle.Segments)
    let arcs = rectangle.Segments |> List.choose (function Arc arc -> Some arc | _ -> None)
    Assert.Equal(4, List.length arcs)
    Assert.True(arcs |> List.forall (fun arc -> arc.Radius = point 10.0 5.0 && arc.Sweep && not arc.LargeArc))

[<Fact>]
let ``rect uses single radius for both axes`` () =
    let copied = BasicShapes.rect (length 0.0) (length 0.0) (length 20.0) (length 20.0) (Some(length 5.0)) None |> Result.defaultWith (failwithf "%A")
    let firstArc (subpath: Subpath) = subpath.Segments |> List.pick (function Arc arc -> Some arc | _ -> None)
    Assert.Equal(point 5.0 5.0, (firstArc copied).Radius)

[<Fact>]
let ``rect clamps corner radii`` () =
    let clamped = BasicShapes.rect (length 0.0) (length 0.0) (length 20.0) (length 10.0) (Some(length 50.0)) (Some(length 50.0)) |> Result.defaultWith (failwithf "%A")
    let firstArc (subpath: Subpath) = subpath.Segments |> List.pick (function Arc arc -> Some arc | _ -> None)
    Assert.Equal(point 10.0 5.0, (firstArc clamped).Radius)

[<Fact>]
let ``circle converts to svg equivalent path`` () =
    let circle = BasicShapes.circle (length 10.0) (length 20.0) (length 5.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 15.0 20.0, circle.Start)
    Assert.Equal(4, List.length circle.Segments)

[<Fact>]
let ``ellipse converts to svg equivalent path`` () =
    let ellipse = BasicShapes.ellipse (length 10.0) (length 20.0) (length 7.0) (length 3.0) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 17.0 20.0, ellipse.Start)
    Assert.Equal(4, List.length ellipse.Segments)
    let midpoint = Segment.point (List.head ellipse.Segments) (Parameter.fromFloat 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance midpoint (point 14.9497474683 22.1213203436) < length 1.0e-9)

[<Fact>]
let ``line converts to subpath`` () =
    let line = BasicShapes.line (length 1.0) (length 2.0) (length 3.0) (length 4.0) |> Result.defaultWith (failwithf "%A")
    Assert.False line.Closed
    Assert.Equal(1, List.length line.Segments)

[<Fact>]
let ``polyline converts points to open subpath`` () =
    let points = [ point 1.0 2.0; point 3.0 4.0; point 5.0 4.0 ]
    let polyline = BasicShapes.polyline points |> Result.defaultWith (failwithf "%A")
    Assert.False polyline.Closed
    Assert.Equal(2, List.length polyline.Segments)

[<Fact>]
let ``polygon converts points to closed subpath`` () =
    let points = [ point 1.0 2.0; point 3.0 4.0; point 5.0 4.0 ]
    let polygon = BasicShapes.polygon points |> Result.defaultWith (failwithf "%A")
    Assert.True polygon.Closed
    Assert.Equal(3, List.length polygon.Segments)
    Assert.Equal(polygon.Start, polygon.Segments |> List.last |> Segment.finish)

[<Fact>]
let ``invalid dimensions return errors`` () =
    Assert.Equal(Error(InvalidRectWidth(length -1.0)), BasicShapes.rect (length 0.0) (length 0.0) (length -1.0) (length 2.0) None None)
    Assert.Equal(Error(InvalidCircleRadius(length -1.0)), BasicShapes.circle (length 0.0) (length 0.0) (length -1.0))
    Assert.Equal(Error(InvalidEllipseRadiusY(length -1.0)), BasicShapes.ellipse (length 0.0) (length 0.0) (length 1.0) (length -1.0))

[<Fact>]
let ``disabled rendering returns error`` () =
    Assert.Equal(Error DisabledRendering, BasicShapes.rect (length 0.0) (length 0.0) (length 0.0) (length 2.0) None None)
    Assert.Equal(Error DisabledRendering, BasicShapes.circle (length 0.0) (length 0.0) (length 0.0))
    Assert.Equal(Error DisabledRendering, BasicShapes.ellipse (length 0.0) (length 0.0) (length 1.0) (length 0.0))

[<Fact>]
let ``invalid point lists return core errors`` () =
    Assert.Equal(Error(PathError EmptySubpath), BasicShapes.polyline [])
    Assert.Equal(Error(PathError EmptySubpath), BasicShapes.polygon [ point 1.0 2.0 ])
