module SvgPath.Tests.ConvexHullTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private near expected actual = Assert.True(abs (actual - expected) <= 1.0e-8<length>, $"expected {expected}, got {actual}")

[<Fact>]
let ``point hull rejects an empty collection`` () =
    Assert.Equal(Error(ConvexHullPathError EmptyPath), ConvexHull.pointsHull [])

[<Fact>]
let ``point hull removes interior points`` () =
    let hull =
        ConvexHull.pointsHull
            [ point 0.0 0.0; point 4.0 0.0; point 4.0 2.0; point 0.0 2.0; point 2.0 1.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Equal(4, List.length hull.Segments)

[<Fact>]
let ``rectangle minimum width is its short side`` () =
    let rectangle =
        Subpath.polygon [ point 0.0 0.0; point 7.0 0.0; point 7.0 2.0; point 0.0 2.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = ConvexHull.subpathMinimumWidth rectangle |> Result.defaultWith (failwithf "%A")
    near 2.0<length> result.Width
    Assert.True(result.Converged)

[<Fact>]
let ``triangle diameter returns witnesses and midpoint`` () =
    let triangle =
        Subpath.polygon [ point 0.0 0.0; point 3.0 4.0; point 0.0 1.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = ConvexHull.subpathDiameter triangle |> Result.defaultWith (failwithf "%A")
    near 5.0<length> result.Width
    Assert.Equal(Point.midpoint result.LowerPoint result.UpperPoint, result.Center)

[<Fact>]
let ``directional support API keeps width units`` () =
    let support direction =
        let lower = point 0.0 0.0
        let upper = Point.scale (2.0<length>) direction
        { LowerPoint = lower; UpperPoint = upper; Width = 2.0<length> }
    let result =
        ConvexHull.minimumWidthWith support 2.0<length>
            { Accuracy = 0.01<length>; MaxDepth = 4 }
    near 2.0<length> result.Width

[<Fact>]
let ``quadratic hull preserves the curve and closes with its chord`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let hull = ConvexHull.segmentHull curve |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ curve; Line(point 4.0 0.0, point 0.0 0.0) ], hull.Segments)
    Assert.True(hull.Closed)

[<Fact>]
let ``arc hull preserves the arc and closes with its chord`` () =
    let arc =
        Arc
            { Start = point -2.0 0.0
              Radius = point 2.0 2.0
              XAxisRotation = Degree.fromFloat 0.0
              LargeArc = false
              Sweep = true
              End = point 2.0 0.0 }
    let hull = ConvexHull.segmentHull arc |> Result.defaultWith (failwithf "%A")
    Assert.Equal(arc, List.head hull.Segments)
    Assert.Equal(Line(point 2.0 0.0, point -2.0 0.0), List.last hull.Segments)

[<Fact>]
let ``cubic hull retains cubic boundary pieces`` () =
    let cubic = CubicBezier(point 0.0 0.0, point 0.0 4.0, point 4.0 4.0, point 4.0 0.0)
    let hull = ConvexHull.segmentHull cubic |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function CubicBezier _ -> true | _ -> false)

[<Fact>]
let ``subpath hull preserves exposed source curves`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let source = Subpath.ofSegment curve
    let hull = ConvexHull.subpathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function QuadraticBezier _ -> true | _ -> false)

[<Fact>]
let ``adaptive search converges on a rotated rectangle`` () =
    let angle = Degree.fromFloat 31.7
    let along = Point.direction angle
    let across = Point.rotateClockwise along
    let center = point 3.0 -2.0
    let corner alongSign acrossSign =
        center
        |> Point.translate (Point.scale (alongSign * 6.0<length>) along)
        |> Point.translate (Point.scale (acrossSign * 0.2<length>) across)
    let vertices = [ corner -1.0 -1.0; corner 1.0 -1.0; corner 1.0 1.0; corner -1.0 1.0 ]
    let support direction =
        let ordered = vertices |> List.sortBy (fun vertex -> Point.dot vertex direction)
        { LowerPoint = List.head ordered
          UpperPoint = List.last ordered
          Width = Point.dot (List.last ordered) direction - Point.dot (List.head ordered) direction }
    let result =
        ConvexHull.minimumWidthWith support 13.0<length>
            { Accuracy = 1.0e-6<length>; MaxDepth = 12 }
    Assert.True(result.Converged)
    Assert.True(abs (result.Width - 0.4<length>) <= 1.0e-6<length>)
    Assert.True(result.LowerBound <= 0.400001<length>)
    Assert.True(result.UpperBound >= 0.399999<length>)

[<Fact>]
let ``path hull refines transitions between distinct source curves`` () =
    let upper = QuadraticBezier(point -4.0 0.0, point -2.0 -3.0, point 0.0 0.0)
    let lower = QuadraticBezier(point 0.0 0.0, point 2.0 3.0, point 4.0 0.0)
    let source = Path.ofSubpaths [ Subpath.ofSegment upper; Subpath.ofSegment lower ]
    let hull = ConvexHull.pathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True(hull.Closed)
    Assert.Contains(hull.Segments, function QuadraticBezier _ -> true | _ -> false)
    hull.Segments
    |> List.pairwise
    |> List.iter (fun (previous, next) -> Assert.Equal(Segment.finish previous, Segment.start next))
