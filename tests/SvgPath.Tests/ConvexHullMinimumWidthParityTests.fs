module SvgPath.Tests.ConvexHullMinimumWidthParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private near expected actual = Assert.True(abs (actual - expected) <= 1.0e-9<length>)
let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)

let private polygon vertices =
    Subpath.polygon vertices |> Result.defaultWith (failwithf "%A")

let private width vertices =
    (ConvexHull.internalConvexPolygonMinimumWidthStrip vertices).Width

let private rectangleAtAngle center length width angle =
    let along = Point.direction (Degree.fromFloat angle) |> Point.scale (Length.fromFloat (length / 2.0))
    let across =
        Point.rotateCounterclockwise along
        |> Point.normalize
        |> Option.defaultWith (fun () -> failwith "degenerate rectangle direction")
        |> Point.scale (Length.fromFloat (width / 2.0))
    [ center |> Point.translate (Point.negate along) |> Point.translate (Point.negate across)
      center |> Point.translate along |> Point.translate (Point.negate across)
      center |> Point.translate along |> Point.translate across
      center |> Point.translate (Point.negate along) |> Point.translate across ]

let private assertFits vertices tolerance maxDepth =
    match ConvexHull.internalConvexPolygonMinimumWidthDecision vertices tolerance maxDepth with
    | MinimumWidthFits strip ->
        Assert.True(width vertices <= tolerance)
        Assert.True(strip.Width <= tolerance)
    | result -> Assert.Fail($"expected fit, got {result}")

let private assertExceeds vertices tolerance maxDepth =
    match ConvexHull.internalConvexPolygonMinimumWidthDecision vertices tolerance maxDepth with
    | MinimumWidthExceeds lowerBound ->
        Assert.True(width vertices > tolerance)
        Assert.True(lowerBound > tolerance)
    | result -> Assert.Fail($"expected excess, got {result}")

let private circleSubpath radius =
    let right, left = point radius 0.0, point -radius 0.0
    Subpath.create
        [ Arc { Start = right; Radius = point radius radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = left }
          Arc { Start = left; Radius = point radius radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = right } ]
    |> Result.bind (Subpath.setClosed true)
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``point and line polygons have zero width`` () =
    near 0.0<length> (width [ point 3.0 4.0 ])
    near 0.0<length> (width [ point -2.0 1.0; point 5.0 4.0 ])

[<Fact>]
let ``rectangle width is its shorter side`` () =
    near 2.0<length> (width [ point 0.0 0.0; point 7.0 0.0; point 7.0 2.0; point 0.0 2.0 ])

[<Fact>]
let ``rotated rectangle width is its shorter side`` () =
    near (sqrt 2.0 * 1.0<length>) (width [ point 0.0 0.0; point 4.0 4.0; point 3.0 5.0; point -1.0 1.0 ])

[<Fact>]
let ``triangle width is its shortest altitude`` () =
    near 2.4<length> (width [ point 0.0 0.0; point 4.0 0.0; point 0.0 3.0 ])

[<Fact>]
let ``width tolerates duplicates and traversal changes`` () =
    let original = [ point 0.0 0.0; point 6.0 0.0; point 6.0 2.0; point 0.0 2.0 ]
    let shifted = [ point 16.0 -1.0; point 16.0 1.0; point 10.0 1.0; point 10.0 -1.0; point 10.0 -1.0; point 16.0 -1.0 ]
    near (width original) (width shifted)
    near (width original) (width (List.rev original))

[<Fact>]
let ``five way search accepts a rotated thin rectangle`` () =
    assertFits (rectangleAtAngle (point 3.0 -2.0) 12.0 0.4 31.7) 0.401<length> 5

[<Fact>]
let ``five way search finds a minimum across the angle seam`` () =
    assertFits (rectangleAtAngle (point -5.0 8.0) 9.0 0.3 89.3) 0.301<length> 6

[<Fact>]
let ``five way search rejects from the support inventory`` () =
    assertExceeds (rectangleAtAngle (point 0.0 0.0) 8.0 1.5 19.0) 1.49<length> 5

[<Fact>]
let ``five way search handles an irregular convex polygon`` () =
    let vertices = [ point -4.0 -1.0; point -1.0 -3.0; point 4.0 -2.0; point 6.0 1.0; point 2.0 4.0; point -3.0 3.0 ]
    let exact = width vertices
    assertFits vertices (exact + 0.01<length>) 6
    assertExceeds vertices (exact - 0.01<length>) 6

[<Fact>]
let ``five way search does not guess at the exact threshold`` () =
    let vertices = rectangleAtAngle (point 0.0 0.0) 7.0 2.0 13.0
    match ConvexHull.internalConvexPolygonMinimumWidthDecision vertices (width vertices) 3 with
    | MinimumWidthUnresolved _ -> ()
    | result -> Assert.Fail($"expected unresolved, got {result}")

[<Fact>]
let ``five way decisions are translation and reversal invariant`` () =
    let original = rectangleAtAngle (point 0.0 0.0) 10.0 0.75 47.0
    let translated = original |> List.map (Point.translate (point 120.0 -90.0)) |> List.rev
    for vertices in [ original; translated ] do
        assertFits vertices 0.751<length> 6
        assertExceeds vertices 0.749<length> 6

[<Fact>]
let ``curved circle hull uses exact directional support`` () =
    let hull = ConvexHull.subpathHull (circleSubpath 2.0) |> Result.defaultWith (failwithf "%A")
    match ConvexHull.internalConvexSubpathMinimumWidthDecision hull 4.001<length> with
    | Ok(MinimumWidthFits strip) -> near 4.0<length> strip.Width
    | result -> Assert.Fail($"expected fit, got {result}")
    match ConvexHull.internalConvexSubpathMinimumWidthDecision hull 3.999<length> with
    | Ok(MinimumWidthExceeds lowerBound) -> Assert.True(lowerBound > 3.999<length>)
    | result -> Assert.Fail($"expected excess, got {result}")

[<Fact>]
let ``curved hull search certifies an arbitrary line at graph tolerance`` () =
    let finish = Point.direction 31.7<degree> |> Point.scale 10.0<length>
    let hull = ConvexHull.segmentHull (Line(point 0.0 0.0, finish)) |> Result.defaultWith (failwithf "%A")
    match ConvexHull.internalConvexSubpathMinimumWidthDecision hull 1.0e-9<length> with
    | Ok(MinimumWidthFits strip) -> Assert.True(strip.Width <= 1.0e-9<length>)
    | result -> Assert.Fail($"expected fit, got {result}")

[<Fact>]
let ``adding a segment returns the augmented hull and width decision`` () =
    let first, second, third = line 0.0 0.0 1.0 0.0, line 1.0 0.0 2.0 0.0, line 2.0 0.0 2.0 2.0
    let firstHull = ConvexHull.segmentHull first |> Result.defaultWith (failwithf "%A")
    let secondHull, secondDecision = ConvexHull.internalConvexSubpathAddSegmentAndTestWidth firstHull second 0.01<length> |> Result.defaultWith (failwithf "%A")
    match secondDecision with MinimumWidthFits strip -> Assert.True(strip.Width <= 0.01<length>) | result -> Assert.Fail($"expected fit, got {result}")
    let _, thirdDecision = ConvexHull.internalConvexSubpathAddSegmentAndTestWidth secondHull third 0.01<length> |> Result.defaultWith (failwithf "%A")
    match thirdDecision with MinimumWidthExceeds _ -> () | result -> Assert.Fail($"expected excess, got {result}")

[<Fact>]
let ``public minimum width finds rotated rectangle thickness`` () =
    let source = rectangleAtAngle (point 3.0 -2.0) 12.0 0.4 31.7 |> polygon
    let result = ConvexHull.subpathMinimumWidthWith source { Accuracy = 1.0e-6<length>; MaxDepth = 12 } |> Result.defaultWith (failwithf "%A")
    Assert.True result.Converged
    Assert.True(abs (result.Width - 0.4<length>) <= 1.0e-6<length>)
    Assert.True(
        result.LowerBound <= 0.4<length> && result.UpperBound >= 0.4<length>,
        $"bounds {result.LowerBound} .. {result.UpperBound}")

[<Fact>]
let ``public diameter returns witness pair and midpoint`` () =
    let result = ConvexHull.subpathDiameterWith (polygon [ point 0.0 0.0; point 3.0 4.0; point 0.0 1.0 ]) { Accuracy = 1.0e-6<length>; MaxDepth = 12 } |> Result.defaultWith (failwithf "%A")
    Assert.True result.Converged
    near 5.0<length> result.Width
    near 5.0<length> (Point.distance result.LowerPoint result.UpperPoint)
    Assert.True(Point.distance result.Center (point 1.5 2.0) <= 1.0e-6<length>)

[<Fact>]
let ``path diameter uses direct support across move only subpaths`` () =
    let source = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); Subpath.empty (point 3.0 4.0) ]
    let result = ConvexHull.pathDiameter source |> Result.defaultWith (failwithf "%A")
    Assert.True result.Converged
    near 5.0<length> result.Width
    near 5.0<length> (Point.distance result.LowerPoint result.UpperPoint)

[<Fact>]
let ``width extremum reports depth limit before convergence`` () =
    let source = rectangleAtAngle (point 0.0 0.0) 7.0 2.0 13.0 |> polygon
    let result = ConvexHull.subpathMinimumWidthWith source { Accuracy = 0.0<length>; MaxDepth = 0 } |> Result.defaultWith (failwithf "%A")
    Assert.False result.Converged
    Assert.True(result.LowerBound < result.UpperBound)

[<Fact>]
let ``longest thin prefix stops before the first wide addition`` () =
    let first, second, third, fourth = line 0.0 0.0 1.0 0.0, line 1.0 0.0 2.0 0.0, line 2.0 0.0 2.0 2.0, line 2.0 2.0 3.0 2.0
    let source = Subpath.create [ first; second; third; fourth ] |> Result.defaultWith (failwithf "%A")
    let prefix = Degeneracy.internalLongestThinPrefix source 0.01<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ first; second ], prefix.Segments)
    Assert.Equal<Segment list>([ third; fourth ], prefix.Remaining)
    Assert.True prefix.Hull.IsSome
    Assert.True(prefix.Strip.Value.Width <= 0.01<length>)

[<Fact>]
let ``longest thin prefix can be empty`` () =
    let first = QuadraticBezier(point 0.0 0.0, point 0.5 1.0, point 1.0 0.0)
    let prefix = Degeneracy.internalLongestThinPrefix (Subpath.ofSegment first) 0.1<length> |> Result.defaultWith (failwithf "%A")
    Assert.Empty prefix.Segments
    Assert.Equal<Segment list>([ first ], prefix.Remaining)
    Assert.True prefix.Hull.IsNone
    Assert.True prefix.Strip.IsNone
