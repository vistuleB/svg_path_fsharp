module SvgPath.Tests.DegeneracyTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``zero tolerance is accepted`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 1.0 0.0) ], normalized.Segments)

[<Fact>]
let ``negative tolerance is rejected`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    Assert.Equal(
        Error(DegeneracyInvalidTolerance -0.000001<length>),
        Degeneracy.normalizeDegenerateSegments source -0.000001<length>)

[<Fact>]
let ``zero tolerance collapses exactly collinear lines`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 1.0 0.0)
              Line(point 1.0 0.0, point 2.0 0.0)
              Line(point 2.0 0.0, point 3.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 3.0 0.0) ], normalized.Segments)

[<Fact>]
let ``zero tolerance keeps near-collinear lines`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 1.0 0.0)
              Line(point 1.0 0.0, point 2.0 1.0e-9)
              Line(point 2.0 1.0e-9, point 3.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>(source.Segments, normalized.Segments)

[<Fact>]
let ``zero tolerance collapses exactly collinear quadratic`` () =
    let source = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 3.0 3.0, point 6.0 6.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 6.0 6.0) ], normalized.Segments)

[<Fact>]
let ``zero tolerance keeps near-collinear quadratic`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 3.0 3.0000001, point 6.0 6.0)
    let source = Subpath.ofSegment curve
    let strict =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ curve ], strict.Segments)
    let loose =
        Degeneracy.normalizeDegenerateSegments source 0.000001<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 6.0 6.0) ], loose.Segments)

[<Fact>]
let ``zero tolerance collapses exactly collinear cubic`` () =
    let source = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 2.0, point 3.0 3.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 3.0 3.0) ], normalized.Segments)

[<Fact>]
let ``zero tolerance collapses horizontal cubic`` () =
    let source = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 1.0 0.0, point 2.0 0.0, point 3.0 0.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 3.0 0.0) ], normalized.Segments)

[<Fact>]
let ``zero tolerance collapses stretched diagonal cubic`` () =
    let source = Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 2.0 1.0, point 4.0 2.0, point 6.0 3.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 6.0 3.0) ], normalized.Segments)

[<Fact>]
let ``zero tolerance keeps near-collinear cubic`` () =
    let curve = CubicBezier(point 0.0 0.0, point 1.0 1.0, point 2.0 2.0000001, point 3.0 3.0)
    let source = Subpath.ofSegment curve
    let strict =
        Degeneracy.normalizeDegenerateSegments source 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ curve ], strict.Segments)
    let loose =
        Degeneracy.normalizeDegenerateSegments source 0.000001<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 3.0 3.0) ], loose.Segments)

[<Fact>]
let ``near-collinear line window preserves global extent rather than local backtracking`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 3.0 0.01)
              Line(point 3.0 0.01, point 1.0 -0.01)
              Line(point 1.0 -0.01, point 5.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.03<length>
        |> Result.defaultWith (failwithf "%A")
    // F#-specific coverage: the new support-based contract deliberately drops
    // local reversals between the global extrema (here the two endpoints).
    Assert.Equal<Segment list>([Line(point 0.0 0.0,point 5.0 0.0)],normalized.Segments)

[<Fact>]
let ``thin quadratic becomes a line traversal`` () =
    let source =
        Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 2.0 0.01, point 4.0 0.0))
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.02<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.All(normalized.Segments, fun segment -> Assert.True(match segment with Line _ -> true | _ -> false))

[<Fact>]
let ``wide quadratic is preserved`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 2.0 2.0, point 4.0 0.0)
    let source = Subpath.ofSegment curve
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.02<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ curve ], normalized.Segments)

[<Fact>]
let ``closedness survives normalization`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 1.0; point 0.0 1.0 ]
        |> Result.defaultWith (failwithf "%A")
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.01<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.True(normalized.Closed)

[<Fact>]
let ``longest thin prefix stops before first wide addition`` () =
    let first = Line(point 0.0 0.0, point 1.0 0.0)
    let second = Line(point 1.0 0.0, point 2.0 0.0)
    let third = Line(point 2.0 0.0, point 2.0 2.0)
    let fourth = Line(point 2.0 2.0, point 3.0 2.0)
    let source = Subpath.create [ first; second; third; fourth ] |> Result.defaultWith (failwithf "%A")
    let prefix = Degeneracy.internalLongestThinPrefix source 0.01<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ first; second ], prefix.Segments)
    Assert.Equal<Segment list>([ third; fourth ], prefix.Remaining)
    Assert.True(prefix.Hull.IsSome)
    Assert.True(prefix.Strip |> Option.exists (fun strip -> strip.Width <= 0.01<length>))

[<Fact>]
let ``longest thin prefix can be empty`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 0.5 1.0, point 1.0 0.0)
    let source = Subpath.ofSegment curve
    let prefix = Degeneracy.internalLongestThinPrefix source 0.1<length> |> Result.defaultWith (failwithf "%A")
    Assert.Empty(prefix.Segments)
    Assert.Equal<Segment list>([ curve ], prefix.Remaining)
    Assert.True(prefix.Hull.IsNone)
    Assert.True(prefix.Strip.IsNone)

[<Fact>]
let ``incremental convex hull retains a degenerate curve`` () =
    let curve = QuadraticBezier(point 0.0 0.0, point 5.0 0.0, point 0.0 0.0)
    let vertical = Line(point 0.0 0.0, point 0.0 10.0)
    let hull = ConvexHull.segmentHull curve |> Result.defaultWith (failwithf "%A")
    let combined, _ =
        ConvexHull.internalConvexSubpathAddSegmentAndTestWidth hull vertical 0.001<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Contains(combined.Segments, function QuadraticBezier _ -> true | _ -> false)

[<Fact>]
let ``thin line window coalesces`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 1.0 0.0)
              Line(point 1.0 0.0, point 2.0 0.0)
              Line(point 2.0 0.0, point 3.0 0.0)
              Line(point 3.0 0.0, point 4.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let normalized = Degeneracy.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 4.0 0.0) ], normalized.Segments)

[<Fact>]
let ``closed two-line backtracking is retained`` () =
    let source =
        Subpath.create
            [ QuadraticBezier(point 0.0 0.0, point 5.0 0.0, point 0.0 0.0)
              Line(point 0.0 0.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let normalized = Degeneracy.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(normalized.Closed)
    Assert.Equal(4, List.length normalized.Segments)
    Assert.Contains(Line(point 0.0 0.0, point 2.5 0.0), normalized.Segments)
    Assert.Contains(Line(point 2.5 0.0, point 0.0 0.0), normalized.Segments)
