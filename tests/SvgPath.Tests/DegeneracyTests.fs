module SvgPath.Tests.DegeneracyTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``invalid tolerance is rejected`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    Assert.Equal(
        Error(DegeneracyPathError(InvalidLinearizeTolerance 0.0<length>)),
        Degeneracy.normalizeDegenerateSegments source 0.0<length>)

[<Fact>]
let ``near-collinear line window preserves axial backtracking`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 3.0 0.01)
              Line(point 3.0 0.01, point 1.0 -0.01)
              Line(point 1.0 -0.01, point 5.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let normalized =
        Degeneracy.normalizeDegenerateSegments source 0.03<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, List.length normalized.Segments)
    Assert.All(normalized.Segments, fun segment -> Assert.True(match segment with Line _ -> true | _ -> false))

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
