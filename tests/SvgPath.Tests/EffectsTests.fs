module SvgPath.Tests.EffectsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private rightAngle () =
    Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ]
    |> Result.defaultWith (failwithf "%A")
let private arcCount (subpath: Subpath) =
    subpath.Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length

[<Fact>]
let ``stretch policy joins at midpoint`` () =
    let source =
        Subpath.createWith
            (Effects.stretchToJoinEndpointPolicy ())
            [ Line(point 0.0 0.0, point 1.0 0.0)
              Line(point 3.0 0.0, point 4.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 2.0 0.0, Segment.finish source.Segments[0])
    Assert.Equal(point 2.0 0.0, Segment.start source.Segments[1])

[<Fact>]
let ``round open polyline inserts circular arc`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 4.0 0.0); Line(point 4.0 0.0, point 4.0 4.0) ]
        |> Result.defaultWith (failwithf "%A")
    let rounded = Effects.roundSubpathCorners source 1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, rounded.Segments.Length)
    Assert.True(match rounded.Segments[1] with Arc arc -> arc.Radius = point 1.0 1.0 | _ -> false)

[<Fact>]
let ``round closed square rounds four corners`` () =
    let source = Subpath.polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 4.0; point 0.0 4.0 ] |> Result.defaultWith (failwithf "%A")
    let rounded = Effects.roundSubpathCorners source 0.5<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(rounded.Closed)
    Assert.Equal(4, rounded.Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length)

[<Fact>]
let ``invalid effect tolerances are rejected`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let invalid = { Effects.defaultRoundCornerOptions with AngularTolerance = -1.0<degree> }
    Assert.Equal(Error(InvalidAngularTolerance -1.0<degree>), Effects.roundSubpathCornersWith source 1.0<length> invalid)

[<Fact>]
let ``degeneracy effect delegates to normalizer`` () =
    let source = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 1.0 0.001, point 2.0 0.0))
    let normalized = Effects.normalizeDegenerateSegments source 0.01<length> |> Result.defaultWith (failwithf "%A")
    Assert.All(normalized.Segments, fun segment -> Assert.True(match segment with Line _ -> true | _ -> false))

[<Fact>]
let ``normalize degenerate segments rejects nonfinite tolerance`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    Assert.True(Effects.normalizeDegenerateSegments source (Length.fromFloat System.Double.PositiveInfinity) |> Result.isError)
    Assert.True(Effects.normalizeDegenerateSegments source (Length.fromFloat System.Double.NaN) |> Result.isError)

[<Fact>]
let ``angular tolerance controls corner eligibility in degrees`` () =
    let source = rightAngle ()
    let skipped = { Effects.defaultRoundCornerOptions with Failure = LeaveCorner; AngularTolerance = 90.0<degree> }
    let rounded = { skipped with AngularTolerance = 89.999<degree> }
    Assert.Equal(Ok source, Effects.roundSubpathCornersWith source 2.0<length> skipped)
    Assert.Equal(1, Effects.roundSubpathCornersWith source 2.0<length> rounded |> Result.defaultWith (failwithf "%A") |> arcCount)

[<Fact>]
let ``distance tolerance controls minimum trim distance`` () =
    let source = rightAngle ()
    let skipped = { Effects.defaultRoundCornerOptions with Failure = LeaveCorner; DistanceTolerance = 2.0<length> }
    let rounded = { skipped with DistanceTolerance = 1.999<length> }
    Assert.Equal(Ok source, Effects.roundSubpathCornersWith source 2.0<length> skipped)
    Assert.Equal(1, Effects.roundSubpathCornersWith source 2.0<length> rounded |> Result.defaultWith (failwithf "%A") |> arcCount)

[<Fact>]
let ``round corners supports curve incident segments`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 10.0 0.0)
              QuadraticBezier(point 10.0 0.0, point 10.0 10.0, point 20.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let rounded = Effects.roundSubpathCorners source 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, rounded.Segments.Length)
    Assert.Equal(1, arcCount rounded)
    Assert.Contains(rounded.Segments, function QuadraticBezier _ -> true | _ -> false)

[<Fact>]
let ``round corners rounds closed one segment cusp`` () =
    let source =
        Subpath.ofSegment (CubicBezier(point 0.0 0.0, point -40.0 -30.0, point -40.0 30.0, point 0.0 0.0))
        |> Subpath.setClosed true
        |> Result.defaultWith (failwithf "%A")
    let options = { Effects.defaultRoundCornerOptions with Failure = AdaptRadius }
    let rounded = Effects.roundSubpathCornersWith source 4.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.True rounded.Closed
    Assert.Equal(2, rounded.Segments.Length)
    Assert.Equal(1, arcCount rounded)
    Assert.Contains(rounded.Segments, function CubicBezier _ -> true | _ -> false)

[<Fact>]
let ``stretch policy closes by dragging last end`` () =
    let a, b, c, nearA = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 1.0 0.0
    let source =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, nearA) ]
        |> Result.defaultWith (failwithf "%A")
    let closed =
        Subpath.setClosedWith (Effects.stretchToJoinEndpointPolicy ()) true source
        |> Result.defaultWith (failwithf "%A")
    Assert.True closed.Closed
    Assert.Equal<Segment list>([ Line(a, b); Line(b, c); Line(c, a) ], closed.Segments)

[<Fact>]
let ``stretch policy closes near loop single segment`` () =
    let a, nearA = point 0.0 0.0, point 0.01 0.0
    let closed =
        Subpath.ofSegment (Line(a, nearA))
        |> Subpath.setClosedWith (Effects.stretchToJoinEndpointPolicy ()) true
        |> Result.defaultWith (failwithf "%A")
    Assert.True closed.Closed
    Assert.Equal<Segment list>([ Line(a, a) ], closed.Segments)

[<Fact>]
let ``round corners errors when radius does not fit`` () =
    let actual = Effects.roundSubpathCorners (rightAngle ()) 20.0<length>
    Assert.Equal(Error(CannotRoundCorner 0), actual)

[<Fact>]
let ``round corners can leave unfittable corner`` () =
    let source = rightAngle ()
    let options = { Effects.defaultRoundCornerOptions with Failure = LeaveCorner }
    let actual = Effects.roundSubpathCornersWith source 20.0<length> options
    Assert.Equal(Ok source, actual)

[<Fact>]
let ``round corners can adapt radius to fit short segments`` () =
    let square =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let options = { Effects.defaultRoundCornerOptions with Failure = AdaptRadius }
    let rounded = Effects.roundSubpathCornersWith square 20.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.True rounded.Closed
    Assert.Equal(8, rounded.Segments.Length)
    Assert.Equal(4, arcCount rounded)
    rounded.Segments
    |> List.choose (function Arc arc -> Some arc.Radius.X | _ -> None)
    |> List.iter (fun radius -> Assert.True(abs (radius - 4.999999<length>) <= 1.0e-6<length>))

[<Fact>]
let ``normalize degenerate segments preserves closed one line replacement`` () =
    let source =
        Subpath.create
            [ QuadraticBezier(point 0.0 0.0, point 5.0 0.0001, point 10.0 0.0)
              Line(point 10.0 0.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let cleaned = Effects.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.True cleaned.Closed
    Assert.Equal(3, cleaned.Segments.Length)
    Assert.Contains(Line(point 0.0 0.0, point 10.0 0.0), cleaned.Segments)

[<Fact>]
let ``normalize degenerate segments coalesces thin line window`` () =
    let source =
        Subpath.polyline [ point 0.0 0.0; point 1.0 0.0; point 2.0 0.0; point 3.0 0.0; point 4.0 0.0 ]
        |> Result.defaultWith (failwithf "%A")
    let cleaned = Effects.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ Line(point 0.0 0.0, point 4.0 0.0) ], cleaned.Segments)

[<Fact>]
let ``normalize degenerate segments preserves closed two line backtracking`` () =
    let source =
        Subpath.create
            [ QuadraticBezier(point 0.0 0.0, point 5.0 0.0, point 0.0 0.0)
              Line(point 0.0 0.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let cleaned = Effects.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.True cleaned.Closed
    Assert.Equal(4, cleaned.Segments.Length)
    Assert.Contains(Line(point 0.0 0.0, point 2.5 0.0), cleaned.Segments)
    Assert.Contains(Line(point 2.5 0.0, point 0.0 0.0), cleaned.Segments)

[<Fact>]
let ``normalize degenerate segments keeps closed three line traversal`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 0.0 0.0)
              Line(point 0.0 0.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let cleaned = Effects.normalizeDegenerateSegments source 0.001<length> |> Result.defaultWith (failwithf "%A")
    Assert.True cleaned.Closed
    Assert.Equal(4, cleaned.Segments.Length)
