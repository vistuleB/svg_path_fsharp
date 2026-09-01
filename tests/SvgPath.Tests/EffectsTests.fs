module SvgPath.Tests.EffectsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

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
