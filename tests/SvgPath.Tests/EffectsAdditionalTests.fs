module SvgPath.Tests.EffectsAdditionalTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``invalid round-corner angular tolerance is rejected`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let invalid = { Effects.defaultRoundCornerOptions with AngularTolerance = -1.0<degree> }
    Assert.Equal(Error(InvalidAngularTolerance -1.0<degree>), Effects.roundSubpathCornersWith source 1.0<length> invalid)

[<Fact>]
let ``degeneracy effect converts a nearly linear quadratic`` () =
    let source = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 1.0 0.001, point 2.0 0.0))
    let normalized = Effects.normalizeDegenerateSegments source 0.01<length> |> Result.defaultWith (failwithf "%A")
    Assert.All(normalized.Segments, fun segment -> Assert.True(match segment with Line _ -> true | _ -> false))
