module SvgPath.Tests.AffineAdditionalTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

[<Fact>]
let ``matrix tuple round trip preserves coefficient units`` () =
    let transform = Affine.matrix 1.0 2.0 3.0 4.0 5.0<length> 6.0<length>
    Assert.Equal((1.0, 2.0, 3.0, 4.0, 5.0<length>, 6.0<length>), Affine.toTuple transform)
    Assert.Equal(transform, Affine.fromTuple (Affine.toTuple transform))

[<Fact>]
let ``chain follows application order`` () =
    let transform = Affine.chain (Affine.translate 2.0<length> 3.0<length>) (Affine.scale 4.0)
    Assert.Equal(point 12.0 20.0, Affine.point transform (point 1.0 2.0))

[<Fact>]
let ``about point leaves its center fixed`` () =
    let center = point 3.0 4.0
    let transform = Affine.aboutPoint (Affine.rotate (degrees 90.0)) center
    Assert.Equal(center, Affine.point transform center)
    Assert.Equal(point 3.0 5.0, Affine.point transform (point 4.0 4.0))

[<Fact>]
let ``linear part preserves the input coordinate unit`` () =
    let derivative = Point.create 2.0<length / parameter> 3.0<length / parameter>
    let transformed: Point<length / parameter> = Affine.linearPoint (Affine.scaleXY 4.0 5.0) derivative
    Assert.Equal(Point.create 8.0<length / parameter> 15.0<length / parameter>, transformed)

[<Fact>]
let ``point_pair_similarity_reports_degenerate_source_test`` () =
    Assert.Equal(Error Affine.Error.DegenerateSourcePair, Affine.pointPairSimilarity (point 1.0 2.0) (point 1.0 2.0) (point 0.0 0.0) (point 1.0 0.0))

[<Fact>]
let ``point_triple_map_reports_degenerate_source_test`` () =
    let result = Affine.pointTripleMap (point 0.0 0.0) (point 1.0 0.0) (point 2.0 0.0) (point 0.0 0.0) (point 1.0 0.0) (point 0.0 1.0)
    Assert.Equal(Error Affine.Error.DegenerateSourceTriple, result)
    let origin = point 0.0 0.0
    Assert.Equal(Error Affine.Error.DegenerateSourceTriple,
        Affine.pointTripleMap origin origin origin origin origin origin)

[<Fact>]
let ``point_pair_similarity_reports_nonfinite_transform_test`` () =
    Assert.Equal(Error Affine.Error.NonFiniteTransform,
        Affine.pointPairSimilarity (point 1.0e200 0.0) (point 1.0e200 1.0) (point 0.0 0.0) (point 0.0 1.0e150))

[<Fact>]
let ``point_triple_map_reports_nonfinite_transform_test`` () =
    Assert.Equal(Error Affine.Error.NonFiniteTransform,
        Affine.pointTripleMap (point 0.0 0.0) (point 0.5 0.0) (point 0.0 0.5)
            (point 0.0 0.0) (point 1.0e308 0.0) (point 0.0 1.0e308))

[<Fact>]
let ``point_correspondence_maps_allow_collapsed_targets_test`` () =
    let target = point 2.0 3.0
    let pair = Affine.pointPairSimilarity (point 0.0 0.0) (point 1.0 0.0) target target
               |> Result.defaultWith (failwithf "%A")
    let triple = Affine.pointTripleMap (point 0.0 0.0) (point 1.0 0.0) (point 0.0 1.0) target target target
                 |> Result.defaultWith (failwithf "%A")
    Assert.Equal(target, Affine.point pair (point 5.0 6.0))
    Assert.Equal(target, Affine.point triple (point 5.0 6.0))

[<Fact>]
let ``finiteness includes length-valued translations`` () =
    Assert.True(Affine.isFinite (Affine.identity ()))
    Assert.False(Affine.isFinite (Affine.translate (Length.fromFloat infinity) 0.0<length>))
