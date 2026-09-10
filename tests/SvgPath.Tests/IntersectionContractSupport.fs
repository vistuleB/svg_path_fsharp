module SvgPath.Tests.IntersectionContractSupport

open SvgPath
open Xunit

// Numerical candidates, not certified distinct mathematical roots.
let assertCandidates (hits: SegmentIntersection list) left right tolerance =
    for hit in hits do
        Assert.InRange(hit.LeftT, 0.0<parameter>, 1.0<parameter>)
        Assert.InRange(hit.RightT, 0.0<parameter>, 1.0<parameter>)
        let a = Segment.point left hit.LeftT |> Result.defaultWith (failwithf "%A")
        let b = Segment.point right hit.RightT |> Result.defaultWith (failwithf "%A")
        Assert.True(Point.distance a b <= tolerance)
        Assert.True(Point.distance a hit.Point <= tolerance)
        Assert.True(Point.distance b hit.Point <= tolerance)
    let rec separated (remaining: SegmentIntersection list) =
        match remaining with
        | [] -> ()
        | first::rest ->
            for other in rest do
                let separation = max (abs(first.LeftT-other.LeftT)) (abs(first.RightT-other.RightT))
                Assert.True(separation >= 1e-7<parameter> - 1e-15<parameter>)
            separated rest
    separated hits

let assertKnown (hits: SegmentIntersection list) leftT rightT tolerance =
    Assert.True(hits |> List.exists (fun hit ->
        abs(hit.LeftT-leftT) <= tolerance && abs(hit.RightT-rightT) <= tolerance))
