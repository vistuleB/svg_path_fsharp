[<Xunit.Trait("Category", "Slow")>]
module SvgPath.Tests.PaperIntersectionAddressesSlowTests

open SvgPath
open Xunit

// Adapted from Paper.js Path_Intersections.js, revision
// 92775f5279c05fb7f0a743e9e7fa02cd40ec1e70 (MIT).
// Copyright 2011–2020 Jürg Lehni & Jonathan Puckey.
// License: docs/paperjs-matrix-LICENSE.txt.
// Mirrors the named test_slow module in the neighboring Gleam repository.
let private unwrap value = value |> Result.defaultWith (failwithf "%A")
let private p x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
let private closed segments = Subpath.create segments |> unwrap |> Subpath.close |> unwrap

let private check left right (hits: Intersections.PathIntersection list) expected =
    let a, b = Path.singleton left, Path.singleton right
    let tolerance = Intersections.defaultOptions.Tolerance
    let near x y = Point.distance x y <= tolerance
    let matches (address: SubpathParameter) index time =
        address.SegmentIndex = index &&
        (if time = 0.0 || time = 1.0 then float address.T = time
         else abs (float address.T - time) <= 0.0000051)
    for hit in hits do
        Assert.NotEmpty hit.LeftParameters
        Assert.NotEmpty hit.RightParameters
        for l in hit.LeftParameters do
            Assert.Equal(0, l.SubpathIndex)
            let lp = Path.point a l |> unwrap
            Assert.True(near lp hit.Point)
            for r in hit.RightParameters do
                Assert.Equal(0, r.SubpathIndex)
                let rp = Path.point b r |> unwrap
                Assert.True(near rp hit.Point)
                Assert.True(near lp rp)
                let classification = Intersections.classifySubpathIntersection left right l.At r.At |> unwrap
                Assert.NotEqual(Intersections.Indeterminate, classification)
    for point, index, time, crossing in expected do
        let candidates = hits |> List.filter (fun hit ->
            abs (hit.Point.X - point.X) <= 0.00001<length> &&
            abs (hit.Point.Y - point.Y) <= 0.00001<length> &&
            List.exists (fun (l: PathParameter) -> matches l.At index time) hit.LeftParameters)
        Assert.NotEmpty candidates
        for hit in candidates do
            for l in hit.LeftParameters |> List.filter (fun (l: PathParameter) -> matches l.At index time) do
                for r in hit.RightParameters do
                    let classification = Intersections.classifySubpathIntersection left right l.At r.At |> unwrap
                    match crossing, classification with
                    | true, Intersections.Crossing _
                    | false, Intersections.Touching _
                    | false, Intersections.EndpointContact _ -> ()
                    | _ -> failwithf "Unexpected classification %A for %A" classification point

[<Fact>]
let ``Paper address 12`` () =
    let left = closed [
        CubicBezier(p 432.0 424.0, p 432.0 454.93, p 406.93 480.0, p 376.0 480.0);
        CubicBezier(p 376.0 480.0, p 360.535 480.0, p 346.535 473.73, p 336.4 463.6);
        CubicBezier(p 336.4 463.6, p 336.4 463.6, p 347.71625000000006 452.28374999999994, p 347.71625000000006 452.28374999999994);
        CubicBezier(p 347.71625000000006 452.28374999999994, p 340.4775 445.04499999999996, p 432.0 408.53499999999997, p 432.0 424.0) ]
    let right = closed [
        CubicBezier(p 480.0 448.0, p 480.0 448.0, p 426.61171800037516 448.0, p 426.61171800037516 448.0);
        CubicBezier(p 426.61171800037516 448.0, p 417.62466981739766 466.9202388307877, p 398.34100264806034 480.0, p 376.0 480.0);
        CubicBezier(p 376.0 480.0, p 353.65899735193966 480.0, p 334.37533018260217 466.9202388307877, p 325.3882819996247 448.00000000000006);
        CubicBezier(p 325.3882819996247 448.00000000000006, p 325.3882819996247 448.00000000000006, p 344.815 333.525, p 344.815 333.525);
        CubicBezier(p 344.815 333.525, p 351.86 333.92999999999995, p 480.0 448.0, p 480.0 448.0) ]
    Assert.Equal(Error OverlappingSegments, Intersections.path (Path.singleton left) (Path.singleton right))
    let found = Encounters.path (Path.singleton left) (Path.singleton right) |> unwrap
    Assert.NotEmpty found.Overlaps
    // Shared-arc endpoints use our local Touching contract, as in Gleam.
    check left right found.Intersections [
        p 426.61172 448.0, 0, 0.27769, false
        p 376.0 480.0, 1, 0.0, false
        p 343.68011 469.7389, 1, 0.77843, true
        p 336.40125 463.59875, 2, 0.00608, true
    ]

[<Fact>]
let ``Paper address 19`` () =
    let left = closed [
        CubicBezier(p 923.0174999999999 265.80499999999995, p 944.3699999999999 289.30999999999995, p 956.29 320.10999999999996, p 956.29 351.86999999999995);
        CubicBezier(p 956.29 351.86999999999995, p 956.29 383.6574999999999, p 944.3499999999999 414.48499999999996, p 922.9625 437.99499999999995);
        CubicBezier(p 922.9625 437.99499999999995, p 922.9625 437.99499999999995, p 911.15 427.24499999999995, p 911.15 427.24499999999995);
        CubicBezier(p 911.15 427.24499999999995, p 929.95 406.5775, p 940.29 379.80749999999995, p 940.29 351.86999999999995);
        CubicBezier(p 940.29 351.86999999999995, p 940.29 323.9625, p 929.97 297.2149999999999, p 911.2025 276.55499999999995);
        CubicBezier(p 911.2025 276.55499999999995, p 911.2025 276.55499999999995, p 923.0174999999999 265.80499999999995, p 923.0174999999999 265.80499999999995) ]
    let right = closed [
        CubicBezier(p 940.29 351.92499999999995, p 940.29 351.92499999999995, p 956.29 351.92499999999995, p 956.29 351.92499999999995);
        CubicBezier(p 956.29 351.92499999999995, p 956.275 383.69249999999994, p 944.3375 414.49999999999994, p 922.9625 437.99499999999995);
        CubicBezier(p 922.9625 437.99499999999995, p 922.9625 437.99499999999995, p 911.15 427.24499999999995, p 911.15 427.24499999999995);
        CubicBezier(p 911.15 427.24499999999995, p 929.9375 406.5925, p 940.275 379.8449999999999, p 940.29 351.92499999999995) ]
    Assert.Equal(Error OverlappingSegments, Intersections.path (Path.singleton left) (Path.singleton right))
    let found = Encounters.path (Path.singleton left) (Path.singleton right) |> unwrap
    Assert.NotEmpty found.Overlaps
    check left right found.Intersections [
        p 956.28999 351.925, 1, 0.000577, true
        p 956.28948 352.2327, 1, 0.003804, true
        p 939.50932 415.17952, 1, 0.701801, true
        p 922.9625 437.995, 2, 0.0, false
        p 911.15 427.245, 3, 0.0, false
        p 932.08099 394.02288, 3, 0.477224, true
        p 940.28956 352.18697, 3, 0.996218, true
    ]
