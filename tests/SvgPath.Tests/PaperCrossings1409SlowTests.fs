[<Xunit.Trait("Category", "Slow")>]
module SvgPath.Tests.PaperCrossings1409SlowTests

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

let private check left right =
    Assert.Equal(Error OverlappingSegments, Intersections.subpath left right)
    let found = Encounters.subpath left right |> unwrap
    Assert.NotEmpty found.Overlaps
    Assert.NotEmpty found.Intersections
    for hit in found.Intersections do
        let classes = Intersections.classifyGroupedSubpathIntersection left right hit |> unwrap
        Assert.NotEmpty classes
        for item in classes do
            match item.Classification with
            | Intersections.Touching _ -> ()
            | other -> failwithf "Expected Touching, got %A" other
    let filtered = Encounters.subpathRemoveRedundantIntersections found left right Intersections.defaultOptions.Tolerance |> unwrap
    Assert.True(filtered.Overlaps = found.Overlaps)
    Assert.Empty filtered.Intersections

[<Fact>]
let ``Paper 1409 assertion 2`` () =
    let left = closed [
        CubicBezier(p 100.0 200.0, p 100.0 200.0, p 100.0 100.0, p 100.0 100.0);
        CubicBezier(p 100.0 100.0, p 100.0 100.0, p 200.0 100.0, p 200.0 100.0);
        CubicBezier(p 200.0 100.0, p 200.0 100.0, p 200.0 200.0, p 200.0 200.0);
        CubicBezier(p 200.0 200.0, p 200.0 200.0, p 100.0 200.0, p 100.0 200.0) ]
    let right = closed [
        CubicBezier(p 100.0 200.0, p 100.0 200.0, p 100.0 100.0, p 100.0 100.0);
        CubicBezier(p 100.0 100.0, p 100.0 100.0, p 200.0 100.0, p 200.0 100.0);
        CubicBezier(p 200.0 100.0, p 200.0 100.0, p 200.0 200.0, p 200.0 200.0);
        CubicBezier(p 200.0 200.0, p 200.0 200.0, p 100.0 200.0, p 100.0 200.0) ]
    check left right

[<Fact>]
let ``Paper 1409 assertion 3`` () =
    let left = closed [
        CubicBezier(p 260.0 300.0, p 260.0 277.90861000676824, p 277.90861000676824 260.0, p 300.0 260.0);
        CubicBezier(p 300.0 260.0, p 322.09138999323176 260.0, p 340.0 277.90861000676824, p 340.0 300.0);
        CubicBezier(p 340.0 300.0, p 340.0 322.09138999323176, p 322.09138999323176 340.0, p 300.0 340.0);
        CubicBezier(p 300.0 340.0, p 277.90861000676824 340.0, p 260.0 322.09138999323176, p 260.0 300.0) ]
    let right = closed [
        CubicBezier(p 260.0 300.0, p 260.0 277.90861000676824, p 277.90861000676824 260.0, p 300.0 260.0);
        CubicBezier(p 300.0 260.0, p 322.09138999323176 260.0, p 340.0 277.90861000676824, p 340.0 300.0);
        CubicBezier(p 340.0 300.0, p 340.0 322.09138999323176, p 322.09138999323176 340.0, p 300.0 340.0);
        CubicBezier(p 300.0 340.0, p 277.90861000676824 340.0, p 260.0 322.09138999323176, p 260.0 300.0) ]
    check left right
