module SvgPath.Tests.ClosedPathAssertions

open SvgPath
open Xunit

// Ignore only closed-walk rotation and contour order, not direction,
// subdivision, segment geometry, or multiplicity.
let private canonical path =
    Path.subpaths path |> List.map (fun subpath ->
        Assert.True(Subpath.isClosed subpath)
        let segments = Subpath.segments subpath |> List.map Serialize.segment
        match segments with
        | [] -> ""
        | _ -> segments |> List.mapi (fun i _ ->
            List.skip i segments @ List.take i segments |> String.concat "|") |> List.sort |> List.head)
    |> List.sort

let equivalent actual expectedData =
    let expected = Parse.path expectedData |> Result.defaultWith (failwithf "%A")
    Assert.True(canonical actual = canonical expected, sprintf "Expected %s\nActual %s" expectedData (Serialize.path actual))
