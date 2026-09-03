module SvgPath.Tests.RoundtripTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private parseAndSerializeWith options input =
    Parse.path input |> Result.map (fun path -> Serialize.pathWith path options)

let private parseAndSerialize input = parseAndSerializeWith Serialize.defaultOptions input

let private subpath segments = Subpath.create segments |> Result.defaultWith (failwithf "%A")
let private path segments = Path.singleton (subpath segments)

let private generatedPaths () =
    [ path [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 20.0); Line(point 10.0 20.0, point -5.0 20.0) ]
      path [ Line(point -10.0 -10.0, point -5.0 -5.0); QuadraticBezier(point -5.0 -5.0, point 0.0 15.0, point 10.0 0.0); QuadraticBezier(point 10.0 0.0, point 20.0 -15.0, point 25.0 5.0) ]
      path [ CubicBezier(point 0.0 0.0, point 5.0 10.0, point 15.0 -10.0, point 20.0 0.0); CubicBezier(point 20.0 0.0, point 30.0 10.0, point 35.0 -10.0, point 40.0 0.0) ]
      path [ Arc { Start = point 0.0 0.0; Radius = point 10.0 5.0; XAxisRotation = 30.0<degree>; LargeArc = false; Sweep = true; End = point 20.0 10.0 }; Arc { Start = point 20.0 10.0; Radius = point 8.0 8.0; XAxisRotation = -45.0<degree>; LargeArc = true; Sweep = false; End = point 40.0 0.0 } ]
      path [ Line(point 0.0 0.0, point 12.0 0.0); QuadraticBezier(point 12.0 0.0, point 18.0 8.0, point 24.0 0.0); CubicBezier(point 24.0 0.0, point 30.0 -8.0, point 36.0 8.0, point 42.0 0.0); Arc { Start = point 42.0 0.0; Radius = point 6.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = false; End = point 50.0 0.0 } ]
      Subpath.polygon [ point 0.0 0.0; point 20.0 0.0; point 20.0 20.0; point 0.0 20.0 ] |> Result.defaultWith (failwithf "%A") |> Path.singleton
      Path.ofSubpaths [ subpath [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]; subpath [ Line(point 30.0 30.0, point 40.0 30.0); Line(point 40.0 30.0, point 40.0 40.0) ] ] ]

let private assertRoundTrips options =
    generatedPaths ()
    |> List.iter (fun path ->
        let serialized = Serialize.pathWith path options
        Assert.Equal(Ok serialized, parseAndSerializeWith options serialized))

let private assertMultilineRoundTrips options =
    generatedPaths ()
    |> List.iter (fun path ->
        let serialized = Serialize.pathWith path options
        Assert.Equal(Ok serialized, parseAndSerializeWith options serialized)
        let parsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
        Assert.Equal(Serialize.path path, Serialize.path parsed))

[<Fact>]
let ``absolute line subset canonicalizes`` () =
    Assert.Equal(Ok "M 0 0 H 10 V 20 H 0", parseAndSerialize "M 0 0 L 10 0 V 20 H 0")

[<Fact>]
let ``relative line subset canonicalizes to absolute by default`` () =
    Assert.Equal(Ok "M 10 10 H 15 V 30 H 10", parseAndSerialize "m 10 10 l 5 0 v 20 h -5")

[<Fact>]
let ``compact input canonicalizes`` () =
    Assert.Equal(Ok "M 0 -1 H 10 V 9 H 0 Z", parseAndSerialize "M0-1L10-1V9H0z")

[<Fact>]
let ``comma separated input canonicalizes`` () =
    Assert.Equal(Ok "M 0 0 H 10 V 20", parseAndSerialize "M0,0 L10,0 10,20")

[<Fact>]
let ``move only subpaths are preserved`` () =
    Assert.Equal(Ok "M 0 0 M 10 10 H 20 M 30 30", parseAndSerialize "M 0 0 M 10 10 L 20 10 M 30 30")

[<Fact>]
let ``relative serialization after parsing`` () =
    Assert.Equal(Ok "m 10 10 h 10 v 20", parseAndSerializeWith (Serialize.relativeDecimalOptions 0) "M 10 10 L 20 10 L 20 30")

[<Fact>]
let ``minimized serialization after parsing`` () =
    let options = Serialize.decimalOptions 0 |> Serialize.minimizeWhitespace
    Assert.Equal(Ok "M0 0H10V20", parseAndSerializeWith options "M 0 0 L 10 0 L 10 20")

[<Fact>]
let ``decimal rounding after parsing`` () =
    Assert.Equal(Ok "M 0 1.23457 H 10", parseAndSerializeWith (Serialize.decimalOptions 5) "M 0.000001 1.234567 L 10.000001 1.234568")

[<Fact>]
let ``generated paths round trip with default options`` () = assertRoundTrips Serialize.defaultOptions

[<Fact>]
let ``generated paths round trip with relative options`` () = assertRoundTrips (Serialize.relativeDecimalOptions 0)

[<Fact>]
let ``generated paths round trip with minimized options`` () = assertRoundTrips (Serialize.decimalOptions 0 |> Serialize.minimizeWhitespace)

[<Fact>]
let ``generated paths round trip with repeat commands false options`` () = assertRoundTrips (Serialize.defaultOptions |> Serialize.repeatCommands false)

[<Fact>]
let ``generated paths round trip with commas`` () = assertRoundTrips (Serialize.defaultOptions |> Serialize.withCommas true)

[<Fact>]
let ``generated paths round trip with minimized repeat commands false options`` () =
    assertRoundTrips (Serialize.decimalOptions 0 |> Serialize.minimizeWhitespace |> Serialize.repeatCommands false)

[<Fact>]
let ``generated paths round trip with subpath newlines and repeat commands`` () =
    assertMultilineRoundTrips (Serialize.defaultOptions |> Serialize.repeatCommands true |> Serialize.withNewlines AtSubpaths)

[<Fact>]
let ``generated paths round trip with subpath newlines and omitted repeat commands`` () =
    assertMultilineRoundTrips (Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSubpaths)

[<Fact>]
let ``generated paths round trip with segment newlines and repeat commands`` () =
    assertMultilineRoundTrips (Serialize.defaultOptions |> Serialize.repeatCommands true |> Serialize.withNewlines AtSegments)

[<Fact>]
let ``generated paths round trip with segment newlines and omitted repeat commands`` () =
    assertMultilineRoundTrips (Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments)

[<Fact>]
let ``generated paths round trip with commas segment newlines and omitted repeat commands`` () =
    assertMultilineRoundTrips (Serialize.defaultOptions |> Serialize.withCommas true |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments)

[<Fact>]
let ``generated paths round trip with commas and minimized whitespace`` () =
    assertRoundTrips (Serialize.decimalOptions 0 |> Serialize.withCommas true |> Serialize.minimizeWhitespace)
