module SvgPath.Tests.SerializeTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``empty and move-only paths serialize`` () =
    Assert.Equal("", Serialize.path Path.empty)
    Assert.Equal("M 1 2", Serialize.subpath (Subpath.empty (point 1.0 2.0)))

[<Fact>]
let ``absolute lines use horizontal and vertical commands`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 V 20", Serialize.subpath subpath)

[<Fact>]
let ``closed path drops only a nonzero closing line`` () =
    let subpath = Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 V 20 Z", Serialize.subpath subpath)

[<Fact>]
let ``curves and arcs serialize`` () =
    let a, b, c, d, e = point 0.0 0.0, point 10.0 0.0, point 20.0 10.0, point 30.0 0.0, point 40.0 20.0
    let subpath =
        Subpath.create
            [ QuadraticBezier(a, b, c)
              CubicBezier(c, d, e, b)
              Arc { Start = b; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = a } ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 20 10 C 30 0 40 20 10 0 A 5 8 45 1 0 0 0", Serialize.subpath subpath)

[<Fact>]
let ``fixed decimals and minified whitespace serialize`` () =
    let line = Line(point 0.0 1.2, point 10.234 -20.235)
    Assert.Equal("M 0.00 1.20 L 10.23 -20.24", Serialize.segmentWith line (Serialize.fixedDecimalOptions 2))
    Assert.Equal("M0 1.2L10.234-20.235", Serialize.segmentWith line (Serialize.decimalOptions 3 |> Serialize.minimizeWhitespace))

[<Fact>]
let ``relative commands preserve geometry through parser`` () =
    let subpath = Subpath.polyline [ point 10.0 20.0; point 13.0 18.0; point 16.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    let serialized = Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 0)
    Assert.Equal("m 10 20 l 3 -2 l 3 -2", serialized)
    Assert.Equal(Ok(Path.ofSubpaths [ subpath ]), Parse.path serialized)

[<Fact>]
let ``minifying omits repeated command and initial lineto`` () =
    let subpath = Subpath.polyline [ point 10.0 20.0; point 13.0 18.0; point 16.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("m10 20 3-2 3-2", Serialize.subpathWith subpath (Serialize.minifyingOptions 0))

[<Fact>]
let ``minified fractions remain parseable`` () =
    let subpath = Subpath.ofSegment (Line(point 0.6 0.5, point 0.4 0.3))
    let options = Serialize.decimalOptions 1 |> Serialize.useHorizontalVertical false |> Serialize.minimizeWhitespace
    let serialized = Serialize.subpathWith subpath options
    Assert.Equal("M.6.5L.4.3", serialized)
    Assert.Equal(Ok(Path.ofSubpaths [ subpath ]), Parse.path serialized)

[<Fact>]
let ``concatenated arc flags round trip`` () =
    let subpath =
        Subpath.ofSegment
            (Arc { Start = point 0.0 0.0; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = point 3.0 -2.0 })
    let serialized = Serialize.subpathWith subpath (Serialize.minifyingOptions 0)
    Assert.Equal("m0 0a5 8 45 103-2", serialized)
    Assert.Equal(Ok(Path.ofSubpaths [ subpath ]), Parse.path serialized)

[<Fact>]
let ``smooth curves use shorthand`` () =
    let subpath =
        Subpath.create
            [ QuadraticBezier(point 0.0 0.0, point 10.0 0.0, point 20.0 0.0)
              QuadraticBezier(point 20.0 0.0, point 30.0 0.0, point 40.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 20 0 T 40 0", Serialize.subpath subpath)

[<Fact>]
let ``repeated commands can be omitted`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 L 10 10 20 20", Serialize.subpathWith subpath (Serialize.repeatCommands false Serialize.defaultOptions))

[<Fact>]
let ``newline policies preserve command grouping`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0\nL 10 10\nL 20 20\nZ", Serialize.subpathWith subpath (Serialize.withNewlines AtSegments Serialize.defaultOptions))
    let compact = Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n0 0 L\n10 10\n20 20 Z", Serialize.subpathWith subpath compact)

[<Fact>]
let ``relative serialization compensates accumulated rounding drift`` () =
    let subpath =
        Subpath.polyline [ point 0.0 0.0; point 0.34 0.34; point 0.68 0.68; point 1.02 1.02 ]
        |> Result.defaultWith (failwithf "%A")
    let options = Serialize.relativeDecimalOptions 1 |> Serialize.useHorizontalVertical false
    Assert.Equal("m 0 0 l 0.3 0.3 l 0.4 0.4 l 0.3 0.3", Serialize.subpathWith subpath options)

[<Fact>]
let ``relative cubic control points follow corrected chord`` () =
    let segment = CubicBezier(point 0.34 0.0, point 0.34 1.0, point 1.39 1.0, point 1.39 0.0)
    let serialized = Serialize.segmentWith segment (Serialize.relativeDecimalOptions 1)
    Assert.Equal("m 0.3 0 c 0 1 1.1 1 1.1 0", serialized)
    let reparsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
    let endpoint = Subpath.finish reparsed.Subpaths[0]
    Assert.InRange(abs (endpoint.X - 1.4<length>), 0.0<length>, 1.0e-12<length>)
    Assert.Equal(0.0<length>, endpoint.Y)

[<Fact>]
let ``automatic left padding follows emitted values`` () =
    let subpath = Subpath.polyline [ point 0.0 -5.0; point 120.0 10.0; point 2.0 -30.0 ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.fixedDecimalOptions 1 |> Serialize.withLeftPadding (AutoLeftPadding Zero)
    Assert.Equal("M 000.0 -05.0 L 120.0 010.0 L 002.0 -30.0", Serialize.subpathWith subpath options)

[<Fact>]
let ``parser-tracked full arc is split into parseable arcs`` () =
    let anchor = point 0.34 0.0
    let arc = Arc { Start = anchor; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = anchor }
    let serialized = Serialize.segmentWith arc (Serialize.relativeDecimalOptions 1)
    Assert.True((serialized |> Seq.filter ((=) 'a') |> Seq.length) = 2, serialized)
    let parsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, parsed.Subpaths[0].Segments.Length)
