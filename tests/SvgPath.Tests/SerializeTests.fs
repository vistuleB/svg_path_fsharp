module SvgPath.Tests.SerializeTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

[<Fact>]
let ``empty path serializes to empty string`` () =
    Assert.Equal("", Serialize.path Path.empty)

[<Fact>]
let ``empty subpath serializes to move`` () =
    Assert.Equal("M 1 2", Serialize.subpath (Subpath.empty (point 1.0 2.0)))

[<Fact>]
let ``serialization preserves scientific exponents`` () =
    let options = Serialize.defaultOptions |> Serialize.withRightDecimals System
    Assert.Equal("M 1e20 0", Serialize.subpathWith (Subpath.empty (point 1.0e20 0.0)) options)

[<Fact>]
let ``serialized padding measures scientific significands`` () =
    let options = Serialize.defaultOptions |> Serialize.withRightDecimals System |> Serialize.withLeftPadding (LeftPadding(4, Zero))
    Assert.Equal("M 0001e20 0002", Serialize.subpathWith (Subpath.empty (point 1.0e20 2.0)) options)

[<Fact>]
let ``serialization uses scientific notation when scaling is unsafe`` () =
    let subpath = Subpath.empty (point 1.0e20 -1.0e20)
    Assert.Equal("M 1e20 -1e20", Serialize.subpathWith subpath (Serialize.decimalOptions 5))
    Assert.Equal("M 1.00e20 -1.00e20", Serialize.subpathWith subpath (Serialize.fixedDecimalOptions 2))

[<Fact>]
let ``closed empty subpath serializes to move and z`` () =
    let subpath = Subpath.empty (point 0.0 0.0) |> Subpath.setClosed true |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Z", Serialize.subpath subpath)

[<Fact>]
let ``path serializes empty subpaths`` () =
    let line = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let path = Path.ofSubpaths [ Subpath.empty (point 0.0 0.0); line; Subpath.empty (point 0.0 0.0) ]
    Assert.Equal("M 0 0 M 0 0 H 10 M 0 0", Serialize.path path)

[<Fact>]
let ``open subpath serializes absolute commands`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 V 20", Serialize.subpath subpath)

[<Fact>]
let ``closed subpath serializes with z`` () =
    let subpath = Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 V 20 Z", Serialize.subpath subpath)

[<Fact>]
let ``closed subpath keeps final curve before z`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); QuadraticBezier(point 10.0 0.0, point 20.0 10.0, point 0.0 0.0) ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 Q 20 10 0 0 Z", Serialize.subpath subpath)

[<Fact>]
let ``closed subpath keeps final zero length line`` () =
    let p = point 0.0 0.0
    let subpath = Subpath.ofSegment (Line(p, p)) |> Subpath.setClosed true |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 0 Z", Serialize.subpath subpath)

[<Fact>]
let ``closed subpath keeps final zero length line after curve`` () =
    let a, b = point 0.0 0.0, point 10.0 0.0
    let subpath = Subpath.create [ QuadraticBezier(a, b, a); Line(a, a) ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 0 0 H 0 Z", Serialize.subpath subpath)

[<Fact>]
let ``relative closed subpath keeps final zero length line`` () =
    let a, b = point 10.0 10.0, point 20.0 10.0
    let subpath = Subpath.create [ Line(a, b); Line(b, a); Line(a, a) ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("m 10 10 h 10 h -10 h 0 z", Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``bezier and arc segments serialize`` () =
    let a, b, c, d, e = point 0.0 0.0, point 10.0 0.0, point 20.0 10.0, point 30.0 0.0, point 40.0 20.0
    let subpath =
        Subpath.create
            [ QuadraticBezier(a, b, c)
              CubicBezier(c, d, e, b)
              Arc { Start = b; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = a } ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 20 10 C 30 0 40 20 10 0 A 5 8 45 1 0 0 0", Serialize.subpath subpath)

[<Fact>]
let ``fixed decimal options round and pad numbers`` () =
    let line = Line(point 0.0 1.2, point 10.234 -20.235)
    Assert.Equal("M 0.00 1.20 L 10.23 -20.24", Serialize.segmentWith line (Serialize.fixedDecimalOptions 2))

[<Fact>]
let ``minimize whitespace removes command spacing`` () =
    let line = Line(point 0.0 1.2, point 10.234 -20.235)
    Assert.Equal("M0 1.2L10.234-20.235", Serialize.segmentWith line (Serialize.decimalOptions 3 |> Serialize.minimizeWhitespace))

[<Fact>]
let ``decimal options round and strip trailing zeros`` () =
    let line = Line(point 0.0 1.2, point 10.234 -20.235)
    Assert.Equal("M 0 1.2 L 10.234 -20.235", Serialize.segmentWith line (Serialize.decimalOptions 3))

[<Fact>]
let ``fixed decimal options keep trailing zeros`` () =
    Assert.Equal("M 1.000 1.200 L 10.000 -20.000", Serialize.segmentWith (Line(point 1.0 1.2, point 10.0 -20.0)) (Serialize.fixedDecimalOptions 3))

[<Fact>]
let ``fixed decimal options can use zero places`` () =
    Assert.Equal("M 0 2 L 10 -21", Serialize.segmentWith (Line(point 0.4 1.5, point 10.49 -20.5)) (Serialize.fixedDecimalOptions 0))

[<Fact>]
let ``left padding pads serialized numbers`` () =
    let options = Serialize.fixedDecimalOptions 1 |> Serialize.withLeftPadding (LeftPadding(3, Zero))
    Assert.Equal("M 000.0 -02.0 L 012.2 010.2", Serialize.segmentWith (Line(point 0.0 -2.0, point 12.2 10.2)) options)

[<Fact>]
let ``space left padding pads serialized numbers`` () =
    let options = Serialize.fixedDecimalOptions 1 |> Serialize.withLeftPadding (LeftPadding(3, Space))
    Assert.Equal("M   0.0  -2.0 L  12.2  10.2", Serialize.segmentWith (Line(point 0.0 -2.0, point 12.2 10.2)) options)

[<Fact>]
let ``minifying options use relative minimized output`` () =
    let subpath = Subpath.polyline [ point 10.0 20.0; point 13.0 18.0; point 16.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("m10 20 3-2 3-2", Serialize.subpathWith subpath (Serialize.minifyingOptions 0))

[<Fact>]
let ``minimized fractions omit leading zero and use decimal boundary`` () =
    let subpath = Subpath.ofSegment (Line(point 0.6 0.5, point 0.4 0.3))
    let options = Serialize.decimalOptions 1 |> Serialize.useHorizontalVertical false |> Serialize.minimizeWhitespace
    let serialized = Serialize.subpathWith subpath options
    Assert.Equal("M.6.5L.4.3", serialized)
    Assert.Equal(Ok(Path.ofSubpaths [ subpath ]), Parse.path serialized)

[<Fact>]
let ``minifying options concatenate arc flags and endpoint`` () =
    let subpath =
        Subpath.ofSegment
            (Arc { Start = point 0.0 0.0; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = point 3.0 -2.0 })
    let serialized = Serialize.subpathWith subpath (Serialize.minifyingOptions 0)
    Assert.Equal("m0 0a5 8 45 103-2", serialized)
    Assert.Equal(Ok(Path.ofSubpaths [ subpath ]), Parse.path serialized)

[<Fact>]
let ``minifying options roundtrip many negative fraction vertices`` () =
    let values = [ -0.97,-0.94; -0.82,-0.71; -0.65,-0.89; -0.48,-0.62; -0.31,-0.83; -0.14,-0.55; -0.02,-0.76; -0.19,-0.43; -0.37,-0.68; -0.53,-0.34; -0.72,-0.58; -0.88,-0.27; -0.99,-0.49; -0.79,-0.08; -0.56,-0.29; -0.33,-0.01 ]
    let path = values |> List.map (fun (x,y) -> point x y) |> Subpath.polyline |> Result.defaultWith (failwithf "%A") |> fun subpath -> Path.ofSubpaths [ subpath ]
    let options = Serialize.minifyingOptions 2
    let serialized = Serialize.pathWith path options
    let parsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
    Assert.Equal(serialized, Serialize.pathWith parsed options)

[<Fact>]
let ``minifying options roundtrip preserves structural path equality`` () =
    let values = [ -1.0,-1.0; -0.75,-0.5; -0.5,-0.75; -0.25,-0.25; 0.0,-0.5; -0.25,-1.0; -0.5,-0.25; -0.75,-0.75; -1.0,-0.25; -0.75,0.0; -0.5,-0.5; -0.25,-0.75; 0.0,-1.0; -0.25,0.0; -0.5,-1.0; -1.0,-0.5 ]
    let path = values |> List.map (fun (x,y) -> point x y) |> Subpath.polyline |> Result.defaultWith (failwithf "%A") |> fun subpath -> Path.ofSubpaths [ subpath ]
    Assert.Equal(Ok path, Serialize.pathWith path (Serialize.minifyingOptions 2) |> Parse.path)

[<Fact>]
let ``use s t uses s and t by default`` () =
    let subpath =
        Subpath.create
            [ QuadraticBezier(point 0.0 0.0, point 10.0 0.0, point 20.0 0.0)
              QuadraticBezier(point 20.0 0.0, point 30.0 0.0, point 40.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 20 0 T 40 0", Serialize.subpath subpath)

[<Fact>]
let ``use h v can be disabled`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 L 10 0 L 10 20", Serialize.subpathWith subpath (Serialize.useHorizontalVertical false Serialize.defaultOptions))

[<Fact>]
let ``use s t can be disabled`` () =
    let subpath = Subpath.create [ QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 30.0 40.0); QuadraticBezier(point 30.0 40.0, point 50.0 60.0, point 70.0 80.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 20 30 40 Q 50 60 70 80", Serialize.subpathWith subpath (Serialize.useSmoothCurves false Serialize.defaultOptions))

[<Fact>]
let ``use s t discovers shorthand after decimal formatting`` () =
    let subpath = Subpath.create [ QuadraticBezier(point 0.0 0.0, point 10.0 20.0, point 30.0 40.0); QuadraticBezier(point 30.0 40.0, point 50.0004 60.0004, point 70.0 80.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 20 30 40 T 70 80", Serialize.subpathWith subpath (Serialize.decimalOptions 3))

[<Fact>]
let ``absolute smooth shorthand respects rounded parser state`` () =
    let subpath = Subpath.create [ CubicBezier(point 0.0 0.0, point 0.0 0.0, point 0.04 0.0, point 0.06 0.0); CubicBezier(point 0.06 0.0, point 0.08 0.0, point 0.2 0.0, point 0.3 0.0) ] |> Result.defaultWith (failwithf "%A")
    let serialized = Serialize.subpathWith subpath (Serialize.decimalOptions 1)
    Assert.Equal("M 0 0 S 0 0 0.1 0 C 0.1 0 0.2 0 0.3 0", serialized)

[<Fact>]
let ``repeat commands false omits repeated line commands`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 L 10 10 20 20", Serialize.subpathWith subpath (Serialize.repeatCommands false Serialize.defaultOptions))

[<Fact>]
let ``repeat commands false omits repeated h and v commands`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 20.0 0.0; point 20.0 10.0; point 20.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 20 V 10 20", Serialize.subpathWith subpath (Serialize.repeatCommands false Serialize.defaultOptions))

[<Fact>]
let ``repeat commands false omits repeated curve commands`` () =
    let a, b, c, d, e, f = point 0.0 0.0, point 10.0 0.0, point 20.0 10.0, point 30.0 0.0, point 40.0 10.0, point 50.0 0.0
    let subpath = Subpath.create [ QuadraticBezier(a,b,c); QuadraticBezier(c,d,e); CubicBezier(e,d,b,f); CubicBezier(f,b,d,a) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 Q 10 0 20 10 30 0 40 10 C 30 0 10 0 50 0 10 0 30 0 0 0", Serialize.subpathWith subpath (Serialize.repeatCommands false Serialize.defaultOptions))

[<Fact>]
let ``repeat commands false omits repeated smooth commands`` () =
    let quadratic = Subpath.create [ QuadraticBezier(point 0.0 0.0, point 10.0 0.0, point 20.0 0.0); QuadraticBezier(point 20.0 0.0, point 30.0 0.0, point 40.0 0.0); QuadraticBezier(point 40.0 0.0, point 50.0 0.0, point 60.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.repeatCommands false Serialize.defaultOptions
    Assert.Equal("M 0 0 Q 10 0 20 0 T 40 0 60 0", Serialize.subpathWith quadratic options)

[<Fact>]
let ``repeat commands false omits repeated arc commands`` () =
    let radius = point 5.0 5.0
    let arc startPoint endPoint = Arc { Start = startPoint; Radius = radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = endPoint }
    let subpath = Subpath.create [ arc (point 0.0 0.0) (point 10.0 0.0); arc (point 10.0 0.0) (point 20.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 A 5 5 0 0 1 10 0 5 5 0 0 1 20 0", Serialize.subpathWith subpath (Serialize.repeatCommands false Serialize.defaultOptions))

[<Fact>]
let ``at segments with repeat commands true starts lines with commands`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0\nL 10 10\nL 20 20\nZ", Serialize.subpathWith subpath (Serialize.withNewlines AtSegments Serialize.defaultOptions))

[<Fact>]
let ``at segments with repeat commands false trails emitted commands`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ]
        |> Result.defaultWith (failwithf "%A")
    let compact = Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n0 0 L\n10 10\n20 20 Z", Serialize.subpathWith subpath compact)

[<Fact>]
let ``at segments with repeat commands true starts curve lines with commands`` () =
    let subpath = Subpath.create [ CubicBezier(point 0.0 0.0, point 10.0 0.0, point 20.0 10.0, point 30.0 0.0); CubicBezier(point 30.0 0.0, point 20.0 10.0, point 10.0 0.0, point 40.0 10.0) ] |> Result.bind (Subpath.setClosedWith Bridge true) |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0\nC 10 0 20 10 30 0\nC 20 10 10 0 40 10\nZ", Serialize.subpathWith subpath (Serialize.withNewlines AtSegments Serialize.defaultOptions))

[<Fact>]
let ``at segments with repeat commands false trails curve commands`` () =
    let subpath = Subpath.create [ CubicBezier(point 0.0 0.0, point 10.0 0.0, point 20.0 10.0, point 30.0 0.0); CubicBezier(point 30.0 0.0, point 20.0 10.0, point 10.0 0.0, point 40.0 10.0) ] |> Result.bind (Subpath.setClosedWith Bridge true) |> Result.defaultWith (failwithf "%A")
    let options = Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n0 0 C\n10 0 20 10 30 0\n20 10 10 0 40 10 Z", Serialize.subpathWith subpath options)

[<Fact>]
let ``at subpaths puts each subpath on its own line`` () =
    let closed points = Subpath.polygon points |> Result.defaultWith (failwithf "%A")
    let path = Path.ofSubpaths [ closed [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ]; closed [ point 100.0 100.0; point 110.0 110.0; point 120.0 120.0 ] ]
    Assert.Equal("M 0 0 L 10 10 L 20 20 Z\nM 100 100 L 110 110 L 120 120 Z", Serialize.pathWith path (Serialize.withNewlines AtSubpaths Serialize.defaultOptions))

[<Fact>]
let ``at segments with repeat commands false starts moves on new lines`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 10.0))
    let second = Subpath.ofSegment (Line(point 100.0 100.0, point 110.0 110.0))
    let options = Serialize.defaultOptions |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n0 0 L\n10 10\nM\n100 100 L\n110 110", Serialize.pathWith (Path.ofSubpaths [ first; second ]) options)

[<Fact>]
let ``commas separate coordinates inside point pairs`` () =
    let subpath = Subpath.polygon [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.defaultOptions |> Serialize.withCommas true |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n0,0 L\n10,10\n20,20 Z", Serialize.subpathWith subpath options)

[<Fact>]
let ``commas preserve spaces between curve point pairs`` () =
    let a, b, c, d = point 20.0 -30.0, point 140.0 20.0, point 480.0 -60.0, point 840.0 -90.0
    let subpath =
        Subpath.create
            [ CubicBezier(a, point -15.0 40.0, point 80.0 -90.0, b)
              CubicBezier(b, point 260.0 30.0, point -320.0 45.0, c)
              CubicBezier(c, point 600.5 -70.25, point 720.0 80.0, d) ]
        |> Result.defaultWith (failwithf "%A")
    let options = Serialize.fixedDecimalOptions 2 |> Serialize.withLeftPadding (AutoLeftPadding Space) |> Serialize.withCommas true |> Serialize.repeatCommands false |> Serialize.withNewlines AtSegments
    Assert.Equal("M\n  20.00, -30.00 C\n -15.00,  40.00   80.00, -90.00  140.00,  20.00\n 260.00,  30.00 -320.00,  45.00  480.00, -60.00\n 600.50, -70.25  720.00,  80.00  840.00, -90.00", Serialize.subpathWith subpath options)

[<Fact>]
let ``commas apply to arc radius and endpoint pairs`` () =
    let arc = Arc { Start = point 10.0 20.0; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = point 13.0 18.0 }
    Assert.Equal("m 10,20 a 5,8 45 1 0 3,-2", Serialize.segmentWith arc (Serialize.relativeDecimalOptions 0 |> Serialize.withCommas true))

[<Fact>]
let ``relative options use relative line commands`` () =
    Assert.Equal("m 10 20 l 3 -2", Serialize.segmentWith (Line(point 10.0 20.0, point 13.0 18.0)) (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``relative repeat commands false omits repeated commands`` () =
    let subpath = Subpath.polyline [ point 10.0 20.0; point 13.0 18.0; point 16.0 16.0 ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.relativeDecimalOptions 0 |> Serialize.repeatCommands false
    Assert.Equal("m 10 20 l 3 -2 3 -2", Serialize.subpathWith subpath options)

[<Fact>]
let ``relative options use relative curve commands`` () =
    let start = point 10.0 20.0
    Assert.Equal("m 10 20 q 2 3 5 5", Serialize.segmentWith (QuadraticBezier(start, point 12.0 23.0, point 15.0 25.0)) (Serialize.relativeDecimalOptions 0))
    Assert.Equal("m 10 20 c 1 1 4 4 8 8", Serialize.segmentWith (CubicBezier(start, point 11.0 21.0, point 14.0 24.0, point 18.0 28.0)) (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``relative options use relative arc endpoint`` () =
    let arc = Arc { Start = point 10.0 20.0; Radius = point 5.0 8.0; XAxisRotation = 45.0<degree>; LargeArc = true; Sweep = false; End = point 13.0 18.0 }
    Assert.Equal("m 10 20 a 5 8 45 1 0 3 -2", Serialize.segmentWith arc (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``relative minimize whitespace removes command spacing`` () =
    let options = Serialize.relativeDecimalOptions 0 |> Serialize.minimizeWhitespace
    Assert.Equal("m10 20l3-2", Serialize.segmentWith (Line(point 10.0 20.0, point 13.0 18.0)) options)

[<Fact>]
let ``minimized repeat commands false omits repeated commands`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 10.0; point 20.0 20.0 ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.decimalOptions 0 |> Serialize.minimizeWhitespace |> Serialize.repeatCommands false
    Assert.Equal("M0 0L10 10 20 20", Serialize.subpathWith subpath options)

[<Fact>]
let ``explicit initial lineto can be omitted absolute`` () =
    let options = Serialize.defaultOptions |> Serialize.explicitInitialLineto false
    Assert.Equal("M 10 20 13 18", Serialize.segmentWith (Line(point 10.0 20.0, point 13.0 18.0)) options)

[<Fact>]
let ``explicit initial lineto can be omitted relative`` () =
    let options = Serialize.relativeOptions |> Serialize.explicitInitialLineto false
    Assert.Equal("m 10 20 3 -2", Serialize.segmentWith (Line(point 10.0 20.0, point 13.0 18.0)) options)

[<Fact>]
let ``parser tracked relative lines correct rounding drift`` () =
    let subpath =
        Subpath.polyline [ point 0.0 0.0; point 0.34 0.34; point 0.68 0.68; point 1.02 1.02 ]
        |> Result.defaultWith (failwithf "%A")
    let options = Serialize.relativeDecimalOptions 1 |> Serialize.useHorizontalVertical false
    Assert.Equal("m 0 0 l 0.3 0.3 l 0.4 0.4 l 0.3 0.3", Serialize.subpathWith subpath options)

[<Fact>]
let ``relative options make moves relative between subpaths`` () =
    let first = Subpath.ofSegment (Line(point 10.0 10.0, point 20.0 10.0))
    let second = Subpath.ofSegment (Line(point 25.0 30.0, point 30.0 30.0))
    let path = Path.ofSubpaths [ first; Subpath.empty (point 20.0 10.0); second ]
    Assert.Equal("m 10 10 h 10 m 0 0 m 5 20 h 5", Serialize.pathWith path (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``relative options move from closed subpath start after z`` () =
    let a, b = point 10.0 10.0, point 20.0 10.0
    let first = Subpath.create [ Line(a,b); Line(b,a) ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    let second = Subpath.ofSegment (Line(point 30.0 10.0, point 40.0 10.0))
    Assert.Equal("m 10 10 h 10 z m 20 0 h 10", Serialize.pathWith (Path.ofSubpaths [ first; second ]) (Serialize.relativeDecimalOptions 0))

[<Fact>]
let ``minimized relative path with multiple subpaths`` () =
    let first = Subpath.ofSegment (Line(point 10.0 10.0, point 20.0 10.0))
    let second = Subpath.ofSegment (Line(point 25.0 30.0, point 30.0 30.0))
    let options = Serialize.relativeDecimalOptions 0 |> Serialize.minimizeWhitespace
    Assert.Equal("m10 10h10m5 20h5", Serialize.pathWith (Path.ofSubpaths [ first; second ]) options)

[<Fact>]
let ``decimal options clamp negative decimal places to zero`` () =
    Assert.Equal("M 0 2 L 10 -21", Serialize.segmentWith (Line(point 0.4 1.5, point 10.49 -20.5)) (Serialize.decimalOptions -3))

[<Fact>]
let ``rounded absolute line uses h or v after formatting`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.000001); Line(point 10.0 0.000001, point 10.000001 20.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 0 H 10 V 20", Serialize.subpathWith subpath (Serialize.decimalOptions 5))

[<Fact>]
let ``rounded relative line uses h or v after formatting`` () =
    let subpath = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.000001); Line(point 10.0 0.000001, point 10.000001 20.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("m 0 0 h 10 v 20", Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 5))

[<Fact>]
let ``parser tracked relative cubic uses similarity correction`` () =
    let segment = CubicBezier(point 0.34 0.0, point 0.34 1.0, point 1.39 1.0, point 1.39 0.0)
    let serialized = Serialize.segmentWith segment (Serialize.relativeDecimalOptions 1)
    Assert.Equal("m 0.3 0 c 0 1 1.1 1 1.1 0", serialized)
    let reparsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
    let endpoint = Subpath.finish reparsed.Subpaths[0]
    Assert.InRange(abs (endpoint.X - 1.4<length>), 0.0<length>, 1.0e-12<length>)
    Assert.Equal(0.0<length>, endpoint.Y)

[<Fact>]
let ``auto left padding aligns serialized path numbers`` () =
    let subpath = Subpath.polyline [ point 0.0 -5.0; point 120.0 10.0; point 2.0 -30.0 ] |> Result.defaultWith (failwithf "%A")
    let options = Serialize.fixedDecimalOptions 1 |> Serialize.withLeftPadding (AutoLeftPadding Zero)
    Assert.Equal("M 000.0 -05.0 L 120.0 010.0 L 002.0 -30.0", Serialize.subpathWith subpath options)

[<Fact>]
let ``parser tracked auto padding uses corrected numbers`` () =
    let subpath = Subpath.ofSegment (Line(point 0.14 0.0, point 10.06 0.0))
    let options = Serialize.relativeDecimalOptions 1 |> Serialize.withLeftPadding (AutoLeftPadding Zero)
    Assert.Equal("m 00.1 00 h 10", Serialize.pathWith (Path.ofSubpaths [ subpath ]) options)

[<Fact>]
let ``parser tracked relative lines preserve axis constraints`` () =
    let subpath = Subpath.create [ Line(point 0.04 0.04, point 0.34 0.34); Line(point 0.34 0.34, point 0.68 0.34); Line(point 0.68 0.34, point 0.68 0.68) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal("m 0 0 l 0.3 0.3 h 0.4 v 0.4", Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative full arc is subdivided`` () =
    let anchor = point 0.34 0.0
    let arc = Arc { Start = anchor; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = anchor }
    let serialized = Serialize.segmentWith arc (Serialize.relativeDecimalOptions 1)
    Assert.True((serialized |> Seq.filter ((=) 'a') |> Seq.length) = 2, serialized)
    let parsed = Parse.path serialized |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, parsed.Subpaths[0].Segments.Length)

[<Fact>]
let ``parser tracked relative close resets the parser current`` () =
    let a, b = point 0.34 0.34, point 0.68 0.34
    let first = Subpath.create [ Line(a,b); Line(b,a) ] |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")
    let second = Subpath.ofSegment (Line(point 1.02 0.34, b))
    Assert.Equal("m 0.3 0.3 h 0.4 z m 0.7 0 h -0.3", Serialize.pathWith (Path.ofSubpaths [ first; second ]) (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative arc applies chord similarity`` () =
    let arc = Arc { Start = point 0.34 0.0; Radius = point 2.0 1.0; XAxisRotation = 15.0<degree>; LargeArc = false; Sweep = true; End = point 1.39 0.0 }
    Assert.Equal("m 0.3 0 a 2.1 1 15 0 1 1.1 0", Serialize.segmentWith arc (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative smooth commands use parser controls`` () =
    let subpath = Subpath.create [ CubicBezier(point 0.04 0.0, point 0.34 1.0, point 0.74 1.0, point 1.04 0.0); CubicBezier(point 1.04 0.0, point 1.323 -0.945, point 1.74 -1.0, point 2.08 0.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Contains(" s ", Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative smooth quadratic uses parser control`` () =
    let subpath = Subpath.create [ QuadraticBezier(point 0.04 0.0, point 0.54 1.0, point 1.04 0.0); QuadraticBezier(point 1.04 0.0, point 1.513 -0.945, point 2.08 0.0) ] |> Result.defaultWith (failwithf "%A")
    Assert.Contains(" t ", Serialize.subpathWith subpath (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative collapsed arc preserves arc fields`` () =
    let arc = Arc { Start = point 0.04 0.0; Radius = point 2.0 1.0; XAxisRotation = 15.0<degree>; LargeArc = false; Sweep = true; End = point 0.049 0.0 }
    Assert.Equal("m 0 0 a 2 1 15 0 1 0 0", Serialize.segmentWith arc (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative unstable cubic uses progressive correction`` () =
    let p = point 0.04 0.0
    let cubic = CubicBezier(p, point 0.34 1.0, point 0.34 -1.0, p)
    Assert.Equal("m 0 0 c 0.3 1 0.3 -1 0 0", Serialize.segmentWith cubic (Serialize.relativeDecimalOptions 1))

[<Fact>]
let ``parser tracked relative serialization is stable after parsing`` () =
    let source = "M 0.04 0 C 0.34 1 0.74 1 1.04 0 A 2 1 15 0 1 2.08 0 L 2.42 0.34"
    let options = Serialize.relativeDecimalOptions 1
    let once = Parse.path source |> Result.defaultWith (failwithf "%A") |> fun path -> Serialize.pathWith path options
    let twice = Parse.path once |> Result.defaultWith (failwithf "%A") |> fun path -> Serialize.pathWith path options
    Assert.Equal(once, twice)
