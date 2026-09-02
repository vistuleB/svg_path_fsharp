module SvgPath.Tests.StrokeTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private simpleLineSubpath a b = Subpath.polyline [ a; b ] |> Result.defaultWith (failwithf "%A")
let private rightAngle () = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ] |> Result.defaultWith (failwithf "%A")
let private stroked subpath options = Stroke.subpathWith subpath options |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``segment stroke with butt caps returns closed outline`` () =
    let path = Stroke.segment (Line(point 0.0 0.0, point 10.0 0.0)) 2.0<length> |> Result.defaultWith (failwithf "%A")
    let outline = List.exactlyOne path.Subpaths
    Assert.True outline.Closed
    Assert.Equal("M 0 -1 H 10 V 1 H 0 Z", Serialize.subpath outline)

[<Fact>]
let ``subpath stroke with round caps adds two cap arcs`` () =
    let path = stroked (simpleLineSubpath (point 0.0 0.0) (point 10.0 0.0)) { Stroke.defaultOptions with Width = 2.0<length>; Cap = StrokeRound }
    let outline = List.exactlyOne path.Subpaths
    Assert.Equal(2, outline.Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length)

[<Fact>]
let ``subpath stroke with round cap serializes semicircles`` () =
    let path = stroked (simpleLineSubpath (point 0.0 0.0) (point 10.0 0.0)) { Stroke.defaultOptions with Width = 2.0<length>; Cap = StrokeRound }
    Assert.Equal("M 0 -1 H 10 A 1 1 0 0 1 10 1 H 0 A 1 1 0 0 1 0 -1 Z", Serialize.subpath (List.exactlyOne path.Subpaths))

[<Fact>]
let ``zero length subpath stroke with butt cap returns empty path`` () =
    let p = point 3.0 4.0
    let path = Stroke.subpath (Subpath.ofSegment (Line(p, p))) 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Empty path.Subpaths

[<Fact>]
let ``zero length subpath stroke with round cap returns circle`` () =
    let p = point 3.0 4.0
    let path = stroked (Subpath.ofSegment (Line(p, p))) { Stroke.defaultOptions with Width = 2.0<length>; Cap = StrokeRound }
    Assert.Equal("M 4 4 A 1 1 0 0 1 2 4 A 1 1 0 0 1 4 4 Z", Serialize.subpath (List.exactlyOne path.Subpaths))

[<Fact>]
let ``subpath stroke with square caps extends by half width`` () =
    let path = stroked (simpleLineSubpath (point 0.0 0.0) (point 10.0 0.0)) { Stroke.defaultOptions with Width = 2.0<length>; Cap = StrokeSquare }
    Assert.Equal("M 0 -1 H 10 H 11 V 1 H 10 H 0 H -1 V -1 Z", Serialize.subpath (List.exactlyOne path.Subpaths))

[<Fact>]
let ``subpath stroke with bevel join keeps corner cut`` () =
    let options = { Stroke.defaultOptions with Width = 2.0<length>; Offset = { Offset.defaultOptions with Join = Bevel } }
    Assert.Equal("M 0 -1 H 10 L 11 0 V 10 H 9 V 1 H 0 Z", Serialize.subpath (stroked (rightAngle ()) options |> _.Subpaths |> List.exactlyOne))

[<Fact>]
let ``subpath stroke with round join adds join arcs`` () =
    let options = { Stroke.defaultOptions with Width = 2.0<length>; Offset = { Offset.defaultOptions with Join = Round } }
    let outline = stroked (rightAngle ()) options |> _.Subpaths |> List.exactlyOne
    Assert.Equal(1, outline.Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length)
    Assert.Equal("M 0 -1 H 10 A 1 1 0 0 1 11 0 V 10 H 9 V 1 H 0 Z", Serialize.subpath outline)

[<Fact>]
let ``subpath stroke with miter join extends to apex`` () =
    let options = { Stroke.defaultOptions with Width = 2.0<length>; Offset = { Offset.defaultOptions with Join = Miter 4.0 } }
    Assert.Equal("M 0 -1 H 10 H 11 V 0 V 10 H 9 V 1 H 0 Z", Serialize.subpath (stroked (rightAngle ()) options |> _.Subpaths |> List.exactlyOne))

[<Fact>]
let ``subpath stroke with low miter limit falls back to bevel`` () =
    let withJoin join = stroked (rightAngle ()) { Stroke.defaultOptions with Width = 2.0<length>; Offset = { Offset.defaultOptions with Join = join } } |> Serialize.path
    Assert.Equal(withJoin Bevel, withJoin (Miter 1.0))

[<Fact>]
let ``zero length subpath stroke with square cap returns square`` () =
    let p = point 3.0 4.0
    let path = stroked (Subpath.ofSegment (Line(p, p))) { Stroke.defaultOptions with Width = 2.0<length>; Cap = StrokeSquare }
    Assert.Equal("M 2 3 H 4 V 5 H 2 Z", Serialize.subpath (List.exactlyOne path.Subpaths))

[<Fact>]
let ``closed subpath stroke returns two closed contours`` () =
    let square = Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let path = Stroke.subpath square 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, path.Subpaths.Length)
    Assert.All(path.Subpaths, fun subpath -> Assert.True subpath.Closed)

[<Fact>]
let ``self meeting closed subpath stroke uses band sections`` () =
    let figureEight =
        Subpath.create [ CubicBezier(point 76.0 0.0, point -2.0 -62.0, point -2.0 62.0, point 76.0 0.0); CubicBezier(point 76.0 0.0, point 154.0 -62.0, point 154.0 62.0, point 76.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let path = Stroke.subpath figureEight 26.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, path.Subpaths.Length)
    Assert.All(path.Subpaths, fun subpath -> Assert.True subpath.Closed)

[<Fact>]
let ``path stroke strokes each subpath`` () =
    let path = Path.ofSubpaths [ simpleLineSubpath (point 0.0 0.0) (point 10.0 0.0); simpleLineSubpath (point 0.0 10.0) (point 10.0 10.0) ]
    let strokedPath = Stroke.path path 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, strokedPath.Subpaths.Length)
let private lineSubpath points = Subpath.polyline points |> Result.defaultWith (failwithf "%A")
let private bounds (subpath: Subpath) = subpath.Start, (subpath.Segments |> List.last |> Segment.finish)

[<Fact>]
let ``line dashes extract visible intervals`` () =
    let source = lineSubpath [ point 0.0 0.0; point 12.0 0.0 ]
    let dashes = Stroke.subpathDashes source [ 3.0<length>; 2.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point 3.0 0.0
          point 5.0 0.0, point 8.0 0.0
          point 10.0 0.0, point 12.0 0.0 ],
        List.map bounds dashes)

[<Fact>]
let ``subpath dashes applies positive dash offset`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    let positive = Stroke.subpathDashes source [ 3.0<length>; 2.0<length> ] 1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point 2.0 0.0; point 4.0 0.0, point 7.0 0.0; point 9.0 0.0, point 10.0 0.0 ],
        List.map bounds positive)

[<Fact>]
let ``subpath dashes applies negative dash offset`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    let negative = Stroke.subpathDashes source [ 3.0<length>; 2.0<length> ] -1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 1.0 0.0, point 4.0 0.0; point 6.0 0.0, point 9.0 0.0 ],
        List.map bounds negative)

[<Fact>]
let ``subpath dashes preserves small scale intervals`` () =
    let source = lineSubpath [ point 0.0 0.0; point 1.0e-9 0.0 ]
    let dash =
        Stroke.subpathDashes source [ 0.5e-9<length>; 0.5e-9<length> ] 0.0<length>
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(point 0.5e-9 0.0, Segment.finish (List.last dash.Segments))

[<Fact>]
let ``odd dash patterns are duplicated`` () =
    let source = lineSubpath [ point 0.0 0.0; point 12.0 0.0 ]
    let dashes = Stroke.subpathDashes source [ 2.0<length>; 1.0<length>; 3.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point 2.0 0.0; point 3.0 0.0, point 6.0 0.0; point 8.0 0.0, point 9.0 0.0 ],
        List.map bounds dashes)

[<Fact>]
let ``subpath dashes skips zero entries in nonzero patterns`` () =
    let source = lineSubpath [ point 0.0 0.0; point 8.0 0.0 ]
    let dashes =
        Stroke.subpathDashes source [ 0.0<length>; 2.0<length>; 3.0<length>; 2.0<length> ] 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 2.0 0.0, point 5.0 0.0 ],
        List.map bounds dashes)

[<Fact>]
let ``subpath dashes treats empty pattern as none`` () =
    let source = lineSubpath [ point 0.0 0.0; point 8.0 0.0 ]
    let dash =
        Stroke.subpathDashes source [] 3.0<length>
        |> Result.defaultWith (failwithf "%A")
        |> List.exactlyOne
    Assert.Equal(source, dash)

[<Fact>]
let ``dash extraction crosses source segment boundaries`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ]
    let dashes = Stroke.subpathDashes source [ 15.0<length>; 5.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A")
    let dash = Assert.Single(dashes)
    Assert.Equal(2, dash.Segments.Length)
    Assert.Equal(point 10.0 5.0, Segment.finish (List.last dash.Segments))

[<Fact>]
let ``inactive pattern preserves closed subpath`` () =
    let source = Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let dash = Stroke.subpathDashes source [ 0.0<length>; 0.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A") |> Assert.Single
    Assert.True(dash.Closed)
    Assert.Equal(source, dash)

[<Fact>]
let ``active full dash opens a closed subpath`` () =
    let source = Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let dash = Stroke.subpathDashes source [ 100.0<length>; 5.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A") |> Assert.Single
    Assert.False(dash.Closed)
    Assert.Equal(source.Segments.Length, dash.Segments.Length)

[<Fact>]
let ``path dash pattern resets for each subpath`` () =
    let source = Path.ofSubpaths [ lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]; lineSubpath [ point 0.0 10.0; point 10.0 10.0 ] ]
    let dashes = Stroke.pathDashes source [ 3.0<length>; 100.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, dashes.Subpaths.Length)
    Assert.Equal(point 3.0 0.0, Segment.finish (List.last dashes.Subpaths[0].Segments))
    Assert.Equal(point 3.0 10.0, Segment.finish (List.last dashes.Subpaths[1].Segments))

[<Fact>]
let ``subpath dashed strokes each dash`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    let path =
        Stroke.subpathDashed source 2.0<length> [ 3.0<length>; 2.0<length> ] 0.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, path.Subpaths.Length)
    Assert.Equal<string list>(
        [ "M 0 -1 H 3 V 1 H 0 Z"; "M 5 -1 H 8 V 1 H 5 Z" ],
        path.Subpaths |> List.map Serialize.subpath)

[<Fact>]
let ``subpath dashes rejects invalid pattern and offset`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    Assert.Equal(
        Error(InvalidDashLength -1.0<length>),
        Stroke.subpathDashes source [ -1.0<length>; 2.0<length> ] 0.0<length>)

[<Fact>]
let ``dash validation runs for an empty path`` () =
    Assert.Equal(
        Error(InvalidDashLength -1.0<length>),
        Stroke.pathDashes (Path.ofSubpaths []) [ -1.0<length>; 2.0<length> ] 0.0<length>)
    let options = { Stroke.defaultDashOptions [ 1.0<length>; 1.0<length> ] 0.0<length> with Length = { Tolerance = 0.0<length>; MaxDepth = 20 } }
    Assert.Equal(Error(StrokePathError(InvalidLengthTolerance 0.0<length>)), Stroke.pathDashesWith (Path.ofSubpaths []) options)

[<Fact>]
let ``dash pattern rejects nonfinite total`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    Assert.Equal(
        Error InvalidDashPatternLength,
        Stroke.subpathDashes source [ Length.fromFloat 1.0e308; Length.fromFloat 1.0e308 ] 0.0<length>)

[<Fact>]
let ``stroke rejects nonpositive width`` () =
    Assert.Equal(
        Error(InvalidStrokeOutlineWidth 0.0<length>),
        Stroke.segment (Line(point 0.0 0.0, point 10.0 0.0)) 0.0<length>)
