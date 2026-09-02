module SvgPath.Tests.StrokeTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
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
let ``dash offsets follow SVG sign convention`` () =
    let source = lineSubpath [ point 0.0 0.0; point 10.0 0.0 ]
    let positive = Stroke.subpathDashes source [ 3.0<length>; 2.0<length> ] 1.0<length> |> Result.defaultWith (failwithf "%A")
    let negative = Stroke.subpathDashes source [ 3.0<length>; 2.0<length> ] -1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point 2.0 0.0; point 4.0 0.0, point 7.0 0.0; point 9.0 0.0, point 10.0 0.0 ],
        List.map bounds positive)
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 1.0 0.0, point 4.0 0.0; point 6.0 0.0, point 9.0 0.0 ],
        List.map bounds negative)

[<Fact>]
let ``odd dash patterns are duplicated`` () =
    let source = lineSubpath [ point 0.0 0.0; point 12.0 0.0 ]
    let dashes = Stroke.subpathDashes source [ 2.0<length>; 1.0<length>; 3.0<length> ] 0.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<(Point<length> * Point<length>) list>(
        [ point 0.0 0.0, point 2.0 0.0; point 3.0 0.0, point 6.0 0.0; point 8.0 0.0, point 9.0 0.0 ],
        List.map bounds dashes)

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
