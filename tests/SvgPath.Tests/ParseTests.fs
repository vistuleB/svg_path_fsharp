module SvgPath.Tests.ParseTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parsed input = Parse.path input |> Result.defaultWith (failwithf "%A")
let private onlySubpath input = (parsed input).Subpaths |> List.exactlyOne

[<Fact>]
let ``empty and none parse as empty paths`` () =
    Assert.Empty((parsed "").Subpaths)
    Assert.Empty((parsed "none").Subpaths)

[<Fact>]
let ``absolute and repeated lines parse`` () =
    let subpath = onlySubpath "M 0 0 L 10 0 10 10 H 5 V 3"
    Assert.Equal(4, subpath.Segments.Length)
    Assert.Equal(point 5.0 3.0, Subpath.finish subpath)

[<Fact>]
let ``relative move and lines use current points`` () =
    let subpath = onlySubpath "m 2 3 4 0 0 5"
    Assert.Equal(point 2.0 3.0, subpath.Start)
    Assert.Equal(point 6.0 8.0, Subpath.finish subpath)

[<Fact>]
let ``quadratic cubic and smooth controls reflect`` () =
    let subpath = onlySubpath "M0 0 Q10 0 10 10 T20 20 C20 30 30 30 30 20 S40 10 50 20"
    match subpath.Segments with
    | [ QuadraticBezier(_, _, _); QuadraticBezier(_, secondControl, _); CubicBezier(_, _, _, _); CubicBezier(_, fourthControl, _, _) ] ->
        Assert.Equal(point 10.0 20.0, secondControl)
        Assert.Equal(point 30.0 10.0, fourthControl)
    | segments -> failwithf "unexpected segments: %A" segments

[<Fact>]
let ``arc flags may be concatenated`` () =
    let subpath = onlySubpath "M0 0 A10 20 30 0110 20"
    match subpath.Segments with
    | [ Arc arc ] ->
        Assert.False arc.LargeArc
        Assert.True arc.Sweep
        Assert.Equal(point 10.0 20.0, arc.End)
    | segments -> failwithf "unexpected segments: %A" segments

[<Fact>]
let ``zero-radius arc becomes a line and coincident arc disappears`` () =
    let line = onlySubpath "M0 0 A0 2 0 0 1 5 0"
    Assert.True(match line.Segments with [ Line _ ] -> true | _ -> false)
    let absent = onlySubpath "M0 0 A2 2 0 0 1 0 0"
    Assert.Empty absent.Segments

[<Fact>]
let ``close inserts bridge and preserves semantic closure`` () =
    let subpath = onlySubpath "M0 0 L10 0 L10 10 Z"
    Assert.True subpath.Closed
    Assert.Equal(3, subpath.Segments.Length)
    Assert.Equal(subpath.Start, Subpath.finish subpath)

[<Fact>]
let ``compact signed and exponent numbers parse`` () =
    let subpath = onlySubpath "M+1e1-2E1L15-20"
    Assert.Equal(point 10.0 -20.0, subpath.Start)
    Assert.Equal(point 15.0 -20.0, Subpath.finish subpath)

[<Fact>]
let ``finite compensated exponent is retained`` () =
    let subpath = onlySubpath "M0.1e309 0"
    Assert.True(System.Double.IsFinite(float subpath.Start.X))

[<Fact>]
let ``overflowing number is rejected at its suffix`` () =
    Assert.Equal(Error(ParseError(InvalidNumber "1e400", "1e400 0")), Parse.path "M 1e400 0")

[<Fact>]
let ``invalid flags commands and separators report exact suffixes`` () =
    Assert.Equal(Error(ParseError(ExpectedArcFlag, "2 1 10 20")), Parse.path "M0 0 A1 1 0 2 1 10 20")
    Assert.Equal(Error(ParseError(UnsupportedCommand "X", "X 1 2")), Parse.path "M0 0 X 1 2")
    Assert.Equal(Error(ParseError(InvalidSeparator, ",0 0")), Parse.path "M,0 0")

[<Fact>]
let ``move-only subpaths are retained`` () =
    let path = parsed "M0 0 M10 10 L20 10"
    Assert.Equal(2, path.Subpaths.Length)
    Assert.Empty(path.Subpaths[0].Segments)

[<Fact>]
let ``relative move after close starts from closed start`` () =
    let path = parsed "M10 10 L20 10 Z m5 0 l5 0"
    Assert.Equal(point 15.0 10.0, path.Subpaths[1].Start)
    Assert.Equal(point 20.0 10.0, Subpath.finish path.Subpaths[1])
