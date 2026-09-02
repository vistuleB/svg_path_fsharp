module SvgPath.Tests.ParseTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private parsed input = Parse.path input |> Result.defaultWith (failwithf "%A")
let private onlySubpath input = (parsed input).Subpaths |> List.exactlyOne

[<Fact>]
let ``quadratic cubic and smooth controls reflect`` () =
    let subpath = onlySubpath "M0 0 Q10 0 10 10 T20 20 C20 30 30 30 30 20 S40 10 50 20"
    match subpath.Segments with
    | [ QuadraticBezier(_, _, _); QuadraticBezier(_, secondControl, _); CubicBezier(_, _, _, _); CubicBezier(_, fourthControl, _, _) ] ->
        Assert.Equal(point 10.0 20.0, secondControl)
        Assert.Equal(point 30.0 10.0, fourthControl)
    | segments -> failwithf "unexpected segments: %A" segments

[<Fact>]
let ``close inserts bridge and preserves semantic closure`` () =
    let subpath = onlySubpath "M0 0 L10 0 L10 10 Z"
    Assert.True subpath.Closed
    Assert.Equal(3, subpath.Segments.Length)
    Assert.Equal(subpath.Start, Subpath.finish subpath)

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
let ``F#-specific coverage: generated SVG separator matrix parses`` () =
    [
        "M0 0L0  0"
        "M0 0L-1  -1"
        "M0 0L+2  +2"
        "M0 0L.5  .5"
        "M0 0L-.25  -.25"
        "M0 0L1e2  1e2"
        "M0 0L2E-1  2E-1"
        "M0 0L0 , 0"
        "M0 0L-1 , -1"
        "M0 0L+2 , +2"
        "M0 0L.5 , .5"
        "M0 0L-.25 , -.25"
        "M0 0L1e2 , 1e2"
        "M0 0L2E-1 , 2E-1"
        "M0 0L0\t0"
        "M0 0L-1\t-1"
        "M0 0L+2\t+2"
        "M0 0L.5\t.5"
        "M0 0L-.25\t-.25"
        "M0 0L1e2\t1e2"
        "M0 0L2E-1\t2E-1"
        "M0 0L0\r0"
        "M0 0L-1\r-1"
        "M0 0L+2\r+2"
        "M0 0L.5\r.5"
        "M0 0L-.25\r-.25"
        "M0 0L1e2\r1e2"
        "M0 0L2E-1\r2E-1"
        "M0 0L0\u000c0"
        "M0 0L-1\u000c-1"
        "M0 0L+2\u000c+2"
        "M0 0L.5\u000c.5"
        "M0 0L-.25\u000c-.25"
        "M0 0L1e2\u000c1e2"
        "M0 0L2E-1\u000c2E-1"
    ]
    |> List.iter (fun source ->
        match Parse.path source with
        | Ok _ -> ()
        | Error error -> failwithf "unexpected parse error: %A" error
    )
