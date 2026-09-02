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

[<Theory>]
[<InlineData("M 100 100 L 200 200", "M 100 100 L 200 200")>]
[<InlineData("M\t100\t100\tL\t200\t200", "M 100 100 L 200 200")>]
[<InlineData("M\n100\n100\nL\n200\n200", "M 100 100 L 200 200")>]
[<InlineData("M\r100\r100\rL\r200\r200", "M 100 100 L 200 200")>]
[<InlineData("M\u000c100\u000c100\u000cL\u000c200\u000c200", "M 100 100 L 200 200")>]
[<InlineData("   \t\n\r  M 100,100 L 200,200", "M 100 100 L 200 200")>]
[<InlineData("M 100,100 L 200,200   \t\n\r  ", "M 100 100 L 200 200")>]
[<InlineData("M100,100L200,200", "M 100 100 L 200 200")>]
[<InlineData("M     100     100     L     200     200", "M 100 100 L 200 200")>]
[<InlineData("M 100 , 100 L 200 , 200", "M 100 100 L 200 200")>]
[<InlineData("M 100 ,100 L 200 ,200", "M 100 100 L 200 200")>]
[<InlineData("M 100, 100 L 200, 200", "M 100 100 L 200 200")>]
[<InlineData("M 100,100 A 50,50 0 0,1 200,100", "M 100 100 A 50 50 0 0 1 200 100")>]
[<InlineData("M 100,100 A 50,50 0 01 200,100", "M 100 100 A 50 50 0 0 1 200 100")>]
[<InlineData("M 100,100 A 50,50 0 0 1 200,100", "M 100 100 A 50 50 0 0 1 200 100")>]
[<InlineData("M 50,350 A 25,25 0 0,1 100,350 25,25 0 0,1 150,350", "M 50 350 A 25 25 0 0 1 100 350 A 25 25 0 0 1 150 350")>]
[<InlineData("M 200,300 A -50,50 0 0,1 300,300", "M 200 300 A 50 50 0 0 1 300 300")>]
[<InlineData("M 200,250 A 0,0 0 0,1 300,250", "M 200 250 H 300")>]
[<InlineData("M 20,30 A 10,10 0 1,1 20,30", "M 20 30")>]
[<InlineData("M 100-200 L 200-100", "M 100 -200 L 200 -100")>]
[<InlineData("M 50+100 L 150+200", "M 50 100 L 150 200")>]
[<InlineData("M 10-20+30-40", "M 10 -20 L 30 -40")>]
[<InlineData("M 0.6.5 L 10.5.6", "M 0.6 0.5 L 10.5 0.6")>]
[<InlineData("M .5.6 L .7.8", "M 0.5 0.6 L 0.7 0.8")>]
[<InlineData("M 1.2.3.4.5", "M 1.2 0.3 L 0.4 0.5")>]
[<InlineData("M 1e2,1e2 L 2E2,1.5e2", "M 100 100 L 200 150")>]
[<InlineData("M 1e+2,2e+1", "M 100 20")>]
[<InlineData("M 1e-1,5e-2", "M 0.1 0.05")>]
[<InlineData("M 1.5e2,2.5e1", "M 150 25")>]
[<InlineData("M 5e0,10e0", "M 5 10")>]
[<InlineData("M 1e2-1e2", "M 100 -100")>]
[<InlineData("M 100,100", "M 100 100")>]
[<InlineData("M 50,50 150,50 150,150 50,150 Z", "M 50 50 H 150 V 150 H 50 Z")>]
[<InlineData("M 10,10 20,20 30,30", "M 10 10 L 20 20 L 30 30")>]
[<InlineData("M 10,10 L 20,20 M 30,30 L 40,40", "M 10 10 L 20 20 M 30 30 L 40 40")>]
[<InlineData("m 100,100 L 150,150", "M 100 100 L 150 150")>]
[<InlineData("M 50,50 L 100,50 m 0,50 L 150,150", "M 50 50 H 100 M 100 100 L 150 150")>]
[<InlineData("M 0,0 L 50,0 m 10,10 L 100,50", "M 0 0 H 50 M 60 10 L 100 50")>]
[<InlineData("m 10,10 20,20 30,30", "M 10 10 L 30 30 L 60 60")>]
[<InlineData("M 100,100 m -50,-50 L 100,100", "M 100 100 M 50 50 L 100 100")>]
[<InlineData("M 50,50 m 0,0 L 100,100", "M 50 50 M 50 50 L 100 100")>]
[<InlineData("M 0,0 L 10,10 m 5,5 m 5,5 L 30,30", "M 0 0 L 10 10 M 15 15 M 20 20 L 30 30")>]
[<InlineData("M 50,50 L 150,150", "M 50 50 L 150 150")>]
[<InlineData("M 50,50 l 100,100", "M 50 50 L 150 150")>]
[<InlineData("M 50,200 H 150", "M 50 200 H 150")>]
[<InlineData("M 200,50 V 150", "M 200 50 V 150")>]
[<InlineData("M 0,0 L 10,0 20,0 30,0", "M 0 0 H 10 H 20 H 30")>]
[<InlineData("M 0,50 H 10 20 30", "M 0 50 H 10 H 20 H 30")>]
[<InlineData("M 50,0 V 10 20 30", "M 50 0 V 10 V 20 V 30")>]
[<InlineData("M 50,50 h 100", "M 50 50 H 150")>]
[<InlineData("M 50,50 v 100", "M 50 50 V 150")>]
[<InlineData("M 100,100 h -50 v -50", "M 100 100 H 50 V 50")>]
[<InlineData("M 50,50 L 100,50 L 100,100 L 50,100 Z", "M 50 50 H 100 V 100 H 50 Z")>]
[<InlineData("M 0,0 L 50,0 l 50,0 L 150,0", "M 0 0 H 50 H 100 H 150")>]
[<InlineData("M 0,0 L 100,0 L 100,100 z", "M 0 0 H 100 V 100 Z")>]
[<InlineData("M 50,50 C 100,25 150,75 200,50", "M 50 50 C 100 25 150 75 200 50")>]
[<InlineData("M 50,50 c 50,-25 100,25 150,0", "M 50 50 C 100 25 150 75 200 50")>]
[<InlineData("M 0,50 C 25,0 50,0 75,50 100,100 125,100 150,50", "M 0 50 C 25 0 50 0 75 50 S 125 100 150 50")>]
[<InlineData("M 50,150 C 75,100 100,100 125,150 S 175,200 200,150", "M 50 150 C 75 100 100 100 125 150 S 175 200 200 150")>]
[<InlineData("M 50,50 C 75,25 100,75 125,50 s 50,-25 75,0", "M 50 50 C 75 25 100 75 125 50 S 175 25 200 50")>]
[<InlineData("M 50,50 S 100,25 150,50", "M 50 50 S 100 25 150 50")>]
[<InlineData("M 0,50 C 25,0 50,0 75,50 S 125,100 150,50 175,0 200,50", "M 0 50 C 25 0 50 0 75 50 S 125 100 150 50 S 175 0 200 50")>]
[<InlineData("M 50,50 Q 100,25 150,50", "M 50 50 Q 100 25 150 50")>]
[<InlineData("M 50,50 q 50,-25 100,0", "M 50 50 Q 100 25 150 50")>]
[<InlineData("M 0,50 Q 25,25 50,50 75,75 100,50", "M 0 50 Q 25 25 50 50 T 100 50")>]
[<InlineData("M 50,150 Q 75,125 100,150 T 150,150", "M 50 150 Q 75 125 100 150 T 150 150")>]
[<InlineData("M 50,50 Q 75,25 100,50 t 50,0", "M 50 50 Q 75 25 100 50 T 150 50")>]
[<InlineData("M 50,200 T 100,200", "M 50 200 T 100 200")>]
[<InlineData("M 0,150 Q 25,125 50,150 T 100,150 150,150", "M 0 150 Q 25 125 50 150 T 100 150 T 150 150")>]
[<InlineData("M 0,200 Q 12.5,187.5 25,200 T 50,200 T 75,200", "M 0 200 Q 12.5 187.5 25 200 T 50 200 T 75 200")>]
[<InlineData("M 0,250 C 12.5,237.5 25,237.5 37.5,250 T 75,250", "M 0 250 C 12.5 237.5 25 237.5 37.5 250 T 75 250")>]
[<InlineData("M 0,300 Q 12.5,287.5 25,300 T 50,300 Q 62.5,287.5 75,300", "M 0 300 Q 12.5 287.5 25 300 T 50 300 T 75 300")>]
let ``WPT path data parses to its canonical form`` source expected =
    Assert.Equal(expected, source |> parsed |> Serialize.path)

[<Theory>]
[<InlineData("M 10,10 L 50,50 L 23.,100")>]
[<InlineData("M 0,0 L 10,10 L 20.,30.")>]
[<InlineData("M 0,0 L 15. 20")>]
[<InlineData("M 100,100 L 150,100 L 150,150 L 100,150 Z M 200.,200.")>]
[<InlineData("M 10,10 L 50,50 X 100,100")>]
[<InlineData("M 10,60 L 50,60 L 100")>]
[<InlineData("M 10,110 L 50,110 60,110 70")>]
[<InlineData("M 10,160 L 50,160 C 60,150 70,170")>]
[<InlineData("M 10,210 L 50,210 A 25,25 0 2,1 100,210")>]
[<InlineData("L 100,260")>]
[<InlineData("M 10,310 L 50,310 X 60,310 Y 70,310")>]
[<InlineData("M 0,0 L 50,50 C 60,40 70,60 80,50 C 90,40 100,60")>]
[<InlineData("M 10,360 L 50,360 L 60 L 100,360")>]
[<InlineData("M 100,10 x 150,10")>]
[<InlineData(",M0 0")>]
[<InlineData("M0,,0")>]
[<InlineData("M0, ,0")>]
[<InlineData("M0 0,")>]
[<InlineData("M0 0,L1 1")>]
[<InlineData("M0 0 L,1 1")>]
[<InlineData("M0 0 Z,")>]
let ``invalid WPT and grammar boundary cases are rejected`` source =
    match Parse.path source with
    | Error _ -> ()
    | Ok path -> failwithf "unexpectedly parsed as %s" (Serialize.path path)
