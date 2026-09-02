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

[<Theory>]
[<InlineData("M0 0")>]
[<InlineData("M-1 -1")>]
[<InlineData("M+2 +2")>]
[<InlineData("M.5 .5")>]
[<InlineData("M-.25 -.25")>]
[<InlineData("M1e2 1e2")>]
[<InlineData("M2E-1 2E-1")>]
[<InlineData("M0,0")>]
[<InlineData("M-1,-1")>]
[<InlineData("M+2,+2")>]
[<InlineData("M.5,.5")>]
[<InlineData("M-.25,-.25")>]
[<InlineData("M1e2,1e2")>]
[<InlineData("M2E-1,2E-1")>]
[<InlineData("M0\t0")>]
[<InlineData("M-1\n-1")>]
[<InlineData("M+2\r+2")>]
[<InlineData("M.5\u000c.5")>]
[<InlineData("M-.25 , -.25")>]
[<InlineData("M1e2  ,  1e2")>]
let ``generated coordinate separator cases parse`` source =
    match Parse.path source with
    | Ok _ -> ()
    | Error error -> failwithf "unexpected parse error: %A" error

[<Theory>]
[<InlineData("M0 0A5 8 30 0010 20")>]
[<InlineData("M0 0A5 8 30 00-10-20")>]
[<InlineData("M0 0A5 8 30 00+10+20")>]
[<InlineData("M0 0A5 8 30 0110 20")>]
[<InlineData("M0 0A5 8 30 01-10-20")>]
[<InlineData("M0 0A5 8 30 01+10+20")>]
[<InlineData("M0 0A5 8 30 1010 20")>]
[<InlineData("M0 0A5 8 30 10-10-20")>]
[<InlineData("M0 0A5 8 30 10+10+20")>]
[<InlineData("M0 0A5 8 30 1110 20")>]
[<InlineData("M0 0A5 8 30 11-10-20")>]
[<InlineData("M0 0A5 8 30 11+10+20")>]
let ``generated compact arc flag cases parse`` source =
    match Parse.path source with
    | Ok _ -> ()
    | Error error -> failwithf "unexpected parse error: %A" error

[<Theory>]
[<InlineData("M0 0A5 5 0 -3 0 10 10")>]
[<InlineData("M0 0A5 5 0 -2 0 10 10")>]
[<InlineData("M0 0A5 5 0 -1 0 10 10")>]
[<InlineData("M0 0A5 5 0 2 0 10 10")>]
[<InlineData("M0 0A5 5 0 3 0 10 10")>]
[<InlineData("M0 0A5 5 0 4 0 10 10")>]
[<InlineData("M0 0A5 5 0 5 0 10 10")>]
[<InlineData("M0 0A5 5 0 6 0 10 10")>]
[<InlineData("M0 0A5 5 0 7 0 10 10")>]
[<InlineData("M0 0A5 5 0 8 0 10 10")>]
[<InlineData("M0 0A5 5 0 9 0 10 10")>]
[<InlineData("M0 0A5 5 0 0 -3 10 10")>]
[<InlineData("M0 0A5 5 0 0 -2 10 10")>]
[<InlineData("M0 0A5 5 0 0 -1 10 10")>]
[<InlineData("M0 0A5 5 0 0 2 10 10")>]
[<InlineData("M0 0A5 5 0 0 3 10 10")>]
[<InlineData("M0 0A5 5 0 0 4 10 10")>]
[<InlineData("M0 0A5 5 0 0 5 10 10")>]
[<InlineData("M0 0A5 5 0 0 6 10 10")>]
[<InlineData("M0 0A5 5 0 0 7 10 10")>]
[<InlineData("M0 0A5 5 0 0 8 10 10")>]
[<InlineData("M0 0A5 5 0 0 9 10 10")>]
let ``generated invalid arc flag cases are rejected`` source =
    match Parse.path source with
    | Error _ -> ()
    | Ok path -> failwithf "unexpectedly parsed as %s" (Serialize.path path)

[<Theory>]
[<InlineData("M 210 130 C 145 130 110 80 110 80 S 75 25 10 25 m 0 105 c 65 0 100 -50 100 -50 s 35 -55 100 -55")>]
[<InlineData("M 240 90 c 0 30 7 50 50 0 c 43 -50 50 -30 50 0 c 0 83 -68 -34 -90 -30 C 240 60 240 90 240 90 z")>]
[<InlineData("M80 170 C100 170 160 170 180 170Z")>]
[<InlineData("M5 260 C40 260 60 175 55 160 c -5 15 15 100 50 100Z")>]
[<InlineData("m 200 260 c 50 -40 50 -100 25 -100 s -25 60 25 100")>]
[<InlineData("M 360 100 C 420 90 460 140 450 190")>]
[<InlineData("M360 210 c 0 20 -16 36 -36 36 s -36 -16 -36 -36 s 16 -36 36 -36 s 36 16 36 36 z")>]
[<InlineData("m 360 325 c -40 -60 95 -100 80 0 z")>]
[<InlineData("M 15 20 Q 30 120 130 30 M 180 80 q -75 -100 -163 -60z")>]
[<InlineData("M372 130Q272 50 422 10zm70 0q50-150-80-90z")>]
[<InlineData("M224 103Q234 -12 304 33Z")>]
[<InlineData("M208 168Q258 268 308 168T258 118Q128 88 208 168z")>]
[<InlineData("M 60 100 Q -40 150 60 200 Q 160 150 60 100 z")>]
[<InlineData("M240 296q25-100 47 0t47 0t47 0t47 0t47 0z")>]
[<InlineData("M172 193q-100 50 0 50Q72 243 172 293q100 -50 0 -50Q272 243 172 193z")>]
[<InlineData("M 25 70 A 40 40 0 1 0 25 69 Z")>]
[<InlineData("m 150 100 a 50 40 0 1 0 25 -70 z")>]
[<InlineData("M 350 245 a 40 40 0 1 0 80 60")>]
[<InlineData("M 270 30 A 50 50 0 1 0 345 30 a 50 50 0 1 0 50 0 a 50 50 0 1 0 25 0 z")>]
[<InlineData("M 30 150 a 40 40 0 0 1 65 50 Z m 30 30 A 20 20 0 0 0 125 230 Z m 40 24 a 20 20 0 0 1 65 50 z")>]
[<InlineData("M 215 190 A 40 200 10 0 0 265 190 A 40 200 20 0 1 315 190 A 40 200 30 0 0 365 190 A 40 200 40 0 1 415 190 A 40 200 50 0 0 465 190")>]
[<InlineData("M 62 56 L 113.96152 146 L 10.03848 146 L 62 56 Z M 62 71 L 100.97114 138.5 L 23.02886 138.5 L 62 71 Z")>]
[<InlineData("M 177 56 L 228.96152 146 L 125.03848 146 L 177 56 Z M 177 71 L 215.97114 138.5 L 138.02886 138.5 L 177 71 Z")>]
[<InlineData("m 62 190 l 51.96152 90 l -103.92304 0 l 51.96152 -90 z m 0 15 l 38.97114 67.5 l -77.91228 0 l 38.97114 -67.5 z")>]
[<InlineData("M 240 56 H 270 V 86 H 300 V 116 H 330 V 146 H 240 V 56 Z")>]
[<InlineData("m 240 190 h 30 v 30 h 30 v 30 h 30 v 30 h -90 v -90 z")>]
[<InlineData("M 62 56 113.96152 146 10.03848 146 62 56 Z M 62 71 100.97114 138.5 23.02886 138.5 62 71 Z")>]
[<InlineData("m 62 190 51.96152 90 -103.92304 0 51.96152 -90 z m 0 15 38.97114 67.5 -77.91228 0 38.97114 -67.5 z")>]
[<InlineData("M 100 0 L 100 80 0 40 100 0")>]
[<InlineData("m 100 0 l 0 80 -100 -40 100 -40")>]
[<InlineData("M 0 0 L 100 40 0 80 Z")>]
[<InlineData("m 0 0 l 100 40 -100 40 z")>]
[<InlineData("M 100 100 C 100 20 200 20 200 100 S 300 180 300 100")>]
[<InlineData("M 100 250 S 200 200 200 250 300 300 300 250")>]
[<InlineData("M 240 56 H 270 300 320 400")>]
[<InlineData("M 240 156 V 180 200 260 300")>]
[<InlineData("m 62 56 51.96152 90 -103.92304 0 51.96152 -90 z m 0 15 38.97114 67.5 -77.91228 0 38.97114 -67.5 z")>]
[<InlineData("M 177 56 228.96152 146 125.03848 146 177 56 Z M 177 71 215.97114 138.5 138.02886 138.5 177 71 Z")>]
[<InlineData("M 20 20 Q 50 10 80 20 110 30 140 20 170 10 200 20")>]
[<InlineData("M 20 50 T 50 50 80 50")>]
[<InlineData("M100,120 L160,220 L40,220 z")>]
[<InlineData("M100,120 160,220 40,220 z")>]
[<InlineData("m350,120 60,100 -120,0 z")>]
let ``W3C SVG 1.1 path data survives canonical round trip`` source =
    let first = parsed source
    let serialized = Serialize.path first
    match Parse.path serialized with
    | Ok _ -> ()
    | Error error -> failwithf "canonical serialization did not parse: %A" error

[<Theory>]
[<InlineData("M0 0L0  0")>]
[<InlineData("M0 0L-1  -1")>]
[<InlineData("M0 0L+2  +2")>]
[<InlineData("M0 0L.5  .5")>]
[<InlineData("M0 0L-.25  -.25")>]
[<InlineData("M0 0L1e2  1e2")>]
[<InlineData("M0 0L2E-1  2E-1")>]
[<InlineData("M0 0L0 , 0")>]
[<InlineData("M0 0L-1 , -1")>]
[<InlineData("M0 0L+2 , +2")>]
[<InlineData("M0 0L.5 , .5")>]
[<InlineData("M0 0L-.25 , -.25")>]
[<InlineData("M0 0L1e2 , 1e2")>]
[<InlineData("M0 0L2E-1 , 2E-1")>]
[<InlineData("M0 0L0\t0")>]
[<InlineData("M0 0L-1\t-1")>]
[<InlineData("M0 0L+2\t+2")>]
[<InlineData("M0 0L.5\t.5")>]
[<InlineData("M0 0L-.25\t-.25")>]
[<InlineData("M0 0L1e2\t1e2")>]
[<InlineData("M0 0L2E-1\t2E-1")>]
[<InlineData("M0 0L0\r0")>]
[<InlineData("M0 0L-1\r-1")>]
[<InlineData("M0 0L+2\r+2")>]
[<InlineData("M0 0L.5\r.5")>]
[<InlineData("M0 0L-.25\r-.25")>]
[<InlineData("M0 0L1e2\r1e2")>]
[<InlineData("M0 0L2E-1\r2E-1")>]
[<InlineData("M0 0L0\u000c0")>]
[<InlineData("M0 0L-1\u000c-1")>]
[<InlineData("M0 0L+2\u000c+2")>]
[<InlineData("M0 0L.5\u000c.5")>]
[<InlineData("M0 0L-.25\u000c-.25")>]
[<InlineData("M0 0L1e2\u000c1e2")>]
[<InlineData("M0 0L2E-1\u000c2E-1")>]
let ``generated SVG separator matrix parses`` source =
    match Parse.path source with
    | Ok _ -> ()
    | Error error -> failwithf "unexpected parse error: %A" error
