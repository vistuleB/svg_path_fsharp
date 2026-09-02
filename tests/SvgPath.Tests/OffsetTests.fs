module SvgPath.Tests.OffsetTests

open SvgPath
open Xunit

let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)

[<Fact>]
let ``subpath offset map uses arc length and visual left normal`` () =
    let subpath =
        Subpath.create [ Line(point 0.0 0.0, point 3.0 0.0); Line(point 3.0 0.0, point 3.0 4.0) ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let mapping = Offset.subpathOffsetMap subpath |> Result.defaultWith (fun error -> failwithf "%A" error)
    let first = mapping (point 2.0 1.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    let second = mapping (point 5.0 1.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(Point.distance first (point 2.0 -1.0) < 1.0e-12<length>)
    Assert.True(Point.distance second (point 4.0 2.0) < 1.0e-12<length>)

[<Fact>]
let ``closed subpath offset map wraps traveled distance`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let mapping = Offset.subpathOffsetMap subpath |> Result.defaultWith (fun error -> failwithf "%A" error)
    let wrapped = mapping (point 9.0 0.0) |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(Point.distance wrapped (point 1.0 0.0) < 1.0e-12<length>)

[<Fact>]
let ``line offset follows the visual left normal exactly`` () =
    let result =
        Offset.segment (Line(point 0.0 0.0, point 10.0 0.0)) 2.0<length>
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal<Segment list>([ Line(point 0.0 -2.0, point 10.0 -2.0) ], result.Segments)

[<Fact>]
let ``quadratic offset fit satisfies parameter samples`` () =
    let source = QuadraticBezier(point 0.0 0.0, point 5.0 8.0, point 10.0 0.0)
    let result = Offset.segment source 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.NotEmpty(result.Segments)
    Assert.True(result.Segments |> List.forall (function CubicBezier _ -> true | _ -> false))

[<Fact>]
let ``offset rejects a negative tangent heal angle`` () =
    let options = { Offset.defaultOptions with TangentHealAngleDegrees = -1.0<degree> }
    match Offset.segmentWith (Line(point 0.0 0.0, point 1.0 0.0)) 1.0<length> options with
    | Error(InvalidTangentHealAngleDegrees angle) -> Assert.Equal(-1.0<degree>, angle)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``untrimmed open offset inserts a bevel join`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 2.0 0.0); Line(point 2.0 0.0, point 2.0 2.0) ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let options = { Offset.defaultOptions with Join = Bevel }
    let result = Offset.subpathUntrimmedWith source 0.5<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(3, List.length result.Segments)
    Assert.False(result.Closed)
    Assert.Equal(Segment.finish result.Segments[0], Segment.start result.Segments[1])
    Assert.Equal(Segment.finish result.Segments[1], Segment.start result.Segments[2])

[<Fact>]
let ``untrimmed join styles match the public polyline contracts`` () =
    let source =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let render join =
        Offset.subpathUntrimmedWith source 2.0<length> { Offset.defaultOptions with Join = join }
        |> Result.defaultWith (failwithf "%A")
        |> Serialize.subpath
    Assert.Equal("M 0 -2 H 10 L 12 0 V 10", render Bevel)
    Assert.Equal("M 0 -2 H 10 H 12 V 0 V 10", render (Miter 4.0))
    Assert.Equal("M 0 -2 H 10 A 2 2 0 0 1 12 0 V 10", render Round)

[<Fact>]
let ``untrimmed closed offset preserves closure`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 4.0; point 0.0 4.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let options = { Offset.defaultOptions with Join = Round }
    let result = Offset.subpathUntrimmedWith source 0.5<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.True(result.Closed)
    Assert.Equal(result.Start, result.Segments |> List.last |> Segment.finish)
    Assert.Contains(result.Segments, function Arc _ -> true | _ -> false)

[<Fact>]
let ``circular arc offset remains circular`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 0.0 10.0 }
    let result = Offset.segment source 2.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    match result.Segments with
    | [ Arc offsetArc ] ->
        Assert.Equal(12.0<length>, offsetArc.Radius.X)
        Assert.Equal(12.0<length>, offsetArc.Radius.Y)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``circular arc offset reverses sweep after crossing its center`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = false
              End = point 0.0 -10.0 }
    let result = Offset.segment source 12.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    match result.Segments with
    | [ Arc offsetArc ] ->
        Assert.Equal(point -2.0 0.0, offsetArc.Start)
        Assert.Equal(2.0<length>, offsetArc.Radius.X)
        Assert.Equal(point 0.0 2.0, offsetArc.End)
        Assert.False(offsetArc.Sweep)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment rejects a circular arc at collapsed offset radius`` () =
    let source =
        Arc
            { Start = point 10.0 0.0
              Radius = point 10.0 10.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 0.0 10.0 }
    match Offset.segment source -10.0<length> with
    | Error(SvgPath.Error.DegenerateTangent parameterValue) -> Assert.Equal(0.0<parameter>, parameterValue)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment rejects a zero-length line`` () =
    match Offset.segment (Line(point 1.0 2.0, point 1.0 2.0)) 1.0<length> with
    | Error(SvgPath.Error.DegenerateTangent parameterValue) -> Assert.Equal(0.0<parameter>, parameterValue)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``untrimmed path offsets each subpath`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let second = Subpath.ofSegment (Line(point 0.0 2.0, point 1.0 2.0))
    let result =
        Offset.pathUntrimmed (Path.ofSubpaths [ first; second ]) 0.5<length>
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``trimmed path normalization drops empty source subpaths`` () =
    let source =
        Path.ofSubpaths
            [ Subpath.empty (point 0.0 0.0)
              Subpath.ofSegment (Line(point 0.0 2.0, point 3.0 2.0)) ]
    let untrimmed = Offset.pathUntrimmed source 0.5<length> |> Result.defaultWith (failwithf "%A")
    let trimmed = Offset.path source 0.5<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, untrimmed.Subpaths.Length)
    Assert.Empty(untrimmed.Subpaths[0].Segments)
    Assert.Single(trimmed.Subpaths) |> ignore

[<Fact>]
let ``untrimmed band shares recursive subdivision between sides`` () =
    let source =
        Subpath.ofSegment (CubicBezier(point 0.0 0.0, point 0.0 12.0, point 10.0 -12.0, point 10.0 0.0))
    let options =
        { Offset.defaultOptions with
            Fitting = { Offset.defaultFittingOptions with Tolerance = 0.005<length> } }
    let result =
        Offset.subpathBandUntrimmedWith source -1.0<length> 2.0<length> options
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)
    Assert.Equal(result.Subpaths[0].Segments.Length, result.Subpaths[1].Segments.Length)
    Assert.True(result.Subpaths[0].Segments.Length > 1)

[<Fact>]
let ``untrimmed band accepts reversed offset order`` () =
    let source =
        Subpath.ofSegment (Line(point 0.0 0.0, point 2.0 1.0))
    let forward =
        Offset.subpathBandUntrimmed source -0.5<length> 1.0<length>
        |> Result.defaultWith (failwithf "%A")
    let reversed =
        Offset.subpathBandUntrimmed source 1.0<length> -0.5<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(forward.Subpaths[0], reversed.Subpaths[1])
    Assert.Equal(forward.Subpaths[1], reversed.Subpaths[0])

[<Fact>]
let ``closed rectangular band produces two closed contours`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (fun error -> failwithf "%A" error)
    let result = Offset.subpathBand source -1.0<length> 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``open line band returns two capless offset sides`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let result = Offset.subpathBand source -1.0<length> 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.False(subpath.Closed))
    let segments = result.Subpaths |> List.collect Subpath.segments
    Assert.Contains(Line(point 0.0 1.0, point 10.0 1.0), segments)
    Assert.Contains(Line(point 0.0 -2.0, point 10.0 -2.0), segments)

[<Fact>]
let ``closed square negative offset produces the exact inset`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpath source -2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths[0].Closed)
    Assert.Equal<Segment list>(
        [ Line(point 2.0 2.0, point 8.0 2.0)
          Line(point 8.0 2.0, point 8.0 8.0)
          Line(point 8.0 8.0, point 2.0 8.0)
          Line(point 2.0 8.0, point 2.0 2.0) ],
        result.Subpaths[0].Segments)

[<Fact>]
let ``exchanging band offsets reverses the oriented result`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let forward = Offset.subpathBand source -1.0<length> 1.0<length> |> Result.defaultWith (failwithf "%A")
    let reversed = Offset.subpathBand source 1.0<length> -1.0<length> |> Result.defaultWith (failwithf "%A")
    let forwardArea = Area.signedPath forward
    let reversedArea = Area.signedPath reversed
    Assert.True(
        abs (forwardArea + reversedArea) < 1.0e-9<length^2>,
        sprintf "forward=%A reversed=%A\nforward path=%s\nreversed path=%s"
            forwardArea reversedArea (Serialize.path forward) (Serialize.path reversed))

[<Fact>]
let ``open line round stroke produces one closed contour`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let options = { Offset.defaultOptions with Join = Round }
    let result = Offset.subpathStrokeWith source 2.0<length> RoundCap options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths[0].Closed)
    Assert.Equal(2, result.Subpaths[0].Segments |> List.filter (function Arc _ -> true | _ -> false) |> List.length)

[<Fact>]
let ``path stroke processes every source subpath`` () =
    let source =
        Path.ofSubpaths
            [ Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
              Subpath.ofSegment (Line(point 0.0 5.0, point 10.0 5.0)) ]
    let result = Offset.pathStroke source 1.0<length> |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``path offset trims straight subpaths in one shared pipeline`` () =
    let first =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 -10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let second =
        Subpath.create [ Line(point 0.0 20.0, point 10.0 20.0); Line(point 10.0 20.0, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.path (Path.ofSubpaths [ first; second ]) 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 -2 H 8 V -10 M 0 18 H 8 V 10", Serialize.path result)

[<Fact>]
let ``path offset orients nested closed contours by depth`` () =
    let square size inset =
        Subpath.polygon
            [ point inset inset
              point (inset + size) inset
              point (inset + size) (inset + size)
              point inset (inset + size) ]
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.path (Path.ofSubpaths [ square 20.0 0.0; square 6.0 7.0 ]) 0.5<length>
        |> Result.defaultWith (failwithf "%A")
    let areas = result.Subpaths |> List.map Area.signedSubpath
    Assert.Equal(2, areas.Length)
    Assert.Equal(1, areas |> List.filter (fun area -> area > 0.0<length^2>) |> List.length)
    Assert.Equal(1, areas |> List.filter (fun area -> area < 0.0<length^2>) |> List.length)

[<Fact>]
let ``path offset shares trimming across closed and open sources`` () =
    let closed =
        Subpath.polygon [ point 0.0 0.0; point 8.0 0.0; point 8.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let openSource = Subpath.ofSegment (Line(point 20.0 0.0, point 28.0 0.0))
    let result =
        Offset.path (Path.ofSubpaths [ closed; openSource ]) 0.5<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)
    Assert.Equal(1, result.Subpaths |> List.filter Subpath.isClosed |> List.length)
    Assert.Equal(1, result.Subpaths |> List.filter (Subpath.isClosed >> not) |> List.length)

[<Fact>]
let ``no-trimming single offset returns its untrimmed walk`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 3.0 0.0))
    let options =
        { Offset.defaultOptions with
            SingleOffsetTrimming = { Offside = false; FinalTrimming = NoTrimming } }
    let result = Offset.subpathWith source 1.0<length> options |> Result.defaultWith (fun error -> failwithf "%A" error)
    Assert.Equal<Segment list>([ Line(point 0.0 -1.0, point 3.0 -1.0) ], result.Subpaths[0].Segments)

[<Fact>]
let ``single offset final trimming policies execute their distinct pipelines`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 8.0 0.0; point 8.0 6.0; point 0.0 6.0 ]
        |> Result.defaultWith (failwithf "%A")
    let run finalTrimming =
        let options =
            { Offset.defaultOptions with
                SingleOffsetTrimming =
                    { Offside = false
                      FinalTrimming = finalTrimming } }
        Offset.subpathWith source 0.5<length> options
        |> Result.defaultWith (failwithf "%A")
    let untrimmed = run NoTrimming
    let cuspTrimmed = run CuspTrimming
    let inBandTrimmed = run InBandTrimming
    Assert.Single(untrimmed.Subpaths) |> ignore
    Assert.Single(cuspTrimmed.Subpaths) |> ignore
    Assert.Single(inBandTrimmed.Subpaths) |> ignore
    Assert.All(cuspTrimmed.Subpaths, fun subpath -> Assert.True(subpath.Closed))
    Assert.All(inBandTrimmed.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``band cusp switches execute side-local trimming before final trimming`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let run innerCusps outerCusps =
        let options =
            { Offset.defaultOptions with
                BandTrimming =
                    { InnerCusps = innerCusps
                      OuterCusps = outerCusps
                      InBand = false } }
        Offset.subpathBandWith source -1.0<length> 1.0<length> options
        |> Result.defaultWith (failwithf "%A")
    for innerCusps, outerCusps in [ false, false; true, false; false, true; true, true ] do
        let result = run innerCusps outerCusps
        Assert.Equal(2, result.Subpaths.Length)
        Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``closed square band matches the public contour contract`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpathBand source -2.0<length> 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        "M 2 2 V 8 H 8 V 2 Z M 0 -2 H 10 H 12 V 0 V 10 V 12 H 10 H 0 H -2 V 10 V 0 V -2 Z",
        Serialize.path result)

[<Fact>]
let ``closed square stroke uses the same capless band`` () =
    let source =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Offset.subpathStroke source 4.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        "M 2 2 V 8 H 8 V 2 Z M 0 -2 H 10 H 12 V 0 V 10 V 12 H 10 H 0 H -2 V 10 V 0 V -2 Z",
        Serialize.path result)

[<Fact>]
let ``figure eight band reconstructs three closed contours`` () =
    let source =
        Subpath.create
            [ CubicBezier(point 0.0 0.0, point -336.0 -234.0, point -336.0 234.0, point 0.0 0.0)
              CubicBezier(point 0.0 0.0, point 336.0 -234.0, point 336.0 234.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.subpathBandWith source 18.0<length> 34.0<length> { Offset.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, result.Subpaths.Length)
    Assert.All(result.Subpaths, fun subpath -> Assert.True(subpath.Closed))

[<Fact>]
let ``figure eight untrimmed band completes`` () =
    let source =
        Subpath.create
            [ CubicBezier(point 0.0 0.0, point -336.0 -234.0, point -336.0 234.0, point 0.0 0.0)
              CubicBezier(point 0.0 0.0, point 336.0 -234.0, point 336.0 234.0, point 0.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let result =
        Offset.subpathBandUntrimmedWith
            source 18.0<length> 34.0<length>
            { Offset.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)

[<Fact>]
let ``zero-length stroke follows cap semantics`` () =
    let source = Subpath.ofSegment (Line(point 2.0 3.0, point 2.0 3.0))
    let butt = Offset.subpathStrokeWith source 4.0<length> Butt Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    let round = Offset.subpathStrokeWith source 4.0<length> RoundCap Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    let square = Offset.subpathStrokeWith source 4.0<length> Square Offset.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Empty(butt.Subpaths)
    Assert.Equal(2, round.Subpaths[0].Segments.Length)
    Assert.Equal(4, square.Subpaths[0].Segments.Length)

[<Fact>]
let ``open line stroke cap geometry matches public contracts`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let render cap =
        Offset.subpathStrokeWith source 2.0<length> cap Offset.defaultOptions
        |> Result.defaultWith (failwithf "%A")
        |> Serialize.path
    Assert.Equal("M 0 -1 H 10 V 1 H 0 Z", render Butt)
    Assert.Equal("M 0 -1 H 10 H 11 V 1 H 10 H 0 H -1 V -1 Z", render Square)
