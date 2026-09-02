namespace SvgPath.Tests

open SvgPath
open Xunit

module ClipTests =
    let private point x y = Point.create x y
    let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)

    let private rectangle () =
        Subpath.create
            [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
              line 10.0<length> 0.0<length> 10.0<length> 10.0<length>
              line 10.0<length> 10.0<length> 0.0<length> 10.0<length>
              line 0.0<length> 10.0<length> 0.0<length> 0.0<length> ]
        |> Result.bind (Subpath.setClosed true)

    let private rectangleAt minX minY maxX maxY =
        Subpath.polygon
            [ point minX minY; point maxX minY; point maxX maxY; point minX maxY ]
        |> Result.defaultWith (failwithf "%A")

    let private rectanglePath minX minY maxX maxY =
        Path.singleton (rectangleAt minX minY maxX maxY)

    [<Fact>]
    let ``line is clipped to filled rectangle`` () =
        match rectangle () with
        | Error error -> failwithf "%A" error
        | Ok boundary ->
            let input = Subpath.ofSegment (line -5.0<length> 5.0<length> 15.0<length> 5.0<length>)
            match Clip.subpath input (Path.singleton boundary) Nonzero with
            | Error error -> failwithf "%A" error
            | Ok pieces ->
                let piece = Assert.Single pieces
                Assert.Equal(point 0.0<length> 5.0<length>, Subpath.start piece)
                Assert.Equal(point 10.0<length> 5.0<length>, Subpath.finish piece)

    [<Fact>]
    let ``outside line is discarded`` () =
        match rectangle () with
        | Error error -> failwithf "%A" error
        | Ok boundary ->
            let input = Subpath.ofSegment (line -5.0<length> 15.0<length> 15.0<length> 15.0<length>)
            match Clip.subpath input (Path.singleton boundary) Nonzero with
            | Error error -> failwithf "%A" error
            | Ok pieces -> Assert.Empty pieces

    [<Fact>]
    let ``inside closed subpath remains closed`` () =
        match rectangle () with
        | Error error -> failwithf "%A" error
        | Ok boundary ->
            let inside =
                Subpath.create
                    [ line 2.0<length> 2.0<length> 4.0<length> 2.0<length>
                      line 4.0<length> 2.0<length> 4.0<length> 4.0<length>
                      line 4.0<length> 4.0<length> 2.0<length> 4.0<length>
                      line 2.0<length> 4.0<length> 2.0<length> 2.0<length> ]
                |> Result.bind (Subpath.setClosed true)
            match inside with
            | Error error -> failwithf "%A" error
            | Ok inside ->
                match Clip.subpath inside (Path.singleton boundary) Nonzero with
                | Error error -> failwithf "%A" error
                | Ok pieces -> Assert.True((Assert.Single pieces).Closed)

    [<Fact>]
    let ``input coincident with clipping boundary is retained`` () =
        match rectangle () with
        | Error error -> failwithf "%A" error
        | Ok boundary ->
            let input = Subpath.ofSegment (line -5.0<length> 0.0<length> 15.0<length> 0.0<length>)
            match Clip.subpath input (Path.singleton boundary) Nonzero with
            | Error error -> failwithf "%A" error
            | Ok pieces ->
                let piece = Assert.Single pieces
                Assert.Equal(point 0.0<length> 0.0<length>, Subpath.start piece)
                Assert.Equal(point 10.0<length> 0.0<length>, Subpath.finish piece)

    [<Fact>]
    let ``empty path still validates options`` () =
        let options = { Clip.defaultOptions with Tolerance = 0.0<length> }
        Assert.Equal(
            Error(InvalidIntersectionTolerance 0.0<length>),
            Clip.pathWith Path.empty Path.empty Nonzero options)

    [<Fact>]
    let ``open subpath clips to multiple open pieces without bridges`` () =
        let input =
            Subpath.create
                [ line -5.0<length> 2.0<length> 5.0<length> 2.0<length>
                  line 5.0<length> 2.0<length> 15.0<length> 2.0<length>
                  line 15.0<length> 2.0<length> 15.0<length> 8.0<length>
                  line 15.0<length> 8.0<length> 5.0<length> 8.0<length>
                  line 5.0<length> 8.0<length> -5.0<length> 8.0<length> ]
            |> Result.defaultWith (failwithf "%A")
        let pieces =
            Clip.subpath input (rectanglePath 0.0<length> 0.0<length> 10.0<length> 10.0<length>) Nonzero
            |> Result.defaultWith (failwithf "%A")
        Assert.Equal(2, pieces.Length)
        Assert.All(pieces, fun piece -> Assert.False piece.Closed)
        Assert.Equal(2, pieces[0].Segments.Length)
        Assert.Equal(2, pieces[1].Segments.Length)

    [<Fact>]
    let ``clip boundary at subpath vertex does not duplicate split`` () =
        let input =
            Subpath.create
                [ line -5.0<length> 5.0<length> 0.0<length> 5.0<length>
                  line 0.0<length> 5.0<length> 5.0<length> 5.0<length> ]
            |> Result.defaultWith (failwithf "%A")
        let clipped =
            Clip.subpath input (rectanglePath 0.0<length> 0.0<length> 10.0<length> 10.0<length>) Nonzero
            |> Result.defaultWith (failwithf "%A")
            |> List.exactlyOne
        Assert.Equal<Segment list>([ line 0.0<length> 5.0<length> 5.0<length> 5.0<length> ], clipped.Segments)

    [<Fact>]
    let ``partial clip boundary overlap splits at overlap endpoint`` () =
        let input = Subpath.ofSegment (line 2.0<length> 0.0<length> 12.0<length> 0.0<length>)
        let clipped =
            Clip.subpath input (rectanglePath 0.0<length> 0.0<length> 10.0<length> 10.0<length>) Nonzero
            |> Result.defaultWith (failwithf "%A")
            |> List.exactlyOne
        Assert.Equal<Segment list>([ line 2.0<length> 0.0<length> 10.0<length> 0.0<length> ], clipped.Segments)

    [<Fact>]
    let ``cut parameter deduplication uses path coordinate tolerance`` () =
        let input = Subpath.ofSegment (line -1.0<length> 0.5<length> 1.0<length> 0.5<length>)
        let region = rectanglePath 0.0<length> 0.0<length> 0.0000005<length> 1.0<length>
        Assert.Equal(Ok [], Clip.subpathWith input region Nonzero { Clip.defaultOptions with Tolerance = 1.0e-6<length> })

    [<Fact>]
    let ``closed cut parameter deduplication wraps across subpath seam`` () =
        let input = rectangleAt 0.0<length> 0.0<length> 10.0<length> 10.0<length>
        let region = rectanglePath -1.0<length> -1.0<length> 0.0000002<length> 0.0000002<length>
        Assert.Equal(Ok [], Clip.subpathWith input region Nonzero { Clip.defaultOptions with Tolerance = 1.0e-6<length> })

    [<Fact>]
    let ``closed circle clips to open arc fragments`` () =
        let input =
            Subpath.create
                [ Arc { Start = point 10.0<length> 0.0<length>; Radius = point 10.0<length> 10.0<length>; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point -10.0<length> 0.0<length> }
                  Arc { Start = point -10.0<length> 0.0<length>; Radius = point 10.0<length> 10.0<length>; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = point 10.0<length> 0.0<length> } ]
            |> Result.bind (Subpath.setClosed true)
            |> Result.defaultWith (failwithf "%A")
        let clipped =
            Clip.subpath input (rectanglePath -20.0<length> -5.0<length> 20.0<length> 5.0<length>) Nonzero
            |> Result.defaultWith (failwithf "%A")
        Assert.Equal(2, clipped.Length)
        Assert.All(clipped, fun piece ->
            Assert.False piece.Closed
            Assert.Contains(piece.Segments, function Arc _ -> true | _ -> false))

    [<Fact>]
    let ``path clipping preserves subpath order`` () =
        let input =
            Path.ofSubpaths
                [ Subpath.ofSegment (line -5.0<length> 2.0<length> 15.0<length> 2.0<length>)
                  Subpath.ofSegment (line -5.0<length> 8.0<length> 15.0<length> 8.0<length>) ]
        let clipped =
            Clip.path input (rectanglePath 0.0<length> 0.0<length> 10.0<length> 10.0<length>) Nonzero
            |> Result.defaultWith (failwithf "%A")
        Assert.Equal(point 0.0<length> 2.0<length>, clipped.Subpaths[0].Start)
        Assert.Equal(point 0.0<length> 8.0<length>, clipped.Subpaths[1].Start)
