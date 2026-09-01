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
