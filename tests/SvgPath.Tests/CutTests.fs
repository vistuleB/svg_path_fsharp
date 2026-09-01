namespace SvgPath.Tests

open SvgPath
open Xunit

module CutTests =
    let private point x y = Point.create x y
    let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)

    [<Fact>]
    let ``open subject is cut in traversal order`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 30.0<length> 0.0<length>)
        let cutter =
            Subpath.create
                [ line 10.0<length> -5.0<length> 10.0<length> 5.0<length>
                  line 10.0<length> 5.0<length> 20.0<length> 5.0<length>
                  line 20.0<length> 5.0<length> 20.0<length> -5.0<length> ]
        match cutter with
        | Error error -> failwithf "%A" error
        | Ok cutter ->
            match Cut.subpath subject cutter with
            | Error error -> failwithf "%A" error
            | Ok pieces ->
                Assert.Equal(3, List.length pieces)
                Assert.Equal(point 10.0<length> 0.0<length>, Subpath.finish pieces[0])
                Assert.Equal(point 20.0<length> 0.0<length>, Subpath.finish pieces[1])

    [<Fact>]
    let ``partial overlap contributes both cut boundaries`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 3.0<length> 0.0<length> 7.0<length> 0.0<length>)
        match Cut.subpath subject cutter with
        | Error error -> failwithf "%A" error
        | Ok pieces -> Assert.Equal(3, List.length pieces)

    [<Fact>]
    let ``open boundary intersection does not create an empty piece`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 0.0<length> -5.0<length> 0.0<length> 5.0<length>)
        match Cut.subpath subject cutter with
        | Error error -> failwithf "%A" error
        | Ok pieces -> Assert.Equal<Subpath list>([ subject ], pieces)

    [<Fact>]
    let ``one cut opens a closed subject`` () =
        let subjectResult =
            Subpath.create
                [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
                  line 10.0<length> 0.0<length> 10.0<length> 10.0<length>
                  line 10.0<length> 10.0<length> 0.0<length> 10.0<length>
                  line 0.0<length> 10.0<length> 0.0<length> 0.0<length> ]
        match subjectResult with
        | Error error -> failwithf "%A" error
        | Ok openSubject ->
            match Subpath.setClosed true openSubject with
            | Error error -> failwithf "%A" error
            | Ok subject ->
                let cutter = Subpath.ofSegment (line 5.0<length> -5.0<length> 5.0<length> 0.0<length>)
                match Cut.subpath subject cutter with
                | Error error -> failwithf "%A" error
                | Ok pieces ->
                    let opened = Assert.Single pieces
                    Assert.False opened.Closed
                    Assert.Equal(Subpath.start opened, Subpath.finish opened)

    [<Fact>]
    let ``two cuts split a closed subject cyclically`` () =
        let subject =
            Subpath.create
                [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
                  line 10.0<length> 0.0<length> 10.0<length> 10.0<length>
                  line 10.0<length> 10.0<length> 0.0<length> 10.0<length>
                  line 0.0<length> 10.0<length> 0.0<length> 0.0<length> ]
            |> Result.bind (Subpath.setClosed true)
        match subject with
        | Error error -> failwithf "%A" error
        | Ok subject ->
            let cutter = Subpath.ofSegment (line 5.0<length> -5.0<length> 5.0<length> 15.0<length>)
            match Cut.subpath subject cutter with
            | Error error -> failwithf "%A" error
            | Ok pieces ->
                Assert.Equal(2, List.length pieces)
                Assert.All(pieces, fun piece -> Assert.False piece.Closed)

    [<Fact>]
    let ``path cut gathers all cutter subpaths before slicing`` () =
        let subject = Path.singleton (Subpath.ofSegment (line 0.0<length> 0.0<length> 30.0<length> 0.0<length>))
        let cutter =
            Path.ofSubpaths
                [ Subpath.ofSegment (line 20.0<length> -5.0<length> 20.0<length> 5.0<length>)
                  Subpath.ofSegment (line 10.0<length> -5.0<length> 10.0<length> 5.0<length>) ]
        match Cut.path subject cutter with
        | Error error -> failwithf "%A" error
        | Ok result ->
            Assert.Equal(3, List.length result.Subpaths)
            Assert.Equal(point 10.0<length> 0.0<length>, Subpath.finish result.Subpaths[0])
            Assert.Equal(point 20.0<length> 0.0<length>, Subpath.finish result.Subpaths[1])

    [<Fact>]
    let ``path cut validates options even for empty paths`` () =
        let options = { Intersections.defaultOptions with Tolerance = 0.0<length> }
        Assert.Equal(
            Error(InvalidIntersectionTolerance 0.0<length>),
            Cut.pathWith Path.empty Path.empty options)
