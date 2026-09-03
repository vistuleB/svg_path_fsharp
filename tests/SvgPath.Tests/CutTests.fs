namespace SvgPath.Tests

open SvgPath
open Xunit

module CutTests =
    let private point x y = Point.create x y
    let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)

    [<Fact>]
    let ``subpath cut splits open subject in order`` () =
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
    let ``subpath cut returns subject when no intersections`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 0.0<length> 5.0<length> 10.0<length> 5.0<length>)
        Assert.Equal(Ok [ subject ], Cut.subpath subject cutter)

    [<Fact>]
    let ``subpath cut splits at partial overlap boundaries`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 3.0<length> 0.0<length> 7.0<length> 0.0<length>)
        match Cut.subpath subject cutter with
        | Error error -> failwithf "%A" error
        | Ok pieces -> Assert.Equal(3, List.length pieces)

    [<Fact>]
    let ``subpath cut ignores full overlap at open boundaries`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        Assert.Equal(Ok [ subject ], Cut.subpath subject subject)

    [<Fact>]
    let ``subpath cut ignores open subject endpoint intersections`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 0.0<length> -5.0<length> 0.0<length> 5.0<length>)
        match Cut.subpath subject cutter with
        | Error error -> failwithf "%A" error
        | Ok pieces -> Assert.Equal<Subpath list>([ subject ], pieces)

    [<Fact>]
    let ``subpath cut dedupes internal boundary aliases`` () =
        let middle = point 10.0<length> 0.0<length>
        let subject =
            Subpath.create
                [ Line(point 0.0<length> 0.0<length>, middle)
                  Line(middle, point 20.0<length> 0.0<length>) ]
            |> Result.defaultWith (failwithf "%A")
        let cutter = Subpath.ofSegment (line 10.0<length> -5.0<length> 10.0<length> 5.0<length>)
        let pieces = Cut.subpath subject cutter |> Result.defaultWith (failwithf "%A")
        Assert.Equal("M 0 0 H 10 M 10 0 H 20", Serialize.path (Path.ofSubpaths pieces))

    [<Fact>]
    let ``subpath cut opens closed subject at single cut`` () =
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
    let ``subpath cut splits closed subject cyclically`` () =
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
    let ``subpath cut propagates intersection option errors`` () =
        let subject = Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>)
        let cutter = Subpath.ofSegment (line 5.0<length> -5.0<length> 5.0<length> 5.0<length>)
        Assert.Equal(
            Error(InvalidIntersectionTolerance 0.0<length>),
            Cut.subpathWith subject cutter { Intersections.defaultOptions with Tolerance = 0.0<length> })

    [<Fact>]
    let ``path cut cuts each subject subpath by all cutter subpaths`` () =
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
    let ``path cut empty subject returns empty path`` () =
        let cutter = Path.singleton (Subpath.ofSegment (line 5.0<length> -5.0<length> 5.0<length> 5.0<length>))
        Assert.Equal(Ok Path.empty, Cut.path Path.empty cutter)

    [<Fact>]
    let ``path cut empty subject still validates options`` () =
        let options = { Intersections.defaultOptions with Tolerance = 0.0<length> }
        Assert.Equal(
            Error(InvalidIntersectionTolerance 0.0<length>),
            Cut.pathWith Path.empty Path.empty options)

    [<Fact>]
    let ``path cut empty cutter returns subject path`` () =
        let subject = Path.singleton (Subpath.ofSegment (line 0.0<length> 0.0<length> 10.0<length> 0.0<length>))
        Assert.Equal(Ok subject, Cut.path subject Path.empty)

    [<Fact>]
    let ``path cut dedupes near internal boundary aliases`` () =
        let middle = point 10.0<length> 0.0<length>
        let subjectSubpath =
            Subpath.create
                [ Line(point 0.0<length> 0.0<length>, middle)
                  Line(middle, point 20.0<length> 0.0<length>) ]
            |> Result.defaultWith (failwithf "%A")
        let cutter =
            Path.ofSubpaths
                [ Subpath.ofSegment (line 9.9999999999<length> -5.0<length> 9.9999999999<length> 5.0<length>)
                  Subpath.ofSegment (line 10.0000000001<length> -5.0<length> 10.0000000001<length> 5.0<length>) ]
        let options =
            { Intersections.defaultOptions with
                Tolerance = 1.0e-6<length>
                MaxDepth = 48
                ParameterSnap = DecimalParameterSnap 7 }
        let result = Cut.pathWith (Path.singleton subjectSubpath) cutter options |> Result.defaultWith (failwithf "%A")
        Assert.Equal("M 0 0 H 10 M 10 0 H 20", Serialize.path result)
