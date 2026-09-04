module SvgPath.Tests.ParameterTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private t value = Parameter.fromFloat value
let private at segmentIndex value = { SegmentIndex = segmentIndex; T = t value }
let private line a b = Line(point a 0.0, point b 0.0)
let private openLines count =
    [ 0 .. count - 1 ]
    |> List.map (fun index -> line (float index * 10.0) (float (index + 1) * 10.0))
    |> Subpath.create
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``compare subpath parameters orders by segment then t`` () =
    Assert.True(Subpath.parametersCompare (at 0 0.75) (at 1 0.25) < 0)
    Assert.Equal(0, Subpath.parametersCompare (at 1 0.25) (at 1 0.25))
    Assert.True(Subpath.parametersCompare (at 2 0.0) (at 1 1.0) > 0)

[<Fact>]
let ``compare path parameters orders by subpath then subpath parameter`` () =
    let pathAt subpathIndex segmentIndex value = { SubpathIndex = subpathIndex; At = at segmentIndex value }
    Assert.True(Path.parametersCompare (pathAt 0 3 0.75) (pathAt 1 0 0.25) < 0)
    Assert.Equal(0, Path.parametersCompare (pathAt 1 0 0.25) (pathAt 1 0 0.25))
    Assert.True(Path.parametersCompare (pathAt 1 2 0.0) (pathAt 1 1 1.0) > 0)

[<Fact>]
let ``from end parameter converts reversed address to original address`` () =
    let subpath = openLines 4
    Assert.Equal(Ok(at 3 1.0), Subpath.parameterFromEnd subpath 0 (t 0.0))
    Assert.Equal(Ok(at 3 0.0), Subpath.parameterFromEnd subpath 0 (t 1.0))
    Assert.Equal(Ok(at 1 0.75), Subpath.parameterFromEnd subpath 2 (t 0.25))

[<Fact>]
let ``from end parameter rejects empty subpaths`` () =
    let empty = Subpath.empty (point 0.0 0.0)
    Assert.Equal(Error EmptySubpath, Subpath.parameterFromEnd empty 0 (t 0.0))

[<Fact>]
let ``from end parameter rejects invalid reversed address`` () =
    let subpath = openLines 2
    Assert.Equal(Error(InvalidSubpathParameter(2, t 0.0, 2)), Subpath.parameterFromEnd subpath 2 (t 0.0))
    Assert.Equal(Error(InvalidSubpathParameter(0, t -0.1, 2)), Subpath.parameterFromEnd subpath 0 (t -0.1))

[<Fact>]
let ``canonicalize subpath parameter only normalizes exact boundaries`` () =
    let subpath = openLines 2
    Assert.Equal(Ok(at 1 0.0), Subpath.parameterCanonicalize subpath (at 0 1.0))
    Assert.Equal(Ok(at 0 0.9999999), Subpath.parameterCanonicalize subpath (at 0 0.9999999))

[<Fact>]
let ``snap subpath parameter snaps internal segment end`` () =
    let openSubpath = openLines 2
    Assert.Equal(Ok(at 1 0.0), Subpath.parameterSnapToBoundary openSubpath (at 0 0.9999999) (t 1.0e-6))

[<Fact>]
let ``snap subpath parameter keeps open final endpoint`` () =
    let one = openLines 1
    Assert.Equal(Ok(at 0 1.0), Subpath.parameterSnapToBoundary one (at 0 0.9999999) (t 1.0e-6))

[<Fact>]
let ``snap subpath parameter snaps closed wrap`` () =
    let closed =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok(at 0 0.0), Subpath.parameterSnapToBoundary closed (at 2 0.9999999) (t 1.0e-6))

[<Fact>]
let ``snap subpath parameter rejects invalid inputs`` () =
    let subpath = openLines 1
    Assert.Equal(
        Error(InvalidParameterSnapTolerance 0.0<parameter>),
        Subpath.parameterSnapToBoundary subpath (at 0 0.5) (t 0.0))
    Assert.Equal(
        Error(InvalidSubpathParameter(1, t 0.5, 1)),
        Subpath.parameterSnapToBoundary subpath (at 1 0.5) (t 1.0e-6))

[<Fact>]
let ``subpath point evaluates segment address`` () =
    let subpath = openLines 2
    Assert.Equal(Ok(point 15.0 0.0), Subpath.point subpath (at 1 0.5))

[<Fact>]
let ``subpath derivative evaluates segment address`` () =
    let subpath = openLines 2
    Assert.Equal(
        Ok(Point.create 10.0<length / parameter> 0.0<length / parameter>),
        Subpath.derivative subpath (at 1 0.5))

[<Fact>]
let ``split subpath splits inside segment`` () =
    let subpath = openLines 2
    let left, right = Subpath.split subpath (at 0 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 5.0 0.0, Segment.finish (List.last left.Segments))
    Assert.Equal(point 5.0 0.0, right.Start)

[<Fact>]
let ``split subpath splits at internal vertex`` () =
    let subpath = openLines 2
    let before, after = Subpath.split subpath (at 0 1.0) |> Result.defaultWith (failwithf "%A")
    Assert.Single(before.Segments) |> ignore
    Assert.Single(after.Segments) |> ignore
    Assert.Equal(point 10.0 0.0, after.Start)

[<Fact>]
let ``split subpath rejects closed empty boundary and outside parameters`` () =
    let openSubpath = openLines 2
    Assert.True(Subpath.split openSubpath (at 0 0.0) |> Result.isError)
    Assert.True(Subpath.split openSubpath (at 1 1.0) |> Result.isError)
    Assert.True(Subpath.split openSubpath (at 2 0.0) |> Result.isError)
    let closed = Subpath.setClosed true openSubpath
    Assert.True(closed |> Result.bind (fun value -> Subpath.split value (at 0 0.5)) |> Result.isError)

[<Fact>]
let ``from end parameter can address open at`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let ab, bc, cd, da = Line(a, b), Line(b, c), Line(c, d), Line(d, a)
    let closed =
        Subpath.create [ ab; bc; cd; da ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let opening = Subpath.parameterFromEnd closed 2 (t 1.0) |> Result.defaultWith (failwithf "%A")
    let opened = Subpath.openAt closed opening |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ bc; cd; da; ab ], opened.Segments)
    Assert.Equal(b, opened.Start)

[<Fact>]
let ``from end parameter can address subpath between`` () =
    let straight = openLines 3
    let fromValue = Subpath.parameterFromEnd straight 2 (t 0.0) |> Result.defaultWith (failwithf "%A")
    let toValue = Subpath.parameterFromEnd straight 0 (t 1.0) |> Result.defaultWith (failwithf "%A")
    let middle = Subpath.between straight fromValue toValue |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ line 10.0 20.0 ], middle.Segments)

[<Fact>]
let ``subpath derivative uses canonical next segment at internal vertices`` () =
    let subpath =
        Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 20.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Ok(Point.create 0.0<length / parameter> 20.0<length / parameter>),
        Subpath.derivative subpath (at 0 1.0))

[<Fact>]
let ``subpath point and derivative reject invalid parameters`` () =
    Assert.Equal(Error(InvalidSubpathParameter(1, t 0.0, 1)), Subpath.point (openLines 1) (at 1 0.0))
    Assert.Equal(Error(InvalidSubpathParameter(0, t -0.1, 1)), Subpath.derivative (openLines 1) (at 0 -0.1))

[<Fact>]
let ``subpath between extracts open interval across segments`` () =
    let straight = openLines 3
    let piece = Subpath.between straight (at 0 0.5) (at 2 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([ line 5.0 10.0; line 10.0 20.0; line 20.0 25.0 ], piece.Segments)

[<Fact>]
let ``subpath between rejects equal and reversed open intervals`` () =
    let straight = openLines 3
    Assert.Equal(Error(InvalidSubpathInterval(at 0 0.5, at 0 0.5)), Subpath.between straight (at 0 0.5) (at 0 0.5))
    Assert.Equal(Error(InvalidSubpathInterval(at 1 0.5, at 0 0.5)), Subpath.between straight (at 1 0.5) (at 0 0.5))

[<Fact>]
let ``subpath between wraps closed intervals`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let closed =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let wrapped = Subpath.between closed (at 2 0.5) (at 1 0.5) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, wrapped.Segments.Length)
    Assert.Equal(point 5.0 10.0, wrapped.Start)
    Assert.Equal(point 10.0 5.0, Segment.finish (List.last wrapped.Segments))

[<Fact>]
let ``subpaths between open returns outer pieces`` () =
    let pieces = Subpath.betweenMany (openLines 3) [ at 0 0.5; at 2 0.5 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, pieces.Length)
    Assert.Equal<Segment list>([ line 0.0 5.0 ], pieces[0].Segments)
    Assert.Equal<Segment list>([ line 5.0 10.0; line 10.0 20.0; line 20.0 25.0 ], pieces[1].Segments)
    Assert.Equal<Segment list>([ line 25.0 30.0 ], pieces[2].Segments)

[<Fact>]
let ``subpaths between open rejects boundary and duplicate points`` () =
    let subpath = openLines 2
    Assert.Equal(
        Error(InvalidSubpathParameter(0, t 0.0, 2)),
        Subpath.betweenMany subpath [ at 0 0.0 ])
    Assert.Equal(
        Error(InvalidSubpathInterval(at 1 0.0, at 1 0.0)),
        Subpath.betweenMany subpath [ at 0 1.0; at 1 0.0 ])

[<Fact>]
let ``subpaths between closed accepts cyclic order`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let closed =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let pieces = Subpath.betweenMany closed [ at 2 0.5; at 3 0.5; at 1 0.5 ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, pieces.Length)
    Assert.Equal(2, pieces[0].Segments.Length)
    Assert.Equal(3, pieces[1].Segments.Length)
    Assert.Equal(2, pieces[2].Segments.Length)

[<Fact>]
let ``subpaths between closed accepts single split point`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let closed =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let opened = Subpath.betweenMany closed [ at 1 0.5 ] |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.False(opened.Closed)
    Assert.Equal(point 10.0 5.0, opened.Start)
    Assert.Equal(point 10.0 5.0, Segment.finish (List.last opened.Segments))

[<Fact>]
let ``subpaths between closed rejects duplicate and nonlinear order`` () =
    let a, b, c, d = point 0.0 0.0, point 10.0 0.0, point 10.0 10.0, point 0.0 10.0
    let closed =
        Subpath.create [ Line(a, b); Line(b, c); Line(c, d); Line(d, a) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Error(InvalidSubpathInterval(at 0 0.0, at 0 0.0)),
        Subpath.betweenMany closed [ at 3 1.0; at 0 0.0 ])
    Assert.Equal(
        Error(InvalidSubpathInterval(at 3 0.5, at 2 0.5)),
        Subpath.betweenMany closed [ at 2 0.5; at 1 0.5; at 3 0.5 ])
