module SvgPath.Tests.EndpointContextTests
open SvgPath
open Xunit
let private line a b = Line(Point.create (Length.fromFloat a) 0.0<length>, Point.create (Length.fromFloat b) 0.0<length>)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private context first last closing = { First = first; Last = last; Closing = closing }
[<Fact>]
let ``endpoint context two segments first and last`` () =
    let segments = [line 0. 1.; line 1. 2.]
    let path = Subpath.createWith (Custom(fun previous next actual ->
        Assert.Equal(context true true false, actual)
        [previous;next])) segments |> get
    Assert.Equal<Segment list>(segments,path.Segments)
[<Fact>]
let ``endpoint context tracks input when replacements expand`` () =
    let segments = [line 0. 1.; line 1. 2.; line 2. 3.; line 3. 4.]
    let path = Subpath.createWith (Custom(fun previous next actual ->
        let x = float (Segment.start next).X
        Assert.Equal(context (x = 1.) (x = 3.) false,actual)
        [previous;line x x;next])) segments |> get
    Assert.Equal(7,path.Segments.Length)
[<Fact>]
let ``endpoint context first does not repeat after pair deletion`` () =
    let segments = [line 0. 1.; line 1. 2.; line 2. 3.; line 3. 4.]
    let path = Subpath.createWith (Custom(fun previous next actual ->
        if (Segment.start next).X = 1.0<length> then
            Assert.Equal(context true false false,actual)
            []
        else
            Assert.Equal(context false true false,actual)
            [previous;next])) segments |> get
    Assert.Equal<Segment list>([line 2. 3.;line 3. 4.],path.Segments)
[<Fact>]
let ``endpoint context coalescing keeps last flag`` () =
    let path = Subpath.createWith (Custom(fun previous next actual ->
        let last = (Segment.finish next).X = 3.0<length>
        Assert.Equal(context (not last) last false,actual)
        [Line(Segment.start previous,Segment.finish next)])) [line 0. 1.;line 1. 2.;line 2. 3.] |> get
    Assert.Equal<Segment list>([line 0. 3.],path.Segments)
[<Fact>]
let ``set closed reapplies policy only at closing boundary`` () =
    let closed = Subpath.create [line 0. 1.;line 1. 0.] |> get |> Subpath.setClosed true |> get
    let reclosed = Subpath.setClosedWith (Custom(fun previous next actual ->
        Assert.Equal(context false false true,actual)
        Assert.Equal(line 1. 0.,previous)
        Assert.Equal(line 0. 1.,next)
        [line 1. 0.5;line 0.5 0.])) true closed |> get
    Assert.True(reclosed.Closed)
    Assert.Equal<Segment list>([line 0. 1.;line 1. 0.5;line 0.5 0.],reclosed.Segments)
[<Fact>]
let ``closed singleton rebuild calls policy once`` () =
    let only = line 0. 0.
    let closed = Subpath.create [only] |> get |> Subpath.setClosed true |> get
    let rebuilt = Subpath.rebuildWith (Custom(fun previous next actual ->
        Assert.Equal(only,previous)
        Assert.Equal(only,next)
        Assert.Equal(context false false true,actual)
        [line 0. 1.;line 1. 0.])) closed |> get
    Assert.True(rebuilt.Closed)
    Assert.Equal<Segment list>([line 0. 1.;line 1. 0.],rebuilt.Segments)
[<Fact>]
let ``empty closure and rebuild do not call policy`` () =
    let empty = Subpath.empty (Point.create 3.0<length> 4.0<length>)
    let policy = Custom(fun _ _ _ -> failwith "Empty subpath has no pair")
    let closed = Subpath.setClosedWith policy true empty |> get
    let reclosed = Subpath.setClosedWith policy true closed |> get
    let rebuilt = Subpath.rebuildWith policy closed |> get
    Assert.True(closed.Closed)
    Assert.True(List.isEmpty closed.Segments)
    Assert.Equal(empty.Start,closed.Start)
    Assert.Equal(closed,reclosed)
    Assert.Equal(closed,rebuilt)
[<Fact>]
let ``open singleton rebuild does not call policy`` () =
    let only = Subpath.create [line 0. 1.] |> get
    let rebuilt = Subpath.rebuildWith (Custom(fun _ _ _ -> failwith "Singleton has no forward pair")) only |> get
    Assert.Equal(only,rebuilt)
[<Fact>]
let ``closed rebuild distinguishes last forward pair from closure`` () =
    let closed = Subpath.create [line 0. 1.;line 1. 0.] |> get |> Subpath.setClosed true |> get
    let rebuilt = Subpath.rebuildWith (Custom(fun previous next actual ->
        if (Segment.start previous).X = 0.0<length> then
            Assert.Equal(context true true false,actual)
            [previous;next]
        else
            Assert.Equal(context false false true,actual)
            [previous])) closed |> get
    Assert.Equal(closed,rebuilt)
