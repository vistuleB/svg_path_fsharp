module SvgPath.Tests.OffsetShortRunsTests
open SvgPath
open Xunit
let private p x = Point.create (Length.fromFloat x) 0.0<length>
let private get result = result |> Result.defaultWith (failwithf "%A")
let private normalize (source: Subpath) =
    let normalized = Offset.normalizeShortSourceRuns source 0.001<length> |> get
    Assert.Equal(source.Start,normalized.Start)
    Assert.Equal(Subpath.finish source,Subpath.finish normalized)
    Assert.Equal(source.Closed,normalized.Closed)
    normalized
[<Fact>]
let ``balanced short run has no greedy remainder`` () =
    let points = [0..7] |> List.map (fun i -> p (float i * 0.0009))
    let source = Subpath.polyline (p -1. :: (points @ [p 1.])) |> get
    let segments = (normalize source).Segments
    Assert.Equal(5,segments.Length)
    Assert.Equal(List.head source.Segments,List.head segments)
    Assert.Equal(List.last source.Segments,List.last segments)
    for chunk in segments |> List.skip 1 |> List.take 3 do
        let bound = Segment.lengthUpperBound chunk |> get
        Assert.True(bound > 0.001<length> && bound <= 0.003<length>)
[<Fact>]
let ``long short run preserves large backtracking extent`` () =
    let outward = [0..100] |> List.map (fun i -> p (float i * 0.0009))
    let inward = List.rev outward |> List.skip 1
    let source = Subpath.polyline (p -1. :: (outward @ inward @ [p -1.])) |> get
    let normalized = normalize source
    Assert.Equal(Subpath.boundingBox source |> get,Subpath.boundingBox normalized |> get)
    let length,original = Subpath.length normalized |> get,Subpath.length source |> get
    Assert.True(abs(length - original) < 1e-12<length>)
[<Fact>]
let ``short run preserves neighbors instead of moving them`` () =
    let first,last = Line(p -1.,p 0.),Line(p 0.001,p 1.)
    let source = Subpath.create [first;Line(p 0.,p 0.0005);Line(p 0.0005,p 0.001);last] |> get
    Assert.Equal<Segment list>([first;Line(p 0.,p 0.001);last],(normalize source).Segments)
[<Fact>]
let ``coincident endpoint curve is not mistaken for short segment`` () =
    let curve = QuadraticBezier(p 0.,p 10.,p 0.)
    let source = Subpath.create [Line(p -1.,p 0.);curve;Line(p 0.,p 1.)] |> get
    Assert.Equal(source,normalize source)
[<Fact>]
let ``short run keeps small first and last segments`` () =
    let source = Subpath.create [Line(p 0.,p 0.0001);Line(p 0.0001,p 1.);Line(p 1.,p 1.0001)] |> get
    Assert.Equal(source,normalize source)
[<Fact>]
let ``short run preserves closed empty and singleton subpaths`` () =
    let empty = Subpath.empty (p 1.)
    let singleton = Subpath.create [Line(p 0.,p 0.)] |> get
    let closed = Subpath.close singleton |> get
    Assert.Equal(empty,normalize empty)
    Assert.Equal(singleton,normalize singleton)
    Assert.Equal(closed,normalize closed)
    let source = Subpath.create [Line(p 0.,p 0.0004);Line(p 0.0004,p 0.0006);Line(p 0.0006,p 0.0008);Line(p 0.0008,p 0.)] |> get |> Subpath.close |> get
    Assert.Equal(3,(normalize source).Segments.Length)
[<Fact>]
let ``zero length run is handled without division by zero`` () =
    let source = Subpath.create [Line(p -1.,p 0.);Line(p 0.,p 0.);Line(p 0.,p 0.);Line(p 0.,p 1.)] |> get
    Assert.Equal(2.0<length>,Subpath.length (normalize source) |> get)
[<Fact>]
let ``short run rejects invalid tolerance`` () =
    Assert.Equal(Error(Offset.InternalInvalidTolerance 0.0<length>),Offset.normalizeShortSourceRuns (Subpath.empty (p 0.)) 0.0<length>)
