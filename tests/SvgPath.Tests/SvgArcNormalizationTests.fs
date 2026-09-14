module SvgPath.Tests.SvgArcNormalizationTests

open SvgPath
open Xunit

// One test per test in Gleam's svg_path_svg_arc_normalization_test.gleam.
let private p x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
let private ok result = result |> Result.defaultWith (failwithf "%A")
let private parse source = Parse.path source |> ok
let private arc radius endpoint =
    Arc { Start = p 0.0 0.0; Radius = radius; XAxisRotation = 30.0<degree>
          LargeArc = true; Sweep = false; End = endpoint }
let private path segment = Path.singleton (Subpath.ofSegment segment)
let private eq expected actual = Assert.True((expected = actual), sprintf "Expected %A; got %A" expected actual)

[<Fact>]
let ``strict cubic conversion accepts empty and non arc paths`` () =
    eq (Ok Path.empty) (Path.toCubicBeziersStrict Path.empty)
    let source = parse "M1 2 M0 0L0 0Q1 2 3 4C4 5 6 7 8 9"
    eq (Ok(Path.toCubicBeziers source)) (Path.toCubicBeziersStrict source)

[<Fact>]
let ``strict cubic conversion matches forgiving for correctable radii`` () =
    let source = parse "M0 0A-2 -3 30 1 0 10 0 M20 20A5 5 0 0 1 25 25"
    let before = Serialize.path source
    eq (Ok(Path.toCubicBeziers source)) (Path.toCubicBeziersStrict source)
    eq before (Serialize.path source)
    match source |> Path.subpaths |> List.head |> Subpath.segments with
    | [Arc a] -> eq (p -2.0 -3.0) a.Radius
    | other -> failwithf "%A" other

[<Fact>]
let ``strict cubic conversion rejects zero small and coincident arcs`` () =
    for text in [ "M0 0A0 10 0 0 1 10 0"; "M0 0A10 0 0 0 1 10 0"
                  "M0 0A0.000000001 10 0 0 1 10 0"; "M0 0A10 -0.000000001 0 0 1 10 0"
                  "M0 0A10 10 0 1 1 0 0" ] do
        let source = parse text
        let subpath = source |> Path.subpaths |> List.exactlyOne
        let segment = subpath |> Subpath.segments |> List.exactlyOne
        eq (Error DegenerateArc) (Path.toCubicBeziersStrict source)
        eq (Error DegenerateArc) (Subpath.toCubicBeziersStrict subpath)
        eq (Error DegenerateArc) (Segment.toCubicBeziersStrict segment)
        eq (Error DegenerateArc) (Segment.arcsToCubicBeziersStrict segment)
        match Segment.toCubicBeziers segment with
        | [CubicBezier(a, _, _, b)] -> eq (Segment.start segment) a; eq (Segment.finish segment) b
        | other -> failwithf "%A" other

[<Fact>]
let ``strict cubic conversion propagates later arc error`` () =
    let source = parse "M0 0A5 5 0 0 1 10 0 M20 0L21 0A0 10 0 0 1 22 0"
    eq (Error DegenerateArc) (Path.toCubicBeziersStrict source)
    let normalized = Path.normalizeSvgArcs source
    eq (Ok(Path.toCubicBeziers normalized)) (Path.toCubicBeziersStrict normalized)

[<Fact>]
let ``strict arc only conversion preserves non arcs`` () =
    for segment in parse "M0 0L1 2Q3 4 5 6C7 8 9 10 11 12" |> Path.subpaths |> List.exactlyOne |> Subpath.segments do
        eq (Ok [segment]) (Segment.arcsToCubicBeziersStrict segment)
        eq (Ok(Segment.toCubicBeziers segment)) (Segment.toCubicBeziersStrict segment)

[<Fact>]
let ``strict cubic conversion preserves closure and exact endpoints`` () =
    let source = parse "M2 3Z M0.3 0.7A5 3 15 1 1 8.2 4.6Z"
    let converted = Path.toCubicBeziersStrict source |> ok
    eq (Path.toCubicBeziers source) converted
    let parts = Path.subpaths converted
    eq true parts[0].Closed
    eq [] parts[0].Segments
    eq (p 2.0 3.0) parts[0].Start
    eq true parts[1].Closed
    eq (p 0.3 0.7) parts[1].Start
    eq (p 0.3 0.7) (Subpath.finish parts[1])
    let a = source |> Path.subpaths |> List.item 1 |> Subpath.segments |> List.head
    eq (Ok(Segment.arcsToCubicBeziers a)) (Segment.arcsToCubicBeziersStrict a)

[<Fact>]
let ``parser preserves signed arc arguments`` () =
    eq (path (arc (p -2.0 -3.0) (p 10.0 0.0))) (parse "M0 0A-2 -3 30 1 0 10 0")

[<Fact>]
let ``manually constructed unusual arcs roundtrip`` () =
    for segment in [ arc (p 0.0 1000000.0) (p 10.0 0.0); arc (p 1000000.0 0.0) (p 10.0 0.0)
                     arc (p -2.0 -3.0) (p 10.0 0.0); arc (p 2.0 3.0) (p 0.0 0.0); arc (p 0.0 3.0) (p 0.0 0.0) ] do
        let source = path segment
        eq source (parse (Serialize.path source))
        eq source (parse (Serialize.pathWith source Serialize.relativeOptions))

[<Fact>]
let ``coincident arc after close preserves new subpath`` () =
    let source = parse "M0 0L1 0Z A2 3 0 1 0 0 0"
    let parts = Path.subpaths source
    eq 2 parts.Length
    eq true parts[0].Closed
    eq false parts[1].Closed
    eq 1 parts[1].Segments.Length
    eq source (parse (Serialize.path source))

[<Fact>]
let ``preserved arc resets smooth control`` () =
    eq (parse "M0 0Q1 2 3 4A0 8 0 0 1 3 4Q3 4 6 7") (parse "M0 0Q1 2 3 4A0 8 0 0 1 3 4T6 7")

[<Fact>]
let ``zero radius normalizes to exact chord`` () =
    for radius in [p 0.0 1000000.0; p 1000000.0 0.0] do
        eq (Some(Line(p 0.0 0.0, p 10.0 0.0))) (Segment.normalizeSvgArc (arc radius (p 10.0 0.0)))

[<Fact>]
let ``coincidence takes precedence over zero radius`` () =
    eq None (Segment.normalizeSvgArc (arc (p 0.0 1000000.0) (p 0.0 0.0)))

[<Fact>]
let ``negative radii normalize without enlargement`` () =
    eq (Some(arc (p 2.0 3.0) (p 10.0 0.0))) (Segment.normalizeSvgArc (arc (p -2.0 -3.0) (p 10.0 0.0)))

[<Fact>]
let ``normalization preserves empty subpaths and closure`` () =
    let normalized = parse "M2 3A0 8 0 1 1 2 3Z M4 5A7 8 0 1 1 4 5 M6 7" |> Path.normalizeSvgArcs
    eq (Path.ofSubpaths [Subpath.empty(p 2.0 3.0) |> Subpath.close |> ok; Subpath.empty(p 4.0 5.0); Subpath.empty(p 6.0 7.0)]) normalized
    eq normalized (Path.normalizeSvgArcs normalized)

[<Fact>]
let ``normalization preserves zero length lines`` () =
    let line = Line(p 0.0 0.0, p 0.0 0.0)
    eq (Some line) (Segment.normalizeSvgArc line)
    eq (path line) (Path.normalizeSvgArcs (path line))

[<Fact>]
let ``mixed arc normalization preserves continuity`` () =
    let normalized = parse "M0 0A0 8 0 0 1 10 0A2 3 0 1 1 10 0A-2 -3 0 0 1 20 0Z" |> Path.normalizeSvgArcs
    eq "M 0 0 H 10 A 2 3 0 0 1 20 0 Z" (Serialize.path normalized)
    eq normalized (Path.normalizeSvgArcs normalized)

[<Fact>]
let ``geometric degeneracy rejects undefined arcs`` () =
    for segment in [arc (p 0.0 1000000.0) (p 10.0 0.0); arc (p 1000000.0 0.0) (p 10.0 0.0)
                    arc (p 2.0 3.0) (p 0.0 0.0); arc (p 0.0 3.0) (p 0.0 0.0)] do
        let subpath = Subpath.ofSegment segment
        eq (Error DegenerateArc) (Segment.toLines segment)
        eq (Error DegenerateArc) (Subpath.toLines subpath)
        eq (Error DegenerateArc) (Path.toLines (Path.singleton subpath))
        eq (Error(Degeneracy.DegeneracyPathError DegenerateArc)) (Degeneracy.segmentLinearizeIfDegenerate segment 0.001<length>)
        eq (Error(Degeneracy.DegeneracyPathError DegenerateArc)) (Degeneracy.subpathLinearizeIfDegenerate subpath 0.001<length>)
        eq (Error(Degeneracy.DegeneracyPathError DegenerateArc)) (Degeneracy.normalizeDegenerateSegments subpath 0.001<length>)
        eq (Error(Effects.EffectsPathError DegenerateArc)) (Effects.normalizeDegenerateSegments subpath 0.001<length>)

[<Fact>]
let ``undefined arc in thin run cannot be hidden by hull`` () =
    let source = parse "M0 0L1 0A0 100 0 0 1 2 0L3 0" |> Path.subpaths |> List.exactlyOne
    eq (Error(Degeneracy.DegeneracyPathError DegenerateArc)) (Degeneracy.normalizeDegenerateSegments source 0.001<length>)
    let simplified = Degeneracy.normalizeDegenerateSegments (Subpath.normalizeSvgArcs source) 0.001<length> |> ok
    eq [Line(p 0.0 0.0, p 3.0 0.0)] simplified.Segments

[<Fact>]
let ``valid narrow arc can still be simplified`` () =
    let segment = Arc {Start=p 0.0 0.0; Radius=p 0.0001 10.0; XAxisRotation=0.0<degree>; LargeArc=true; Sweep=true; End=p 0.0 10.0}
    let lines = Degeneracy.segmentLinearizeIfDegenerate segment 0.001<length> |> ok |> Option.get
    Assert.True(lines.Length > 1)
    let simplified = Subpath.create lines |> ok
    eq (Segment.start segment) simplified.Start
    eq (Segment.finish segment) (Subpath.finish simplified)
