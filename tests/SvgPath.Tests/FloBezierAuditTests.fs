module SvgPath.Tests.FloBezierAuditTests

open SvgPath
open Xunit

// Regression cases mirrored from the Gleam FloBezier audit, closed at 575a5b2.
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private ok result = result |> Result.defaultWith (failwithf "%A")
let inline private near expected (actual:float<'u>) = Assert.True(abs(float actual-expected)<1e-9, sprintf "Expected %.17g; got %.17g" expected (float actual))
let private constants p = [Line(p,p);QuadraticBezier(p,p,p);CubicBezier(p,p,p,p)]
let private hit a b p : Intersections.SegmentIntersection = {LeftT=Parameter.fromFloat a;RightT=Parameter.fromFloat b;Point=p}
let private rejected a b =
    Assert.True(Overlaps.segment a b = Error NonAffineOverlapCorrespondence)
    Assert.True(Intersections.segment a b = Error NonAffineOverlapCorrespondence)
    Assert.True(Encounters.segment a b = Error NonAffineOverlapCorrespondence)

[<Fact>]
let ``folded collinear overlaps reject non affine correspondence`` () =
    let fixtures =
        [ Line(p 1. 1.,p 2. 2.),QuadraticBezier(p 0. 0.,p 3. 3.,p 0.5 0.5)
          QuadraticBezier(p 0. 0.,p 1. 1.,p 0.25 0.25),CubicBezier(p 2. 2.,p 0.5 0.5,p 0.25 0.25,p -1. -1.)
          QuadraticBezier(p 0.1 0.1,p 0.8 0.8,p 0.2 0.2),QuadraticBezier(p 1. 1.,p 0.5 0.5,p 0.3 0.3) ]
    for a,b in fixtures do rejected a b; rejected b a

[<Fact>]
let ``folded line overlap contact and separation`` () =
    for scale in [0.01;1.;1000.] do
        for vertical in [false;true] do
            let at x = if vertical then p 17. (-23.+scale*x) else p (17.+scale*x) -23.
            for curve in [QuadraticBezier(at 0.,at 4.,at 0.);CubicBezier(at 0.,at (8./3.),at (8./3.),at 0.)] do
                let overlapping = Line(at 1.,at 3.)
                rejected overlapping curve
                rejected curve overlapping
                let touching = Line(at 2.,at 3.)
                Assert.Empty(Overlaps.segment touching curve |> ok)
                let forward = Intersections.segment touching curve |> ok |> Assert.Single
                near 0. forward.LeftT; near 0.5 forward.RightT
                let reverse = Intersections.segment curve touching |> ok |> Assert.Single
                near 0.5 reverse.LeftT; near 0. reverse.RightT
                let separate = Line(at 3.,at 4.)
                Assert.Empty(Overlaps.segment separate curve |> ok)
                Assert.Empty(Intersections.segment separate curve |> ok)
                Assert.Empty(Intersections.segment curve separate |> ok)

[<Fact>]
let ``conflicting retraced overlap is not empty success`` () =
    let curve = QuadraticBezier(p 0. 0.,p 4. 0.,p 0. 0.)
    rejected curve curve
    rejected curve (Segment.reverse curve)

[<Fact>]
let ``affine collinear overlaps remain supported`` () =
    let line = Line(p 0. 0.,p 2. 0.)
    let curve = QuadraticBezier(p 0. 0.,p 1. 0.,p 2. 0.)
    for other in [curve;Segment.reverse curve] do
        Assert.Single(Overlaps.segment line other |> ok) |> ignore
        Assert.Single(Overlaps.segment other line |> ok) |> ignore
        Assert.True(Intersections.segment line other = Error OverlappingSegments)

let private lengthFixtures () =
    let q = QuadraticBezier(p 0. 0.,p 3. 1.,p 1. (1./3.))
    let c = CubicBezier(p 0. 0.,p 3. 0.,p -1. 0.,p 2. 0.)
    [q,2.7406406388125954;Segment.between q 0.25<parameter> 0.75<parameter> |> ok,0.7642171012073583
     c,2.7559289460184545;Segment.between c 0.25<parameter> 0.75<parameter> |> ok,0.44342894601845445]

[<Fact>]
let ``folded lengths succeed with defaults and reversal`` () =
    for curve,expected in lengthFixtures() do
        Measure.segmentLength curve |> ok |> near expected
        Measure.segmentLength (Segment.reverse curve) |> ok |> near expected

[<Fact>]
let ``folded lengths partition before spending depth`` () =
    for curve,expected in lengthFixtures() do
        Measure.segmentLengthWith curve {Tolerance=1e-9<length>;MaxDepth=1} |> ok |> near expected

[<Fact>]
let ``folded lengths preserve scale translation and axis`` () =
    for curve,expected in lengthFixtures() do
        for scale in [0.001;1.;1000.] do
            for swap in [false;true] do
                let moved = Segment.mapPoints (fun at ->
                    if swap then p (17.+scale*float at.Y) (-23.+scale*float at.X)
                    else p (17.+scale*float at.X) (-23.+scale*float at.Y)) curve |> ok
                Measure.segmentLength moved |> ok |> near (scale*expected)

[<Fact>]
let ``folded length inversion preserves traveled distance`` () =
    let curve = QuadraticBezier(p 0. 0.,p 3. 0.,p 1. 0.)
    Measure.segmentParameterAtLength curve 2.0<length> |> ok |> near 0.8
    let at = Measure.segmentPointAtLength curve 2.0<length> |> ok
    near 1.6 at.X; near 0. at.Y

[<Fact>]
let ``zero and near collinear lengths`` () =
    let at = p 17. -23.
    Assert.True(Measure.segmentLength (CubicBezier(at,at,at,at)) = Ok 0.0<length>)
    let curve = CubicBezier(p 0. 0.,p 3. 1e-8,p -1. -1e-8,p 2. 0.)
    Assert.True(abs(float(Measure.segmentLength curve |> ok)-2.7559289460184545)<1e-7)

[<Fact>]
let ``constant pairs have one canonical contact`` () =
    let at = p 1. 1.
    for a in constants at do
        for b in constants at do
            let expected = [hit 0. 0. at]
            Assert.True(Intersections.segment a b = Ok expected)
            Assert.Empty(Overlaps.segment a b |> ok)
            let found = Encounters.segment a b |> ok
            Assert.Empty(found.Overlaps)
            Assert.True(found.Intersections=expected)
        for b in constants (p 2. 1.) do
            Assert.Empty(Intersections.segment a b |> ok)
            Assert.Empty(Intersections.segment b a |> ok)

[<Fact>]
let ``constant arc queries are symmetric`` () =
    let arc = Arc {Start=p 2. 1.;Radius=p 1. 1.;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=p 0. 1.}
    for at,t in [p 2. 1.,0.;p 1. 2.,0.5] do
        for a in constants at do
            Assert.True(Intersections.segment a arc = Ok [hit 0. t at])
            Assert.True(Intersections.segment arc a = Ok [hit t 0. at])
            let found = Encounters.segment a arc |> ok
            Assert.Empty(found.Overlaps)
            Assert.True(found.Intersections=[hit 0. t at])
    for at in [p 1. 1.;p 3. 3.] do
        for a in constants at do
            Assert.Empty(Intersections.segment a arc |> ok)
            Assert.Empty(Intersections.segment arc a |> ok)

[<Fact>]
let ``constant line queries are symmetric`` () =
    let at = p 1. 1.
    let line = Line(p 0. 1.,p 2. 1.)
    for a in constants at do
        Assert.True(Intersections.segment a line = Ok [hit 0. 0.5 at])
        Assert.True(Intersections.segment line a = Ok [hit 0.5 0. at])

[<Fact>]
let ``constant contacts preserve other curve addresses`` () =
    let origin = p 0. 0.
    let loop = CubicBezier(origin,p 2. 2.,p -2. 2.,origin)
    let found = Intersections.segment (Line(origin,origin)) loop |> ok
    for t in [0.0<parameter>;1.0<parameter>] do
        Assert.True(found |> List.exists (fun h -> h.LeftT=0.0<parameter> && h.RightT=t))
    let at = p 1. 0.
    let folded = QuadraticBezier(origin,p 4. 0.,origin)
    let hits = Intersections.segment (Line(at,at)) folded |> ok
    Assert.Equal(2,hits.Length)
    Assert.True(hits[0].LeftT=0.0<parameter> && hits[1].LeftT=0.0<parameter>)
    Assert.True(hits[0].RightT<0.5<parameter> && hits[1].RightT>0.5<parameter>)
    let endpoint = p 1e-10 0.
    let hits = Intersections.segment (Line(endpoint,endpoint)) (Line(origin,endpoint)) |> ok
    Assert.True(hits |> List.exists (fun h -> h.RightT=1.0<parameter>))

[<Fact>]
let ``constant contacts validate options and arc geometry`` () =
    let origin = p 0. 0.
    let a = Line(origin,origin)
    let invalid = {Intersections.defaultOptions with Tolerance = -1.0<length>}
    Assert.True(Intersections.segmentWith a a invalid = Error(InvalidIntersectionTolerance -1.0<length>))
    Assert.True(Encounters.segmentWith a a invalid = Error(InvalidIntersectionTolerance -1.0<length>))
    Assert.True(Overlaps.segmentWith a a -1.0<length> = Error(InvalidOverlapTolerance -1.0<length>))
    let arc = Arc {Start=origin;Radius=p 0. 1.;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=p 1. 0.}
    Assert.True(Intersections.segment a arc = Error DegenerateArc)
    Assert.True(Overlaps.segment a arc = Error DegenerateArc)
    let options = {Intersections.defaultOptions with Tolerance=0.125<length>}
    let at = p 0.125 0.
    Assert.True(Intersections.segmentWith a (Line(at,at)) options = Ok [hit 0. 0. origin])
    let at = p 0.25 0.
    Assert.Empty(Intersections.segmentWith a (Line(at,at)) options |> ok)

let private conditionedPair () =
    CubicBezier(p 0.9646201687528162 1.4210854715202004e-14,p 0.6772878019443311 2.1316282072803006e-14,
                p 0.34565238552668376 1.4210854715202004e-14,p 0.43502028382843605 1.4210854715202004e-14),
    CubicBezier(p 0.4954801054242637 2.1316282072803006e-14,p 0.7755793433895946 1.4210854715202004e-14,
                p 0.013640682690727601 0.,p 0.8957075918855395 1.4210854715202004e-14)

[<Fact>]
let ``conditioned search recovers thin curve root`` () =
    let a,b = conditionedPair()
    let options = {Intersections.defaultOptions with Tolerance=1e-12<length>}
    for left,right,reversed in [a,b,false;b,a,true] do
        let found = Intersections.segmentWith left right options |> ok
        Assert.True(found |> List.exists (fun h ->
            let t,u = if reversed then h.RightT,h.LeftT else h.LeftT,h.RightT
            abs(float t-0.4962432835063433)<1e-7 && abs(float u-0.18393974411322186)<1e-7))

[<Fact>]
let ``conditioned search preserves original coordinate tolerance`` () =
    let a,b = conditionedPair()
    let options = {Intersections.defaultOptions with Tolerance=1e-12<length>}
    let found = Intersections.segmentWith a b options |> ok
    Assert.True(found |> List.exists (fun h -> h.RightT=0.0<parameter>))
    for h in found do
        for at in [Segment.point a h.LeftT |> ok;Segment.point b h.RightT |> ok] do
            Assert.True(abs(at.X-h.Point.X)<=options.Tolerance && abs(at.Y-h.Point.Y)<=options.Tolerance)

[<Fact>]
let ``conditioned search honors overlap and certification errors`` () =
    let a,b = conditionedPair()
    Assert.True(Intersections.segment a b = Error NonAffineOverlapCorrespondence)
    match Intersections.segmentWith a b {Intersections.defaultOptions with Tolerance=1e-16<length>} with
    | Error(InternalUncertifiedSegmentIntersection _) -> ()
    | result -> failwithf "Expected certification error, got %A" result

[<Fact>]
let ``broad closest pair search has a bounded stack`` () =
    let curve = CubicBezier(p 0. 0.,p 0. 1.,p 1. 1.,p 1. 0.)
    let line = Line(p 0. 1.,p 1. 1.)
    for a,b in [curve,line;line,curve] do
        let found = Distance.segmentSegmentClosestPair a b |> ok
        near 0.5 found.LeftAt; near 0.5 found.RightAt; near 0.25 found.Distance
