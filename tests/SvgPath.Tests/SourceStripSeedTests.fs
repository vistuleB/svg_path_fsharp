module SvgPath.Tests.SourceStripSeedTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private candidate segments = ConvexHull.internalSourceStripCandidate segments |> Result.defaultWith (failwithf "%A") |> Option.get
[<Fact>]
let ``source seed uses controls when endpoints coincide`` () =
    let strip = candidate [QuadraticBezier(p 0. 0., p -20. 0., p 0. 0.)]
    Assert.Equal(0.0<length>, strip.Width)
    Assert.Equal(0.0, strip.Normal.X)
[<Fact>]
let ``source seed preserves exact rotated collinearity`` () =
    let strip = candidate [CubicBezier(p 1. 1., p -10. -10., p 20. 20., p 2. 2.)]
    Assert.Equal(0.0<length>, strip.Width)
[<Fact>]
let ``source seed checks actual arc not endpoint chord`` () =
    let strip = candidate [Arc ({ Start = p 1. 0.; Radius = p 1. 1.; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = p -1. 0. }: Ellipse.EndpointArcData)]
    Assert.True(abs(strip.Width - 1.0<length>) < 1e-9<length>)
[<Fact>]
let ``source seed skips empty or coincident point cloud`` () =
    Assert.Equal(Ok None, ConvexHull.internalSourceStripCandidate [])
    Assert.Equal(Ok None, ConvexHull.internalSourceStripCandidate [Line(p 2. 3., p 2. 3.)])
[<Fact>]
let ``source seed allows zero tolerance bezier prefix`` () =
    let first = QuadraticBezier(p 0. 0., p -20. 0., p 0. 0.)
    let prefix = Degeneracy.internalLongestThinPrefix (Subpath.ofSegment first) 0.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>([first], prefix.Segments)
    Assert.Equal(0.0<length>, prefix.Strip.Value.Width)
