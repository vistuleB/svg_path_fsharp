module SvgPath.Tests.LengthUpperBoundTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private arc start radius finish = Arc ({ Start=start; Radius=radius; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=finish }: Ellipse.EndpointArcData)
[<Fact>]
let ``line length upper bound is chord`` () = Assert.Equal(Ok 5.0<length>,Segment.lengthUpperBound (Line(p 0. 0.,p 3. 4.)))
[<Fact>]
let ``bezier length upper bounds include backtracking`` () =
    let a,b = p 0. 0.,p 3. 4.
    Assert.Equal(Ok 10.0<length>,Segment.lengthUpperBound (QuadraticBezier(a,b,a)))
    Assert.Equal(Ok 15.0<length>,Segment.lengthUpperBound (CubicBezier(a,b,a,b)))
    Assert.Equal(Ok 0.0<length>,Segment.lengthUpperBound (CubicBezier(a,a,a,a)))
[<Fact>]
let ``circular arc length bound is exact up to roundoff`` () =
    let bound = Segment.lengthUpperBound (arc (p 2. 0.) (p 2. 2.) (p 0. 2.)) |> get
    Assert.True(abs(bound - 3.141592653589793<length>) < 1e-12<length>)
[<Fact>]
let ``arc length bound uses corrected radii`` () =
    let bound = Segment.lengthUpperBound (arc (p -2. 0.) (p 1. 1.) (p 2. 0.)) |> get
    Assert.True(abs(bound - 6.283185307179586<length>) < 1e-12<length>)
[<Fact>]
let ``ellipse length bound exceeds integrated length`` () =
    let segment = arc (p 3. 0.) (p 3. 1.) (p 0. 1.)
    let bound,length = Segment.lengthUpperBound segment |> get,Segment.length segment |> get
    Assert.True(bound >= length)
    Assert.True(abs(bound - 4.71238898038469<length>) < 1e-12<length>)
[<Fact>]
let ``length bounds sum subpaths without counting gaps`` () =
    let empty = Subpath.empty (p 100. 100.)
    let a,b = p 0. 0.,p 3. 4.
    let subpath = Subpath.create [Line(a,b);Line(b,a)] |> get
    let closed = Subpath.setClosed true subpath |> get
    Assert.Equal(Ok 0.0<length>,Subpath.lengthUpperBound empty)
    Assert.Equal(Ok 10.0<length>,Subpath.lengthUpperBound closed)
    Assert.Equal(Ok 0.0<length>,Path.lengthUpperBound (Path.ofSubpaths []))
    Assert.Equal(Ok 20.0<length>,Path.lengthUpperBound (Path.ofSubpaths [closed;empty;subpath]))
[<Fact>]
let ``length bound propagates invalid arc`` () =
    let segment = arc (p 0. 0.) (p 1. 1.) (p 0. 0.)
    let subpath = Subpath.create [segment] |> get
    Assert.Equal(Error DegenerateArc,Segment.lengthUpperBound segment)
    Assert.Equal(Error DegenerateArc,Subpath.lengthUpperBound subpath)
    Assert.Equal(Error DegenerateArc,Path.lengthUpperBound (Path.ofSubpaths [subpath]))
