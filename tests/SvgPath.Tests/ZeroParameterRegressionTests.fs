module SvgPath.Tests.ZeroParameterRegressionTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private get result = result |> Result.defaultWith (failwithf "%A")
let private corner () = Subpath.create [Line(p 0. 0.,p 1. 0.);Line(p 1. 0.,p 1. 1.)] |> get
let private arc () = Arc ({ Start=p 0.1 0.2; Radius=p 3. 2.; XAxisRotation=17.0<degree>; LargeArc=false; Sweep=true; End=p 2. 3. }: Ellipse.EndpointArcData)
let private address index t = { SegmentIndex=index; T=t }
[<Fact>]
let ``endpoint arc splits return usable empty lines`` () =
    let arc = Arc ({Start=p 1. 0.;Radius=p 1. 1.;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=true;End=p 0. 1.}: Ellipse.EndpointArcData)
    for split in [Segment.split;Segment.splitInside] do
        for t in [0.0<parameter>; -0.0<parameter>;1.0<parameter>] do
            let left,right = split arc t |> get
            let empty,retained,endpoint = if t=1.0<parameter> then right,left,Segment.finish arc else left,right,Segment.start arc
            Assert.Equal(Line(endpoint,endpoint),empty)
            Assert.Equal(arc,retained)
            Assert.Equal(Ok 0.0<length>,Segment.length empty)
            Assert.Equal(Ok endpoint,Segment.point empty 0.5<parameter>)
            Assert.Equal(Segment.length arc,Segment.length retained)

[<Fact>]
let ``subpath canonicalization normalizes negative zero`` () =
    let result = Subpath.parameterCanonicalize (corner()) (address 1 -0.0<parameter>) |> get
    Assert.Equal(address 1 0.0<parameter>,result)
    Assert.Equal(0L,System.BitConverter.DoubleToInt64Bits(float result.T))
[<Fact>]
let ``negative zero corner preserves both directions`` () =
    let result = Subpath.directions (corner()) (address 1 -0.0<parameter>) |> get
    Assert.Equal(Some(Point.create 1.0 0.0),result.Incoming)
    Assert.Equal(Some(Point.create 0.0 1.0),result.Outgoing)
[<Fact>]
let ``negative zero interval end does not add a segment`` () =
    let expected = Subpath.create [Line(p 0. 0.,p 1. 0.)] |> get
    Assert.Equal(Ok expected,Subpath.between (corner()) (address 0 0.0<parameter>) (address 1 -0.0<parameter>))
[<Fact>]
let ``arc negative zero evaluates to exact start`` () =
    Assert.Equal(Ok(Segment.start (arc())),Segment.point (arc()) -0.0<parameter>)
[<Fact>]
let ``arc negative zero has no incoming direction`` () =
    let expected = Segment.directions (arc()) 0.0<parameter> |> get
    Assert.Equal(None,expected.Incoming)
    Assert.True(expected.Outgoing.IsSome)
    Assert.Equal(Ok expected,Segment.directions (arc()) -0.0<parameter>)
[<Fact>]
let ``mixed signed zero interval is a point like line`` () =
    let segment = arc()
    let start = Segment.start segment
    for a,b in [0.0<parameter>,-0.0<parameter>; -0.0<parameter>,0.0<parameter>; -0.0<parameter>,-0.0<parameter>] do
        Assert.Equal(Ok(Line(start,start)),Segment.between segment a b)
        Assert.Equal(Ok(Line(start,start)),Segment.betweenInside segment a b)
[<Fact>]
let ``arc split treats signed zeros identically`` () =
    let segment = arc()
    let expected = Segment.split segment 0.0<parameter>
    Assert.True(Result.isOk expected)
    Assert.Equal(expected,Segment.split segment -0.0<parameter>)
    Assert.Equal(expected,Segment.splitInside segment -0.0<parameter>)
