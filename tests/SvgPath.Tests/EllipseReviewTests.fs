module SvgPath.Tests.EllipseReviewTests
open SvgPath
open Xunit

[<Fact>]
let ``transformed axes preserves small nonsingular eigenvalue`` () =
    let radius,_ = Ellipse.transformedAxes (Point.create 3.0<length> 2.0<length>) 2.0<degree> (Affine.scaleXY 1.0 1e-8) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs(radius.X * radius.Y / 6e-8<length^2> - 1.0) < 1e-9)
    Assert.True(min radius.X radius.Y>1e-8<length>)
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
[<Fact>]
let ``collapsed quarter preserves direction and endpoints`` () =
    let arc = Arc ({ Start=p 1. 0.; Radius=p 1. 1.; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=p 0. 1. }: Ellipse.EndpointArcData)
    Assert.Equal(Ok(Line(p 1. 0.,p 0. 0.)),Transform.segmentGracefully arc (Transform.scaleXY 1.0 0.0))
    Assert.Equal(Ok(Line(p -1. 0.,p 0. 0.)),Transform.segmentGracefully arc (Transform.scaleXY -1.0 0.0))
[<Fact>]
let ``small rotated ellipse identity preserves axes`` () =
    let radius,angle = Ellipse.transformedAxes (p 0.00001 0.00002) 17.0<degree> (Affine.identity()) |> Result.defaultWith (failwithf "%A")
    Assert.True(abs(radius.X-0.00001<length>)<1e-15<length>)
    Assert.True(abs(radius.Y-0.00002<length>)<1e-15<length>)
    Assert.True(abs(angle-17.0<degree>)<1e-9<degree>)
[<Fact>]
let ``coincident signed zero endpoints are degenerate`` () =
    Assert.Equal(Error Ellipse.DegenerateInputArc,Ellipse.endpointToCenter ({ Start=p 0. 0.; Radius=p 10. 10.; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=p -0. 0. }: Ellipse.EndpointArcData))
let private quarter = ({ Center=p 0. 0.; Radius=p 1. 1.; XAxisRotation=0.0<degree>; StartAngle=0.0<degree>; DeltaAngle=90.0<degree> }: Ellipse.CenterArcData)
[<Fact>]
let ``multi turn projection extrema include each visit`` () =
    for t in [8.0<parameter>;-8.0<parameter>] do
        let arc,_ = Ellipse.splitArc quarter t
        Assert.Equal<float<parameter> list>([0.0<parameter>;0.25<parameter>;0.5<parameter>;0.75<parameter>;1.0<parameter>],Ellipse.arcProjectionExtrema arc (Point.create 1.0 0.0))
[<Fact>]
let ``zero sweep has no isolated projection extrema`` () =
    Assert.Empty(Ellipse.arcProjectionExtrema { quarter with DeltaAngle=0.0<degree> } (Point.create 1.0 0.0))
