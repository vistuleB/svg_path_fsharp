module SvgPath.Tests.ExplicitOverlapEndpointTests
open SvgPath
open Xunit
let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private line = Line(p 0. 0.,p 10. 0.)
[<Fact>]
let ``explicit correspondence rejects mismatching endpoint pairs`` () =
    for right in [Line(p 0. 1.1,p 10. 0.);Line(p 0. 0.,p 10. 1.1)] do
        Assert.Equal(Ok None,Overlaps.checkParameterCorrespondence line right 0.0<parameter> 1.0<parameter> 0.0<parameter> 1.0<parameter> 1.0<length> 5)
        Assert.Equal(Ok None,Overlaps.checkParameterCorrespondence line (Segment.reverse right) 0.0<parameter> 1.0<parameter> 1.0<parameter> 0.0<parameter> 1.0<length> 5)
[<Fact>]
let ``explicit correspondence accepts endpoints at tolerance`` () =
    let result = Overlaps.checkParameterCorrespondence line (Line(p 0. 1.,p 10. 1.)) 0.0<parameter> 1.0<parameter> 0.0<parameter> 1.0<parameter> 1.0<length> 5
    match result with
    | Ok(Some _) -> ()
    | other -> failwithf "Expected correspondence, got %A" other
