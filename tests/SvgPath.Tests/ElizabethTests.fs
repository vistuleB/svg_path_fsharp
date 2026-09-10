module SvgPath.Tests.ElizabethTests

open SvgPath
open Xunit

let private p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private horizontal = QuadraticBezier(p 0.0 0.0,p 0.5 0.0,p 1.0 0.0)
let private diagonal = QuadraticBezier(p 0.0 -0.5,p 0.5 0.0,p 1.0 0.5)
let private tangent = QuadraticBezier(p 0.0 0.25,p 0.5 -0.25,p 1.0 0.25)
let private unwrap result = Result.defaultWith (failwithf "%A") result
let private options = Intersections.defaultOptions
let private beam left right options = Intersections.elizabethBeamIntersections left right options
let private near value (hit:SegmentIntersection) tolerance = abs(hit.LeftT-value)<=tolerance && abs(hit.RightT-value)<=tolerance
let private residuals left right tolerance found =
    for (hit:SegmentIntersection) in found do
        let a,b = Segment.point left hit.LeftT |> unwrap,Segment.point right hit.RightT |> unwrap
        Assert.True(Point.squaredDistance a b <= tolerance*tolerance)
let private clustered offset =
    let d = -0.2 * 0.21 * 0.22
    let c = 0.2 * 0.21 + 0.2 * 0.22 + 0.21 * 0.22
    let b = -0.63
    CubicBezier(p offset (offset+d),p (offset+1.0/3.0) (offset+(d+c/3.0)),
                p (offset+2.0/3.0) (offset+(d+2.0*c/3.0+b/3.0)),p (offset+1.0) (offset+(d+c+b+1.0)))

[<Fact>]
let ``elizabeth_endpoint_keeps_multiple_target_parameters_test`` () =
    let retraced = QuadraticBezier(p 0.25 0.0,p -0.25 0.0,p 0.25 0.0)
    let endpoint = Line(p 0.0625 0.0,p 0.0625 1.0)
    let report = beam endpoint retraced options |> unwrap
    Assert.Equal(2,report.Intersections.Length)
    for t in [0.25<parameter>;0.75<parameter>] do
        Assert.True(report.Intersections |> List.exists (fun hit -> hit.LeftT=0.0<parameter> && abs(hit.RightT-t)<1e-7<parameter>))

[<Fact>]
let ``elizabeth_polygon_axes_separate_collinear_degeneracies_test`` () =
    let left = QuadraticBezier(p 0.0 0.0,p 0.5 0.5,p 1.0 1.0)
    let right = QuadraticBezier(p 2.0 2.0,p 2.5 2.5,p 3.0 3.0)
    let report = beam left right {Intersections.defaultOptions with MaxDepth=1} |> unwrap
    Assert.Empty(report.Intersections)

[<Fact>]
let ``elizabeth_beam_simple_crossing_needs_no_culling_test`` () =
    let report = beam horizontal diagonal options |> unwrap
    Assert.Equal(1,report.Intersections.Length)
    Assert.Equal(0,report.DiscardedCrossing)
    Assert.Equal(0,report.DiscardedOther)
    Assert.True(report.PeakRetained<=1000)

[<Fact>]
let ``elizabeth_beam_still_reports_depth_exhaustion_test`` () =
    match beam horizontal diagonal {options with MaxDepth=1} with
    | Error(CurveSolverDepthLimit _) -> ()
    | result -> failwithf "%A" result

[<Fact>]
let ``elizabeth_beam_flat_crossing_completes_with_explicit_loss_test`` () =
    let curve = CubicBezier(p 0.0 -0.125,p (1.0/3.0) 0.125,p (2.0/3.0) -0.125,p 1.0 0.125)
    let options = {options with Tolerance=5e-14<length>;MaxDepth=48}
    let report = beam curve horizontal options |> unwrap
    Assert.True(report.DiscardedOther>0)
    Assert.True(report.PeakRetained<=250)
    // Count snapshot, not a mathematical root count.
    Assert.Equal(6,report.Intersections.Length)
    Assert.True(report.DiscardedCandidates>0)
    IntersectionContractSupport.assertCandidates report.Intersections curve horizontal options.Tolerance
    Assert.True(report.Intersections |> List.exists (fun hit -> near 0.5<parameter> hit 1e-7<parameter>))
    residuals curve horizontal options.Tolerance report.Intersections

[<Fact>]
let ``elizabeth_beam_join_line_selection_keeps_endpoint_test`` () =
    let arc = Arc {Start=p 430.66681589309076 178.69245771161582;Radius=p 3.0 3.0
                   XAxisRotation=0.0<degree>;LargeArc=false;Sweep=false;End=p 430.670203101245 178.69477431938788}
    let line = Line(Segment.finish arc,p 430.22232031893986 178.38890397610967)
    let options = {options with Tolerance=5e-14<length>;MaxDepth=48}
    let report = beam arc line options |> unwrap
    match report.Intersections with
    | [interior;endpoint] ->
        Assert.True(report.DiscardedCandidates>0)
        Assert.True(endpoint.LeftT=1.0<parameter> && endpoint.RightT=0.0<parameter>)
        Assert.True(interior.LeftT<1.0<parameter> && interior.RightT>0.0<parameter>)
    | found -> failwithf "%A" found
    residuals arc line options.Tolerance report.Intersections

[<Fact>]
let ``elizabeth_transverse_crossing_test`` () =
    let found = Intersections.segmentWith horizontal diagonal options |> unwrap
    Assert.Single(found) |> ignore
    Assert.True(near 0.5<parameter> found.Head 1e-9<parameter>)

[<Fact>]
let ``elizabeth_candidate_does_not_finish_coarse_window_test`` () =
    match Intersections.segmentWith horizontal diagonal {options with MaxDepth=1} with
    | Error(IntersectionDepthLimitReached _) -> ()
    | result -> failwithf "%A" result


[<Fact>]
let ``elizabeth_clustered_crossings_test`` () =
    let found = Intersections.segmentWith (clustered 0.0) horizontal options |> unwrap
    Assert.Equal(3,found.Length)
    for t in [0.2<parameter>;0.21<parameter>;0.22<parameter>] do
        Assert.True(found |> List.exists (fun hit -> near t hit 1e-7<parameter>))

[<Fact>]
let ``elizabeth_endpoint_preference_test`` () =
    let rising = QuadraticBezier(p 0.0 0.0,p 0.5 0.5,p 1.0 1.0)
    let found = Intersections.segmentWith horizontal rising options |> unwrap
    Assert.Single(found) |> ignore
    Assert.True(found.Head.LeftT=0.0<parameter> && found.Head.RightT=0.0<parameter>)


[<Fact>]
let ``elizabeth_kissing_candidates_obey_resolution_contract_test`` () =
    let found = Intersections.segmentWith horizontal tangent options |> unwrap
    Assert.NotEmpty(found)
    Assert.True(found |> List.exists (fun hit -> hit.LeftT=0.5<parameter> && hit.RightT=0.5<parameter>))
    residuals horizontal tangent 1e-12<length> found
    for i,hit in List.indexed found do
        for other in List.skip (i+1) found do
            Assert.True(abs(hit.LeftT-other.LeftT)>1e-7<parameter> || abs(hit.RightT-other.RightT)>1e-7<parameter>)

[<Fact>]
let ``elizabeth_disjoint_windows_need_no_refinement_test`` () =
    let other = QuadraticBezier(p 0.0 1.0,p 0.5 1.0,p 1.0 1.0)
    Assert.True(Intersections.segmentWith horizontal other {options with MaxDepth=1} = Ok [])
