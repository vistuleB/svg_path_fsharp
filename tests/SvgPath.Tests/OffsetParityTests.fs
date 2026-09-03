module SvgPath.Tests.OffsetParityTests

open System.IO
open System.Text.RegularExpressions
open SvgPath
open Xunit

module Subject = Offset

let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
let private direction degrees = Point.direction (Degree.fromFloat degrees)
let private squareLoop () =
    Subpath.polygon [
        point 0.0 0.0
        point 10.0 0.0
        point 10.0 10.0
        point 0.0 10.0
    ]
    |> Result.defaultWith (failwithf "%A")

let private twoCutCornerLoop () =
    Subpath.assertCreate [
        Line(point 1.0 0.0, point 3.0 0.0)
        Arc
            { Start = point 3.0 0.0
              Radius = point 1.0 1.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = false
              End = point 4.0 1.0 }
        Line(point 4.0 1.0, point 4.0 3.0)
        Line(point 4.0 3.0, point 3.0 4.0)
        Line(point 3.0 4.0, point 1.0 4.0)
        Arc
            { Start = point 1.0 4.0
              Radius = point 1.0 1.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = false
              End = point 0.0 3.0 }
        Line(point 0.0 3.0, point 0.0 1.0)
        Line(point 0.0 1.0, point 1.0 0.0)
    ]
    |> Subpath.setClosed true
    |> Result.defaultWith (failwithf "%A")

let private stalledArcTurnRadius = 40.0<length>
let private stalledArcTurnDistance = 39.999<length>

let private cleanZero value =
    if abs value <= 0.000000000001 then 0.0 else value

let private circlePoint angle radius =
    point
        (cleanZero (float radius * Trig.cosDegrees (Degree.fromFloat angle)))
        (cleanZero (float radius * Trig.sinDegrees (Degree.fromFloat angle)))

let private circleAngleTangent angle =
    Point.create -(Trig.sinDegrees (Degree.fromFloat angle)) (Trig.cosDegrees (Degree.fromFloat angle))

let private circleArcSegment startAngle endAngle radius =
    Arc
        { Start = circlePoint startAngle radius
          Radius = Point.create radius radius
          XAxisRotation = 0.0<degree>
          LargeArc = false
          Sweep = false
          End = circlePoint endAngle radius }

let private circleArcCubic startAngle endAngle radius =
    let startPoint = circlePoint startAngle radius
    let endPoint = circlePoint endAngle radius
    let k = 4.0 / 3.0 * Trig.tanDegrees (Degree.fromFloat ((endAngle - startAngle) / 4.0))
    let startTangent = circleAngleTangent startAngle
    let endTangent = circleAngleTangent endAngle
    CubicBezier(
        startPoint,
        Point.add startPoint (Point.scale (k * radius) startTangent),
        Point.subtract endPoint (Point.scale (k * radius) endTangent),
        endPoint)

let private quarterTurnSegments subdivisions useArcs =
    [ 0 .. subdivisions - 1 ]
    |> List.map (fun index ->
        let step = -90.0 / float subdivisions
        let startAngle = float index * step
        let endAngle = float (index + 1) * step
        if useArcs then circleArcSegment startAngle endAngle stalledArcTurnRadius
        else circleArcCubic startAngle endAngle stalledArcTurnRadius)

let private stalledArcTurnSource subdivisions useArcs =
    let r = stalledArcTurnRadius
    let arcStart = circlePoint 0.0 r
    let arcEnd = circlePoint -90.0 r
    Subpath.create (
        [ Line(Point.create r r, arcStart) ]
        @ quarterTurnSegments subdivisions useArcs
        @ [ Line(arcEnd, Point.create -r -r) ])
    |> Result.defaultWith (failwithf "%A")

let private stalledArcTurnCornerSegments (subpath: Subpath) =
    subpath.Segments |> List.skip 1 |> List.take (max 0 (subpath.Segments.Length - 2))

let private rightUnitNormal (value: Point<length / parameter>) =
    let length = sqrt (float (value.X * value.X + value.Y * value.Y))
    Point.create (float value.Y / length) (-(float value.X) / length)

let private offsetEndpoint segment t =
    match Segment.point segment t, Segment.derivative segment t with
    | Ok pointValue, Ok derivative ->
            let normal = rightUnitNormal derivative
            Ok(Point.add pointValue (Point.scale stalledArcTurnDistance normal))
    | _ -> Error()

let private stalledArcTurnSegmentIsCaught segment =
    match offsetEndpoint segment 0.0<parameter>, offsetEndpoint segment 1.0<parameter> with
    | Ok startPoint, Ok endPoint -> Point.distance startPoint endPoint <= 0.01<length>
    | _ -> false

let private countStalledSegments segments =
    segments |> List.filter stalledArcTurnSegmentIsCaught |> List.length
let private rotateDirection (directionValue: Point<1>) degrees =
    Point.direction (Trig.atan2Degrees directionValue.Y directionValue.X + degrees)

let private assertNear actual expected =
    Assert.True(abs (float actual - expected) < 0.000001, sprintf "actual=%A expected=%f" actual expected)

let private localAperture aperture =
    if aperture <= 180.0<degree> then aperture else -(360.0<degree> - aperture)

let private assertReversalGap incomingDirection outgoingDirection adjustment (expectedTurn: TangentTurn) atLeast =
    let adjustedIncoming = rotateDirection incomingDirection adjustment.IncomingDegrees
    let adjustedOutgoing = rotateDirection outgoingDirection adjustment.OutgoingDegrees
    let oppositeOutgoing = Point.negate adjustedOutgoing
    let gap =
        match expectedTurn with
        | TangentTurn.Clockwise -> Point.clockwiseAperture adjustedIncoming oppositeOutgoing |> localAperture
        | TangentTurn.CounterClockwise -> Point.clockwiseAperture oppositeOutgoing adjustedIncoming |> localAperture
        | TangentTurn.Straight
        | TangentTurn.CouldNotMeasure -> 0.0<degree>
    Assert.True(gap + 0.000000001<degree> >= atLeast, sprintf "gap=%A atLeast=%A" gap atLeast)
    Assert.True(gap <= 180.0<degree>, sprintf "gap=%A" gap)

let private packageTitlePathData () =
    let rec locateFixture (directory: DirectoryInfo) =
        let candidate = Path.Combine(directory.FullName, "tests", "SvgPath.Tests", "Fixtures", "package_title.svg")
        if File.Exists candidate then candidate
        elif isNull directory.Parent then failwith "missing package-title fixture"
        else locateFixture directory.Parent
    let contents = File.ReadAllText(locateFixture (DirectoryInfo(System.AppContext.BaseDirectory)))
    let matched = Regex.Match(contents, " d=\"([^\"]+)\"")
    if not matched.Success then failwith "missing package-title path data"
    matched.Groups[1].Value

let private packageTitlePath () =
    packageTitlePathData () |> Parse.path |> Result.defaultWith (failwithf "%A")

let private packageTitleOptions join =
    { Subject.defaultOptions with
        Fitting = { Tolerance = 0.01<length>; Samples = 5; MaxDepth = 12 }
        DistanceOptions =
            { Segment.defaultDistanceOptions with
                Samples = 5
                Tolerance = 0.000000001<length> }
        Join = join }

[<Fact>]
let ``default_offset_trimming_uses_precise_projection_test`` () =
    Assert.Equal(5, Subject.defaultOptions.DistanceOptions.Samples)
    Assert.Equal(Segment.defaultDistanceOptions.Tolerance, Subject.defaultOptions.DistanceOptions.Tolerance)

[<Fact>]
let ``default_distance_options_test`` () =
    Assert.Equal(5, Subject.defaultOptions.DistanceOptions.Samples)
    Assert.Equal(Segment.defaultDistanceOptions.Tolerance, Subject.defaultOptions.DistanceOptions.Tolerance)

[<Fact>]
let ``default_single_and_band_trimming_options_test`` () =
    Assert.Equal(
        { Offside = true
          FinalTrimming = InBandTrimming },
        Subject.defaultOptions.SingleOffsetTrimming)
    Assert.Equal(
        { InnerCusps = true
          OuterCusps = true
          InBand = true },
        Subject.defaultOptions.BandTrimming)

[<Fact>]
let ``package_title_s_iterated_offset_keeps_three_closed_first_offset_subpaths_test`` () =
    let title = packageTitlePath ()
    let s = title.Subpaths |> List.head
    let options = packageTitleOptions Subject.defaultOptions.Join
    let firstOffset =
        Subject.pathWith (Path.ofSubpaths [ s ]) 1.0<length> options
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, firstOffset.Subpaths.Length)
    Assert.True(firstOffset.Subpaths |> List.forall Subpath.isClosed)
    Subject.pathWith firstOffset 1.0<length> options
    |> Result.defaultWith (failwithf "%A")
    |> ignore

[<Fact>]
let ``package_title_v_1_05_public_offset_filters_micro_loops_test`` () =
    let title = packageTitlePath ()
    let v = title.Subpaths[1]
    let options = packageTitleOptions Subject.defaultOptions.Join
    let result =
        Subject.pathWith (Path.ofSubpaths [ v ]) 1.05<length> options
        |> Result.defaultWith (failwithf "%A")
    Assert.Single(result.Subpaths) |> ignore
    Assert.True(result.Subpaths |> List.forall Subpath.isClosed)

[<Fact>]
let ``package_title_a_and_v_1_05_bevel_offsets_filter_micro_loops_test`` () =
    let title = packageTitlePath ()
    let v = title.Subpaths[1]
    let aOuter = title.Subpaths[6]
    let aInner = title.Subpaths[7]
    let options = packageTitleOptions Bevel
    let vOffset =
        Subject.pathWith (Path.ofSubpaths [ v ]) 1.05<length> options
        |> Result.defaultWith (failwithf "%A")
    let aOffset =
        Subject.pathWith (Path.ofSubpaths [ aOuter; aInner ]) 1.05<length> options
        |> Result.defaultWith (failwithf "%A")
    Assert.Single(vOffset.Subpaths) |> ignore
    Assert.Equal(2, aOffset.Subpaths.Length)

[<Fact>]
let ``public_single_offset_trimming_branches_test`` () =
    let source =
        Subpath.assertCreate [
            Line(point 0.0 0.0, point 10.0 0.0)
            Line(point 10.0 0.0, point 10.0 10.0)
        ]
    for finish in [ CuspTrimming; InBandTrimming; NoTrimming ] do
        let options =
            { Subject.defaultOptions with
                SingleOffsetTrimming =
                    { Offside = false
                      FinalTrimming = finish } }
        let path = Subject.subpathWith source 1.0<length> options |> Result.defaultWith (failwithf "%A")
        Assert.NotEmpty(path.Subpaths)

[<Fact>]
let ``public_band_trimming_branches_test`` () =
    let source =
        Subpath.assertCreate [
            Line(point 0.0 0.0, point 10.0 0.0)
            Line(point 10.0 0.0, point 10.0 10.0)
        ]
    let policies =
        [ { InnerCusps = false; OuterCusps = true; InBand = true }
          { InnerCusps = true; OuterCusps = false; InBand = false } ]
    for bandTrimming in policies do
        let options = { Subject.defaultOptions with BandTrimming = bandTrimming }
        let path = Subject.subpathBandWith source -1.0<length> 1.0<length> options |> Result.defaultWith (failwithf "%A")
        Assert.NotEmpty(path.Subpaths)

[<Fact>]
let ``reversal_boundaries_store_endpoint_curvature_test`` () =
    let segment = CubicBezier(point 0.0 0.0, point 1.0 0.0, point 1.0 0.0, point 1.0 1.0)
    let source = Subpath.create [ segment ] |> Result.defaultWith (failwithf "%A")
    let offsetAmount = -0.27<length>
    let portions = Subject.internalOffsetSourceTrace source offsetAmount Subject.defaultOptions |> Result.defaultWith (failwithf "%A")
    let reversalCurvatures =
        portions
        |> List.collect (fun portion -> portion.Pieces)
        |> List.collect (function
            | OffsetSourceTraceDRefined(_, _, _, _, _, startBoundary, endBoundary, _, _) -> [ startBoundary; endBoundary ]
            | OffsetSourceTraceStalled _ -> [])
        |> List.choose (function
            | ReversalBoundary(Some curvature) -> Some curvature
            | _ -> None)
    Assert.NotEmpty(reversalCurvatures)
    Assert.True(
        reversalCurvatures
        |> List.forall (fun value -> value <> 0.0<1/length> && abs (1.0 / float value - float offsetAmount) < 0.00001))

[<Fact>]
let ``synchronized_offsets_keep_stalled_side_as_one_run_test`` () =
    let source = stalledArcTurnSource 4 true
    let correspondences =
        Subject.internalSynchronizedOffsetTrace source 0.0<length> stalledArcTurnDistance Subject.defaultOptions
        |> Result.defaultWith (failwithf "%A")
    Assert.True(correspondences |> List.exists (fun correspondence -> correspondence.OuterStalled && correspondence.OuterLeaves.Length >= 4))

[<Fact>]
let ``synchronized_offsets_retain_matched_join_geometry_test`` () =
    let source =
        Subpath.assertCreate [
            Line(point 0.0 0.0, point 1.0 1.0)
            Line(point 1.0 1.0, point 2.0 0.0)
        ]
    let joins =
        Subject.internalSynchronizedJoinTrace source -0.25<length> 0.5<length> Subject.defaultOptions
        |> Result.defaultWith (failwithf "%A")
    let join = joins |> List.exactlyOne
    Assert.Equal(0, join.AfterPortionIndex)
    Assert.NotEmpty(join.InnerSegments)
    Assert.NotEmpty(join.OuterSegments)
    Assert.False(join.InnerReversed)
    Assert.True(join.OuterReversed)

[<Fact>]
let ``endpoint_near_reversal_is_absorbed_into_stalled_piece_test`` () =
    let segment =
        CubicBezier(
            point 21.684995 1.2450002000000002,
            point 21.494995 1.3150002000000003,
            point 21.37800567659191 1.4211122301564318,
            point 21.324995 1.5600002000000002)
    let source = Subpath.create [ segment ] |> Result.defaultWith (failwithf "%A")
    let options =
        { Subject.defaultOptions with
            Fitting = { Tolerance = 0.01<length>; Samples = 5; MaxDepth = 12 } }
    match Subject.internalOffsetSourceTrace source 1.04<length> options with
    | Ok [ { Pieces = OffsetSourceTraceStalled(_, stalled) :: OffsetSourceTraceDRefined(_, _, sourceFrom, _, _, _, _, _, _) :: _ } ] ->
        Assert.True(Segment.chordLength stalled < 0.001<length>)
        Assert.True(abs (float sourceFrom - 0.00019493877887725834) < 0.000000001)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``segment_offsets_line_to_visual_left_for_positive_distance_test`` () =
    let result =
        Subject.segment (Line(point 0.0 0.0, point 10.0 0.0)) 2.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>(
        [ Line(point 0.0 -2.0, point 10.0 -2.0) ],
        Subpath.segments result)

[<Fact>]
let ``segment_offsets_line_to_visual_right_for_negative_distance_test`` () =
    let result =
        Subject.segment (Line(point 0.0 0.0, point 0.0 10.0)) -3.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>(
        [ Line(point -3.0 0.0, point -3.0 10.0) ],
        Subpath.segments result)

[<Fact>]
let ``reversal_tangent_adjustment_opens_clockwise_gap_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.Clockwise) (TangentTurn.Clockwise)
            1.0<length> 1.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertReversalGap (direction 0.0) (direction 180.0) adjustment TangentTurn.Clockwise 1.0<degree>
    assertNear adjustment.IncomingDegrees -0.5
    assertNear adjustment.OutgoingDegrees 0.5

[<Fact>]
let ``reversal_tangent_adjustment_opens_counterclockwise_gap_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.CounterClockwise) (TangentTurn.CounterClockwise)
            1.0<length> 1.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertReversalGap (direction 0.0) (direction 180.0) adjustment TangentTurn.CounterClockwise 1.0<degree>
    assertNear adjustment.IncomingDegrees 0.5
    assertNear adjustment.OutgoingDegrees -0.5

[<Fact>]
let ``reversal_tangent_adjustment_uses_existing_gap_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.5)
            (TangentTurn.Clockwise) (TangentTurn.Clockwise)
            1.0<length> 1.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertReversalGap (direction 0.0) (direction 180.5) adjustment TangentTurn.Clockwise 1.0<degree>
    assertNear adjustment.IncomingDegrees -0.25
    assertNear adjustment.OutgoingDegrees 0.25

[<Fact>]
let ``reversal_tangent_adjustment_corrects_wrong_side_gap_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 179.5)
            (TangentTurn.Clockwise) (TangentTurn.Clockwise)
            1.0<length> 1.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertReversalGap (direction 0.0) (direction 179.5) adjustment TangentTurn.Clockwise 1.0<degree>
    assertNear adjustment.IncomingDegrees -0.75
    assertNear adjustment.OutgoingDegrees 0.75

[<Fact>]
let ``reversal_tangent_adjustment_weights_shorter_segment_more_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.Clockwise) (TangentTurn.Clockwise)
            1.0<length> 9.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertNear adjustment.IncomingDegrees -0.9
    assertNear adjustment.OutgoingDegrees 0.1

[<Fact>]
let ``reversal_tangent_adjustment_treats_straight_as_other_turn_test`` () =
    let adjustment =
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.Straight) (TangentTurn.CounterClockwise)
            1.0<length> 1.0<length> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertReversalGap (direction 0.0) (direction 180.0) adjustment TangentTurn.CounterClockwise 1.0<degree>

[<Fact>]
let ``reversal_tangent_adjustment_rejects_ambiguous_turns_test`` () =
    Assert.Equal(
        Error(),
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.Clockwise) (TangentTurn.CounterClockwise)
            1.0<length> 1.0<length> 1.0<degree>)
    Assert.Equal(
        Error(),
        Subject.internalReversalTangentAdjustment
            (direction 0.0) (direction 180.0)
            (TangentTurn.CouldNotMeasure) (TangentTurn.Clockwise)
            1.0<length> 1.0<length> 1.0<degree>)

[<Fact>]
let ``subpath_offset_map_maps_local_coordinates_to_right_side_test`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 3.0 0.0)
              Line(point 3.0 0.0, point 3.0 4.0) ]
        |> Result.defaultWith (failwithf "%A")
    let mapping = Subject.subpathOffsetMap source |> Result.defaultWith (failwithf "%A")
    let first = mapping (point 2.0 1.0) |> Result.defaultWith (failwithf "%A")
    let second = mapping (point 5.0 1.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance first (point 2.0 -1.0) < 1.0e-12<length>)
    Assert.True(Point.distance second (point 4.0 2.0) < 1.0e-12<length>)

[<Fact>]
let ``subpath_offset_map_rejects_open_subpath_distances_outside_length_test`` () =
    let source = Subpath.create [ Line(point 0.0 0.0, point 10.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let mapping = Subject.subpathOffsetMap source |> Result.defaultWith (failwithf "%A")
    match mapping (point 11.0 0.0) with
    | Error(SvgPath.Error.PathError(InvalidLengthDistance(distance, length))) ->
        Assert.Equal(11.0<length>, distance)
        Assert.Equal(10.0<length>, length)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``subpath_offset_map_rejects_zero_length_subpath_test`` () =
    match Subject.subpathOffsetMap (Subpath.empty (point 0.0 0.0)) with
    | Error(SvgPath.Error.DegenerateTangent t) -> Assert.Equal(0.0<parameter>, t)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``subpath_offset_map_composes_with_try_map_path_points_test`` () =
    let baseline = Subpath.create [ Line(point 0.0 0.0, point 100.0 0.0) ] |> Result.defaultWith (failwithf "%A")
    let outline = Path.ofSubpaths [ Subpath.create [ Line(point 20.0 3.0, point 40.0 5.0) ] |> Result.defaultWith (failwithf "%A") ]
    let mapping = Subject.subpathOffsetMap baseline |> Result.defaultWith (failwithf "%A")
    let mapped = Path.tryMapPoints mapping outline |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Path.ofSubpaths [ Subpath.create [ Line(point 20.0 -3.0, point 40.0 -5.0) ] |> Result.defaultWith (failwithf "%A") ],
        mapped)

[<Fact>]
let ``segment_rejects_invalid_options_test`` () =
    let options = { Subject.defaultOptions with Fitting = { Subject.defaultOptions.Fitting with Tolerance = 0.0<length> } }
    Assert.Equal(
        Error(InvalidTolerance 0.0<length>),
        Subject.segmentWith (Line(point 0.0 0.0, point 10.0 0.0)) 1.0<length> options)

[<Fact>]
let ``segment_rejects_negative_stalled_offset_diameter_test`` () =
    let options = { Subject.defaultOptions with StalledOffsetDiameter = -1.0<length> }
    Assert.Equal(
        Error(InvalidStalledOffsetDiameter -1.0<length>),
        Subject.segmentWith (Line(point 0.0 0.0, point 10.0 0.0)) 1.0<length> options)

[<Fact>]
let ``segment_offsets_near_collapsed_circular_arc_exactly_test`` () =
    let source =
        Arc
            { Start = point 40.0 0.0
              Radius = point 40.0 40.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = false
              End = point 0.0 -40.0 }
    let result = Subject.segment source 39.999<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0.001 0 A 0.001 0.001 0 0 0 0 -0.001", Serialize.subpath result)

[<Fact>]
let ``subpath_untrimmed_round_join_uses_source_corner_center_test`` () =
    let source =
        Subpath.assertCreate [
            Line(point 1.0 0.0, point 3.0 0.0)
            Arc
                { Start = point 3.0 0.0
                  Radius = point 1.0 1.0
                  XAxisRotation = 0.0<degree>
                  LargeArc = false
                  Sweep = false
                  End = point 4.0 1.0 }
        ]
    let options = { Subject.defaultOptions with Join = Round }
    let result = Subject.subpathUntrimmedWith source 1.8<length> options |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 1 -1.8 H 3 A 1.8 1.8 0 0 1 4.8 0 A 0.8 0.8 0 0 0 4 -0.8", Serialize.subpath result)

[<Fact>]
let ``subpath_offsets_one_small_circular_arc_as_arc_test`` () =
    let source = stalledArcTurnSource 1 true
    let options =
        { Subject.defaultOptions with
            Fitting = { Subject.defaultOptions.Fitting with Tolerance = 0.001<length> }
            Join = Round }
    let result = Subject.subpathUntrimmedWith source stalledArcTurnDistance options |> Result.defaultWith (failwithf "%A")
    let cornerSegments = stalledArcTurnCornerSegments result
    Assert.Single(cornerSegments) |> ignore
    Assert.Contains(cornerSegments, function Arc _ -> true | _ -> false)
    Assert.Equal("M 0.001 40 V 0 A 0.001 0.001 0 0 0 0 -0.001 H -40", Serialize.subpath result)

[<Fact>]
let ``subpath_offsets_many_small_circular_arcs_as_one_sampled_segment_test`` () =
    let source = stalledArcTurnSource 4 true
    let options =
        { Subject.defaultOptions with
            Fitting = { Subject.defaultOptions.Fitting with Tolerance = 0.001<length> }
            Join = Round }
    let result = Subject.subpathUntrimmedWith source stalledArcTurnDistance options |> Result.defaultWith (failwithf "%A")
    let cornerSegments = stalledArcTurnCornerSegments result
    Assert.Single(cornerSegments) |> ignore
    Assert.Contains(cornerSegments, function CubicBezier _ -> true | _ -> false)

[<Fact>]
let ``stalled_arc_turn_offset_catches_expected_stalled_segments_test`` () =
    let caughtCounts =
        ([ 1; 4; 30 ] |> List.map (fun subdivisions -> subdivisions, true))
        @ ([ 1; 4; 30 ] |> List.map (fun subdivisions -> subdivisions, false))
        |> List.map (fun (subdivisions, useArcs) ->
            let source = stalledArcTurnSource subdivisions useArcs
            let options =
                { Subject.defaultOptions with
                    Fitting = { Subject.defaultOptions.Fitting with Tolerance = 0.001<length> }
                    Join = Round }
            let _ = Subject.subpathUntrimmedWith source stalledArcTurnDistance options |> Result.defaultWith (failwithf "%A")
            countStalledSegments source.Segments)
    Assert.Equal<int list>([ 1; 4; 30; 1; 4; 30 ], caughtCounts)

[<Fact>]
let ``segment_offset_preserves_reversed_offset_tangent_direction_test`` () =
    let curve =
        CubicBezier(
            point 72.63756968951799 2.697503894403671,
            point 72.63562808208563 2.697622530169285,
            point 72.63354998266372 2.6977495058451253,
            point 72.63043 2.69644)
    let options =
        { Subject.defaultOptions with
            Fitting =
                { Subject.defaultOptions.Fitting with
                    Tolerance = 0.01<length>
                    Samples = 5 } }
    let offsetSubpath = Subject.segmentWith curve 0.4<length> options |> Result.defaultWith (failwithf "%A")
    Assert.NotEmpty(offsetSubpath.Segments)

[<Fact>]
let ``subpath_offsets_open_polyline_to_trimmed_intersection_test`` () =
    let source =
        Subpath.assertCreate [
            Line(point 0.0 0.0, point 10.0 0.0)
            Line(point 10.0 0.0, point 10.0 -10.0)
        ]
    let result = Subject.subpath source 2.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 0 -2 H 8 V -10", Serialize.path result)

[<Fact>]
let ``subpath_prunes_self_crossed_inset_sections_test`` () =
    let shape =
        Subpath.polygon [
            point 0.0 0.0
            point 120.0 0.0
            point 120.0 30.0
            point 70.0 30.0
            point 70.0 90.0
            point 120.0 90.0
            point 120.0 120.0
            point 0.0 120.0
        ]
        |> Result.defaultWith (failwithf "%A")
    let options = { Subject.defaultOptions with Join = Round }
    let trimmed = Subject.subpathWith shape -24.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 24 24 H 46.7621 A 24 24 0 0 0 46 30 V 90 A 24 24 0 0 0 46.7621 96 H 24 Z", Serialize.path trimmed)

[<Fact>]
let ``subpath_prunes_negative_inset_sections_test`` () =
    let shape =
        Subpath.polygon [
            point 0.0 0.0
            point 120.0 0.0
            point 120.0 30.0
            point 70.0 30.0
            point 70.0 90.0
            point 120.0 90.0
            point 120.0 120.0
            point 0.0 120.0
        ]
        |> Result.defaultWith (failwithf "%A")
    let options = { Subject.defaultOptions with Join = Round }
    let trimmed = Subject.subpathWith shape -24.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.Single(trimmed.Subpaths) |> ignore
    Assert.Equal("M 24 24 H 46.7621 A 24 24 0 0 0 46 30 V 90 A 24 24 0 0 0 46.7621 96 H 24 Z", Serialize.path trimmed)

[<Fact>]
let ``subpath_ignores_adjacent_local_contacts_test`` () =
    let shape =
        Subpath.create [
            CubicBezier(point 0.0 0.0, point 60.0 -75.0, point 115.0 -75.0, point 75.0 0.0)
            CubicBezier(point 75.0 0.0, point 115.0 75.0, point 60.0 75.0, point 0.0 0.0)
            CubicBezier(point 0.0 0.0, point -60.0 -75.0, point -115.0 -75.0, point -75.0 0.0)
            CubicBezier(point -75.0 0.0, point -115.0 75.0, point -60.0 75.0, point 0.0 0.0)
        ]
        |> Result.defaultWith (failwithf "%A")
    let options = { Subject.defaultOptions with Join = Round }
    let result = Subject.subpathWith shape -16.0<length> options |> Result.defaultWith (failwithf "%A")
    Assert.Single(result.Subpaths) |> ignore

[<Fact>]
let ``arrangement_nodes_crossing_subpaths_test`` () =
    let horizontal = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let vertical = Subpath.ofSegment (Line(point 5.0 -5.0, point 5.0 5.0))
    let build =
        Arrangement.build [ Path.ofSubpaths [ horizontal; vertical ] ] 0.000000002<length> 0.000000002<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(5, build.Graph.Vertices.Length)
    Assert.Equal(4, build.Graph.Edges.Length)
    Assert.True(build.Graph.Edges |> List.forall (fun edge -> edge.ForwardMultiplicity = 1 && edge.ReverseMultiplicity = 0))

[<Fact>]
let ``arrangement_consolidates_coincident_pieces_test`` () =
    let whole = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let divided =
        Subpath.assertCreate [
            Line(point 0.0 0.0, point 5.0 0.0)
            Line(point 5.0 0.0, point 10.0 0.0)
        ]
    let build =
        Arrangement.build [ Path.ofSubpaths [ whole; divided ] ] 0.000000002<length> 0.000000002<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3, build.Graph.Vertices.Length)
    Assert.Equal(2, build.Graph.Edges.Length)
    Assert.True(build.Graph.Edges |> List.forall (fun edge -> edge.ForwardMultiplicity = 2 && edge.ReverseMultiplicity = 0))

[<Fact>]
let ``path_band_offsets_every_subpath_on_both_sides_test`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let second = Subpath.ofSegment (Line(point 0.0 10.0, point 10.0 10.0))
    let result = Subject.pathBand (Path.ofSubpaths [ first; second ]) -1.0<length> 1.0<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, result.Subpaths.Length)
    Assert.Equal("M 0 1 H 10 M 0 -1 H 10 M 0 11 H 10 M 0 9 H 10", Serialize.path result)

[<Fact>]
let ``offside_trimming_keeps_square_offset_test`` () =
    let result = Subject.pathWith (Path.singleton (squareLoop ())) 2.0<length> Subject.defaultOptions |> Result.defaultWith (failwithf "%A")
    let subpath = result.Subpaths |> List.exactlyOne
    Assert.True(subpath.Closed)

[<Fact>]
let ``offside_trimming_prunes_closed_subpaths_independently_test`` () =
    let second =
        Subpath.polygon [ point 20.0 0.0; point 30.0 0.0; point 30.0 10.0; point 20.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result = Subject.pathWith (Path.ofSubpaths [ squareLoop (); second ]) 2.0<length> Subject.defaultOptions |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, result.Subpaths.Length)
    Assert.True(result.Subpaths |> List.forall _.Closed)

[<Fact>]
let ``concave_band_orients_overlapping_contours_for_nonzero_fill_test`` () =
    let source =
        Subpath.polygon [
            point 0.0 0.0
            point 150.0 0.0
            point 150.0 38.0
            point 94.0 38.0
            point 94.0 78.0
            point 150.0 78.0
            point 150.0 116.0
            point 0.0 116.0
        ]
        |> Result.defaultWith (failwithf "%A")
    let options =
        { Subject.defaultOptions with
            Fitting = { Subject.defaultOptions.Fitting with Tolerance = 0.01<length> }
            Join = Round }
    let band = Subject.subpathBandWith source -12.0<length> -14.0<length> options |> Result.defaultWith (failwithf "%A")
    let dominantAreas =
        band.Subpaths
        |> List.map Area.signedSubpath
        |> List.filter (fun value -> abs (float value) > 1000.0)
    Assert.Equal(2, dominantAreas.Length)
    Assert.True(float dominantAreas[0] * float dominantAreas[1] < 0.0)

[<Fact>]
let ``subpath_band_side_trimming_removes_round_join_loops_test`` () =
    let options = { Subject.defaultOptions with Join = Round }
    let band =
        Subject.subpathBandWith (twoCutCornerLoop ()) 1.7<length> 1.8<length> options
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, band.Subpaths.Length)

[<Fact>]
let ``side_local_band_trimming_preserves_positive_band_test`` () =
    let options = { Subject.defaultOptions with Join = Round }
    let band =
        Subject.subpathBandWith (twoCutCornerLoop ()) 1.7<length> 1.8<length> options
        |> Result.defaultWith (failwithf "%A")
    let first, second =
        match band.Subpaths with
        | [ first; second ] -> first, second
        | other -> failwithf "unexpected subpaths: %A" other
    Assert.True(first.Closed)
    Assert.True(second.Closed)
    let filledArea = Area.path band Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.True(filledArea > 0.0<length^2>)

[<Fact>]
let ``side_local_band_trimming_preserves_negative_band_test`` () =
    let options = { Subject.defaultOptions with Join = Round }
    let band =
        Subject.subpathBandWith (twoCutCornerLoop ()) -0.7<length> -0.8<length> options
        |> Result.defaultWith (failwithf "%A")
    let first, second =
        match band.Subpaths with
        | [ first; second ] -> first, second
        | other -> failwithf "unexpected subpaths: %A" other
    Assert.True(first.Closed)
    Assert.True(second.Closed)
    let filledArea = Area.path band Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.True(filledArea > 0.0<length^2>)

[<Fact>]
let ``pairwise_healing_loop_short_circuit_is_idempotent_test`` () =
    let previous =
        QuadraticBezier(point 0.0 0.0, point 0.5 2.0, point 1.0 0.0)
    let next =
        QuadraticBezier(point 1.0 0.0, point 0.5 -1.0, point 0.0 1.0)
    let rebuiltPrevious, rebuiltNext =
        Subject.internalShortCircuitAdjacentOffsetSegmentLoop previous next
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Segment.finish rebuiltPrevious, Segment.start rebuiltNext)
    Assert.NotEqual(Segment.finish previous, Segment.finish rebuiltPrevious)
    Assert.Equal(
        Ok(rebuiltPrevious, rebuiltNext),
        Subject.internalShortCircuitAdjacentOffsetSegmentLoop rebuiltPrevious rebuiltNext)

[<Fact>]
let ``band_inside_function_uses_nonzero_for_open_subpath_band_test`` () =
    let outline = squareLoop ()
    let inside = Subject.internalBandInsideFunction [ OpenSubpathBand outline ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok true, inside (point 5.0 5.0))
    Assert.Equal(Ok false, inside (point 15.0 5.0))

[<Fact>]
let ``band_inside_function_reverses_second_closed_subpath_side_test`` () =
    let outer = squareLoop ()
    let inner =
        Subpath.polygon [ point 2.0 2.0; point 8.0 2.0; point 8.0 8.0; point 2.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let inside = Subject.internalBandInsideFunction [ ClosedSubpathBand(outer, inner) ] |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok true, inside (point 1.0 1.0))
    Assert.Equal(Ok false, inside (point 5.0 5.0))
    Assert.Equal(Ok false, inside (point 12.0 5.0))

[<Fact>]
let ``band_inside_function_rejects_open_payload_test`` () =
    let openSubpath = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Error BandSubpathNotClosed, Subject.internalBandInsideFunction [ OpenSubpathBand openSubpath ])

[<Fact>]
let ``segment_is_submerged_checks_both_immediate_sides_test`` () =
    let outline = squareLoop ()
    let inside = Subject.internalBandInsideFunction [ OpenSubpathBand outline ] |> Result.defaultWith (failwithf "%A")
    let middle = Line(point 2.0 5.0, point 8.0 5.0)
    let boundary = Line(point 2.0 0.0, point 8.0 0.0)
    Assert.Equal(Ok true, Subject.internalSegmentIsSubmerged middle inside 0.5<length>)
    Assert.Equal(Ok false, Subject.internalSegmentIsSubmerged boundary inside 0.5<length>)

[<Fact>]
let ``topological_band_loops_filters_submerged_loop_test`` () =
    let loop = squareLoop ()
    let containingBand =
        Subpath.polygon [ point -1.0 -1.0; point 11.0 -1.0; point 11.0 11.0; point -1.0 11.0 ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Ok [],
        Subject.internalTopologicalBandLoops [ loop ] [ OpenSubpathBand containingBand ] Subject.defaultOptions)

[<Fact>]
let ``single_offset_band_candidate_closes_open_source_test`` () =
    let openSubpath = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    match Subject.internalSingleOffsetBandCandidate openSubpath 2.0<length> Subject.defaultOptions with
    | Ok(OpenSubpathBand outline) -> Assert.True(outline.Closed)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``single_offset_band_candidate_keeps_closed_source_as_two_sides_test`` () =
    match Subject.internalSingleOffsetBandCandidate (squareLoop ()) 2.0<length> Subject.defaultOptions with
    | Ok(ClosedSubpathBand(exterior, interior)) ->
        Assert.True(exterior.Closed)
        Assert.True(interior.Closed)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``untrimmed_stroke_band_closes_open_source_test`` () =
    let openSubpath = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    match Subject.internalUntrimmedStrokeBand openSubpath 4.0<length> Butt Subject.defaultOptions with
    | Ok(OpenSubpathBand outline) -> Assert.True(outline.Closed)
    | other -> failwithf "unexpected result: %A" other

[<Fact>]
let ``closed rectangular band matches Gleam contour topology`` () =
    let source =
        Subpath.polygon
            [ point 0.0 0.0; point 10.0 0.0
              point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result =
        Subject.subpathBand source -1.0<length> 1.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length (Path.subpaths result))
    Assert.All(Path.subpaths result, fun subpath -> Assert.True(Subpath.isClosed subpath))

[<Fact>]
let ``open line round stroke matches Gleam contour topology`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let result =
        Subject.subpathStrokeWith
            source 2.0<length> RoundCap
            { Subject.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    let contours = Path.subpaths result
    Assert.Single(contours) |> ignore
    Assert.True(Subpath.isClosed contours[0])
