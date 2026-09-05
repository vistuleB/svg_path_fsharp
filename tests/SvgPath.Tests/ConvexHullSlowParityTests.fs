[<Xunit.Trait("Category", "Slow")>]
module SvgPath.Tests.ConvexHullSlowParityTests

open SvgPath
open Xunit
open SvgPath.Tests.ConvexHullSlowSupport

let private tolerance = 1.0e-6<length>
let private supportUnitDiameterTolerance = 2.0e-8
let private smartSupportBaseTolerance = 1.0e-9<length>
let private smartSupportUnitDiameterTolerance = 1.0e-9
let private repairModesToCheck = [ "dumb"; "ambitious" ]
let private scaleCovarianceRelativeTolerance = 1.0e-7

let private directionOf angle = angleDirection angle
let private pointSupport point angle = Point.dot point (directionOf angle)

let private segmentSupportPoint segment angle =
    let direction = directionOf angle
    Segment.minimize segment (fun candidate -> -(float (Point.dot candidate direction)))
    |> Result.bind (Segment.point segment)

let private originalSupportValue segment angle =
    segmentSupportPoint segment angle
    |> Result.map (fun point -> pointSupport point angle)

let private segmentsSupportPoint segments angle =
    match segments with
    | [] -> Error EmptySubpath
    | first :: rest ->
        segmentSupportPoint first angle
        |> Result.bind (fun best ->
            let rec loop remaining current =
                match remaining with
                | [] -> Ok current
                | segment :: tail ->
                    segmentSupportPoint segment angle
                    |> Result.bind (fun candidate ->
                        let next =
                            if pointSupport candidate angle > pointSupport current angle then candidate
                            else current
                        loop tail next)
            loop rest best)

let private hullSupportValue segments angle =
    segmentsSupportPoint segments angle
    |> Result.map (fun point -> pointSupport point angle)

let private near a b = abs (a - b) < tolerance

let private nearValue result expected =
    match result with
    | Ok value -> near value expected
    | Error _ -> false

let private segmentBoxDiameter segment =
    match Segment.boundingBox segment with
    | Ok box -> Some(BoundingBox.diameter box)
    | Error _ -> None

let private supportTolerance segment =
    match segmentBoxDiameter segment with
    | Some diameter -> max tolerance (diameter * supportUnitDiameterTolerance)
    | None -> tolerance

let private smartSupportTolerance segment =
    match segmentBoxDiameter segment with
    | Some diameter -> max smartSupportBaseTolerance (diameter * smartSupportUnitDiameterTolerance)
    | None -> smartSupportBaseTolerance

let private subpathSupportTolerance segments =
    segments
    |> List.fold
        (fun best segment ->
            match segmentBoxDiameter segment with
            | Some diameter -> max best (diameter * supportUnitDiameterTolerance)
            | None -> best)
        tolerance

let private multiplesOfTenDegrees () = [ 0.0 .. 10.0 .. 350.0 ]

let private supportMismatchReport segment (hull: Subpath) =
    let rec loop = function
        | [] -> Error()
        | angle :: rest ->
            match originalSupportValue segment angle, hullSupportValue hull.Segments angle with
            | Ok original, Ok hullValue ->
                let difference = abs (original - hullValue)
                let allowed = supportTolerance segment
                if difference < allowed then loop rest
                else
                    Ok(
                        sprintf
                            "angle=%A original=%A hull=%A diff=%A tolerance=%A"
                            angle original hullValue difference allowed)
            | Error error, _ -> Ok(sprintf "original support errored %A" error)
            | _, Error error -> Ok(sprintf "hull support errored %A" error)
    loop (multiplesOfTenDegrees ())

let private subpathSupportMatches originalSegments (hull: Subpath) =
    let rec loop = function
        | [] -> Ok()
        | angle :: rest ->
            match hullSupportValue originalSegments angle, hullSupportValue hull.Segments angle with
            | Ok originalValue, Ok hullValue ->
                let difference = abs (originalValue - hullValue)
                let allowed = subpathSupportTolerance originalSegments
                if difference < allowed then loop rest
                else
                    Error(
                        sprintf
                            "angle=%A original=%A hull=%A diff=%A tolerance=%A"
                            angle originalValue hullValue difference allowed)
            | Error error, _ -> Error(sprintf "original support errored %A" error)
            | _, Error error -> Error(sprintf "hull support errored %A" error)
    loop (multiplesOfTenDegrees ())

let private subpathSupportMatchesBool originalSegments (hull: Subpath) =
    multiplesOfTenDegrees ()
    |> List.forall (fun angle ->
        match hullSupportValue originalSegments angle, hullSupportValue hull.Segments angle with
        | Ok originalValue, Ok hullValue ->
            abs (originalValue - hullValue) < subpathSupportTolerance originalSegments
        | _ -> false)

let private normalizeDegrees (degrees: float<degree>) =
    if degrees < 0.0<degree> then degrees + 360.0<degree>
    elif degrees >= 360.0<degree> then degrees - 360.0<degree>
    else degrees

let private segmentDerivativeAngle segment t =
    Segment.derivative segment t
    |> Result.defaultWith (failwithf "%A")
    |> fun derivative -> Trig.atan2Degrees derivative.Y derivative.X
    |> normalizeDegrees

let private segmentDerivativeAngles segments =
    segments
    |> List.collect (fun segment ->
        [ segmentDerivativeAngle segment 0.1<parameter>
          segmentDerivativeAngle segment 0.9<parameter> ])

let private rotateToSmallestPositiveAngle (angles: float<degree> list) =
    let rec smallestPositiveIndex position bestIndex bestAngle remaining =
        match remaining with
        | [] -> bestIndex
        | angle :: rest when angle > 0.0<degree> && (bestIndex < 0 || angle < bestAngle) ->
            smallestPositiveIndex (position + 1) position angle rest
        | _ :: rest ->
            smallestPositiveIndex (position + 1) bestIndex bestAngle rest
    let index = smallestPositiveIndex 0 -1 0.0<degree> angles
    if index < 0 then angles
    else List.skip index angles @ List.take index angles

let private unwrapAngles (angles: float<degree> list) =
    match angles with
    | [] -> []
    | first :: rest ->
        let rec loop previous offset unwrapped remaining =
            match remaining with
            | [] -> List.rev unwrapped
            | angle :: tail ->
                let offset = if angle + offset < previous then offset + 360.0<degree> else offset
                let angle = angle + offset
                loop angle offset (angle :: unwrapped) tail
        loop first 0.0<degree> [ first ] rest

let rec private nondecreasing toleranceValue values =
    match values with
    | [] | [ _ ] -> true
    | first :: second :: rest ->
        first <= second + toleranceValue && nondecreasing toleranceValue (second :: rest)

let private hullDerivativeAnglesAreNondecreasing (hull: Subpath) =
    hull.Segments
    |> segmentDerivativeAngles
    |> rotateToSmallestPositiveAngle
    |> unwrapAngles
    |> nondecreasing 0.0<degree>

let private hullFailureReason segment =
    match ConvexHull.segmentHull segment with
    | Error error -> Error(sprintf "segment_hull returned %A" error)
    | Ok subpath ->
        if not subpath.Closed then Error "hull subpath is not closed"
        elif subpath.Segments.Length < 2 then Error "hull has fewer than two segments"
        elif not (hullDerivativeAnglesAreNondecreasing subpath) then
            Error "derivative angles are not nondecreasing"
        else
            match supportMismatchReport segment subpath with
            | Ok report -> Error(sprintf "support values do not match: %s" report)
            | Error() -> Ok()

let private failingSpecimenReports specimens =
    specimens
    |> List.choose (fun (name, segment) ->
        match hullFailureReason segment with
        | Ok() -> None
        | Error reason -> Some(sprintf "%s: %s" name reason))

let private subpathHullFailureReason segments =
    match Subpath.create segments with
    | Error error -> Error(sprintf "subpath constructor returned %A" error)
    | Ok subpath ->
        match ConvexHull.subpathHull subpath with
        | Error error -> Error(sprintf "subpath_hull returned %A" error)
        | Ok hull ->
            if not hull.Closed then Error "hull subpath is not closed"
            elif hull.Segments.Length < 2 then Error "hull has fewer than two segments"
            else
                match subpathSupportMatches segments hull with
                | Ok() -> Ok()
                | Error report -> Error(sprintf "support values do not match: %s" report)

let private failingSubpathSpecimenReports specimens =
    specimens
    |> List.choose (fun (name, segments) ->
        match subpathHullFailureReason segments with
        | Ok() -> None
        | Error reason -> Some(sprintf "%s: %s" name reason))



let private quadraticArch () =
    QuadraticBezier(point 0.0 0.0, point 40.0 90.0, point 100.0 0.0)

let private shallowQuadratic () =
    QuadraticBezier(point -50.0 -5.0, point 10.0 8.0, point 75.0 4.0)

let private offAxisQuadratic () =
    QuadraticBezier(point 42.0 -80.0, point -35.0 140.0, point 118.0 25.0)

let private farControlQuadratic () =
    QuadraticBezier(point -12.0 18.0, point 260.0 -310.0, point 95.0 44.0)

let private degenerateLine () =
    Line(point 33.0 -17.0, point 33.0 -17.0)

let private pointQuadratic () =
    QuadraticBezier(point 12.0 -9.0, point 12.0 -9.0, point 12.0 -9.0)

let private closedQuadratic () =
    QuadraticBezier(point 12.0 -9.0, point 45.0 30.0, point 12.0 -9.0)

let private colinearInsideQuadratic () =
    QuadraticBezier(point 0.0 0.0, point 40.0 0.0, point 100.0 0.0)

let private colinearOutsideQuadratic () =
    QuadraticBezier(point 0.0 0.0, point 140.0 0.0, point 100.0 0.0)

let private diagonalColinearInsideQuadratic () =
    QuadraticBezier(point 0.0 0.0, point 40.0 40.0, point 100.0 100.0)

let private diagonalColinearOutsideQuadratic () =
    QuadraticBezier(point 0.0 0.0, point 140.0 140.0, point 100.0 100.0)

let private stem () =
    CubicBezier(point 5.0 70.0, point 30.0 20.0, point 65.0 105.0, point 95.0 30.0)

let private horseshoe () =
    CubicBezier(point 20.0 80.0, point 20.0 5.0, point 100.0 5.0, point 100.0 80.0)

let private horseshoeWide () =
    CubicBezier(point 20.0 90.0, point -25.0 0.0, point 145.0 0.0, point 100.0 90.0)

let private diagonalLine () =
    Line(point 10.0 85.0, point 120.0 15.0)

let private reverseDiagonalLine () =
    Line(point 10.0 15.0, point 120.0 85.0)

let private horizontalLine () =
    Line(point 10.0 50.0, point 120.0 50.0)

let private verticalLine () =
    Line(point 65.0 10.0, point 65.0 90.0)

let private snakeCubic () =
    CubicBezier(point 15.0 55.0, point 135.0 0.0, point -20.0 110.0, point 105.0 55.0)

let private fishCubic () =
    CubicBezier(point 25.0 40.0, point 155.0 100.0, point 155.0 10.0, point 25.0 70.0)

let private delCubic () =
    CubicBezier(point 100.0 20.0, point 120.0 60.0, point 0.0 140.0, point 100.0 40.0)

let private flourishCubic () =
    CubicBezier(point 100.0 20.0, point 120.0 60.0, point 20.0 140.0, point 120.0 40.0)

let private leftHookCubic () =
    CubicBezier(point 120.0 120.0, point 121.0 120.0, point 20.0 20.0, point 120.0 20.0)

let private halfCircleArc sweep =
    Arc
        { Start = point 20.0 80.0
          Radius = point 40.0 40.0
          XAxisRotation = Degree.fromFloat 0.0
          LargeArc = false
          Sweep = sweep
          End = point 100.0 80.0 }

let private rotatedArc sweep =
    Arc
        { Start = point 30.0 80.0
          Radius = point 55.0 25.0
          XAxisRotation = Degree.fromFloat 30.0
          LargeArc = false
          Sweep = sweep
          End = point 120.0 40.0 }

let private largeArc sweep =
    Arc
        { Start = point 20.0 70.0
          Radius = point 50.0 35.0
          XAxisRotation = Degree.fromFloat 0.0
          LargeArc = true
          Sweep = sweep
          End = point 100.0 70.0 }

let private tinyLine () =
    Line(point 0.0 0.0, point 0.00001 0.00001)

let private almostHorizontalLine () =
    Line(point -100.0 0.0, point 100.0 0.000001)

let private almostVerticalLine () =
    Line(point 0.0 -100.0, point 0.000001 100.0)

let private nearlyStraightCubic () =
    CubicBezier(point 0.0 0.0, point 33.0 0.000001, point 66.0 -0.000001, point 100.0 0.0)

let private tinyCubic () =
    CubicBezier(point 0.0 0.0, point 0.00001 0.00002, point -0.00003 0.00004, point 0.00005 0.0)

let private flatCubic () =
    CubicBezier(point -120.0 0.0, point -60.0 0.1, point 60.0 -0.1, point 120.0 0.0)

let private farControlCubic () =
    CubicBezier(point 0.0 0.0, point 1000.0 600.0, point -900.0 700.0, point 100.0 0.0)

let private endpointControlCubic () =
    CubicBezier(point 0.0 0.0, point 0.0 0.0, point 100.0 0.0, point 100.0 0.0)

let private oppositeFarControlsCubic () =
    CubicBezier(point -20.0 -10.0, point 500.0 -450.0, point -520.0 470.0, point 30.0 20.0)

let private nearCuspCubic () =
    CubicBezier(point 0.0 0.0, point 100.0 0.0, point -100.0 0.0, point 0.001 0.0)

let private wideLoopCubic () =
    CubicBezier(point -80.0 0.0, point 180.0 160.0, point -180.0 160.0, point 80.0 0.0)

let private narrowLoopCubic () =
    CubicBezier(point -5.0 0.0, point 95.0 120.0, point -95.0 120.0, point 5.0 0.0)

let private flatArc sweep =
    Arc
        { Start = point -100.0 0.0
          Radius = point 120.0 1.0
          XAxisRotation = Degree.fromFloat 0.0
          LargeArc = false
          Sweep = sweep
          End = point 100.0 0.0 }

let private tallArc sweep =
    Arc
        { Start = point 0.0 -100.0
          Radius = point 1.0 120.0
          XAxisRotation = Degree.fromFloat 0.0
          LargeArc = false
          Sweep = sweep
          End = point 0.0 100.0 }

let private rotatedLargeArc sweep =
    Arc
        { Start = point -70.0 20.0
          Radius = point 95.0 20.0
          XAxisRotation = Degree.fromFloat 73.0
          LargeArc = true
          Sweep = sweep
          End = point 80.0 -10.0 }

let private nearEndpointArc sweep =
    Arc
        { Start = point 10.0 10.0
          Radius = point 40.0 30.0
          XAxisRotation = Degree.fromFloat 15.0
          LargeArc = false
          Sweep = sweep
          End = point 10.0001 10.0001 }

let private wave value salt = System.Math.Sin (value * salt * 12.9898) * 50.0

let private generatedCubic i =
    let x = float i
    let scale =
        match i % 4 with
        | 0 -> 1.0
        | 1 -> 0.01
        | 2 -> 100.0
        | _ -> 10.0
    CubicBezier(
        point (scale * wave x 3.0) (scale * wave x 11.0),
        point (scale * 4.0 * wave x 17.0) (scale * 3.0 * wave x 23.0),
        point (scale * 4.0 * wave x 31.0) (scale * 3.0 * wave x 41.0),
        point (scale * wave x 47.0) (scale * wave x 59.0))

let private normalizeDegreesFloat (degrees: float) =
    if degrees < 0.0 then degrees + 360.0
    elif degrees >= 360.0 then degrees - 360.0
    else degrees

let private generatedArc i =
    let x = float i + 1.0
    let scale =
        match i % 4 with
        | 0 -> 1.0
        | 1 -> 0.05
        | 2 -> 40.0
        | _ -> 8.0
    Arc
        { Start = point (scale * wave x 5.0) (scale * wave x 7.0)
          Radius = point (1.0 + scale * abs (wave x 11.0)) (1.0 + scale * abs (wave x 13.0))
          XAxisRotation = Degree.fromFloat (normalizeDegreesFloat (wave x 17.0))
          LargeArc = i % 3 = 0
          Sweep = i % 2 = 0
          End = point (scale * (wave x 19.0 + 0.5)) (scale * (wave x 23.0 - 0.5)) }

let private generatedCubicSpecimens () =
    [ 0 .. 35 ]
    |> List.map (fun i -> sprintf "generated_cubic_%d" i, generatedCubic i)

let private generatedArcSpecimens () =
    [ 3; 11; 22 ]
    |> List.map (fun i -> sprintf "generated_arc_%d" i, generatedArc i)

let private reversedGeneratedWitnessSpecimens () =
    [ "generated_cubic_0_reverse", Segment.reverse (generatedCubic 0)
      "generated_arc_3_reverse", Segment.reverse (generatedArc 3)
      "generated_arc_11_reverse", Segment.reverse (generatedArc 11)
      "generated_arc_22_reverse", Segment.reverse (generatedArc 22) ]

let private curveAndLineSpecimens () =
    [ "stem", stem ()
      "horseshoe", horseshoe ()
      "horseshoe_wide", horseshoeWide ()
      "diagonal_line", diagonalLine ()
      "reverse_diagonal_line", reverseDiagonalLine ()
      "horizontal_line", horizontalLine ()
      "vertical_line", verticalLine ()
      "snake_cubic", snakeCubic ()
      "fish_cubic", fishCubic ()
      "del_cubic", delCubic ()
      "flourish_cubic", flourishCubic ()
      "left_hook_cubic", leftHookCubic ()
      "quadratic_arch", quadraticArch ()
      "shallow_quadratic", shallowQuadratic ()
      "off_axis_quadratic", offAxisQuadratic ()
      "far_control_quadratic", farControlQuadratic ()
      "degenerate_line", degenerateLine ()
      "point_quadratic", pointQuadratic ()
      "closed_quadratic", closedQuadratic ()
      "colinear_inside_quadratic", colinearInsideQuadratic ()
      "colinear_outside_quadratic", colinearOutsideQuadratic ()
      "diagonal_colinear_inside_quadratic", diagonalColinearInsideQuadratic ()
      "diagonal_colinear_outside_quadratic", diagonalColinearOutsideQuadratic () ]

let private arcSpecimens () =
    [ "half_circle_arc", halfCircleArc true
      "half_circle_arc_reverse", halfCircleArc false
      "rotated_arc", rotatedArc true
      "rotated_arc_reverse", rotatedArc false
      "large_arc", largeArc true
      "large_arc_reverse", largeArc false ]

let private specimens () =
    List.append (curveAndLineSpecimens ()) (arcSpecimens ())

let private adversarialSpecimens () =
    let named =
        [ "tiny_line", tinyLine ()
          "almost_horizontal_line", almostHorizontalLine ()
          "almost_vertical_line", almostVerticalLine ()
          "nearly_straight_cubic", nearlyStraightCubic ()
          "tiny_cubic", tinyCubic ()
          "flat_cubic", flatCubic ()
          "far_control_cubic", farControlCubic ()
          "endpoint_control_cubic", endpointControlCubic ()
          "opposite_far_controls_cubic", oppositeFarControlsCubic ()
          "near_cusp_cubic", nearCuspCubic ()
          "wide_loop_cubic", wideLoopCubic ()
          "narrow_loop_cubic", narrowLoopCubic ()
          "flat_arc", flatArc true
          "flat_arc_reverse", flatArc false
          "tall_arc", tallArc true
          "tall_arc_reverse", tallArc false
          "rotated_large_arc", rotatedLargeArc true
          "rotated_large_arc_reverse", rotatedLargeArc false
          "near_endpoint_arc", nearEndpointArc true
          "near_endpoint_arc_reverse", nearEndpointArc false ]
    named
    |> List.append (generatedCubicSpecimens ())
    |> List.append (generatedArcSpecimens ())
    |> List.append (reversedGeneratedWitnessSpecimens ())

let private connectSegmentAfter segment endPoint =
    let start = Segment.start segment
    Transform.translateSegment segment (endPoint.X - start.X) (endPoint.Y - start.Y)
    |> Result.mapError (fun _ -> ())

let private transformedSpecimenVariants name segment =
    let apply suffix transform =
        match transform () with
        | Ok segment -> Some(name + "_" + suffix, segment)
        | Error _ -> None
    [ apply "translated" (fun () -> Transform.translateSegment segment (Length.fromFloat 37.0) (Length.fromFloat -19.0))
      apply "rotated" (fun () -> Transform.rotateSegment segment (Degree.fromFloat 37.0))
      apply "scaled" (fun () -> Transform.scaleSegment segment 1.7)
      apply "reflected_x" (fun () -> Transform.scaleXYSegment segment -1.0 1.0)
      apply "reflected_y" (fun () -> Transform.scaleXYSegment segment 1.0 -1.0)
      apply "stretched" (fun () -> Transform.scaleXYSegment segment 0.25 3.0)
      apply "skewed_x" (fun () -> Transform.skewXSegment segment (Degree.fromFloat 12.0)) ]
    |> List.choose id

let private transformedAdversarialSpecimens () =
    adversarialSpecimens ()
    |> List.take 14
    |> List.collect (fun (name, segment) ->
        transformedSpecimenVariants name segment
        @ [ "reverse_" + name, Segment.reverse segment ])

let private pairedSubpathSpecimens () =
    let adjacentPairs items =
        let rec loop = function
            | [] | [ _ ] -> []
            | left :: right :: rest -> (left, right) :: loop (right :: rest)
        loop items
    curveAndLineSpecimens ()
    |> List.take 8
    |> adjacentPairs
    |> List.choose (fun ((leftName, left), (rightName, right)) ->
        match connectSegmentAfter right (Segment.finish left) with
        | Ok connectedRight -> Some(sprintf "%s_then_%s" leftName rightName, [ left; connectedRight ])
        | Error() -> None)


let private pointCloudHullIsValid points (hull: Subpath) =
    hull.Closed
    && points
       |> List.forall (fun candidate ->
           ConvexHull.internalPointChordPolygonLoopSeparation hull.Segments candidate = None)
    && tenDegreeAngles ()
       |> List.forall (fun angle ->
           match pointCloudSupportValue points angle, segmentsSupportValue hull.Segments angle with
           | Some original, Some hullValue -> valuesNear original hullValue tolerance
           | _ -> false)

let private publicPointCloudHullIsValid points =
    match ConvexHull.pointsHull points with
    | Error _ -> false
    | Ok hull -> pointCloudHullIsValid points hull

let private pointCloudIsValidInAllModes points =
    publicPointCloudHullIsValid points
    && repairModesToCheck
       |> List.forall (fun repairMode ->
           let path = points |> List.map Subpath.empty |> Path.ofSubpaths
           match ConvexHull.internalPathHullWithRepairMode path repairMode with
           | Error _ -> false
           | Ok hull -> pointCloudHullIsValid points hull)

let private randomPoint index =
    point
        (float (((index * 73 + 19) * (index * 17 + 23) + 11) % 10001) / 100.0)
        (float (((index * 41 + 29) * (index * 97 + 31) + 7) % 10001) / 100.0)

let private randomPoints count = [ 0 .. count - 1 ] |> List.map randomPoint

let private pointCloudHullIsValidForCount count =
    pointCloudIsValidInAllModes (randomPoints count)

let private unitCirclePoint index =
    let angle = float ((index * 97 + 13) % 10000) / 10000.0 * System.Math.PI * 2.0
    let radius = sqrt (float (((index * 37 + 17) * (index * 53 + 29) + 5) % 10000) / 10000.0)
    point (radius * cos angle) (radius * sin angle)

let private unitCirclePoints count = [ 0 .. count - 1 ] |> List.map unitCirclePoint

let private radius1000Point angle =
    point
        (1000.0 * Trig.cosDegrees (Degree.fromFloat angle))
        (1000.0 * Trig.sinDegrees (Degree.fromFloat angle))

let private chordPointAtY y lineStart lineEnd =
    let t = (y - lineStart.Y) / (lineEnd.Y - lineStart.Y)
    point (float (lineStart.X + t * (lineEnd.X - lineStart.X))) (float y)

let private crescentCandidatePoint index startAngle endAngle =
    let angleSpan = endAngle - startAngle
    let angle =
        startAngle
        + angleSpan * (float ((index * 89 + 37) % 10000) / 10000.0)
    let circle =
        point
            (1000.0 * Trig.cosDegrees (Degree.fromFloat angle))
            (1000.0 * Trig.sinDegrees (Degree.fromFloat angle))
    let lineStart = radius1000Point startAngle
    let lineEnd = radius1000Point endAngle
    let chord = chordPointAtY circle.Y lineStart lineEnd
    let fraction =
        0.05
        + 0.9 * (float (((index * 61 + 43) * (index * 31 + 29) + 17) % 10000) / 10000.0)
    point
        (float (chord.X + fraction * (circle.X - chord.X)))
        (float (chord.Y + fraction * (circle.Y - chord.Y)))
let private chordSide point lineStart lineEnd =
    (lineEnd.X - lineStart.X) * (point.Y - lineStart.Y)
    - (lineEnd.Y - lineStart.Y) * (point.X - lineStart.X)

let private pointInsideCrescent candidate lineStart lineEnd =
    let radiusSquared = candidate.X * candidate.X + candidate.Y * candidate.Y
    radiusSquared <= 1000.0<length> * 1000.0<length> + 1.0e-6<length^2>
    && chordSide candidate lineStart lineEnd <= 1.0e-6<length^2>

let private crescentPoints count startAngle endAngle =
    let lineStart = radius1000Point startAngle
    let lineEnd = radius1000Point endAngle
    [ 0 .. count * 20 - 1 ]
    |> List.choose (fun index ->
        let candidate = crescentCandidatePoint index startAngle endAngle
        if pointInsideCrescent candidate lineStart lineEnd then Some candidate else None)
    |> List.truncate count

let private oneSidedCrescentPoints () =
    crescentPoints 100 0.0 1.0

let private twoSidedCrescentPoints () =
    crescentPoints 100 -0.5 0.5

let private crescentPath points lineStart lineEnd =
    let line = Subpath.ofSegment (Line(lineStart, lineEnd))
    Path.ofSubpaths (line :: (points |> List.map Subpath.empty))

let private crescentPathIsValidInAllModes points startAngle endAngle =
    let start = radius1000Point startAngle
    let finish = radius1000Point endAngle
    let supportPoints = start :: finish :: points
    repairModesToCheck
    |> List.forall (fun repairMode ->
        match ConvexHull.internalPathHullWithRepairMode (crescentPath points start finish) repairMode with
        | Error _ -> false
        | Ok hull -> pointCloudHullIsValid supportPoints hull)

let private rectanglePath minX minY maxX maxY : Path =
    Path.ofSubpaths
        [ Subpath.polygon [ point minX minY; point maxX minY; point maxX maxY; point minX maxY ]
          |> Result.defaultWith (failwithf "%A") ]

let private unscalePoint (point: Point<length>) (scale: float) : Point<length> =
    Point.create (point.X / scale) (point.Y / scale)

let private unscaleBox (box: BoundingBox) (scale: float) : BoundingBox =
    { Min = unscalePoint box.Min scale
      Max = unscalePoint box.Max scale }

let private floatsNearlyEqual (left: float<'Unit>) (right: float<'Unit>) =
    let magnitude = max 1.0 (max (abs (float left)) (abs (float right)))
    abs (left - right)
    <= LanguagePrimitives.FloatWithMeasure<'Unit> (scaleCovarianceRelativeTolerance * magnitude)

let private pointsNearlyEqual (left: Point<length>) (right: Point<length>) =
    floatsNearlyEqual left.X right.X && floatsNearlyEqual left.Y right.Y

let private boxesNearlyEqual (left: BoundingBox) (right: BoundingBox) =
    pointsNearlyEqual left.Min right.Min && pointsNearlyEqual left.Max right.Max

let private representativeGeometryIsCovariantAtScale scale =
    let cubic =
        CubicBezier(point -2.0 1.0, point 0.5 5.0, point 4.0 -3.0, point 7.0 2.0)
    let crossingLeft = Line(point 0.0 0.0, point 10.0 10.0)
    let crossingRight = Line(point 0.0 10.0, point 10.0 0.0)
    let overlapLeft = Line(point 0.0 0.0, point 10.0 0.0)
    let overlapRight = Line(point 4.0 0.0, point 12.0 0.0)
    let leftPath = rectanglePath 0.0 0.0 4.0 3.0
    let rightPath = rectanglePath 2.0 1.0 6.0 4.0

    let scaledCubic = Transform.scaleSegment cubic scale |> Result.defaultWith (failwithf "%A")
    let referencePoint =
        Segment.point cubic (Parameter.fromFloat 0.37) |> Result.defaultWith (failwithf "%A")
    let scaledPoint =
        Segment.point scaledCubic (Parameter.fromFloat 0.37)
        |> Result.defaultWith (failwithf "%A")
    let scaledCrossingLeft =
        Transform.scaleSegment crossingLeft scale |> Result.defaultWith (failwithf "%A")
    let scaledCrossingRight =
        Transform.scaleSegment crossingRight scale |> Result.defaultWith (failwithf "%A")
    let intersectionOptions =
        { Tolerance = Length.fromFloat (1.0e-9 * scale)
          MaxDepth = 48
          ParameterSnap = NoParameterSnap }
    let intersectionsResult =
        Intersections.segmentWith scaledCrossingLeft scaledCrossingRight intersectionOptions
        |> Result.defaultWith (failwithf "%A")
    let scaledOverlapLeft =
        Transform.scaleSegment overlapLeft scale |> Result.defaultWith (failwithf "%A")
    let scaledOverlapRight =
        Transform.scaleSegment overlapRight scale |> Result.defaultWith (failwithf "%A")
    let overlapsResult =
        Overlaps.segmentWith scaledOverlapLeft scaledOverlapRight (Length.fromFloat (1.0e-9 * scale))
        |> Result.defaultWith (failwithf "%A")
    let referenceHull = ConvexHull.segmentHull cubic |> Result.defaultWith (failwithf "%A")
    let scaledHull = ConvexHull.segmentHull scaledCubic |> Result.defaultWith (failwithf "%A")
    let referenceHullBox =
        Subpath.boundingBox referenceHull |> Result.defaultWith (failwithf "%A")
    let scaledHullBox =
        Subpath.boundingBox scaledHull |> Result.defaultWith (failwithf "%A")
    let scaledLeftPath =
        Transform.scalePath leftPath scale |> Result.defaultWith (failwithf "%A")
    let scaledRightPath =
        Transform.scalePath rightPath scale |> Result.defaultWith (failwithf "%A")
    let unionResult =
        Csg.unionWith
            scaledLeftPath
            scaledRightPath
            Nonzero
            { Tolerance = Length.fromFloat (1.0e-6 * scale)
              MinimumChord = Length.fromFloat (1.0e-5 * scale) }
        |> Result.defaultWith (failwithf "%A")
    let unionBox = Path.boundingBox unionResult.Path |> Result.defaultWith (failwithf "%A")

    match intersectionsResult, overlapsResult with
    | [ intersection ], [ overlap ] ->
        pointsNearlyEqual referencePoint (unscalePoint scaledPoint scale)
        && floatsNearlyEqual intersection.LeftT (0.5<parameter>)
        && floatsNearlyEqual intersection.RightT (0.5<parameter>)
        && pointsNearlyEqual (point 5.0 5.0) (unscalePoint intersection.Point scale)
        && floatsNearlyEqual overlap.LeftFrom (0.4<parameter>)
        && floatsNearlyEqual overlap.LeftTo (1.0<parameter>)
        && floatsNearlyEqual overlap.RightFrom (0.0<parameter>)
        && floatsNearlyEqual overlap.RightTo (0.75<parameter>)
        && pointsNearlyEqual (point 4.0 0.0) (unscalePoint overlap.Start scale)
        && pointsNearlyEqual (point 10.0 0.0) (unscalePoint overlap.Finish scale)
        && boxesNearlyEqual referenceHullBox (unscaleBox scaledHullBox scale)
        && boxesNearlyEqual
            { Min = point 0.0 0.0
              Max = point 6.0 4.0 }
            (unscaleBox unionBox scale)
        && unionResult.Path.Subpaths.Length = 1
    | _ -> false

[<Fact>]
let ``representative geometry is covariant at 1e-3`` () =
    Assert.True(representativeGeometryIsCovariantAtScale 0.001)

[<Fact>]
let ``representative geometry is covariant at 1`` () =
    Assert.True(representativeGeometryIsCovariantAtScale 1.0)

[<Fact>]
let ``representative geometry is covariant at 1e3`` () =
    Assert.True(representativeGeometryIsCovariantAtScale 1000.0)

[<Fact>]
let ``representative geometry is covariant at 1e6`` () =
    Assert.True(representativeGeometryIsCovariantAtScale 1_000_000.0)

[<Fact>]
let ``representative geometry is covariant at 1e9`` () =
    Assert.True(representativeGeometryIsCovariantAtScale 1_000_000_000.0)

[<Fact>]
let ``point cloud hull handles 1000 point cloud`` () =
    Assert.True (pointCloudHullIsValidForCount 1000)

[<Fact>]
let ``point cloud hull handles 1000 unit circle point cloud`` () =
    Assert.True (pointCloudIsValidInAllModes (unitCirclePoints 1000))

[<Fact>]
let ``point cloud hull handles one sided 1 degree crescent point cloud`` () =
    Assert.True (pointCloudIsValidInAllModes (oneSidedCrescentPoints ()))

[<Fact>]
let ``point cloud hull handles two sided 1 degree crescent point cloud`` () =
    Assert.True (pointCloudIsValidInAllModes (twoSidedCrescentPoints ()))

[<Fact>]
let ``path hull handles one sided 1 degree crescent points and chord`` () =
    Assert.True (crescentPathIsValidInAllModes (oneSidedCrescentPoints ()) 0.0 1.0)

[<Fact>]
let ``path hull handles two sided 1 degree crescent points and chord`` () =
    Assert.True (crescentPathIsValidInAllModes (twoSidedCrescentPoints ()) -0.5 0.5)

[<Fact>]
let ``segment hull returns two segments for point cubic`` () =
    let segment =
        CubicBezier(point 0.0 0.0, point 0.0 0.0, point 0.0 0.0, point 0.0 0.0)
    let subpath = ConvexHull.segmentHull segment |> Result.defaultWith (failwithf "%A")
    Assert.True subpath.Closed
    Assert.Equal(2, subpath.Segments.Length)

[<Fact>]
let ``segment hull handles near endpoint arc`` () =
    let subpath =
        ConvexHull.segmentHull (nearEndpointArc true)
        |> Result.defaultWith (failwithf "%A")
    Assert.True subpath.Closed
    Assert.Equal(2, subpath.Segments.Length)

[<Fact>]
let ``subpath hull handles curved subpath`` () =
    let curve =
        CubicBezier(point 0.0 0.0, point 30.0 60.0, point 80.0 -30.0, point 100.0 20.0)
    let tail = Line(point 100.0 20.0, point 135.0 70.0)
    let segments = [ curve; tail ]
    let subpath = Subpath.create segments |> Result.defaultWith (failwithf "%A")
    let hull = ConvexHull.subpathHull subpath |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.True(hull.Segments.Length >= 3)
    Assert.True (subpathSupportMatchesBool segments hull)

[<Fact>]
let ``path hull returns closed hull for multiple subpaths`` () =
    let leftSegments = [ Line(point 0.0 0.0, point 20.0 0.0) ]
    let rightSegments = [ Line(point 40.0 30.0, point 50.0 -10.0) ]
    let path =
        Path.ofSubpaths
            [ Subpath.ofSegment (List.head leftSegments)
              Subpath.ofSegment (List.head rightSegments) ]
    let hull = ConvexHull.pathHull path |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.True (subpathSupportMatchesBool (leftSegments @ rightSegments) hull)

[<Fact>]
let ``path hull handles customer line path`` () =
    let source =
        "M -0.00000 -299.30766 L -0.00000 299.30766 "
        + "M -0.00000 -299.30766 L 8.65413 -304.36344 "
        + "M -0.00000 -299.30766 L -79.02201 -331.51341 "
        + "M -0.00000 299.30766 L 8.65413 294.25187 "
        + "M -0.00000 299.30766 L -79.02201 267.10191 "
        + "M 8.65413 -304.36344 L 8.65413 294.25187 "
        + "M 8.65413 -304.36344 L -70.36788 -336.56919 "
        + "M -79.02201 -331.51341 L -79.02201 267.10191 "
        + "M -79.02201 -331.51341 L -70.36788 -336.56919 "
        + "M 8.65413 294.25187 L -70.36788 262.04612 "
        + "M -79.02201 267.10191 L -70.36788 262.04612 "
        + "M -70.36788 -336.56919 L -70.36788 262.04612"
    let path = Parse.path source |> Result.defaultWith (failwithf "%A")
    let hull = ConvexHull.pathHull path |> Result.defaultWith (failwithf "%A")
    let originalSegments = path.Subpaths |> List.collect (fun subpath -> subpath.Segments)
    Assert.True hull.Closed
    Assert.True (subpathSupportMatchesBool originalSegments hull)

[<Fact>]
let ``path hull handles customer polyline path`` () =
    let source =
        "M -0.00000 -299.30766 L -0.00000 299.30766 "
        + "L 8.65413 -304.36344 L -79.02201 -331.51341 "
        + "L 8.65413 294.25187 L -79.02201 267.10191 "
        + "L -70.36788 -336.56919 L -70.36788 262.04612"
    let path = Parse.path source |> Result.defaultWith (failwithf "%A")
    let hull = ConvexHull.pathHull path |> Result.defaultWith (failwithf "%A")
    let originalSegments = path.Subpaths |> List.collect (fun subpath -> subpath.Segments)
    Assert.True hull.Closed
    Assert.True (subpathSupportMatchesBool originalSegments hull)

[<Fact>]
let ``path hull treats path with only empty subpaths as points`` () =
    let left = point 0.0 0.0
    let right = point 10.0 0.0
    let hull =
        ConvexHull.pathHull (Path.ofSubpaths [ Subpath.empty left; Subpath.empty right ])
        |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    Assert.True (nearValue (hullSupportValue hull.Segments 0.0) 10.0<length>)
    Assert.True (nearValue (hullSupportValue hull.Segments 180.0) 0.0<length>)

[<Fact>]
let ``specimen hulls survive strict subpath constructor`` () =
    let failures =
        specimens ()
        |> List.choose (fun (name, segment) ->
            match ConvexHull.segmentHull segment with
            | Ok _ -> None
            | Error _ -> Some(name + ": segment hull failed"))
    Assert.Empty failures

[<Fact>]
let ``specimen hulls have at least two segments`` () =
    let failures =
        specimens ()
        |> List.choose (fun (name, segment) ->
            match ConvexHull.segmentHull segment with
            | Ok hull when hull.Segments.Length >= 2 -> None
            | Ok hull -> Some(sprintf "%s: hull has %d segments" name hull.Segments.Length)
            | Error _ -> Some(name + ": segment hull failed"))
    Assert.Empty failures

[<Fact>]
let ``specimen hull derivative angles are nondecreasing`` () =
    let failures =
        specimens ()
        |> List.choose (fun (name, segment) ->
            match ConvexHull.segmentHull segment with
            | Ok hull when hullDerivativeAnglesAreNondecreasing hull -> None
            | Ok _ -> Some(name + ": derivative angles are not nondecreasing")
            | Error _ -> Some(name + ": segment hull failed"))
    Assert.Empty failures

[<Fact>]
let ``specimen hull support matches original at 10 degree steps`` () =
    let failures =
        specimens ()
        |> List.collect (fun (name, segment) ->
            match ConvexHull.segmentHull segment with
            | Error _ -> [ name + ": segment hull failed" ]
            | Ok hull ->
                multiplesOfTenDegrees ()
                |> List.choose (fun angle ->
                    match originalSupportValue segment angle, hullSupportValue hull.Segments angle with
                    | Ok original, Ok hullValue ->
                        if near original hullValue then None
                        else Some(sprintf "%s angle=%A original=%A hull=%A" name angle original hullValue)
                    | _ -> Some(sprintf "%s angle=%A support errored" name angle)))
    Assert.Empty failures

[<Fact>]
let ``smart segment support matches brute support at 10 degree steps`` () =
    let failures =
        specimens ()
        |> List.collect (fun (name, segment) ->
            multiplesOfTenDegrees ()
            |> List.choose (fun angle ->
                let smart =
                    ConvexHull.internalSegmentSupport segment (Degree.fromFloat angle)
                    |> Result.map (fun (_, _, value) -> value)
                let brute =
                    segmentSupportPoint segment angle
                    |> Result.map (fun point -> pointSupport point angle)
                match smart, brute with
                | Ok smartValue, Ok bruteValue ->
                    if abs (smartValue - bruteValue) <= smartSupportTolerance segment then None
                    else Some(sprintf "%s angle=%A smart=%A brute=%A" name angle smartValue bruteValue)
                | _ -> Some(sprintf "%s angle=%A support errored" name angle)))
    Assert.Empty failures

[<Fact>]
let ``adversarial segment hulls pass geometry checks`` () =
    Assert.Empty (failingSpecimenReports (adversarialSpecimens ()))

[<Fact>]
let ``paired specimen subpath hulls pass support checks`` () =
    Assert.Empty (failingSubpathSpecimenReports (pairedSubpathSpecimens ()))

[<Fact>]
let ``transformed adversarial segment hulls pass geometry checks`` () =
    Assert.Empty (failingSpecimenReports (transformedAdversarialSpecimens ()))