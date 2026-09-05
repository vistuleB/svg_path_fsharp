module SvgPath.Tests.ConvexHullSlowSupport

open SvgPath

let point (x: float) (y: float) = Point.create (Length.fromFloat x) (Length.fromFloat y)

let octantAngles () = [ 0.0; 45.0; 90.0; 135.0; 180.0; 225.0; 270.0; 315.0 ]

let tenDegreeAngles () = [ 0.0 .. 10.0 .. 350.0 ]

let angleDirection angle = Point.direction (Degree.fromFloat angle)

let dot (a: Point<'Left>) (b: Point<'Right>) = Point.dot a b

let pointCloudSupportValue (points: Point<length> list) angle =
    let direction = angleDirection angle
    match points with
    | [] -> None
    | first :: rest ->
        rest
        |> List.fold
            (fun best candidate -> max best (dot candidate direction))
            (dot first direction)
        |> Some

let segmentsSupportValue (segments: Segment list) angle =
    let value segment =
        ConvexHull.internalSegmentSupport segment (Degree.fromFloat angle)
        |> Result.map (fun (_, _, value) -> value)
    match segments with
    | [] -> None
    | first :: rest ->
        rest
        |> List.fold
            (fun best segment ->
                match best, value segment with
                | Some bestValue, Ok candidate -> Some(max bestValue candidate)
                | None, Ok candidate -> Some candidate
                | _, Error _ -> None)
            (value first |> Result.toOption)

let valuesNear (a: float<length>) (b: float<length>) (tolerance: float<length>) =
    abs (a - b) <= tolerance