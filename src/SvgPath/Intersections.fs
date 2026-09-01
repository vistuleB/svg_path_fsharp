namespace SvgPath

[<Struct>]
type SegmentIntersection =
    { LeftT: float<parameter>
      RightT: float<parameter>
      Point: Point<length> }

[<Struct>]
type SubpathIntersection =
    { Point: Point<length>
      LeftParameters: SubpathParameter list
      RightParameters: SubpathParameter list }

[<Struct>]
type PathIntersection =
    { Point: Point<length>
      LeftParameters: PathParameter list
      RightParameters: PathParameter list }

[<Struct>]
type IntersectionOptions =
    { Tolerance: float<length>
      MaxDepth: int }

[<Struct>]
type private IntersectionWindow =
    { LeftFrom: float<parameter>
      LeftTo: float<parameter>
      RightFrom: float<parameter>
      RightTo: float<parameter>
      Depth: int }

[<RequireQualifiedAccess>]
module Intersections =
    let defaultOptions =
        { Tolerance = 1.0e-9<length>
          MaxDepth = 48 }

    let private maximumWindows = 1000
    let private parameterTolerance = 1.0e-7<parameter>
    let private enclosureSlack = 1.0e-12<length>

    let private parameter value = Parameter.fromFloat value
    let private ratio (value: float<parameter>) = Parameter.ratio value
    let private clamp01 value = max 0.0<parameter> (min 1.0<parameter> value)
    let private interpolate
        (fromValue: float<parameter>)
        (toValue: float<parameter>)
        (portion: float<parameter>)
        : float<parameter> =
        fromValue + ratio portion * (toValue - fromValue)

    let private midpoint (left: Point<length>) (right: Point<length>) =
        Point.create ((left.X + right.X) / 2.0) ((left.Y + right.Y) / 2.0)

    let private boxesOverlap slack left right =
        left.Max.X + slack >= right.Min.X
        && right.Max.X + slack >= left.Min.X
        && left.Max.Y + slack >= right.Min.Y
        && right.Max.Y + slack >= left.Min.Y

    let private pointsNear tolerance left right = Point.squaredDistance left right <= tolerance * tolerance

    let private insert tolerance candidate existing =
        if existing
           |> List.exists (fun found ->
               abs (found.LeftT - candidate.LeftT) <= parameterTolerance
               && abs (found.RightT - candidate.RightT) <= parameterTolerance
               || pointsNear tolerance found.Point candidate.Point) then existing
        else candidate :: existing

    let private endpointCandidates left right tolerance =
        let samples =
            [ 0.0<parameter>, 0.0<parameter>
              0.0<parameter>, 1.0<parameter>
              1.0<parameter>, 0.0<parameter>
              1.0<parameter>, 1.0<parameter> ]
        samples
        |> List.fold (fun state (leftT, rightT) ->
            state
            |> Result.bind (fun found ->
                match Segment.point left leftT, Segment.point right rightT with
                | Ok leftPoint, Ok rightPoint when pointsNear tolerance leftPoint rightPoint ->
                    Ok(insert tolerance { LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint } found)
                | Ok _, Ok _ -> Ok found
                | Error error, _
                | _, Error error -> Error error)) (Ok [])

    let private cross (left: Point<'Left>) (right: Point<'Right>) =
        left.X * right.Y - left.Y * right.X

    let private chordCrossing p p2 q q2 =
        let r = Point.displacement p p2
        let s = Point.displacement q q2
        let denominator = cross r s
        if denominator = 0.0<length^2> then None
        else
            let offset = Point.displacement p q
            let leftT = cross offset s / denominator |> Parameter.fromFloat
            let rightT = cross offset r / denominator |> Parameter.fromFloat
            if leftT >= 0.0<parameter> && leftT <= 1.0<parameter>
               && rightT >= 0.0<parameter> && rightT <= 1.0<parameter> then Some(leftT, rightT)
            else None

    let private chordClosestParameters p p2 q q2 =
        let u = Point.displacement p p2
        let v = Point.displacement q q2
        let w = Point.displacement q p
        let a = Point.dot u u
        let b = Point.dot u v
        let c = Point.dot v v
        let d = Point.dot u w
        let e = Point.dot v w
        let denominator = a * c - b * b
        let leftT, rightT =
            if denominator = 0.0<length^4> then 0.5<parameter>, 0.5<parameter>
            else
                Parameter.fromFloat (float ((b * e - c * d) / denominator)),
                Parameter.fromFloat (float ((a * e - b * d) / denominator))
        clamp01 leftT, clamp01 rightT

    let private refineCandidate left right leftT rightT =
        let rec loop leftT rightT iterations =
            if iterations = 0 then Ok(leftT, rightT)
            else
                match Segment.point left leftT, Segment.point right rightT,
                      Segment.derivative left leftT, Segment.derivative right rightT with
                | Ok leftPoint, Ok rightPoint, Ok leftDerivative, Ok rightDerivative ->
                    let delta = Point.displacement leftPoint rightPoint
                    let determinant = cross leftDerivative rightDerivative
                    if abs determinant <= 1.0e-20<length^2 / parameter^2> then Ok(leftT, rightT)
                    else
                        let leftStep = cross delta rightDerivative / determinant
                        let rightStep = cross delta leftDerivative / determinant
                        let nextLeft = clamp01 (leftT + leftStep)
                        let nextRight = clamp01 (rightT + rightStep)
                        if abs (nextLeft - leftT) <= 1.0e-14<parameter>
                           && abs (nextRight - rightT) <= 1.0e-14<parameter> then Ok(nextLeft, nextRight)
                        else loop nextLeft nextRight (iterations - 1)
                | Error error, _, _, _
                | _, Error error, _, _
                | _, _, Error error, _
                | _, _, _, Error error -> Error error
        loop leftT rightT 24

    let private candidateAt left right tolerance leftT rightT =
        refineCandidate left right leftT rightT
        |> Result.bind (fun (leftT, rightT) ->
            match Segment.point left leftT, Segment.point right rightT with
            | Ok leftPoint, Ok rightPoint when pointsNear tolerance leftPoint rightPoint ->
                Ok(Some { LeftT = leftT; RightT = rightT; Point = midpoint leftPoint rightPoint })
            | Ok _, Ok _ -> Ok None
            | Error error, _
            | _, Error error -> Error error)

    let private initialWindows maxDepth =
        [ for leftIndex in 0 .. 7 do
              for rightIndex in 0 .. 7 do
                  let leftFrom = parameter (float leftIndex / 8.0)
                  let leftTo = parameter (float (leftIndex + 1) / 8.0)
                  let rightFrom = parameter (float rightIndex / 8.0)
                  let rightTo = parameter (float (rightIndex + 1) / 8.0)
                  yield
                      { LeftFrom = leftFrom
                        LeftTo = leftTo
                        RightFrom = rightFrom
                        RightTo = rightTo
                        Depth = maxDepth } ]

    let private splitNine window =
        let leftThird = (window.LeftTo - window.LeftFrom) / 3.0
        let rightThird = (window.RightTo - window.RightFrom) / 3.0
        [ for leftIndex in 0 .. 2 do
              for rightIndex in 0 .. 2 do
                  let leftFrom = window.LeftFrom + float leftIndex * leftThird
                  let rightFrom = window.RightFrom + float rightIndex * rightThird
                  yield
                      { LeftFrom = leftFrom
                        LeftTo = leftFrom + leftThird
                        RightFrom = rightFrom
                        RightTo = rightFrom + rightThird
                        Depth = window.Depth - 1 } ]

    let private inspectWindow left right tolerance window =
        match Segment.betweenInside left window.LeftFrom window.LeftTo,
              Segment.betweenInside right window.RightFrom window.RightTo with
        | Error error, _
        | _, Error error -> Error error
        | Ok leftPiece, Ok rightPiece ->
            match Segment.boundingBox leftPiece, Segment.boundingBox rightPiece with
            | Error error, _
            | _, Error error -> Error error
            | Ok leftBox, Ok rightBox when not (boxesOverlap enclosureSlack leftBox rightBox) -> Ok(None, false)
            | Ok _, Ok _ ->
                match Segment.point left window.LeftFrom, Segment.point left window.LeftTo,
                      Segment.point right window.RightFrom, Segment.point right window.RightTo with
                | Ok leftStart, Ok leftFinish, Ok rightStart, Ok rightFinish ->
                    let local =
                        chordCrossing leftStart leftFinish rightStart rightFinish
                        |> Option.defaultWith (fun () -> chordClosestParameters leftStart leftFinish rightStart rightFinish)
                    let leftT = interpolate window.LeftFrom window.LeftTo (fst local)
                    let rightT = interpolate window.RightFrom window.RightTo (snd local)
                    candidateAt left right tolerance leftT rightT
                    |> Result.map (fun candidate -> candidate, Option.isNone candidate)
                | Error error, _, _, _
                | _, Error error, _, _
                | _, _, Error error, _
                | _, _, _, Error error -> Error error

    let private search left right options initial =
        let rec loop pending found examined =
            match pending with
            | [] -> Ok(found |> List.sortBy (fun item -> item.LeftT, item.RightT))
            | _ when examined >= maximumWindows -> Error(IntersectionTerminalWindowLimitExceeded maximumWindows)
            | window :: rest ->
                inspectWindow left right options.Tolerance window
                |> Result.bind (fun (candidate, refine) ->
                    let found = candidate |> Option.map (fun value -> insert options.Tolerance value found) |> Option.defaultValue found
                    if refine && window.Depth > 0 then loop (rest @ splitNine window) found (examined + 1)
                    else loop rest found (examined + 1))
        loop initial [] 0

    let validateOptions options =
        if options.Tolerance < 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidIntersectionTolerance options.Tolerance)
        elif options.MaxDepth <= 0 then Error(InvalidIntersectionMaxDepth options.MaxDepth)
        else Ok()

    let segmentWithoutOverlapPrecheckWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            endpointCandidates left right options.Tolerance
            |> Result.bind (fun endpoints ->
                search left right options (initialWindows options.MaxDepth)
                |> Result.map (List.fold (fun found item -> insert options.Tolerance item found) endpoints >> List.sortBy (fun item -> item.LeftT, item.RightT))))

    let segmentWith left right options =
        validateOptions options
        |> Result.bind (fun () ->
            OverlapDetection.detect left right options.Tolerance
            |> Result.bind (function
                | _ :: _ -> Error OverlappingSegments
                | [] -> segmentWithoutOverlapPrecheckWith left right options))

    let segment left right = segmentWith left right defaultOptions

    let private canonicalSubpathParameter subpath parameterValue =
        Subpath.parameterCanonicalize subpath parameterValue

    let private sortUniqueSubpathParameters (subpath: Subpath) (parameters: SubpathParameter list) =
        parameters
        |> List.fold (fun state parameterValue ->
            state
            |> Result.bind (fun found ->
                canonicalSubpathParameter subpath parameterValue
                |> Result.map (fun canonical ->
                    if List.contains canonical found then found else canonical :: found))) (Ok [])
        |> Result.map (List.sortBy (fun parameterValue -> parameterValue.SegmentIndex, parameterValue.T))

    let private insertSubpathIntersection
        (tolerance: float<length>)
        (point: Point<length>)
        (leftParameter: SubpathParameter)
        (rightParameter: SubpathParameter)
        (found: SubpathIntersection list) =
        let rec insert (accumulated: SubpathIntersection list) (remaining: SubpathIntersection list) =
            match remaining with
            | [] ->
                List.rev accumulated
                @ [ ({ Point = point
                       LeftParameters = [ leftParameter ]
                       RightParameters = [ rightParameter ] } : SubpathIntersection) ]
            | first :: rest when Point.distance first.Point point <= tolerance ->
                List.rev accumulated
                @ ({ first with
                        LeftParameters = leftParameter :: first.LeftParameters
                        RightParameters = rightParameter :: first.RightParameters } :: rest)
            | first :: rest -> insert (first :: accumulated) rest
        insert [] found

    let subpathWithoutOverlapPrecheckWith (left: Subpath) (right: Subpath) options =
        validateOptions options
        |> Result.bind (fun () ->
            let pairs =
                [ for leftIndex, leftSegment in List.indexed left.Segments do
                    for rightIndex, rightSegment in List.indexed right.Segments do
                        yield leftIndex, leftSegment, rightIndex, rightSegment ]
            pairs
            |> List.fold (fun state (leftIndex, leftSegment, rightIndex, rightSegment) ->
                state
                |> Result.bind (fun found ->
                    OverlapDetection.detect leftSegment rightSegment options.Tolerance
                    |> Result.bind (function
                        | _ :: _ -> Ok found
                        | [] ->
                            segmentWithoutOverlapPrecheckWith leftSegment rightSegment options
                            |> Result.map (fun intersections ->
                                intersections
                                |> List.fold (fun grouped intersection ->
                                    insertSubpathIntersection
                                        options.Tolerance
                                        intersection.Point
                                        { SegmentIndex = leftIndex; T = intersection.LeftT }
                                        { SegmentIndex = rightIndex; T = intersection.RightT }
                                        grouped) found)))) (Ok([]: SubpathIntersection list))
            |> Result.bind (fun grouped ->
                grouped
                |> List.fold (fun state (intersection: SubpathIntersection) ->
                    state
                    |> Result.bind (fun normalized ->
                        sortUniqueSubpathParameters left intersection.LeftParameters
                        |> Result.bind (fun leftParameters ->
                            sortUniqueSubpathParameters right intersection.RightParameters
                            |> Result.map (fun rightParameters ->
                                { intersection with
                                    LeftParameters = leftParameters
                                    RightParameters = rightParameters } :: normalized)))) (Ok([]: SubpathIntersection list))
                |> Result.map (List.sortBy (fun (intersection: SubpathIntersection) ->
                    intersection.LeftParameters
                    |> List.tryHead
                    |> Option.map (fun parameterValue -> parameterValue.SegmentIndex, parameterValue.T)
                    |> Option.defaultValue (System.Int32.MaxValue, 1.0<parameter>)))))

    let segmentSubpathWithoutOverlapPrecheckWith segmentValue subpathValue options =
        subpathWithoutOverlapPrecheckWith (Subpath.ofSegment segmentValue) subpathValue options
        |> Result.map (List.map (fun (intersection: SubpathIntersection) ->
            let segmentParameters = intersection.LeftParameters |> List.map (fun value -> value.T)
            let segmentParameter = segmentParameters |> List.tryHead |> Option.defaultValue 0.0<parameter>
            intersection.Point, segmentParameter, intersection.RightParameters))

    let pathWithoutOverlapPrecheckWith (left: Path) (right: Path) options =
        validateOptions options
        |> Result.bind (fun () ->
            let pairs =
                [ for leftIndex, leftSubpath in List.indexed left.Subpaths do
                    for rightIndex, rightSubpath in List.indexed right.Subpaths do
                        yield leftIndex, leftSubpath, rightIndex, rightSubpath ]
            let insertPath
                (intersection: SubpathIntersection)
                leftIndex
                rightIndex
                (found: PathIntersection list) =
                let lifted: PathIntersection =
                    { Point = intersection.Point
                      LeftParameters =
                        intersection.LeftParameters
                        |> List.map (fun at -> { SubpathIndex = leftIndex; At = at })
                      RightParameters =
                        intersection.RightParameters
                        |> List.map (fun at -> { SubpathIndex = rightIndex; At = at }) }
                match found |> List.tryFindIndex (fun existing -> Point.distance existing.Point lifted.Point <= options.Tolerance) with
                | None -> lifted :: found
                | Some index ->
                    found
                    |> List.mapi (fun current existing ->
                        if current <> index then existing
                        else
                            { existing with
                                LeftParameters = List.distinct (lifted.LeftParameters @ existing.LeftParameters)
                                RightParameters = List.distinct (lifted.RightParameters @ existing.RightParameters) })
            pairs
            |> List.fold (fun state (leftIndex, leftSubpath, rightIndex, rightSubpath) ->
                state
                |> Result.bind (fun found ->
                    subpathWithoutOverlapPrecheckWith leftSubpath rightSubpath options
                    |> Result.map (fun intersections ->
                        intersections
                        |> List.fold (fun grouped (intersection: SubpathIntersection) -> insertPath intersection leftIndex rightIndex grouped) found))) (Ok([]: PathIntersection list))
            |> Result.map (List.sortBy (fun (intersection: PathIntersection) ->
                intersection.LeftParameters
                |> List.tryHead
                |> Option.map (fun parameterValue ->
                    parameterValue.SubpathIndex,
                    parameterValue.At.SegmentIndex,
                    parameterValue.At.T)
                |> Option.defaultValue (System.Int32.MaxValue, System.Int32.MaxValue, 1.0<parameter>))))
