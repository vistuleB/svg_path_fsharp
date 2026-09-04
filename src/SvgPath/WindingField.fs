namespace SvgPath

[<Struct>]
/// Accuracy and fallback rays used for containment and winding calculations.
type ContainmentOptions =
    { Tolerance: float<length>
      Samples: int
      MaxIterations: int
      FallbackRayAngles: float<degree> list }

/// Classification of a point relative to filled path geometry.
type PointContainment =
    | Inside
    | Outside
    | Boundary

/// A path winding number, or an indication that the point lies on its boundary.
type PathWinding =
    | Winding of int
    | BoundaryWinding

type private ContainmentCalculation =
    | CalculatedBoundary
    | CalculatedWinding of winding: int * crossings: int

[<RequireQualifiedAccess>]
module internal WindingField =
    let defaultOptions =
        { Tolerance = 1.0e-9<length>
          Samples = 100
          MaxIterations = 100
          FallbackRayAngles =
            [ 0.0<degree>; 15.0<degree>; 30.0<degree>; 45.0<degree>
              60.0<degree>; 75.0<degree>; 90.0<degree>; 105.0<degree>
              120.0<degree>; 135.0<degree>; 150.0<degree>; 165.0<degree> ] }

    let validateOptions options =
        if options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidContainmentTolerance options.Tolerance)
        elif options.Samples <= 0 then Error(InvalidContainmentSamples options.Samples)
        elif options.MaxIterations <= 0 then Error(InvalidContainmentMaxIterations options.MaxIterations)
        else
            options.FallbackRayAngles
            |> List.tryFind (float >> System.Double.IsFinite >> not)
            |> function
                | Some angle -> Error(InvalidContainmentRayAngle angle)
                | None -> Ok()

    let private pointToLineDistance
        (point: Point<length>)
        (startPoint: Point<length>)
        (endPoint: Point<length>) =
        let chord = Point.displacement startPoint endPoint
        let length = Point.norm chord
        if length = 0.0<length> then Point.distance point startPoint
        else abs (Point.cross chord (Point.displacement startPoint point)) / length

    let private subpathBoundaryProjection
        (point: Point<length>)
        (subpath: Subpath)
        (options: ContainmentOptions) =
        let segments = Subpath.segments subpath
        segments
        |> List.indexed
        |> List.fold (fun state (index, segment) ->
            state
            |> Result.bind (fun best ->
                Segment.projectionWith segment point
                    { Samples = options.Samples
                      Tolerance = options.Tolerance
                      MaxIterations = options.MaxIterations }
                |> Result.map (fun (t, _, distance) ->
                    match best with
                    | None -> Some(distance, index, t)
                    | Some(bestDistance, _, _) when distance < bestDistance -> Some(distance, index, t)
                    | _ -> best))) (Ok None)

    [<Struct>]
    type private ContainmentRay =
        { Angle: float<degree>
          Direction: Point<1> }

    let private rayForAngle angle =
        { Angle = angle
          Direction = Point.create (Trig.cosDegrees angle) (Trig.sinDegrees angle) }

    let private projectionRay subpath projection =
        let fallback = rayForAngle 0.0<degree>
        match projection with
        | None -> fallback
        | Some(_, index, t) ->
            match Subpath.segments subpath |> List.tryItem index with
            | None -> fallback
            | Some segment ->
                match Segment.directions segment t with
                | Error _ -> fallback
                | Ok directions ->
                    let direction =
                        match directions.Incoming, directions.Outgoing with
                        | Some incoming, Some outgoing -> Some(Point.add incoming outgoing)
                        | Some direction, None
                        | None, Some direction -> Some direction
                        | None, None -> None
                    match direction with
                    | Some direction when abs direction.X >= abs direction.Y -> rayForAngle 90.0<degree>
                    | Some _ -> fallback
                    | None -> fallback

    let private oppositeRay ray =
        { Angle = ray.Angle + 180.0<degree>
          Direction = Point.scale -1.0 ray.Direction }

    let private crossingValue point candidate ray =
        Point.cross ray.Direction (Point.displacement point candidate)

    // A positive crossing follows SVG's visual clockwise-positive convention.
    let private lineWindingContribution
        (point: Point<length>)
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        ray =
        let startY = crossingValue point startPoint ray
        let endY = crossingValue point endPoint ray
        let side = Point.cross (Point.displacement startPoint endPoint) (Point.displacement startPoint point)
        if startY <= 0.0<length> then
            if endY > 0.0<length> && side > 0.0<length^2> then 1 else 0
        else if endY <= 0.0<length> && side < 0.0<length^2> then -1
        else 0

    let private crossingTransition before after =
        if before <= 0.0<length> && after > 0.0<length> then 1
        elif before > 0.0<length> && after <= 0.0<length> then -1
        else 0

    let private curvedCrossingContribution point segment ray t rayT =
        if rayT <= 0.0<length> then Ok 0
        else
            let t = max 0.0<parameter> (min 1.0<parameter> t)
            let rec probe width =
                Segment.point segment (max 0.0<parameter> (t - width))
                |> Result.bind (fun before ->
                    Segment.point segment (min 1.0<parameter> (t + width))
                    |> Result.bind (fun after ->
                        let contribution = crossingTransition (crossingValue point before ray) (crossingValue point after ray)
                        if contribution <> 0 || width >= 0.001<parameter> then Ok contribution
                        else probe (width * 10.0)))
            probe 1.0e-7<parameter>

    let private segmentBidirectionalContribution point segment ray (options: ContainmentOptions) =
        match segment with
        | Line(startPoint, endPoint) ->
            Ok(lineWindingContribution point startPoint endPoint ray,
               lineWindingContribution point startPoint endPoint (oppositeRay ray))
        | _ ->
            let crossingOptions: CrossingOptions =
                { Samples = options.Samples
                  SignedLineDistanceTolerance = options.Tolerance
                  MaxIterations = options.MaxIterations }
            Segment.rayCrossingsWith segment point ray.Direction crossingOptions
            |> Result.bind (fun crossings ->
                crossings
                |> List.fold (fun state (t, rayT) ->
                    state
                    |> Result.bind (fun (forward, backward) ->
                        if t < 0.0<parameter> || t > 1.0<parameter> then Ok(forward, backward)
                        elif rayT > 0.0<length> then
                            curvedCrossingContribution point segment ray t rayT
                            |> Result.map (fun contribution -> forward + contribution, backward)
                        elif rayT < 0.0<length> then
                            curvedCrossingContribution point segment (oppositeRay ray) t (-rayT)
                            |> Result.map (fun contribution -> forward, backward + contribution)
                        else Ok(forward, backward))) (Ok(0, 0)))

    let private crossingCount contribution = if contribution = 0 then 0 else 1

    let private bidirectionalSubpathWinding point subpath ray options =
        Subpath.segments subpath
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun (forwardWinding, forwardCrossings, backwardWinding, backwardCrossings) ->
                segmentBidirectionalContribution point segment ray options
                |> Result.map (fun (forward, backward) ->
                    forwardWinding + forward,
                    forwardCrossings + crossingCount forward,
                    backwardWinding + backward,
                    backwardCrossings + crossingCount backward))) (Ok(0, 0, 0, 0))
        |> Result.map (fun (forwardWinding, forwardCrossings, backwardWinding, backwardCrossings) ->
            let forwardClosing =
                lineWindingContribution point (Subpath.finish subpath) (Subpath.start subpath) ray
            let backwardClosing =
                lineWindingContribution point (Subpath.finish subpath) (Subpath.start subpath) (oppositeRay ray)
            forwardWinding + forwardClosing,
            forwardCrossings + crossingCount forwardClosing,
            backwardWinding + backwardClosing,
            backwardCrossings + crossingCount backwardClosing)

    let private subpathWinding
        (point: Point<length>)
        (options: ContainmentOptions)
        (subpath: Subpath)
        initialRay =
        match Subpath.segments subpath with
        | [] -> Ok(0, 0)
        | _ ->
            let rays = initialRay :: (options.FallbackRayAngles |> List.map rayForAngle)
            let rec tryRays remaining =
                match remaining with
                | [] -> Error InconsistentContainment
                | ray :: rest ->
                    bidirectionalSubpathWinding point subpath ray options
                    |> Result.bind (fun (forwardWinding, forwardCrossings, backwardWinding, backwardCrossings) ->
                        if forwardWinding = backwardWinding
                           && forwardCrossings % 2 = backwardCrossings % 2 then
                            Ok(forwardWinding, forwardCrossings)
                        else tryRays rest)
            tryRays rays

    let private subpathContainmentCalculation point subpath options =
        match Subpath.segments subpath with
        | [] -> Ok(CalculatedWinding(0, 0))
        | _ ->
            subpathBoundaryProjection point subpath options
            |> Result.bind (fun projection ->
                let segmentDistance =
                    projection
                    |> Option.map (fun (distance, _, _) -> distance)
                    |> Option.defaultValue (Length.fromFloat System.Double.PositiveInfinity)
                let closingDistance =
                    pointToLineDistance point (Subpath.finish subpath) (Subpath.start subpath)
                if min segmentDistance closingDistance <= options.Tolerance then
                    Ok CalculatedBoundary
                else
                    subpathWinding point options subpath (projectionRay subpath projection)
                    |> Result.map (fun (winding, crossings) -> CalculatedWinding(winding, crossings)))

    let private containmentFromWinding winding crossings fillRule =
        match fillRule with
        | Nonzero -> if winding = 0 then Outside else Inside
        | EvenOdd -> if crossings % 2 = 0 then Outside else Inside

    let private containmentFromCalculation calculation fillRule =
        match calculation with
        | CalculatedBoundary -> Boundary
        | CalculatedWinding(winding, crossings) -> containmentFromWinding winding crossings fillRule

    let subpathContainmentWith point subpath fillRule options =
        validateOptions options
        |> Result.bind (fun () ->
            subpathContainmentCalculation point subpath options
            |> Result.map (fun calculation -> containmentFromCalculation calculation fillRule))

    let subpathContainment point subpath fillRule =
        subpathContainmentWith point subpath fillRule defaultOptions

    let pathWindingWith (point: Point<length>) (path: Path) (options: ContainmentOptions) =
        validateOptions options
        |> Result.bind (fun () ->
            Path.subpaths path
            |> List.fold (fun state subpath ->
                state
                |> Result.bind (function
                    | BoundaryWinding -> Ok BoundaryWinding
                    | Winding winding ->
                        subpathContainmentCalculation point subpath options
                        |> Result.map (function
                            | CalculatedBoundary -> BoundaryWinding
                            | CalculatedWinding(next, _) -> Winding(winding + next)))) (Ok(Winding 0)))

    let pathWinding point path = pathWindingWith point path defaultOptions

    let pathContainmentWith
        (point: Point<length>)
        (path: Path)
        (fillRule: FillRule)
        (options: ContainmentOptions) =
        validateOptions options
        |> Result.bind (fun () ->
            Path.subpaths path
            |> List.fold (fun state subpath ->
                state
                |> Result.bind (function
                    | CalculatedBoundary -> Ok CalculatedBoundary
                    | CalculatedWinding(winding, crossings) ->
                        subpathContainmentCalculation point subpath options
                        |> Result.map (function
                            | CalculatedBoundary -> CalculatedBoundary
                            | CalculatedWinding(nextWinding, nextCrossings) ->
                                CalculatedWinding(winding + nextWinding, crossings + nextCrossings))))
                (Ok(CalculatedWinding(0, 0)))
            |> Result.map (fun calculation -> containmentFromCalculation calculation fillRule))

    let pathContainment point path fillRule = pathContainmentWith point path fillRule defaultOptions

    let nonzeroLevelAt point path options =
        pathWindingWith point path options
        |> Result.bind (function
            | Winding value -> Ok value
            | BoundaryWinding ->
                pathContainmentWith point path Nonzero options
                |> Result.map (function Inside | Boundary -> 1 | Outside -> 0))

    let private sampleSegmentSides
        (segment: Segment)
        (t: float<parameter>)
        (path: Path)
        (sideSamplingDistance: float<length>)
        (options: ContainmentOptions) =
        match Segment.point segment t, Segment.derivative segment t with
        | Error error, _
        | _, Error error -> Error error
        | Ok sample, Ok derivative ->
            let lengthSquared = Point.dot derivative derivative
            if lengthSquared <= 0.0<length^2 / parameter^2> || not (System.Double.IsFinite(float lengthSquared)) then Ok None
            else
                let derivativeLength: float<length / parameter> = sqrt lengthSquared
                let normal: Point<length> =
                    Point.create
                        (derivative.Y / derivativeLength * sideSamplingDistance)
                        (-derivative.X / derivativeLength * sideSamplingDistance)
                let left = Point.add sample normal
                let right = Point.subtract sample normal
                nonzeroLevelAt left path options
                |> Result.bind (fun leftLevel -> nonzeroLevelAt right path options |> Result.map (fun rightLevel -> Some(leftLevel, rightLevel)))

    let segmentSideNonzeroLevels
        (segment: Segment)
        (path: Path)
        (sideSamplingDistance: float<length>)
        (options: ContainmentOptions) =
        if sideSamplingDistance <= 0.0<length> || not (System.Double.IsFinite(float sideSamplingDistance)) then
            Error(InvalidContainmentTolerance sideSamplingDistance)
        else
            validateOptions options
            |> Result.bind (fun () ->
                sampleSegmentSides segment 0.5<parameter> path sideSamplingDistance options
                |> Result.bind (function
                    | Some levels -> Ok levels
                    | None ->
                        let rec tryPairs pairs =
                            match pairs with
                            | [] -> Error IndeterminateWindingSideLevels
                            | (before, after) :: rest ->
                                match sampleSegmentSides segment before path sideSamplingDistance options,
                                      sampleSegmentSides segment after path sideSamplingDistance options with
                                | Error error, _
                                | _, Error error -> Error error
                                | Ok(Some beforeLevels), Ok(Some afterLevels) when beforeLevels = afterLevels -> Ok beforeLevels
                                | Ok(Some _), Ok(Some _) -> Error InconsistentWindingSideLevels
                                | _ -> tryPairs rest
                        tryPairs
                            [ 0.25<parameter>, 0.75<parameter>
                              0.125<parameter>, 0.875<parameter>
                              0.375<parameter>, 0.625<parameter> ]))

[<AutoOpen>]
/// Public containment and winding operations attached to Subpath and Path.
module PathContainmentExtensions =
    type Subpath with
        /// Classifies a point using an explicit fill rule and containment options.
        static member containmentWith point subpath fillRule options =
            WindingField.subpathContainmentWith point subpath fillRule options

        /// Classifies a point using default containment options.
        static member containment point subpath fillRule =
            WindingField.subpathContainment point subpath fillRule

    type Path with
        /// Default containment and winding options.
        static member defaultContainmentOptions = WindingField.defaultOptions

        /// Computes path winding with explicit containment options.
        static member windingWith point path options =
            WindingField.pathWindingWith point path options

        /// Computes path winding with default containment options.
        static member winding point path = WindingField.pathWinding point path

        /// Classifies a point against a path with explicit options.
        static member containmentWith point path fillRule options =
            WindingField.pathContainmentWith point path fillRule options

        /// Classifies a point against a path with default options.
        static member containment point path fillRule =
            WindingField.pathContainment point path fillRule
