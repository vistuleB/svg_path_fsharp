namespace SvgPath

[<Struct>]
type ContainmentOptions =
    { Tolerance: float<length>
      Samples: int
      MaxIterations: int
      FallbackRayAngles: float<degree> list }

type PointContainment =
    | Inside
    | Outside
    | Boundary

type PathWinding =
    | Winding of int
    | BoundaryWinding

[<RequireQualifiedAccess>]
module WindingField =
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

    let private subpathBoundaryDistance (point: Point<length>) (subpath: Subpath) =
        let segments = Subpath.segments subpath
        let segmentDistance segment = Segment.distance segment point
        segments
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun best -> segmentDistance segment |> Result.map (min best)))
            (Ok(Length.fromFloat System.Double.PositiveInfinity))
        |> Result.map (fun best ->
            match segments with
            | [] -> best
            | _ -> min best (pointToLineDistance point (Subpath.finish subpath) (Subpath.start subpath)))

    // A positive crossing follows SVG's visual clockwise-positive convention.
    let private lineWinding
        (point: Point<length>)
        (startPoint: Point<length>)
        (endPoint: Point<length>) =
        let upward = startPoint.Y <= point.Y && endPoint.Y > point.Y
        let downward = endPoint.Y <= point.Y && startPoint.Y > point.Y
        if not upward && not downward then 0, 0
        else
            let side = Point.cross (Point.displacement startPoint endPoint) (Point.displacement startPoint point)
            if upward && side > 0.0<length^2> then 1, 1
            elif downward && side < 0.0<length^2> then -1, 1
            else 0, 0

    let private subpathWinding
        (point: Point<length>)
        (options: ContainmentOptions)
        (subpath: Subpath) =
        match Subpath.segments subpath with
        | [] -> Ok(0, 0)
        | _ ->
            let linearizeOptions: LinearizeOptions =
                { Tolerance = max 1.0e-10<length> (options.Tolerance / 4.0)
                  MaxDepth = options.MaxIterations }
            Subpath.toLinesWith linearizeOptions subpath
            |> Result.map (fun linearized ->
                let winding, crossings =
                    Subpath.segments linearized
                    |> List.fold (fun (winding, crossings) segment ->
                        let nextWinding, nextCrossings = lineWinding point (Segment.start segment) (Segment.finish segment)
                        winding + nextWinding, crossings + nextCrossings) (0, 0)
                let closingWinding, closingCrossings =
                    lineWinding point (Subpath.finish subpath) (Subpath.start subpath)
                winding + closingWinding, crossings + closingCrossings)

    let pathWindingWith (point: Point<length>) (path: Path) (options: ContainmentOptions) =
        validateOptions options
        |> Result.bind (fun () ->
            Path.subpaths path
            |> List.fold (fun state subpath ->
                state
                |> Result.bind (function
                    | BoundaryWinding -> Ok BoundaryWinding
                    | Winding winding ->
                        subpathBoundaryDistance point subpath
                        |> Result.bind (fun boundaryDistance ->
                            if boundaryDistance <= options.Tolerance then Ok BoundaryWinding
                            else subpathWinding point options subpath |> Result.map (fun (next, _) -> Winding(winding + next))))) (Ok(Winding 0)))

    let pathWinding point path = pathWindingWith point path defaultOptions

    let pathContainmentWith
        (point: Point<length>)
        (path: Path)
        (fillRule: FillRule)
        (options: ContainmentOptions) =
        validateOptions options
        |> Result.bind (fun () ->
            pathWindingWith point path options
            |> Result.bind (function
                | BoundaryWinding -> Ok Boundary
                | Winding winding ->
                    match fillRule with
                    | Nonzero -> Ok(if winding = 0 then Outside else Inside)
                    | EvenOdd ->
                        Path.subpaths path
                        |> List.fold (fun state subpath ->
                            state
                            |> Result.bind (fun crossings -> subpathWinding point options subpath |> Result.map (fun (_, next) -> crossings + next))) (Ok 0)
                        |> Result.map (fun crossings -> if crossings % 2 = 0 then Outside else Inside)))

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
