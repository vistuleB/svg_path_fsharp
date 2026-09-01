namespace SvgPath

type Segment =
    | Line of startPoint: Point<length> * endPoint: Point<length>
    | QuadraticBezier of startPoint: Point<length> * control: Point<length> * endPoint: Point<length>
    | CubicBezier of
        startPoint: Point<length> *
        control1: Point<length> *
        control2: Point<length> *
        endPoint: Point<length>
    | Arc of EndpointArcData

type EndpointPolicy =
    | Strict
    | Wiggle
    | WiggleWith of float<length>
    | Bridge
    | WiggleThenBridge
    | WiggleThenBridgeWith of float<length>
    | Custom of (Segment -> Segment -> bool -> Segment list)

[<Struct>]
type SubpathParameter =
    { SegmentIndex: int
      T: float<parameter> }

[<Struct>]
type PathParameter =
    { SubpathIndex: int
      At: SubpathParameter }

type SegmentError =
    | DegenerateArc
    | EmptySubpath
    | EmptyPath
    | SplitOutsideSegment
    | InvalidLinearizeTolerance of float<length>
    | InvalidLinearizeMaxDepth of int
    | LinearizeMaxDepthReached of float<length>
    | InvalidOverlapTolerance of float<length>
    | InvalidOverlapSamples of int
    | NonAffineOverlapCorrespondence
    | InvalidIntersectionTolerance of float<length>
    | InvalidIntersectionMaxDepth of int
    | IntersectionTerminalWindowLimitExceeded of int
    | OverlappingSegments
    | InternalOverlapParameterCorrespondenceInconsistency
    | InvalidContainmentTolerance of float<length>
    | InvalidContainmentSamples of int
    | InvalidContainmentMaxIterations of int
    | InvalidContainmentRayAngle of float<degree>
    | IndeterminateWindingSideLevels
    | InconsistentWindingSideLevels
    | CannotMapArcNonlinearly
    | InvalidSubpathParameter of segmentIndex: int * t: float<parameter> * length: int
    | InvalidPathParameter of subpathIndex: int * length: int
    | InvalidSubpathInterval of fromValue: SubpathParameter * toValue: SubpathParameter
    | InvalidSplice of start: int * delete: int * length: int
    | Discontinuous of
        previousIndex: int *
        nextIndex: int *
        expected: Point<length> *
        actual: Point<length> *
        distance: float<length>
    | AlreadyClosed
    | InvalidWiggleTolerance of float<length>

type PointMapError<'error> =
    | PointMappingError of 'error
    | PointMapSegmentError of SegmentError

[<Struct>]
type Subpath =
    private
        { startPoint: Point<length>
          segmentList: Segment list
          isClosed: bool }

    member this.Start = this.startPoint
    member this.Segments = this.segmentList
    member this.Closed = this.isClosed

[<Struct>]
type Path =
    private { subpathList: Subpath list }

    member this.Subpaths = this.subpathList

type FillRule =
    | Nonzero
    | EvenOdd

[<Struct>]
type BoundingBox =
    { Min: Point<length>
      Max: Point<length> }

[<RequireQualifiedAccess>]
module BoundingBox =
    let fromPoint point = { Min = point; Max = point }

    let union left right =
        { Min = Point.create (min left.Min.X right.Min.X) (min left.Min.Y right.Min.Y)
          Max = Point.create (max left.Max.X right.Max.X) (max left.Max.Y right.Max.Y) }

    let center box =
        Point.create ((box.Min.X + box.Max.X) / 2.0) ((box.Min.Y + box.Max.Y) / 2.0)

    let width box = box.Max.X - box.Min.X
    let height box = box.Max.Y - box.Min.Y
    let diameter box = Point.distance box.Min box.Max

    let unionMany boxes =
        match boxes with
        | [] -> None
        | first :: rest -> Some(List.fold union first rest)

    let ofPoints points =
        points |> List.map fromPoint |> unionMany

[<Struct>]
type LinearizeOptions =
    { Tolerance: float<length>
      MaxDepth: int }

[<RequireQualifiedAccess>]
module Segment =
    let defaultLinearizeOptions =
        { Tolerance = 0.01<length>
          MaxDepth = 20 }

    let start segment =
        match segment with
        | Line(startPoint, _)
        | QuadraticBezier(startPoint, _, _)
        | CubicBezier(startPoint, _, _, _) -> startPoint
        | Arc endpoint -> endpoint.Start

    let finish segment =
        match segment with
        | Line(_, endPoint)
        | QuadraticBezier(_, _, endPoint)
        | CubicBezier(_, _, _, endPoint) -> endPoint
        | Arc endpoint -> endpoint.End

    let boundingBox segment =
        match segment with
        | Line(startPoint, endPoint) ->
            Ok
                { Min = Point.create (min startPoint.X endPoint.X) (min startPoint.Y endPoint.Y)
                  Max = Point.create (max startPoint.X endPoint.X) (max startPoint.Y endPoint.Y) }
        | QuadraticBezier(startPoint, control, endPoint) ->
            let box = Bezier.boundingBox (QuadraticBezierData(startPoint, control, endPoint))
            Ok { Min = box.Min; Max = box.Max }
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let box = Bezier.boundingBox (CubicBezierData(startPoint, control1, control2, endPoint))
            Ok { Min = box.Min; Max = box.Max }
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.map (fun arc ->
                let box = Ellipse.arcBoundingBox arc
                { Min = box.Min; Max = box.Max })
            |> Result.mapError (fun _ -> DegenerateArc)

    let reverse segment =
        match segment with
        | Line(startPoint, endPoint) -> Line(endPoint, startPoint)
        | QuadraticBezier(startPoint, control, endPoint) -> QuadraticBezier(endPoint, control, startPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) -> CubicBezier(endPoint, control2, control1, startPoint)
        | Arc endpoint -> Arc { endpoint with Start = endpoint.End; End = endpoint.Start; Sweep = not endpoint.Sweep }

    let chordLength segment = Point.distance (start segment) (finish segment)
    let squaredChordLength segment = Point.squaredDistance (start segment) (finish segment)

    let withStart newStart segment =
        match segment with
        | Line(_, endPoint) -> Line(newStart, endPoint)
        | QuadraticBezier(_, control, endPoint) -> QuadraticBezier(newStart, control, endPoint)
        | CubicBezier(_, control1, control2, endPoint) -> CubicBezier(newStart, control1, control2, endPoint)
        | Arc endpoint -> Arc { endpoint with Start = newStart }

    let withFinish newFinish segment =
        match segment with
        | Line(startPoint, _) -> Line(startPoint, newFinish)
        | QuadraticBezier(startPoint, control, _) -> QuadraticBezier(startPoint, control, newFinish)
        | CubicBezier(startPoint, control1, control2, _) -> CubicBezier(startPoint, control1, control2, newFinish)
        | Arc endpoint -> Arc { endpoint with End = newFinish }

    let mapPoints mapping segment =
        match segment with
        | Line(startPoint, endPoint) -> Ok(Line(mapping startPoint, mapping endPoint))
        | QuadraticBezier(startPoint, control, endPoint) ->
            Ok(QuadraticBezier(mapping startPoint, mapping control, mapping endPoint))
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Ok(CubicBezier(mapping startPoint, mapping control1, mapping control2, mapping endPoint))
        | Arc _ -> Error CannotMapArcNonlinearly

    let tryMapPoints mapping segment =
        let mapped point = mapping point |> Result.mapError PointMappingError
        let bind2 constructor first second =
            mapped first |> Result.bind (fun mappedFirst -> mapped second |> Result.map (constructor mappedFirst))

        match segment with
        | Line(startPoint, endPoint) -> bind2 (fun a b -> Line(a, b)) startPoint endPoint
        | QuadraticBezier(startPoint, control, endPoint) ->
            mapped startPoint
            |> Result.bind (fun a ->
                mapped control
                |> Result.bind (fun b -> mapped endPoint |> Result.map (fun c -> QuadraticBezier(a, b, c))))
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            mapped startPoint
            |> Result.bind (fun a ->
                mapped control1
                |> Result.bind (fun b ->
                    mapped control2
                    |> Result.bind (fun c -> mapped endPoint |> Result.map (fun d -> CubicBezier(a, b, c, d)))))
        | Arc _ -> Error(PointMapSegmentError CannotMapArcNonlinearly)

    let arcsToCubicBeziers segment =
        match segment with
        | Arc endpoint ->
            match Ellipse.arcToCubics
                endpoint.Start endpoint.Radius endpoint.XAxisRotation
                endpoint.LargeArc endpoint.Sweep endpoint.End with
            | Ok cubics ->
                cubics
                |> List.map (fun cubic -> CubicBezier(cubic.Start, cubic.Control1, cubic.Control2, cubic.End))
            | Error _ ->
                [ CubicBezier(
                    endpoint.Start,
                    Point.interpolate endpoint.Start endpoint.End (Parameter.fromFloat (1.0 / 3.0)),
                    Point.interpolate endpoint.Start endpoint.End (Parameter.fromFloat (2.0 / 3.0)),
                    endpoint.End
                  ) ]
        | _ -> [ segment ]

    let private asBezier segment =
        match segment with
        | Line(startPoint, endPoint) -> LinearBezierData(startPoint, endPoint)
        | QuadraticBezier(startPoint, control, endPoint) -> QuadraticBezierData(startPoint, control, endPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            CubicBezierData(startPoint, control1, control2, endPoint)
        | Arc _ -> invalidArg (nameof segment) "arcs are not Bezier segments"

    let point segment t =
        match segment with
        | Arc endpoint -> Ellipse.endpointToCenter endpoint |> Result.map (fun arc -> Ellipse.arcPoint arc t) |> Result.mapError (fun _ -> DegenerateArc)
        | _ -> Ok(Bezier.point (asBezier segment) t)

    let private fromBezier curve =
        match curve with
        | LinearBezierData(startPoint, endPoint) -> Line(startPoint, endPoint)
        | QuadraticBezierData(startPoint, control, endPoint) -> QuadraticBezier(startPoint, control, endPoint)
        | CubicBezierData(startPoint, control1, control2, endPoint) -> CubicBezier(startPoint, control1, control2, endPoint)

    let rec betweenInside segment fromParameter toParameter =
        if fromParameter < 0.0<parameter> || fromParameter > 1.0<parameter>
           || toParameter < 0.0<parameter> || toParameter > 1.0<parameter> then Error SplitOutsideSegment
        elif fromParameter > toParameter then
            betweenInside segment toParameter fromParameter |> Result.map reverse
        elif fromParameter = toParameter then
            point segment fromParameter |> Result.map (fun sample -> Line(sample, sample))
        else
            match segment with
            | Arc endpoint ->
                Ellipse.endpointToCenter endpoint
                |> Result.mapError (fun _ -> DegenerateArc)
                |> Result.map (fun arc ->
                    let _, afterStart = Ellipse.splitArc arc fromParameter
                    let relative = (toParameter - fromParameter) / (1.0<parameter> - fromParameter) |> Parameter.fromFloat
                    let piece, _ = Ellipse.splitArc afterStart relative
                    Arc(Ellipse.centerToEndpoint piece))
            | _ ->
                let _, afterStart = Bezier.split (asBezier segment) fromParameter
                let relative = (toParameter - fromParameter) / (1.0<parameter> - fromParameter) |> Parameter.fromFloat
                let piece, _ = Bezier.split afterStart relative
                Ok(fromBezier piece)

    let split segment t =
        betweenInside segment 0.0<parameter> t
        |> Result.bind (fun left ->
            betweenInside segment t 1.0<parameter>
            |> Result.map (fun right -> left, right))

    let derivative segment t : Result<Point<length / parameter>, SegmentError> =
        match segment with
        | Arc endpoint -> Ellipse.endpointToCenter endpoint |> Result.map (fun arc -> Ellipse.arcDerivative arc t) |> Result.mapError (fun _ -> DegenerateArc)
        | _ -> Ok(Bezier.derivative (asBezier segment) t)

    let secondDerivative segment t : Result<Point<length / parameter^2>, SegmentError> =
        let perParameterSquared = 1.0<1 / parameter^2>
        match segment with
        | Line _ -> Ok(Point.create 0.0<length / parameter^2> 0.0<length / parameter^2>)
        | QuadraticBezier(startPoint, control, endPoint) ->
            Point.add
                (Point.displacement control endPoint)
                (Point.displacement control startPoint)
            |> Point.scale (2.0 * perParameterSquared)
            |> Ok
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let left =
                Point.add
                    (Point.displacement control1 startPoint)
                    (Point.displacement control1 control2)
            let right =
                Point.add
                    (Point.displacement control2 control1)
                    (Point.displacement control2 endPoint)
            Point.add
                (Point.scale (1.0 - Parameter.ratio t) left)
                (Point.scale (Parameter.ratio t) right)
            |> Point.scale (6.0 * perParameterSquared)
            |> Ok
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.map (fun arc -> Ellipse.arcSecondDerivative arc t)
            |> Result.mapError (fun _ -> DegenerateArc)

    let projection target sample =
        let candidates = [ 0 .. 64 ] |> List.map (fun index -> Parameter.fromFloat (float index / 64.0))
        let distanceSquared t = point target t |> Result.map (Point.squaredDistance sample)
        let rec refine t iterations =
            if iterations = 0 then t
            else
                match point target t, derivative target t, secondDerivative target t with
                | Ok curvePoint, Ok first, Ok second ->
                    let offset = Point.displacement sample curvePoint
                    let numerator = Point.dot offset first
                    let denominator = Point.dot first first + Point.dot offset second
                    if denominator = 0.0<length^2 / parameter^2> then t
                    else
                        let step = numerator / denominator
                        let next = max 0.0<parameter> (min 1.0<parameter> (t - step))
                        if abs (next - t) <= 1.0e-14<parameter> then next else refine next (iterations - 1)
                | _ -> t
        candidates
        |> List.fold (fun state t ->
            state
            |> Result.bind (fun evaluated ->
                distanceSquared t |> Result.map (fun distance -> (t, distance) :: evaluated))) (Ok [])
        |> Result.bind (fun evaluated ->
            let initial = evaluated |> List.minBy snd |> fst
            let t = refine initial 24
            point target t |> Result.map (fun projected -> t, projected, Point.distance sample projected))

    let distance target sample = projection target sample |> Result.map (fun (_, _, distance) -> distance)

    let private controlDistanceToChord startPoint endPoint control =
        let chord = Point.displacement startPoint endPoint
        let length = Point.norm chord
        if length = 0.0<length> then Point.distance startPoint control
        else abs (Point.cross chord (Point.displacement startPoint control)) / length

    let private bezierError curve =
        let startPoint, endPoint, controls =
            match curve with
            | LinearBezierData(startPoint, endPoint) -> startPoint, endPoint, []
            | QuadraticBezierData(startPoint, control, endPoint) -> startPoint, endPoint, [ control ]
            | CubicBezierData(startPoint, control1, control2, endPoint) -> startPoint, endPoint, [ control1; control2 ]
        controls
        |> List.map (controlDistanceToChord startPoint endPoint)
        |> List.fold max 0.0<length>

    let rec private linearizeBezier options depth curve =
        let error = bezierError curve
        if error <= options.Tolerance then Ok [ Line(Bezier.start curve, Bezier.finish curve) ]
        elif depth >= options.MaxDepth then Error(LinearizeMaxDepthReached error)
        else
            let left, right = Bezier.split curve (Parameter.fromFloat 0.5)
            match linearizeBezier options (depth + 1) left, linearizeBezier options (depth + 1) right with
            | Ok leftLines, Ok rightLines -> Ok(leftLines @ rightLines)
            | Error error, _
            | _, Error error -> Error error

    let toLinesWith options segment =
        if options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidLinearizeTolerance options.Tolerance)
        elif options.MaxDepth <= 0 then Error(InvalidLinearizeMaxDepth options.MaxDepth)
        else
            match segment with
            | Line _ -> Ok [ segment ]
            | QuadraticBezier _
            | CubicBezier _ -> linearizeBezier options 0 (asBezier segment)
            | Arc endpoint ->
                match Ellipse.arcToCubics endpoint.Start endpoint.Radius endpoint.XAxisRotation endpoint.LargeArc endpoint.Sweep endpoint.End with
                | Error _ -> Ok [ Line(endpoint.Start, endpoint.End) ]
                | Ok cubics ->
                    cubics
                    |> List.fold (fun state cubic ->
                        state
                        |> Result.bind (fun lines ->
                            linearizeBezier options 0 (CubicBezierData(cubic.Start, cubic.Control1, cubic.Control2, cubic.End))
                            |> Result.map (fun next -> lines @ next))) (Ok [])

    let toLines segment = toLinesWith defaultLinearizeOptions segment

[<RequireQualifiedAccess>]
module Subpath =
    let private defaultWiggleTolerance = 1.0e-9<length>

    let empty startPoint =
        { startPoint = startPoint
          segmentList = []
          isClosed = false }

    let ofSegment segment =
        { startPoint = Segment.start segment
          segmentList = [ segment ]
          isClosed = false }

    let private discontinuity previousIndex nextIndex expected actual =
        Discontinuous(previousIndex, nextIndex, expected, actual, Point.distance expected actual)

    let private validateFrom startPoint segments =
        match segments with
        | [] ->
            Ok
                { startPoint = startPoint
                  segmentList = []
                  isClosed = false }
        | first :: _ when Segment.start first <> startPoint ->
            Error(discontinuity -1 0 startPoint (Segment.start first))
        | _ ->
            segments
            |> List.pairwise
            |> List.indexed
            |> List.tryPick (fun (index, (previous, next)) ->
                let expected = Segment.finish previous
                let actual = Segment.start next
                if expected = actual then None
                else Some(discontinuity index (index + 1) expected actual))
            |> function
                | Some error -> Error error
                | None ->
                    Ok
                        { startPoint = startPoint
                          segmentList = segments
                          isClosed = false }

    let private shiftDiscontinuity offset error =
        match error with
        | Discontinuous(previousIndex, nextIndex, expected, actual, distance) ->
            Discontinuous(previousIndex + offset, nextIndex + offset, expected, actual, distance)
        | other -> other

    let private validateReplacement previousIndex previous replacement =
        match replacement with
        | [] -> Ok []
        | _ ->
            validateFrom (Segment.start previous) replacement
            |> Result.map (fun subpath -> subpath.Segments)
            |> Result.mapError (shiftDiscontinuity previousIndex)

    let rec private reconcileCustom remaining reversedAccumulated previousIndex reconcile =
        match reversedAccumulated, remaining with
        | [], [] -> Ok []
        | [], next :: rest -> reconcileCustom rest [ next ] previousIndex reconcile
        | previous :: before, [] -> Ok(List.rev (previous :: before))
        | previous :: before, next :: rest ->
            validateReplacement previousIndex previous (reconcile previous next false)
            |> Result.bind (fun replacement ->
                let accumulated = (List.rev replacement) @ before
                reconcileCustom rest accumulated (previousIndex + List.length replacement - 1) reconcile)

    let private strictReconcile previous next closing =
        if closing then [ previous ] else [ previous; next ]

    let private bridgeReconcile previous next closing =
        let previousEnd = Segment.finish previous
        let nextStart = Segment.start next
        if previousEnd = nextStart then strictReconcile previous next closing
        else
            let bridge = Line(previousEnd, nextStart)
            if closing then [ previous; bridge ] else [ previous; bridge; next ]

    let private isHorizontal segment =
        match segment with
        | Line(startPoint, endPoint) -> startPoint.Y = endPoint.Y
        | _ -> false

    let private isVertical segment =
        match segment with
        | Line(startPoint, endPoint) -> startPoint.X = endPoint.X
        | _ -> false

    let private wiggleNearby previous next closing =
        let previousEnd = Segment.finish previous
        let nextStart = Segment.start next
        let sameAxisMisalignment =
            match previous, next with
            | Line(previousStart, _), Line(_, nextEnd) ->
                (previousStart.Y = previousEnd.Y && nextStart.Y = nextEnd.Y && previousEnd.Y <> nextStart.Y)
                || (previousStart.X = previousEnd.X && nextStart.X = nextEnd.X && previousEnd.X <> nextStart.X)
            | _ -> false

        if sameAxisMisalignment then bridgeReconcile previous next closing
        elif closing then [ Segment.withFinish nextStart previous ]
        else
            let joinX =
                if isVertical previous then previousEnd.X
                elif isVertical next then nextStart.X
                else (previousEnd.X + nextStart.X) / 2.0
            let joinY =
                if isHorizontal previous then previousEnd.Y
                elif isHorizontal next then nextStart.Y
                else (previousEnd.Y + nextStart.Y) / 2.0
            let join = Point.create joinX joinY
            [ Segment.withFinish join previous; Segment.withStart join next ]

    let private wiggleReconcile tolerance previous next closing =
        if Point.distance (Segment.finish previous) (Segment.start next) <= tolerance then
            wiggleNearby previous next closing
        else strictReconcile previous next closing

    let private wiggleThenBridgeReconcile tolerance previous next closing =
        if Point.distance (Segment.finish previous) (Segment.start next) <= tolerance then
            wiggleNearby previous next closing
        else bridgeReconcile previous next closing

    let private validatePolicy policy =
        match policy with
        | WiggleWith tolerance
        | WiggleThenBridgeWith tolerance when float tolerance < 0.0 || not (System.Double.IsFinite(float tolerance)) ->
            Error(InvalidWiggleTolerance tolerance)
        | _ -> Ok()

    let private policyReconcile policy =
        match policy with
        | Strict -> strictReconcile
        | Wiggle -> wiggleReconcile defaultWiggleTolerance
        | WiggleWith tolerance -> wiggleReconcile tolerance
        | Bridge -> bridgeReconcile
        | WiggleThenBridge -> wiggleThenBridgeReconcile defaultWiggleTolerance
        | WiggleThenBridgeWith tolerance -> wiggleThenBridgeReconcile tolerance
        | Custom reconcile -> reconcile

    /// Construct an open subpath while validating every endpoint-policy replacement.
    let createWith policy segments =
        validatePolicy policy
        |> Result.bind (fun _ ->
            match segments with
            | [] -> Error EmptySubpath
            | first :: _ ->
                reconcileCustom segments [] 0 (policyReconcile policy)
                |> Result.bind (function
                    | [] -> Ok(empty (Segment.start first))
                    | reconciled -> validateFrom (Segment.start (List.head reconciled)) reconciled))

    /// Construct a strictly continuous open subpath.
    let create segments = createWith Strict segments

    let isClosed (subpath: Subpath) = subpath.Closed

    let private validateClosed startPoint segments =
        validateFrom startPoint segments
        |> Result.bind (fun openSubpath ->
            match List.tryLast openSubpath.segmentList with
            | None -> Ok { openSubpath with isClosed = true }
            | Some last when Segment.finish last = startPoint -> Ok { openSubpath with isClosed = true }
            | Some last ->
                Error(
                    discontinuity
                        (List.length openSubpath.segmentList - 1)
                        0
                        startPoint
                        (Segment.finish last)
                ))

    /// Set semantic closure while validating any custom replacement.
    let setClosedWith policy closed subpath =
        validatePolicy policy
        |> Result.bind (fun _ ->
            if not closed then Ok { subpath with isClosed = false }
            elif subpath.isClosed then Error AlreadyClosed
            else
                let reconcile = policyReconcile policy
                match subpath.segmentList with
                | [] -> Ok { subpath with isClosed = true }
                | [ only ] ->
                    validateReplacement 0 only (reconcile only only true)
                    |> Result.bind (validateClosed subpath.startPoint)
                | first :: rest ->
                    let last = List.last rest
                    let middle = rest |> List.take (List.length rest - 1)
                    validateReplacement 0 last (reconcile last first true)
                    |> Result.bind (fun replacement -> validateClosed subpath.startPoint (first :: (middle @ replacement))))

    let setClosed closed subpath = setClosedWith Strict closed subpath

    let rebuildWith policy subpath =
        match subpath.segmentList with
        | [] -> Ok subpath
        | segments ->
            createWith policy segments
            |> Result.bind (fun rebuilt ->
                if subpath.isClosed then setClosedWith policy true rebuilt
                else Ok rebuilt)

    let polyline points =
        match points with
        | [] | [ _ ] -> Error EmptySubpath
        | _ -> points |> List.pairwise |> List.map Line |> create

    let polygon points =
        match points with
        | [] | [ _ ] -> Error EmptySubpath
        | first :: _ ->
            let closedPoints =
                if List.last points = first then points else points @ [ first ]
            polyline closedPoints |> Result.bind (setClosed true)

    let normalizeZeroLengthLines subpath =
        let isZeroLengthLine = function
            | Line(startPoint, endPoint) -> startPoint = endPoint
            | _ -> false
        let cleaned = List.filter (isZeroLengthLine >> not) subpath.segmentList
        match cleaned, subpath.segmentList with
        | [], [] -> subpath
        | [], first :: _ -> { subpath with startPoint = Segment.start first; segmentList = [ first ] }
        | first :: _, _ -> { subpath with startPoint = Segment.start first; segmentList = cleaned }

    let spliceWith policy startIndex deleteCount inserted subpath =
        let length = List.length subpath.segmentList
        if startIndex < 0 || deleteCount < 0 || startIndex > length then
            Error(InvalidSplice(startIndex, deleteCount, length))
        else
            let prefix = List.take startIndex subpath.segmentList
            let suffix = subpath.segmentList |> List.skip (min length (startIndex + deleteCount))
            let edited = prefix @ inserted @ suffix
            match edited with
            | [] -> Ok { subpath with segmentList = [] }
            | _ ->
                createWith policy edited
                |> Result.bind (fun rebuilt ->
                    if subpath.isClosed then setClosedWith policy true rebuilt else Ok rebuilt)

    let splice startIndex deleteCount inserted subpath =
        spliceWith Strict startIndex deleteCount inserted subpath

    let reverse subpath =
        match subpath.segmentList with
        | [] -> subpath
        | segments ->
            let reversed = segments |> List.rev |> List.map Segment.reverse
            { startPoint = Segment.start (List.head reversed)
              segmentList = reversed
              isClosed = subpath.isClosed }

    let mapPoints mapping subpath =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun mapped ->
                Segment.mapPoints mapping segment |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.bind (fun reversed ->
            match List.rev reversed with
            | [] -> Ok(empty (mapping subpath.startPoint))
            | segments ->
                create segments
                |> Result.bind (fun mapped ->
                    if subpath.isClosed then setClosed true mapped else Ok mapped))

    let tryMapPoints mapping subpath =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun mapped ->
                Segment.tryMapPoints mapping segment |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.bind (fun reversed ->
            match List.rev reversed with
            | [] ->
                mapping subpath.startPoint
                |> Result.mapError PointMappingError
                |> Result.map empty
            | segments ->
                create segments
                |> Result.mapError PointMapSegmentError
                |> Result.bind (fun mapped ->
                    if subpath.isClosed then
                        setClosed true mapped |> Result.mapError PointMapSegmentError
                    else Ok mapped))

    let arcsToCubicBeziers subpath =
        { subpath with
            segmentList = subpath.segmentList |> List.collect Segment.arcsToCubicBeziers }

    let segments subpath = subpath.segmentList
    let start subpath = subpath.startPoint
    let finish subpath = subpath.segmentList |> List.tryLast |> Option.map Segment.finish |> Option.defaultValue subpath.startPoint

    let appendWith policy segment subpath =
        if subpath.isClosed then Error AlreadyClosed
        else
            match subpath.segmentList with
            | _ :: _ -> createWith policy (subpath.segmentList @ [ segment ])
            | [] ->
                let actual = Segment.start segment
                let distance = Point.distance subpath.startPoint actual
                let rebuilt =
                    match policy with
                    | Strict
                    | Custom _ ->
                        if actual = subpath.startPoint then Ok [ segment ]
                        else Error(discontinuity -1 0 subpath.startPoint actual)
                    | Wiggle
                    | WiggleWith _ when distance <= (match policy with WiggleWith value -> value | _ -> defaultWiggleTolerance) ->
                        Ok [ Segment.withStart subpath.startPoint segment ]
                    | WiggleThenBridge
                    | WiggleThenBridgeWith _ when distance <= (match policy with WiggleThenBridgeWith value -> value | _ -> defaultWiggleTolerance) ->
                        Ok [ Segment.withStart subpath.startPoint segment ]
                    | Bridge
                    | WiggleThenBridge
                    | WiggleThenBridgeWith _ -> Ok [ Line(subpath.startPoint, actual); segment ]
                    | Wiggle
                    | WiggleWith _ -> Error(discontinuity -1 0 subpath.startPoint actual)
                validatePolicy policy
                |> Result.bind (fun _ -> rebuilt)
                |> Result.bind (validateFrom subpath.startPoint)

    let append segment subpath = appendWith Strict segment subpath
    let boundingBox subpath =
        match subpath.segmentList with
        | [] -> Error EmptySubpath
        | first :: rest ->
            Segment.boundingBox first
            |> Result.bind (fun initial ->
                rest
                |> List.fold (fun state segment ->
                    state
                    |> Result.bind (fun box ->
                        Segment.boundingBox segment
                        |> Result.map (BoundingBox.union box))) (Ok initial))
    let parameterCanonicalize subpath parameter =
        let length = List.length subpath.segmentList
        if length = 0 then Error EmptySubpath
        elif parameter.SegmentIndex < 0 || parameter.SegmentIndex >= length
             || parameter.T < 0.0<parameter> || parameter.T > 1.0<parameter> then
            Error(InvalidSubpathParameter(parameter.SegmentIndex, parameter.T, length))
        elif parameter.T = 1.0<parameter> && parameter.SegmentIndex + 1 < length then
            Ok { SegmentIndex = parameter.SegmentIndex + 1; T = 0.0<parameter> }
        elif subpath.isClosed && parameter.T = 1.0<parameter> && parameter.SegmentIndex = length - 1 then
            Ok { SegmentIndex = 0; T = 0.0<parameter> }
        else Ok parameter

    let parameterFromEnd subpath segmentIndex t =
        let length = List.length subpath.segmentList
        if length = 0 then Error EmptySubpath
        elif segmentIndex < 0 || segmentIndex >= length || t < 0.0<parameter> || t > 1.0<parameter> then
            Error(InvalidSubpathParameter(segmentIndex, t, length))
        else
            Ok
                { SegmentIndex = length - 1 - segmentIndex
                  T = 1.0<parameter> - t }

    let point subpath parameterValue =
        parameterCanonicalize subpath parameterValue
        |> Result.bind (fun canonical -> Segment.point subpath.segmentList[canonical.SegmentIndex] canonical.T)

    let derivative subpath parameterValue =
        parameterCanonicalize subpath parameterValue
        |> Result.bind (fun canonical -> Segment.derivative subpath.segmentList[canonical.SegmentIndex] canonical.T)

    let secondDerivative subpath parameterValue =
        parameterCanonicalize subpath parameterValue
        |> Result.bind (fun canonical -> Segment.secondDerivative subpath.segmentList[canonical.SegmentIndex] canonical.T)
    let toLinesWith options subpath =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun lines -> Segment.toLinesWith options segment |> Result.map (fun next -> lines @ next))) (Ok [])
        |> Result.map (fun segments -> { subpath with segmentList = segments })

[<RequireQualifiedAccess>]
module Path =
    let empty = { subpathList = [] }
    let ofSubpaths subpaths = { subpathList = subpaths }
    let subpaths path = path.subpathList
    let singleton subpath = { subpathList = [ subpath ] }
    let append subpath path = { subpathList = path.subpathList @ [ subpath ] }
    let combine paths = { subpathList = paths |> List.collect (fun path -> path.subpathList) }
    let mapSubpaths mapping path = { subpathList = List.map mapping path.subpathList }
    let filterSubpaths predicate path = { subpathList = List.filter predicate path.subpathList }
    let reverse path = { subpathList = path.subpathList |> List.map Subpath.reverse }

    let rebuildWith policy path =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun rebuilt ->
                Subpath.rebuildWith policy subpath |> Result.map (fun next -> next :: rebuilt))) (Ok [])
        |> Result.map (fun reversed -> { subpathList = List.rev reversed })

    let mapPoints mapping path =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun mapped ->
                Subpath.mapPoints mapping subpath |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.map (fun reversed -> { subpathList = List.rev reversed })

    let tryMapPoints mapping path =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun mapped ->
                Subpath.tryMapPoints mapping subpath |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.map (fun reversed -> { subpathList = List.rev reversed })

    let arcsToCubicBeziers path =
        { subpathList = path.subpathList |> List.map Subpath.arcsToCubicBeziers }

    let start path =
        match path.subpathList with
        | [] -> Error EmptyPath
        | first :: _ -> Ok(Subpath.start first)

    let finish path =
        match List.tryLast path.subpathList with
        | None -> Error EmptyPath
        | Some last -> Ok(Subpath.finish last)

    let point path parameterValue =
        if parameterValue.SubpathIndex < 0 || parameterValue.SubpathIndex >= List.length path.subpathList then
            Error(InvalidPathParameter(parameterValue.SubpathIndex, List.length path.subpathList))
        else
            Subpath.point path.subpathList[parameterValue.SubpathIndex] parameterValue.At

    let derivative path parameterValue =
        if parameterValue.SubpathIndex < 0 || parameterValue.SubpathIndex >= List.length path.subpathList then
            Error(InvalidPathParameter(parameterValue.SubpathIndex, List.length path.subpathList))
        else
            Subpath.derivative path.subpathList[parameterValue.SubpathIndex] parameterValue.At

    let secondDerivative path parameterValue =
        if parameterValue.SubpathIndex < 0 || parameterValue.SubpathIndex >= List.length path.subpathList then
            Error(InvalidPathParameter(parameterValue.SubpathIndex, List.length path.subpathList))
        else
            Subpath.secondDerivative path.subpathList[parameterValue.SubpathIndex] parameterValue.At

    let boundingBox path =
        match path.subpathList with
        | [] -> Error EmptyPath
        | first :: rest ->
            Subpath.boundingBox first
            |> Result.bind (fun initial ->
                rest
                |> List.fold (fun state subpath ->
                    state
                    |> Result.bind (fun box ->
                        Subpath.boundingBox subpath
                        |> Result.map (BoundingBox.union box))) (Ok initial))

    let toLinesWith options path =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun subpaths -> Subpath.toLinesWith options subpath |> Result.map (fun next -> next :: subpaths))) (Ok [])
        |> Result.map (fun subpaths -> { subpathList = List.rev subpaths })
