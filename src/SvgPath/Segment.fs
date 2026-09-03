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

[<Struct>]
type SubpathProjection =
    { At: SubpathParameter
      Point: Point<length>
      Distance: float<length> }

[<Struct>]
type PathProjection =
    { At: PathParameter
      Point: Point<length>
      Distance: float<length> }

type SegmentError =
    | DegenerateArc
    | EmptySubpath
    | EmptySubpaths
    | EmptyPath
    | MultipleNonemptySubpaths
    | NotClosed
    | SplitOutsideSegment
    | InvalidLinearizeTolerance of float<length>
    | InvalidLinearizeMaxDepth of int
    | LinearizeMaxDepthReached of float<length>
    | InvalidOverlapTolerance of float<length>
    | InvalidOverlapSamples of int
    | NonAffineOverlapCorrespondence
    | InvalidIntersectionTolerance of float<length>
    | InvalidIntersectionMaxDepth of int
    | InvalidIntersectionParameterSnapExponent of int
    | IntersectionTerminalWindowLimitExceeded of int
    | OverlappingSegments
    | InternalOverlapClassificationInconsistency
    | InternalUncertifiedSegmentIntersection of
        leftDistance: float<length> * rightDistance: float<length> * tolerance: float<length>
    | InvalidSelfIntersectionMinimumArcLengthSeparation of float<length>
    | InvalidSelfIntersectionDistanceTolerance of float<length>
    | InternalOverlapParameterCorrespondenceInconsistency
    | InvalidCrossingTolerance of float<length>
    | InvalidCrossingSamples of int
    | InvalidCrossingMaxIterations of int
    | CrossingMaxIterationsReached of estimate: float<parameter> * value: float<length>
    | IndeterminateDirection
    | InconsistentContainment
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
    | InvalidDirectionRelativeTolerance of float
    | InvalidLengthTolerance of float<length>
    | InvalidLengthMaxDepth of int
    | InvalidLengthDistance of distance: float<length> * segmentLength: float<length>
    | InvalidZeroLengthTolerance of float<length>
    | InvalidSubdivisionMaxLength of float<length>
    | InvalidMinimizeSamples of int
    | InvalidMinimizeTolerance of float<parameter>
    | InvalidMinimizeMaxIterations of int
    | MinimizeMaxIterationsReached of estimate: float<parameter> * value: float
    | DegeneratePointPairSimilarity
    | InvalidParametricTolerance of float<length>
    | InvalidParametricSamplesPerPiece of int
    | InvalidParametricInitialPieceCount of int
    | InvalidParametricMaxDepth of int
    | InvalidParametricInterval of startValue: float * endValue: float
    | NonFiniteParametricPoint of parameterValue: float * point: Point<length>
    | NonFiniteParametricTangent of parameterValue: float * tangent: Point<length>
    | ParametricMaxDepthReached of float<length>
    | ParametricFitFailed
    | InvalidDistanceTolerance of float<length>
    | InvalidDistanceSamples of int
    | InvalidDistanceMaxIterations of int
    | DistanceMaxIterationsReached of estimate: float<parameter> * value: float<length^2 / parameter>
    | DistanceRootIsolationFailed

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
type Directions =
    { Incoming: Point<1> option
      Outgoing: Point<1> option }

[<Struct>]
type DirectionOptions =
    { RelativeTolerance: float }

[<Struct>]
type LengthOptions =
    { Tolerance: float<length>
      MaxDepth: int }

[<Struct>]
type MinimizeOptions =
    { Samples: int
      ParameterTolerance: float<parameter>
      MaxIterations: int }

[<Struct>]
type ParametricOptions =
    { Tolerance: float<length>
      SamplesPerPiece: int
      InitialPieceCount: int
      MaxDepth: int
      Tangent: (float -> Point<length>) option }

[<Struct>]
type DistanceOptions =
    { Samples: int
      Tolerance: float<length>
      MaxIterations: int }

[<Struct>]
type CrossingOptions =
    { Samples: int
      SignedLineDistanceTolerance: float<length>
      MaxIterations: int }

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
    let diameter box = width box + height box

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
    let arcFromEndpointData data = Arc data

    let arcFromCenterData data =
        data |> Ellipse.centerToEndpoint |> Arc

    let private curveParameterTolerance = 1.0e-9<parameter>

    let defaultLinearizeOptions =
        { Tolerance = 0.01<length>
          MaxDepth = 20 }

    let start segment =
        match segment with
        | Line(startPoint, _)
        | QuadraticBezier(startPoint, _, _)
        | CubicBezier(startPoint, _, _, _) -> startPoint
        | Arc endpoint -> endpoint.Start

    let asSubpath segment =
        { startPoint = start segment
          segmentList = [ segment ]
          isClosed = false }

    let asPath segment = { subpathList = [ asSubpath segment ] }

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
    let ``end`` segment = finish segment
    let chordLengthSquared segment = squaredChordLength segment

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

    let private similarityPoint transform sourceStart sourceEnd targetStart targetEnd point =
        if point = sourceStart then targetStart
        elif point = sourceEnd then targetEnd
        else Affine.point transform point

    let byPointPairSimilarity segment sourceStart sourceEnd targetStart targetEnd =
        Affine.pointPairSimilarity sourceStart sourceEnd targetStart targetEnd
        |> Result.mapError (fun _ -> DegeneratePointPairSimilarity)
        |> Result.bind (fun transform ->
            let map = similarityPoint transform sourceStart sourceEnd targetStart targetEnd
            match segment with
            | Line(a, b) -> Ok(Line(map a, map b))
            | QuadraticBezier(a, b, c) -> Ok(QuadraticBezier(map a, map b, map c))
            | CubicBezier(a, b, c, d) -> Ok(CubicBezier(map a, map b, map c, map d))
            | Arc endpoint ->
                let scale = sqrt (transform.A * transform.A + transform.B * transform.B)
                if scale = 0.0 then Error CannotMapArcNonlinearly
                else
                    Ok(Arc
                        { endpoint with
                            Start = map endpoint.Start
                            Radius = Point.create (abs (scale * endpoint.Radius.X)) (abs (scale * endpoint.Radius.Y))
                            XAxisRotation = endpoint.XAxisRotation + Trig.atan2Degrees transform.B transform.A
                            End = map endpoint.End }))

    let remapEndpoints segment newStart newEnd =
        byPointPairSimilarity segment (start segment) (finish segment) newStart newEnd
        |> Result.map (withStart newStart >> withFinish newEnd)

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

    let toCubicBeziers segment =
        match segment with
        | Line(startPoint, endPoint) ->
            [ CubicBezier(
                startPoint,
                Point.interpolate startPoint endPoint (Parameter.fromFloat (1.0 / 3.0)),
                Point.interpolate startPoint endPoint (Parameter.fromFloat (2.0 / 3.0)),
                endPoint) ]
        | QuadraticBezier(startPoint, control, endPoint) ->
            [ CubicBezier(
                startPoint,
                Point.interpolate startPoint control (Parameter.fromFloat (2.0 / 3.0)),
                Point.interpolate endPoint control (Parameter.fromFloat (2.0 / 3.0)),
                endPoint) ]
        | CubicBezier _ -> [ segment ]
        | Arc _ -> arcsToCubicBeziers segment

    let private asBezier segment =
        match segment with
        | Line(startPoint, endPoint) -> LinearBezierData(startPoint, endPoint)
        | QuadraticBezier(startPoint, control, endPoint) -> QuadraticBezierData(startPoint, control, endPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            CubicBezierData(startPoint, control1, control2, endPoint)
        | Arc _ -> invalidArg (nameof segment) "arcs are not Bezier segments"

    let arcCenterData segment =
        match segment with
        | Arc endpoint -> Ellipse.endpointToCenter endpoint |> Result.mapError (fun _ -> DegenerateArc)
        | _ -> Error DegenerateArc

    let arcPoint segment t = arcCenterData segment |> Result.map (fun arc -> Ellipse.arcPoint arc t)
    let arcDerivative segment t = arcCenterData segment |> Result.map (fun arc -> Ellipse.arcDerivative arc t)
    let arcPointAtAngle segment angle = arcCenterData segment |> Result.map (fun arc -> Ellipse.arcPointAtAngle arc angle)
    let arcDerivativeAtAngle segment angle = arcCenterData segment |> Result.map (fun arc -> Ellipse.arcDerivativeAtAngle arc angle)
    let arcAngleAt segment t = arcCenterData segment |> Result.map (fun arc -> Ellipse.angleAt arc t)
    let arcEndAngle segment = arcCenterData segment |> Result.map Ellipse.arcEndAngle

    let point segment t =
        match segment with
        | Arc endpoint -> Ellipse.endpointToCenter endpoint |> Result.map (fun arc -> Ellipse.arcPoint arc t) |> Result.mapError (fun _ -> DegenerateArc)
        | _ -> Ok(Bezier.point (asBezier segment) t)

    let private fromBezier curve =
        match curve with
        | LinearBezierData(startPoint, endPoint) -> Line(startPoint, endPoint)
        | QuadraticBezierData(startPoint, control, endPoint) -> QuadraticBezier(startPoint, control, endPoint)
        | CubicBezierData(startPoint, control1, control2, endPoint) -> CubicBezier(startPoint, control1, control2, endPoint)

    let private splitUnchecked segment t =
        match segment with
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.mapError (fun _ -> DegenerateArc)
            |> Result.map (fun arc ->
                let leftArc, rightArc = Ellipse.splitArc arc t
                let splitPoint = Ellipse.arcPoint arc t
                let left = Arc(Ellipse.centerToEndpoint leftArc) |> withStart endpoint.Start |> withFinish splitPoint
                let right = Arc(Ellipse.centerToEndpoint rightArc) |> withStart splitPoint |> withFinish endpoint.End
                left, right)
        | _ ->
            let left, right = Bezier.split (asBezier segment) t
            Ok(fromBezier left, fromBezier right)

    let split segment t = splitUnchecked segment t

    let splitInside segment t =
        if t < 0.0<parameter> || t > 1.0<parameter> then Error SplitOutsideSegment
        else splitUnchecked segment t

    let rec between segment fromParameter toParameter =
        if fromParameter > toParameter then
            between segment toParameter fromParameter |> Result.map reverse
        elif fromParameter = toParameter then
            point segment fromParameter |> Result.map (fun sample -> Line(sample, sample))
        elif fromParameter = 1.0<parameter> then
            between (reverse segment) (1.0<parameter> - toParameter) 0.0<parameter>
            |> Result.map reverse
        else
            splitUnchecked segment fromParameter
            |> Result.bind (fun (_, afterStart) ->
                let relative = Parameter.fromFloat (float ((toParameter - fromParameter) / (1.0<parameter> - fromParameter)))
                splitUnchecked afterStart relative
                |> Result.bind (fun (piece, _) ->
                    point segment fromParameter
                    |> Result.bind (fun exactStart ->
                        point segment toParameter
                        |> Result.map (fun exactEnd -> piece |> withStart exactStart |> withFinish exactEnd))))

    let betweenInside segment fromParameter toParameter =
        if fromParameter < 0.0<parameter> || fromParameter > 1.0<parameter>
           || toParameter < 0.0<parameter> || toParameter > 1.0<parameter> then Error SplitOutsideSegment
        else between segment fromParameter toParameter

    let betweenMany segment parameters =
        parameters
        |> List.pairwise
        |> List.fold (fun state (fromParameter, toParameter) ->
            state
            |> Result.bind (fun pieces ->
                between segment fromParameter toParameter
                |> Result.map (fun piece -> piece :: pieces))) (Ok [])
        |> Result.map List.rev

    let betweenManyInside segment parameters =
        if parameters |> List.exists (fun t -> t < 0.0<parameter> || t > 1.0<parameter>) then Error SplitOutsideSegment
        else betweenMany segment parameters

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

    let defaultDirectionOptions = { RelativeTolerance = 1.0e-9 }

    let private directionFromCandidates options candidates =
        let scaleSquared =
            candidates
            |> List.fold (fun scale candidate -> max scale (Point.squaredNorm candidate)) 0.0<length^2>
        let thresholdSquared = options.RelativeTolerance * options.RelativeTolerance * scaleSquared
        candidates
        |> List.tryPick (fun candidate ->
            let magnitudeSquared = Point.squaredNorm candidate
            if magnitudeSquared > thresholdSquared then Point.normalize candidate else None)

    let private endpointDirection options incoming segment =
        let candidates =
            match segment, incoming with
            | Line(startPoint, endPoint), _ -> [ Point.displacement startPoint endPoint ]
            | QuadraticBezier(startPoint, control, endPoint), false ->
                [ Point.displacement startPoint control; Point.displacement startPoint endPoint ]
            | QuadraticBezier(startPoint, control, endPoint), true ->
                [ Point.displacement control endPoint; Point.displacement startPoint endPoint ]
            | CubicBezier(startPoint, control1, control2, endPoint), false ->
                [ Point.displacement startPoint control1
                  Point.displacement startPoint control2
                  Point.displacement startPoint endPoint ]
            | CubicBezier(startPoint, control1, control2, endPoint), true ->
                [ Point.displacement control2 endPoint
                  Point.displacement control1 endPoint
                  Point.displacement startPoint endPoint ]
            | Arc _, _ -> []
        directionFromCandidates options candidates

    /// Return singularity-safe unit traversal directions at a segment parameter.
    let directionsWith options segment t =
        if options.RelativeTolerance < 0.0 || not (System.Double.IsFinite options.RelativeTolerance) then
            Error(InvalidDirectionRelativeTolerance options.RelativeTolerance)
        else
            match segment with
            | Arc _ ->
                derivative segment t
                |> Result.map (fun derivative ->
                    let direction = Point.normalize derivative
                    if t = 0.0<parameter> then { Incoming = None; Outgoing = direction }
                    elif t = 1.0<parameter> then { Incoming = direction; Outgoing = None }
                    else { Incoming = direction; Outgoing = direction })
            | _ when t = 0.0<parameter> ->
                Ok { Incoming = None; Outgoing = endpointDirection options false segment }
            | _ when t = 1.0<parameter> ->
                Ok { Incoming = endpointDirection options true segment; Outgoing = None }
            | _ ->
                split segment t
                |> Result.map (fun (left, right) ->
                    { Incoming = endpointDirection options true left
                      Outgoing = endpointDirection options false right })

    let directions segment t = directionsWith defaultDirectionOptions segment t

    let defaultLengthOptions: LengthOptions =
        { Tolerance = 1.0e-6<length>
          MaxDepth = 20 }

    let defaultDistanceOptions =
        { Samples = 100
          Tolerance = 1.0e-9<length>
          MaxIterations = 100 }

    let defaultCrossingOptions =
        { Samples = 100
          SignedLineDistanceTolerance = 1.0e-9<length>
          MaxIterations = 100 }

    let defaultMinimizeOptions =
        { Samples = 100
          ParameterTolerance = 1.0e-9<parameter>
          MaxIterations = 100 }

    let private validateCrossingOptions (options: CrossingOptions) =
        if options.Samples <= 0 then Error(InvalidCrossingSamples options.Samples)
        elif options.SignedLineDistanceTolerance <= 0.0<length>
             || not (System.Double.IsFinite(float options.SignedLineDistanceTolerance)) then
            Error(InvalidCrossingTolerance options.SignedLineDistanceTolerance)
        elif options.MaxIterations <= 0 then Error(InvalidCrossingMaxIterations options.MaxIterations)
        else Ok()

    let crossingsWith segment (measure: Point<length> -> float<length>) (options: CrossingOptions) =
        let sameSign (a: float<length>) (b: float<length>) =
            (a < 0.0<length> && b < 0.0<length>)
            || (a > 0.0<length> && b > 0.0<length>)
        let value (t: float<parameter>) = point segment t |> Result.map measure
        let rec refine (leftT: float<parameter>) leftValue (rightT: float<parameter>) remaining =
            let middle = leftT + (rightT - leftT) / 2.0
            value middle
            |> Result.bind (fun middleValue ->
                if abs middleValue <= options.SignedLineDistanceTolerance then Ok middle
                elif remaining <= 1 || middle = leftT || middle = rightT then
                    Error(CrossingMaxIterationsReached(middle, middleValue))
                elif sameSign leftValue middleValue then refine middle middleValue rightT (remaining - 1)
                else refine leftT leftValue middle (remaining - 1))
        let window (previousT: float<parameter>) previousValue (nextT: float<parameter>) nextValue =
            if previousValue = 0.0<length> then Ok(Some previousT)
            elif nextValue = 0.0<length> then Ok(Some nextT)
            elif sameSign previousValue nextValue then Ok None
            elif abs previousValue <= options.SignedLineDistanceTolerance then Ok(Some previousT)
            elif abs nextValue <= options.SignedLineDistanceTolerance then Ok(Some nextT)
            else refine previousT previousValue nextT options.MaxIterations |> Result.map Some
        let insertUnique (value: float<parameter>) (values: float<parameter> list) =
            match values with
            | previous :: _ when abs (previous - value) <= curveParameterTolerance -> values
            | _ -> value :: values
        validateCrossingOptions options
        |> Result.bind (fun () ->
            value 0.0<parameter>
            |> Result.bind (fun firstValue ->
                let rec scan index previousT previousValue found =
                    if index > options.Samples then Ok(List.rev found)
                    else
                        let nextT = Parameter.fromFloat (float index / float options.Samples)
                        value nextT
                        |> Result.bind (fun nextValue ->
                            window previousT previousValue nextT nextValue
                            |> Result.bind (fun crossing ->
                                let found = crossing |> Option.map (fun t -> insertUnique t found) |> Option.defaultValue found
                                scan (index + 1) nextT nextValue found))
                scan 1 0.0<parameter> firstValue []))

    let crossings segment measure = crossingsWith segment measure defaultCrossingOptions

    let minimizeWith segment measure (options: MinimizeOptions) =
        let candidate (t: float<parameter>) = point segment t |> Result.map (fun point -> t, measure point)
        let best ((_, leftValue) as left) ((_, rightValue) as right) = if leftValue <= rightValue then left else right
        let ratio = 0.6180339887498949
        let rec golden (left: float<parameter>) (right: float<parameter>) (innerLeft: float<parameter>) leftCandidate (innerRight: float<parameter>) rightCandidate remaining =
            if right - left <= options.ParameterTolerance then Ok(best leftCandidate rightCandidate)
            elif remaining <= 0 then
                let estimate, value = best leftCandidate rightCandidate
                Error(MinimizeMaxIterationsReached(estimate, value))
            elif snd leftCandidate < snd rightCandidate then
                let nextRight = innerRight
                let nextInnerRight = innerLeft
                let nextInnerLeft = nextRight - (nextRight - left) * ratio
                candidate nextInnerLeft
                |> Result.bind (fun next -> golden left nextRight nextInnerLeft next nextInnerRight leftCandidate (remaining - 1))
            else
                let nextLeft = innerLeft
                let nextInnerLeft = innerRight
                let nextInnerRight = nextLeft + (right - nextLeft) * ratio
                candidate nextInnerRight
                |> Result.bind (fun next -> golden nextLeft right nextInnerLeft rightCandidate nextInnerRight next (remaining - 1))
        let minimizeWindow (left: float<parameter>) (right: float<parameter>) =
            let span = right - left
            let innerLeft = right - span * ratio
            let innerRight = left + span * ratio
            candidate innerLeft
            |> Result.bind (fun leftCandidate ->
                candidate innerRight
                |> Result.bind (fun rightCandidate -> golden left right innerLeft leftCandidate innerRight rightCandidate options.MaxIterations))
        if options.Samples <= 0 then Error(InvalidMinimizeSamples options.Samples)
        elif options.ParameterTolerance <= 0.0<parameter> || not (System.Double.IsFinite(float options.ParameterTolerance)) then
            Error(InvalidMinimizeTolerance options.ParameterTolerance)
        elif options.MaxIterations <= 0 then Error(InvalidMinimizeMaxIterations options.MaxIterations)
        else
            candidate 0.0<parameter>
            |> Result.bind (fun first ->
                let rec scan index previousT currentBest =
                    if index > options.Samples then Ok(fst currentBest)
                    else
                        let nextT = Parameter.fromFloat (float index / float options.Samples)
                        candidate nextT
                        |> Result.bind (fun next ->
                            minimizeWindow previousT nextT
                            |> Result.bind (fun windowBest -> scan (index + 1) nextT (best (best currentBest next) windowBest)))
                scan 1 0.0<parameter> first)

    let minimize segment measure = minimizeWith segment measure defaultMinimizeOptions

    let private crossingRootError (error: RootError<length>) =
        match error with
        | InvalidMaxIterations iterations -> InvalidCrossingMaxIterations iterations
        | MaxIterationsReached(estimate, value) -> CrossingMaxIterationsReached(estimate, value)
        | NotBracketed(left, _, leftValue, _) -> CrossingMaxIterationsReached(left, leftValue)

    let private lineCrossingConstant startPoint point normal =
        Point.dot (Point.displacement point startPoint) normal

    let private lineClassifiedRoots
        segment
        (point: Point<length>)
        (normal: Point<1>)
        signedLineDistanceTolerance
        maxIterations =
        let options: PolynomialOptions = { MaxIterations = maxIterations }
        let solve coefficients =
            Root.classifiedPolynomialRootsWith coefficients 0.0<parameter> 1.0<parameter> options
            |> Result.mapError crossingRootError
        match segment with
        | Line(startPoint, endPoint) ->
            solve
                [ Point.dot (Point.displacement startPoint endPoint) normal
                  lineCrossingConstant startPoint point normal ]
        | QuadraticBezier(startPoint, control, endPoint) ->
            let p0 = Point.dot startPoint normal
            let p1 = Point.dot control normal
            let p2 = Point.dot endPoint normal
            solve
                [ p0 - 2.0 * p1 + p2
                  2.0 * (p1 - p0)
                  lineCrossingConstant startPoint point normal ]
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let p0 = Point.dot startPoint normal
            let p1 = Point.dot control1 normal
            let p2 = Point.dot control2 normal
            let p3 = Point.dot endPoint normal
            solve
                [ -p0 + 3.0 * p1 - 3.0 * p2 + p3
                  3.0 * (p0 - 2.0 * p1 + p2)
                  3.0 * (p1 - p0)
                  lineCrossingConstant startPoint point normal ]
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.mapError (fun _ -> DegenerateArc)
            |> Result.bind (fun arc ->
                let cosineRotation = Trig.cosDegrees arc.XAxisRotation
                let sineRotation = Trig.sinDegrees arc.XAxisRotation
                let xAxis = Point.create (arc.Radius.X * cosineRotation) (arc.Radius.X * sineRotation)
                let yAxis = Point.create (-arc.Radius.Y * sineRotation) (arc.Radius.Y * cosineRotation)
                let alpha = Point.dot normal xAxis
                let beta = Point.dot normal yAxis
                let constant = Point.dot (Point.displacement point arc.Center) normal
                let radius = sqrt (alpha * alpha + beta * beta)
                if radius <= signedLineDistanceTolerance then Ok []
                else
                    let cosine = float (-constant / radius)
                    let cosineTolerance = float (signedLineDistanceTolerance / radius)
                    let aperture =
                        if cosine < -1.0 || cosine > 1.0 then
                            if abs cosine <= 1.0 + cosineTolerance then
                                Some(if cosine < 0.0 then 180.0<degree> else 0.0<degree>)
                            else None
                        else Trig.acosDegrees cosine
                    match aperture with
                    | None when cosine < -1.0 || cosine > 1.0 -> Ok []
                    | None -> Error(InvalidCrossingTolerance signedLineDistanceTolerance)
                    | Some aperture ->
                        let phase = Trig.atan2Degrees beta alpha
                        let positiveRemainder angle =
                            let value = Degree.toFloat angle % 360.0
                            Degree.fromFloat(if value < 0.0 then value + 360.0 else value)
                        let angleInSweep angle =
                            if arc.DeltaAngle >= 0.0<degree> then
                                positiveRemainder (angle - arc.StartAngle) <= arc.DeltaAngle + 1.0e-9<degree>
                            else
                                positiveRemainder (arc.StartAngle - angle) <= -arc.DeltaAngle + 1.0e-9<degree>
                        let progress angle =
                            if arc.DeltaAngle >= 0.0<degree> then positiveRemainder (angle - arc.StartAngle)
                            else positiveRemainder (arc.StartAngle - angle)
                        [ phase + aperture; phase - aperture ]
                        |> List.filter angleInSweep
                        |> List.map (fun angle -> Parameter.fromFloat(float (progress angle / abs arc.DeltaAngle)))
                        |> List.filter (fun t -> t >= 0.0<parameter> && t <= 1.0<parameter>)
                        |> List.sort
                        |> List.fold (fun roots t ->
                            match roots with
                            | previous :: _ when abs (t - previous) <= 1.0e-12<parameter> -> roots
                            | _ -> t :: roots) []
                        |> List.rev
                        |> List.map (fun t ->
                            { Isolation = { Lower = t; Estimate = t; Upper = t }
                              Kind = Ambiguous })
                        |> Ok)

    /// Find crossings between a segment and a ray's supporting line.
    /// Negative ray parameters represent crossings behind the ray origin.
    let rayCrossingsWith
        segment
        (origin: Point<length>)
        (direction: Point<1>)
        (options: CrossingOptions) =
        validateCrossingOptions options
        |> Result.bind (fun () ->
            let directionSquared = Point.squaredNorm direction
            if directionSquared <= 0.0 || not (System.Double.IsFinite(float directionSquared)) then
                Error IndeterminateDirection
            else
                let normal = Point.create direction.Y (-direction.X)
                lineClassifiedRoots
                    segment origin normal
                    (options.SignedLineDistanceTolerance * sqrt directionSquared)
                    options.MaxIterations
                |> Result.bind (fun roots ->
                    roots
                    |> List.fold (fun state root ->
                        state
                        |> Result.bind (fun crossings ->
                            let t = root.Isolation.Estimate
                            point segment t
                            |> Result.map (fun sample ->
                                let rayT = Point.dot (Point.displacement origin sample) direction / directionSquared
                                (t, rayT) :: crossings))) (Ok [])
                    |> Result.map List.rev))

    let rayCrossings segment origin direction =
        rayCrossingsWith segment origin direction defaultCrossingOptions

    let validateLengthOptions (options: LengthOptions) =
        if options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidLengthTolerance options.Tolerance)
        elif options.MaxDepth <= 0 then Error(InvalidLengthMaxDepth options.MaxDepth)
        else Ok()

    let private speed segment t =
        derivative segment t |> Result.map Point.norm

    let private curvedLengthWith segment (options: LengthOptions) =
        let simpson
            (a: float<parameter>)
            (b: float<parameter>)
            (fa: float<length / parameter>)
            (fm: float<length / parameter>)
            (fb: float<length / parameter>) : float<length> =
            (fa + 4.0 * fm + fb) * (b - a) / 6.0
        let rec refine a b fa fm fb estimate depth =
            let middle = Parameter.fromFloat ((Parameter.ratio a + Parameter.ratio b) / 2.0)
            let leftMiddle = Parameter.fromFloat ((Parameter.ratio a + Parameter.ratio middle) / 2.0)
            let rightMiddle = Parameter.fromFloat ((Parameter.ratio middle + Parameter.ratio b) / 2.0)
            speed segment leftMiddle
            |> Result.bind (fun flm ->
                speed segment rightMiddle
                |> Result.bind (fun frm ->
                    let left = simpson a middle fa flm fm
                    let right = simpson middle b fm frm fb
                    let combined = left + right
                    if depth = 0 || abs (combined - estimate) <= 15.0 * options.Tolerance then
                        Ok(combined + (combined - estimate) / 15.0)
                    else
                        refine a middle fa flm fm left (depth - 1)
                        |> Result.bind (fun leftLength ->
                            refine middle b fm frm fb right (depth - 1)
                            |> Result.map (fun rightLength -> leftLength + rightLength))))
        let a = 0.0<parameter>
        let b = 1.0<parameter>
        let middle = 0.5<parameter>
        speed segment a
        |> Result.bind (fun fa ->
            speed segment middle
            |> Result.bind (fun fm ->
                speed segment b
                |> Result.bind (fun fb -> refine a b fa fm fb (simpson a b fa fm fb) options.MaxDepth)))

    /// Approximate segment arc length with explicit error and depth controls.
    let lengthWith segment (options: LengthOptions) =
        validateLengthOptions options
        |> Result.bind (fun () ->
            match segment with
            | Line(startPoint, endPoint) -> Ok(Point.distance startPoint endPoint)
            | _ -> curvedLengthWith segment options)

    let length segment = lengthWith segment defaultLengthOptions

    let private validateLengthDistance distance segmentLength =
        if not (System.Double.IsFinite(float distance)) || distance < 0.0<length> || distance > segmentLength then
            Error(InvalidLengthDistance(distance, segmentLength))
        else Ok()

    /// Return the segment parameter at a traveled arc length.
    let parameterAtLengthWith segment distance (options: LengthOptions) =
        lengthWith segment options
        |> Result.bind (fun segmentLength ->
            validateLengthDistance distance segmentLength
            |> Result.bind (fun () ->
                if segmentLength = 0.0<length> || distance = 0.0<length> then Ok 0.0<parameter>
                elif distance = segmentLength then Ok 1.0<parameter>
                else
                    match segment with
                    | Line _ -> Ok(Parameter.fromFloat(float (distance / segmentLength)))
                    | _ ->
                        let rec search low high iterations =
                            let middle = Parameter.fromFloat ((Parameter.ratio low + Parameter.ratio high) / 2.0)
                            betweenInside segment 0.0<parameter> middle
                            |> Result.bind (fun prefix -> lengthWith prefix options)
                            |> Result.bind (fun prefixLength ->
                                if iterations = 0 || abs (prefixLength - distance) <= options.Tolerance then Ok middle
                                elif prefixLength < distance then search middle high (iterations - 1)
                                else search low middle (iterations - 1))
                        search 0.0<parameter> 1.0<parameter> 64))

    let parameterAtLength segment distance = parameterAtLengthWith segment distance defaultLengthOptions

    let pointAtLengthWith segment distance (options: LengthOptions) =
        parameterAtLengthWith segment distance options |> Result.bind (point segment)

    let pointAtLength segment distance = pointAtLengthWith segment distance defaultLengthOptions

    let derivativeAtLengthWith segment distance (options: LengthOptions) =
        parameterAtLengthWith segment distance options |> Result.bind (derivative segment)

    let derivativeAtLength segment distance = derivativeAtLengthWith segment distance defaultLengthOptions

    let isZeroLength segment tolerance =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidZeroLengthTolerance tolerance)
        else length segment |> Result.map (fun value -> value <= tolerance)

    let betweenLengthsWith segment fromDistance toDistance options =
        lengthWith segment options
        |> Result.bind (fun segmentLength ->
            parameterAtLengthWith segment fromDistance options
            |> Result.bind (fun fromParameter ->
                parameterAtLengthWith segment toDistance options
                |> Result.bind (fun toParameter -> between segment fromParameter toParameter)))

    let betweenLengths segment fromDistance toDistance =
        betweenLengthsWith segment fromDistance toDistance defaultLengthOptions

    let betweenLengthsManyWith segment distances options =
        lengthWith segment options
        |> Result.bind (fun _ ->
            distances
            |> List.fold (fun state distance ->
                state
                |> Result.bind (fun parameters ->
                    parameterAtLengthWith segment distance options
                    |> Result.map (fun parameter -> parameter :: parameters))) (Ok [])
            |> Result.bind (List.rev >> betweenMany segment))

    let betweenLengthsMany segment distances =
        betweenLengthsManyWith segment distances defaultLengthOptions

    let subdivideToMaxLengthWith segment maxLength options =
        if maxLength <= 0.0<length> || not (System.Double.IsFinite(float maxLength)) then
            Error(InvalidSubdivisionMaxLength maxLength)
        else
            lengthWith segment options
            |> Result.bind (fun segmentLength ->
                if segmentLength = 0.0<length> then Ok [ segment ]
                else
                    let pieceCount = int (ceil (float (segmentLength / maxLength)))
                    let step = segmentLength / float pieceCount
                    [ 0 .. pieceCount ]
                    |> List.map (fun index -> if index = pieceCount then segmentLength else float index * step)
                    |> fun distances -> betweenLengthsManyWith segment distances options)

    let subdivideToMaxLength segment maxLength =
        subdivideToMaxLengthWith segment maxLength defaultLengthOptions

    let internal validateDistanceOptions (options: DistanceOptions) =
        if options.Samples <= 0 then Error(InvalidDistanceSamples options.Samples)
        elif options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidDistanceTolerance options.Tolerance)
        elif options.MaxIterations <= 0 then Error(InvalidDistanceMaxIterations options.MaxIterations)
        else Ok()

    let private projectionAt sample segment t =
        point segment t
        |> Result.map (fun projected -> t, projected, Point.distance sample projected)

    let private smallestProjection sample segment candidates =
        candidates
        |> List.fold (fun state t ->
            state
            |> Result.bind (fun best ->
                projectionAt sample segment t
                |> Result.map (fun candidate ->
                    match best with
                    | None -> Some candidate
                    | Some(_, _, bestDistance) when (let _, _, distance = candidate in distance < bestDistance) -> Some candidate
                    | _ -> best))) (Ok None)
        |> Result.bind (function
            | Some best -> Ok best
            | None -> projectionAt sample segment 0.0<parameter>)

    let private distanceStationaryValue sample segment t =
        point segment t
        |> Result.bind (fun segmentPoint ->
            derivative segment t
            |> Result.map (fun tangent -> Point.dot (Point.displacement sample segmentPoint) tangent))

    let private distanceRootError (error: RootError<length^2 / parameter>) =
        match error with
        | InvalidMaxIterations iterations -> InvalidDistanceMaxIterations iterations
        | MaxIterationsReached(estimate, value) -> DistanceMaxIterationsReached(estimate, value)
        | NotBracketed _ -> DistanceRootIsolationFailed

    let private sameSign a b =
        (a < 0.0<_> && b < 0.0<_>) || (a > 0.0<_> && b > 0.0<_>)

    let private bestDistanceParameter sample segment (leftT: float<parameter>) (rightT: float<parameter>) =
        let midpointT = leftT + (rightT - leftT) / 2.0
        point segment leftT
        |> Result.bind (fun left ->
            point segment midpointT
            |> Result.bind (fun midpoint ->
                point segment rightT
                |> Result.map (fun right ->
                    let leftDistance = Point.squaredDistance sample left
                    let midpointDistance = Point.squaredDistance sample midpoint
                    let rightDistance = Point.squaredDistance sample right
                    if leftDistance <= midpointDistance && leftDistance <= rightDistance then leftT
                    elif midpointDistance <= rightDistance then midpointT
                    else rightT)))

    let private derivativeScale segment =
        match segment with
        | Line(startPoint, endPoint) -> Ok(Point.distance startPoint endPoint)
        | QuadraticBezier(startPoint, control, endPoint) ->
            Ok(2.0 * max (Point.distance startPoint control) (Point.distance control endPoint))
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            Ok(3.0 * max (Point.distance startPoint control1) (max (Point.distance control1 control2) (Point.distance control2 endPoint)))
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.map (fun arc ->
                abs (Degree.toRadians arc.DeltaAngle |> Radian.toFloat)
                * max (abs arc.Radius.X) (abs arc.Radius.Y))
            |> Result.mapError (fun _ -> DegenerateArc)

    let private derivativeScaleSquared segment =
        derivativeScale segment |> Result.map (fun scale -> scale * scale)

    let private tangentialErrorIsImproving segment previousT previousValue proposalT proposalValue =
        derivative segment previousT
        |> Result.bind (fun previousDerivative ->
            derivative segment proposalT
            |> Result.bind (fun proposalDerivative ->
                derivativeScaleSquared segment
                |> Result.map (fun derivativeScaleSquared ->
                    let previousSpeedSquared = Point.dot previousDerivative previousDerivative
                    let proposalSpeedSquared = Point.dot proposalDerivative proposalDerivative
                    let reliableSpeedSquared = derivativeScaleSquared * 1.0e-10<1/parameter^2>
                    previousSpeedSquared >= reliableSpeedSquared
                    && proposalSpeedSquared >= reliableSpeedSquared
                    && proposalValue * proposalValue * previousSpeedSquared
                       < previousValue * previousValue * proposalSpeedSquared)))

    let rec private polishProjectionWindowByBisection sample segment (leftT: float<parameter>) leftValue (rightT: float<parameter>) (estimate: float<parameter>) estimateValue remaining =
        if remaining <= 0 || estimateValue = 0.0<_> then Ok estimate
        else
            let midpointT = leftT + (rightT - leftT) / 2.0
            distanceStationaryValue sample segment midpointT
            |> Result.bind (fun midpointValue ->
                let nextLeft, nextLeftValue, nextRight =
                    if sameSign leftValue midpointValue then midpointT, midpointValue, rightT
                    else leftT, leftValue, midpointT
                bestDistanceParameter sample segment nextLeft nextRight
                |> Result.bind (fun proposal ->
                    distanceStationaryValue sample segment proposal
                    |> Result.bind (fun proposalValue ->
                        tangentialErrorIsImproving segment estimate estimateValue proposal proposalValue
                        |> Result.bind (fun progressing ->
                            if proposal = estimate || not progressing then Ok estimate
                            else polishProjectionWindowByBisection sample segment nextLeft nextLeftValue nextRight proposal proposalValue (remaining - 1)))))

    let rec private refineProjectionWindowByBisection sample segment tolerance (leftT: float<parameter>) leftValue (rightT: float<parameter>) remainingIterations polishIterations =
        betweenInside segment leftT rightT
        |> Result.bind boundingBox
        |> Result.bind (fun box ->
            if BoundingBox.diameter box <= tolerance then
                bestDistanceParameter sample segment leftT rightT
                |> Result.bind (fun estimate ->
                    distanceStationaryValue sample segment estimate
                    |> Result.bind (fun estimateValue ->
                        polishProjectionWindowByBisection sample segment leftT leftValue rightT estimate estimateValue polishIterations))
            else
                let midpointT = leftT + (rightT - leftT) / 2.0
                distanceStationaryValue sample segment midpointT
                |> Result.bind (fun midpointValue ->
                    if remainingIterations <= 1 then Error(DistanceMaxIterationsReached(midpointT, midpointValue))
                    elif midpointValue = 0.0<_> then Ok midpointT
                    elif sameSign leftValue midpointValue then
                        refineProjectionWindowByBisection sample segment tolerance midpointT midpointValue rightT (remainingIterations - 1) polishIterations
                    else
                        refineProjectionWindowByBisection sample segment tolerance leftT leftValue midpointT (remainingIterations - 1) polishIterations))

    let private refineIsolatedDistanceRootByBisection sample segment (options: DistanceOptions) (isolation: RootIsolation) =
        if isolation.Lower = isolation.Upper then Ok isolation.Estimate
        else
            distanceStationaryValue sample segment isolation.Lower
            |> Result.bind (fun lowerValue ->
                distanceStationaryValue sample segment isolation.Upper
                |> Result.bind (fun upperValue ->
                    if sameSign lowerValue upperValue then Ok isolation.Estimate
                    else
                        refineProjectionWindowByBisection
                            sample segment options.Tolerance isolation.Lower lowerValue isolation.Upper
                            options.MaxIterations options.MaxIterations))

    let private bezierDistancePolynomial sample segment : Result<float<length^2 / parameter> list, SegmentError> =
        let coefficient value = LanguagePrimitives.FloatWithMeasure<length^2 / parameter> value
        match segment with
        | QuadraticBezier(startPoint, control, endPoint) ->
            let ax = float (startPoint.X - 2.0 * control.X + endPoint.X)
            let ay = float (startPoint.Y - 2.0 * control.Y + endPoint.Y)
            let bx = float (2.0 * (control.X - startPoint.X))
            let by = float (2.0 * (control.Y - startPoint.Y))
            let cx = float (startPoint.X - sample.X)
            let cy = float (startPoint.Y - sample.Y)
            Ok [ coefficient (2.0 * (ax*ax + ay*ay)); coefficient (3.0 * (ax*bx + ay*by));
                 coefficient (bx*bx + by*by + 2.0*(ax*cx + ay*cy)); coefficient (bx*cx + by*cy) ]
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let ax = float (-startPoint.X + 3.0*control1.X - 3.0*control2.X + endPoint.X)
            let ay = float (-startPoint.Y + 3.0*control1.Y - 3.0*control2.Y + endPoint.Y)
            let bx = float (3.0*startPoint.X - 6.0*control1.X + 3.0*control2.X)
            let by = float (3.0*startPoint.Y - 6.0*control1.Y + 3.0*control2.Y)
            let cx = float (3.0*(control1.X - startPoint.X))
            let cy = float (3.0*(control1.Y - startPoint.Y))
            let dx = float (startPoint.X - sample.X)
            let dy = float (startPoint.Y - sample.Y)
            Ok [ coefficient (3.0*(ax*ax + ay*ay)); coefficient (5.0*(ax*bx + ay*by));
                 coefficient (4.0*(ax*cx + ay*cy) + 2.0*(bx*bx + by*by));
                 coefficient (3.0*(ax*dx + ay*dy) + 3.0*(bx*cx + by*cy));
                 coefficient (2.0*(bx*dx + by*dy) + cx*cx + cy*cy); coefficient (cx*dx + cy*dy) ]
        | _ -> Error DistanceRootIsolationFailed

    let private bezierProjectionWith sample segment (options: DistanceOptions) =
        bezierDistancePolynomial sample segment
        |> Result.bind (fun coefficients ->
            let polynomialOptions: PolynomialOptions =
                { MaxIterations = options.MaxIterations }
            Root.polynomialRootIsolationsWith coefficients 0.0<parameter> 1.0<parameter> polynomialOptions
            |> Result.mapError distanceRootError
            |> Result.bind (fun roots ->
                roots
                |> List.fold (fun state isolation ->
                    state
                    |> Result.bind (fun refined ->
                        refineIsolatedDistanceRootByBisection sample segment options isolation
                        |> Result.map (fun root -> root :: refined))) (Ok [])
                |> Result.bind (fun roots -> smallestProjection sample segment (0.0<parameter> :: 1.0<parameter> :: List.rev roots))))

    let private arcProjectionWith sample segment (options: DistanceOptions) =
        derivativeScale segment
        |> Result.bind (fun scale ->
            let stationaryValueTolerance = options.Tolerance * scale / 1.0<parameter>
            let close value = abs value <= stationaryValueTolerance
            let insertNearUnique value candidates =
                match candidates with
                | previous :: _ when abs (previous - value) <= curveParameterTolerance -> candidates
                | _ -> value :: candidates

            let refineWindow leftT leftValue rightT =
                refineProjectionWindowByBisection
                    sample segment options.Tolerance leftT leftValue rightT
                    options.MaxIterations options.MaxIterations
                |> Result.map Some

            distanceStationaryValue sample segment 0.0<parameter>
            |> Result.bind (fun firstValue ->
                let rec scan
                    index
                    (previousT: float<parameter>)
                    (previousValue: float<length^2 / parameter>)
                    (candidates: float<parameter> list) =
                    if index > options.Samples then Ok candidates
                    else
                        let nextT = Parameter.fromFloat(float index / float options.Samples)
                        distanceStationaryValue sample segment nextT
                        |> Result.bind (fun nextValue ->
                            let candidate =
                                if close previousValue then Ok(Some previousT)
                                elif close nextValue then Ok(Some nextT)
                                elif sameSign previousValue nextValue then Ok None
                                else refineWindow previousT previousValue nextT
                            candidate
                            |> Result.bind (fun candidate ->
                                let candidates = match candidate with Some t -> insertNearUnique t candidates | None -> candidates
                                scan (index + 1) nextT nextValue candidates))
                scan 1 0.0<parameter> firstValue [ 1.0<parameter>; 0.0<parameter> ]
                |> Result.bind (smallestProjection sample segment)))

    let projectionWith target sample (options: DistanceOptions) =
        validateDistanceOptions options
        |> Result.bind (fun () ->
            match target with
            | Line(startPoint, endPoint) ->
                let line = Point.displacement startPoint endPoint
                let lengthSquared = Point.squaredNorm line
                let t =
                    if lengthSquared = 0.0<length^2> then 0.0<parameter>
                    else Parameter.fromFloat(float (Point.dot (Point.displacement startPoint sample) line / lengthSquared))
                         |> max 0.0<parameter> |> min 1.0<parameter>
                projectionAt sample target t
            | Arc _ -> arcProjectionWith sample target options
            | QuadraticBezier _
            | CubicBezier _ -> bezierProjectionWith sample target options)

    let projection target sample = projectionWith target sample defaultDistanceOptions

    let distanceWith target sample options = projectionWith target sample options |> Result.map (fun (_, _, distance) -> distance)
    let distance target sample = distanceWith target sample defaultDistanceOptions

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

    let degenerateLines segment tolerance =
        let finitePositive = tolerance > 0.0<length> && System.Double.IsFinite(float tolerance)
        let farthest points origin =
            points |> List.fold (fun best point -> if Point.squaredDistance point origin > Point.squaredDistance best origin then point else best) origin
        let axisFor points =
            match points with
            | [] -> None
            | origin :: _ ->
                let farthestPoint = farthest points origin
                if Point.squaredDistance farthestPoint origin <= tolerance * tolerance then None
                else Some(origin, Point.displacement origin farthestPoint)
        let inStrip points origin axis =
            let axisLength = Point.norm axis
            points |> List.forall (fun point -> abs (Point.cross (Point.displacement origin point) axis) / axisLength <= tolerance)
        let coordinate point origin axis =
            let denominator = Point.squaredNorm axis
            if denominator = 0.0<length^2> then 0.0
            else float (Point.dot (Point.displacement origin point) axis / denominator)
        let pieces at =
            at
            |> List.pairwise
            |> List.fold (fun state (fromParameter, toParameter) ->
                state
                |> Result.bind (fun lines ->
                    point segment (Parameter.fromFloat fromParameter)
                    |> Result.bind (fun startPoint ->
                        point segment (Parameter.fromFloat toParameter)
                        |> Result.map (fun endPoint -> Line(startPoint, endPoint) :: lines)))) (Ok [])
            |> Result.map (List.rev >> List.filter (fun line -> start line <> finish line))
        let bezierResult definingPoints breaks =
            match axisFor definingPoints with
            | Some(origin, axis) when inStrip definingPoints origin axis -> pieces (0.0 :: breaks @ [ 1.0 ]) |> Result.map Some
            | None -> Ok(Some [])
            | _ -> Ok None
        if not finitePositive then Error(InvalidLinearizeTolerance tolerance)
        else
            match segment with
            | Line _ -> Ok None
            | QuadraticBezier(startPoint, control, endPoint) ->
                let points = [ startPoint; control; endPoint ]
                let origin = startPoint
                let axis = Point.displacement origin (farthest points origin)
                let s, c, e = coordinate startPoint origin axis, coordinate control origin axis, coordinate endPoint origin axis
                let denominator = s - 2.0 * c + e
                let breaks = if denominator = 0.0 then [] else [ (s - c) / denominator ] |> List.filter (fun t -> t > 0.0 && t < 1.0)
                bezierResult points breaks
            | CubicBezier(startPoint, control1, control2, endPoint) ->
                let points = [ startPoint; control1; control2; endPoint ]
                let origin = startPoint
                let axis = Point.displacement origin (farthest points origin)
                let s = coordinate startPoint origin axis
                let c1 = coordinate control1 origin axis
                let c2 = coordinate control2 origin axis
                let e = coordinate endPoint origin axis
                let a = -s + 3.0*c1 - 3.0*c2 + e
                let b = 3.0*s - 6.0*c1 + 3.0*c2
                let c = 3.0*c1 - 3.0*s
                let breaks = Root.strictlyInside (Root.quadratic (3.0*a) (2.0*b) c) 0.0<parameter> 1.0<parameter> |> List.map Parameter.ratio
                bezierResult points breaks
            | Arc endpoint when endpoint.Radius.X = 0.0<length> || endpoint.Radius.Y = 0.0<length> ->
                if endpoint.Start = endpoint.End then Ok(Some []) else Ok(Some [ Line(endpoint.Start, endpoint.End) ])
            | Arc _ ->
                toLinesWith { Tolerance = tolerance; MaxDepth = defaultLinearizeOptions.MaxDepth } segment
                |> Result.map (fun lines ->
                    let points = lines |> List.collect (fun line -> [ start line; finish line ])
                    match axisFor points with
                    | None -> Some []
                    | Some(origin, axis) when inStrip points origin axis -> Some(List.filter (fun line -> start line <> finish line) lines)
                    | _ -> None)

[<RequireQualifiedAccess>]
module Subpath =
    let private defaultWiggleTolerance = 1.0e-9<length>

    let defaultParametricOptions =
        { Tolerance = 0.01<length>
          SamplesPerPiece = 5
          InitialPieceCount = 1
          MaxDepth = 10
          Tangent = None }

    let empty startPoint =
        { startPoint = startPoint
          segmentList = []
          isClosed = false }

    let ofSegment segment =
        { startPoint = Segment.start segment
          segmentList = [ segment ]
          isClosed = false }

    let asPath subpath = { subpathList = [ subpath ] }

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

    let assertWith policy segments =
        match createWith policy segments with
        | Ok subpath -> subpath
        | Error _ -> invalidArg (nameof segments) "invalid subpath segments"

    let assertCreate segments = assertWith Strict segments
    let ``assert`` segments = assertCreate segments

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
            elif subpath.isClosed then Ok subpath
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

    let assertSetClosedWith policy closed subpath =
        match setClosedWith policy closed subpath with
        | Ok result -> result
        | Error _ -> invalidArg (nameof closed) "invalid closed subpath"

    let assertSetClosed closed subpath = assertSetClosedWith Strict closed subpath

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

    let assertPolyline points =
        match polyline points with
        | Ok subpath -> subpath
        | Error _ -> invalidArg (nameof points) "invalid polyline points"

    let assertPolygon points =
        match polygon points with
        | Ok subpath -> subpath
        | Error _ -> invalidArg (nameof points) "invalid polygon points"

    let parametricWith startValue endValue pointFunction (options: ParametricOptions) =
        let finitePoint (point: Point<length>) =
            System.Double.IsFinite(float point.X) && System.Double.IsFinite(float point.Y)
        let getPoint parameterValue =
            let point = pointFunction parameterValue
            if finitePoint point then Ok point else Error(NonFiniteParametricPoint(parameterValue, point))
        let getTangent tangentFunction parameterValue =
            let tangent = tangentFunction parameterValue
            if finitePoint tangent then Ok tangent else Error(NonFiniteParametricTangent(parameterValue, tangent))
        let interpolateValue a b t = a + (b - a) * t
        let fit intervalStart intervalEnd =
            getPoint intervalStart
            |> Result.bind (fun startPoint ->
                getPoint intervalEnd
                |> Result.bind (fun endPoint ->
                    [ 1 .. options.SamplesPerPiece ]
                    |> List.fold (fun state index ->
                        state
                        |> Result.bind (fun samples ->
                            let t = float index / float (options.SamplesPerPiece + 1)
                            let parameterValue = interpolateValue intervalStart intervalEnd t
                            getPoint parameterValue
                            |> Result.map (fun point -> (Parameter.fromFloat t, point) :: samples))) (Ok [])
                    |> Result.bind (fun reversedSamples ->
                        let samples = List.rev reversedSamples
                        match options.Tangent with
                        | None ->
                            Bezier.fitCubicWithEndpoints startPoint endPoint samples
                            |> Result.mapError (fun _ -> ParametricFitFailed)
                        | Some tangentFunction ->
                            getTangent tangentFunction intervalStart
                            |> Result.bind (fun startTangent ->
                                getTangent tangentFunction intervalEnd
                                |> Result.bind (fun endTangent ->
                                    Bezier.fitCubicWithEndpointTangents startPoint endPoint startTangent endTangent samples
                                    |> Result.mapError (fun _ -> ParametricFitFailed))))
                    |> Result.bind (fun (curve, report) ->
                        match curve with
                        | CubicBezierData(a, b, c, d) ->
                            let segment = CubicBezier(a, b, c, d)
                            if [ a; b; c; d ] |> List.forall finitePoint then Ok(segment, report.Max)
                            else Error ParametricFitFailed
                        | _ -> Error ParametricFitFailed)))
        let rec interval intervalStart intervalEnd depthRemaining =
            fit intervalStart intervalEnd
            |> Result.bind (fun (segment, error) ->
                if error <= options.Tolerance then Ok [ segment ]
                elif depthRemaining <= 0 then Error(ParametricMaxDepthReached error)
                else
                    let middle = interpolateValue intervalStart intervalEnd 0.5
                    interval intervalStart middle (depthRemaining - 1)
                    |> Result.bind (fun left -> interval middle intervalEnd (depthRemaining - 1) |> Result.map (fun right -> left @ right)))
        if options.Tolerance <= 0.0<length> || not (System.Double.IsFinite(float options.Tolerance)) then
            Error(InvalidParametricTolerance options.Tolerance)
        elif options.SamplesPerPiece < 2 then Error(InvalidParametricSamplesPerPiece options.SamplesPerPiece)
        elif options.InitialPieceCount <= 0 then Error(InvalidParametricInitialPieceCount options.InitialPieceCount)
        elif options.MaxDepth < 0 then Error(InvalidParametricMaxDepth options.MaxDepth)
        elif startValue = endValue || not (System.Double.IsFinite startValue) || not (System.Double.IsFinite endValue) then
            Error(InvalidParametricInterval(startValue, endValue))
        else
            [ 0 .. options.InitialPieceCount - 1 ]
            |> List.fold (fun state index ->
                state
                |> Result.bind (fun segments ->
                    let pieceStart = interpolateValue startValue endValue (float index / float options.InitialPieceCount)
                    let pieceEnd = interpolateValue startValue endValue (float (index + 1) / float options.InitialPieceCount)
                    interval pieceStart pieceEnd options.MaxDepth |> Result.map (fun pieces -> segments @ pieces))) (Ok [])
            |> Result.bind create

    let parametric startValue endValue pointFunction =
        parametricWith startValue endValue pointFunction defaultParametricOptions

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

    let assertSpliceWith policy startIndex deleteCount inserted subpath =
        match spliceWith policy startIndex deleteCount inserted subpath with
        | Ok result -> result
        | Error _ -> invalidArg (nameof startIndex) "invalid subpath splice"

    let assertSplice startIndex deleteCount inserted subpath =
        assertSpliceWith Strict startIndex deleteCount inserted subpath

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
        |> Result.map (fun reversed ->
            { startPoint = mapping subpath.startPoint
              segmentList = List.rev reversed
              isClosed = subpath.isClosed })

    let tryMapPoints mapping subpath =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun mapped ->
                Segment.tryMapPoints mapping segment |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.bind (fun reversed ->
            mapping subpath.startPoint
            |> Result.mapError PointMappingError
            |> Result.map (fun startPoint ->
                { startPoint = startPoint
                  segmentList = List.rev reversed
                  isClosed = subpath.isClosed }))

    let byPointPairSimilarity subpath sourceStart sourceEnd targetStart targetEnd =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun mapped ->
                Segment.byPointPairSimilarity segment sourceStart sourceEnd targetStart targetEnd
                |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.map (fun reversed ->
            let segments = List.rev reversed
            let startPoint =
                if subpath.startPoint = sourceStart then targetStart
                elif subpath.startPoint = sourceEnd then targetEnd
                else
                    match Affine.pointPairSimilarity sourceStart sourceEnd targetStart targetEnd with
                    | Ok transform -> Affine.point transform subpath.startPoint
                    | Error _ -> targetStart
            { startPoint = startPoint; segmentList = segments; isClosed = subpath.isClosed })

    let remapEndpoints subpath newStart newEnd =
        match subpath.segmentList with
        | [] -> Ok { subpath with startPoint = newStart }
        | _ ->
            let currentEnd = subpath.segmentList |> List.last |> Segment.finish
            byPointPairSimilarity subpath subpath.startPoint currentEnd newStart newEnd
            |> Result.map (fun mapped ->
                let rec forceEnd segments =
                    match segments with
                    | [] -> []
                    | [ last ] -> [ Segment.withFinish newEnd last ]
                    | first :: rest -> first :: forceEnd rest
                { mapped with startPoint = newStart; segmentList = mapped.segmentList |> List.mapi (fun i s -> if i = 0 then Segment.withStart newStart s else s) |> forceEnd })

    let arcsToCubicBeziers subpath =
        { subpath with
            segmentList = subpath.segmentList |> List.collect Segment.arcsToCubicBeziers }

    let toCubicBeziers subpath =
        { subpath with segmentList = subpath.segmentList |> List.collect Segment.toCubicBeziers }

    let degenerateLines subpath tolerance =
        if tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidLinearizeTolerance tolerance)
        else
            subpath.segmentList
            |> List.fold (fun state segment ->
                state
                |> Result.bind (fun accumulated ->
                    match accumulated with
                    | None -> Ok None
                    | Some accumulated ->
                        Segment.degenerateLines segment tolerance
                        |> Result.map (fun replacement ->
                            match segment, replacement with
                            | Line _, None -> Some(accumulated @ [ segment ])
                            | _, Some lines -> Some(accumulated @ lines)
                            | _ -> None))) (Ok(Some []))
            |> Result.map (fun replacements ->
                match replacements with
                | None -> None
                | Some lines ->
                    let points = lines |> List.collect (fun line -> [ Segment.start line; Segment.finish line ])
                    match points with
                    | [] -> Some []
                    | origin :: _ ->
                        let farthest = points |> List.maxBy (fun point -> Point.squaredDistance point origin)
                        let axis = Point.displacement origin farthest
                        let axisLength = Point.norm axis
                        if axisLength = 0.0<length> then Some []
                        elif points |> List.forall (fun point -> abs (Point.cross (Point.displacement origin point) axis) / axisLength <= tolerance) then
                            Some(List.filter (fun line -> Segment.start line <> Segment.finish line) lines)
                        else None)

    let segments subpath = subpath.segmentList
    let start subpath = subpath.startPoint
    let finish subpath = subpath.segmentList |> List.tryLast |> Option.map Segment.finish |> Option.defaultValue subpath.startPoint
    let ``end`` subpath = finish subpath

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

    let assertAppendWith policy segment subpath =
        match appendWith policy segment subpath with
        | Ok result -> result
        | Error _ -> invalidArg (nameof segment) "invalid appended segment"

    let assertAppend segment subpath = assertAppendWith Strict segment subpath

    let joinWith policy subpaths =
        if subpaths |> List.exists isClosed then Error AlreadyClosed
        else
            match subpaths with
            | [] -> Error EmptySubpaths
            | first :: rest ->
                rest
                |> List.fold (fun state next ->
                    state
                    |> Result.bind (fun accumulated ->
                        next.segmentList
                        |> List.fold (fun appended segment -> appended |> Result.bind (appendWith policy segment)) (Ok accumulated))) (Ok first)

    let join subpaths = joinWith Strict subpaths

    let assertJoinWith policy subpaths =
        match joinWith policy subpaths with
        | Ok result -> result
        | Error _ -> invalidArg (nameof subpaths) "invalid subpaths"

    let assertJoin subpaths = assertJoinWith Strict subpaths
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

    let parametersCompare left right =
        compare (left.SegmentIndex, left.T) (right.SegmentIndex, right.T)

    let parameterSnapToBoundary subpath parameter tolerance =
        if tolerance <= 0.0<parameter> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidIntersectionTolerance(LanguagePrimitives.FloatWithMeasure<length>(float tolerance)))
        else
            parameterCanonicalize subpath parameter
            |> Result.bind (fun parameter ->
                let snapped =
                    if parameter.T <= tolerance then { parameter with T = 0.0<parameter> }
                    elif 1.0<parameter> - parameter.T <= tolerance then { parameter with T = 1.0<parameter> }
                    else parameter
                parameterCanonicalize subpath snapped)

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

    let directionsWith subpath parameterValue options =
        parameterCanonicalize subpath parameterValue
        |> Result.bind (fun canonical ->
            let count = List.length subpath.segmentList
            let rec seek index step remaining incoming =
                if remaining <= 0 || count = 0 then Ok None
                else
                    let normalized =
                        if subpath.isClosed && index < 0 then count - 1
                        elif subpath.isClosed && index >= count then 0
                        else index
                    if normalized < 0 || normalized >= count then Ok None
                    else
                        Segment.directionsWith options subpath.segmentList[normalized] (if incoming then 1.0<parameter> else 0.0<parameter>)
                        |> Result.bind (fun directions ->
                            let found = if incoming then directions.Incoming else directions.Outgoing
                            match found with
                            | Some _ -> Ok found
                            | None -> seek (normalized + step) step (remaining - 1) incoming)
            if canonical.T = 0.0<parameter> then
                seek (canonical.SegmentIndex - 1) -1 count true
                |> Result.bind (fun incoming ->
                    seek canonical.SegmentIndex 1 count false
                    |> Result.map (fun outgoing -> { Incoming = incoming; Outgoing = outgoing }))
            elif canonical.T = 1.0<parameter> then
                seek canonical.SegmentIndex -1 count true
                |> Result.bind (fun incoming ->
                    seek (canonical.SegmentIndex + 1) 1 count false
                    |> Result.map (fun outgoing -> { Incoming = incoming; Outgoing = outgoing }))
            else Segment.directionsWith options subpath.segmentList[canonical.SegmentIndex] canonical.T)

    let directions subpath parameterValue = directionsWith subpath parameterValue Segment.defaultDirectionOptions

    let private intervalSegments subpath fromParameter toParameter =
        let fromIndex, toIndex = fromParameter.SegmentIndex, toParameter.SegmentIndex
        if parametersCompare fromParameter toParameter = 0 then Ok []
        elif parametersCompare fromParameter toParameter > 0 then Error(InvalidSubpathInterval(fromParameter, toParameter))
        elif fromIndex = toIndex then
            Segment.betweenInside subpath.segmentList[fromIndex] fromParameter.T toParameter.T |> Result.map List.singleton
        else
            let startPiece =
                if fromParameter.T = 0.0<parameter> then Ok [ subpath.segmentList[fromIndex] ]
                else Segment.betweenInside subpath.segmentList[fromIndex] fromParameter.T 1.0<parameter> |> Result.map List.singleton
            let middle = subpath.segmentList |> List.skip (fromIndex + 1) |> List.take (max 0 (toIndex - fromIndex - 1))
            let endPiece =
                if toParameter.T = 0.0<parameter> then Ok []
                elif toParameter.T = 1.0<parameter> then Ok [ subpath.segmentList[toIndex] ]
                else Segment.betweenInside subpath.segmentList[toIndex] 0.0<parameter> toParameter.T |> Result.map List.singleton
            startPiece |> Result.bind (fun first -> endPiece |> Result.map (fun last -> first @ middle @ last))

    let between subpath fromParameter toParameter =
        parameterCanonicalize subpath fromParameter
        |> Result.bind (fun fromParameter ->
            parameterCanonicalize subpath toParameter
            |> Result.bind (fun toParameter ->
                let order = parametersCompare fromParameter toParameter
                if order = 0 then Error(InvalidSubpathInterval(fromParameter, toParameter))
                elif order < 0 then intervalSegments subpath fromParameter toParameter |> Result.bind create
                elif not subpath.isClosed then Error(InvalidSubpathInterval(fromParameter, toParameter))
                else
                    let last = { SegmentIndex = List.length subpath.segmentList - 1; T = 1.0<parameter> }
                    let first = { SegmentIndex = 0; T = 0.0<parameter> }
                    intervalSegments subpath fromParameter last
                    |> Result.bind (fun before -> intervalSegments subpath first toParameter |> Result.bind (fun after -> create (before @ after)))))

    let split subpath at =
        if subpath.isClosed then Error AlreadyClosed
        else
            parameterCanonicalize subpath at
            |> Result.bind (fun at ->
                let count = List.length subpath.segmentList
                if (at.SegmentIndex = 0 && at.T = 0.0<parameter>)
                   || (at.SegmentIndex = count - 1 && at.T = 1.0<parameter>) then
                    Error(InvalidSubpathParameter(at.SegmentIndex, at.T, count))
                else
                    between subpath { SegmentIndex = 0; T = 0.0<parameter> } at
                    |> Result.bind (fun left ->
                        between subpath at { SegmentIndex = count - 1; T = 1.0<parameter> }
                        |> Result.map (fun right -> left, right)))

    let openAt subpath at =
        if not subpath.isClosed then Error NotClosed
        else
            parameterCanonicalize subpath at
            |> Result.bind (fun at ->
                let last = { SegmentIndex = List.length subpath.segmentList - 1; T = 1.0<parameter> }
                let first = { SegmentIndex = 0; T = 0.0<parameter> }
                intervalSegments subpath at last
                |> Result.bind (fun before -> intervalSegments subpath first at |> Result.bind (fun after -> create (before @ after))))

    let betweenMany subpath points =
        points
        |> List.fold (fun state point ->
            state |> Result.bind (fun validated -> parameterCanonicalize subpath point |> Result.map (fun value -> value :: validated))) (Ok [])
        |> Result.bind (fun reversed ->
            let points = List.rev reversed
            let makePairs values = values |> List.pairwise
            let build pairs =
                pairs
                |> List.fold (fun state (fromParameter, toParameter) ->
                    state |> Result.bind (fun paths -> between subpath fromParameter toParameter |> Result.map (fun path -> path :: paths))) (Ok [])
                |> Result.map List.rev
            let length = List.length subpath.segmentList
            let startParameter = { SegmentIndex = 0; T = 0.0<parameter> }
            let endParameter = { SegmentIndex = length - 1; T = 1.0<parameter> }
            let isBoundary parameterValue =
                parametersCompare parameterValue startParameter = 0
                || parametersCompare parameterValue endParameter = 0
            let invalidParameter parameterValue =
                Error(InvalidSubpathParameter(parameterValue.SegmentIndex, parameterValue.T, length))
            let rec validateOpen previous = function
                | [] -> Ok()
                | point :: rest when isBoundary point -> invalidParameter point
                | point :: rest when parametersCompare previous point < 0 -> validateOpen point rest
                | point :: _ -> Error(InvalidSubpathInterval(previous, point))
            let rec validateClosed first previous descents = function
                | [] ->
                    let order = parametersCompare previous first
                    if order = 0 then Error(InvalidSubpathInterval(previous, first))
                    else
                        let descents = if order > 0 then descents + 1 else descents
                        if descents = 1 then Ok()
                        else Error(InvalidSubpathInterval(previous, first))
                | point :: rest ->
                    let order = parametersCompare previous point
                    if order = 0 then Error(InvalidSubpathInterval(previous, point))
                    else
                        let descents = if order > 0 then descents + 1 else descents
                        if descents > 1 then Error(InvalidSubpathInterval(previous, point))
                        else validateClosed first point descents rest
            if not subpath.isClosed then
                match points with
                | [] -> Ok [ subpath ]
                | _ ->
                    match points with
                    | first :: rest when isBoundary first -> invalidParameter first
                    | first :: rest ->
                        validateOpen first rest
                        |> Result.bind (fun () -> build (makePairs (startParameter :: (points @ [ endParameter ]))))
                    | [] -> Ok [ subpath ]
            else
                match points with
                | [] -> Ok []
                | [ point ] -> openAt subpath point |> Result.map List.singleton
                | first :: second :: rest ->
                    validateClosed first first 0 (second :: rest)
                    |> Result.bind (fun () -> build (makePairs points @ [ List.last points, first ])))

    let projectionWith (subpath: Subpath) sample options =
        Segment.validateDistanceOptions options
        |> Result.bind (fun () ->
            let rec loop index (best: SubpathProjection option) (segments: Segment list) =
                match segments with
                | [] -> best |> Option.map Ok |> Option.defaultValue (Error EmptySubpath)
                | segment :: rest ->
                    Segment.projectionWith segment sample options
                    |> Result.bind (fun (t, point, distance) ->
                        let candidate: SubpathProjection =
                            { At = { SegmentIndex = index; T = t }
                              Point = point
                              Distance = distance }
                        let best =
                            match best with
                            | None -> Some candidate
                            | Some current when candidate.Distance < current.Distance -> Some candidate
                            | _ -> best
                        loop (index + 1) best rest)
            loop 0 None subpath.segmentList)

    let projection subpath sample = projectionWith subpath sample Segment.defaultDistanceOptions

    let distanceWith subpath sample options =
        projectionWith subpath sample options |> Result.map _.Distance

    let distance subpath sample = distanceWith subpath sample Segment.defaultDistanceOptions

    let lengthWith subpath (options: LengthOptions) =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun total ->
                Segment.lengthWith segment options |> Result.map (fun value -> total + value))) (Ok 0.0<length>)

    let length subpath = lengthWith subpath Segment.defaultLengthOptions

    let parameterAtLengthWith subpath distance (options: LengthOptions) =
        lengthWith subpath options
        |> Result.bind (fun total ->
            if List.isEmpty subpath.segmentList then Error EmptySubpath
            elif not (System.Double.IsFinite(float distance)) || distance < 0.0<length> || distance > total then
                Error(InvalidLengthDistance(distance, total))
            else
                let rec locate index remaining segments =
                    match segments with
                    | [] -> Ok { SegmentIndex = subpath.segmentList.Length - 1; T = 1.0<parameter> }
                    | segment :: rest ->
                        Segment.lengthWith segment options
                        |> Result.bind (fun segmentLength ->
                            if remaining <= segmentLength || List.isEmpty rest then
                                Segment.parameterAtLengthWith segment remaining options
                                |> Result.map (fun t -> { SegmentIndex = index; T = t })
                            else locate (index + 1) (remaining - segmentLength) rest)
                locate 0 distance subpath.segmentList)

    let parameterAtLength subpath distance =
        parameterAtLengthWith subpath distance Segment.defaultLengthOptions

    let pointAtLengthWith subpath distance (options: LengthOptions) =
        parameterAtLengthWith subpath distance options |> Result.bind (point subpath)

    let pointAtLength subpath distance = pointAtLengthWith subpath distance Segment.defaultLengthOptions

    let derivativeAtLengthWith subpath distance (options: LengthOptions) =
        parameterAtLengthWith subpath distance options |> Result.bind (derivative subpath)

    let derivativeAtLength subpath distance =
        derivativeAtLengthWith subpath distance Segment.defaultLengthOptions

    let isZeroLength subpath tolerance =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidZeroLengthTolerance tolerance)
        elif List.isEmpty subpath.segmentList then Ok false
        else
            subpath.segmentList
            |> List.fold (fun state segment ->
                state |> Result.bind (fun allZero -> if not allZero then Ok false else Segment.isZeroLength segment tolerance)) (Ok true)

    let betweenLengthsWith subpath fromDistance toDistance options =
        parameterAtLengthWith subpath fromDistance options
        |> Result.bind (fun fromParameter ->
            parameterAtLengthWith subpath toDistance options
            |> Result.bind (fun toParameter -> between subpath fromParameter toParameter))

    let betweenLengths subpath fromDistance toDistance =
        betweenLengthsWith subpath fromDistance toDistance Segment.defaultLengthOptions

    let betweenLengthsManyWith subpath distances options =
        distances
        |> List.fold (fun state distance ->
            state |> Result.bind (fun parameters -> parameterAtLengthWith subpath distance options |> Result.map (fun value -> value :: parameters))) (Ok [])
        |> Result.bind (List.rev >> betweenMany subpath)

    let betweenLengthsMany subpath distances =
        betweenLengthsManyWith subpath distances Segment.defaultLengthOptions

    let subdivideToMaxLengthWith subpath maxLength options =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun pieces ->
                Segment.subdivideToMaxLengthWith segment maxLength options
                |> Result.map (fun next -> pieces @ next))) (Ok [])
        |> Result.map (fun segments -> { subpath with segmentList = segments })

    let subdivideToMaxLength subpath maxLength =
        subdivideToMaxLengthWith subpath maxLength Segment.defaultLengthOptions

    let toLinesWith options subpath =
        subpath.segmentList
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun lines -> Segment.toLinesWith options segment |> Result.map (fun next -> lines @ next))) (Ok [])
        |> Result.map (fun segments -> { subpath with segmentList = segments })

    let toLines subpath = toLinesWith Segment.defaultLinearizeOptions subpath

[<RequireQualifiedAccess>]
module Path =
    let empty = { subpathList = [] }
    let ofSubpaths subpaths = { subpathList = subpaths }
    let subpaths path = path.subpathList
    let singleton subpath = { subpathList = [ subpath ] }
    let asSubpath path =
        match path.subpathList with
        | [] -> Error EmptySubpaths
        | subpaths ->
            match subpaths |> List.filter (fun subpath -> not (List.isEmpty subpath.Segments)) with
            | [] -> Ok(List.head subpaths)
            | [ subpath ] -> Ok subpath
            | _ -> Error MultipleNonemptySubpaths
    let append subpath path = { subpathList = path.subpathList @ [ subpath ] }
    let appendSubpath path subpath = append subpath path
    let combine paths = { subpathList = paths |> List.collect (fun path -> path.subpathList) }
    let mapSubpaths mapping path = { subpathList = List.map mapping path.subpathList }
    let filterSubpaths predicate path = { subpathList = List.filter predicate path.subpathList }
    let reverse path = { subpathList = path.subpathList |> List.rev |> List.map Subpath.reverse }

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

    let byPointPairSimilarity path sourceStart sourceEnd targetStart targetEnd =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun mapped ->
                Subpath.byPointPairSimilarity subpath sourceStart sourceEnd targetStart targetEnd
                |> Result.map (fun next -> next :: mapped))) (Ok [])
        |> Result.map (fun reversed -> { subpathList = List.rev reversed })

    let arcsToCubicBeziers path =
        { subpathList = path.subpathList |> List.map Subpath.arcsToCubicBeziers }

    let toCubicBeziers path =
        { subpathList = path.subpathList |> List.map Subpath.toCubicBeziers }

    let start path =
        match path.subpathList with
        | [] -> Error EmptyPath
        | first :: _ -> Ok(Subpath.start first)

    let finish path =
        match List.tryLast path.subpathList with
        | None -> Error EmptyPath
        | Some last -> Ok(Subpath.finish last)

    let ``end`` path = finish path

    let parametersCompare left right =
        let subpathOrder = compare left.SubpathIndex right.SubpathIndex
        if subpathOrder <> 0 then subpathOrder else Subpath.parametersCompare left.At right.At

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

    let directionsWith path parameterValue options =
        if parameterValue.SubpathIndex < 0 || parameterValue.SubpathIndex >= List.length path.subpathList then
            Error(InvalidPathParameter(parameterValue.SubpathIndex, List.length path.subpathList))
        else Subpath.directionsWith path.subpathList[parameterValue.SubpathIndex] parameterValue.At options

    let directions path parameterValue = directionsWith path parameterValue Segment.defaultDirectionOptions

    let lengthWith path (options: LengthOptions) =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun total ->
                Subpath.lengthWith subpath options |> Result.map (fun value -> total + value))) (Ok 0.0<length>)

    let length path = lengthWith path Segment.defaultLengthOptions

    let parameterAtLengthWith path distance (options: LengthOptions) =
        lengthWith path options
        |> Result.bind (fun total ->
            if List.isEmpty path.subpathList then Error EmptyPath
            elif not (System.Double.IsFinite(float distance)) || distance < 0.0<length> || distance > total then
                Error(InvalidLengthDistance(distance, total))
            else
                let nonempty =
                    path.subpathList
                    |> List.indexed
                    |> List.filter (fun (_, subpath) -> not (List.isEmpty subpath.Segments))
                match nonempty with
                | [] -> Error EmptySubpaths
                | _ ->
                    let rec locate remaining candidates =
                        match candidates with
                        | [] ->
                            let index, subpath = List.last nonempty
                            Ok { SubpathIndex = index; At = { SegmentIndex = subpath.Segments.Length - 1; T = 1.0<parameter> } }
                        | (index, subpath) :: rest ->
                            Subpath.lengthWith subpath options
                            |> Result.bind (fun subpathLength ->
                                if remaining <= subpathLength || List.isEmpty rest then
                                    Subpath.parameterAtLengthWith subpath remaining options
                                    |> Result.map (fun at -> { SubpathIndex = index; At = at })
                                else locate (remaining - subpathLength) rest)
                    locate distance nonempty)

    let parameterAtLength path distance =
        parameterAtLengthWith path distance Segment.defaultLengthOptions

    let pointAtLengthWith path distance (options: LengthOptions) =
        parameterAtLengthWith path distance options |> Result.bind (point path)

    let pointAtLength path distance = pointAtLengthWith path distance Segment.defaultLengthOptions

    let derivativeAtLengthWith path distance (options: LengthOptions) =
        parameterAtLengthWith path distance options |> Result.bind (derivative path)

    let derivativeAtLength path distance =
        derivativeAtLengthWith path distance Segment.defaultLengthOptions

    let projectionWith (path: Path) sample options =
        match path.subpathList with
        | [] -> Error EmptyPath
        | subpaths ->
            Segment.validateDistanceOptions options
            |> Result.bind (fun () ->
                let rec loop index (best: PathProjection option) (remaining: Subpath list) =
                    match remaining with
                    | [] -> best |> Option.map Ok |> Option.defaultValue (Error EmptySubpaths)
                    | subpath :: rest when List.isEmpty subpath.Segments -> loop (index + 1) best rest
                    | subpath :: rest ->
                        Subpath.projectionWith subpath sample options
                        |> Result.bind (fun (projection: SubpathProjection) ->
                            let candidate: PathProjection =
                                { At = { SubpathIndex = index; At = projection.At }
                                  Point = projection.Point
                                  Distance = projection.Distance }
                            let best =
                                match best with
                                | None -> Some candidate
                                | Some current when candidate.Distance < current.Distance -> Some candidate
                                | _ -> best
                            loop (index + 1) best rest)
                loop 0 None subpaths)

    let projection path sample = projectionWith path sample Segment.defaultDistanceOptions

    let distanceWith path sample options =
        projectionWith path sample options |> Result.map _.Distance

    let distance path sample = distanceWith path sample Segment.defaultDistanceOptions

    let subdivideToMaxLengthWith path maxLength options =
        path.subpathList
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun subpaths ->
                Subpath.subdivideToMaxLengthWith subpath maxLength options
                |> Result.map (fun next -> next :: subpaths))) (Ok [])
        |> Result.map (fun reversed -> { subpathList = List.rev reversed })

    let subdivideToMaxLength path maxLength =
        subdivideToMaxLengthWith path maxLength Segment.defaultLengthOptions

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

    let toLines path = toLinesWith Segment.defaultLinearizeOptions path
