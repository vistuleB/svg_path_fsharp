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
    | InvalidSubpathParameter of segmentIndex: int * t: float<parameter> * length: int

[<Struct>]
type Subpath =
    { Start: Point<length>
      Segments: Segment list
      Closed: bool }

[<Struct>]
type Path = { Subpaths: Subpath list }

[<Struct>]
type SubpathParameter =
    { SegmentIndex: int
      T: float<parameter> }

[<Struct>]
type PathParameter =
    { SubpathIndex: int
      At: SubpathParameter }

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
    let segments subpath = subpath.Segments
    let start subpath = if List.isEmpty subpath.Segments then None else Some subpath.Start
    let finish subpath = subpath.Segments |> List.tryLast |> Option.map Segment.finish
    let boundingBox subpath =
        match subpath.Segments with
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
        let length = List.length subpath.Segments
        if length = 0 then Error EmptySubpath
        elif parameter.SegmentIndex < 0 || parameter.SegmentIndex >= length
             || parameter.T < 0.0<parameter> || parameter.T > 1.0<parameter> then
            Error(InvalidSubpathParameter(parameter.SegmentIndex, parameter.T, length))
        elif parameter.T = 1.0<parameter> && parameter.SegmentIndex + 1 < length then
            Ok { SegmentIndex = parameter.SegmentIndex + 1; T = 0.0<parameter> }
        elif subpath.Closed && parameter.T = 1.0<parameter> && parameter.SegmentIndex = length - 1 then
            Ok { SegmentIndex = 0; T = 0.0<parameter> }
        else Ok parameter
    let toLinesWith options subpath =
        subpath.Segments
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun lines -> Segment.toLinesWith options segment |> Result.map (fun next -> lines @ next))) (Ok [])
        |> Result.map (fun segments -> { subpath with Segments = segments })

[<RequireQualifiedAccess>]
module Path =
    let boundingBox path =
        match path.Subpaths with
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
        path.Subpaths
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun subpaths -> Subpath.toLinesWith options subpath |> Result.map (fun next -> next :: subpaths))) (Ok [])
        |> Result.map (fun subpaths -> { Subpaths = List.rev subpaths })
