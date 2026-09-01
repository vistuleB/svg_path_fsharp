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
    | InvalidLinearizeTolerance of float<length>
    | InvalidLinearizeMaxDepth of int
    | LinearizeMaxDepthReached of float<length>

[<Struct>]
type Subpath =
    { Start: Point<length>
      Segments: Segment list
      Closed: bool }

[<Struct>]
type Path = { Subpaths: Subpath list }

type FillRule =
    | Nonzero
    | EvenOdd

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
    let toLinesWith options subpath =
        subpath.Segments
        |> List.fold (fun state segment ->
            state
            |> Result.bind (fun lines -> Segment.toLinesWith options segment |> Result.map (fun next -> lines @ next))) (Ok [])
        |> Result.map (fun segments -> { subpath with Segments = segments })

[<RequireQualifiedAccess>]
module Path =
    let toLinesWith options path =
        path.Subpaths
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (fun subpaths -> Subpath.toLinesWith options subpath |> Result.map (fun next -> next :: subpaths))) (Ok [])
        |> Result.map (fun subpaths -> { Subpaths = List.rev subpaths })
