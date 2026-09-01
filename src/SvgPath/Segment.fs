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

[<RequireQualifiedAccess>]
module Segment =
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
