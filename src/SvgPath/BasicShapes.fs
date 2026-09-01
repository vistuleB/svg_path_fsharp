namespace SvgPath

type BasicShapeError =
    | InvalidRectWidth of float<length>
    | InvalidRectHeight of float<length>
    | InvalidRectRadiusX of float<length>
    | InvalidRectRadiusY of float<length>
    | InvalidCircleRadius of float<length>
    | InvalidEllipseRadiusX of float<length>
    | InvalidEllipseRadiusY of float<length>
    | DisabledRendering
    | PathError of SegmentError

[<RequireQualifiedAccess>]
module BasicShapes =
    let private degrees value = Degree.fromFloat value

    let private closed segments =
        Subpath.create segments
        |> Result.bind (Subpath.setClosed true)
        |> Result.mapError PathError

    let private radii
        (width: float<length>)
        (height: float<length>)
        (rx: float<length> option)
        (ry: float<length> option) =
        let rx, ry =
            match rx, ry with
            | None, None -> 0.0<length>, 0.0<length>
            | Some radius, None -> radius, radius
            | None, Some radius -> radius, radius
            | Some rx, Some ry -> rx, ry
        if rx < 0.0<length> then Error(InvalidRectRadiusX rx)
        elif ry < 0.0<length> then Error(InvalidRectRadiusY ry)
        else Ok(min rx (width / 2.0), min ry (height / 2.0))

    let rect
        (x: float<length>)
        (y: float<length>)
        (width: float<length>)
        (height: float<length>)
        (rx: float<length> option)
        (ry: float<length> option) =
        if width < 0.0<length> then Error(InvalidRectWidth width)
        elif height < 0.0<length> then Error(InvalidRectHeight height)
        elif width = 0.0<length> || height = 0.0<length> then Error DisabledRendering
        else
            radii width height rx ry
            |> Result.bind (fun (rx, ry) ->
                let x2, y2 = x + width, y + height
                let startPoint = Point.create (x + rx) y
                if rx > 0.0<length> && ry > 0.0<length> then
                    let radius = Point.create rx ry
                    [ Line(startPoint, Point.create (x2 - rx) y)
                      Arc { Start = Point.create (x2 - rx) y; Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create x2 (y + ry) }
                      Line(Point.create x2 (y + ry), Point.create x2 (y2 - ry))
                      Arc { Start = Point.create x2 (y2 - ry); Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create (x2 - rx) y2 }
                      Line(Point.create (x2 - rx) y2, Point.create (x + rx) y2)
                      Arc { Start = Point.create (x + rx) y2; Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create x (y2 - ry) }
                      Line(Point.create x (y2 - ry), Point.create x (y + ry))
                      Arc { Start = Point.create x (y + ry); Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = startPoint } ]
                    |> closed
                else
                    [ Line(startPoint, Point.create x2 y)
                      Line(Point.create x2 y, Point.create x2 y2)
                      Line(Point.create x2 y2, Point.create x y2)
                      Line(Point.create x y2, startPoint) ]
                    |> closed)

    let ellipse cx cy rx ry =
        if rx < 0.0<length> then Error(InvalidEllipseRadiusX rx)
        elif ry < 0.0<length> then Error(InvalidEllipseRadiusY ry)
        elif rx = 0.0<length> || ry = 0.0<length> then Error DisabledRendering
        else
            let startPoint = Point.create (cx + rx) cy
            let radius = Point.create rx ry
            [ Arc { Start = startPoint; Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create cx (cy + ry) }
              Arc { Start = Point.create cx (cy + ry); Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create (cx - rx) cy }
              Arc { Start = Point.create (cx - rx) cy; Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = Point.create cx (cy - ry) }
              Arc { Start = Point.create cx (cy - ry); Radius = radius; XAxisRotation = degrees 0.0; LargeArc = false; Sweep = true; End = startPoint } ]
            |> closed

    let circle cx cy radius =
        if radius < 0.0<length> then Error(InvalidCircleRadius radius)
        elif radius = 0.0<length> then Error DisabledRendering
        else ellipse cx cy radius radius

    let line x1 y1 x2 y2 =
        let startPoint, endPoint = Point.create x1 y1, Point.create x2 y2
        Subpath.create [ Line(startPoint, endPoint) ] |> Result.mapError PathError

    let polyline points =
        match points with
        | [] | [ _ ] -> Error(PathError EmptySubpath)
        | _ -> Subpath.polyline points |> Result.mapError PathError

    let polygon points =
        match points with
        | [] | [ _ ] -> Error(PathError EmptySubpath)
        | _ -> Subpath.polygon points |> Result.mapError PathError
