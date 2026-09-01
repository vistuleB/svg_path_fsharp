namespace SvgPath

type private AreaEdge = { Start: Point<length>; Finish: Point<length> }
type private Crossing = { Edge: AreaEdge; Y: float<length>; Winding: int }
type private CrossingGroup = { Edge: AreaEdge; Y: float<length>; Winding: int; Crossings: int }
type private AreaMode = FillRuleArea of FillRule | AbsoluteWindingArea

[<RequireQualifiedAccess>]
module Area =
    let private relativeTolerance = 1.0e-12

    let signedPoints points : float<length^2> =
        match points with
        | [] | [ _ ] | [ _; _ ] -> 0.0<length^2>
        | origin :: rest ->
            let rebased = rest |> List.map (fun point -> Point.displacement origin point)
            rebased
            |> List.fold
                (fun (area, previous) point -> area + Point.cross previous point, point)
                (0.0<length^2>, Point.create 0.0<length> 0.0<length>)
            |> fst
            |> fun integral -> integral / 2.0

    let private combine terms =
        terms
        |> List.fold (fun sum (point: Point<length>, scale) -> Point.add sum (Point.scale scale point)) (Point.create 0.0<length> 0.0<length>)

    let private quadraticSigned startPoint control endPoint =
        let a = combine [ startPoint, 1.0; control, -2.0; endPoint, 1.0 ]
        let b = combine [ control, 2.0; startPoint, -2.0 ]
        (-Point.cross a b / 3.0 + Point.cross startPoint a + Point.cross startPoint b) / 2.0

    let private cubicSigned startPoint control1 control2 endPoint =
        let a = combine [ startPoint, -1.0; control1, 3.0; control2, -3.0; endPoint, 1.0 ]
        let b = combine [ startPoint, 3.0; control1, -6.0; control2, 3.0 ]
        let c = combine [ control1, 3.0; startPoint, -3.0 ]
        (-Point.cross a b / 5.0
         - Point.cross a c / 2.0
         + (-Point.cross b c + 3.0 * Point.cross startPoint a) / 3.0
         + Point.cross startPoint b
         + Point.cross startPoint c) / 2.0

    let signedSegment segment : float<length^2> =
        match segment with
        | Line(startPoint, endPoint) -> Point.cross startPoint endPoint / 2.0
        | QuadraticBezier(startPoint, control, endPoint) -> quadraticSigned startPoint control endPoint
        | CubicBezier(startPoint, control1, control2, endPoint) -> cubicSigned startPoint control1 control2 endPoint
        | Arc endpoint ->
            match Ellipse.endpointToCenter endpoint with
            | Error _ -> Point.cross endpoint.Start endpoint.End / 2.0
            | Ok arc ->
                let delta = Degree.toRadians arc.DeltaAngle |> Radian.toFloat
                (Point.cross arc.Center (Point.displacement endpoint.Start endpoint.End)
                 + arc.Radius.X * arc.Radius.Y * delta) / 2.0

    let private rebaseSegment origin segment =
        let rebase point = Point.displacement origin point
        match segment with
        | Line(startPoint, endPoint) -> Line(rebase startPoint, rebase endPoint)
        | QuadraticBezier(startPoint, control, endPoint) -> QuadraticBezier(rebase startPoint, rebase control, rebase endPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) -> CubicBezier(rebase startPoint, rebase control1, rebase control2, rebase endPoint)
        | Arc endpoint -> Arc { endpoint with Start = rebase endpoint.Start; End = rebase endpoint.End }

    let signedSubpath subpath =
        match Subpath.segments subpath with
        | [] -> 0.0<length^2>
        | segments -> segments |> List.sumBy (rebaseSegment (Subpath.start subpath) >> signedSegment)

    let signedPath path = Path.subpaths path |> List.sumBy signedSubpath

    let private addEdge startPoint endPoint edges =
        if startPoint = endPoint then edges else { Start = startPoint; Finish = endPoint } :: edges

    let private edges path =
        Path.subpaths path
        |> List.fold (fun edges subpath ->
            match Subpath.segments subpath with
            | [] -> edges
            | segments ->
                let edges = segments |> List.fold (fun edges segment -> addEdge (Segment.start segment) (Segment.finish segment) edges) edges
                let finish = segments |> List.last |> Segment.finish
                addEdge finish (Subpath.start subpath) edges) []

    let private edgeIntersectionX left right =
        let leftDirection = Point.displacement left.Start left.Finish
        let rightDirection = Point.displacement right.Start right.Finish
        let denominator = Point.cross leftDirection rightDirection
        let denominatorTolerance = relativeTolerance * Point.norm leftDirection * Point.norm rightDirection
        if abs denominator <= denominatorTolerance then None
        else
            let offset = Point.displacement left.Start right.Start
            let leftT = Point.cross offset rightDirection / denominator
            let rightT = Point.cross offset leftDirection / denominator
            if leftT >= -relativeTolerance && leftT <= 1.0 + relativeTolerance
               && rightT >= -relativeTolerance && rightT <= 1.0 + relativeTolerance then
                Some(left.Start.X + max 0.0 (min 1.0 leftT) * leftDirection.X)
            else None

    let private arrangementTolerance edges =
        let points = edges |> List.collect (fun edge -> [ edge.Start; edge.Finish ])
        let minX = points |> List.minBy _.X |> _.X
        let maxX = points |> List.maxBy _.X |> _.X
        let minY = points |> List.minBy _.Y |> _.Y
        let maxY = points |> List.maxBy _.Y |> _.Y
        max (maxX - minX) (maxY - minY) * relativeTolerance

    let private dedupeSorted tolerance values =
        values
        |> List.sort
        |> List.fold (fun kept value ->
            match kept with
            | previous :: _ when abs (value - previous) <= tolerance -> kept
            | _ -> value :: kept) []
        |> List.rev

    let private arrangementXs edges tolerance =
        let endpoints = edges |> List.collect (fun edge -> [ edge.Start.X; edge.Finish.X ])
        let intersections =
            edges
            |> List.mapi (fun index edge -> edges |> List.skip (index + 1) |> List.choose (edgeIntersectionX edge))
            |> List.concat
        dedupeSorted tolerance (endpoints @ intersections)

    let private edgeYAt edge x =
        let dx = edge.Finish.X - edge.Start.X
        if dx = 0.0<length> then edge.Start.Y
        else edge.Start.Y + (edge.Finish.Y - edge.Start.Y) * ((x - edge.Start.X) / dx)

    let private crossingGroups edges x tolerance =
        let crossings =
            edges
            |> List.choose (fun edge ->
                let minX, maxX = min edge.Start.X edge.Finish.X, max edge.Start.X edge.Finish.X
                if x <= minX || x >= maxX then None
                else Some { Edge = edge; Y = edgeYAt edge x; Winding = if edge.Finish.X > edge.Start.X then 1 else -1 })
            |> List.sortBy _.Y
        crossings
        |> List.fold (fun groups crossing ->
            match groups with
            | previous :: rest when abs (crossing.Y - previous.Y) <= tolerance ->
                { previous with Winding = previous.Winding + crossing.Winding; Crossings = previous.Crossings + 1 } :: rest
            | _ -> { Edge = crossing.Edge; Y = crossing.Y; Winding = crossing.Winding; Crossings = 1 } :: groups) []
        |> List.rev

    let private intervalWeight winding crossings mode =
        match mode with
        | AbsoluteWindingArea -> float (abs winding)
        | FillRuleArea Nonzero -> if winding <> 0 then 1.0 else 0.0
        | FillRuleArea EvenOdd -> if crossings % 2 = 1 then 1.0 else 0.0

    let private intervalArea lower upper left right =
        let leftHeight = max 0.0<length> (edgeYAt upper left - edgeYAt lower left)
        let rightHeight = max 0.0<length> (edgeYAt upper right - edgeYAt lower right)
        (right - left) * (leftHeight + rightHeight) / 2.0

    let private slabArea groups left right mode =
        let folder (area, winding, crossings, previous) current =
            let area =
                match previous with
                | Some lower -> area + intervalWeight winding crossings mode * intervalArea lower.Edge current.Edge left right
                | None -> area
            area, winding + current.Winding, crossings + current.Crossings, Some current
        groups |> List.fold folder (0.0<length^2>, 0, 0, None) |> fun (area, _, _, _) -> area

    let private arrangementArea path mode options =
        Path.toLinesWith options path
        |> Result.map (fun linearized ->
            let edges = edges linearized
            match edges with
            | [] -> 0.0<length^2>
            | _ ->
                let tolerance = arrangementTolerance edges
                arrangementXs edges tolerance
                |> List.pairwise
                |> List.sumBy (fun (left, right) ->
                    if right <= left then 0.0<length^2>
                    else crossingGroups edges ((left + right) / 2.0) tolerance |> fun groups -> slabArea groups left right mode))

    let absolutePathWith path options = arrangementArea path AbsoluteWindingArea options
    let absolutePath path = absolutePathWith path Segment.defaultLinearizeOptions
    let pathWith path fillRule options = arrangementArea path (FillRuleArea fillRule) options
    let path pathValue fillRule = pathWith pathValue fillRule Segment.defaultLinearizeOptions

    let private asPath subpath = Path.singleton subpath
    let absoluteSubpathWith subpath options = absolutePathWith (asPath subpath) options
    let absoluteSubpath subpath = absoluteSubpathWith subpath Segment.defaultLinearizeOptions
    let subpathWith subpath fillRule options = pathWith (asPath subpath) fillRule options
    let subpath subpathValue fillRule = subpathWith subpathValue fillRule Segment.defaultLinearizeOptions

    let subpathClockwisenessWith subpath options =
        let signed = signedSubpath subpath
        absoluteSubpathWith subpath options
        |> Result.map (fun absolute ->
            if absolute <= 0.0<length^2> then 0.5
            else max 0.0 (min 1.0 (0.5 + signed / (2.0 * absolute))))

    let subpathClockwiseness subpath = subpathClockwisenessWith subpath Segment.defaultLinearizeOptions
