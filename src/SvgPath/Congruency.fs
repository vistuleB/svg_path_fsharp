namespace SvgPath

type TransformFamily =
    | Similar
    | Affine

[<Struct>]
type CongruencyTolerance =
    { Distance: float<length>
      Angle: float<degree> }

[<Struct>]
type CongruencyFit =
    { Transform: Affine
      Error: float<length> }

[<RequireQualifiedAccess>]
module Congruency =
    let private defaultAngleTolerance = 1.0e-9<degree>

    type private IndexedPoint =
        { Source: Point<length>
          Target: Point<length> }

    type private PointCloud =
        { SourcePoints: Point<length> list
          TargetPoints: Point<length> list
          HasArc: bool }

    let private validTolerance tolerance =
        tolerance.Distance >= 0.0<length>
        && System.Double.IsFinite(float tolerance.Distance)
        && tolerance.Angle >= 0.0<degree>
        && System.Double.IsFinite(float tolerance.Angle)

    let private indexedPoints source target =
        if List.length source <> List.length target then Error()
        else Ok(List.map2 (fun sourcePoint targetPoint -> { Source = sourcePoint; Target = targetPoint }) source target)

    let private distanceScale (a: Point<length>) (b: Point<length>) =
        max (abs (a.X - b.X)) (abs (a.Y - b.Y))

    let private farthestFrom point points =
        points |> List.maxBy (fun candidate -> distanceScale point.Source candidate.Source)

    let private sweptPair points =
        let first = List.head points
        let second = farthestFrom first points
        let third = farthestFrom second points
        second, third

    let private checkPoints points transform tolerance =
        if points |> List.forall (fun pair -> Point.near tolerance (Affine.point transform pair.Source) pair.Target) then Ok transform
        else Error()

    /// Find a translation, rotation, and uniform scale mapping corresponding points.
    let points source target (tolerance: float<length>) =
        if tolerance < 0.0<length> || not (System.Double.IsFinite(float tolerance)) then Error()
        else
            indexedPoints source target
            |> Result.bind (function
                | [] -> Error()
                | [ only ] ->
                    Affine.translate (only.Target.X - only.Source.X) (only.Target.Y - only.Source.Y)
                    |> checkPoints [ only ] <| tolerance
                | indexed ->
                    let first, second = sweptPair indexed
                    if distanceScale first.Source second.Source <= 0.0<length> then
                        Affine.translate (first.Target.X - first.Source.X) (first.Target.Y - first.Source.Y)
                        |> checkPoints indexed <| tolerance
                    else
                        Transform.pointPairMap
                            first.Source second.Source first.Target second.Target tolerance
                        |> Result.bind (fun transform -> checkPoints indexed transform tolerance))

    let private centroids points =
        match points with
        | [] -> Error()
        | _ ->
            let count = float (List.length points)
            let sum source =
                points
                |> List.fold (fun (x, y) pair ->
                    let point = source pair
                    x + point.X, y + point.Y) (0.0<length>, 0.0<length>)
                |> fun (x, y) -> Point.create (x / count) (y / count)
            Ok(sum _.Source, sum _.Target)

    let private rmsError points transform =
        let accumulate (scale, scaledSquares, finite) error =
            let magnitude = abs (float error)
            if not (System.Double.IsFinite magnitude) then scale, scaledSquares, false
            elif magnitude = 0.0 then scale, scaledSquares, finite
            elif scale = 0.0 then magnitude, 1.0, finite
            elif magnitude > scale then
                let ratio = scale / magnitude
                magnitude, 1.0 + scaledSquares * ratio * ratio, finite
            else
                let ratio = magnitude / scale
                scale, scaledSquares + ratio * ratio, finite
        let scale, squares, finite =
            points
            |> List.fold (fun state pair ->
                let mapped = Affine.point transform pair.Source
                state
                |> fun next -> accumulate next (mapped.X - pair.Target.X)
                |> fun next -> accumulate next (mapped.Y - pair.Target.Y)) (0.0, 0.0, true)
        if not finite || points.IsEmpty then Error()
        else
            let error = scale * sqrt (squares / float points.Length) |> Length.fromFloat
            if System.Double.IsFinite(float error) then Ok error else Error()

    let private fitFromMatrix points transform =
        if not (Affine.isFinite transform) then Error()
        else rmsError points transform |> Result.map (fun error -> { Transform = transform; Error = error })

    let private fitSimilar points =
        centroids points
        |> Result.bind (fun (sourceCenter, targetCenter) ->
            let dot, cross, sourceSquared =
                points
                |> List.fold (fun (dot, cross, sourceSquared) pair ->
                    let source = Point.displacement sourceCenter pair.Source
                    let target = Point.displacement targetCenter pair.Target
                    dot + Point.dot source target,
                    cross + Point.cross source target,
                    sourceSquared + Point.squaredNorm source) (0.0<length^2>, 0.0<length^2>, 0.0<length^2>)
            let transform =
                if sourceSquared <= 0.0<length^2> then
                    Affine.translate (targetCenter.X - sourceCenter.X) (targetCenter.Y - sourceCenter.Y)
                else
                    let scaleCos = float (dot / sourceSquared)
                    let scaleSin = float (cross / sourceSquared)
                    Affine.matrix
                        scaleCos scaleSin -scaleSin scaleCos
                        (targetCenter.X - (scaleCos * sourceCenter.X - scaleSin * sourceCenter.Y))
                        (targetCenter.Y - (scaleSin * sourceCenter.X + scaleCos * sourceCenter.Y))
            fitFromMatrix points transform)

    let private fitAffine points =
        centroids points
        |> Result.bind (fun (sourceCenter, targetCenter) ->
            let xx, xy, yy, txx, tyx, txy, tyy =
                points
                |> List.fold (fun (xx, xy, yy, txx, tyx, txy, tyy) pair ->
                    let source = Point.displacement sourceCenter pair.Source
                    let target = Point.displacement targetCenter pair.Target
                    xx + source.X * source.X,
                    xy + source.X * source.Y,
                    yy + source.Y * source.Y,
                    txx + source.X * target.X,
                    tyx + source.Y * target.X,
                    txy + source.X * target.Y,
                    tyy + source.Y * target.Y)
                    (0.0<length^2>, 0.0<length^2>, 0.0<length^2>, 0.0<length^2>, 0.0<length^2>, 0.0<length^2>, 0.0<length^2>)
            let determinant = xx * yy - xy * xy
            let determinantScale = xx * yy
            if not (System.Double.IsFinite(float determinant))
               || determinant <= 0.0<length^4>
               || determinant <= determinantScale * 1.0e-12 then fitSimilar points
            else
                let a = float ((txx * yy - tyx * xy) / determinant)
                let c = float ((xx * tyx - xy * txx) / determinant)
                let b = float ((txy * yy - tyy * xy) / determinant)
                let d = float ((xx * tyy - xy * txy) / determinant)
                Affine.matrix a b c d
                    (targetCenter.X - a * sourceCenter.X - c * sourceCenter.Y)
                    (targetCenter.Y - b * sourceCenter.X - d * sourceCenter.Y)
                |> fitFromMatrix points)

    let fitPoints source target family =
        indexedPoints source target
        |> Result.bind (function
            | [] -> Error()
            | indexed -> match family with Similar -> fitSimilar indexed | Affine -> fitAffine indexed)

    let private arcOppositePoint segment =
        match segment with
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.map (fun arc -> Ellipse.arcPointAtAngle arc (arc.StartAngle + 180.0<degree>))
            |> Result.mapError (fun _ -> ())
        | _ -> Error()

    let private segmentPoints source target =
        match source, target with
        | Line(_, sourceEnd), Line(_, targetEnd) -> Ok([ sourceEnd ], [ targetEnd ], false)
        | QuadraticBezier(_, sourceControl, sourceEnd), QuadraticBezier(_, targetControl, targetEnd) ->
            Ok([ sourceControl; sourceEnd ], [ targetControl; targetEnd ], false)
        | CubicBezier(_, sourceControl1, sourceControl2, sourceEnd),
          CubicBezier(_, targetControl1, targetControl2, targetEnd) ->
            Ok([ sourceControl1; sourceControl2; sourceEnd ], [ targetControl1; targetControl2; targetEnd ], false)
        | Arc sourceArc, Arc targetArc when sourceArc.LargeArc = targetArc.LargeArc && sourceArc.Sweep = targetArc.Sweep ->
            match arcOppositePoint source, arcOppositePoint target with
            | Ok sourceOpposite, Ok targetOpposite -> Ok([ sourceOpposite; sourceArc.End ], [ targetOpposite; targetArc.End ], true)
            | _ -> Error()
        | _ -> Error()

    let private segmentPointCloud source target =
        segmentPoints source target
        |> Result.map (fun (sourceExtra, targetExtra, hasArc) ->
            { SourcePoints = Segment.start source :: sourceExtra
              TargetPoints = Segment.start target :: targetExtra
              HasArc = hasArc })

    let private subpathPointCloud source target =
        let sourceSegments = Subpath.segments source
        let targetSegments = Subpath.segments target
        if List.length sourceSegments <> List.length targetSegments then Error()
        else
            List.zip sourceSegments targetSegments
            |> List.fold (fun state (sourceSegment, targetSegment) ->
                state
                |> Result.bind (fun cloud ->
                    segmentPoints sourceSegment targetSegment
                    |> Result.map (fun (sourceExtra, targetExtra, hasArc) ->
                        { SourcePoints = cloud.SourcePoints @ sourceExtra
                          TargetPoints = cloud.TargetPoints @ targetExtra
                          HasArc = cloud.HasArc || hasArc })))
                (Ok { SourcePoints = [ Subpath.start source ]; TargetPoints = [ Subpath.start target ]; HasArc = false })

    let private pathPointCloud source target =
        let sourceSubpaths = Path.subpaths source
        let targetSubpaths = Path.subpaths target
        if List.length sourceSubpaths <> List.length targetSubpaths then Error()
        else
            List.zip sourceSubpaths targetSubpaths
            |> List.fold (fun state (sourceSubpath, targetSubpath) ->
                state
                |> Result.bind (fun cloud ->
                    subpathPointCloud sourceSubpath targetSubpath
                    |> Result.map (fun next ->
                        { SourcePoints = cloud.SourcePoints @ next.SourcePoints
                          TargetPoints = cloud.TargetPoints @ next.TargetPoints
                          HasArc = cloud.HasArc || next.HasArc })))
                (Ok { SourcePoints = []; TargetPoints = []; HasArc = false })

    let private angleNear tolerance a b =
        let remainder = abs (Degree.toFloat (a - b)) % 180.0
        min remainder (180.0 - remainder) <= Degree.toFloat tolerance

    let private arcFieldMatch source target transform tolerance =
        match source, target with
        | Arc _, Arc targetArc ->
            match Transform.segment source transform with
            | Ok(Arc actual) ->
                let radiiMatch = Point.near tolerance.Distance actual.Radius targetArc.Radius
                let actualCircular = abs (actual.Radius.X - actual.Radius.Y) <= tolerance.Distance
                let targetCircular = abs (targetArc.Radius.X - targetArc.Radius.Y) <= tolerance.Distance
                let axesMatch =
                    if radiiMatch && actualCircular && targetCircular then true
                    elif radiiMatch then angleNear tolerance.Angle actual.XAxisRotation targetArc.XAxisRotation
                    else
                        Point.near tolerance.Distance actual.Radius (Point.create targetArc.Radius.Y targetArc.Radius.X)
                        && angleNear tolerance.Angle actual.XAxisRotation (targetArc.XAxisRotation + 90.0<degree>)
                actual.LargeArc = targetArc.LargeArc && actual.Sweep = targetArc.Sweep && axesMatch
            | _ -> false
        | _ -> true

    let private segmentPairsMatch source target transform tolerance =
        List.zip source target |> List.forall (fun (a, b) -> arcFieldMatch a b transform tolerance)

    let segmentWith source target tolerance =
        if not (validTolerance tolerance) then Error()
        else segmentPointCloud source target
        |> Result.bind (fun cloud ->
            points cloud.SourcePoints cloud.TargetPoints tolerance.Distance
            |> Result.bind (fun transform ->
                if not cloud.HasArc || arcFieldMatch source target transform tolerance then Ok transform else Error()))

    let segment source target (tolerance: float<length>) =
        segmentWith source target { Distance = tolerance; Angle = defaultAngleTolerance }

    let fitSegment source target family =
        segmentPointCloud source target |> Result.bind (fun cloud -> fitPoints cloud.SourcePoints cloud.TargetPoints family)

    let subpathWith source target tolerance =
        if not (validTolerance tolerance) then Error()
        else subpathPointCloud source target
        |> Result.bind (fun cloud ->
            points cloud.SourcePoints cloud.TargetPoints tolerance.Distance
            |> Result.bind (fun transform ->
                if not cloud.HasArc
                   || segmentPairsMatch (Subpath.segments source) (Subpath.segments target) transform tolerance then Ok transform
                else Error()))

    let subpath source target (tolerance: float<length>) =
        subpathWith source target { Distance = tolerance; Angle = defaultAngleTolerance }

    let fitSubpath source target family =
        subpathPointCloud source target |> Result.bind (fun cloud -> fitPoints cloud.SourcePoints cloud.TargetPoints family)

    let pathWith source target tolerance =
        if not (validTolerance tolerance) then Error()
        else pathPointCloud source target
        |> Result.bind (fun cloud ->
            points cloud.SourcePoints cloud.TargetPoints tolerance.Distance
            |> Result.bind (fun transform ->
                let arcsMatch =
                    List.zip (Path.subpaths source) (Path.subpaths target)
                    |> List.forall (fun (a, b) -> segmentPairsMatch (Subpath.segments a) (Subpath.segments b) transform tolerance)
                if not cloud.HasArc || arcsMatch then Ok transform else Error()))

    let path source target (tolerance: float<length>) =
        pathWith source target { Distance = tolerance; Angle = defaultAngleTolerance }

    let fitPath source target family =
        pathPointCloud source target |> Result.bind (fun cloud -> fitPoints cloud.SourcePoints cloud.TargetPoints family)
