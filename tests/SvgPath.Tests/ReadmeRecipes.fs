module ReadmeRecipes

open SvgPath

// Fit on 0..1, then retain the middle half by traveled distance.
let middleHalf (point: float<parameter> -> Point<length>) =
    Fit.subpathFromParametric 0.0<parameter> 1.0<parameter> point
    |> Result.bind (fun curve ->
        Measure.subpathLength curve
        |> Result.bind (fun length ->
            Measure.subpathBetweenLengths curve (length * 0.25) (length * 0.75)))

// Recover a nearest address and its derivative. The derivative may be zero
// at a singularity and is not a unit tangent.
let nearestLocation point path =
    Distance.pathProjection path point
    |> Result.bind (fun projection ->
        Path.derivative path projection.At
        |> Result.map (fun derivative -> projection, derivative))

// Preserve diagnostics when composing operations with different error types.
type OutlineUnionError =
    | StrokeFailure of error: Stroke.Error
    | BooleanFailure of error: Csg.Error

let outlineUnion centerlines width filledRegion =
    Stroke.path centerlines width Offset.Round Offset.Butt
    |> Result.mapError StrokeFailure
    |> Result.bind (fun outline ->
        Csg.unionPath outline filledRegion Nonzero
        |> Result.mapError BooleanFailure)
