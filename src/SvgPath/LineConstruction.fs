namespace SvgPath

[<RequireQualifiedAccess>]
module internal LineConstruction =
    // Preserve adjacent-point filtering; do not bridge across skipped points.
    let rec segments tolerance points =
        match points with
        | []
        | [ _ ] -> []
        | first :: second :: rest ->
            let tail = segments tolerance (second :: rest)
            if Point.near tolerance first second then tail
            else Line(first, second) :: tail
