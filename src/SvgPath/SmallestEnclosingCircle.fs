namespace SvgPath

[<Struct>]
type EnclosingCircle =
    { Center: Point<length>
      RadiusSquared: float<length^2> }

/// Smallest enclosing circles for finite point collections.
[<RequireQualifiedAccess>]
module SmallestEnclosingCircle =
    let private pointCircle sample =
        { Center = sample
          RadiusSquared = 0.0<length^2> }

    let private twoPointCircle first second =
        if first = second then pointCircle first
        else
            let center = Point.midpoint first second
            { Center = center
              RadiusSquared = Point.squaredDistance center first }

    let private contains circle sample =
        Point.squaredDistance circle.Center sample <= circle.RadiusSquared

    let private comparePoints first second =
        match compare first.X second.X with
        | 0 -> compare first.Y second.Y
        | ordering -> ordering

    let private compareCircles first second =
        match compare first.RadiusSquared second.RadiusSquared with
        | 0 -> comparePoints first.Center second.Center
        | ordering -> ordering

    let private farthestPairCircle first second third =
        [ twoPointCircle first second
          twoPointCircle first third
          twoPointCircle second third ]
        |> List.sortWith compareCircles
        |> List.last

    let private circumcircle first second third =
        // Work in coordinates relative to the first point. This avoids the
        // cancellation in the expanded absolute-coordinate formula when a
        // small configuration is translated far from the origin.
        let a = Point.displacement first second
        let b = Point.displacement first third
        let denominator = 2.0 * Point.cross a b
        if denominator = 0.0<length^2> then
            farthestPairCircle first second third
        else
            let aNorm = Point.squaredNorm a
            let bNorm = Point.squaredNorm b
            let offset =
                Point.create
                    ((aNorm * b.Y - bNorm * a.Y) / denominator)
                    ((a.X * bNorm - b.X * aNorm) / denominator)
            let center = Point.translate offset first
            { Center = center
              RadiusSquared = Point.squaredDistance center first }

    let private threePointCircle first second third =
        let samples = [ first; second; third ]
        let pairCircle =
            [ twoPointCircle first second
              twoPointCircle first third
              twoPointCircle second third ]
            |> List.filter (fun candidate -> samples |> List.forall (contains candidate))
            |> List.sortWith compareCircles
            |> List.tryHead
        pairCircle |> Option.defaultWith (fun () -> circumcircle first second third)

    let private enclosingWithTwo processed first second =
        (twoPointCircle first second, List.rev processed)
        ||> List.fold (fun circle third ->
            if contains circle third then circle else threePointCircle first second third)

    let private enclosingWithOne processed first =
        let folder (circle, seen) second =
            let circle =
                if contains circle second then circle
                else enclosingWithTwo seen first second
            circle, second :: seen
        List.rev processed
        |> List.fold folder (pointCircle first, [])
        |> fst

    let private enclosingLoop samples =
        match samples with
        | [] -> None
        | first :: rest ->
            let folder (circle, processed) sample =
                let circle =
                    if contains circle sample then circle
                    else enclosingWithOne processed sample
                circle, sample :: processed
            rest
            |> List.fold folder (pointCircle first, [ first ])
            |> fst
            |> Some

    let private withExactRadius circle samples =
        { circle with
            RadiusSquared =
                samples
                |> List.map (Point.squaredDistance circle.Center)
                |> List.fold max 0.0<length^2> }

    /// Return the deterministic smallest circle containing a non-empty point set.
    let points samples =
        let samples = samples |> List.sortWith comparePoints |> List.distinct
        match samples with
        | [] -> Error()
        | [ only ] -> Ok(pointCircle only)
        | [ first; second ] -> Ok(twoPointCircle first second)
        | _ ->
            enclosingLoop samples
            |> Option.map (fun circle -> withExactRadius circle samples)
            |> Option.map Ok
            |> Option.defaultValue (Error())
