namespace SvgPath

/// Trigonometry helpers for SVG-facing degree angles.
[<RequireQualifiedAccess>]
module Trig =
    let private positiveRemainder (value: float<degree>) (modulus: float<degree>) : float<degree> =
        let remainder = value % modulus
        if remainder < 0.0<degree> then remainder + modulus else remainder

    let private normalizedQuarterTurn (degrees: float<degree>) : float<degree> option =
        if not (System.Double.IsFinite(Degree.toFloat degrees)) then
            None
        else
            let normalized = positiveRemainder degrees (Degree.fromFloat 360.0)

            match Degree.toFloat normalized with
            | 0.0
            | 90.0
            | 180.0
            | 270.0 -> Some normalized
            | _ -> None

    let private normalizedEighthTurn (degrees: float<degree>) : float<degree> option =
        if not (System.Double.IsFinite(Degree.toFloat degrees)) then
            None
        else
            let normalized = positiveRemainder degrees (Degree.fromFloat 360.0)

            match Degree.toFloat normalized with
            | 0.0
            | 45.0
            | 90.0
            | 135.0
            | 180.0
            | 225.0
            | 270.0
            | 315.0 -> Some normalized
            | _ -> None

    let sinDegrees (degrees: float<degree>) : float =
        match normalizedQuarterTurn degrees |> Option.map Degree.toFloat with
        | Some 0.0
        | Some 180.0 -> 0.0
        | Some 90.0 -> 1.0
        | Some 270.0 -> -1.0
        | _ -> sin (Degree.toRadians degrees |> Radian.toFloat)

    let cosDegrees (degrees: float<degree>) : float =
        match normalizedQuarterTurn degrees |> Option.map Degree.toFloat with
        | Some 0.0 -> 1.0
        | Some 90.0
        | Some 270.0 -> 0.0
        | Some 180.0 -> -1.0
        | _ -> cos (Degree.toRadians degrees |> Radian.toFloat)

    let tanDegrees (degrees: float<degree>) : float =
        match normalizedEighthTurn degrees |> Option.map Degree.toFloat with
        | Some 0.0
        | Some 180.0 -> 0.0
        | Some 45.0
        | Some 225.0 -> 1.0
        | Some 135.0
        | Some 315.0 -> -1.0
        | _ -> tan (Degree.toRadians degrees |> Radian.toFloat)

    let atanDegrees (value: float) : float<degree> =
        atan value |> Radian.fromFloat |> Radian.toDegrees

    let private diagonalAtan2 (y: float<'Unit>) (x: float<'Unit>) : float<degree> =
        match x > 0.0<_>, y > 0.0<_> with
        | true, true -> Degree.fromFloat 45.0
        | false, true -> Degree.fromFloat 135.0
        | false, false -> Degree.fromFloat -135.0
        | true, false -> Degree.fromFloat -45.0

    /// Return atan2(y, x) in degrees, with exact axis and diagonal results.
    let atan2Degrees (y: float<'Unit>) (x: float<'Unit>) : float<degree> =
        if x = 0.0<_> && y = 0.0<_> then
            System.Math.Atan2(float y, float x) |> Radian.fromFloat |> Radian.toDegrees
        elif x = 0.0<_> then
            Degree.fromFloat (if y > 0.0<_> then 90.0 else -90.0)
        elif y = 0.0<_> then
            Degree.fromFloat (if x > 0.0<_> then 0.0 else 180.0)
        elif abs x = abs y then
            diagonalAtan2 y x
        else
            System.Math.Atan2(float y, float x) |> Radian.fromFloat |> Radian.toDegrees

    let acosDegrees (value: float) : float<degree> option =
        if value >= -1.0 && value <= 1.0 then
            acos value |> Radian.fromFloat |> Radian.toDegrees |> Some
        else
            None
