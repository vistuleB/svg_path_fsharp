namespace SvgPath

/// Trigonometry helpers for SVG-facing degree angles.
[<RequireQualifiedAccess>]
module Trig =
    let private reducedDegrees (degrees: float<degree>) : float<degree> =
        if not (System.Double.IsFinite(float degrees)) then degrees
        else
            // Signed remainder avoids losing a small negative angle by adding
            // 360, and avoids cancellation from floor(value/360)*360.
            let reduced = degrees % 360.0<degree>
            if InternalNumber.isZero reduced then 0.0<degree> else reduced

    let sinDegrees (degrees: float<degree>) : float =
        match reducedDegrees degrees |> Degree.toFloat with
        | 0.0 | 180.0 | -180.0 -> 0.0
        | 90.0 | -270.0 -> 1.0
        | 270.0 | -90.0 -> -1.0
        | reduced -> sin (Degree.toRadians (Degree.fromFloat reduced) |> Radian.toFloat)

    let cosDegrees (degrees: float<degree>) : float =
        match reducedDegrees degrees |> Degree.toFloat with
        | 0.0 -> 1.0
        | 90.0 | -90.0 | 270.0 | -270.0 -> 0.0
        | 180.0 | -180.0 -> -1.0
        | reduced -> cos (Degree.toRadians (Degree.fromFloat reduced) |> Radian.toFloat)

    let tanDegrees (degrees: float<degree>) : float =
        match reducedDegrees degrees |> Degree.toFloat with
        | 0.0 | 180.0 | -180.0 -> 0.0
        | 45.0 | -315.0 | 225.0 | -135.0 -> 1.0
        | 135.0 | -225.0 | 315.0 | -45.0 -> -1.0
        | reduced -> tan (Degree.toRadians (Degree.fromFloat reduced) |> Radian.toFloat)

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
