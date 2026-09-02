namespace SvgPath

open System
open System.Globalization

[<RequireQualifiedAccess>]
module InternalNumber =
    let hypot x y =
        let x, y = abs x, abs y
        let largest = max x y
        if largest = 0.0 || not (Double.IsFinite largest) then largest
        else
            let scaledX, scaledY = x / largest, y / largest
            largest * sqrt (scaledX * scaledX + scaledY * scaledY)

    let parse (raw: string) =
        let exponentAt = raw.IndexOfAny([| 'e'; 'E' |])
        let mantissa = if exponentAt < 0 then raw else raw.Substring(0, exponentAt)
        if mantissa.EndsWith(".", StringComparison.Ordinal) then Error()
        else
            match Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, value when Double.IsFinite value -> Ok value
            | _ -> Error()

    let checkedProduct first second =
        let absoluteSecond = abs second
        if first = 0.0 || second = 0.0 then Ok 0.0
        elif absoluteSecond <= 1.0 then Ok(first * second)
        elif abs first > Double.MaxValue / absoluteSecond then Error()
        else Ok(first * second)

    let checkedSum first second =
        let sameSign = (first > 0.0 && second > 0.0) || (first < 0.0 && second < 0.0)
        if sameSign && abs first > Double.MaxValue - abs second then Error()
        else Ok(first + second)

    let isFinite value = Double.IsFinite value
