namespace SvgPath

open System
open System.Globalization

[<RequireQualifiedAccess>]
module InternalNumber =
    /// Mathematical zero of either sign, without a tolerance. Unlike Erlang
    /// exact equality, .NET equality already equates positive and negative zero.
    let inline isZero (value: float<'Unit>) = value = 0.0<_>

    /// Canonicalize either signed zero, leaving every nonzero value unchanged.
    let inline normalizeZero (value: float<'Unit>) =
        if isZero value then 0.0<_> else value

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
        // A divided overflow threshold can round upward. Check the actual
        // result; .NET yields infinity rather than Erlang's arithmetic error.
        let result = first * second
        if Double.IsFinite result then Ok result else Error()

    let checkedSum (first: float<'Unit>) (second: float<'Unit>) =
        let result = first + second
        if Double.IsFinite(float result) then Ok result else Error()

    let isFinite value = Double.IsFinite value
