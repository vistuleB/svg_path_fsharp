namespace SvgPath

/// Length in SVG user-space units.
[<Measure>]
type length

/// A parameter in the domain of a curve segment.
[<Measure>]
type parameter

/// An angle measured in degrees.
[<Measure>]
type degree

/// An angle measured in radians.
[<Measure>]
type radian

[<RequireQualifiedAccess>]
module Length =
    let fromFloat (value: float) : float<length> =
        LanguagePrimitives.FloatWithMeasure<length> value

    let toFloat (value: float<length>) : float = float value

    let squared (value: float<length>) : float<length^2> = value * value

[<RequireQualifiedAccess>]
module Parameter =
    let fromFloat (value: float) : float<parameter> =
        LanguagePrimitives.FloatWithMeasure<parameter> value

    /// Convert a nominal curve parameter to its dimensionless coefficient.
    let ratio (value: float<parameter>) : float = float value

[<RequireQualifiedAccess>]
module Degree =
    let fromFloat (value: float) : float<degree> =
        LanguagePrimitives.FloatWithMeasure<degree> value

    let toFloat (value: float<degree>) : float = float value

    let toRadians (value: float<degree>) : float<radian> =
        LanguagePrimitives.FloatWithMeasure<radian> (float value * System.Math.PI / 180.0)

[<RequireQualifiedAccess>]
module Radian =
    let fromFloat (value: float) : float<radian> =
        LanguagePrimitives.FloatWithMeasure<radian> value

    let toFloat (value: float<radian>) : float = float value

    let toDegrees (value: float<radian>) : float<degree> =
        LanguagePrimitives.FloatWithMeasure<degree> (float value * 180.0 / System.Math.PI)
