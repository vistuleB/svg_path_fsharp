namespace SvgPath

type TransformSerializeOptions =
    { DecimalPlaces: int option
      FixedDecimals: bool
      ForceMatrix: bool }

type private LinearTransform =
    | Matrix2x2
    | Identity2x2
    | Scale2x2 of x: float * y: float
    | SkewX2x2 of tangent: float
    | SkewY2x2 of tangent: float
    | RotateScale2x2 of degrees: float<degree> * scaleX: float * scaleY: float

/// Serialize affine matrices as SVG transform attribute values.
[<RequireQualifiedAccess>]
module TransformSerialize =
    let private rotationScaleEpsilon = 0.000001

    let defaultOptions () =
        { DecimalPlaces = Some 5
          FixedDecimals = false
          ForceMatrix = false }

    let decimalOptions decimalPlaces =
        { DecimalPlaces = Some decimalPlaces
          FixedDecimals = false
          ForceMatrix = false }

    let fixedDecimalOptions decimalPlaces =
        { DecimalPlaces = Some decimalPlaces
          FixedDecimals = true
          ForceMatrix = false }

    let forceMatrix options = { options with ForceMatrix = true }

    let private number value options =
        let rightDecimals =
            match options.DecimalPlaces, options.FixedDecimals with
            | None, _ -> System
            | Some decimalPlaces, false -> AtMost decimalPlaces
            | Some decimalPlaces, true -> Fixed decimalPlaces

        NumberFormat.prepare
            { LeftDecimals = Succinct
              RightDecimals = rightDecimals }
            [ value ]
        |> NumberFormat.number value

    let private transformFunction name arguments = name + "(" + arguments + ")"

    let private translateTransform (x: float<length>) (y: float<length>) options =
        let arguments =
            if y = 0.0<length> then number (float x) options
            else number (float x) options + " " + number (float y) options
        transformFunction "translate" arguments

    let private scaleTransform x y options =
        let arguments =
            if x = y then number x options
            else number x options + " " + number y options
        transformFunction "scale" arguments

    let private rotateTransform degrees options =
        transformFunction "rotate" (number (Degree.toFloat degrees) options)

    let private skewXTransform tangent options =
        transformFunction "skewX" (number (Trig.atanDegrees tangent |> Degree.toFloat) options)

    let private skewYTransform tangent options =
        transformFunction "skewY" (number (Trig.atanDegrees tangent |> Degree.toFloat) options)

    let private joinTransforms first second =
        match first, second with
        | "", _ -> second
        | _, "" -> first
        | _ -> first + " " + second

    let private closeToZero value = abs value <= rotationScaleEpsilon

    let private analyzeRotationScale a b c d =
        let scaleX = sqrt (a * a + b * b)
        let scaleY = sqrt (c * c + d * d)
        let determinant = a * d - b * c

        if scaleX > rotationScaleEpsilon
           && scaleY > rotationScaleEpsilon
           && determinant > rotationScaleEpsilon then
            let normalizedDotProduct =
                a / scaleX * (c / scaleY) + b / scaleX * (d / scaleY)
            if closeToZero normalizedDotProduct then
                RotateScale2x2(Trig.atan2Degrees b a, scaleX, scaleY)
            else
                Matrix2x2
        else
            Matrix2x2

    let private analyzeLinearTransform a b c d =
        if a = 1.0 && b = 0.0 && c = 0.0 && d = 1.0 then Identity2x2
        elif b = 0.0 && c = 0.0 then Scale2x2(a, d)
        elif a = 1.0 && b = 0.0 && d = 1.0 then SkewX2x2 c
        elif a = 1.0 && c = 0.0 && d = 1.0 then SkewY2x2 b
        else analyzeRotationScale a b c d

    let private scaleOptionalTransform x y options =
        if x = 1.0 && y = 1.0 then "" else scaleTransform x y options

    let private linearTransform linear options =
        match linear with
        | Matrix2x2
        | Identity2x2 -> ""
        | Scale2x2(x, y) -> scaleTransform x y options
        | SkewX2x2 tangent -> skewXTransform tangent options
        | SkewY2x2 tangent -> skewYTransform tangent options
        | RotateScale2x2(degrees, scaleX, scaleY) ->
            joinTransforms
                (rotateTransform degrees options)
                (scaleOptionalTransform scaleX scaleY options)

    let private translateOptionalTransform x y options =
        if x = 0.0<length> && y = 0.0<length> then ""
        else translateTransform x y options

    let private affineTransform linear translateX translateY options =
        match linearTransform linear options with
        | "" -> translateTransform translateX translateY options
        | linearValue ->
            joinTransforms
                (translateOptionalTransform translateX translateY options)
                linearValue

    let private matrixTransform a b c d e f options =
        [ number a options
          number b options
          number c options
          number d options
          number (float e) options
          number (float f) options ]
        |> String.concat " "
        |> transformFunction "matrix"

    let toStringWith (transform: Affine) options =
        let a, b, c, d, e, f = Affine.toTuple transform
        if options.ForceMatrix then
            matrixTransform a b c d e f options
        else
            match analyzeLinearTransform a b c d with
            | Matrix2x2 -> matrixTransform a b c d e f options
            | linear -> affineTransform linear e f options

    let toString transform = toStringWith transform (defaultOptions ())
