namespace SvgPath

/// Serialize affine matrices as SVG transform attribute values.
[<RequireQualifiedAccess>]
module TransformSerialize =

    type Options =
        { DecimalPlaces: int option
          FixedDecimals: bool
          ForceMatrix: bool }

    type private LinearTransform =
        | Matrix2x2
        | Identity2x2
        | Scale2x2 of x: float * y: float
        | SkewX2x2 of tangent: float
        | SkewY2x2 of tangent: float

    let private identityScaleEpsilon = 0.0000000000001

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
            | None, _ -> NumberFormat.System
            | Some decimalPlaces, false -> NumberFormat.AtMost decimalPlaces
            | Some decimalPlaces, true -> NumberFormat.Fixed decimalPlaces

        NumberFormat.prepare
            { LeftDecimals = NumberFormat.Succinct
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

    let private analyzeLinearTransform a b c d =
        if a = 1.0 && b = 0.0 && c = 0.0 && d = 1.0 then Identity2x2
        elif b = 0.0 && c = 0.0 then Scale2x2(a, d)
        elif a = 1.0 && b = 0.0 && d = 1.0 then SkewX2x2 c
        elif a = 1.0 && c = 0.0 && d = 1.0 then SkewY2x2 b
        else Matrix2x2

    let private scaleOptionalTransform x y options =
        let closeToIdentity value = abs (value - 1.0) <= identityScaleEpsilon
        if closeToIdentity x && closeToIdentity y then "" else scaleTransform x y options

    let private linearTransform linear options =
        match linear with
        | Matrix2x2
        | Identity2x2 -> ""
        | Scale2x2(x, y) -> scaleTransform x y options
        | SkewX2x2 tangent -> skewXTransform tangent options
        | SkewY2x2 tangent -> skewYTransform tangent options
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

    type Error =
        /// Explicit ordering cannot be combined with ForceMatrix.
        | ConflictingMatrixOption
        | FactorizationFailed of reason: Affine.DecompositionError
        /// Rounded factors cannot reconstruct within the formatting budget.
        | InsufficientPrecision

    let private vectorMatches x y u v budget =
        let magnitude = max 1.0 (max (abs x) (abs y))
        abs (x / magnitude - u / magnitude) <= budget
        && abs (y / magnitude - v / magnitude) <= budget

    let rec private decimalBudget places value =
        if places <= 0 || value <= 1e-12 then max value 1e-12
        else decimalBudget (places - 1) (value / 10.0)

    let private checkText text (original: Affine) options =
        match TransformParse.attribute text with
        | Error _ -> Error InsufficientPrecision
        | Ok rebuilt ->
            let budget =
                match options.DecimalPlaces with
                | None -> 1e-12
                | Some places -> decimalBudget places 1.0
            if Affine.isFinite original && Affine.isFinite rebuilt
               && vectorMatches original.A original.B rebuilt.A rebuilt.B budget
               && vectorMatches original.C original.D rebuilt.C rebuilt.D budget
               && vectorMatches (float original.E) (float original.F) (float rebuilt.E) (float rebuilt.F) budget
            then Ok text else Error InsufficientPrecision

    let private factorString (f: Affine.Decomposition) options simplify =
        let t = translateOptionalTransform f.TranslationX f.TranslationY options
        let r = if f.RotationDegrees = 0.0<degree> then "" else rotateTransform f.RotationDegrees options
        let h = if f.ShearX = 0.0 || (simplify && abs f.ShearX <= 1e-13) then "" else skewXTransform f.ShearX options
        let s =
            if simplify then scaleOptionalTransform f.ScaleX f.ScaleY options
            elif f.ScaleX = 1.0 && f.ScaleY = 1.0 then ""
            else scaleTransform f.ScaleX f.ScaleY options
        let parts =
            match f.Order with
            | Affine.TranslationRotationShearXScale -> [t; r; h; s]
            | Affine.TranslationRotationScaleShearX -> [t; r; s; h]
            | Affine.TranslationShearXRotationScale -> [t; h; r; s]
            | Affine.TranslationShearXScaleRotation -> [t; h; s; r]
            | Affine.TranslationScaleRotationShearX -> [t; s; r; h]
            | Affine.TranslationScaleShearXRotation -> [t; s; h; r]
            | Affine.RotationTranslationShearXScale -> [r; t; h; s]
            | Affine.RotationTranslationScaleShearX -> [r; t; s; h]
            | Affine.RotationShearXTranslationScale -> [r; h; t; s]
            | Affine.RotationShearXScaleTranslation -> [r; h; s; t]
            | Affine.RotationScaleTranslationShearX -> [r; s; t; h]
            | Affine.RotationScaleShearXTranslation -> [r; s; h; t]
            | Affine.ShearXTranslationRotationScale -> [h; t; r; s]
            | Affine.ShearXTranslationScaleRotation -> [h; t; s; r]
            | Affine.ShearXRotationTranslationScale -> [h; r; t; s]
            | Affine.ShearXRotationScaleTranslation -> [h; r; s; t]
            | Affine.ShearXScaleTranslationRotation -> [h; s; t; r]
            | Affine.ShearXScaleRotationTranslation -> [h; s; r; t]
            | Affine.ScaleTranslationRotationShearX -> [s; t; r; h]
            | Affine.ScaleTranslationShearXRotation -> [s; t; h; r]
            | Affine.ScaleRotationTranslationShearX -> [s; r; t; h]
            | Affine.ScaleRotationShearXTranslation -> [s; r; h; t]
            | Affine.ScaleShearXTranslationRotation -> [s; h; t; r]
            | Affine.ScaleShearXRotationTranslation -> [s; h; r; t]
        match List.fold joinTransforms "" parts with
        | "" -> translateTransform 0.0<length> 0.0<length> options
        | text -> text

    let private orderedString transform order options =
        Affine.decomposeWithOrder transform order
        |> Result.mapError FactorizationFailed
        |> Result.bind (fun factors ->
            match checkText (factorString factors options true) transform options with
            | Ok text -> Ok text
            | Error _ -> checkText (factorString factors options false) transform options)

    /// Serialize in the requested algebraic order, rightmost acting first.
    /// Identity factors and verified negligible noise may be omitted. Never falls
    /// back to another order or matrix notation. ForceMatrix conflicts with this API.
    /// Rounded output is parsed and checked per column/translation vector against
    /// max(1, input max-norm) * max(1e-12, 10^-DecimalPlaces). None uses 1e-12;
    /// negative places mean zero. This is not exact equivalence or factor accuracy.
    let toStringInOrder transform order options =
        if options.ForceMatrix then Error ConflictingMatrixOption
        else orderedString transform order options

    /// Prefer simple forms (including skewY), then standard affine decomposition.
    /// Failed decomposition or rounded-factor checks fall back to matrix notation.
    /// Matrix fallback still rounds coefficients. Output is deterministic, but not
    /// necessarily shortest; construction history and animation continuity are not retained.
    let toStringWith (transform: Affine) options =
        let a, b, c, d, e, f = Affine.toTuple transform
        let a,b,c,d = InternalNumber.normalizeZero a,InternalNumber.normalizeZero b,InternalNumber.normalizeZero c,InternalNumber.normalizeZero d
        let e,f = InternalNumber.normalizeZero e,InternalNumber.normalizeZero f
        if options.ForceMatrix then matrixTransform a b c d e f options
        else
            let original = Affine.matrix a b c d e f
            let candidate =
                match analyzeLinearTransform a b c d with
                | Matrix2x2 -> orderedString original Affine.TranslationRotationShearXScale options
                | linear -> checkText (affineTransform linear e f options) original options
            match candidate with
            | Ok text -> text
            | Error _ -> matrixTransform a b c d e f options

    let toString transform = toStringWith transform (defaultOptions ())
