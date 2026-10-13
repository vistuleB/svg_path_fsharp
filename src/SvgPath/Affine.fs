namespace SvgPath

/// A two-dimensional affine transform in SVG's six-value matrix form.
/// The linear coefficients are dimensionless; translations use SVG lengths.
[<Struct>]
type Affine =
    { A: float
      B: float
      C: float
      D: float
      E: float<length>
      F: float<length> }

/// Generic two-dimensional affine matrices and matrix composition.
[<RequireQualifiedAccess>]
module Affine =
    /// Failures while constructing a transform from point correspondences.
    type Error =
        /// Zero source separation in the scaled calculation.
        | DegenerateSourcePair
        /// Zero computed determinant, for example collinear or repeated source points.
        | DegenerateSourceTriple
        /// Construction produced a non-finite denominator or matrix coefficient.
        | NonFiniteTransform

    let matrix a b c d (e: float<length>) (f: float<length>) : Affine =
        { A = a; B = b; C = c; D = d; E = e; F = f }

    let fromTuple (a, b, c, d, e, f) = matrix a b c d e f

    let toTuple transform =
        transform.A, transform.B, transform.C, transform.D, transform.E, transform.F

    let identity () = matrix 1.0 0.0 0.0 1.0 0.0<length> 0.0<length>

    /// Multiply two matrices in algebraic order: `left * right`.
    let multiply left right =
        matrix
            (left.A * right.A + left.C * right.B)
            (left.B * right.A + left.D * right.B)
            (left.A * right.C + left.C * right.D)
            (left.B * right.C + left.D * right.D)
            (left.A * right.E + left.C * right.F + left.E)
            (left.B * right.E + left.D * right.F + left.F)

    /// Apply `first` first and `second` second.
    let chain first second = multiply second first

    let translate (x: float<length>) (y: float<length>) =
        matrix 1.0 0.0 0.0 1.0 x y

    let scale factor = matrix factor 0.0 0.0 factor 0.0<length> 0.0<length>

    let scaleXY x y = matrix x 0.0 0.0 y 0.0<length> 0.0<length>

    /// Positive angles rotate visually clockwise in SVG's y-down coordinates.
    let rotate (degrees: float<degree>) =
        let cosine = Trig.cosDegrees degrees
        let sine = Trig.sinDegrees degrees
        matrix cosine sine -sine cosine 0.0<length> 0.0<length>

    let skewX (degrees: float<degree>) =
        matrix 1.0 0.0 (Trig.tanDegrees degrees) 1.0 0.0<length> 0.0<length>

    let skewY (degrees: float<degree>) =
        matrix 1.0 (Trig.tanDegrees degrees) 0.0 1.0 0.0<length> 0.0<length>

    let aboutPoint transform (point: Point<length>) =
        translate -point.X -point.Y
        |> fun shiftToOrigin -> chain shiftToOrigin transform
        |> fun transformed -> chain transformed (translate point.X point.Y)

    /// Apply the full affine transform to a geometric coordinate pair.
    let point transform (point: Point<length>) : Point<length> =
        Point.create
            (transform.A * point.X + transform.C * point.Y + transform.E)
            (transform.B * point.X + transform.D * point.Y + transform.F)

    /// Apply only the dimensionless linear part to any coordinate-pair unit.
    let linearPoint transform (point: Point<'Unit>) : Point<'Unit> =
        Point.create
            (transform.A * point.X + transform.C * point.Y)
            (transform.B * point.X + transform.D * point.Y)

    let determinant transform = transform.A * transform.D - transform.B * transform.C

    let isFinite transform =
        System.Double.IsFinite transform.A
        && System.Double.IsFinite transform.B
        && System.Double.IsFinite transform.C
        && System.Double.IsFinite transform.D
        && System.Double.IsFinite (float transform.E)
        && System.Double.IsFinite (float transform.F)

    /// Find a translation, rotation, and uniform scale mapping one point pair to another.
    /// Reports DegenerateSourcePair for zero computed source separation;
    /// NonFiniteTransform distinguishes non-finite arithmetic from degeneracy.
    let pointPairSimilarity
        (sourceStart: Point<length>)
        (sourceEnd: Point<length>)
        (targetStart: Point<length>)
        (targetEnd: Point<length>)
        : Result<Affine, Error> =
        let source = Point.displacement sourceStart sourceEnd
        let target = Point.displacement targetStart targetEnd
        let vectorScale =
            max
                (max (abs (float source.X)) (abs (float source.Y)))
                (max (abs (float target.X)) (abs (float target.Y)))
        let divisor = if vectorScale > 0.0 then vectorScale else 1.0
        let sourceX = float source.X / divisor
        let sourceY = float source.Y / divisor
        let targetX = float target.X / divisor
        let targetY = float target.Y / divisor
        let denominator = sourceX * sourceX + sourceY * sourceY

        if denominator = 0.0 then
            Error DegenerateSourcePair
        elif not (System.Double.IsFinite denominator) then
            Error NonFiniteTransform
        else
            let a = (sourceX * targetX + sourceY * targetY) / denominator
            let b = (sourceX * targetY - sourceY * targetX) / denominator
            let transform =
                matrix
                    a
                    b
                    -b
                    a
                    (targetStart.X - (a * sourceStart.X - b * sourceStart.Y))
                    (targetStart.Y - (b * sourceStart.X + a * sourceStart.Y))
            if isFinite transform then Ok transform else Error NonFiniteTransform

    /// Find an affine transform mapping one point triple to another.
    /// Reports DegenerateSourceTriple for a zero computed source determinant.
    /// Non-finite arithmetic returns NonFiniteTransform. Collapsed targets are allowed.
    let pointTripleMap
        (sourceA: Point<length>)
        (sourceB: Point<length>)
        (sourceC: Point<length>)
        (targetA: Point<length>)
        (targetB: Point<length>)
        (targetC: Point<length>)
        : Result<Affine, Error> =
        let sourceAB = Point.displacement sourceA sourceB
        let sourceAC = Point.displacement sourceA sourceC
        let targetAB = Point.displacement targetA targetB
        let targetAC = Point.displacement targetA targetC
        let denominator = sourceAB.X * sourceAC.Y - sourceAB.Y * sourceAC.X
        if denominator = 0.0<length^2> then Error DegenerateSourceTriple
        elif not (System.Double.IsFinite(float denominator)) then Error NonFiniteTransform
        else
            let a = (targetAB.X * sourceAC.Y - targetAC.X * sourceAB.Y) / denominator
            let b = (targetAB.Y * sourceAC.Y - targetAC.Y * sourceAB.Y) / denominator
            let c = (targetAC.X * sourceAB.X - targetAB.X * sourceAC.X) / denominator
            let d = (targetAC.Y * sourceAB.X - targetAB.Y * sourceAC.X) / denominator
            let transform =
                matrix
                    (float a)
                    (float b)
                    (float c)
                    (float d)
                    (targetA.X - (float a * sourceA.X + float c * sourceA.Y))
                    (targetA.Y - (float b * sourceA.X + float d * sourceA.Y))
            if isFinite transform then Ok transform else Error NonFiniteTransform


    /// Algebraic factor order; the rightmost factor acts first on a point.
    type DecompositionOrder =
        | TranslationRotationShearXScale
        | TranslationRotationScaleShearX
        | TranslationShearXRotationScale
        | TranslationShearXScaleRotation
        | TranslationScaleRotationShearX
        | TranslationScaleShearXRotation
        | RotationTranslationShearXScale
        | RotationTranslationScaleShearX
        | RotationShearXTranslationScale
        | RotationShearXScaleTranslation
        | RotationScaleTranslationShearX
        | RotationScaleShearXTranslation
        | ShearXTranslationRotationScale
        | ShearXTranslationScaleRotation
        | ShearXRotationTranslationScale
        | ShearXRotationScaleTranslation
        | ShearXScaleTranslationRotation
        | ShearXScaleRotationTranslation
        | ScaleTranslationRotationShearX
        | ScaleTranslationShearXRotation
        | ScaleRotationTranslationShearX
        | ScaleRotationShearXTranslation
        | ScaleShearXTranslationRotation
        | ScaleShearXRotationTranslation

    /// Numerical factors. ShearX is a coefficient, not a skew angle.
    type Decomposition =
        { Order: DecompositionOrder
          TranslationX: float<length>
          TranslationY: float<length>
          RotationDegrees: float<degree>
          ShearX: float
          ScaleX: float
          ScaleY: float }

    type DecompositionError =
        /// No finite real factors under the computed rank/solvability tests.
        | NoFactorizationInOrder of order: DecompositionOrder
        /// Input or a required factor/intermediate is not finite.
        | DecompositionNonFinite
        /// Numerical range or conditioning prevented certified reconstruction.
        | DecompositionUnstable

    type private LinearOrder = Rhs | Rsh | Srh | Shr | Hsr | Hrs
    type private Factor = RotationFactor | ShearFactor | ScaleFactor

    let private orderParts order =
        match order with
        | TranslationRotationShearXScale -> Rhs, 0
        | TranslationRotationScaleShearX -> Rsh, 0
        | TranslationShearXRotationScale -> Hrs, 0
        | TranslationShearXScaleRotation -> Hsr, 0
        | TranslationScaleRotationShearX -> Srh, 0
        | TranslationScaleShearXRotation -> Shr, 0
        | RotationTranslationShearXScale -> Rhs, 1
        | RotationTranslationScaleShearX -> Rsh, 1
        | RotationShearXTranslationScale -> Rhs, 2
        | RotationShearXScaleTranslation -> Rhs, 3
        | RotationScaleTranslationShearX -> Rsh, 2
        | RotationScaleShearXTranslation -> Rsh, 3
        | ShearXTranslationRotationScale -> Hrs, 1
        | ShearXTranslationScaleRotation -> Hsr, 1
        | ShearXRotationTranslationScale -> Hrs, 2
        | ShearXRotationScaleTranslation -> Hrs, 3
        | ShearXScaleTranslationRotation -> Hsr, 2
        | ShearXScaleRotationTranslation -> Hsr, 3
        | ScaleTranslationRotationShearX -> Srh, 1
        | ScaleTranslationShearXRotation -> Shr, 1
        | ScaleRotationTranslationShearX -> Srh, 2
        | ScaleRotationShearXTranslation -> Srh, 3
        | ScaleShearXTranslationRotation -> Shr, 2
        | ScaleShearXRotationTranslation -> Shr, 3

    let private linearFactors = function
        | Rhs -> [RotationFactor; ShearFactor; ScaleFactor]
        | Rsh -> [RotationFactor; ScaleFactor; ShearFactor]
        | Srh -> [ScaleFactor; RotationFactor; ShearFactor]
        | Shr -> [ScaleFactor; ShearFactor; RotationFactor]
        | Hsr -> [ShearFactor; ScaleFactor; RotationFactor]
        | Hrs -> [ShearFactor; RotationFactor; ScaleFactor]

    let private canonicalAngle (angle: float<degree>) =
        let angle =
            if angle <= -180.0<degree> then angle + 360.0<degree>
            elif angle > 180.0<degree> then angle - 360.0<degree>
            else angle
        InternalNumber.normalizeZero angle

    let private factors order angle shear sx sy =
        { Order = order; TranslationX = 0.0<length>; TranslationY = 0.0<length>
          RotationDegrees = canonicalAngle angle; ShearX = InternalNumber.normalizeZero shear
          ScaleX = InternalNumber.normalizeZero sx; ScaleY = InternalNumber.normalizeZero sy }

    let private columnMatches a b x y =
        let magnitude = max (abs a) (abs b)
        if magnitude = 0.0 then x = 0.0 && y = 0.0
        else abs (a / magnitude - x / magnitude) <= 1e-12
             && abs (b / magnitude - y / magnitude) <= 1e-12

    let private factorsFinite (f: Decomposition) =
        [float f.TranslationX; float f.TranslationY; float f.RotationDegrees; f.ShearX; f.ScaleX; f.ScaleY]
        |> List.forall System.Double.IsFinite

    // Lost normalized entries are numerical range failures, not proof of rank loss.
    let private lostEntries entries =
        entries |> List.exists (fun (original, normalized) -> original <> 0.0 && normalized = 0.0)

    let private triangularCandidates (m: Affine) order shearBeforeScale =
        let first = max (abs m.A) (abs m.B)
        let second = max (abs m.C) (abs m.D)
        if first = 0.0 && second = 0.0 then Ok [factors order 0.0<degree> 0.0 0.0 0.0]
        elif first = 0.0 then
            Ok [factors order (Trig.atan2Degrees -m.C m.D) 0.0 0.0 (InternalNumber.hypot m.C m.D)]
        elif second = 0.0 then
            Ok [factors order (Trig.atan2Degrees m.B m.A) 0.0 (InternalNumber.hypot m.A m.B) 0.0]
        else
            let x, y = m.A / first, m.B / first
            let norm = InternalNumber.hypot x y
            let u, v = x / norm, y / norm
            let c, d = m.C / second, m.D / second
            let perpendicular = u * d - v * c
            let projection = u * c + v * d
            if perpendicular = 0.0 && lostEntries [m.A,x; m.B,y; m.C,c; m.D,d] then
                Error DecompositionUnstable
            elif shearBeforeScale && perpendicular = 0.0 then Error (NoFactorizationInOrder order)
            else
                let sx, sy = first * norm, perpendicular * second
                if perpendicular <> 0.0 && sy = 0.0 then Error DecompositionUnstable
                else
                    let shear = if shearBeforeScale then projection / perpendicular else (projection * second) / sx
                    Ok [factors order (Trig.atan2Degrees y x) shear sx sy]

    let private middleRotationFactors (m: Affine) order angle a b c d =
        let angle =
            if (m.A < 0.0) <> (Trig.cosDegrees angle < 0.0) then canonicalAngle (angle + 180.0<degree>)
            else canonicalAngle angle
        let cosine, sine = Trig.cosDegrees angle, Trig.sinDegrees angle
        if cosine = 0.0 || sine = 0.0 then Error DecompositionUnstable
        else
            let shear = if abs a >= abs b then c / a + sine / cosine else d / b - cosine / sine
            let topRight, bottomRight = cosine * shear - sine, sine * shear + cosine
            // Larger row coefficients avoid amplifying angle noise near quarter turns.
            let sx = if abs cosine >= abs topRight then m.A / cosine else m.C / topRight
            let sy = if abs sine >= abs bottomRight then m.B / sine else m.D / bottomRight
            Ok (factors order angle shear sx sy)

    let private orderCandidates first second =
        match first, second with
        | Error error, Error _ -> Error error
        | Ok f, Error _ | Error _, Ok f -> Ok [f]
        | Ok first, Ok second ->
            if abs first.RotationDegrees < abs second.RotationDegrees
               || (abs first.RotationDegrees = abs second.RotationDegrees && first.RotationDegrees <= second.RotationDegrees)
            then Ok [first; second] else Ok [second; first]

    let private middleRotationCandidates (m: Affine) order =
        let top = max (abs m.A) (abs m.C)
        let bottom = max (abs m.B) (abs m.D)
        if top = 0.0 && bottom = 0.0 then Ok [factors order 0.0<degree> 0.0 0.0 0.0]
        elif top = 0.0 then
            Ok [factors order (Trig.atan2Degrees m.B m.D) 0.0 0.0 (InternalNumber.hypot m.B m.D)]
        elif bottom = 0.0 then
            Ok [factors order (Trig.atan2Degrees -m.C m.A) 0.0 (InternalNumber.hypot m.A m.C) 0.0]
        else
            let a, c, b, d = m.A / top, m.C / top, m.B / bottom, m.D / bottom
            let det = a * d - b * c
            if det = 0.0 && lostEntries [m.A,a; m.B,b; m.C,c; m.D,d] then Error DecompositionUnstable
            elif det = 0.0 then Error (NoFactorizationInOrder order)
            elif m.A = 0.0 then
                let sign = if m.C > 0.0 then -1.0 else 1.0
                Ok [factors order (sign * 90.0<degree>) (m.D / m.B) (abs m.C) (m.B / sign)]
            elif m.B = 0.0 then
                let sign = if m.A > 0.0 then 1.0 else -1.0
                Ok [factors order (if sign > 0.0 then 0.0<degree> else 180.0<degree>) (m.C / m.A) (abs m.A) (m.D / sign)]
            else
                let k = a * b
                let twiceK, absDet = 2.0 * abs k, abs det
                let gap = absDet - twiceK
                let allowance = 1e-14 * max absDet twiceK
                if k = 0.0 then Error DecompositionUnstable
                elif gap < -allowance then Error (NoFactorizationInOrder order)
                else
                    let root = sqrt (max 0.0 gap * (absDet + twiceK))
                    let denominator = det + (if det < 0.0 then -root else root)
                    let angle1 = Trig.atan2Degrees (2.0 * k) denominator
                    let angle2 = Trig.atan2Degrees denominator (2.0 * k)
                    orderCandidates
                        (middleRotationFactors m order angle1 a b c d)
                        (middleRotationFactors m order angle2 a b c d)

    let rec private linearCandidates m order linear =
        match linear with
        | Rhs | Rsh -> triangularCandidates m order (linear = Rhs)
        | Srh -> middleRotationCandidates m order
        | Shr | Hsr | Hrs ->
            let dual = matrix m.D m.B m.C m.A 0.0<length> 0.0<length>
            let core = match linear with Shr -> Rhs | Hsr -> Rsh | _ -> Srh
            linearCandidates dual order core
            |> Result.map (List.map (fun f -> { f with ScaleX = f.ScaleY; ScaleY = f.ScaleX }))

    let rec private recomposeLinear (f: Decomposition) linear =
        match linear with
        | Shr | Hsr | Hrs ->
            let core = match linear with Shr -> Rhs | Hsr -> Rsh | _ -> Srh
            let m = recomposeLinear { f with ScaleX = f.ScaleY; ScaleY = f.ScaleX } core
            matrix m.D m.B m.C m.A 0.0<length> 0.0<length>
        | _ ->
            let c, s = Trig.cosDegrees f.RotationDegrees, Trig.sinDegrees f.RotationDegrees
            match linear with
            | Rhs | Rsh ->
                let q = f.ShearX * (if linear = Rhs then f.ScaleY else f.ScaleX)
                matrix (c * f.ScaleX) (s * f.ScaleX) (c * q - s * f.ScaleY) (s * q + c * f.ScaleY) 0.0<length> 0.0<length>
            | _ ->
                let a, b = f.ScaleX * c, f.ScaleY * s
                matrix a b (a * f.ShearX - f.ScaleX * s) (b * f.ShearX + f.ScaleY * c) 0.0<length> 0.0<length>

    let private applyFactor factor (f: Decomposition) (x: float) (y: float) =
        match factor with
        | RotationFactor ->
            let c, s = Trig.cosDegrees f.RotationDegrees, Trig.sinDegrees f.RotationDegrees
            c * x - s * y, s * x + c * y
        | ShearFactor -> x + f.ShearX * y, y
        | ScaleFactor -> f.ScaleX * x, f.ScaleY * y

    let rec private applyFactors parts f x y =
        match parts with
        | [] -> x, y
        | first :: rest ->
            let x, y = applyFactors rest f x y
            applyFactor first f x y

    let rec private inverseFactors parts f x y =
        match parts with
        | [] -> x, y
        | first :: rest ->
            let x, y =
                match first with
                | RotationFactor -> applyFactor RotationFactor { f with RotationDegrees = -f.RotationDegrees } x y
                | ShearFactor -> x - f.ShearX * y, y
                | ScaleFactor -> x / f.ScaleX, y / f.ScaleY
            inverseFactors rest f x y

    /// Recompose in the stored algebraic order. Raw factors are not validated.
    let recompose (f: Decomposition) =
        let linear, position = orderParts f.Order
        let m = recomposeLinear f linear
        let prefix = linearFactors linear |> List.take position
        let e, v = applyFactors prefix f (float f.TranslationX) (float f.TranslationY)
        { m with E = Length.fromFloat e; F = Length.fromFloat v }

    let private singularTranslation a b c d e f order =
        let first, second = max (abs a) (abs b), max (abs c) (abs d)
        let x, y =
            if first >= second then
                if first = 0.0 then 0.0, 0.0
                else (if abs a >= abs b then e / a else f / b), 0.0
            else 0.0, (if abs c >= abs d then e / c else f / d)
        if not (System.Double.IsFinite x && System.Double.IsFinite y) then Error DecompositionNonFinite
        elif columnMatches e f (a * x + c * y) (b * x + d * y) then Ok (x, y)
        else Error (NoFactorizationInOrder order)

    let private withTranslation f (m: Affine) linear position =
        let prefix = linearFactors linear |> List.take position
        let singular = List.contains ScaleFactor prefix && (f.ScaleX = 0.0 || f.ScaleY = 0.0)
        let translation =
            if not singular then Ok (inverseFactors prefix f (float m.E) (float m.F))
            else
                let a, b = applyFactors prefix f 1.0 0.0
                let c, d = applyFactors prefix f 0.0 1.0
                if isFinite (matrix a b c d 0.0<length> 0.0<length>) then
                    singularTranslation a b c d (float m.E) (float m.F) f.Order
                else Error DecompositionNonFinite
        translation |> Result.map (fun (x, y) -> { f with TranslationX = Length.fromFloat x; TranslationY = Length.fromFloat y })

    let rec private tryCandidates candidates m linear position lastError =
        match candidates with
        | [] -> Error lastError
        | first :: rest ->
            let attempt =
                if not (factorsFinite first) then Error DecompositionNonFinite
                else
                    withTranslation first m linear position
                    |> Result.bind (fun f ->
                        let rebuilt = recompose f
                        if not (isFinite rebuilt && factorsFinite f) then Error DecompositionNonFinite
                        elif columnMatches m.A m.B rebuilt.A rebuilt.B
                             && columnMatches m.C m.D rebuilt.C rebuilt.D
                             && columnMatches (float m.E) (float m.F) (float rebuilt.E) (float rebuilt.F) then Ok f
                        else Error DecompositionUnstable)
            match attempt with
            | Ok f -> Ok f
            | Error error -> tryCandidates rest m linear position error

    /// Factor in the requested algebraic order without substituting another order.
    /// RHS/RSH choose positive x-scale; SHR/HSR choose positive y-scale. SRH chooses
    /// positive x-scale and smallest absolute rotation (then signed angle); HRS is
    /// its reversed dual. Degenerate branches use zero shear where possible.
    /// Rotation is in (-180,180]. Factors need not vary continuously for animation.
    /// Each linear column and translation vector reconstructs within relative
    /// max-norm error 1e-12; exactly zero vectors must stay zero. This backward-error
    /// bound does not guarantee accurate individual factors or exact solvability.
    /// Near quadratic double roots allow a relative negative gap up to 1e-14, but
    /// every candidate still requires reconstruction certification. Floating rank
    /// tests are not exact proofs; normalization underflow is a numerical failure.
    /// Singular translation after scale must lie in the prefix image. One free
    /// translation coordinate is set to zero using the largest prefix entry.
    /// Extreme finite inputs can return a numerical error instead of factors.
    let decomposeWithOrder (transform: Affine) order : Result<Decomposition, DecompositionError> =
        if not (isFinite transform) then Error DecompositionNonFinite
        else
            let linear, position = orderParts order
            linearCandidates transform order linear
            |> Result.bind (fun candidates -> tryCandidates candidates transform linear position DecompositionUnstable)

    /// Standard Translation × Rotation × ShearX × Scale factorization.
    let decompose transform = decomposeWithOrder transform TranslationRotationShearXScale
