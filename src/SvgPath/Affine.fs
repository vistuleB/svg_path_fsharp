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
    let pointPairSimilarity
        (sourceStart: Point<length>)
        (sourceEnd: Point<length>)
        (targetStart: Point<length>)
        (targetEnd: Point<length>)
        : Result<Affine, unit> =
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
            Error()
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
            if isFinite transform then Ok transform else Error()

    /// Find an affine transform mapping one point triple to another.
    let pointTripleMap
        (sourceA: Point<length>)
        (sourceB: Point<length>)
        (sourceC: Point<length>)
        (targetA: Point<length>)
        (targetB: Point<length>)
        (targetC: Point<length>)
        : Result<Affine, unit> =
        let sourceAB = Point.displacement sourceA sourceB
        let sourceAC = Point.displacement sourceA sourceC
        let targetAB = Point.displacement targetA targetB
        let targetAC = Point.displacement targetA targetC
        let denominator = sourceAB.X * sourceAC.Y - sourceAB.Y * sourceAC.X
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
        if isFinite transform then Ok transform else Error()
