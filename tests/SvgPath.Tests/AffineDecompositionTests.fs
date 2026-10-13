module SvgPath.Tests.AffineDecompositionTests

open SvgPath
open Xunit

let private ok result = result |> Result.defaultWith (failwithf "Unexpected error: %A")
let private matrix a b c d e f = Affine.matrix a b c d (Length.fromFloat e) (Length.fromFloat f)
let private close a b = Assert.True(abs(a-b) <= 1e-10, sprintf "%g <> %g" a b)
let private column a b x y =
    let magnitude = max (abs a) (abs b)
    if magnitude = 0.0 then Assert.True(x = 0.0 && y = 0.0)
    else
        Assert.True(abs(a/magnitude-x/magnitude) <= 1e-12 && abs(b/magnitude-y/magnitude) <= 1e-12,
                    sprintf "(%g,%g) <> (%g,%g)" a b x y)
let private same (m: Affine) (n: Affine) =
    column m.A m.B n.A n.B
    column m.C m.D n.C n.D
    column (float m.E) (float m.F) (float n.E) (float n.F)
let private orders = [
    Affine.TranslationRotationShearXScale, "TRHS"
    Affine.TranslationRotationScaleShearX, "TRSH"
    Affine.TranslationShearXRotationScale, "THRS"
    Affine.TranslationShearXScaleRotation, "THSR"
    Affine.TranslationScaleRotationShearX, "TSRH"
    Affine.TranslationScaleShearXRotation, "TSHR"
    Affine.RotationTranslationShearXScale, "RTHS"
    Affine.RotationTranslationScaleShearX, "RTSH"
    Affine.RotationShearXTranslationScale, "RHTS"
    Affine.RotationShearXScaleTranslation, "RHST"
    Affine.RotationScaleTranslationShearX, "RSTH"
    Affine.RotationScaleShearXTranslation, "RSHT"
    Affine.ShearXTranslationRotationScale, "HTRS"
    Affine.ShearXTranslationScaleRotation, "HTSR"
    Affine.ShearXRotationTranslationScale, "HRTS"
    Affine.ShearXRotationScaleTranslation, "HRST"
    Affine.ShearXScaleTranslationRotation, "HSTR"
    Affine.ShearXScaleRotationTranslation, "HSRT"
    Affine.ScaleTranslationRotationShearX, "STRH"
    Affine.ScaleTranslationShearXRotation, "STHR"
    Affine.ScaleRotationTranslationShearX, "SRTH"
    Affine.ScaleRotationShearXTranslation, "SRHT"
    Affine.ScaleShearXTranslationRotation, "SHTR"
    Affine.ScaleShearXRotationTranslation, "SHRT"
 ]
let private compose sequence angle shear sx sy =
    sequence |> Seq.fold (fun m factor ->
        let n =
            match factor with
            | 'T' -> Affine.translate 7.0<length> -11.0<length>
            | 'R' -> Affine.rotate (Degree.fromFloat angle)
            | 'H' -> matrix 1.0 0.0 shear 1.0 0.0 0.0
            | _ -> Affine.scaleXY sx sy
        Affine.multiply m n) (Affine.identity())
let private roundTrip m order =
    let f = Affine.decomposeWithOrder m order |> ok
    Assert.Equal(order, f.Order)
    Assert.True(f.RotationDegrees > -180.0<degree> && f.RotationDegrees <= 180.0<degree>)
    same m (Affine.recompose f)
    f

[<Fact>]
let ``all 24 identities have canonical factors`` () =
    for order,_ in orders do
        let f = roundTrip (Affine.identity()) order
        close (float f.RotationDegrees) 0.0
        close f.ShearX 0.0
        close f.ScaleX 1.0
        close f.ScaleY 1.0

[<Fact>]
let ``all orders independently composed nonsingular grid`` () =
    for order,sequence in orders do
        for angle in [-180.0;-135.0;-90.0;-45.0;0.0;30.0;45.0;90.0;135.0;179.999] do
            for shear in [-2.0;0.0;1.5] do
                for sx,sy in [2.0,3.0; -2.0,3.0; 2.0,-3.0; -2.0,-3.0] do
                    roundTrip (compose sequence angle shear sx sy) order |> ignore

[<Fact>]
let ``all orders independently composed singular grid`` () =
    for order,sequence in orders do
        for angle in [-30.0;0.0;90.0] do
            for shear in [-2.0;0.0;1.5] do
                for sx,sy in [0.0,3.0;2.0,0.0;0.0,0.0] do
                    roundTrip (compose sequence angle shear sx sy) order |> ignore

[<Fact>]
let ``standard order grid and wrapper`` () =
    for angle in [-180.0;-90.0;-30.0;0.0;30.0;90.0;179.0] do
        for shear in [-2.0;0.0;3.0] do
            for sx in [-3.0;0.5;2.0] do
                for sy in [-4.0;0.25;5.0] do
                    let m = compose "TRHS" angle shear sx sy
                    let f = roundTrip m Affine.TranslationRotationShearXScale
                    Assert.Equal(Ok f, Affine.decompose m)
                    Assert.True(f.ScaleX > 0.0)

[<Fact>]
let ``translation after collapsed scale must be reachable`` () =
    let m = matrix 1.0 0.0 0.0 0.0 0.0 1.0
    for order,sequence in orders do
        if sequence.IndexOf('T') < sequence.IndexOf('S') then roundTrip m order |> ignore
        else Assert.Equal(Error(Affine.NoFactorizationInOrder order), Affine.decomposeWithOrder m order)

[<Fact>]
let ``restricted linear orders reject an invertible counterexample`` () =
    let m = matrix 1.0 1.0 0.0 1.0 0.0 0.0
    for order,sequence in orders do
        let linear = sequence.Replace("T", "")
        if linear = "SRH" || linear = "HRS" then
            Assert.Equal(Error(Affine.NoFactorizationInOrder order), Affine.decomposeWithOrder m order)
        else roundTrip m order |> ignore

[<Fact>]
let ``rank one collinear columns differ by order`` () =
    let m = matrix 1.0 2.0 2.0 4.0 0.0 0.0
    Assert.Equal(Error(Affine.NoFactorizationInOrder Affine.TranslationRotationShearXScale), Affine.decompose m)
    roundTrip m Affine.TranslationRotationScaleShearX |> ignore

[<Fact>]
let ``standard scale range and anisotropic columns`` () =
    for x,y in [1e200,1e200;1e-200,1e-200;1e200,1e-200;1e-200,1e200;1.0,1e-14] do
        roundTrip (matrix x 0.0 0.0 y 0.0 0.0) Affine.TranslationRotationShearXScale |> ignore

[<Fact>]
let ``all orders large and small scales`` () =
    for order,sequence in orders do
        for scale in [1e-150;1e150] do
            roundTrip (compose sequence 30.0 0.5 (2.0*scale) (3.0*scale)) order |> ignore

[<Fact>]
let ``nonfinite and overflowing factors report numerical errors`` () =
    Assert.Equal(Error Affine.DecompositionNonFinite, Affine.decompose (matrix nan 0.0 0.0 1.0 0.0 0.0))
    Assert.Equal(Error Affine.DecompositionNonFinite, Affine.decompose (matrix 1.7e308 1.7e308 0.0 1.0 0.0 0.0))

[<Fact>]
let ``near half turn row scale regression and dual`` () =
    let m = matrix -0.3651838632260481 0.000010556228948517661 -1.0865702276357765 -2.25492845028511 2.556287042610347 -24.8045584525913
    roundTrip m Affine.ScaleTranslationRotationShearX |> ignore
    let dual = matrix m.D m.B m.C m.A 0.0 0.0
    roundTrip dual Affine.ShearXRotationScaleTranslation |> ignore

[<Fact>]
let ``normalization underflow is numerical not impossible`` () =
    for m,order in [
        matrix 1e-300 1e-300 1e300 2e300 0.0 0.0, Affine.TranslationScaleRotationShearX
        matrix 1e300 0.0 1e300 1e-300 0.0 0.0, Affine.TranslationRotationShearXScale
        matrix 2e300 1e-300 1e300 1e-300 0.0 0.0, Affine.TranslationShearXRotationScale
        matrix 1e-300 0.0 1e300 1e300 0.0 0.0, Affine.TranslationScaleShearXRotation ] do
        Assert.Equal(Error Affine.DecompositionUnstable, Affine.decomposeWithOrder m order)

[<Fact>]
let ``quadratic boundary follows backward error contract`` () =
    for top in [1e-300;1e-150;1.0;1e150;1e300] do
        for bottom in [1e-300;1e-150;1.0;1e150;1e300] do
            for delta in [-1e-10;-1e-13;-1e-14;0.0;1e-14;1e-13;1e-10] do
                let m = matrix top bottom 0.0 (bottom*(2.0+delta)) 0.0 0.0
                let dual = matrix m.D m.B m.C m.A 0.0 0.0
                for input,order in [m,Affine.TranslationScaleRotationShearX;dual,Affine.TranslationShearXRotationScale] do
                    let result = Affine.decomposeWithOrder input order
                    if delta < -1e-14 then Assert.Equal(Error(Affine.NoFactorizationInOrder order),result)
                    else
                        match result with
                        | Ok f -> same input (Affine.recompose f)
                        | Error error ->
                            Assert.True(top <> bottom)
                            Assert.Equal(Affine.DecompositionUnstable,error)

[<Fact>]
let ``near double root factors are sensitive`` () =
    let order = Affine.TranslationScaleRotationShearX
    let first = roundTrip (matrix 1.0 1.0 0.0 2.0 0.0 0.0) order
    let second = roundTrip (matrix 1.0 1.0 0.0 (2.0+1e-10) 0.0 0.0) order
    Assert.InRange(abs(float(first.RotationDegrees-second.RotationDegrees)),0.0002,0.0004)

[<Fact>]
let ``extreme quadratic range fails numerically`` () =
    for value in [1e-160;1e-170;1e-200;1e-300] do
        Assert.Equal(Error Affine.DecompositionUnstable,
                     Affine.decomposeWithOrder (matrix value value 1.0 2.0 0.0 0.0) Affine.TranslationScaleRotationShearX)

[<Fact>]
let ``all 24 orders serialize in requested order and reconstruct`` () =
    let options = TransformSerialize.decimalOptions 10
    for order,sequence in orders do
        let m = compose sequence 20.0 0.17632698070846498 2.0 3.0
        let text = TransformSerialize.toStringInOrder m order options |> ok
        let actualOrder =
            text.Split(')') |> Array.filter (fun x -> x <> "")
            |> Array.map (fun x ->
                let name = x.Trim().Split('(').[0]
                match name with "translate" -> "T" | "rotate" -> "R" | "skewX" -> "H" | "scale" -> "S" | _ -> failwith text)
            |> String.concat ""
        Assert.Equal(sequence, actualOrder)
        let rebuilt = TransformParse.attribute text |> ok
        for a,b in [m.A,rebuilt.A;m.B,rebuilt.B;m.C,rebuilt.C;m.D,rebuilt.D;float m.E,float rebuilt.E;float m.F,float rebuilt.F] do
            Assert.True(abs(a-b) < 1e-8)

[<Fact>]
let ``general transform uses all four readable factors`` () =
    let text = "translate(11 13) rotate(90) skewX(45) scale(3 5)"
    Assert.Equal(text, TransformParse.attribute text |> ok |> TransformSerialize.toString)

[<Fact>]
let ``extreme skew angle rounding falls back or errors`` () =
    for m in [matrix 1.0 0.0 1e12 1.0 0.0 0.0; matrix 1.0 1e12 0.0 1.0 0.0 0.0] do
        Assert.StartsWith("matrix(", TransformSerialize.toString m)
    Assert.Equal(Error TransformSerialize.InsufficientPrecision,
        TransformSerialize.toStringInOrder (matrix 1.0 0.0 1e12 1.0 0.0 0.0) Affine.TranslationRotationShearXScale (TransformSerialize.defaultOptions()))

[<Fact>]
let ``ordered precision can be increased`` () =
    let m = matrix 1.0 0.0 1000.0 1.0 0.0 0.0
    let order = Affine.TranslationRotationShearXScale
    Assert.Equal(Error TransformSerialize.InsufficientPrecision, TransformSerialize.toStringInOrder m order (TransformSerialize.decimalOptions 3))
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    Assert.StartsWith("skewX(", TransformSerialize.toStringInOrder m order options |> ok)

[<Fact>]
let ``ordered formatting reports impossible order and option conflict`` () =
    let m = matrix 1.0 1.0 0.0 1.0 0.0 0.0
    let order = Affine.TranslationScaleRotationShearX
    Assert.Equal(Error(TransformSerialize.FactorizationFailed(Affine.NoFactorizationInOrder order)), TransformSerialize.toStringInOrder m order (TransformSerialize.defaultOptions()))
    Assert.Equal("skewY(45)", TransformSerialize.toString m)
    Assert.Equal(Error TransformSerialize.ConflictingMatrixOption, TransformSerialize.toStringInOrder m order (TransformSerialize.defaultOptions() |> TransformSerialize.forceMatrix))

[<Fact>]
let ``skew Y retained by ordinary output but decomposed when ordered`` () =
    let m = TransformParse.attribute "translate(7 -11) skewY(30)" |> ok
    Assert.Equal("translate(7 -11) skewY(30)",TransformSerialize.toString m)
    let output = TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale (TransformSerialize.decimalOptions 10) |> ok
    Assert.Contains("skewX(", output)
    Assert.DoesNotContain("skewY(", output)

[<Fact>]
let ``anisotropic scale preserves amplified small shear`` () =
    let m = matrix 1e12 0.0 0.01 1.0 0.0 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    let text = TransformSerialize.toStringInOrder m Affine.TranslationRotationScaleShearX options |> ok
    Assert.Contains("skewX(",text)
    same m (TransformParse.attribute text |> ok)

// Paper.js Matrix.js, MIT: pinned upstream fixtures from the Gleam audit.
// Copyright (c) 2011-2020 Jürg Lehni & Jonathan Puckey.
// License: docs/paperjs-matrix-LICENSE.txt.
// The five -180 degree expectations are adapted to the documented +180 endpoint.

[<Fact>]
let ``Paper matrix 0 getRotation`` () =
    let m = matrix 1.0 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 1 getScaling`` () =
    let m = matrix 1.0 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 2 getRotation`` () =
    let m = matrix 0.9998476951563913 0.01745240643728351 (-0.01745240643728351) 0.9998476951563913 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (1.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 3 getScaling`` () =
    let m = matrix 0.9998476951563913 0.01745240643728351 (-0.01745240643728351) 0.9998476951563913 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 4 getRotation`` () =
    let m = matrix 0.7071067811865476 0.7071067811865475 (-0.7071067811865475) 0.7071067811865476 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (45.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 5 getScaling`` () =
    let m = matrix 0.7071067811865476 0.7071067811865475 (-0.7071067811865475) 0.7071067811865476 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 6 getRotation`` () =
    let m = matrix 6.123233995736766e-17 1.0 (-1.0) 6.123233995736766e-17 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (90.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 7 getScaling`` () =
    let m = matrix 6.123233995736766e-17 1.0 (-1.0) 6.123233995736766e-17 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 8 getRotation`` () =
    let m = matrix (-0.7071067811865475) 0.7071067811865476 (-0.7071067811865476) (-0.7071067811865475) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (135.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 9 getScaling`` () =
    let m = matrix (-0.7071067811865475) 0.7071067811865476 (-0.7071067811865476) (-0.7071067811865475) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 10 getRotation`` () =
    let m = matrix (-1.0) 1.2246467991473532e-16 (-1.2246467991473532e-16) (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 11 getScaling`` () =
    let m = matrix (-1.0) 1.2246467991473532e-16 (-1.2246467991473532e-16) (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 12 getRotation`` () =
    let m = matrix (-1.8369701987210297e-16) (-1.0) 1.0 (-1.8369701987210297e-16) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-90.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 13 getScaling`` () =
    let m = matrix (-1.8369701987210297e-16) (-1.0) 1.0 (-1.8369701987210297e-16) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 14 getRotation`` () =
    let m = matrix 0.9998476951563913 (-0.01745240643728351) 0.01745240643728351 0.9998476951563913 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-1.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 15 getScaling`` () =
    let m = matrix 0.9998476951563913 (-0.01745240643728351) 0.01745240643728351 0.9998476951563913 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 16 getRotation`` () =
    let m = matrix 0.7071067811865476 (-0.7071067811865475) 0.7071067811865475 0.7071067811865476 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-45.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 17 getScaling`` () =
    let m = matrix 0.7071067811865476 (-0.7071067811865475) 0.7071067811865475 0.7071067811865476 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 18 getRotation`` () =
    let m = matrix 6.123233995736766e-17 (-1.0) 1.0 6.123233995736766e-17 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-90.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 19 getScaling`` () =
    let m = matrix 6.123233995736766e-17 (-1.0) 1.0 6.123233995736766e-17 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 20 getRotation`` () =
    let m = matrix (-0.7071067811865475) (-0.7071067811865476) 0.7071067811865476 (-0.7071067811865475) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-135.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 21 getScaling`` () =
    let m = matrix (-0.7071067811865475) (-0.7071067811865476) 0.7071067811865476 (-0.7071067811865475) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 22 getRotation`` () =
    let m = matrix (-1.0) (-1.2246467991473532e-16) 1.2246467991473532e-16 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 23 getScaling`` () =
    let m = matrix (-1.0) (-1.2246467991473532e-16) 1.2246467991473532e-16 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 24 getRotation`` () =
    let m = matrix (-1.8369701987210297e-16) 1.0 (-1.0) (-1.8369701987210297e-16) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (90.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 25 getScaling`` () =
    let m = matrix (-1.8369701987210297e-16) 1.0 (-1.0) (-1.8369701987210297e-16) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 26 getRotation`` () =
    let m = matrix 1.0 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 27 getScaling`` () =
    let m = matrix 1.0 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 28 getRotation`` () =
    let m = matrix 1.0 0.0 0.0 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 29 getScaling`` () =
    let m = matrix 1.0 0.0 0.0 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (-1.0)

[<Fact>]
let ``Paper matrix 30 getRotation`` () =
    let m = matrix (-1.0) 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 31 getScaling`` () =
    let m = matrix (-1.0) 0.0 0.0 1.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (-1.0)

[<Fact>]
let ``Paper matrix 32 getRotation`` () =
    let m = matrix (-1.0) 0.0 0.0 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 33 getScaling`` () =
    let m = matrix (-1.0) 0.0 0.0 (-1.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (1.0)
    close f.ScaleY (1.0)

[<Fact>]
let ``Paper matrix 34 getRotation`` () =
    let m = matrix 2.0 0.0 0.0 4.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 35 getScaling`` () =
    let m = matrix 2.0 0.0 0.0 4.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (4.0)

[<Fact>]
let ``Paper matrix 36 getRotation`` () =
    let m = matrix 2.0 0.0 0.0 (-4.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 37 getScaling`` () =
    let m = matrix 2.0 0.0 0.0 (-4.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (-4.0)

[<Fact>]
let ``Paper matrix 38 getRotation`` () =
    let m = matrix 4.0 0.0 0.0 2.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (0.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 39 getScaling`` () =
    let m = matrix 4.0 0.0 0.0 2.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (4.0)
    close f.ScaleY (2.0)

[<Fact>]
let ``Paper matrix 40 getRotation`` () =
    let m = matrix (-4.0) 0.0 0.0 2.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 41 getScaling`` () =
    let m = matrix (-4.0) 0.0 0.0 2.0 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (4.0)
    close f.ScaleY (-2.0)

[<Fact>]
let ``Paper matrix 42 getRotation`` () =
    let m = matrix (-4.0) 0.0 0.0 (-4.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (180.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 43 getScaling`` () =
    let m = matrix (-4.0) 0.0 0.0 (-4.0) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (4.0)
    close f.ScaleY (4.0)

[<Fact>]
let ``Paper matrix 44 getRotation`` () =
    let m = matrix 1.4142135623730951 1.414213562373095 (-2.82842712474619) 2.8284271247461903 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (45.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 45 getScaling`` () =
    let m = matrix 1.4142135623730951 1.414213562373095 (-2.82842712474619) 2.8284271247461903 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (4.0)

[<Fact>]
let ``Paper matrix 46 getRotation`` () =
    let m = matrix 1.4142135623730951 1.414213562373095 2.82842712474619 (-2.8284271247461903) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (45.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 47 getScaling`` () =
    let m = matrix 1.4142135623730951 1.414213562373095 2.82842712474619 (-2.8284271247461903) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (-4.0)

[<Fact>]
let ``Paper matrix 48 getRotation`` () =
    let m = matrix (-1.4142135623730951) (-1.414213562373095) (-2.82842712474619) 2.8284271247461903 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-135.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 49 getScaling`` () =
    let m = matrix (-1.4142135623730951) (-1.414213562373095) (-2.82842712474619) 2.8284271247461903 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (-4.0)

[<Fact>]
let ``Paper matrix 50 getRotation`` () =
    let m = matrix (-1.4142135623730951) (-1.414213562373095) 2.82842712474619 (-2.8284271247461903) 0.0 0.0
    let f = Affine.decompose m |> ok
    close (float f.RotationDegrees) (-135.0)
    same m (Affine.recompose f)
    close f.ShearX 0.0
    let options = { TransformSerialize.defaultOptions() with DecimalPlaces = None }
    same m (TransformSerialize.toStringWith m options |> TransformParse.attribute |> ok)
    same m (TransformSerialize.toStringInOrder m Affine.TranslationRotationShearXScale options |> ok |> TransformParse.attribute |> ok)

[<Fact>]
let ``Paper matrix 51 getScaling`` () =
    let m = matrix (-1.4142135623730951) (-1.414213562373095) 2.82842712474619 (-2.8284271247461903) 0.0 0.0
    let f = Affine.decompose m |> ok
    close f.ScaleX (2.0)
    close f.ScaleY (4.0)
