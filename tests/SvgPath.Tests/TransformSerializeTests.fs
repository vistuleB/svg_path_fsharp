module SvgPath.Tests.TransformSerializeTests

open SvgPath
open Xunit

module TransformSerializeTests =
    [<Fact>]
    let ``transform translate serializes nicely`` () =
        Assert.Equal("translate(10)", Transform.translate 10.0<length> 0.0<length> |> TransformSerialize.toString)
        Assert.Equal("translate(10 20)", Transform.translate 10.0<length> 20.0<length> |> TransformSerialize.toString)

    [<Fact>]
    let ``transform scale serializes nicely`` () =
        Assert.Equal("scale(2)", Transform.scale 2.0 |> TransformSerialize.toString)
        Assert.Equal("scale(2 3)", Transform.scaleXY 2.0 3.0 |> TransformSerialize.toString)

    [<Fact>]
    let ``transform rotate serializes nicely`` () =
        Assert.Equal("rotate(90)", Transform.rotate 90.0<degree> |> TransformSerialize.toString)

    [<Fact>]
    let ``transform scaled rotate serializes nicely`` () =
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.rotate 90.0<degree>)
        Assert.Equal("rotate(90) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform rotation scale recognition is scale independent`` () =
        let transform =
            Transform.scaleXY 2.0e6 3.0e6
            |> fun scale -> Transform.chain scale (Transform.rotate 30.0<degree>)
        Assert.Equal("rotate(30) scale(2000000 3000000)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform translate scale serializes nicely`` () =
        let transform =
            Transform.scale 2.0
            |> fun scale -> Transform.chain scale (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("translate(10 20) scale(2)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform translate scale xy serializes nicely`` () =
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("translate(10 20) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform translate scaled rotate serializes nicely`` () =
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.rotate 90.0<degree>)
            |> fun rotated -> Transform.chain rotated (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("translate(10 20) rotate(90) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform skew serializes nicely`` () =
        Assert.Equal(
            "skewX(45)",
            Transform.skewX 45.0<degree>
            |> fun transform -> TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))
        Assert.Equal(
            "skewY(-30)",
            Transform.skewY -30.0<degree>
            |> fun transform -> TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))

    [<Fact>]
    let ``transform translate skew serializes nicely`` () =
        let transform =
            Transform.skewX 45.0<degree>
            |> fun skew -> Transform.chain skew (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("translate(10 20) skewX(45)", TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))

    [<Fact>]
    let ``transform matrix fallback serializes`` () =
        let transform = Transform.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length>
        Assert.Equal("matrix(2 3 5 7 11 13)", TransformSerialize.toString transform)

    [<Fact>]
    let ``transform serialization uses decimal options`` () =
        let transform = Transform.translate 10.234<length> -20.235<length>
        Assert.Equal(
            "translate(10.23 -20.24)",
            TransformSerialize.toStringWith transform (TransformSerialize.fixedDecimalOptions 2))

    [<Fact>]
    let ``transform serialization uses scientific notation when scaling is unsafe`` () =
        let transform = Transform.translate 1.0e20<length> -2.5e20<length>
        Assert.Equal(
            "translate(1.00e20 -2.50e20)",
            TransformSerialize.toStringWith transform (TransformSerialize.fixedDecimalOptions 2))

    [<Fact>]
    let ``transform serialization preserves near identity rotation scale`` () =
        let transform =
            Transform.scale 1.0000005
            |> fun scale -> Transform.chain scale (Transform.rotate 90.0<degree>)
        let options =
            { DecimalPlaces = None
              FixedDecimals = false
              ForceMatrix = false }
        Assert.Equal("rotate(90) scale(1.0000005)", TransformSerialize.toStringWith transform options)

        let machineEpsilonScale =
            Transform.scale 1.00000000000005
            |> fun scale -> Transform.chain scale (Transform.rotate 30.125<degree>)
        Assert.DoesNotContain("scale", TransformSerialize.toStringWith machineEpsilonScale options)

    [<Fact>]
    let ``transform serialization can force matrix output`` () =
        let options = TransformSerialize.defaultOptions () |> TransformSerialize.forceMatrix
        Assert.Equal(
            "matrix(1 0 0 1 10 20)",
            TransformSerialize.toStringWith (Transform.translate 10.0<length> 20.0<length>) options)

    [<Fact>]
    let ``transform translate scale can force matrix output`` () =
        let options = TransformSerialize.defaultOptions () |> TransformSerialize.forceMatrix
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("matrix(2 0 0 3 10 20)", TransformSerialize.toStringWith transform options)

    [<Fact>]
    let ``parsed transform serializes to canonical translate scale`` () =
        let transform = TransformParse.attribute "translate(10,20) scale(2)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("translate(10 20) scale(2)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed transform serializes to canonical scale translate`` () =
        let transform = TransformParse.attribute "scale(2) translate(10 20)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("translate(20 40) scale(2)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed matrix serializes to nicer transform`` () =
        let transform = TransformParse.attribute "matrix(2 0 0 3 10 20)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("translate(10 20) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed rotation matrix serializes to nicer transform`` () =
        let transform = TransformParse.attribute "matrix(0 2 -3 0 10 20)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("translate(10 20) rotate(90) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed rotate transform serializes nicely`` () =
        let transform = TransformParse.attribute "rotate(30.125)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("rotate(30.125)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed translate rotate scale transform serializes nicely`` () =
        let transform = TransformParse.attribute "translate(10 20) rotate(30.125) scale(2 3)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("translate(10 20) rotate(30.125) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed rotate transform uses requested decimal options`` () =
        let transform = TransformParse.attribute "rotate(30.000001)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("rotate(30.000001)", TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 6))

    [<Fact>]
    let ``reflected rotation matrix uses matrix fallback`` () =
        let transform = Transform.matrix 0.0 2.0 3.0 0.0 10.0<length> 20.0<length>
        Assert.Equal("matrix(0 2 3 0 10 20)", TransformSerialize.toString transform)

    [<Fact>]
    let ``parsed unmatched transform serializes to matrix`` () =
        let transform = TransformParse.attribute "skewX(30) scale(2)" |> Result.defaultWith (failwithf "%A")
        Assert.Equal("matrix(2 0 1.155 2 0 0)", TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))
