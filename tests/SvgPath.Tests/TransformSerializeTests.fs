module SvgPath.Tests.TransformSerializeTests

open SvgPath
open Xunit

module TransformSerializeTests =
    [<Fact>]
    let ``translations serialize compactly`` () =
        Assert.Equal("translate(10)", Transform.translate 10.0<length> 0.0<length> |> TransformSerialize.toString)
        Assert.Equal("translate(10 20)", Transform.translate 10.0<length> 20.0<length> |> TransformSerialize.toString)

    [<Fact>]
    let ``scales and rotations serialize as readable transforms`` () =
        Assert.Equal("scale(2)", Transform.scale 2.0 |> TransformSerialize.toString)
        Assert.Equal("scale(2 3)", Transform.scaleXY 2.0 3.0 |> TransformSerialize.toString)
        Assert.Equal("rotate(90)", Transform.rotate 90.0<degree> |> TransformSerialize.toString)

    [<Fact>]
    let ``rotation and nonuniform scale retain their order`` () =
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.rotate 90.0<degree>)
        Assert.Equal("rotate(90) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``rotation scale recognition is scale independent`` () =
        let transform =
            Transform.scaleXY 2.0e6 3.0e6
            |> fun scale -> Transform.chain scale (Transform.rotate 30.0<degree>)
        Assert.Equal("rotate(30) scale(2000000 3000000)", TransformSerialize.toString transform)

    [<Fact>]
    let ``translation precedes the readable linear transform`` () =
        let transform =
            Transform.scaleXY 2.0 3.0
            |> fun scale -> Transform.chain scale (Transform.translate 10.0<length> 20.0<length>)
        Assert.Equal("translate(10 20) scale(2 3)", TransformSerialize.toString transform)

    [<Fact>]
    let ``skews serialize in degrees`` () =
        Assert.Equal(
            "skewX(45)",
            Transform.skewX 45.0<degree>
            |> fun transform -> TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))
        Assert.Equal(
            "skewY(-30)",
            Transform.skewY -30.0<degree>
            |> fun transform -> TransformSerialize.toStringWith transform (TransformSerialize.decimalOptions 3))

    [<Fact>]
    let ``unrecognized linear transforms fall back to matrices`` () =
        let transform = Transform.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length>
        Assert.Equal("matrix(2 3 5 7 11 13)", TransformSerialize.toString transform)

    [<Fact>]
    let ``fixed decimal options apply to translations`` () =
        let transform = Transform.translate 10.234<length> -20.235<length>
        Assert.Equal(
            "translate(10.23 -20.24)",
            TransformSerialize.toStringWith transform (TransformSerialize.fixedDecimalOptions 2))

    [<Fact>]
    let ``fixed decimal options use scientific notation when scaling is unsafe`` () =
        let transform = Transform.translate 1.0e20<length> -2.5e20<length>
        Assert.Equal(
            "translate(1.00e20 -2.50e20)",
            TransformSerialize.toStringWith transform (TransformSerialize.fixedDecimalOptions 2))

    [<Fact>]
    let ``near identity rotation scales are not discarded`` () =
        let transform =
            Transform.scale 1.0000005
            |> fun scale -> Transform.chain scale (Transform.rotate 90.0<degree>)
        let options =
            { DecimalPlaces = None
              FixedDecimals = false
              ForceMatrix = false }
        Assert.Equal("rotate(90) scale(1.0000005)", TransformSerialize.toStringWith transform options)

    [<Fact>]
    let ``matrix output can be forced`` () =
        let options = TransformSerialize.defaultOptions () |> TransformSerialize.forceMatrix
        Assert.Equal(
            "matrix(1 0 0 1 10 20)",
            TransformSerialize.toStringWith (Transform.translate 10.0<length> 20.0<length>) options)

    [<Fact>]
    let ``reflections use matrix fallback`` () =
        let transform = Transform.matrix 0.0 2.0 3.0 0.0 10.0<length> 20.0<length>
        Assert.Equal("matrix(0 2 3 0 10 20)", TransformSerialize.toString transform)
