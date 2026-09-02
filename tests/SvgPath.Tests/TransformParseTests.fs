module SvgPath.Tests.TransformParseTests

open SvgPath
open Xunit

module TransformParseTests =
    let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

    let private parsed input =
        TransformParse.attribute input |> Result.defaultWith (failwithf "%A")

    [<Fact>]
    let ``empty attribute is identity`` () =
        Assert.Equal(point 2.0 3.0, Transform.point (parsed "") (point 2.0 3.0))

    [<Fact>]
    let ``matrix transform parses`` () =
        Assert.Equal(point 30.0 40.0, Transform.point (parsed "matrix(2 3 5 7 11 13)") (point 2.0 3.0))

    [<Fact>]
    let ``translate accepts one or two arguments`` () =
        Assert.Equal(point 7.0 3.0, Transform.point (parsed "translate(5)") (point 2.0 3.0))
        Assert.Equal(point 7.0 -4.0, Transform.point (parsed "translate(5 -7)") (point 2.0 3.0))

    [<Fact>]
    let ``scale accepts one or two arguments`` () =
        Assert.Equal(point 8.0 12.0, Transform.point (parsed "scale(4)") (point 2.0 3.0))
        Assert.Equal(point 8.0 -6.0, Transform.point (parsed "scale(4 -2)") (point 2.0 3.0))

    [<Fact>]
    let ``rotate about a center parses`` () =
        let transformed = Transform.point (parsed "rotate(90 1 1)") (point 2.0 1.0)
        Assert.Equal(point 1.0 2.0, transformed)

    [<Fact>]
    let ``skew transforms parse`` () =
        let transform = parsed "skewX(45) skewY(45)"
        Assert.Equal(point 7.0 5.0, Transform.point transform (point 2.0 3.0))

    [<Fact>]
    let ``transform list uses SVG matrix order`` () =
        Assert.Equal(point 12.0 22.0, Transform.point (parsed "translate(10 20) scale(2)") (point 1.0 1.0))

    [<Fact>]
    let ``commas whitespace and compact numbers parse`` () =
        let transform = parsed "translate(10,20)\nscale(.5 -2) rotate(+0e0)"
        Assert.Equal(point 12.0 26.0, Transform.point transform (point 4.0 -3.0))

    [<Fact>]
    let ``transform arguments require comma whitespace`` () =
        Assert.True(Result.isError (TransformParse.attribute "translate(10-20)"))

    [<Fact>]
    let ``transform functions require comma whitespace`` () =
        Assert.True(Result.isError (TransformParse.attribute "translate(1)rotate(2)"))

    [<Fact>]
    let ``invalid comma placements are rejected`` () =
        [ ",translate(1)"; "translate(1),"; "translate(,1)"; "translate(1,)"; "translate(1,,2)" ]
        |> List.iter (TransformParse.attribute >> Result.isError >> Assert.True)

    [<Fact>]
    let ``overflowing numbers are rejected`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.InvalidNumber "1e400", "1e400)")),
            TransformParse.attribute "scale(1e400)")

    [<Fact>]
    let ``finite transforms whose composition overflows are rejected`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.NonFiniteTransform, "scale(1e308)")),
            TransformParse.attribute "scale(1e308) scale(1e308)")

    [<Fact>]
    let ``exponent scaling preserves finite compensated values`` () =
        Assert.True(Result.isOk (TransformParse.attribute "scale(0.1e309)"))

    [<Fact>]
    let ``large exponents do not require linear recursion`` () =
        Assert.True(Result.isError (TransformParse.attribute "scale(1e1000000000)"))
        let underflow = parsed "scale(1e-1000000000)"
        Assert.Equal(point 0.0 0.0, Transform.point underflow (point 2.0 3.0))

    [<Fact>]
    let ``overflowing integer syntax is rejected`` () =
        Assert.True(Result.isError (TransformParse.attribute ("scale(" + String.replicate 400 "9" + ")")))

    [<Fact>]
    let ``zero with a large exponent remains zero`` () =
        let transform = parsed "scale(0e1000000000)"
        Assert.Equal(point 0.0 0.0, Transform.point transform (point 2.0 3.0))

    [<Fact>]
    let ``unknown transform reports its exact remaining suffix`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.UnknownTransform "perspective", "perspective(1)")),
            TransformParse.attribute "perspective(1)")

    [<Fact>]
    let ``wrong argument count reports its exact remaining suffix`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.InvalidArgumentCount("translate", 3), "translate(1 2 3)")),
            TransformParse.attribute "translate(1 2 3)")

    [<Fact>]
    let ``missing close reports end of input`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.ExpectedClose, "")),
            TransformParse.attribute "scale(2")

    [<Fact>]
    let ``error remaining preserves unicode suffix`` () =
        Assert.Equal(
            Error(TransformParse.ParseError(TransformParse.UnexpectedToken "💥", "💥more")),
            TransformParse.attribute "translate(1)💥more")

    [<Fact>]
    let ``serialization canonicalizes parsed transforms`` () =
        Assert.Equal("translate(10 20) scale(2)", parsed "translate(10,20) scale(2)" |> TransformSerialize.toString)
        Assert.Equal("translate(20 40) scale(2)", parsed "scale(2) translate(10 20)" |> TransformSerialize.toString)
