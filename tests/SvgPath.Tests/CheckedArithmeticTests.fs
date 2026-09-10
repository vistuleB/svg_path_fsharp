module SvgPath.Tests.CheckedArithmeticTests
open SvgPath
open Xunit
let private maximum = System.Double.MaxValue
[<Fact>]
let ``checked product rejects rounded guard boundary`` () =
    let boundary = maximum / 3.
    for sign in [1.;-1.] do
        Assert.Equal(Error(),InternalNumber.checkedProduct (sign * boundary) 3.)
        Assert.Equal(Error(),InternalNumber.checkedProduct 3. (sign * boundary))
[<Fact>]
let ``checked sum rejects rounded guard boundary`` () =
    let a,b = 1.7658771479739417e308,3.181598688837415e306
    for sign in [1.;-1.] do
        Assert.Equal(Error(),InternalNumber.checkedSum (sign * a) (sign * b))
        Assert.Equal(Error(),InternalNumber.checkedSum (sign * b) (sign * a))
[<Fact>]
let ``checked arithmetic accepts finite extremes`` () =
    Assert.Equal(Ok maximum,InternalNumber.checkedProduct maximum 1.)
    Assert.Equal(Ok -maximum,InternalNumber.checkedProduct maximum -1.)
    Assert.Equal(Ok maximum,InternalNumber.checkedProduct (maximum/2.) 2.)
    Assert.Equal(Ok maximum,InternalNumber.checkedSum (maximum/2.) (maximum/2.))
    Assert.Equal(Ok 0.,InternalNumber.checkedSum maximum -maximum)
    Assert.Equal(Ok maximum,InternalNumber.checkedSum maximum 0.)
    Assert.Equal(Ok 0.,InternalNumber.checkedProduct maximum 0.)
[<Fact>]
let ``checked arithmetic accepts ordinary values and underflow`` () =
    Assert.Equal(Ok 3.75,InternalNumber.checkedSum 1.25 2.5)
    Assert.Equal(Ok -6.,InternalNumber.checkedProduct -2. 3.)
    Assert.Equal(Ok 0.,InternalNumber.checkedProduct 1e-300 1e-300)
[<Fact>]
let ``transform parser reports product overflow without raising`` () =
    let input = "scale(59923104495410526931242990468434471693311377570012608978724592993481656097588250315549672659195735698776762138897629303648851849283980134210219162890501940227302967333569461225424618281939237177254825243423356618523788986540947638273286944978825097573024722814788503568114237186566502697680960059301391499264) scale(3)"
    match TransformParse.attribute input with
    | Error(TransformParse.ParseError(TransformParse.NonFiniteTransform,_)) -> ()
    | other -> failwithf "Expected nonfinite-transform error: %A" other
