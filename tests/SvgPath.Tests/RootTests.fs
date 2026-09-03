module SvgPath.Tests.RootTests

open SvgPath
open Xunit

let private coefficient value = Length.fromFloat value
let private parameter value = Parameter.fromFloat value
let private rootFloat value = Parameter.ratio value
let private near expected actual = Assert.Equal(expected, rootFloat actual, 6)
let private unwrap result = Result.defaultWith (fun error -> failwithf "%A" error) result

[<Fact>]
let ``linear root`` () =
    Assert.Equal<float<parameter> list>([ parameter 0.5 ], Root.linear (coefficient 2.0) (coefficient -1.0))
    Assert.Empty(Root.linear (coefficient 0.0) (coefficient 1.0))
    Assert.Empty(Root.linear (coefficient 0.0) (coefficient 0.0))

[<Fact>]
let ``quadratic real roots`` () =
    Assert.Equal<float<parameter> list>(
        [ parameter 1.0; parameter 2.0 ],
        Root.quadratic (coefficient 1.0) (coefficient -3.0) (coefficient 2.0)
    )

[<Fact>]
let ``quadratic preserves small root under large scale separation`` () =
    Assert.Equal<float<parameter> list>(
        [ parameter 1.0e-16; parameter 1.0e16 ],
        Root.quadratic (coefficient 1.0) (coefficient -1.0e16) (coefficient 1.0)
    )

[<Fact>]
let ``quadratic reduces to linear`` () =
    Assert.Equal<float<parameter> list>(
        [ parameter 0.5 ],
        Root.quadratic (coefficient 0.0) (coefficient 2.0) (coefficient -1.0)
    )

[<Fact>]
let ``quadratic repeated root policy`` () =
    let preserve =
        { CoefficientTolerance = coefficient 0.0
          RepeatedRootPolicy = PreserveRepeatedRoot }

    Assert.Equal<float<parameter> list>(
        [ parameter 1.0 ],
        Root.quadratic (coefficient 1.0) (coefficient -2.0) (coefficient 1.0)
    )
    Assert.Equal<float<parameter> list>(
        [ parameter 1.0; parameter 1.0 ],
        Root.quadraticWith preserve (coefficient 1.0) (coefficient -2.0) (coefficient 1.0)
    )

[<Fact>]
let ``quadratic coefficient tolerance`` () =
    let tolerant =
        { CoefficientTolerance = coefficient 1.0e-6
          RepeatedRootPolicy = ConsolidateRepeatedRoot }

    Assert.Equal<float<parameter> list>(
        [ parameter 0.5 ],
        Root.quadraticWith tolerant (coefficient 1.0e-7) (coefficient 2.0) (coefficient -1.0)
    )

[<Fact>]
let ``root interval filters and sorts`` () =
    let roots = [ parameter 1.0; parameter 0.75; parameter -0.1; parameter 0.25; parameter 0.0 ]

    Assert.Equal<float<parameter> list>(
        [ parameter 0.25; parameter 0.75 ],
        Root.strictlyInside roots (parameter 0.0) (parameter 1.0)
    )

    Assert.Equal<float<parameter> list>(
        [ parameter 0.0; parameter 0.25; parameter 0.75; parameter 1.0 ],
        Root.inside roots (parameter 1.0) (parameter 0.0)
    )

[<Fact>]
let ``evaluate polynomial and derivative`` () =
    let coefficients = [ coefficient 2.0; coefficient -3.0; coefficient 4.0 ]
    let value: float<length> = Root.evaluatePolynomial coefficients (parameter 2.0)
    let derivative: float<length> list = Root.polynomialDerivative coefficients

    Assert.Equal(6.0, Length.toFloat value, 12)
    Assert.Equal<float<length> list>([ coefficient 4.0; coefficient -3.0 ], derivative)

[<Fact>]
let ``cubic finds three real roots`` () =
    let three =
        Root.cubic (coefficient 1.0) (coefficient -6.0) (coefficient 11.0) (coefficient -6.0)
        |> unwrap

    Assert.Equal(3, three.Length)
    List.iter2 near [ 1.0; 2.0; 3.0 ] three

[<Fact>]
let ``cubic preserves repeated root`` () =
    let repeated =
        Root.cubic (coefficient 1.0) (coefficient 0.0) (coefficient -3.0) (coefficient 2.0)
        |> unwrap

    Assert.Equal(2, repeated.Length)
    List.iter2 near [ -2.0; 1.0 ] repeated

[<Fact>]
let ``polynomial roots find even multiplicity root`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let repeated =
        Root.polynomialRootsWith
            [ coefficient 1.0; coefficient -2.0; coefficient 1.0 ]
            (parameter 0.0)
            (parameter 2.0)
            options
        |> unwrap

    Assert.Single(repeated) |> ignore
    near 1.0 repeated.Head

[<Fact>]
let ``quintic roots in unit interval`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let quintic =
        Root.polynomialRootsWith
            [ coefficient 1.0
              coefficient -2.5
              coefficient 2.3
              coefficient -0.95
              coefficient 0.1689
              coefficient -0.00945 ]
            (parameter 0.0)
            (parameter 1.0)
            options
        |> unwrap

    Assert.Equal(5, quintic.Length)
    List.iter2 near [ 0.1; 0.3; 0.5; 0.7; 0.9 ] quintic

[<Fact>]
let ``polynomial isolation preserves crossing bracket`` () =
    let options: PolynomialOptions = { MaxIterations = 100 }

    let coefficients = [ coefficient 1.0; coefficient 0.0; coefficient 0.0; coefficient -2.0 ]

    let isolation =
        Root.polynomialRootIsolationsWith coefficients (parameter 0.0) (parameter 2.0) options
        |> unwrap
        |> List.exactlyOne

    Assert.True(isolation.Lower <= isolation.Estimate && isolation.Estimate <= isolation.Upper)

    let lowerValue = Root.evaluatePolynomial coefficients isolation.Lower
    let upperValue = Root.evaluatePolynomial coefficients isolation.Upper
    Assert.True(lowerValue * upperValue <= 0.0<length^2>)
    Assert.True(isolation.Upper - isolation.Lower <= parameter 1.0e-9)

[<Fact>]
let ``direct polynomial root receives fixed parameter window`` () =
    let isolation =
        Root.polynomialRootIsolationsWith
            [ coefficient 1.0; coefficient -1.0; coefficient 0.25 ]
            (parameter 0.0)
            (parameter 1.0)
            (Root.defaultPolynomialOptions ())
        |> unwrap
        |> List.exactlyOne

    Assert.Equal(parameter 0.5, isolation.Estimate)
    Assert.Equal(parameter (0.5 - 1.0e-9 / 2.0), isolation.Lower)
    Assert.Equal(parameter (0.5 + 1.0e-9 / 2.0), isolation.Upper)

[<Fact>]
let ``polynomial classification is coefficient scale independent`` () =
    let classify scale =
        Root.classifiedPolynomialRootsWith
            [ coefficient scale; coefficient 0.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            (Root.defaultPolynomialOptions ())
        |> unwrap
        |> List.exactlyOne
        |> _.Kind

    Assert.Equal(PositiveToPositive, classify 1.0e-18)
    Assert.Equal(PositiveToPositive, classify 1.0)
    Assert.Equal(PositiveToPositive, classify 1.0e18)

[<Fact>]
let ``polynomial bisection handles flat derivative region`` () =
    let solution =
        Root.polynomialRootsWith
            [ coefficient 1.0; coefficient 0.0; coefficient 0.0; coefficient -0.001 ]
            (parameter -1.0)
            (parameter 1.0)
            (Root.defaultPolynomialOptions ())
        |> unwrap
        |> List.exactlyOne

    near 0.1 solution

[<Fact>]
let ``classified polynomial roots report sign changes`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let crossing =
        Root.classifiedPolynomialRootsWith
            [ coefficient 1.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    near 0.0 crossing.Isolation.Estimate
    Assert.Equal(NegativeToPositive, crossing.Kind)
    Assert.True(Root.isSignChangeRoot NegativeToPositive)
    Assert.True(Root.isCrossingRoot PositiveToNegative)

[<Fact>]
let ``classified polynomial roots report even roots`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let positiveEven =
        Root.classifiedPolynomialRootsWith
            [ coefficient 1.0; coefficient 0.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    let negativeEven =
        Root.classifiedPolynomialRootsWith
            [ coefficient -1.0; coefficient 0.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    Assert.Equal(PositiveToPositive, positiveEven.Kind)
    Assert.False(Root.isSignChangeRoot positiveEven.Kind)
    Assert.Equal(NegativeToNegative, negativeEven.Kind)
    Assert.False(Root.isCrossingRoot negativeEven.Kind)

[<Fact>]
let ``classified polynomial roots classify endpoint roots`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let startPoint =
        Root.classifiedPolynomialRootsWith
            [ coefficient 1.0; coefficient 0.0 ]
            (parameter 0.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    let endPoint =
        Root.classifiedPolynomialRootsWith
            [ coefficient -1.0; coefficient 1.0 ]
            (parameter 0.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    let negativeEvenEndpoint =
        Root.classifiedPolynomialRootsWith
            [ coefficient -1.0; coefficient 0.0; coefficient 0.0 ]
            (parameter 0.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    Assert.Equal(NegativeToPositive, startPoint.Kind)
    Assert.Equal(PositiveToNegative, endPoint.Kind)
    Assert.Equal(NegativeToNegative, negativeEvenEndpoint.Kind)

[<Fact>]
let ``classified polynomial roots reject invalid iterations`` () =
    let options: PolynomialOptions =
        { Root.defaultPolynomialOptions () with
            MaxIterations = 0 }

    match
        Root.realLinear01Roots (coefficient 1.0) (coefficient -0.5) options
    with
    | Error(InvalidMaxIterations 0) -> ()
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``real 01 root helpers classify by degree`` () =
    let options: PolynomialOptions = Root.defaultPolynomialOptions ()

    let linear =
        Root.realLinear01Roots (coefficient 1.0) (coefficient -0.25) options
        |> unwrap
        |> List.exactlyOne

    let quadratic =
        Root.realQuadratic01Roots
            (coefficient 1.0)
            (coefficient -1.0)
            (coefficient 0.25)
            options
        |> unwrap
        |> List.exactlyOne

    let cubic =
        Root.realCubic01Roots
            (coefficient 1.0)
            (coefficient 0.0)
            (coefficient 0.0)
            (coefficient -0.125)
            options
        |> unwrap
        |> List.exactlyOne

    near 0.25 linear.Isolation.Estimate
    Assert.Equal(NegativeToPositive, linear.Kind)
    near 0.5 quadratic.Isolation.Estimate
    Assert.Equal(PositiveToPositive, quadratic.Kind)
    near 0.5 cubic.Isolation.Estimate
    Assert.Equal(NegativeToPositive, cubic.Kind)

[<Fact>]
let ``bisect isolation until preserves certified bracket`` () =
    let isolation =
        Root.bisectIsolationUntil
            (fun t -> coefficient (Parameter.ratio t - 0.3))
            (parameter 0.0)
            (parameter 1.0)
            100
            (fun left right -> right - left <= parameter 0.01)
        |> unwrap

    Assert.True(isolation.Lower <= parameter 0.3)
    Assert.True(isolation.Upper >= parameter 0.3)
    Assert.True(isolation.Upper - isolation.Lower <= parameter 0.01)

[<Fact>]
let ``bisect isolation until can use non parameter certification`` () =
    let isolation =
        Root.bisectIsolationUntil
            (fun t -> coefficient (Parameter.ratio t - 0.3))
            (parameter 0.0)
            (parameter 1.0)
            100
            (fun left right -> Parameter.ratio (right - left) * 1000.0 <= 0.01)
        |> unwrap

    Assert.True(isolation.Lower <= parameter 0.3)
    Assert.True(isolation.Upper >= parameter 0.3)
    Assert.True(Parameter.ratio (isolation.Upper - isolation.Lower) * 1000.0 <= 0.01)

[<Fact>]
let ``bisect isolation until stops at float resolution`` () =
    let isolation =
        Root.bisectIsolationUntil
            (fun t -> coefficient (Parameter.ratio t * Parameter.ratio t - 2.0))
            (parameter 1.0)
            (parameter 2.0)
            200
            (fun _ _ -> false)
        |> unwrap

    Assert.True(isolation.Lower < isolation.Upper)
    Assert.True(Parameter.ratio isolation.Lower ** 2.0 <= 2.0)
    Assert.True(Parameter.ratio isolation.Upper ** 2.0 >= 2.0)
