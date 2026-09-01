module SvgPath.Tests.RootTests

open SvgPath
open Xunit

let private coefficient value = Length.fromFloat value
let private parameter value = Parameter.fromFloat value
let private rootFloat value = Parameter.ratio value
let private near expected actual = Assert.Equal(expected, rootFloat actual, 6)
let private unwrap result = Result.defaultWith (fun error -> failwithf "%A" error) result

[<Fact>]
let ``linear and quadratic solvers return parameter roots`` () =
    Assert.Equal<float<parameter> list>([ parameter 0.5 ], Root.linear (coefficient 2.0) (coefficient -1.0))
    Assert.Empty(Root.linear (coefficient 0.0) (coefficient 1.0))

    Assert.Equal<float<parameter> list>(
        [ parameter 1.0; parameter 2.0 ],
        Root.quadratic (coefficient 1.0) (coefficient -3.0) (coefficient 2.0)
    )

[<Fact>]
let ``quadratic solver preserves a small root under cancellation pressure`` () =
    Assert.Equal<float<parameter> list>(
        [ parameter 1.0e-16; parameter 1.0e16 ],
        Root.quadratic (coefficient 1.0) (coefficient -1.0e16) (coefficient 1.0)
    )

[<Fact>]
let ``quadratic degree and repeated-root policies are explicit`` () =
    let preserve =
        { CoefficientTolerance = coefficient 0.0
          RepeatedRootPolicy = PreserveRepeatedRoot }

    let tolerant =
        { CoefficientTolerance = coefficient 1.0e-6
          RepeatedRootPolicy = ConsolidateRepeatedRoot }

    Assert.Equal<float<parameter> list>(
        [ parameter 1.0; parameter 1.0 ],
        Root.quadraticWith preserve (coefficient 1.0) (coefficient -2.0) (coefficient 1.0)
    )

    Assert.Equal<float<parameter> list>(
        [ parameter 0.5 ],
        Root.quadraticWith tolerant (coefficient 1.0e-7) (coefficient 2.0) (coefficient -1.0)
    )

[<Fact>]
let ``interval filters order nominal parameters`` () =
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
let ``polynomial evaluation and differentiation preserve coefficient units`` () =
    let coefficients = [ coefficient 2.0; coefficient -3.0; coefficient 4.0 ]
    let value: float<length> = Root.evaluatePolynomial coefficients (parameter 2.0)
    let derivative: float<length> list = Root.polynomialDerivative coefficients

    Assert.Equal(6.0, Length.toFloat value, 12)
    Assert.Equal<float<length> list>([ coefficient 4.0; coefficient -3.0 ], derivative)

[<Fact>]
let ``cubic solver finds distinct and repeated real roots`` () =
    let three =
        Root.cubic (coefficient 1.0) (coefficient -6.0) (coefficient 11.0) (coefficient -6.0)
        |> unwrap

    let repeated =
        Root.cubic (coefficient 1.0) (coefficient 0.0) (coefficient -3.0) (coefficient 2.0)
        |> unwrap

    Assert.Equal(3, three.Length)
    List.iter2 near [ 1.0; 2.0; 3.0 ] three
    Assert.Equal(2, repeated.Length)
    List.iter2 near [ -2.0; 1.0 ] repeated

[<Fact>]
let ``polynomial isolation preserves even roots and five simple roots`` () =
    let options: PolynomialOptions<length> = Root.defaultPolynomialOptions ()

    let repeated =
        Root.polynomialRootsWith
            [ coefficient 1.0; coefficient -2.0; coefficient 1.0 ]
            (parameter 0.0)
            (parameter 2.0)
            options
        |> unwrap

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

    Assert.Single(repeated) |> ignore
    near 1.0 repeated.Head
    Assert.Equal(5, quintic.Length)
    List.iter2 near [ 0.1; 0.3; 0.5; 0.7; 0.9 ] quintic

[<Fact>]
let ``root isolation retains a sign-changing bracket`` () =
    let options: PolynomialOptions<length> =
        { CoefficientTolerance = coefficient 1.0e-12
          ParameterTolerance = parameter 0.01
          ValueTolerance = coefficient 1.0e-12
          MaxIterations = 100 }

    let coefficients = [ coefficient 1.0; coefficient 0.0; coefficient 0.0; coefficient -2.0 ]

    let isolation =
        Root.polynomialRootIsolationsWith coefficients (parameter 0.0) (parameter 2.0) options
        |> unwrap
        |> List.exactlyOne

    Assert.True(isolation.Lower <= isolation.Estimate && isolation.Estimate <= isolation.Upper)

    let lowerValue = Root.evaluatePolynomial coefficients isolation.Lower
    let upperValue = Root.evaluatePolynomial coefficients isolation.Upper
    Assert.True(lowerValue * upperValue <= 0.0<length^2>)
    Assert.True((isolation.Upper - isolation.Lower) / 2.0 <= options.ParameterTolerance)

[<Fact>]
let ``classified roots distinguish crossings even roots and endpoints`` () =
    let options: PolynomialOptions<length> = Root.defaultPolynomialOptions ()

    let crossing =
        Root.classifiedPolynomialRootsWith
            [ coefficient 1.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    let positiveEven =
        Root.classifiedPolynomialRootsWith
            [ coefficient 1.0; coefficient 0.0; coefficient 0.0 ]
            (parameter -1.0)
            (parameter 1.0)
            options
        |> unwrap
        |> List.exactlyOne

    let endpoint =
        Root.realLinear01Roots (coefficient 1.0) (coefficient 0.0) options
        |> unwrap
        |> List.exactlyOne

    Assert.Equal(NegativeToPositive, crossing.Kind)
    Assert.True(Root.isSignChangeRoot crossing.Kind)
    Assert.Equal(PositiveToPositive, positiveEven.Kind)
    Assert.False(Root.isCrossingRoot positiveEven.Kind)
    Assert.Equal(Ambiguous, endpoint.Kind)

[<Fact>]
let ``classified root isolation validates maximum iterations`` () =
    let options: PolynomialOptions<length> =
        { Root.defaultPolynomialOptions () with
            MaxIterations = 0 }

    match
        Root.realLinear01Roots (coefficient 1.0) (coefficient -0.5) options
    with
    | Error(InvalidMaxIterations 0) -> ()
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``degree-specific unit-interval helpers classify roots`` () =
    let options: PolynomialOptions<length> = Root.defaultPolynomialOptions ()

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
let ``bisection separates value tolerance from parameter tolerance`` () =
    let options: BisectionOptions<length> =
        { ParameterTolerance = parameter 1.0e-9
          ValueTolerance = coefficient 1.0e-12
          MaxIterations = 100 }

    let solution =
        Root.bisectWith
            (fun t -> coefficient (Parameter.ratio t * Parameter.ratio t - 2.0))
            (parameter 0.0)
            (parameter 2.0)
            options
        |> unwrap

    near 1.414213562 solution

[<Fact>]
let ``bisection reports bracket and iteration errors`` () =
    let options: BisectionOptions<length> =
        { ParameterTolerance = parameter 1.0e-9
          ValueTolerance = coefficient 1.0e-9
          MaxIterations = 1 }

    match
        Root.bisectWith
            (fun t -> coefficient (Parameter.ratio t - 0.3))
            (parameter 0.0)
            (parameter 1.0)
            options
    with
    | Error(MaxIterationsReached(estimate, value)) ->
        near 0.5 estimate
        Assert.Equal(0.2, Length.toFloat value, 12)
    | result -> failwithf "unexpected result: %A" result

    match
        Root.bisect
            (fun t -> coefficient (Parameter.ratio t * Parameter.ratio t + 1.0))
            (parameter 0.0)
            (parameter 2.0)
    with
    | Error(NotBracketed _) -> ()
    | result -> failwithf "unexpected result: %A" result

[<Fact>]
let ``custom bisection certification preserves its bracket`` () =
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
let ``custom bisection stops at floating-point resolution`` () =
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
