module SvgPath.Tests.RootClassificationRegressionTests
open SvgPath
open Xunit
let private get result = result |> Result.defaultWith (failwithf "%A")
let private classified coefficients = Root.classifiedPolynomialRootsWith coefficients 0.0<parameter> 1.0<parameter> (Root.defaultPolynomialOptions ()) |> get
[<Fact>]
let ``repeated quartic roots retain touch classification`` () =
    for sign in [1.;-1.] do
        let roots = classified (List.map ((*) sign) [1.;-2.;1.375;-0.375;0.03515625])
        Assert.Equal(2,roots.Length)
        let expected = if sign > 0. then PositiveToPositive else NegativeToNegative
        Assert.Equal(expected,roots[0].Kind)
        Assert.Equal(expected,roots[1].Kind)
        Assert.True(roots[0].Isolation.Lower <= 0.25<parameter> && roots[0].Isolation.Upper >= 0.25<parameter>)
        Assert.True(roots[1].Isolation.Lower <= 0.75<parameter> && roots[1].Isolation.Upper >= 0.75<parameter>)
[<Fact>]
let ``inherited odd multiplicity still crosses`` () =
    let roots = classified [1.;-2.375;2.0625;-0.8125;0.1484375;-0.01025390625]
    Assert.Equal<RootKind list>([NegativeToPositive;PositiveToNegative;NegativeToPositive],List.map _.Kind roots)
[<Fact>]
let ``repeated endpoint keeps derivative information`` () =
    let roots = classified [1.;-1.;0.25;0.;0.]
    Assert.Equal(2,roots.Length)
    Assert.Equal(0.0<parameter>,roots[0].Isolation.Estimate)
    Assert.Equal(PositiveToPositive,roots[0].Kind)
    Assert.Equal(PositiveToPositive,roots[1].Kind)
[<Fact>]
let ``exact bisection root receives centered window`` () =
    let isolation = Root.polynomialRootIsolationsWith [1.;0.;1.;-0.625] 0.0<parameter> 1.0<parameter> (Root.defaultPolynomialOptions ()) |> get |> List.exactlyOne
    Assert.Equal(0.5<parameter>,isolation.Estimate)
    Assert.Equal(0.5<parameter> - 0.5e-9<parameter>,isolation.Lower)
    Assert.Equal(0.5<parameter> + 0.5e-9<parameter>,isolation.Upper)
[<Fact>]
let ``exact root on last polynomial iteration succeeds`` () =
    let isolation = Root.polynomialRootIsolationsWith [1.;0.;1.;-0.625] 0.0<parameter> 1.0<parameter> {MaxIterations=1} |> get |> List.exactlyOne
    Assert.Equal(0.5<parameter>,isolation.Estimate)
[<Fact>]
let ``exact root on last certified bisection iteration succeeds`` () =
    Assert.Equal(Ok {Lower=0.5<parameter>;Estimate=0.5<parameter>;Upper=0.5<parameter>},
        Root.bisectIsolationUntil (fun t -> t - 0.5<parameter>) 0.0<parameter> 1.0<parameter> 1 (fun _ _ -> false))
