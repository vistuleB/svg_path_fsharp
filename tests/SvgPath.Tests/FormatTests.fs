namespace SvgPath.Tests

open SvgPath
open Xunit

module FormatAdditionalTests =
    let private options left right =
        { LeftDecimals = left
          RightDecimals = right }

    [<Fact>]
    let ``at most decimals round and strip trailing zeros`` () =
        let format = NumberFormat.prepare (options Succinct (AtMost 2)) []
        Assert.Equal("12.35", NumberFormat.number 12.345 format)
        Assert.Equal("12", NumberFormat.number 12.0001 format)

    [<Fact>]
    let ``fixed decimals retain trailing zeros`` () =
        let format = NumberFormat.prepare (options Succinct (Fixed 3)) []
        Assert.Equal("12.000", NumberFormat.number 12.0 format)

    [<Fact>]
    let ``code numbers retain an explicit fractional suffix`` () =
        let format = NumberFormat.prepare (options Succinct (AtMost 3)) []
        Assert.Equal("12.0", NumberFormat.codeNumber 12.0 format)

    [<Fact>]
    let ``zero padding follows a negative sign`` () =
        let format = NumberFormat.prepare (options (LeftPadding(4, Zero)) (Fixed 1)) []
        Assert.Equal("-003.5", NumberFormat.number -3.5 format)
        Assert.Equal("0003.5", NumberFormat.number 3.5 format)

    [<Fact>]
    let ``automatic padding measures formatted significands`` () =
        let options = options (AutoLeftPadding Space) (Fixed 1)
        let format = NumberFormat.prepare options [ -12.0; 3.0 ]
        Assert.Equal("  3.0", NumberFormat.number 3.0 format)
        Assert.Equal("-12.0", NumberFormat.number -12.0 format)

    [<Fact>]
    let ``unsafe fixed scaling uses normalized scientific notation`` () =
        let format = NumberFormat.prepare (options Succinct (Fixed 2)) []
        Assert.Equal("1.00e20", NumberFormat.number 1.0e20 format)

    [<Fact>]
    let ``system formatting normalizes scientific exponents`` () =
        let format = NumberFormat.prepare (options Succinct System) []
        Assert.Equal("1e20", NumberFormat.number 1.0e20 format)
        Assert.Equal("-1e-20", NumberFormat.number -1.0e-20 format)

    [<Fact>]
    let ``negative decimal places clamp to zero`` () =
        let format = NumberFormat.prepare (options Succinct (Fixed -4)) []
        Assert.Equal("13", NumberFormat.number 12.6 format)

    [<Fact>]
    let ``fixed formatting does not invent negative zero`` () =
        let format = NumberFormat.prepare (options Succinct (Fixed 2)) []
        Assert.Equal("0.00", NumberFormat.number -0.001 format)
