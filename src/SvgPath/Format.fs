namespace SvgPath

open System
open System.Globalization

type LeftPaddingStyle =
    | Zero
    | Space

type LeftDecimalOptions =
    | Succinct
    | AutoLeftPadding of LeftPaddingStyle
    | LeftPadding of width: int * style: LeftPaddingStyle

type RightDecimalOptions =
    | System
    | AtMost of decimalPlaces: int
    | Fixed of decimalPlaces: int

[<Struct>]
type NumberFormatOptions =
    { LeftDecimals: LeftDecimalOptions
      RightDecimals: RightDecimalOptions }

type NumberFormat =
    private
        { Options: NumberFormatOptions
          LeftPadding: (int * LeftPaddingStyle) option }

[<RequireQualifiedAccess>]
module NumberFormat =
    let private invariant = CultureInfo.InvariantCulture
    let private maximumDecimalPlaces = 100

    let private splitExponent (number: string) =
        let lower = number.IndexOf('e')
        let upper = number.IndexOf('E')
        let index = if lower >= 0 then lower else upper
        if index < 0 then number, ""
        else number.Substring(0, index), number.Substring(index)

    let private stripTrailingZeros (value: string) = value.TrimEnd('0')

    let private stripTrailingDecimalZeros number =
        let significand, exponent = splitExponent number
        let dot = significand.IndexOf('.')
        if dot < 0 then number
        else
            let whole = significand.Substring(0, dot)
            let fractional = stripTrailingZeros (significand.Substring(dot + 1))
            if String.IsNullOrEmpty fractional then whole + exponent
            else whole + "." + fractional + exponent

    let private powerOfTenInt exponent =
        let rec loop remaining value =
            if remaining <= 0 then value else loop (remaining - 1) (value * 10L)
        loop exponent 1L

    let private powerOfTen exponent = float (powerOfTenInt exponent)

    let private fixedDecimalIsSafe number decimalPlaces =
        decimalPlaces <= 15
        && abs number * powerOfTen decimalPlaces <= 9_007_199_254_740_992.0

    let private fixedDecimal number decimalPlaces =
        let decimalPlaces = max decimalPlaces 0
        let scale = powerOfTen decimalPlaces
        let scaled = Math.Round(number * scale, MidpointRounding.AwayFromZero) |> int64
        let sign = if scaled < 0L then "-" else ""
        let absoluteScaled = abs scaled
        if decimalPlaces = 0 then sign + absoluteScaled.ToString(invariant)
        else
            let integerScale = powerOfTenInt decimalPlaces
            let whole = absoluteScaled / integerScale
            let fractional = absoluteScaled % integerScale
            sign
            + whole.ToString(invariant)
            + "."
            + fractional.ToString(invariant).PadLeft(decimalPlaces, '0')

    let private normalizeScientificExponent number =
        let significand, exponent = splitExponent number
        if String.IsNullOrEmpty exponent then significand
        else
            match Int32.TryParse(exponent.Substring(1), NumberStyles.Integer, invariant) with
            | true, value -> significand + "e" + value.ToString(invariant)
            | false, _ -> number

    let private scientificDecimal (number: float) decimalPlaces =
        let decimalPlaces = max decimalPlaces 0
        number.ToString("E" + decimalPlaces.ToString(invariant), invariant)
        |> normalizeScientificExponent

    let private decimal number decimalPlaces fixedDecimals =
        // Keep behavior identical to JavaScript's `toExponential` range.
        // Additional digits cannot carry information from a binary64 value.
        let decimalPlaces = decimalPlaces |> max 0 |> min maximumDecimalPlaces
        let formatted =
            if fixedDecimalIsSafe number decimalPlaces then fixedDecimal number decimalPlaces
            else scientificDecimal number decimalPlaces
        if fixedDecimals then formatted else stripTrailingDecimalZeros formatted

    let rawNumber (number: float) (options: NumberFormatOptions) =
        match options.RightDecimals with
        | System ->
            number.ToString("G", invariant)
            |> stripTrailingDecimalZeros
            |> normalizeScientificExponent
        | AtMost decimalPlaces -> decimal number decimalPlaces false
        | Fixed decimalPlaces -> decimal number decimalPlaces true

    let private leftWidth number =
        let significand, _ = splitExponent number
        let dot = significand.IndexOf('.')
        if dot < 0 then significand.Length else dot

    let prepare options numbers =
        let leftPadding =
            match options.LeftDecimals with
            | Succinct -> None
            | LeftPadding(width, style) -> Some(max width 0, style)
            | AutoLeftPadding style ->
                numbers
                |> List.map (fun number -> rawNumber number options |> leftWidth)
                |> List.fold max 0
                |> fun width -> Some(width, style)
        { Options = options; LeftPadding = leftPadding }

    let prepareRaw options numbers =
        let leftPadding =
            match options.LeftDecimals with
            | Succinct -> None
            | LeftPadding(width, style) -> Some(max width 0, style)
            | AutoLeftPadding style ->
                numbers |> List.map leftWidth |> List.fold max 0 |> fun width -> Some(width, style)
        { Options = options; LeftPadding = leftPadding }

    let private zeroPadWhole (whole: string) width =
        if whole.StartsWith("-", StringComparison.Ordinal) then
            "-" + whole.Substring(1).PadLeft(max (width - 1) 0, '0')
        else whole.PadLeft(width, '0')

    let private padLeftSide number width style =
        let significand, exponent = splitExponent number
        let dot = significand.IndexOf('.')
        let whole, suffix =
            if dot < 0 then significand, ""
            else significand.Substring(0, dot), significand.Substring(dot)
        let padded =
            match style with
            | Space -> whole.PadLeft(width, ' ')
            | Zero -> zeroPadWhole whole width
        padded + suffix + exponent

    let private leftPad number format =
        match format.LeftPadding with
        | None -> number
        | Some(width, style) -> padLeftSide number width style

    let number value format = rawNumber value format.Options |> fun raw -> leftPad raw format

    let codeNumber value format =
        let raw = rawNumber value format.Options
        let significand, exponent = splitExponent raw
        let significand = if significand.Contains('.') then significand else significand + ".0"
        leftPad (significand + exponent) format
