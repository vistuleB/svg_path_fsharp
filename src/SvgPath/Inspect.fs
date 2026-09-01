namespace SvgPath

type InspectOptions =
    { LeftDecimals: LeftDecimalOptions
      RightDecimals: RightDecimalOptions }

/// Human-readable structural inspection for path values.
[<RequireQualifiedAccess>]
module Inspect =
    let defaultOptions () =
        { LeftDecimals = Succinct
          RightDecimals = System }

    let decimalOptions decimalPlaces =
        { LeftDecimals = Succinct
          RightDecimals = AtMost decimalPlaces }

    let fixedDecimalOptions decimalPlaces =
        { LeftDecimals = Succinct
          RightDecimals = Fixed decimalPlaces }

    let withLeftDecimals leftDecimals (options: InspectOptions) = { options with LeftDecimals = leftDecimals }
    let withRightDecimals rightDecimals (options: InspectOptions) = { options with RightDecimals = rightDecimals }
    let withLeftPadding leftPadding options = withLeftDecimals leftPadding options

    let private numberFormat options numbers =
        NumberFormat.prepare
            { LeftDecimals = options.LeftDecimals
              RightDecimals = options.RightDecimals }
            numbers

    let private number value format = NumberFormat.number value format
    let private codeNumber value format = NumberFormat.codeNumber value format
    let private boolean value = if value then "true" else "false"

    let private pointNumbers (point: Point<length>) = [ float point.X; float point.Y ]

    let private segmentNumbers segment =
        match segment with
        | Line(startPoint, endPoint) -> pointNumbers startPoint @ pointNumbers endPoint
        | QuadraticBezier(startPoint, control, endPoint) ->
            pointNumbers startPoint @ pointNumbers control @ pointNumbers endPoint
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            pointNumbers startPoint @ pointNumbers control1 @ pointNumbers control2 @ pointNumbers endPoint
        | Arc endpoint ->
            pointNumbers endpoint.Start
            @ pointNumbers endpoint.Radius
            @ [ float endpoint.XAxisRotation ]
            @ pointNumbers endpoint.End

    let private subpathNumbers (subpath: Subpath) =
        pointNumbers subpath.Start @ (subpath.Segments |> List.collect segmentNumbers)

    let private pathNumbers (path: Path) = path.Subpaths |> List.collect subpathNumbers

    let private indentLines (lines: string) =
        lines.Split('\n') |> Array.map (fun line -> "  " + line) |> String.concat "\n"

    let private doPoint (point: Point<length>) format =
        number (float point.X) format + "," + number (float point.Y) format

    let private doPointCode (point: Point<length>) format =
        "Point.create ("
        + codeNumber (float point.X) format
        + "<length>) ("
        + codeNumber (float point.Y) format
        + "<length>)"

    let private doSegment segment format =
        match segment with
        | Line(startPoint, endPoint) ->
            "Line(start=" + doPoint startPoint format + " end=" + doPoint endPoint format + ")"
        | QuadraticBezier(startPoint, control, endPoint) ->
            "QuadraticBezier(start=" + doPoint startPoint format
            + " control=" + doPoint control format
            + " end=" + doPoint endPoint format + ")"
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            "CubicBezier(start=" + doPoint startPoint format
            + " control1=" + doPoint control1 format
            + " control2=" + doPoint control2 format
            + " end=" + doPoint endPoint format + ")"
        | Arc endpoint ->
            "Arc(start=" + doPoint endpoint.Start format
            + " radius=" + doPoint endpoint.Radius format
            + " x_axis_rotation=" + number (float endpoint.XAxisRotation) format
            + " large_arc=" + boolean endpoint.LargeArc
            + " sweep=" + boolean endpoint.Sweep
            + " end=" + doPoint endpoint.End format + ")"

    let private doSegmentCode segment format =
        match segment with
        | Line(startPoint, endPoint) ->
            "Line(" + doPointCode startPoint format + ", " + doPointCode endPoint format + ")"
        | QuadraticBezier(startPoint, control, endPoint) ->
            "QuadraticBezier(" + doPointCode startPoint format
            + ", " + doPointCode control format
            + ", " + doPointCode endPoint format + ")"
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            "CubicBezier(" + doPointCode startPoint format
            + ", " + doPointCode control1 format
            + ", " + doPointCode control2 format
            + ", " + doPointCode endPoint format + ")"
        | Arc endpoint ->
            "Arc { Start = " + doPointCode endpoint.Start format
            + "; Radius = " + doPointCode endpoint.Radius format
            + "; XAxisRotation = (" + codeNumber (float endpoint.XAxisRotation) format + "<degree>)"
            + "; LargeArc = " + boolean endpoint.LargeArc
            + "; Sweep = " + boolean endpoint.Sweep
            + "; End = " + doPointCode endpoint.End format + " }"

    let private doSubpath (subpath: Subpath) format =
        let state = if subpath.Closed then "closed" else "open"
        let start = "start=" + doPoint subpath.Start format
        match subpath.Segments with
        | [] -> "Subpath(" + state + ", " + start + ", [])"
        | segments ->
            "Subpath(" + state + ", " + start + ", [\n"
            + (segments |> List.map (fun segment -> doSegment segment format) |> String.concat ",\n" |> indentLines)
            + "\n])"

    let private doSubpathCode (subpath: Subpath) format =
        let constructor =
            match subpath.Segments with
            | [] -> "Subpath.empty (" + doPointCode subpath.Start format + ")"
            | segments ->
                "Subpath.create [\n"
                + (segments |> List.map (fun segment -> doSegmentCode segment format) |> String.concat ";\n" |> indentLines)
                + "\n]\n|> Result.defaultWith (failwithf \"%A\")"
        if subpath.Closed then
            constructor + "\n|> Subpath.setClosed true\n|> Result.defaultWith (failwithf \"%A\")"
        else constructor

    let private doPath (path: Path) format =
        match path.Subpaths with
        | [] -> "Path([])"
        | subpaths ->
            "Path([\n"
            + (subpaths |> List.map (fun subpath -> doSubpath subpath format) |> String.concat ",\n" |> indentLines)
            + "\n])"

    let private doPathCode (path: Path) format =
        match path.Subpaths with
        | [] -> "Path.empty"
        | subpaths ->
            "Path.ofSubpaths [\n"
            + (subpaths |> List.map (fun subpath -> doSubpathCode subpath format) |> String.concat ";\n" |> indentLines)
            + "\n]"

    let pointWith point options = numberFormat options (pointNumbers point) |> doPoint point
    let point pointValue = pointWith pointValue (defaultOptions ())
    let pointCodeWith point options = numberFormat options (pointNumbers point) |> doPointCode point
    let pointCode pointValue = pointCodeWith pointValue (defaultOptions ())

    let segmentWith segment options = numberFormat options (segmentNumbers segment) |> doSegment segment
    let segment segmentValue = segmentWith segmentValue (defaultOptions ())
    let segmentCodeWith segment options = numberFormat options (segmentNumbers segment) |> doSegmentCode segment
    let segmentCode segmentValue = segmentCodeWith segmentValue (defaultOptions ())

    let subpathWith subpath options = numberFormat options (subpathNumbers subpath) |> doSubpath subpath
    let subpath subpathValue = subpathWith subpathValue (defaultOptions ())
    let subpathCodeWith subpath options = numberFormat options (subpathNumbers subpath) |> doSubpathCode subpath
    let subpathCode subpathValue = subpathCodeWith subpathValue (defaultOptions ())

    let pathWith path options = numberFormat options (pathNumbers path) |> doPath path
    let path pathValue = pathWith pathValue (defaultOptions ())
    let pathCodeWith path options = numberFormat options (pathNumbers path) |> doPathCode path
    let pathCode pathValue = pathCodeWith pathValue (defaultOptions ())
