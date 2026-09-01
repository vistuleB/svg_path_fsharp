namespace SvgPath

open System

/// One item rendered inside a generated SVG document.
type ThingToDraw =
    | StyledPath of Path * style: string
    | Rectangle of topLeft: Point<length> * width: float<length> * height: float<length> * style: string
    | RotatedRectangle of topLeft: Point<length> * width: float<length> * height: float<length> * style: string * rotation: float<degree> * origin: Point<length>
    | Circle of center: Point<length> * radius: float<length> * style: string
    | Ellipse of center: Point<length> * radius: Point<length> * style: string
    | Text of label: string * style: string * point: Point<length> * fontSize: float<length>
    | RotatedText of label: string * style: string * point: Point<length> * fontSize: float<length> * rotation: float<degree> * origin: Point<length>

type ThingsToDraw = ThingToDraw list

[<RequireQualifiedAccess>]
module Svg =
    let private numberFormat numbers =
        NumberFormat.prepare
            { LeftDecimals = Succinct
              RightDecimals = AtMost 5 }
            numbers

    let private number (value: float<'unit>) format = NumberFormat.number (float value) format

    let private replace (pattern: string) (replacement: string) (value: string) = value.Replace(pattern, replacement)

    let private attributeEscape value =
        value
        |> replace "&" "&amp;"
        |> replace "\"" "&quot;"
        |> replace "<" "&lt;"
        |> replace ">" "&gt;"

    let private textEscape value =
        value
        |> replace "&" "&amp;"
        |> replace "<" "&lt;"
        |> replace ">" "&gt;"

    let private pathElement path style =
        "  <path d=\"" + attributeEscape (Serialize.path path) + "\" style=\"" + attributeEscape style + "\" />"

    let private rectangleElement topLeft width height style format =
        "  <rect x=\"" + number topLeft.X format
        + "\" y=\"" + number topLeft.Y format
        + "\" width=\"" + number width format
        + "\" height=\"" + number height format
        + "\" style=\"" + attributeEscape style + "\" />"

    let private circleElement center radius style format =
        "  <circle cx=\"" + number center.X format
        + "\" cy=\"" + number center.Y format
        + "\" r=\"" + number radius format
        + "\" style=\"" + attributeEscape style + "\" />"

    let private ellipseElement center radius style format =
        "  <ellipse cx=\"" + number center.X format
        + "\" cy=\"" + number center.Y format
        + "\" rx=\"" + number radius.X format
        + "\" ry=\"" + number radius.Y format
        + "\" style=\"" + attributeEscape style + "\" />"

    let private textElement label style point fontSize format =
        "  <text x=\"" + number point.X format
        + "\" y=\"" + number point.Y format
        + "\" font-size=\"" + number fontSize format
        + "\" style=\"" + attributeEscape style + "\">"
        + textEscape label + "</text>"

    let private addRotation (element: string) rotation origin format =
        let transform =
            " transform=\"rotate(" + number rotation format
            + " " + number origin.X format
            + " " + number origin.Y format + ")\""
        let separator = element.IndexOf('>')
        let before, after = element.Substring(0, separator), element.Substring(separator + 1)
        if before.EndsWith(" /", StringComparison.Ordinal) then
            before.Substring(0, before.Length - 2) + transform + " />" + after
        else
            before + transform + ">" + after

    let private thingElement thing format =
        match thing with
        | StyledPath(path, style) -> pathElement path style
        | Rectangle(topLeft, width, height, style) -> rectangleElement topLeft width height style format
        | RotatedRectangle(topLeft, width, height, style, rotation, origin) ->
            rectangleElement topLeft width height style format |> fun element -> addRotation element rotation origin format
        | Circle(center, radius, style) -> circleElement center radius style format
        | Ellipse(center, radius, style) -> ellipseElement center radius style format
        | Text(label, style, point, fontSize) -> textElement label style point fontSize format
        | RotatedText(label, style, point, fontSize, rotation, origin) ->
            textElement label style point fontSize format |> fun element -> addRotation element rotation origin format

    /// Draw a labeled square-and-cross marker centered on a point.
    let labeledPoint label color point (fontSize: float<length>) : ThingsToDraw =
        let side = fontSize
        let halfSide = side / 2.0
        let left, right = point.X - halfSide, point.X + halfSide
        let top, bottom = point.Y - halfSide, point.Y + halfSide
        let topLeft, topRight = Point.create left top, Point.create right top
        let bottomRight, bottomLeft = Point.create right bottom, Point.create left bottom
        let closedMarker =
            [ Line(topLeft, topRight)
              Line(topRight, bottomRight)
              Line(bottomRight, bottomLeft)
              Line(bottomLeft, topLeft) ]
            |> Subpath.create
            |> Result.bind (Subpath.setClosed true)
            |> Result.defaultWith (failwithf "%A")
        let diagonal startPoint endPoint =
            Subpath.create [ Line(startPoint, endPoint) ] |> Result.defaultWith (failwithf "%A")
        let marker =
            Path.ofSubpaths
                [ closedMarker
                  diagonal topLeft bottomRight
                  diagonal bottomLeft topRight ]
        [ StyledPath(marker, "fill: none; stroke: " + color + "; stroke-width: 1; stroke-linecap: square; stroke-linejoin: miter")
          Text(label, "fill: " + color + "; font-family: system-ui, sans-serif", Point.create (right + halfSide) (point.Y + halfSide), side) ]

    /// Render drawing items as a complete SVG document.
    let document (things: ThingsToDraw) (viewBox: BoundingBox) =
        let width, height = BoundingBox.width viewBox, BoundingBox.height viewBox
        let format = numberFormat [ float viewBox.Min.X; float viewBox.Min.Y; float width; float height ]
        let viewBoxValue =
            [ number viewBox.Min.X format
              number viewBox.Min.Y format
              number width format
              number height format ]
            |> String.concat " "
        let elements = things |> List.map (fun thing -> thingElement thing format) |> String.concat "\n"
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"" + viewBoxValue
        + "\" width=\"" + number width format
        + "\" height=\"" + number height format
        + "\">\n" + elements + "\n</svg>"
