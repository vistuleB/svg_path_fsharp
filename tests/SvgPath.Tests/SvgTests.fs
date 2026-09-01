module SvgPath.Tests.SvgTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private length value = Length.fromFloat value
let private box minX minY maxX maxY : BoundingBox = { Min = point minX minY; Max = point maxX maxY }

[<Fact>]
let ``document renders styled paths and text`` () =
    let path = Path.ofSubpaths [ Subpath.ofSegment (Line(point 1.0 2.0, point 11.0 2.0)) ]
    let actual =
        Svg.document
            [ StyledPath(path, "fill: none; stroke: red; stroke-width: 0.25")
              Text("start", "fill: black; font-family: sans-serif", point 1.0 2.0, length 4.0) ]
            (box 0.0 -5.0 20.0 15.0)
    Assert.Equal(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 -5 20 20\" width=\"20\" height=\"20\">\n"
        + "  <path d=\"M 1 2 H 11\" style=\"fill: none; stroke: red; stroke-width: 0.25\" />\n"
        + "  <text x=\"1\" y=\"2\" font-size=\"4\" style=\"fill: black; font-family: sans-serif\">start</text>\n"
        + "</svg>", actual)

[<Fact>]
let ``document renders basic drawing elements`` () =
    let actual =
        Svg.document
            [ Rectangle(point 1.0 2.0, length 10.0, length 5.0, "fill: white; stroke: black")
              Circle(point 8.0 9.0, length 3.0, "fill: red; stroke: none")
              Ellipse(point 12.0 13.0, point 4.0 2.0, "fill: blue; stroke: none") ]
            (box 0.0 0.0 20.0 20.0)
    Assert.Contains("<rect x=\"1\" y=\"2\" width=\"10\" height=\"5\"", actual)
    Assert.Contains("<circle cx=\"8\" cy=\"9\" r=\"3\"", actual)
    Assert.Contains("<ellipse cx=\"12\" cy=\"13\" rx=\"4\" ry=\"2\"", actual)

[<Fact>]
let ``document escapes attributes and text separately`` () =
    let actual =
        Svg.document
            [ StyledPath(Path.empty, "stroke: \"red\"; marker: url(a&b<c>d)")
              Text("\"a\" & <b>", "font-family: \"serif\"; fill: a&b<c>d", point 0.5 1.0, length 12.0) ]
            (box 0.0 0.0 1.0 1.0)
    Assert.Contains("style=\"stroke: &quot;red&quot;; marker: url(a&amp;b&lt;c&gt;d)\"", actual)
    Assert.Contains(">\"a\" &amp; &lt;b&gt;</text>", actual)

[<Fact>]
let ``rotated elements insert transforms before the closing bracket`` () =
    let actual =
        Svg.document
            [ RotatedRectangle(point 1.0 2.0, length 3.0, length 4.0, "fill: red", 45.0<degree>, point 5.0 6.0)
              RotatedText("label", "fill: black", point 7.0 8.0, length 9.0, -30.0<degree>, point 1.0 2.0) ]
            (box 0.0 0.0 20.0 20.0)
    Assert.Contains("style=\"fill: red\" transform=\"rotate(45 5 6)\" />", actual)
    Assert.Contains("style=\"fill: black\" transform=\"rotate(-30 1 2)\">label</text>", actual)

[<Fact>]
let ``labeled point draws marker and label`` () =
    let actual = Svg.document (Svg.labeledPoint "p0" "red" (point 10.0 10.0) (length 4.0)) (box 0.0 0.0 20.0 20.0)
    Assert.Equal(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 20 20\" width=\"20\" height=\"20\">\n"
        + "  <path d=\"M 8 8 H 12 V 12 H 8 Z M 8 8 L 12 12 M 8 12 L 12 8\" style=\"fill: none; stroke: red; stroke-width: 1; stroke-linecap: square; stroke-linejoin: miter\" />\n"
        + "  <text x=\"14\" y=\"12\" font-size=\"4\" style=\"fill: red; font-family: system-ui, sans-serif\">p0</text>\n"
        + "</svg>", actual)
