namespace ReadmeFigures

open SvgPath

module Drawing =
    let document width height body =
        $"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\" viewBox=\"0 0 {width} {height}\">\n  <rect x=\"0\" y=\"0\" width=\"{width}\" height=\"{height}\" fill=\"white\"/>\n{body}\n</svg>\n"
    let pathElement path style = $"  <path d=\"{Serialize.path path}\" style=\"{style}\"/>"
    let group transform body = $"  <g transform=\"{transform}\">\n{body}\n  </g>"
    let label x y text = $"  <text x=\"{x}\" y=\"{y}\" text-anchor=\"middle\" font-family=\"system-ui,sans-serif\" font-size=\"18\" font-weight=\"600\" fill=\"#172033\">{text}</text>"
    let require label = function Ok value -> value | Error error -> failwith $"{label}: {error}"
    let parse data = Parse.path data |> require "parse"
    let subpath data = match (parse data).Subpaths with [ value ] -> value | _ -> failwith "expected one subpath"
    let panelPath x y scale path style = pathElement path style |> group $"translate({x} {y}) scale({scale})"
