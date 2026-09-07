namespace GalleryFigures

open System
open System.Globalization
open SvgPath

/// Rendering only: all geometric construction is performed by the library.
module Drawing =
    let require label = function Ok value -> value | Error error -> failwithf "%s: %A" label error
    let number (x: float) = x.ToString("G17", CultureInfo.InvariantCulture)
    let point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
    let parse data = Parse.path data |> require "parse"
    let subpath data =
        match (parse data).Subpaths with
        | [ source ] -> source
        | _ -> failwith "expected one source subpath"
    let path subpath = Path.ofSubpaths [subpath]
    let combine paths = paths |> List.collect Path.subpaths |> Path.ofSubpaths
    let layer geometry fill stroke width =
        geometry, sprintf "fill:%s;stroke:%s;stroke-width:%s;stroke-linejoin:round" fill stroke (number width)
    let element (geometry, style) = sprintf "<path d=\"%s\" style=\"%s\"/>" (Serialize.path geometry) style
    let bounds layers =
        layers |> List.map fst |> combine |> Path.boundingBox |> require "figure bounds"
    /// Each panel is centered using the bounds of its actual rendered paths.
    let panel x width height title layers =
        let box = bounds layers
        let w = max 1e-6 (float (box.Max.X - box.Min.X))
        let h = max 1e-6 (float (box.Max.Y - box.Min.Y))
        let pad = 0.12 * max w h
        let vx, vy = float box.Min.X - pad, float box.Min.Y - pad
        let vw, vh = w + 2.0*pad, h + 2.0*pad
        sprintf "<text x=\"%d\" y=\"24\" text-anchor=\"middle\" font-family=\"sans-serif\" font-size=\"16\">%s</text>\n<svg x=\"%d\" y=\"35\" width=\"%d\" height=\"%d\" viewBox=\"%s %s %s %s\">\n%s\n</svg>"
            (x+width/2) (System.Security.SecurityElement.Escape title) x width (height-40)
            (number vx) (number vy) (number vw) (number vh)
            (layers |> List.map element |> String.concat "\n")
    let panels width height examples =
        let total = width * List.length examples
        let body = examples |> List.mapi (fun i (title,layers) -> panel (i*width) width height title layers) |> String.concat "\n"
        sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"white\"/>\n%s\n</svg>\n" total height total height total height body
