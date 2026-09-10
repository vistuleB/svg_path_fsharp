namespace GalleryFigures
open System.IO
open System.Text.RegularExpressions
open SvgPath
module W3cJoins =
    let private ok r = Result.defaultWith (failwithf "%A") r
    let private source d = (Parse.path d |> ok).Subpaths.Head
    let private draw path transform = $"<path transform=\"{transform}\" d=\"{Serialize.path path}\" fill=\"none\" stroke=\"#1565ff\" stroke-width=\"1.4\"/>"
    let private reference raw prefix w h x y width height opacity =
        let body=Regex.Replace(raw,@"^[\s\S]*?<svg\b[^>]*>","") |> fun s -> Regex.Replace(s,@"</svg>\s*$","")
        let mutable body=body
        for m in Regex.Matches(body,"id=\"([^\"]+)\"") do
            let id=m.Groups[1].Value
            body<-body.Replace($"id=\"{id}\"",$"id=\"{prefix}{id}\"").Replace($"#{id}\"",$"#{prefix}{id}\"").Replace($"#{id})",$"#{prefix}{id})")
        body<-body.Replace("text {",$".{prefix} text {{").Replace("circle {",$".{prefix} circle {{")
        $"<svg x=\"{x}\" y=\"{y}\" width=\"{width}\" height=\"{height}\" viewBox=\"0 0 {w} {h}\" opacity=\"{opacity}\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><g class=\"{prefix}\">{body}</g></svg>"
    let private read name=File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"w3c-join-reference",name))
    let private doc w h body= $"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{w}\" height=\"{h}\" viewBox=\"0 0 {w} {h}\"><rect x=\"0\" y=\"0\" width=\"{w}\" height=\"{h}\" fill=\"white\"/>{body}</svg>"
    let private fallback file title () =
        let raw=read file
        let d=Regex.Match(raw,"<path style=\"fill:none;stroke:#444;stroke-width:40px\"\\s+d=\"([^\"]+)\"").Groups[1].Value
        let sub=source (Regex.Replace(d,@"m\s+0,0",""))
        let result=Stroke.subpath sub 40.0<length> (Offset.Arcs(if file.Contains("fallback3") then 5.0 else 100.0)) Offset.Butt |> ok
        let panels=reference raw "left" 500 250 10 70 500 250 1.0+reference raw "right" 500 250 530 70 500 250 0.45
        let overlay = draw result ""
        doc 1040 360 ($"<g font-family=\"sans-serif\" fill=\"#333\"><text x=\"20\" y=\"25\" font-size=\"18\">{title}</text><text x=\"20\" y=\"55\">W3C illustration</text><text x=\"540\" y=\"55\">Same illustration + F# computed outline (blue)</text>{panels}<svg x=\"530\" y=\"70\" width=\"500\" height=\"250\" viewBox=\"0 0 500 250\">{overlay}</svg><text x=\"20\" y=\"345\" font-size=\"12\">Original W3C viewport retained; width 40, Butt caps. Reference guides are rounded approximations.</text></g>")
    let private miter () =
        let raw=read "miter-limit.svg"
        let sub=source "M25,60 L175,90 L25,120"
        let m=Stroke.subpath sub 35.0<length> (Offset.Miter 3.0) Offset.Butt |> ok
        let c=Stroke.subpath sub 35.0<length> (Offset.MiterClip 3.0) Offset.Butt |> ok
        if abs((Path.boundingBox c |> ok).Max.X-227.5<length>)>=1e-9<length> then failwith "Miter clip plane mismatch"
        doc 1000 690 ("<text x=\"20\" y=\"25\" font-family=\"sans-serif\" font-size=\"18\">Miter limit 3 — W3C reference (top), F# blue overlay (bottom)</text>"+reference raw "top" 600 180 20 40 960 288 1.0+reference raw "bottom" 600 180 20 360 960 288 0.45+"<svg x=\"20\" y=\"360\" width=\"960\" height=\"288\" viewBox=\"0 0 600 180\">"+draw m ""+draw c "translate(300 0)"+"</svg>")
    let all =
        ["w3c-miter-limit.svg",miter
         "w3c-linejoin-construction-fallback.svg",fallback "linejoin-construction-fallback.svg" "Arcs: nested continuation circles"
         "w3c-linejoin-construction-fallback2.svg",fallback "linejoin-construction-fallback2.svg" "Arcs: disjoint continuation circles"
         "w3c-linejoin-construction-fallback3.svg",fallback "linejoin-construction-fallback3.svg" "Arcs: parallel tangents (intentional Round fallback)"]
