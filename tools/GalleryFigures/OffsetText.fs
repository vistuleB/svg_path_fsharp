namespace GalleryFigures

open System
open System.IO
open System.Xml.Linq
open SvgPath
open GalleryFigures.Drawing

/// Faithful counterparts of Gleam's two offset-map text fixture generators.
module OffsetText =
    let private source () =
        let filename = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"Inputs/the_quick_brown_khmer.svg")
        let document = XDocument.Load filename
        let path = document.Descendants(XName.Get("path","http://www.w3.org/2000/svg")) |> Seq.head
        parse (path.Attribute(XName.Get "d").Value)

    let private curve decaying =
        let turns = if decaying then 5 else 6
        let position (angle: float<degree>) =
            let degrees = float angle
            let radius = if decaying then 100.0 * Math.Pow(0.8,degrees/360.0) else 100.0
            point (radius * Trig.cosDegrees angle + (if decaying then 0.0 else 16.0*degrees*Math.PI/180.0))
                  (radius * Trig.sinDegrees angle)
        let tangent (angle: float<degree>) =
            let radius = if decaying then 100.0*Math.Pow(0.8,float angle/360.0) else 100.0
            let radiusDerivative = if decaying then log 0.8 / 360.0 * radius else 0.0
            let da = Math.PI/180.0
            Point.create
                (LanguagePrimitives.FloatWithMeasure<length/degree>
                    (radiusDerivative*Trig.cosDegrees angle-radius*Trig.sinDegrees angle*da+(if decaying then 0.0 else 16.0*da)))
                (LanguagePrimitives.FloatWithMeasure<length/degree>
                    (radiusDerivative*Trig.sinDegrees angle+radius*Trig.cosDegrees angle*da))
        Subpath.parametricWith 0.0<degree> (float turns*360.0<degree>) position
            {Tolerance=0.001<length>;SamplesPerPiece=3;InitialPieceCount=turns*36;MaxDepth=0;Tangent=Some tangent}
        |> require "offset text source curve"

    let generate decaying () =
        let text = source ()
        let textBox = Path.boundingBox text |> require "text bounds"
        let baseline = curve decaying
        let total = Subpath.length baseline |> require "baseline length"
        let map = Offset.subpathOffsetMap baseline |> require "offset map"
        let rate = -(5.0*log 0.8 / float total)
        let maximum = float total - 1e-6
        let capacity = if decaying then (exp(rate*maximum)-1.0)/rate else float total
        let mapping (p: Point<length>) =
            if not decaying then map p
            else
                let slowed = log(1.0+rate*float p.X)/rate
                let distance = max 0.0 (min maximum slowed)
                let decay = Math.Pow(0.8,distance/float total*5.0)
                map (point distance (float p.Y*decay))
        let text = if decaying then Path.subdivideToMaxLength text 1.0<length> |> require "subdivide offset text" else text
        let width = float (BoundingBox.width textBox)
        let height = float (BoundingBox.height textBox)
        let xScale = height/15.0
        let textLength = width*xScale
        let pitch = textLength+15.0
        let rec fullCopyCount remaining count = if remaining>=textLength then fullCopyCount (remaining-pitch) (count+1) else count
        let full = fullCopyCount capacity 0
        let remainder = capacity-float full*pitch
        let copy index available =
            Path.tryMapPoints (fun p ->
                let x = min (float p.X) (float textBox.Min.X+available)
                let distance = float index*pitch+(x-float textBox.Min.X)*xScale
                let bandOffset = 5.0+(float textBox.Max.Y-float p.Y)/height*15.0
                mapping (point distance bandOffset)) text
            |> require "map text copy"
        let copies = [for i in 0..full-1 -> copy i width]
        let copies = if remainder>0.0 then
                         let available = min width (remainder/xScale)
                         if available>0.0 then copies @ [copy full available] else copies
                     else copies
        let mapped = combine copies
        let box = Path.boundingBox mapped |> require "mapped text bounds"
        let view: BoundingBox =
            {Min=point (float box.Min.X-30.0) (float box.Min.Y-30.0)
             Max=point (float box.Max.X+30.0) (float box.Max.Y+45.0)}
        let style = if decaying then "fill: #581c87; fill-opacity: 0.78; stroke: #2e1065; stroke-width: 0.2"
                    else "fill: #0f766e; fill-opacity: 0.78; stroke: #064e3b; stroke-width: 0.25"
        Svg.document [ThingToDraw.Rectangle(view.Min,BoundingBox.width view,BoundingBox.height view,"fill: #ffffff; stroke: none")
                      StyledPath(mapped,style)] view
