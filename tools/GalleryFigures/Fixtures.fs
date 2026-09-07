namespace GalleryFigures

open System.IO
open System.Xml.Linq
open SvgPath
open Drawing

/// Geometry fixtures ported from Gleam's svg_path_gallery_test and its helpers.
/// Layout is recomputed from F# result bounds; no algorithm is replicated here.
module Fixtures =
    let root = System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../.."))
    let input name = System.IO.Path.Combine(root, "tools/GalleryFigures/Inputs", name)
    let sourceFile name =
        let doc = XDocument.Load(input name)
        let node = doc.Descendants(XName.Get("path", "http://www.w3.org/2000/svg")) |> Seq.head
        parse (node.Attribute(XName.Get "d").Value)
    let only (p: Path) = match p.Subpaths with [s] -> s | _ -> failwith "expected one subpath"
    let sourceStyle s = layer (path s) "none" "#be123c" 1.5
    let green p = layer p "#bbf7d0" "#14532d" 1.5
    let band s inner outer = Offset.subpathBand s (inner*1.0<length>) (outer*1.0<length>) Round Butt |> require "band"
    let figureEight = subpath "M0 0C-336 -234 -336 234 0 0C336 -234 336 234 0 0Z"
    let figureBand = lazy (band figureEight 18.0 34.0)
    let rect x y xx yy = parse (sprintf "M%g %gH%gV%gH%gZ" x y xx yy x)
    let roundedRectangles () =
        let rectangles = [rect 0.0 22.0 96.0 118.0; rect 42.0 0.0 150.0 64.0; rect 118.0 38.0 210.0 118.0; rect 24.0 88.0 146.0 146.0; rect 152.0 86.0 226.0 152.0]
        let union = rectangles |> List.fold (fun p next -> (Csg.union p next Nonzero |> require "union").Path) Path.empty
        let rounded = Effects.roundCornersWith union 8.0<length> {Effects.defaultRoundCornerOptions with Failure=AdaptRadius} |> require "round corners"
        panels 330 300 ["Rectangles",[layer (combine rectangles) "#d9f99d" "#365314" 2.0]; "Union",[layer union "#bfdbfe" "#1f2937" 3.0]; "Rounded union",[green rounded]]
    let strokeCaps () =
        let s = subpath "M0 20C40 -58 100 78 150 0"
        ["Butt",Butt;"Square",Square;"Round",RoundCap]
        |> List.map(fun (name,cap) -> name,[layer (Stroke.subpath s 28.0<length> Round cap |> require "stroke") "#fed7aa" "#7c2d12" 2.5;sourceStyle s])
        |> panels 330 280
    let dashSource = subpath "M0 28C48 -62 112 88 154 16C194 -52 218 70 188 42"
    let dashedStrokes () =
        ["Short dashes",[18.;12.],0.,"#fecaca","#7f1d1d";"Offset pattern",[26.;12.;8.;12.],18.,"#fde68a","#854d0e";"Round caps",[34.;18.],9.,"#bbf7d0","#14532d"]
        |> List.map(fun (title,pattern,phase,fill,color) ->
            let result = Stroke.subpathDashed dashSource 16.0<length> (List.map (fun x -> x*1.0<length>) pattern) (phase*1.0<length>) Round RoundCap |> require "dashed stroke"
            title,[layer result fill color 2.2;sourceStyle dashSource]) |> panels 330 300
    let recursiveDashes () =
        let s = subpath "M0 34C88 -112 180 146 270 10C344 -98 418 138 520 22"
        let s = Transform.translateSubpath s 92.0<length> 154.0<length> |> require "place recursive source"
        // Same source truncation: end at the last on-interval of [112,48], phase 10.
        let length = Subpath.length s |> require "dash source length" |> float
        let rec lastOn pos remaining on last =
            if pos >= length then last
            else
                let finish = min length (pos+remaining)
                lastOn finish (if on then 48. else 112.) (not on) (if on then finish else last)
        let ending = lastOn 0. 102. true 0.
        let s = Subpath.betweenLengths s 0.0<length> (ending*1.0<length>) |> require "truncate dash source"
        let first = Stroke.subpathDashed s 58.0<length> [112.0<length>;48.0<length>] 10.0<length> Round RoundCap |> require "first dash stroke"
        let second = first.Subpaths |> List.collect(fun outline ->
            Stroke.subpathDashes outline [17.0<length>;9.0<length>] 3.0<length> |> require "second dashes"
            |> List.filter(fun dash -> (Subpath.length dash |> require "dash length") > 0.1<length>)
            |> List.map(fun dash -> Stroke.subpath dash 6.0<length> Round RoundCap |> require "second dash stroke")) |> combine
        panels 1000 450 ["Recursive dashes",[(first,"fill:#fed7aa;stroke:#9a3412;stroke-width:1.4;opacity:.42");layer second "#fee2e2" "#7f1d1d" 1.7;sourceStyle s]]
    let figureEightBand () = panels 900 420 ["Offsets +18 / +34",[green figureBand.Value;sourceStyle figureEight]]
    let symmetricBands () =
        let s=sourceFile "loop_8_symmetric_arcs.svg" |> only
        [10.,20.; -5.,25.] |> List.map(fun (a,b) -> sprintf "Offsets %g / %g" a b,[green (band s a b);sourceStyle s]) |> panels 500 470
    let colors = [|"#ef4444";"#3b82f6";"#22c55e";"#f59e0b";"#a855f7";"#06b6d4";"#ec4899";"#84cc16"|]
    let correspondence () =
        let untrimmed = Offset.subpathBandUntrimmed figureEight 18.0<length> 34.0<length> Round |> require "untrimmed band"
        let blocks =
            match untrimmed.Subpaths with
            | [inner;outer] ->
                inner.Segments |> List.mapi(fun i a ->
                    // Matches the current Gleam display fixture's endpoint pairing heuristic.
                    let score b = Point.squaredDistance (Segment.start a) (Segment.start b) + Point.squaredDistance (Segment.finish a) (Segment.finish b)
                    let b = outer.Segments |> List.minBy score
                    let block = Subpath.createWith Bridge [a;Segment.reverse b] |> require "block" |> Subpath.setClosedWith Bridge true |> require "close block"
                    path block,sprintf "fill:%s;fill-opacity:.42;stroke:%s;stroke-opacity:.7;stroke-width:.8" colors[i%8] colors[i%8])
            | _ -> failwith "expected two untrimmed walks"
        panels 1000 450 ["Display correspondence blocks",(layer figureBand.Value "#f1f5f9" "none" 0.)::blocks@[layer figureBand.Value "none" "#0f172a" 1.5;sourceStyle figureEight]]
    let hulls () =
        ["Source hull",path figureEight;"Band hull",figureBand.Value;"Combined hull",combine[path figureEight;figureBand.Value]]
        |> List.map(fun (title,p) -> let h=ConvexHull.pathHull p |> require "hull" in title,[(path h,"fill:#bfdbfe;fill-opacity:.35;stroke:#1d4ed8;stroke-width:3");layer p "none" "#be123c" 1.5]) |> panels 360 290
    let trackSource = subpath "M0 32C82 -108 150 142 232 12C300 -92 414 118 532 -16"
    let family s offsets (palette: string array) =
        (offsets |> List.mapi(fun i d ->
            let result=Offset.subpathUntrimmed s (d*1.0<length>) Round |> require "untrimmed offset"
            layer (path result) "none" palette[i % Array.length palette] 2.8)) @ [sourceStyle s]
    let tracks () = panels 1000 350 ["Untrimmed offset tracks",family trackSource [-42.;-28.;-14.;14.;28.;42.] [|"#7f1d1d";"#c2410c";"#b45309";"#047857";"#0369a1";"#6d28d9"|]]
    let earth () =
        ["Soft arc","M0 28C42 -24 118 -24 164 28";"Bending line","M0 42C34 6 82 -18 126 0C156 12 160 50 188 66";"Quiet turn","M0 34L68 -8C110 -34 150 34 188 10"]
        |> List.map(fun (title,data) -> title,family (subpath data) [8.;16.;24.;32.;40.] [|"#5f4339";"#8a5a3c";"#a36a2d";"#7c6a3d";"#51633f"|]) |> panels 330 300
    let packageFirst () =
        let source=sourceFile "package_title.svg"
        let options={Offset.defaultOptions with Fitting={Tolerance=0.01<length>;Samples=5;MaxDepth=12};DistanceOptions={Segment.defaultDistanceOptions with Tolerance=1e-9<length>}}
        let raw=Offset.pathUntrimmedWith source 1.05<length> (Miter Offset.defaultMiterLimit) options |> require "title untrimmed"
        let result=Offset.pathWith source 1.05<length> (Miter Offset.defaultMiterLimit) Butt options |> require "title offset"
        panels 1800 430 ["Offset 1.05",[(source,"fill:#111827;opacity:.18");layer raw "none" "#9ca3af" 0.1;layer result "none" "#2563eb" 0.16]]
    let packageNine () =
        let source=sourceFile "package_title.svg"
        let levels=[1..9] |> List.scan(fun p level ->
            printfn "  title offset %d/9" level
            Offset.path p 1.04<length> (Miter Offset.defaultMiterLimit) Butt |> require "successive title offset") source |> List.tail
        let palette=[|"#2563eb";"#dc2626";"#16a34a";"#9333ea";"#ea580c";"#0891b2";"#be185d";"#4f46e5";"#0d9488"|]
        panels 1800 440 ["Nine offsets at 1.04",(source,"fill:#111827;opacity:.14")::(levels |> List.mapi(fun i p -> layer p "none" palette[i] 0.055))]
    let crescent () =
        let radial angle = point (120.*Trig.cosDegrees(angle*1.0<degree>)) (120.*Trig.sinDegrees(angle*1.0<degree>))
        let start,finish = radial -35.,radial 35.
        let points=[0..53] |> List.map(fun i ->
            let circle=radial(-33.5+float i*67./53.)
            let chord=120.0<length>*Trig.cosDegrees 35.0<degree>
            let fraction=0.1+0.82*float (((i*61+43)*(i*31+29)+17)%10000)/10000.
            {circle with X=chord+fraction*(circle.X-chord)})
        let cloud=Path.ofSubpaths (Subpath.ofSegment(Line(start,finish))::List.map Subpath.empty points)
        let hull=ConvexHull.pathHull cloud |> require "crescent hull"
        let reference=Subpath.ofSegment(Arc {Start=start; Radius=point 120. 120.; XAxisRotation=0.0<degree>; LargeArc=false; Sweep=true; End=finish}) |> path
        let matrix=Transform.matrix 5.8 0. 0. 2. (-482.4<length>) 182.0<length>
        let display p=Transform.path p matrix |> require "crescent transform"
        let markers=points |> List.map(fun p ->
            let x,y=float p.X,float p.Y
            parse(sprintf "M%g %gA.4 .4 0 1 0 %g %gA.4 .4 0 1 0 %g %gZ" (x+0.4) y (x-0.4) y (x+0.4) y)) |> combine
        panels 400 450 ["Crescent hull",[layer (display reference) "none" "#94a3b8" 1.4;layer(display(path hull)) "#fed7aa" "#9a3412" 2.4;layer(display markers) "#166534" "none" 0.]]
    let radiator () =
        let cutter=File.ReadAllText(input "cut-radiator.path") |> parse
        let last=16.26195-0.55
        let step=(last-0.55)/55.
        let ys = [1..55] |> List.scan (fun y _ -> y+step) 0.55 |> List.toArray
        let segments=[0..55] |> List.collect(fun i ->
            let x,xx=if i%2=0 then 0.55,last else last,0.55
            let y=ys[i]
            [yield Line(point x y,point xx y)
             if i<55 then yield Line(point xx y,point xx ys[i+1])])
        let snake=Subpath.createWith Strict segments |> require "radiator"
        let cut=Cut.path (path snake) cutter |> require "cut"
        let kept=cut.Subpaths |> List.filter(fun p ->
            let length=Subpath.length p |> require "piece length"
            if length<=1e-6<length> then false
            else
                let sample=Subpath.pointAtLength p (length/2.) |> require "piece midpoint"
                (Path.containment sample cutter Nonzero |> require "containment") <> Inside) |> Path.ofSubpaths
        panels 650 650 ["Cut radiator",[layer kept "none" "#0f172a" 0.035]]
    let csgCases () =
        let nested=combine[rect 0. 0. 120. 100.;rect 30. 22. 90. 78.]
        let circle=parse "M100 50A45 45 0 0 1 10 50A45 45 0 0 1 100 50Z"
        let common=["rectangles",rect 0. 0. 80. 80.,rect 40. 0. 120. 80.,Nonzero
                    "circle-rectangle",circle,rect 45. 0. 105. 100.,Nonzero
                    "nested-nonzero",nested,rect 42. 32. 78. 68.,Nonzero
                    "nested-evenodd",nested,rect 42. 32. 78. 68.,EvenOdd
                    "bowtie-rectangle",parse "M5 5L115 95L115 5L5 95Z",rect 35. 25. 88. 82.,Nonzero]
        let render operation a b rule () =
            let result=(operation a b rule |> require "boolean").Path
            let graph=Arrangement.build [a;b] 1e-6<length> 1e-5<length> |> require "arrangement"
            let edges=graph.Graph.Edges |> List.map(fun edge -> path(Subpath.ofSegment edge.Segment) |> fun p -> layer p "none" "#334155" 0.65)
            let fill=if rule=EvenOdd then "fill-rule:evenodd" else "fill-rule:nonzero"
            let operands=[a,"fill:#bfdbfe;fill-opacity:.55;stroke:#2563eb;stroke-width:1;"+fill;b,"fill:#fed7aa;fill-opacity:.55;stroke:#c2410c;stroke-width:1;"+fill]
            // Empty results still use the operand bounds, with invisible source paths.
            panels 340 330 ["Operands",operands;"Arrangement",edges;"Result",[combine[a;b],"fill:none;stroke:none";layer result "#bbf7d0" "#14532d" 1.]]
        [ for kind,operation,extra in
            ["intersection",Csg.intersection,["edge-tangent",rect 0. 0. 60. 80.,rect 60. 15. 120. 65.,Nonzero]
             "difference",Csg.difference,["hole",rect 0. 0. 120. 100.,rect 32. 22. 88. 78.,Nonzero]] do
            for name,a,b,rule in common@extra do
                yield "gallery-"+kind+"-"+name+".svg",render operation a b rule ]
    let all () =
        ["gallery-rounded-rectangle-union.svg",roundedRectangles
         "gallery-stroke-caps.svg",strokeCaps
         "gallery-dashed-strokes.svg",dashedStrokes
         "gallery-recursive-dashes.svg",recursiveDashes
         "gallery-figure-eight-band.svg",figureEightBand
         "gallery-symmetric-figure-eight-bands.svg",symmetricBands
         "gallery-figure-eight-correspondence-blocks.svg",correspondence
         "gallery-figure-eight-convex-hulls.svg",hulls
         "gallery-stroke-offset-tracks.svg",tracks
         "gallery-earth-tone-offsets.svg",earth
         "gallery-package-title-first-offset.svg",packageFirst
         "gallery-package-title-nine-offsets.svg",packageNine
         "gallery-crescent-hull.svg",crescent
         "gallery-cut-radiator.svg",radiator] @ csgCases()
    let snapshots =
        ["gallery-package-title-second-offset-arrangement.svg"
         "gallery-lazy-dog-offset-coil.svg"
         "gallery-lazy-dog-offset-decaying-spiral.svg"]
