namespace ReadmeFigures

open SvgPath
open Drawing

[<RequireQualifiedAccess>]
module Fixtures =
    let private sourceStyle = "fill:none;stroke:#94a3b8;stroke-width:.055;stroke-linejoin:round"
    let private resultStyle = "fill:none;stroke:#2563eb;stroke-width:.04;stroke-linecap:round;stroke-linejoin:round"
    let private offset options distance source = Offset.pathWith source distance options |> require "offset"
    let private band options inner outer source = Offset.subpathBandWith source inner outer options |> require "band"

    let zeroLengthClosepath () =
        let samples = [ "M90 50","round"; "M260 50L260 50","round"; "M90 120","square"; "M260 120L260 120","square"; "M90 230","round"; "M260 230Z","round"; "M90 300","square"; "M260 300Z","square" ]
        let strokes = samples |> List.map (fun (d,cap) -> $"  <path d=\"{d}\" fill=\"none\" stroke=\"#2563eb\" stroke-width=\"24\" stroke-linecap=\"{cap}\"/>") |> String.concat "\n"
        let guides = [50;120;230;300] |> List.map (fun y -> $"  <path d=\"M20 {y}H390\" stroke=\"#ddd\"/>\n  <circle cx=\"90\" cy=\"{y}\" r=\"2\" fill=\"#dc2626\"/><circle cx=\"260\" cy=\"{y}\" r=\"2\" fill=\"#dc2626\"/>") |> String.concat "\n"
        document 500 360 (guides + "\n" + strokes)

    let singleOffsetFinalTrimming () =
        let source = parse "M0 -1L-.8660254 -.5L.8660254 .5L.8660254 -.5L-.8660254 .5L0 1"
        [ NoTrimming,"No final trimming"; CuspTrimming,"Cusp trimming"; InBandTrimming,"In-band trimming" ]
        |> List.mapi (fun i (finish,title) ->
            let options = { Offset.defaultOptions with Join=Round; SingleOffsetTrimming={Offside=false;FinalTrimming=finish} }
            let answer = offset options 0.2<length> source
            let x = 175 + i*350
            String.concat "\n" [label x 30 title; panelPath x 170 90.0 source sourceStyle; panelPath x 170 90.0 answer resultStyle])
        |> String.concat "\n" |> document 1050 320

    let singleOffsetOffsideTrimming () =
        let source = parse "M-2 -1.5H2V1.5H-2ZM-1 -.5V.5H1V-.5Z"
        [false,"offside: False";true,"offside: True"]
        |> List.mapi (fun i (offside,title) ->
            let options = {Offset.defaultOptions with Join=Round;SingleOffsetTrimming={Offside=offside;FinalTrimming=NoTrimming}}
            let answer = offset options 1.2<length> source
            let x=300+i*600
            String.concat "\n" [label x 32 title;panelPath x 190 75.0 source sourceStyle;panelPath x 190 75.0 answer resultStyle])
        |> String.concat "\n" |> document 1200 380

    let private concaveSquare = subpath "M1 0H3A1 1 0 0 0 4 1V3A1 1 0 0 0 3 4H1A1 1 0 0 0 0 3V1A1 1 0 0 0 1 0Z"
    let bandCuspTrimming () =
        [true,true,"inner_cusps: True · outer_cusps: True";false,true,"inner_cusps: False · outer_cusps: True";false,false,"inner_cusps: False · outer_cusps: False"]
        |> List.mapi(fun i (inner,outer,title) ->
            let options={Offset.defaultOptions with Join=Round;BandTrimming={InnerCusps=inner;OuterCusps=outer;InBand=true}}
            let answer=band options 1.7<length> 1.8<length> concaveSquare
            let x=70+i*420
            String.concat "\n" [label (x+140) 30 title;panelPath x 95 65.0 answer "fill:#fdba74;fill-opacity:.55;stroke:#c2410c;stroke-width:.025";panelPath x 95 65.0 (Path.ofSubpaths[concaveSquare]) sourceStyle])
        |> String.concat "\n" |> document 1260 390

    let private figureEight = subpath "M0 0C-336 -234 -336 234 0 0C336 -234 336 234 0 0Z"
    let bandInBandTrimming () =
        [false,"in_band: False";true,"in_band: True"]
        |> List.mapi(fun i (inBand,title) ->
            let options={Offset.defaultOptions with Join=Round;BandTrimming={InnerCusps=true;OuterCusps=true;InBand=inBand}}
            let answer=band options 18.0<length> 34.0<length> figureEight
            let x=320+i*640
            String.concat "\n" [label x 32 title;panelPath x 220 0.78 answer "fill:#bbf7d0;stroke:#14532d;stroke-width:2.2";panelPath x 220 0.78 (Path.ofSubpaths[figureEight]) "fill:none;stroke:#be123c;stroke-width:2;stroke-dasharray:7 6"])
        |> String.concat "\n" |> document 1280 440

    let private arrangementFigure source =
        let built=Arrangement.build [source] 1e-6<length> 1e-5<length> |> require "arrangement"
        let box: BoundingBox={ Min=Point.create -10.0<length> -10.0<length>; Max=Point.create 410.0<length> 310.0<length> }
        Svg.document (Rectangle(Point.create -10.0<length> -10.0<length>,420.0<length>,320.0<length>,"fill:white")::ArrangementDrawing.drawing built.Graph) box
    let arrangementOverlappingSquares () = arrangementFigure(parse "M30 30H230V230H30ZM150 80H350V280H150Z")
    let arrangementSemanticCircleOverlap () = arrangementFigure(parse "M210 60A100 100 0 1 1 209.999 60ZM240 60A100 100 0 1 1 239.999 60Z")

    let private csgGrid rule =
        let a=parse "M30 30H210V210H30Z"
        let b=parse "M130 90H310V270H130Z"
        ["Union",Csg.union a b rule;"Intersection",Csg.intersection a b rule;"Difference",Csg.difference a b rule;"Symmetric difference",Csg.symmetricDifference a b rule]
        |> List.mapi(fun i (title,value) ->
            let x=35+(i%2)*390
            let y=60+(i/2)*290
            let result = require title value
            String.concat "\n" [label (x+145) (y-20) title;panelPath x y 0.85 result.Path "fill:#7dd3fc;stroke:#0369a1;stroke-width:2"])
        |> String.concat "\n" |> document 780 600
    let arrangementCsgNonzero ()=csgGrid Nonzero
    let arrangementCsgEvenOdd ()=csgGrid EvenOdd

    let all=["zero_length_closepath_probe.svg",zeroLengthClosepath;"single_offset_final_trimming.svg",singleOffsetFinalTrimming;"single_offset_offside_trimming.svg",singleOffsetOffsideTrimming;"band_cusp_trimming.svg",bandCuspTrimming;"band_in_band_trimming.svg",bandInBandTrimming;"arrangement_graph_overlapping_squares.svg",arrangementOverlappingSquares;"arrangement_graph_semantic_circle_overlap.svg",arrangementSemanticCircleOverlap;"arrangement_csg_nonzero.svg",arrangementCsgNonzero;"arrangement_csg_evenodd.svg",arrangementCsgEvenOdd]
