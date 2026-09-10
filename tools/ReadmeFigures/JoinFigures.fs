namespace ReadmeFigures
open SvgPath
open Drawing

// Source fixtures ported in order from Gleam 411c34a, 71b05d1, 9789684.
// Only public stroke calls produce the purple geometry.
module JoinFigures =
    let private p x y = Point.create (x*1.0<length>) (y*1.0<length>)
    let private arc a r sweep b = Arc ({Start=a;Radius=p r r;XAxisRotation=0.0<degree>;LargeArc=false;Sweep=sweep;End=b}: Ellipse.EndpointArcData)
    let private strip title source cases =
        let results=cases |> List.map (fun (label,join) ->
            let path=Stroke.subpath source 2.0<length> join Offset.Butt |> require label
            let b=Path.boundingBox path |> require "bounds"
            let sb=Subpath.boundingBox source |> require "source bounds"
            label,path,{Min=Point.create (min b.Min.X sb.Min.X) (min b.Min.Y sb.Min.Y);Max=Point.create (max b.Max.X sb.Max.X) (max b.Max.Y sb.Max.Y)})
        let scale=min (270.0/(results |> List.map(fun (_,_,b)->float(b.Max.X-b.Min.X)) |> List.max)) (290.0/(results |> List.map(fun (_,_,b)->float(b.Max.Y-b.Min.Y)) |> List.max))
        let body=results |> List.mapi(fun i (name,path,b) ->
            let cx,cy=float(b.Min.X+b.Max.X)/2.0,float(b.Min.Y+b.Max.Y)/2.0
            label (320*i+160) 75 name +
            group $"translate({320*i+160} 245) scale({scale}) translate({-cx} {-cy})"
                (pathElement path $"fill:#9573dc;fill-opacity:0.3;stroke:#7343de;stroke-width:{1.0/scale}" +
                 pathElement (Path.ofSubpaths [source]) $"fill:none;stroke:#666;stroke-width:{0.85/scale}")) |> String.concat "\n"
        document 1280 430 (label 640 30 title + body + label 640 418 "Gray: source · purple: computed stroke · width 2 · Butt caps")
    let private miterSource=Subpath.assertPolyline [p -6.0 10.0;p 0.0 0.0;p 6.0 10.0]
    let miterComparison () = strip "Miter versus clipped miter" miterSource ["Miter(4)",Offset.Miter 4.0;"Miter(1.5)",Offset.Miter 1.5;"MiterClip(1.5)",Offset.MiterClip 1.5;"Round",Offset.Round]
    let miterLimits () = strip "MiterClip: varying the limit" miterSource ([0.5;1.0;1.5;2.0] |> List.map(fun x -> $"MiterClip({x})",Offset.MiterClip x))
    let private arcCases=["MiterClip(4)",Offset.MiterClip 4.0;"Round",Offset.Round;"Arcs(4)",Offset.Arcs 4.0;"Arcs(1.1)",Offset.Arcs 1.1]
    let arcsSelfIntersection () =
        let source=Subpath.create [arc (p -3.0 3.0) 3.0 true (p 0.0 0.0);arc (p 0.0 0.0) 5.0 true (p -5.0 5.0)] |> require "source"
        strip "Unequal source curvatures — self-intersecting example" source arcCases
    let arcsDisjoint () =
        let source=Subpath.create [arc (p -3.0 -3.0) 3.0 false (p 0.0 0.0);arc (p 0.0 0.0) 3.0 false (p 3.0 3.0)] |> require "source"
        strip "Continuation circles initially disjoint" source arcCases
    let all=["miter_clip_comparison.svg",miterComparison;"miter_clip_limits.svg",miterLimits;"arcs_join_self_intersection.svg",arcsSelfIntersection;"arcs_join_comparison_2.svg",arcsDisjoint]
