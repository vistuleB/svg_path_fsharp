namespace GalleryFigures

open System.IO
open System.Xml.Linq
open SvgPath
open Drawing

/// Render the actual private call results from a diagnostic-only library build.
module ArrangementCapture =
    let generate () =
#if GALLERY_DIAGNOSTICS
        let root = System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"../.."))
        let document = XDocument.Load(System.IO.Path.Combine(__SOURCE_DIRECTORY__,"Inputs/package_title.svg"))
        let sourceNode = document.Descendants(XName.Get("path","http://www.w3.org/2000/svg")) |> Seq.head
        let source = parse (sourceNode.Attribute(XName.Get "d").Value)
        let first = Offset.pathWith source 1.05<length> (Offset.Miter 4.0) Offset.Butt Offset.defaultOptions |> require "first offset"
        let options = {Offset.defaultOptions with Offset.SingleOffsetTrimming={Offset.defaultOptions.SingleOffsetTrimming with Offside=false}}
        Offset.diagnosticClassification.Clear()
        Offset.diagnosticParity.Clear()
        Offset.pathWith first 1.05<length> (Offset.Miter 4.0) Offset.Butt options |> require "second offset" |> ignore
        let build,eligible,retainedResult = Offset.diagnosticClassification |> Seq.exactlyOne
        let retained = retainedResult |> require "captured classification"
        let reduced = Offset.diagnosticParity |> Seq.exactlyOne |> require "captured parity reduction"
        let ids (graph:Offset.OffsetTrimGraph) = graph.Edges |> List.map _.Id |> Set.ofList
        let eligibleIds,retainedIds,survivorIds = ids eligible,ids retained,ids reduced
        let incidences = retained.Edges |> List.collect (fun e -> [e.StartVertex;e.EndVertex])
        let firstDeleted = retained.Edges |> List.choose (fun e ->
            if not(Set.contains e.Id survivorIds)
               && ([e.StartVertex;e.EndVertex] |> List.exists (fun vertex -> incidences |> List.filter ((=) vertex) |> List.length = 1))
            then Some e.Id else None) |> Set.ofList
        let layer geometry style = "<path d=\""+Serialize.path geometry+"\" style=\""+style+"\"/>"
        let edge (e:Arrangement.ArrangementEdge) color =
            let width,opacity = match color with
                                | "#facc15" -> "0.22","; opacity: 0.98"
                                | "#7c3aed" -> "0.16","; opacity: 0.95"
                                | "#cbd5e1" -> "0.05",""
                                | _ -> "0.09","; opacity: 0.82"
            "<g><title>edge "+string e.Id+"</title>"+
            layer (Path.singleton (Subpath.ofSegment e.Segment))
                ("fill: none; stroke: "+color+"; stroke-width: "+width+"; stroke-linecap: round; stroke-linejoin: round"+opacity)+"</g>"
        let label (e:Arrangement.ArrangementEdge) =
            let box = Segment.boundingBox e.Segment |> require "edge bounds"
            "<text x=\""+number(float((box.Min.X+box.Max.X)/2.0))+"\" y=\""+number(float((box.Min.Y+box.Max.Y)/2.0))+
            "\" font-size=\"0.3\" style=\"fill: #1e3a8a; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; text-anchor: middle; dominant-baseline: central\">"+string e.Id+"</text>"
        let dot (p:Point<length>) = "<circle cx=\""+number(float p.X)+"\" cy=\""+number(float p.Y)+"\" r=\"0.035\" style=\"fill: #111827; stroke: none; opacity: 0.8\"/>"
        let selected ids = build.Graph.Edges |> List.filter (fun e -> Set.contains e.Id ids)
        let svg =
            [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1800\" height=\"420\" viewBox=\"-5.1 -5.1 99.06998 23.565\">"
              "<rect x=\"-5.1\" y=\"-5.1\" width=\"99.06998\" height=\"23.565\" fill=\"white\"/>"
              layer source "fill: #111827; stroke: none; opacity: 0.16"
              layer first "fill: none; stroke: #2563eb; stroke-width: 0.07; stroke-linecap: round; stroke-linejoin: round"
              yield! build.Graph.Edges |> List.map (fun e -> edge e "#cbd5e1")
              yield! selected eligibleIds |> List.map (fun e -> edge e (if Set.contains e.Id retainedIds then "#16a34a" else "#dc2626"))
              yield! selected firstDeleted |> List.map (fun e -> edge e "#facc15")
              yield! selected survivorIds |> List.map (fun e -> edge e "#7c3aed")
              yield! selected retainedIds |> List.map label
              yield! selected eligibleIds |> List.collect (fun e -> [dot(Segment.start e.Segment);dot(Segment.finish e.Segment)])
              "</svg>" ] |> String.concat ""
        let summary = sprintf "Offset 1.05 twice; Miter(4); second offset offside=false.\nVertices: %d\nGraph edges: %d\nEligible offset edges: %d\nInitially submerged: %d\nInitially retained: %d\nPositive final capacity: %d\nInitially dangling deleted edge IDs: [%s]\n"
                          build.Graph.Vertices.Length build.Graph.Edges.Length eligibleIds.Count (eligibleIds.Count-retainedIds.Count) retainedIds.Count survivorIds.Count (firstDeleted |> Seq.map string |> String.concat "; ")
        File.WriteAllText(System.IO.Path.Combine(root,"docs/gallery/package-title-second-offset-arrangement-capture.txt"),summary)
        printf "%s" summary
        svg
#else
        failwith "This fixture requires scripts/generate-gallery-figures (diagnostic-only build)."
#endif
