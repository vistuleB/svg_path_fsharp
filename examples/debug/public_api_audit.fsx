#r "../../src/SvgPath/bin/Debug/net9.0/SvgPath.dll"
open System
open Microsoft.FSharp.Reflection
open SvgPath
let p x y = Point.create (x*1.0<length>) (y*1.0<length>)
let assembly=typeof<Affine>.Assembly
let errors=assembly.GetExportedTypes() |> Array.filter(fun t -> t.Name.EndsWith("Error") && FSharpType.IsUnion t)
printfn "Public error unions: %d" errors.Length
for t in errors do
    for c in FSharpType.GetUnionCases t do
        for f in c.GetFields() do
            if f.Name="Item" || (f.Name.StartsWith("Item") && f.Name.Length>4 && Char.IsDigit f.Name[4]) then
                printfn "UNLABELLED: %s.%s.%s" t.FullName c.Name f.Name
printfn "Root exported: %b" (assembly.GetExportedTypes() |> Array.exists(fun t -> t.FullName="SvgPath.Root"))
printfn "Affine degenerate: %A" (Affine.pointPairSimilarity (p 0.0 0.0) (p 0.0 0.0) (p 1.0 1.0) (p 2.0 2.0))
printfn "Transform degenerate: %A" (Transform.pointPairSimilarity (p 0.0 0.0) (p 0.0 0.0) (p 1.0 1.0) (p 2.0 2.0) 1e-9<length>)
printfn "Transform negative tolerance: %A" (Transform.pointPairSimilarity (p 0.0 0.0) (p 1.0 0.0) (p 1.0 1.0) (p 2.0 1.0) -1.0<length>)
for join in [Offset.MiterClip nan;Offset.MiterClip infinity;Offset.Arcs nan;Offset.Arcs infinity] do
    printfn "Empty stroke invalid join %A: %A" join (Stroke.path Path.empty 1.0<length> join Offset.Butt)
printfn "Exported bare Error type: %b" (assembly.GetExportedTypes() |> Array.exists(fun t -> t.FullName="SvgPath.Error"))
