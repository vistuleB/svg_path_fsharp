// Gleam 7a41f3e private production-helper diagnostics, using reflection only.
#r "../../src/SvgPath/bin/Debug/net9.0/SvgPath.dll"
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open SvgPath
let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static
let assembly = typeof<Segment>.Assembly
let moduleType = assembly.GetType("SvgPath.Arrangement")
let invoke name args = moduleType.GetMethod(name, flags).Invoke(null,args)
let fields (value: obj) = FSharpValue.GetUnionFields(value,value.GetType())
let ok value = let case, values = fields value in if case.Name <> "Ok" then failwithf "%A" value else values[0]
let error value = let case,_ = fields value in if case.Name <> "Error" then failwithf "Expected error: %A" value
let items (value: obj) = (value :?> System.Collections.IEnumerable) |> Seq.cast<obj> |> Seq.toList
let record name values =
    let t = assembly.GetTypes() |> Array.find (fun t -> t.Name=name)
    FSharpValue.MakeRecord(t,values,BindingFlags.Public ||| BindingFlags.NonPublic)
let field name (value: obj) = value.GetType().GetProperty(name,BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(value)
let listOfType (element: Type) values =
    let t = typedefof<list<_>>.MakeGenericType(element)
    let cases = FSharpType.GetUnionCases t
    List.foldBack (fun value tail -> FSharpValue.MakeUnion(cases[1],[|value;tail|])) values (FSharpValue.MakeUnion(cases[0],[||]))
let sameList (prototype: obj) values = listOfType (prototype.GetType().GetGenericArguments()[0]) values
let p x y = Point.create (x*1.0<length>) (y*1.0<length>)
let d x y : Point<1> = Point.create x y
let square = Subpath.polygon [p 0. 0.;p 10. 0.;p 10. 10.;p 0. 10.] |> Result.defaultWith (failwithf "%A")
let graph = Arrangement.build [Path.singleton square] 1e-6<length> 1e-5<length> |> Result.defaultWith (failwithf "%A") |> _.Graph
let rec gather remaining found =
    match remaining with
    | [] -> List.rev found
    | first::_ ->
        let edges = invoke "faceWalk" [|box graph;box first|] |> ok :?> Arrangement.ArrangementFaceEdge list
        gather (remaining |> List.filter (fun e -> not(List.contains e edges))) ({Offset.Outer=false;Edges=edges}::found)
let walks = gather (graph.Edges |> List.collect (fun e -> [({EdgeId=e.Id;Left=true}: Arrangement.ArrangementFaceEdge);({EdgeId=e.Id;Left=false}: Arrangement.ArrangementFaceEdge)])) []
let components = invoke "dualComponents" [|box graph.Edges|]
let edges = invoke "dualSweepEdges" [|box graph.Edges;components;box walks|] |> ok
let tolerance = invoke "dualSweepTolerance" [|box graph|]
let line origin direction = record "DualSweepLine" [|box origin;box direction|]
let sweep line = invoke "dualSweepIntersections" [|edges;box graph.Vertices;line;tolerance|]
let hits = sweep(line (p 5. 5.) (d 1. 0.)) |> ok
if (items hits).Length<>2 then failwith "Expected two hits"
let exteriors = invoke "dualLineExteriors" [|hits|] |> ok
let ext = items exteriors |> List.exactlyOne
let states = [unbox<int>(field "Component" ext),unbox<int>(field "Walk" ext)]
let placementType = assembly.GetTypes() |> Array.find (fun t -> t.Name="DualPlacement")
let placements = invoke "dualLinePlacements" [|hits;box states;exteriors;box walks;listOfType placementType []|] |> ok
if items placements |> List.exists (fun p -> unbox<int>(field "Confirmations" p)<>1) then failwith "Per-line confirmation"
let twice = invoke "dualMergePlacements" [|placements;placements;box true|] |> ok
if items twice |> List.exists (fun p -> unbox<int>(field "Confirmations" p)<>2) then failwith "Repeated confirmation"
let bad = record "DualPlacement" [|field "Walk" (items placements).Head;box [true;true];box 1|]
invoke "dualMergePlacements" [|sameList placements [bad];placements;box true|] |> error
invoke "dualFindExteriors" [|edges;box graph.Vertices;tolerance;box 1;sameList exteriors [];box 1729L;box 0|] |> error
sweep(line (p 0. 0.) (d 1. 0.)) |> error
sweep(line (p 0. 0.) (d 0.6 0.8)) |> error
let curve = QuadraticBezier(p -1. 1.,p 0. -1.,p 1. 1.)
let bounds = Segment.boundingBox curve |> Result.defaultWith (failwithf "%A")
let edge = record "DualSweepEdge" [|box 0;box 0;box 0;box 1;box curve;box bounds|]
invoke "dualSweepEdgeHits" [|edge;line (p 0. 0.) (d 1. 0.);box 1e-9<length>|] |> error
let closeA = record "DualSweepHit" [|box 0;box 0.0<length>;box 1e-8<length>;box 0;box 0;box 1|]
let closeB = record "DualSweepHit" [|box 1;box 1e-9<length>;box 1e-8<length>;box 0;box 1;box 0|]
invoke "dualCheckHitSeparation" [|sameList hits [closeA;closeB];box 1e-9<length>|] |> error
printfn "Sweep acceptance, confirmation, contradiction, exhaustion, vertex, touching-root and separation checks passed."
