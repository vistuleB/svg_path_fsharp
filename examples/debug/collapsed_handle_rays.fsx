// Gleam 0181593 diagnostic: exercise private production fitters via reflection.
#r "../../src/SvgPath/bin/Debug/net9.0/SvgPath.dll"
open SvgPath
open System.Reflection
let moduleType = typeof<Segment>.Assembly.GetType("SvgPath.Offset")
let invoke name args = moduleType.GetMethod(name, BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).Invoke(null,args)
let p x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
let d x y : Point<1> = Point.create x y
let cases =
    [ "correct rays", d 1. 1., d 0. -1., true
      "wrong collapsed ray", d -1. -1., d 0. -1., false
      "wrong noncollapsed ray", d 1. 1., d 0. 1., false
      "both wrong", d -1. -1., d 0. 1., false
      "parallel backward", d -1. 0., d -1. 0., false
      "parallel forward", d 1. 0., d 1. 0., true ]
let samples = [0.5<parameter>, p 0.4 0.]
for label, ds, de, expected in cases do
    let a = invoke "stalledStartControl2" [|box(p 0. 0.); box(p 1. 0.); box ds; box de; box samples|]
    let b = invoke "stalledEndControl1" [|box(p 1. 0.); box(p 0. 0.); box(Point.negate de); box(Point.negate ds); box samples|]
    // Private error type is deliberately not exposed just for diagnostics.
    let isOk result = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(result, result.GetType()) |> fst |> fun case -> case.Name = "Ok"
    printfn "%s: start=%A end=%A" label a b
    if isOk a <> expected || isOk b <> expected then failwith label
printfn "12 collapsed-handle ray cases passed."
