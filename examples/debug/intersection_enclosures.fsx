// Run after building: dotnet fsi examples/debug/intersection_enclosures.fsx
// Exercise private production helpers without adding public API.
#r "../../src/SvgPath/bin/Debug/net9.0/SvgPath.dll"
open SvgPath
open System.Reflection
let moduleType = typeof<Segment>.Assembly.GetType("SvgPath.Intersections")
let invoke name args = moduleType.GetMethod(name, BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).Invoke(null,args)
let p x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let separate a b = invoke "enclosingPointsDisjoint" [|box a;box b|] :?> bool
let mutable checks = 0
let check condition = if not condition then failwith "Enclosure check failed" else checks <- checks + 1
check (not (separate [p 0. 0.] [p 0. 0.]))
check (separate [p 0. 0.] [p 1. 0.])
check (not (separate [p 0. 0.;p 1. 0.] [p 1. 0.;p 2. 0.]))
check (separate [p 0. 0.;p 1. 0.] [p 2. 0.;p 3. 0.])
check (separate [p 0. 0.;p 2. 2.;p 1. 1.01] [p 0. 0.1;p 2. 2.1;p 1. 1.11])
check (not (separate [p 0. 0.;p 2. 2.] [p 0. 2.;p 2. 0.]))
let curves =
    [ Line(p 0. 0.,p 2. 3.)
      QuadraticBezier(p 0. 0.,p 3. -2.,p 1. 4.)
      CubicBezier(p 0. 0.,p 3. -2.,p -4. 2.,p 1. 4.) ]
    @ [for sweep in [false;true] do
           for large in [false;true] do
               yield Arc {Start=p 1. 2.;Radius=p 5. 3.;XAxisRotation=37.0<degree>;LargeArc=large;Sweep=sweep;End=p 4. -1.}]
for curve in curves do
    for a,b in [0.,1.;0.2,0.8;0.5,0.50000001] do
        let points = (invoke "segmentEnclosingPoints" [|box curve;box a;box b|] :?> Result<Point<length> list,SegmentError>)
                     |> Result.defaultWith (failwithf "%A")
        for i in 0..100 do
            let point = Segment.point curve (Parameter.fromFloat (a+(b-a)*float i/100.)) |> Result.defaultWith (failwithf "%A")
            check (not (separate points [point]))
printfn "Enclosure helper checks: %d passed" checks
