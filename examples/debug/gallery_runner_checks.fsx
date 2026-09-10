#load "../../tools/GalleryFigures/GalleryJobs.fs"
open System
open System.IO
open System.Threading
open GalleryFigures

let directory = Path.Combine(Path.GetTempPath(), "fsharp-gallery-check-" + Guid.NewGuid().ToString("N"))
Directory.CreateDirectory directory |> ignore
use barrier = new Barrier(2)
let mutable observed = false
let result = GalleryJobs.run directory
                ["first.svg", (fun () ->
                    if not(barrier.SignalAndWait(10000)) then failwith "No concurrent start"
                    File.WriteAllText(Path.Combine(directory,"first.svg"),"<svg/>\n")
                    Ok ())
                 "failure.svg", (fun () ->
                    if not(barrier.SignalAndWait(10000)) then failwith "No concurrent start"
                    for _ in 1..100 do
                        if not(File.Exists(Path.Combine(directory,"first.svg"))) then Thread.Sleep 10
                    observed <- File.Exists(Path.Combine(directory,"first.svg"))
                    failwith "intentional_fixture_failure")
                 "exited.svg", (fun () -> Error "worker exited unsuccessfully")]
assert(not result && observed)
assert(File.Exists(Path.Combine(directory,"first.svg")))
assert(not(File.Exists(Path.Combine(directory,"failure.svg"))))
assert(File.Exists(Path.Combine(directory,"failure.svg.error.txt")))
assert(not(File.ReadAllText(Path.Combine(directory,"README.md")).Contains("failure.svg")))
assert(File.ReadAllText(Path.Combine(directory,"timings.tsv")).Contains("exited.svg\tfailed"))
printfn "Runner checks passed: concurrent start, immediate write, exception/exit isolation, reports. %s" directory
