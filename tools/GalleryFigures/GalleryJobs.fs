namespace GalleryFigures

open System
open System.IO
open System.Diagnostics
open System.Threading.Tasks

// Development-only scheduler. Each figure gets an isolated worker process,
// including the diagnostic observer, so tracing cannot mix between jobs.
module GalleryJobs =
    let run directory (jobs: (string * (unit -> Result<unit,string>)) list) =
        let names = List.map fst jobs
        if List.isEmpty jobs || List.distinct names <> names then
            invalidArg "jobs" "Expected nonempty uniquely named Gallery jobs"
        Directory.CreateDirectory directory |> ignore
        let timingFile = Path.Combine(directory,"timings.tsv")
        let indexFile = Path.Combine(directory,"README.md")
        File.WriteAllText(timingFile,"filename\tstatus\telapsed_ms\n")
        File.WriteAllText(indexFile,"# Generated Gallery Figures\n\nGeneration in progress; see timings.tsv.\n")
        let mutable pending = jobs |> List.map (fun (name,generate) ->
            let clock = Stopwatch.StartNew()
            let task = Task.Run(fun () ->
                let result = try generate() with ex -> Error(ex.ToString())
                result,clock.ElapsedMilliseconds)
            printfn "START %s" name
            name,clock,task)
        let mutable results = []
        while not (List.isEmpty pending) do
            let tasks = pending |> List.map (fun (_,_,task) -> task :> Task) |> List.toArray
            let completed = Task.WaitAny(tasks,10000)
            if completed < 0 then
                for name,clock,_ in pending do printfn "RUNNING %s elapsed=%.2fs" name clock.Elapsed.TotalSeconds
            else
                let name,_,task = pending[completed]
                let result,millis = task.Result
                let status = match result with Ok () -> "ok" | Error _ -> "failed"
                match result with
                | Ok () -> printfn "DONE %s elapsed=%.2fs" name (float millis / 1000.0)
                | Error error ->
                    File.WriteAllText(Path.Combine(directory,name+".error.txt"),error)
                    eprintfn "FAILED %s elapsed=%.2fs (see %s.error.txt)" name (float millis / 1000.0) name
                results <- results @ [name,status,millis]
                pending <- pending |> List.mapi (fun i item -> i,item) |> List.choose (fun (i,item) -> if i=completed then None else Some item)
                File.WriteAllText(timingFile,"filename\tstatus\telapsed_ms\n" +
                    (results |> List.map (fun (name,status,millis) -> sprintf "%s\t%s\t%d\n" name status millis) |> String.concat ""))
        let succeeded = results |> List.filter (fun (_,status,_) -> status="ok") |> List.map (fun (name,_,_) -> name) |> Set.ofList
        File.WriteAllText(indexFile,"# Generated Gallery Figures\n\n" +
            (names |> List.filter (fun name -> Set.contains name succeeded) |> List.map (fun name -> sprintf "- [%s](%s)\n" name name) |> String.concat ""))
        printfn "Gallery: %d succeeded, %d failed." succeeded.Count (names.Length-succeeded.Count)
        succeeded.Count = names.Length
