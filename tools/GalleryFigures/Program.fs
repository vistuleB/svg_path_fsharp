namespace GalleryFigures

open System
open System.IO
open System.Xml.Linq
open System.Text.RegularExpressions
open System.Diagnostics
open System.Reflection

module Program =
    let private generate arguments =
        let output = Path.Combine(Fixtures.root,"docs/gallery")
        Directory.CreateDirectory(output) |> ignore
        let check = Array.contains "--check" arguments
        let selected = arguments |> Array.filter(fun arg -> arg<>"--check")
        let fixtures = Fixtures.all()
        let names = (fixtures |> List.map fst) @ Fixtures.snapshots |> Set.ofList
        let links = Regex.Matches(File.ReadAllText(Path.Combine(Fixtures.root,"GALLERY.md")), @"docs/gallery/([^\)]+\.svg)")
                    |> Seq.cast<Match> |> Seq.map(fun m -> m.Groups[1].Value) |> Set.ofSeq
        if names <> links then failwith "Gallery links and generator filenames differ"
        for name in selected do
            if not(Set.contains name names) then failwithf "Unknown figure: %s" name
        let wanted name = selected.Length=0 || selected |> Array.contains name
        let mutable failed = false
        let save name contents =
            let xml = XDocument.Parse contents
            let root = xml.Root
            for attribute in ["width";"height";"viewBox"] do
                if isNull(root.Attribute(XName.Get attribute)) then
                    failwithf "%s: missing SVG %s" name attribute
            let destination=Path.Combine(output,name)
            if check then
                if not(File.Exists destination) || File.ReadAllText destination<>contents then
                    eprintfn "out of date: %s" name
                    failed<-true
            else File.WriteAllText(destination,contents)
        for name,generate in fixtures do
            if wanted name then
                printfn "generating %s" name
                try save name (generate())
                with ex -> eprintfn "FAILED %s: %s" name ex.Message; failed<-true
        for name in Fixtures.snapshots do
            if wanted name then
                printfn "archived snapshot %s (not regenerated geometry)" name
                save name (File.ReadAllText(Path.Combine(Fixtures.root,"tools/GalleryFigures/Snapshots",name)))
        if failed then 1 else 0

    [<EntryPoint>]
    let main arguments =
        if Array.contains "--worker" arguments then
            generate (Array.filter ((<>) "--worker") arguments)
        else
            let names = (Fixtures.all() |> List.map fst) @ Fixtures.snapshots
            let selected = arguments |> Array.filter ((<>) "--check")
            for name in selected do
                if not(List.contains name names) then invalidArg "arguments" ("Unknown figure: " + name)
            let jobs = names |> List.filter (fun name -> selected.Length=0 || Array.contains name selected)
                       |> List.map (fun name -> name, fun () ->
                           let start = ProcessStartInfo("dotnet")
                           start.ArgumentList.Add(Assembly.GetExecutingAssembly().Location)
                           start.ArgumentList.Add("--worker")
                           start.ArgumentList.Add(name)
                           if Array.contains "--check" arguments then start.ArgumentList.Add("--check")
                           start.UseShellExecute <- false
                           start.RedirectStandardOutput <- true
                           start.RedirectStandardError <- true
                           use worker = Process.Start(start)
                           let stdout = worker.StandardOutput.ReadToEndAsync()
                           let stderr = worker.StandardError.ReadToEndAsync()
                           worker.WaitForExit()
                           let log = stdout.Result + stderr.Result
                           File.WriteAllText(Path.Combine(Fixtures.root,"docs/gallery",name+".log"),log)
                           if worker.ExitCode=0 then Ok () else Error log)
            if GalleryJobs.run (Path.Combine(Fixtures.root,"docs/gallery")) jobs then 0 else 1
