namespace ReadmeFigures

open System.IO

module Program =
    [<EntryPoint>]
    let main arguments =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../.."))
        let output = Path.Combine(root, "docs/readme")
        Directory.CreateDirectory output |> ignore
        let check = arguments |> Array.contains "--check"
        let mutable mismatch = false
        for name, generate in Fixtures.all do
            let destination = Path.Combine(output, name)
            let contents = generate ()
            if check then
                if not (File.Exists destination) || File.ReadAllText destination <> contents then
                    eprintfn "out of date: %s" name; mismatch <- true
            else File.WriteAllText(destination, contents); printfn "generated %s" name
        if mismatch then 1 else 0
