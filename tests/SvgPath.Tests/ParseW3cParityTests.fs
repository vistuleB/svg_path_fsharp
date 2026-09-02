module SvgPath.Tests.ParseW3cParityTests

open SvgPath
open Xunit

let private parsed source = Parse.path source |> Result.defaultWith (failwithf "%A")
let private roundTrips sources =
    sources |> List.iter (fun source ->
        let serialized = source |> parsed |> Serialize.path
        match Parse.path serialized with | Ok _ -> () | Error error -> failwithf "%A" error)
let private equivalent first second = Assert.Equal(parsed first, parsed second)

[<Fact>]
let ``w3c_svg11_paths_data_01_test`` () =
    roundTrips [
        "M 210 130 C 145 130 110 80 110 80 S 75 25 10 25 m 0 105 c 65 0 100 -50 100 -50 s 35 -55 100 -55"
        "M 240 90 c 0 30 7 50 50 0 c 43 -50 50 -30 50 0 c 0 83 -68 -34 -90 -30 C 240 60 240 90 240 90 z"
        "M80 170 C100 170 160 170 180 170Z"
        "M5 260 C40 260 60 175 55 160 c -5 15 15 100 50 100Z"
        "m 200 260 c 50 -40 50 -100 25 -100 s -25 60 25 100"
        "M 360 100 C 420 90 460 140 450 190"
        "M360 210 c 0 20 -16 36 -36 36 s -36 -16 -36 -36 s 16 -36 36 -36 s 36 16 36 36 z"
        "m 360 325 c -40 -60 95 -100 80 0 z"
    ]

[<Fact>]
let ``w3c_svg11_paths_data_02_test`` () =
    roundTrips [
        "M 15 20 Q 30 120 130 30 M 180 80 q -75 -100 -163 -60z"
        "M372 130Q272 50 422 10zm70 0q50-150-80-90z"
        "M224 103Q234 -12 304 33Z"
        "M208 168Q258 268 308 168T258 118Q128 88 208 168z"
        "M 60 100 Q -40 150 60 200 Q 160 150 60 100 z"
        "M240 296q25-100 47 0t47 0t47 0t47 0t47 0z"
        "M172 193q-100 50 0 50Q72 243 172 293q100 -50 0 -50Q272 243 172 193z"
    ]

[<Fact>]
let ``w3c_svg11_paths_data_03_test`` () =
    roundTrips [
        "M 25 70 A 40 40 0 1 0 25 69 Z"
        "m 150 100 a 50 40 0 1 0 25 -70 z"
        "M 350 245 a 40 40 0 1 0 80 60"
        "M 270 30 A 50 50 0 1 0 345 30 a 50 50 0 1 0 50 0 a 50 50 0 1 0 25 0 z"
        "M 30 150 a 40 40 0 0 1 65 50 Z m 30 30 A 20 20 0 0 0 125 230 Z m 40 24 a 20 20 0 0 1 65 50 z"
        "M 215 190 A 40 200 10 0 0 265 190 A 40 200 20 0 1 315 190 A 40 200 30 0 0 365 190 A 40 200 40 0 1 415 190 A 40 200 50 0 0 465 190"
    ]

[<Fact>]
let ``w3c_svg11_paths_data_04_through_10_test`` () =
    roundTrips [
        "M 62 56 L 113.96152 146 L 10.03848 146 L 62 56 Z M 62 71 L 100.97114 138.5 L 23.02886 138.5 L 62 71 Z"
        "M 177 56 L 228.96152 146 L 125.03848 146 L 177 56 Z M 177 71 L 215.97114 138.5 L 138.02886 138.5 L 177 71 Z"
        "m 62 190 l 51.96152 90 l -103.92304 0 l 51.96152 -90 z m 0 15 l 38.97114 67.5 l -77.91228 0 l 38.97114 -67.5 z"
        "M 240 56 H 270 V 86 H 300 V 116 H 330 V 146 H 240 V 56 Z"
        "m 240 190 h 30 v 30 h 30 v 30 h 30 v 30 h -90 v -90 z"
        "M 62 56 113.96152 146 10.03848 146 62 56 Z M 62 71 100.97114 138.5 23.02886 138.5 62 71 Z"
        "m 62 190 51.96152 90 -103.92304 0 51.96152 -90 z m 0 15 38.97114 67.5 -77.91228 0 38.97114 -67.5 z"
        "M 100 0 L 100 80 0 40 100 0"
        "m 100 0 l 0 80 -100 -40 100 -40"
        "M 0 0 L 100 40 0 80 Z"
        "m 0 0 l 100 40 -100 40 z"
    ]

[<Fact>]
let ``w3c_svg11_paths_data_12_through_16_test`` () =
    roundTrips [
        "M 100 100 C 100 20 200 20 200 100 S 300 180 300 100"
        "M 100 250 S 200 200 200 250 300 300 300 250"
        "M 240 56 H 270 300 320 400"
        "M 240 156 V 180 200 260 300"
        "m 62 56 51.96152 90 -103.92304 0 51.96152 -90 z m 0 15 38.97114 67.5 -77.91228 0 38.97114 -67.5 z"
        "M 177 56 228.96152 146 125.03848 146 177 56 Z M 177 71 215.97114 138.5 138.02886 138.5 177 71 Z"
        "M 20 20 Q 50 10 80 20 110 30 140 20 170 10 200 20"
        "M 20 50 T 50 50 80 50"
        "M100,120 L160,220 L40,220 z"
        "M100,120 160,220 40,220 z"
        "m350,120 60,100 -120,0 z"
    ]

[<Fact>]
let ``w3c_svg11_paths_data_17_test`` () =
    equivalent "M 50 50 L 50 150 L 150 150 L 150 50 z" "M 50 50 L 50 150 L 150 150 L 150 50 Z"

[<Fact>]
let ``w3c_svg11_paths_data_18_test`` () =
    equivalent "M 20 40\nH 40" "M 20 40 H 40"
    equivalent "\nM\n20\n60\nH\n40\n" "M 20 60 H 40"
    equivalent "M 20 80 H40" "M 20,80 H 40"
    equivalent "M 20 120 H 40.5 0.6" "M 20 120 H 40.5.6"
    equivalent "M 20 140 h 10 -20" "M 20 140 h 10-20"
    [ "M 20 100 H 40#90"; "M 20 160 H 40#90" ]
    |> List.iter (fun source -> match Parse.path source with | Error _ -> () | Ok _ -> failwith "expected parse error")

[<Fact>]
let ``w3c_svg11_paths_data_19_test`` () =
    [ "M20 20 H40 H60", "M20 20 H40 60"; "M20 40 h20 h20", "M20 40 h20 20"; "M120 20 V40 V60", "M120 20 V40 60"; "M140 20 v20 v20", "M140 20 v20 20"; "M220 20 L240 20 L260 20", "M220 20 L240 20 260 20"; "M220 40 l20 0 l20 0", "M220 40 l20 0 20 0"; "M50 150 C50 50 200 50 200 150 C200 50 350 50 350 150", "M50 150 C50 50 200 50 200 150 200 50 350 50 350 150"; "M50 250 S125 200 200 250 S275 200 350 250", "M50 250 S125 200 200 250 275 200 350 250"; "M50 300 Q125 275 200 300 Q275 325 350 300", "M50 300 Q125 275 200 300 275 325 350 300"; "M425 25 T425 75 T425 125", "M425 25 T425 75 425 125"; "M400 200 A25 25 0 0 0 425 150 A25 25 0 0 0 400 200", "M400 200 A25 25 0 0 0 425 150 25 25 0 0 0 400 200" ]
    |> List.iter (fun (first, second) -> equivalent first second)

[<Fact>]
let ``w3c_svg11_paths_data_20_test`` () =
    equivalent "M120,120 h25 a25,25 0 1,0 -25,25 z" "M120,120 h25 a25,25 0 10 -25,25z"
    equivalent "M200,120 h-25 a25,25 0 1,1 25,25 z" "M200,120 h-25 a25,25 0 1125,25 z"
    equivalent "M120,200 h25 a25,25 0 1,1 -25,-25 z" "M120,200 h25 a25,25 0 1 1-25,-25 z"
    [ "M280,120 h25 a25,25 0 6 0 -25,25 z"; "M360,120 h-25 a25,25 0 1 -1 25,25 z"; "M200,200 h-25 a25,2501 025,-25 z"; "M280,200 h25 a25 25 0 1 7 -25 -25 z"; "M360,200 h-25 a25,25 0 -1 0 25,-25 z" ]
    |> List.iter (fun source -> match Parse.path source with | Error _ -> () | Ok _ -> failwith "expected parse error")
