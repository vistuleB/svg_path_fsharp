namespace SvgPath.Tests

open Xunit
open SvgPath

// F#-specific smoke coverage; the one-to-one Gleam ports live in CsgParityTests.
module CsgAdditionalTests =
    let point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)
    let rectangle x y width height =
        [ Line(point x y, point (x + width) y)
          Line(point (x + width) y, point (x + width) (y + height))
          Line(point (x + width) (y + height), point x (y + height))
          Line(point x (y + height), point x y) ]
        |> Subpath.create
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (fun error -> failwithf "%A" error)
        |> fun subpath -> Path.ofSubpaths [ subpath ]

    let result operation =
        operation
        |> Result.defaultWith (fun error -> failwithf "%A" error)
        |> _.Path

    [<Fact>]
    let ``overlapping rectangle union has expected area`` () =
        let output = Csg.union (rectangle 0.0 0.0 2.0 2.0) (rectangle 1.0 0.0 2.0 2.0) Nonzero |> result
        Assert.Equal(6.0, float (abs (Area.signedPath output)), 8)
        Assert.Single(Path.subpaths output) |> ignore

    [<Fact>]
    let ``overlapping rectangle intersection has expected area`` () =
        let output = Csg.intersection (rectangle 0.0 0.0 2.0 2.0) (rectangle 1.0 0.0 2.0 2.0) Nonzero |> result
        Assert.Equal(2.0, float (abs (Area.signedPath output)), 8)

    [<Fact>]
    let ``rectangle difference has expected area`` () =
        let output = Csg.difference (rectangle 0.0 0.0 3.0 2.0) (rectangle 1.0 0.0 1.0 2.0) Nonzero |> result
        Assert.Equal(4.0, float (abs (Area.signedPath output)), 8)

    [<Fact>]
    let ``symmetric difference retains both exclusive regions`` () =
        let output = Csg.symmetricDifference (rectangle 0.0 0.0 2.0 2.0) (rectangle 1.0 0.0 2.0 2.0) Nonzero |> result
        Assert.Equal(4.0, float (abs (Area.signedPath output)), 8)
        Assert.Equal(2, Path.subpaths output |> List.length)

    [<Fact>]
    let ``nested contours retain concentric winding layers`` () =
        let path =
            Path.ofSubpaths
                [ Path.subpaths (rectangle 0.0 0.0 10.0 10.0) |> List.head
                  Path.subpaths (rectangle 2.0 2.0 6.0 6.0) |> List.head ]
        let output = Csg.nestedContours path |> result
        Assert.Equal(2, Path.subpaths output |> List.length)
