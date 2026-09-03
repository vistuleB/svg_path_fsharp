module SvgPath.Tests.ConvexHullGalleryRegressionParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private figureEight () =
    Subpath.create
        [ CubicBezier(point 0.0 0.0, point -336.0 -234.0, point -336.0 234.0, point 0.0 0.0)
          CubicBezier(point 0.0 0.0, point 336.0 -234.0, point 336.0 234.0, point 0.0 0.0) ]
    |> Result.bind (Subpath.setClosed true)
    |> Result.defaultWith (failwithf "%A")

let private figureEightBand () =
    Offset.subpathBandWith
        (figureEight ())
        18.0<length>
        34.0<length>
        { Offset.defaultOptions with Join = Round }
    |> Result.defaultWith (failwithf "%A")

let private supportValue segments angle =
    segments
    |> List.map (fun segment ->
        ConvexHull.internalSegmentSupport segment (Degree.fromFloat angle)
        |> Result.defaultWith (failwithf "%A")
        |> fun (_, _, value) -> value)
    |> List.max

let private supportMatches original hull =
    for angle in [ 0.0 .. 10.0 .. 350.0 ] do
        Assert.True(abs (supportValue original angle - supportValue hull angle) <= 1.0e-5<length>)

[<Fact>]
let ``figure eight hull preserves source support`` () =
    let source = figureEight ()
    let hull = ConvexHull.subpathHull source |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    supportMatches source.Segments hull.Segments

[<Fact>]
let ``figure eight band hull preserves band support`` () =
    let band = figureEightBand ()
    let hull = ConvexHull.pathHull band |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    supportMatches (band.Subpaths |> List.collect _.Segments) hull.Segments

[<Fact>]
let ``figure eight and band hull preserves combined support`` () =
    let source = figureEight ()
    let band = figureEightBand ()
    let combined = Path.ofSubpaths (source :: band.Subpaths)
    let hull = ConvexHull.pathHull combined |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    supportMatches (combined.Subpaths |> List.collect _.Segments) hull.Segments
