module SvgPath.Tests.ReadmeRecipesTests

open SvgPath
open Xunit

let private require result = result |> Result.defaultWith (failwithf "%A")
let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)

[<Fact>]
let ``fitted recipe trims by distance`` () =
    let curve t = point (10.0 * float t) (10.0 * float t * float t)
    let full = Fit.subpathFromParametric 0.0<parameter> 1.0<parameter> curve |> require
    let middle = ReadmeRecipes.middleHalf curve |> require
    let fullLength = Measure.subpathLength full |> require
    let middleLength = Measure.subpathLength middle |> require
    Assert.True(abs (middleLength - fullLength / 2.0) < 0.00001<length>)
    Assert.Equal(Measure.subpathPointAtLength full (fullLength / 4.0) |> require, Subpath.start middle)

[<Fact>]
let ``projected recipe keeps a reusable path address`` () =
    let path = Parse.path "M0 0H10 M20 0V10" |> require
    let projection, derivative = ReadmeRecipes.nearestLocation (point 23.0 5.0) path |> require
    Assert.Equal(3.0<length>, projection.Distance)
    Assert.Equal(point 20.0 5.0, projection.Point)
    Assert.Equal(1, projection.At.SubpathIndex)
    Assert.Equal(Ok projection.Point, Path.point path projection.At)
    Assert.Equal(Point.create 0.0<length/parameter> 10.0<length/parameter>, derivative)

[<Fact>]
let ``stroke union recipe preserves geometry and errors`` () =
    let line = Parse.path "M0 0H10" |> require
    let region = Parse.path "M5 -1H15V1H5Z" |> require
    let combined = ReadmeRecipes.outlineUnion line 2.0<length> region |> require
    let size = Area.path combined Nonzero |> require
    Assert.True(abs (size - 30.0<length^2>) < 0.000001<length^2>)
    Assert.Equal(Error(ReadmeRecipes.StrokeFailure(Stroke.InvalidStrokeOutlineWidth 0.0<length>)), ReadmeRecipes.outlineUnion line 0.0<length> region)
