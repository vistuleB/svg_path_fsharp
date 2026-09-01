module SvgPath.Tests.WindingFieldTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private polygon points =
    Subpath.polygon points |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``clockwise SVG polygon has positive winding`` () =
    let square = polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
    let path = Path.singleton square
    Assert.Equal(Ok(Winding 1), WindingField.pathWinding (point 1.0 1.0) path)
    Assert.Equal(Ok(Winding 0), WindingField.pathWinding (point 3.0 1.0) path)
    Assert.Equal(Ok BoundaryWinding, WindingField.pathWinding (point 2.0 1.0) path)

[<Fact>]
let ``path winding accumulates oriented subpaths`` () =
    let outer = polygon [ point 0.0 0.0; point 4.0 0.0; point 4.0 4.0; point 0.0 4.0 ]
    let inner = polygon [ point 1.0 1.0; point 1.0 3.0; point 3.0 3.0; point 3.0 1.0 ]
    Assert.Equal(Ok(Winding 0), WindingField.pathWinding (point 2.0 2.0) (Path.ofSubpaths [ outer; inner ]))

[<Fact>]
let ``side levels follow geometric left and right`` () =
    let top = Line(point 0.0 0.0, point 2.0 0.0)
    let square = polygon [ point 0.0 0.0; point 2.0 0.0; point 2.0 2.0; point 0.0 2.0 ]
    Assert.Equal(
        Ok(0, 1),
        WindingField.segmentSideNonzeroLevels top (Path.singleton square) 0.001<length> WindingField.defaultOptions)

[<Fact>]
let ``midpoint cusp uses symmetric fallback`` () =
    let cusp = CubicBezier(point -1.0 0.0, point 1.0 1.0, point -1.0 1.0, point 1.0 0.0)
    let source =
        Subpath.create [ cusp; Line(point 1.0 0.0, point -1.0 0.0) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(
        Ok(-1, 0),
        WindingField.segmentSideNonzeroLevels cusp (Path.singleton source) 0.0001<length> WindingField.defaultOptions)

[<Fact>]
let ``side sampling rejects invalid distance and collapsed segment`` () =
    let line = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(
        Error(InvalidContainmentTolerance 0.0<length>),
        WindingField.segmentSideNonzeroLevels line Path.empty 0.0<length> WindingField.defaultOptions)
    let collapsed = Line(point 1.0 2.0, point 1.0 2.0)
    Assert.Equal(
        Error IndeterminateWindingSideLevels,
        WindingField.segmentSideNonzeroLevels collapsed Path.empty 0.0001<length> WindingField.defaultOptions)

[<Fact>]
let ``side sampling validates containment options before degenerate fallback`` () =
    let collapsed = Line(point 1.0 2.0, point 1.0 2.0)
    let invalid = { WindingField.defaultOptions with Samples = 0 }
    Assert.Equal(
        Error(InvalidContainmentSamples 0),
        WindingField.segmentSideNonzeroLevels collapsed Path.empty 0.0001<length> invalid)
