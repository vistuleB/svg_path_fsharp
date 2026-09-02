module SvgPath.Tests.OffsetParityTests

open SvgPath
open Xunit

module Subject = Offset

let private point x y = Point.create (x * 1.0<length>) (y * 1.0<length>)

[<Fact>]
let ``defaults use the Gleam trimming sample count`` () =
    Assert.Equal(5, Subject.defaultOptions.DistanceOptions.Samples)

[<Fact>]
let ``line offset follows the Gleam visual-left convention`` () =
    let result =
        Subject.segment (Line(point 0.0 0.0, point 10.0 0.0)) 2.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal<Segment list>(
        [ Line(point 0.0 -2.0, point 10.0 -2.0) ],
        Subpath.segments result)

[<Fact>]
let ``offset map matches Gleam arc-length and visual-left semantics`` () =
    let source =
        Subpath.create
            [ Line(point 0.0 0.0, point 3.0 0.0)
              Line(point 3.0 0.0, point 3.0 4.0) ]
        |> Result.defaultWith (failwithf "%A")
    let mapping = Subject.subpathOffsetMap source |> Result.defaultWith (failwithf "%A")
    let first = mapping (point 2.0 1.0) |> Result.defaultWith (failwithf "%A")
    let second = mapping (point 5.0 1.0) |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.distance first (point 2.0 -1.0) < 1.0e-12<length>)
    Assert.True(Point.distance second (point 4.0 2.0) < 1.0e-12<length>)

[<Fact>]
let ``closed rectangular band matches Gleam contour topology`` () =
    let source =
        Subpath.polygon
            [ point 0.0 0.0; point 10.0 0.0
              point 10.0 8.0; point 0.0 8.0 ]
        |> Result.defaultWith (failwithf "%A")
    let result =
        Subject.subpathBand source -1.0<length> 1.0<length>
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length (Path.subpaths result))
    Assert.All(Path.subpaths result, fun subpath -> Assert.True(Subpath.isClosed subpath))

[<Fact>]
let ``open line round stroke matches Gleam contour topology`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let result =
        Subject.subpathStrokeWith
            source 2.0<length> RoundCap
            { Subject.defaultOptions with Join = Round }
        |> Result.defaultWith (failwithf "%A")
    let contours = Path.subpaths result
    Assert.Single(contours) |> ignore
    Assert.True(Subpath.isClosed contours[0])
