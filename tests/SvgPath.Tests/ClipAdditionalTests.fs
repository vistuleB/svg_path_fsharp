module SvgPath.Tests.ClipAdditionalTests

open SvgPath
open Xunit

let private point x y = Point.create x y
let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)

[<Fact>]
let ``outside line is discarded`` () =
    let boundary =
        Subpath.polygon
            [ point 0.0<length> 0.0<length>
              point 10.0<length> 0.0<length>
              point 10.0<length> 10.0<length>
              point 0.0<length> 10.0<length> ]
        |> Result.defaultWith (failwithf "%A")
    let input = Subpath.ofSegment (line -5.0<length> 15.0<length> 15.0<length> 15.0<length>)
    Assert.Empty(Clip.subpath input (Path.singleton boundary) Nonzero |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``empty path still validates options`` () =
    let options = { Clip.defaultOptions with Tolerance = 0.0<length> }
    Assert.Equal(
        Error(InvalidIntersectionTolerance 0.0<length>),
        Clip.pathWith Path.empty Path.empty Nonzero options)
