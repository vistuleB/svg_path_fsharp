module SvgPath.Tests.AffineTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
[<Fact>]
let ``matrix transforms raw coordinates`` () =
    let transform = Affine.matrix 2.0 3.0 5.0 7.0 11.0<length> 13.0<length>
    Assert.Equal(point 30.0 40.0, Affine.point transform (point 2.0 3.0))
    Assert.Equal(Point.create 19.0<length> 27.0<length>, Affine.linearPoint transform (Point.create 2.0<length> 3.0<length>))

[<Fact>]
let ``point pair similarity maps coordinates`` () =
    let sourceStart, sourceEnd = point 1.0 2.0, point 4.0 2.0
    let targetStart, targetEnd = point 10.0 -5.0, point 10.0 1.0
    let transform =
        Affine.pointPairSimilarity sourceStart sourceEnd targetStart targetEnd
        |> Result.defaultWith (fun () -> failwith "expected similarity")
    Assert.Equal(targetStart, Affine.point transform sourceStart)
    Assert.Equal(targetEnd, Affine.point transform sourceEnd)

[<Fact>]
let ``point triple map maps coordinates`` () =
    let sourceA, sourceB, sourceC = point 0.0 0.0, point 1.0 0.0, point 0.0 1.0
    let targetA, targetB, targetC = point 10.0 20.0, point 12.0 20.0, point 10.0 23.0
    let transform =
        Affine.pointTripleMap sourceA sourceB sourceC targetA targetB targetC
        |> Result.defaultWith (fun () -> failwith "expected affine map")
    Assert.Equal(targetA, Affine.point transform sourceA)
    Assert.Equal(targetB, Affine.point transform sourceB)
    Assert.Equal(targetC, Affine.point transform sourceC)
