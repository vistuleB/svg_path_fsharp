module SvgPath.Tests.AffineTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private degrees value = Degree.fromFloat value

[<Fact>]
let ``matrix tuple round trip preserves coefficient units`` () =
    let transform = Affine.matrix 1.0 2.0 3.0 4.0 5.0<length> 6.0<length>
    Assert.Equal((1.0, 2.0, 3.0, 4.0, 5.0<length>, 6.0<length>), Affine.toTuple transform)
    Assert.Equal(transform, Affine.fromTuple (Affine.toTuple transform))

[<Fact>]
let ``chain follows application order`` () =
    let transform = Affine.chain (Affine.translate 2.0<length> 3.0<length>) (Affine.scale 4.0)
    Assert.Equal(point 12.0 20.0, Affine.point transform (point 1.0 2.0))

[<Fact>]
let ``about point leaves its center fixed`` () =
    let center = point 3.0 4.0
    let transform = Affine.aboutPoint (Affine.rotate (degrees 90.0)) center
    Assert.Equal(center, Affine.point transform center)
    Assert.Equal(point 3.0 5.0, Affine.point transform (point 4.0 4.0))

[<Fact>]
let ``linear part preserves the input coordinate unit`` () =
    let derivative = Point.create 2.0<length / parameter> 3.0<length / parameter>
    let transformed: Point<length / parameter> = Affine.linearPoint (Affine.scaleXY 4.0 5.0) derivative
    Assert.Equal(Point.create 8.0<length / parameter> 15.0<length / parameter>, transformed)

[<Fact>]
let ``point pair similarity maps both endpoints`` () =
    let sourceStart, sourceEnd = point 2.0 3.0, point 4.0 3.0
    let targetStart, targetEnd = point -1.0 7.0, point -1.0 13.0
    let transform =
        Affine.pointPairSimilarity sourceStart sourceEnd targetStart targetEnd
        |> Result.defaultWith (fun () -> failwith "expected similarity")
    Assert.Equal(targetStart, Affine.point transform sourceStart)
    Assert.Equal(targetEnd, Affine.point transform sourceEnd)

[<Fact>]
let ``point pair similarity rejects a collapsed source pair`` () =
    Assert.Equal(Error(), Affine.pointPairSimilarity (point 1.0 2.0) (point 1.0 2.0) (point 3.0 4.0) (point 5.0 6.0))

[<Fact>]
let ``point triple map maps all three points`` () =
    let sourceA, sourceB, sourceC = point 0.0 0.0, point 2.0 0.0, point 0.0 3.0
    let targetA, targetB, targetC = point 5.0 7.0, point 9.0 9.0, point 2.0 13.0
    let transform =
        Affine.pointTripleMap sourceA sourceB sourceC targetA targetB targetC
        |> Result.defaultWith (fun () -> failwith "expected affine map")
    Assert.Equal(targetA, Affine.point transform sourceA)
    Assert.Equal(targetB, Affine.point transform sourceB)
    Assert.Equal(targetC, Affine.point transform sourceC)

[<Fact>]
let ``point triple map rejects collinear source points`` () =
    let result =
        Affine.pointTripleMap
            (point 0.0 0.0)
            (point 1.0 1.0)
            (point 2.0 2.0)
            (point 0.0 0.0)
            (point 1.0 0.0)
            (point 0.0 1.0)
    Assert.Equal(Error(), result)

[<Fact>]
let ``finiteness includes length-valued translations`` () =
    Assert.True(Affine.isFinite (Affine.identity ()))
    Assert.False(Affine.isFinite (Affine.translate (Length.fromFloat infinity) 0.0<length>))
