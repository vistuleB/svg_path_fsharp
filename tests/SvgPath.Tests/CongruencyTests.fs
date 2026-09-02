module SvgPath.Tests.CongruencyTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private tolerance = { Distance = 1.0e-9<length>; Angle = 1.0e-9<degree> }

[<Fact>]
let ``points recover a similarity transform`` () =
    let source = [ point 0.0 0.0; point 2.0 0.0; point 0.0 1.0 ]
    let expected = Affine.matrix 0.0 2.0 -2.0 0.0 5.0<length> 7.0<length>
    let target = List.map (Affine.point expected) source
    let actual = Congruency.points source target tolerance.Distance |> Result.defaultWith (failwithf "%A")
    Assert.All(List.zip source target, fun (sourcePoint, targetPoint) -> Assert.True(Point.near tolerance.Distance (Affine.point actual sourcePoint) targetPoint))

[<Fact>]
let ``similar fit rejects reflection while affine fit accepts it`` () =
    let source = [ point 0.0 0.0; point 2.0 0.0; point 0.0 1.0 ]
    let target = List.map (fun value -> point (-Length.toFloat value.X) (Length.toFloat value.Y)) source
    let similar = Congruency.fitPoints source target Similar |> Result.defaultWith (failwithf "%A")
    let affine = Congruency.fitPoints source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    Assert.True(similar.Error > 0.1<length>)
    Assert.True(affine.Error < 1.0e-12<length>)
    Assert.True(Affine.determinant affine.Transform < 0.0)

[<Fact>]
let ``segment congruency requires matching constructors`` () =
    let line = Line(point 0.0 0.0, point 2.0 0.0)
    let quadratic = QuadraticBezier(point 0.0 0.0, point 1.0 0.0, point 2.0 0.0)
    Assert.Equal(Error(), Congruency.segmentWith line quadratic tolerance)

[<Fact>]
let ``semantic congruency rejects invalid angle tolerance`` () =
    let line = Line(point 0.0 0.0, point 2.0 0.0)
    Assert.Equal(Error(), Congruency.segmentWith line line { tolerance with Angle = -1.0<degree> })

[<Fact>]
let ``subpath congruency ignores semantic closure`` () =
    let openSubpath = Subpath.empty (point 0.0 0.0)
    let closedSubpath = Subpath.setClosed true openSubpath |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.subpathWith openSubpath closedSubpath tolerance |> Result.isOk)

[<Fact>]
let ``arc congruency preserves sweep semantics`` () =
    let arc sweep =
        Arc
            { Start = point 1.0 0.0
              Radius = point 2.0 1.0
              XAxisRotation = 15.0<degree>
              LargeArc = false
              Sweep = sweep
              End = point -1.0 0.0 }
    Assert.True(Congruency.segmentWith (arc true) (arc true) tolerance |> Result.isOk)
    Assert.Equal(Error(), Congruency.segmentWith (arc true) (arc false) tolerance)
