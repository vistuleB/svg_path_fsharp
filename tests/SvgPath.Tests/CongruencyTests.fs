module SvgPath.Tests.CongruencyTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private tolerance = { Distance = 1.0e-6<length>; Angle = 1.0e-6<degree> }
let private near expected actual = abs (expected - actual) <= 1.0e-6<length>

[<Fact>]
let ``segment rejects different constructors`` () =
    let line = Line(point 0.0 0.0, point 10.0 0.0)
    let quadratic = QuadraticBezier(point 0.0 0.0, point 5.0 5.0, point 10.0 0.0)
    Assert.Equal(Error(), Congruency.segmentWith line quadratic tolerance)

[<Fact>]
let ``semantic congruency rejects invalid angle tolerance`` () =
    let line = Line(point 0.0 0.0, point 1.0 0.0)
    Assert.Equal(Error(), Congruency.segmentWith line line { tolerance with Angle = -1.0<degree> })
    Assert.True(Congruency.segmentWith line line { tolerance with Angle = 0.0<degree> } |> Result.isOk)

[<Fact>]
let ``subpath ignores closed field`` () =
    let openSubpath =
        Subpath.create
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 0.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let closedSubpath = Subpath.setClosed true openSubpath |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.subpathWith openSubpath closedSubpath tolerance |> Result.isOk)

[<Fact>]
let ``points rejects empty lists`` () =
    Assert.Equal(Error(), Congruency.points [] [] 1.0e-6<length>)

[<Fact>]
let ``points rejects different length lists`` () =
    Assert.Equal(Error(), Congruency.points [ point 0.0 0.0 ] [ point 1.0 1.0; point 2.0 2.0 ] 1.0e-6<length>)

[<Fact>]
let ``points maps single points with translation`` () =
    let transform = Congruency.points [ point 2.0 3.0 ] [ point 7.0 11.0 ] 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> (Affine.point transform (point 2.0 3.0)) (point 7.0 11.0))

[<Fact>]
let ``points maps collapsed source to collapsed target`` () =
    Assert.True(
        Congruency.points
            [ point 2.0 3.0; point 2.0 3.0; point 2.0 3.0 ]
            [ point 7.0 11.0; point 7.0 11.0; point 7.0 11.0 ]
            1.0e-6<length>
        |> Result.isOk)

[<Fact>]
let ``points rejects collapsed source to spread target`` () =
    Assert.Equal(
        Error(),
        Congruency.points
            [ point 2.0 3.0; point 2.0 3.0; point 2.0 3.0 ]
            [ point 7.0 11.0; point 8.0 11.0; point 7.0 12.0 ]
            1.0e-6<length>)

[<Fact>]
let ``points checks all ordered points`` () =
    let source = [ point 0.0 0.0; point 1.0 1.0; point 10.0 0.0; point 2.0 3.0 ]
    let target = [ point 5.0 7.0; point 3.0 9.0; point 5.0 27.0; point -1.0 11.0 ]
    let wrongOrder = [ point 5.0 7.0; point -1.0 11.0; point 5.0 27.0; point 3.0 9.0 ]
    Assert.True(Congruency.points source target 1.0e-6<length> |> Result.isOk)
    Assert.Equal(Error(), Congruency.points source wrongOrder 1.0e-6<length>)

[<Fact>]
let ``points maps long ordered point list`` () =
    let source = [ 0 .. 1499 ] |> List.map (fun index -> point (float index) (float ((index * index) % 17)))
    let target = source |> List.map (fun p -> point (10.0 - 2.0 * Length.toFloat p.Y) (-3.0 + 2.0 * Length.toFloat p.X))
    let transform = Congruency.points source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> (Affine.point transform source.Head) target.Head)

[<Fact>]
let ``points match identical large finite coordinates`` () =
    let points = [ point -1.0e200 0.0; point 1.0e200 0.0; point 0.0 1.0e200 ]
    let transform = Congruency.points points points 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal((1.0, 0.0, 0.0, 1.0, 0.0<length>, 0.0<length>), Affine.toTuple transform)

[<Fact>]
let ``fit points with similar returns rms error`` () =
    let source = [ point 0.0 0.0; point 10.0 0.0; point 0.0 10.0; point 10.0 10.0 ]
    let exact = Affine.matrix 0.0 2.0 -2.0 0.0 5.0<length> 7.0<length>
    let target = source |> List.map (Affine.point exact) |> List.mapi (fun index p -> if index = 3 then point -14.0 35.0 else p)
    let fit = Congruency.fitPoints source target Similar |> Result.defaultWith (failwithf "%A")
    Assert.True(fit.Error > 0.0<length> && fit.Error < 3.0<length>)
    Assert.True((Affine.point fit.Transform (point 0.0 0.0)).X > 4.0<length>)

[<Fact>]
let ``fit points with affine maps square to parallelogram`` () =
    let source = [ point 0.0 0.0; point 1.0 0.0; point 0.0 1.0; point 1.0 1.0 ]
    let expected = Affine.matrix 3.0 1.0 2.0 5.0 7.0<length> 11.0<length>
    let target = source |> List.map (Affine.point expected)
    let affine = Congruency.fitPoints source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    let similar = Congruency.fitPoints source target Similar |> Result.defaultWith (failwithf "%A")
    Assert.True(near 0.0<length> affine.Error)
    Assert.True(similar.Error > 0.5<length>)
    Assert.Equal(Affine.toTuple expected, Affine.toTuple affine.Transform)

[<Fact>]
let ``affine fit is independent of coordinate scale`` () =
    let source = [ point 0.0 0.0; point 0.0001 0.0; point 0.0 0.0001 ]
    let expected = Affine.matrix 2.0 0.0 1.0 1.0 0.0<length> 0.0<length>
    let target = source |> List.map (Affine.point expected)
    let fit = Congruency.fitPoints source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    Assert.True(near 0.0<length> fit.Error)
    Assert.Equal(Affine.toTuple expected, Affine.toTuple fit.Transform)

[<Fact>]
let ``fit points with affine falls back to similar for collinear source`` () =
    let source = [ point 0.0 0.0; point 1.0 0.0; point 2.0 0.0 ]
    let target = [ point 5.0 5.0; point 7.0 5.0; point 9.0 5.0 ]
    let affine = Congruency.fitPoints source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    let similar = Congruency.fitPoints source target Similar |> Result.defaultWith (failwithf "%A")
    Assert.True(near 0.0<length> affine.Error && near 0.0<length> similar.Error)
    Assert.Equal(Affine.toTuple similar.Transform, Affine.toTuple affine.Transform)

[<Fact>]
let ``fit points rejects empty and mismatched lists`` () =
    Assert.Equal(Error(), Congruency.fitPoints [] [] TransformFamily.Affine)
    Assert.Equal(Error(), Congruency.fitPoints [ point 0.0 0.0 ] [] Similar)

[<Fact>]
let ``fit points centroids do not overflow on large finite points`` () =
    let points = [ point 1.0e308 1.0e308; point 1.0e308 1.0e308 ]
    let fit = Congruency.fitPoints points points Similar |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0.0<length>, fit.Error)
    Assert.Equal(Affine.toTuple (Affine.identity ()), Affine.toTuple fit.Transform)

[<Fact>]
let ``fit points rms error does not overflow on large finite residuals`` () =
    let source = [ point 0.0 0.0; point 0.0 0.0 ]
    let target = [ point -1.0e200 0.0; point 1.0e200 0.0 ]
    let fit = Congruency.fitPoints source target Similar |> Result.defaultWith (failwithf "%A")
    Assert.Equal(1.0e200<length>, fit.Error)

[<Fact>]
let ``line returns transform mapping source to target`` () =
    let source = Line(point 0.0 0.0, point 10.0 0.0)
    let target = Line(point 3.0 4.0, point 3.0 24.0)
    let transform = Congruency.segment source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(target, Transform.segment source transform |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``fit segment rejects different constructors`` () =
    let source = Line(point 0.0 0.0, point 10.0 0.0)
    let target = QuadraticBezier(point 0.0 0.0, point 5.0 5.0, point 10.0 0.0)
    Assert.Equal(Error(), Congruency.fitSegment source target TransformFamily.Affine)

[<Fact>]
let ``line congruency allows zero scale directionally`` () =
    let source = Line(point 0.0 0.0, point 10.0 0.0)
    let target = Line(point 5.0 5.0, point 5.0 5.0)
    Assert.True(Congruency.segment source target 1.0e-6<length> |> Result.isOk)
    Assert.Equal(Error(), Congruency.segment target source 1.0e-6<length>)

[<Fact>]
let ``quadratic uses control points in final check`` () =
    let source = QuadraticBezier(point 0.0 0.0, point 5.0 10.0, point 10.0 0.0)
    let target = QuadraticBezier(point 10.0 20.0, point -10.0 30.0, point 10.0 40.0)
    let wrong = QuadraticBezier(point 10.0 20.0, point -9.0 30.0, point 10.0 40.0)
    Assert.True(Congruency.segment source target 1.0e-6<length> |> Result.isOk)
    Assert.Equal(Error(), Congruency.segment source wrong 1.0e-6<length>)

[<Fact>]
let ``cubic returns transform mapping source to target`` () =
    let source = CubicBezier(point 0.0 0.0, point 2.0 8.0, point 8.0 8.0, point 10.0 0.0)
    let expected = Affine.matrix 0.0 2.0 -2.0 0.0 12.0<length> -3.0<length>
    let target = Transform.segment source expected |> Result.defaultWith (failwithf "%A")
    let found = Congruency.segment source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(target, Transform.segment source found |> Result.defaultWith (failwithf "%A"))

let private ellipseArc radii rotation largeArc sweep =
    Arc { Start = point 0.0 0.0; Radius = radii; XAxisRotation = rotation; LargeArc = largeArc; Sweep = sweep; End = point 20.0 0.0 }

[<Fact>]
let ``arc returns transform mapping source to target`` () =
    let source = ellipseArc (point 10.0 5.0) 30.0<degree> false true
    let expected = Affine.matrix 1.0606601717798214 1.0606601717798212 -1.0606601717798212 1.0606601717798214 3.0<length> -7.0<length>
    let target = Transform.segment source expected |> Result.defaultWith (failwithf "%A")
    let found = Congruency.segment source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    let mapped = Transform.segment source found |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.segment mapped target 1.0e-6<length> |> Result.isOk)

[<Fact>]
let ``arc rejects mismatched flags`` () =
    let source = ellipseArc (point 10.0 10.0) 0.0<degree> false true
    let target = ellipseArc (point 10.0 10.0) 0.0<degree> false false
    Assert.Equal(Error(), Congruency.segment source target 1.0e-6<length>)

[<Fact>]
let ``arc accepts equivalent axis representations`` () =
    let source = ellipseArc (point 10.0 5.0) 30.0<degree> false true
    let fullTurn = ellipseArc (point 10.0 5.0) 390.0<degree> false true
    let reversedAxes = ellipseArc (point 5.0 10.0) 120.0<degree> false true
    Assert.True(Congruency.segment source fullTurn 1.0e-6<length> |> Result.isOk)
    Assert.True(Congruency.segment source reversedAxes 1.0e-6<length> |> Result.isOk)

[<Fact>]
let ``circular arc ignores axis rotation`` () =
    let source = ellipseArc (point 10.0 10.0) 0.0<degree> false true
    let target = ellipseArc (point 10.0 10.0) 47.0<degree> false true
    Assert.True(Congruency.segment source target 1.0e-6<length> |> Result.isOk)

let private mixedSubpath () =
    Subpath.create
        [ Line(point 0.0 0.0, point 10.0 0.0)
          QuadraticBezier(point 10.0 0.0, point 15.0 5.0, point 20.0 0.0) ]
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``subpath maps ordered segments to target`` () =
    let source = mixedSubpath ()
    let expected = Affine.matrix 0.0 2.0 -2.0 0.0 3.0<length> 4.0<length>
    let target = Transform.subpath source expected |> Result.defaultWith (failwithf "%A")
    let found = Congruency.subpath source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(target, Transform.subpath source found |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``fit subpath with affine uses semantic point cloud`` () =
    let source = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let expected = Affine.matrix 2.0 1.0 0.5 3.0 -4.0<length> 8.0<length>
    let target = Transform.subpath source expected |> Result.defaultWith (failwithf "%A")
    let fit = Congruency.fitSubpath source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    Assert.True(near 0.0<length> fit.Error)
    let mapped = Transform.subpath source fit.Transform |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.subpathWith mapped target tolerance |> Result.isOk)

[<Fact>]
let ``subpath maps move-only subpaths`` () =
    let source = Subpath.empty (point 1.0 2.0)
    let target = Subpath.empty (point 6.0 8.0) |> Subpath.setClosed true |> Result.defaultWith (failwithf "%A")
    let found = Congruency.subpath source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.True(Point.near 1.0e-6<length> (Affine.point found source.Start) target.Start)

[<Fact>]
let ``subpath rejects different segment constructors`` () =
    let source = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    let target = Subpath.ofSegment (QuadraticBezier(point 0.0 0.0, point 5.0 5.0, point 10.0 0.0))
    Assert.Equal(Error(), Congruency.subpath source target 1.0e-6<length>)

[<Fact>]
let ``subpath does not cycle segments`` () =
    let source = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 20.0 0.0; point 30.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let target =
        Subpath.create [ Line(point 10.0 0.0, point 20.0 0.0); Line(point 20.0 0.0, point 30.0 0.0); Line(point 30.0 0.0, point 0.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Error(), Congruency.subpath source target 1.0e-6<length>)

[<Fact>]
let ``subpath rejects arc field mismatch after points match`` () =
    let source = Subpath.ofSegment (ellipseArc (point 10.0 10.0) 0.0<degree> false true)
    let target = Subpath.ofSegment (ellipseArc (point 12.0 12.0) 0.0<degree> false true)
    Assert.Equal(Error(), Congruency.subpath source target 1.0e-6<length>)

let private twoLines () =
    Path.ofSubpaths
        [ Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
          Subpath.ofSegment (Line(point 0.0 10.0, point 10.0 10.0)) ]

[<Fact>]
let ``path uses one transform across subpaths`` () =
    let source = twoLines ()
    let expected = Affine.matrix 2.0 0.0 0.0 2.0 5.0<length> 5.0<length>
    let target = Transform.path source expected |> Result.defaultWith (failwithf "%A")
    let found = Congruency.path source target 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
    Assert.Equal(target, Transform.path source found |> Result.defaultWith (failwithf "%A"))

[<Fact>]
let ``fit path with affine uses one transform across subpaths`` () =
    let source = twoLines ()
    let expected = Affine.matrix 2.0 1.0 -0.5 3.0 7.0<length> -2.0<length>
    let target = Transform.path source expected |> Result.defaultWith (failwithf "%A")
    let fit = Congruency.fitPath source target TransformFamily.Affine |> Result.defaultWith (failwithf "%A")
    Assert.True(near 0.0<length> fit.Error)
    let mapped = Transform.path source fit.Transform |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.pathWith mapped target tolerance |> Result.isOk)

[<Fact>]
let ``path recognizes transformed mixed fixture`` () =
    let source =
        Path.singleton (
            Subpath.create
                [ Line(point 0.0 0.0, point 12.0 0.0)
                  QuadraticBezier(point 12.0 0.0, point 18.0 8.0, point 24.0 0.0)
                  CubicBezier(point 24.0 0.0, point 30.0 -8.0, point 36.0 8.0, point 42.0 0.0)
                  Arc
                      { Start = point 42.0 0.0
                        Radius = point 6.0 10.0
                        XAxisRotation = 0.0<degree>
                        LargeArc = false
                        Sweep = false
                        End = point 50.0 0.0 } ]
            |> Result.defaultWith (failwithf "%A"))
    let expected = Affine.matrix 1.3972614213376766 1.053176941516799 -1.053176941516799 1.3972614213376766 17.0<length> -9.0<length>
    let target = Transform.path source expected |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.path source target 1.0e-6<length> |> Result.isOk)

[<Fact>]
let ``path recognizes transformed multi-subpath fixture`` () =
    let first =
        Subpath.create
            [ Line(point 0.0 0.0, point 20.0 0.0)
              Line(point 20.0 0.0, point 20.0 20.0)
              Line(point 20.0 20.0, point 0.0 20.0)
              Line(point 0.0 20.0, point 0.0 0.0) ]
        |> Result.defaultWith (failwithf "%A")
    let second =
        Subpath.create
            [ Line(point 30.0 30.0, point 40.0 30.0)
              Line(point 40.0 30.0, point 40.0 40.0) ]
        |> Result.defaultWith (failwithf "%A")
    let source = Path.ofSubpaths [ first; second ]
    let angle = -82.0 * System.Math.PI / 180.0
    let expected = Affine.matrix (0.6 * cos angle) (0.6 * sin angle) (-0.6 * sin angle) (0.6 * cos angle) -24.0<length> 11.0<length>
    let target = Transform.path source expected |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.path source target 1.0e-6<length> |> Result.isOk)

[<Fact>]
let ``path rejects individually congruent but globally inconsistent subpaths`` () =
    let source = twoLines ()
    let target =
        Path.ofSubpaths
            [ Subpath.ofSegment (Line(point 5.0 5.0, point 25.0 5.0))
              Subpath.ofSegment (Line(point 100.0 25.0, point 120.0 25.0)) ]
    Assert.Equal(Error(), Congruency.path source target 1.0e-6<length>)

[<Fact>]
let ``path ignores subpath closed fields`` () =
    let openSubpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 0.0 0.0 ] |> Result.defaultWith (failwithf "%A")
    let closed = Subpath.setClosed true openSubpath |> Result.defaultWith (failwithf "%A")
    Assert.True(Congruency.path (Path.singleton openSubpath) (Path.singleton closed) 1.0e-6<length> |> Result.isOk)

[<Fact>]
let ``path rejects different subpath counts`` () =
    let subpath = Subpath.ofSegment (Line(point 0.0 0.0, point 10.0 0.0))
    Assert.Equal(Error(), Congruency.path (Path.singleton subpath) (Path.ofSubpaths [ subpath; subpath ]) 1.0e-6<length>)

[<Fact>]
let ``path rejects arc field mismatch after points match`` () =
    let source = Path.singleton (Subpath.ofSegment (ellipseArc (point 10.0 10.0) 0.0<degree> false true))
    let target = Path.singleton (Subpath.ofSegment (ellipseArc (point 12.0 12.0) 0.0<degree> false true))
    Assert.Equal(Error(), Congruency.path source target 1.0e-6<length>)
