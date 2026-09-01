module SvgPath.Tests.MarkerTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private basicPose =
    { Kind = MarkerEnd
      Point = point 10.0 20.0
      Angle = 90.0<degree> }

let private basicLayout =
    { Reference = point 0.0 0.0
      MarkerWidth = 10.0<length>
      MarkerHeight = 10.0<length>
      MarkerUnits = UserSpaceOnUse
      StrokeWidth = 1.0<length>
      ViewBox = None
      PreserveAspectRatio = Meet XMidYMid }

[<Fact>]
let ``open polyline has start mid and end poses`` () =
    let subpath =
        Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let poses = Marker.subpathPoses subpath Auto |> Result.defaultWith (failwithf "%A")
    Assert.Equal<MarkerKind list>([ MarkerStart; MarkerMid; MarkerEnd ], poses |> List.map _.Kind)
    Assert.Equal(0.0<degree>, poses[0].Angle)
    Assert.Equal(45.0<degree>, poses[1].Angle)
    Assert.Equal(90.0<degree>, poses[2].Angle)

[<Fact>]
let ``closed polygon start and end use the closing corner`` () =
    let subpath =
        Subpath.polygon [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0; point 0.0 10.0 ]
        |> Result.defaultWith (failwithf "%A")
    let poses = Marker.subpathPoses subpath Auto |> Result.defaultWith (failwithf "%A")
    Assert.Equal(-45.0<degree>, poses[0].Angle)
    Assert.Equal(-45.0<degree>, (List.last poses).Angle)

[<Fact>]
let ``auto start reverse changes only the start pose`` () =
    let subpath = Subpath.polyline [ point 0.0 0.0; point 10.0 0.0; point 10.0 10.0 ] |> Result.defaultWith (failwithf "%A")
    let poses = Marker.subpathPoses subpath AutoStartReverse |> Result.defaultWith (failwithf "%A")
    Assert.Equal< float<degree> list>([ 180.0<degree>; 45.0<degree>; 90.0<degree> ], poses |> List.map _.Angle)

[<Fact>]
let ``singular endpoint direction uses the first noncollapsed handle`` () =
    let segment = CubicBezier(point 0.0 0.0, point 0.0 0.0, point 2.0 0.0, point 2.0 1.0)
    let poses = Marker.subpathPoses (Subpath.ofSegment segment) Auto |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0.0<degree>, poses[0].Angle)

[<Fact>]
let ``orientation searches across collapsed segments`` () =
    let join = point 10.0 0.0
    let subpath =
        Subpath.create [ Line(point 0.0 0.0, join); Line(join, join); Line(join, point 10.0 10.0) ]
        |> Result.defaultWith (failwithf "%A")
    let poses = Marker.subpathPoses subpath Auto |> Result.defaultWith (failwithf "%A")
    Assert.Equal<float<degree> list>([ 0.0<degree>; 45.0<degree>; 45.0<degree>; 90.0<degree> ], poses |> List.map _.Angle)

[<Fact>]
let ``path poses skip move-only subpaths`` () =
    let first = Subpath.ofSegment (Line(point 0.0 0.0, point 1.0 0.0))
    let second = Subpath.polyline [ point 2.0 0.0; point 2.0 1.0; point 3.0 1.0 ] |> Result.defaultWith (failwithf "%A")
    let path = Path.ofSubpaths [ first; Subpath.empty (point 1.5 1.5); second ]
    let poses = Marker.pathPoses path Auto |> Result.defaultWith (failwithf "%A")
    Assert.Equal(5, poses.Length)

[<Fact>]
let ``reference transform places the reference at the pose`` () =
    let matrix = Marker.poseTransformWithReference basicPose (point 2.0 0.0)
    Assert.Equal(basicPose.Point, Affine.point matrix (point 2.0 0.0))

[<Fact>]
let ``view box stretch fills marker viewport`` () =
    let layout =
        { basicLayout with
            MarkerWidth = 20.0<length>
            ViewBox = Some { Min = point 0.0 0.0; Max = point 10.0 10.0 }
            PreserveAspectRatio = Stretch }
    let matrix = Marker.poseLayoutTransform { basicPose with Angle = 0.0<degree> } layout |> Result.defaultWith (failwithf "%A")
    Assert.Equal(point 30.0 30.0, Affine.point matrix (point 10.0 10.0))

[<Fact>]
let ``stroke-width units scale marker content about its reference`` () =
    let layout =
        { basicLayout with
            Reference = point 2.0 0.0
            MarkerUnits = StrokeWidth
            StrokeWidth = 4.0<length> }
    let matrix = Marker.poseLayoutTransform basicPose layout |> Result.defaultWith (failwithf "%A")
    Assert.Equal(basicPose.Point, Affine.point matrix (point 2.0 0.0))
    Assert.Equal(point 10.0 24.0, Affine.point matrix (point 3.0 0.0))

[<Fact>]
let ``invalid marker dimensions are rejected`` () =
    Assert.Equal(Error(InvalidMarkerWidth 0.0<length>), Marker.poseLayoutTransform basicPose { basicLayout with MarkerWidth = 0.0<length> })
    Assert.Equal(Error(InvalidMarkerHeight 0.0<length>), Marker.poseLayoutTransform basicPose { basicLayout with MarkerHeight = 0.0<length> })

[<Fact>]
let ``empty subpath has no marker poses`` () =
    Assert.Equal(Error EmptyMarkerSubpath, Marker.subpathPoses (Subpath.empty (point 0.0 0.0)) Auto)
