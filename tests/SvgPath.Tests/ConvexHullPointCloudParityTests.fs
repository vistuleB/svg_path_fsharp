module SvgPath.Tests.ConvexHullPointCloudParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private supportValueForPoints points angle =
    let direction = Point.direction (Degree.fromFloat angle)
    points |> List.map (fun candidate -> Point.dot candidate direction) |> List.max

let private supportValueForSegments segments angle =
    segments
    |> List.map (fun segment ->
        ConvexHull.internalSegmentSupport segment (Degree.fromFloat angle)
        |> Result.defaultWith (failwithf "%A")
        |> fun (_, _, value) -> value)
    |> List.max

let private assertValidHull points (hull: Subpath) =
    Assert.True hull.Closed
    for candidate in points do
        Assert.Equal(None, ConvexHull.internalPointChordPolygonLoopSeparation hull.Segments candidate)
    for angle in [ 0.0 .. 10.0 .. 350.0 ] do
        Assert.True(abs (supportValueForPoints points angle - supportValueForSegments hull.Segments angle) <= 1.0e-6<length>)

let private assertValidInAllModes points =
    ConvexHull.pointsHull points
    |> Result.defaultWith (failwithf "%A")
    |> assertValidHull points
    let path = points |> List.map Subpath.empty |> Path.ofSubpaths
    for mode in [ "dumb"; "ambitious" ] do
        ConvexHull.internalPathHullWithRepairMode path mode
        |> Result.defaultWith (failwithf "%A")
        |> assertValidHull points

let private randomPoint index =
    point
        (float (((index * 73 + 19) * (index * 17 + 23) + 11) % 10001) / 100.0)
        (float (((index * 41 + 29) * (index * 97 + 31) + 7) % 10001) / 100.0)

let private randomPoints count = [ 0 .. count - 1 ] |> List.map randomPoint

[<Fact>]
let ``point cloud hull handles 10 point cloud`` () =
    assertValidInAllModes (randomPoints 10)

[<Fact>]
let ``point cloud hull rejects empty point cloud`` () =
    Assert.Equal(Error(ConvexHullPathError EmptyPath), ConvexHull.pointsHull [])

[<Fact>]
let ``point cloud hull handles points`` () =
    let points = [ point -2.0 1.0; point 5.0 1.0; point 0.0 4.0; point 1.0 2.0 ]
    ConvexHull.pointsHull points
    |> Result.defaultWith (failwithf "%A")
    |> assertValidHull points

[<Fact>]
let ``point cloud hull handles single point cloud`` () =
    assertValidInAllModes [ point 4.0 -3.0 ]

[<Fact>]
let ``point cloud hull handles duplicate single point cloud`` () =
    let candidate = point 4.0 -3.0
    assertValidInAllModes [ candidate; candidate; candidate ]

[<Fact>]
let ``point cloud hull handles two point cloud`` () =
    assertValidInAllModes [ point -2.0 1.0; point 5.0 1.0 ]

[<Fact>]
let ``point cloud hull handles duplicate two point cloud`` () =
    let a, b = point -2.0 1.0, point 5.0 1.0
    assertValidInAllModes [ a; b; a; b; a ]

[<Fact>]
let ``point cloud hull handles horizontal collinear point cloud`` () =
    assertValidInAllModes [ point -2.0 1.0; point 0.0 1.0; point 3.0 1.0; point 5.0 1.0 ]

[<Fact>]
let ``point cloud hull handles vertical collinear point cloud`` () =
    assertValidInAllModes [ point 2.0 -3.0; point 2.0 -1.0; point 2.0 4.0; point 2.0 8.0 ]

[<Fact>]
let ``point cloud hull handles positive diagonal collinear point cloud`` () =
    assertValidInAllModes [ point -2.0 -1.0; point 0.0 1.0; point 3.0 4.0; point 5.0 6.0 ]

[<Fact>]
let ``point cloud hull handles negative diagonal collinear point cloud`` () =
    assertValidInAllModes [ point -2.0 6.0; point 0.0 4.0; point 3.0 1.0; point 5.0 -1.0 ]

[<Fact>]
let ``point cloud hull handles duplicate collinear point cloud`` () =
    let a, b, c, d = point -2.0 -1.0, point 0.0 1.0, point 3.0 4.0, point 5.0 6.0
    assertValidInAllModes [ b; a; c; b; d; a; c ]

[<Fact>]
let ``point cloud hull handles 100 point cloud`` () =
    assertValidInAllModes (randomPoints 100)
