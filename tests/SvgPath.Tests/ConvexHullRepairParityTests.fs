module SvgPath.Tests.ConvexHullRepairParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private pointLoop value = [ Line(value, value); Line(value, value) ]

let private bigLineLoop () =
    let startPoint, endPoint = point 1000.0 0.0, point 999.84769516 17.45240644
    [ Line(startPoint, endPoint); Line(endPoint, startPoint) ]

let private smallArc () =
    Arc
        { Start = point 999.94340504 7.63106966
          Radius = point 30.0 30.0
          XAxisRotation = 0.0<degree>
          LargeArc = false
          Sweep = true
          End = point 999.92428935 9.82151131 }

let private smallArcLoop () =
    let arc = smallArc ()
    [ arc; Line(Segment.finish arc, Segment.start arc) ]

let private lineArcProbePath () =
    Path.ofSubpaths
        [ Subpath.ofSegment (List.head (bigLineLoop ()))
          Subpath.ofSegment (smallArc ()) ]

let private assertNearDegree expected actual =
    Assert.True(abs (actual - expected) <= 1.0e-6<degree>)

let private assertProbeEndpointsInside (hull: Subpath) =
    for candidate in [ point 999.94340504 7.63106966; point 999.92428935 9.82151131 ] do
        Assert.Equal(None, ConvexHull.internalPointChordPolygonLoopSeparation hull.Segments candidate)

[<Fact>]
let ``seeded worst direction stays put at local maximum`` () =
    let lower, upper =
        ConvexHull.internalFindSeededWorstDirection
            (pointLoop (point 0.0 0.0)) (pointLoop (point 1.0 0.0)) 0.0<degree> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertNearDegree 0.0<degree> lower
    assertNearDegree 0.0<degree> upper

[<Fact>]
let ``seeded worst direction walks to local maximum`` () =
    let lower, upper =
        ConvexHull.internalFindSeededWorstDirection
            (pointLoop (point 0.0 0.0)) (pointLoop (point 1.0 0.0)) 5.0<degree> 10.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertNearDegree 0.0<degree> lower
    assertNearDegree 0.0<degree> upper

[<Fact>]
let ``seeded worst direction stays within max drift`` () =
    let lower, upper =
        ConvexHull.internalFindSeededWorstDirection
            (pointLoop (point 0.0 0.0)) (pointLoop (point 1.0 0.0)) 5.0<degree> 1.0<degree>
        |> Result.defaultWith (failwithf "%A")
    assertNearDegree 4.0<degree> lower
    assertNearDegree 4.0<degree> upper

[<Fact>]
let ``loop initial sample angles merges sorted seed angles`` () =
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 45.0<degree>; 90.0<degree>; 180.0<degree>; 225.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ 45.0<degree>; 225.0<degree> ])

[<Fact>]
let ``loop initial sample angles normalizes seed angles`` () =
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 45.0<degree>; 90.0<degree>; 180.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ -90.0<degree>; 405.0<degree> ])

[<Fact>]
let ``loop initial sample angles removes near seed angles`` () =
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 45.0<degree>; 90.0<degree>; 180.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ 45.0<degree>; 45.01<degree> ])

[<Fact>]
let ``loop initial sample angles removes wraparound duplicates`` () =
    Assert.Equal<float<degree> list>(
        [ 0.0<degree>; 90.0<degree>; 180.0<degree>; 270.0<degree> ],
        ConvexHull.internalLoopInitialSampleAngles 4 [ -0.0005<degree> ])

[<Fact>]
let ``loop union with seed angles removes zero length endpoint pieces`` () =
    let segments =
        ConvexHull.internalLoopUnionSegmentsWithSeedAngles
            (bigLineLoop ()) (smallArcLoop ()) [ 0.49724434278326146<degree>; 0.5027556573338349<degree> ]
    Assert.Equal(4, segments.Length)
    Assert.All(segments, fun segment -> Assert.NotEqual(Segment.start segment, Segment.finish segment))

[<Fact>]
let ``ambitious repair loop with loop adds tiny arc slice`` () =
    let segments =
        ConvexHull.internalAmbitiousRepairLoopWithLoop (bigLineLoop ()) (smallArcLoop ())
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, segments.Length)
    Assert.Contains(segments, function Arc _ -> true | _ -> false)

[<Fact>]
let ``path hull handles scaled two arc probe`` () =
    let largeArc =
        Arc
            { Start = point 1000.0 0.0
              Radius = point 1000.0 1000.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 999.84769516 17.45240644 }
    let path = Path.ofSubpaths [ Subpath.ofSegment largeArc; Subpath.ofSegment (smallArc ()) ]
    let hull = ConvexHull.pathHull path |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed

[<Fact>]
let ``path hull with dumb repair mode handles line arc probe`` () =
    let hull =
        ConvexHull.internalPathHullWithRepairMode (lineArcProbePath ()) "dumb"
        |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    assertProbeEndpointsInside hull

[<Fact>]
let ``path hull with ambitious repair mode handles line arc probe`` () =
    let hull =
        ConvexHull.internalPathHullWithRepairMode (lineArcProbePath ()) "ambitious"
        |> Result.defaultWith (failwithf "%A")
    Assert.True hull.Closed
    assertProbeEndpointsInside hull
    Assert.Contains(hull.Segments, function Arc _ -> true | _ -> false)
