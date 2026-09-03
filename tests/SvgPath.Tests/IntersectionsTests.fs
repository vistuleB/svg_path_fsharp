module SvgPath.Tests.IntersectionsTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)

let private assertParameterNear expected actual tolerance =
    Assert.True(abs (actual - expected) <= Parameter.fromFloat tolerance, $"expected {expected}, got {actual}")

let private arcPair () =
    let left =
        Arc
            { Start = point 82.60920101224798 220.34092587189474
              Radius = point 20.01 20.01
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 43.21295323581002 213.39430445023285 }
    let right =
        Arc
            { Start = point 43.190371867436326 213.5338826899446
              Radius = point 210.0 210.0
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = true
              End = point 454.61771360489934 202.76027858778826 }
    left, right

[<Fact>]
let ``arc arc crossing regression`` () =
    let left, right = arcPair ()
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A")
    Assert.Single(found) |> ignore

[<Fact>]
let ``production arc arc crossing regression both orders`` () =
    let left, right = arcPair ()
    let forward = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let backward = Intersections.segment right left |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear forward.LeftT backward.RightT 1.0e-6
    assertParameterNear forward.RightT backward.LeftT 1.0e-6

[<Fact>]
let ``cubic cubic crossing regression`` () =
    let leftArc, rightArc = arcPair ()
    let left = Segment.arcsToCubicBeziers leftArc |> List.last
    let right = Segment.arcsToCubicBeziers rightArc |> List.head
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A")
    Assert.Single(found) |> ignore

[<Fact>]
let ``symmetric kissing quadratics regression`` () =
    let upper = QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0)
    let lower = QuadraticBezier(point -1.0 -1.0, point 0.0 1.0, point 1.0 -1.0)
    let intersection = Intersections.segment upper lower |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``production symmetric kissing quadratics`` () =
    let upper = QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0)
    let lower = QuadraticBezier(point -1.0 -1.0, point 0.0 1.0, point 1.0 -1.0)
    let intersection = Intersections.segment upper lower |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-7
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-7

[<Fact>]
let ``flat cubic crossing regression`` () =
    let rising = CubicBezier(point 0.0 -0.125, point (1.0 / 3.0) 0.125, point (2.0 / 3.0) -0.125, point 1.0 0.125)
    let falling = CubicBezier(point 0.0 0.125, point (1.0 / 3.0) -0.125, point (2.0 / 3.0) 0.125, point 1.0 -0.125)
    let intersection = Intersections.segment rising falling |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``disjoint quadratics regression`` () =
    let upper = QuadraticBezier(point -1.0 2.0, point 0.0 1.0, point 1.0 2.0)
    let lower = QuadraticBezier(point -1.0 -2.0, point 0.0 -1.0, point 1.0 -2.0)
    Assert.Equal(Ok [], Intersections.segment upper lower)

[<Fact>]
let ``production off center kissing quadratics`` () =
    let left = QuadraticBezier(point 0.0 0.1369, point 0.5 -0.2331, point 1.0 0.3969)
    let right = QuadraticBezier(point -0.26 -0.3969, point 0.24 0.2331, point 0.74 -0.1369)
    let intersection = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.37<parameter> intersection.LeftT 2.0e-5
    assertParameterNear 0.63<parameter> intersection.RightT 2.0e-5

[<Fact>]
let ``production two close quadratic crossings`` () =
    let axis = QuadraticBezier(point 0.0 0.0, point 0.5 0.0, point 1.0 0.0)
    let curve = QuadraticBezier(point 0.0 0.24999999, point 0.5 -0.25000001, point 1.0 0.24999999)
    let found = Intersections.segment axis curve |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``adjacent quadratic crossing regression`` () =
    let previous = QuadraticBezier(point 0.0 0.0, point 0.5 2.0, point 1.0 0.0)
    let next = QuadraticBezier(point 1.0 0.0, point 0.5 -1.0, point 0.0 1.0)
    let found = Intersections.segment previous next |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``window guard fallback regression`` () =
    let previous =
        CubicBezier(
            point 3.7326908112839723 3.0798423879604986,
            point 3.7326908112839723 3.0798423879604986,
            point 3.732979794442744 3.079321742790136,
            point 3.7326907230069644 3.079843411893102)
    let next =
        CubicBezier(
            point 3.7326907230069644 3.079843411893102,
            point 3.7867214554038764 2.9802982971613803,
            point 3.813617440000867 2.8517024128302615,
            point 3.8148384145946803 2.834959380847173)
    let found = Intersections.segment previous next |> Result.defaultWith (failwithf "%A")
    Assert.True(List.length found >= 1)

[<Fact>]
let ``invalid intersection options are rejected`` () =
    let line = Line(point 0.0 0.0, point 1.0 1.0)
    Assert.Equal(Error(InvalidIntersectionTolerance -1.0e-9<length>), Intersections.segmentWith line line { Intersections.defaultOptions with Tolerance = -1.0e-9<length> })
    Assert.Equal(Error(InvalidIntersectionMaxDepth 0), Intersections.segmentWith line line { Intersections.defaultOptions with MaxDepth = 0 })

[<Fact>]
let ``overlapping segments are rejected`` () =
    let line = Line(point 0.0 0.0, point 1.0 1.0)
    Assert.Equal(Error OverlappingSegments, Intersections.segment line line)

[<Fact>]
let ``near parallel line projection is scale invariant`` () =
    let left = Line(point -1.0 -1.0e-8, point 1.0 1.0e-8)
    let right = Line(point -1.0 1.0e-8, point 1.0 -1.0e-8)
    for scale in [ 1.0e-6; 1.0; 1.0e6 ] do
        let scaled segmentValue =
            Transform.scaleSegment segmentValue scale
            |> Result.defaultWith (failwithf "%A")
        let projection =
            Intersections.segmentSegmentProjectionWith (scaled left) (scaled right)
                { Intersections.defaultOptions with Tolerance = 1.0e-12<length> * scale }
            |> Result.defaultWith (failwithf "%A")
        assertParameterNear 0.5<parameter> projection.LeftT 1.0e-6
        assertParameterNear 0.5<parameter> projection.RightT 1.0e-6
        Assert.Equal(0.0<length>, projection.Distance)

[<Fact>]
let ``segment projection reports crossing separated and overlapping lines`` () =
    let horizontal = Line(point 0.0 0.0, point 10.0 0.0)
    let crossing = Line(point 5.0 -2.0, point 5.0 2.0)
    let crossed = Intersections.segmentSegmentProjection horizontal crossing |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0.0<length>, crossed.Distance)
    assertParameterNear 0.5<parameter> crossed.LeftT 1.0e-9
    assertParameterNear 0.5<parameter> crossed.RightT 1.0e-9

    let separated = Line(point 2.0 3.0, point 8.0 3.0)
    let nearest = Intersections.segmentSegmentProjection horizontal separated |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3.0<length>, nearest.Distance)
    Assert.Equal(nearest.LeftPoint.X, nearest.RightPoint.X)

    let overlapping = Line(point 3.0 0.0, point 7.0 0.0)
    let overlap = Intersections.segmentSegmentProjection horizontal overlapping |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0.0<length>, overlap.Distance)

[<Fact>]
let ``line intersections cover crossing endpoint disjoint and point contact`` () =
    let horizontal = Line(point 0.0 0.0, point 10.0 0.0)
    let vertical = Line(point 5.0 -1.0, point 5.0 1.0)
    let crossing = Intersections.segment horizontal vertical |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> crossing.LeftT 1.0e-9
    assertParameterNear 0.5<parameter> crossing.RightT 1.0e-9

    let endpoint = Line(point 10.0 0.0, point 10.0 5.0)
    let contact = Intersections.segment horizontal endpoint |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.Equal(1.0<parameter>, contact.LeftT)
    Assert.Equal(0.0<parameter>, contact.RightT)

    Assert.Equal(Ok [], Intersections.segment horizontal (Line(point 0.0 2.0, point 10.0 2.0)))
    let pointContact = Line(point 10.0 0.0, point 12.0 0.0)
    Assert.Single(Intersections.segment horizontal pointContact |> Result.defaultWith (failwithf "%A")) |> ignore

let private subpath segments = Subpath.create segments |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``segment self intersections finds cubic crossing`` () =
    let curve =
        CubicBezier(
            point 0.0 0.0,
            point -0.2708333333333333 -0.3333333333333333,
            point -0.5416666666666666 -0.3333333333333333,
            point 0.1875 0.0)
    let intersection = Intersections.segmentSelf curve |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.25<parameter> intersection.LeftT 1.0e-6
    assertParameterNear 0.75<parameter> intersection.RightT 1.0e-6

    Assert.True(Point.near 1.0e-6<length> intersection.Point (Segment.point curve 0.25<parameter> |> Result.defaultWith (failwithf "%A")))

[<Fact>]
let ``segment self intersections reports same endpoint arc`` () =
    let arc =
        Arc
            { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 0.0 0.0 }
    Assert.Equal(
        Ok [ { LeftT = 0.0<parameter>; RightT = 1.0<parameter>; Point = point 0.0 0.0 } ],
        Intersections.segmentSelf arc)

[<Fact>]
let ``segment self intersections ignores same endpoint zero radius arc`` () =
    let arc =
        Arc
            { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 0.0 0.0 }
    Assert.Equal(Ok [], Intersections.segmentSelf arc)

[<Fact>]
let ``subpath self intersections finds line crossing`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 10.0 0.0) ]
    let intersection = Intersections.subpathSelf value |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = intersection.Parameters
    Assert.Equal(0, first.SegmentIndex)
    Assert.Equal(2, second.SegmentIndex)
    assertParameterNear 0.5<parameter> first.T 1.0e-6
    assertParameterNear 0.5<parameter> second.T 1.0e-6

[<Fact>]
let ``subpath self intersections ignores adjacent segment join`` () =
    let openValue =
        subpath [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
    Assert.Equal(Ok [], Intersections.subpathSelf openValue)

[<Fact>]
let ``subpath self intersections ignores closed endpoint join`` () =
    let closedValue =
        subpath
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Subpath.assertSetClosed true
    Assert.Equal(Ok [], Intersections.subpathSelf closedValue)

[<Fact>]
let ``subpath self intersections rejects overlapping segments`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 8.0 0.0)
              Line(point 8.0 0.0, point 2.0 0.0) ]
    Assert.Equal(Error OverlappingSegments, Intersections.subpathSelf value)

[<Fact>]
let ``subpath self intersections respects minimum arc length separation`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 10.0 0.0) ]
    let options =
        { Intersections.defaultSelfIntersectionOptions with MinimumArcLengthSeparation = 100.0<length> }
    Assert.Equal(Ok [], Intersections.subpathSelfWith value options)

[<Fact>]
let ``subpath self intersections rejects semantic arc overlap`` () =
    let left =
        Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>
              LargeArc = false; Sweep = true; End = point 10.0 0.0 }
    let sameGeometry =
        Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 10.0 0.0 }
    Assert.Equal(Error OverlappingSegments, Intersections.subpathSelf (subpath [ left; Segment.reverse sameGeometry ]))

[<Fact>]
let ``subpath self intersections finds cubic self intersection`` () =
    let curve =
        CubicBezier(
            point 0.0 0.0,
            point -0.2708333333333333 -0.3333333333333333,
            point -0.5416666666666666 -0.3333333333333333,
            point 0.1875 0.0)
    let intersection = Intersections.subpathSelf (subpath [ curve ]) |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = intersection.Parameters
    Assert.Equal(0, first.SegmentIndex)
    Assert.Equal(0, second.SegmentIndex)
    assertParameterNear 0.25<parameter> first.T 1.0e-6
    assertParameterNear 0.75<parameter> second.T 1.0e-6
    Assert.True(Point.near 1.0e-6<length> intersection.Point (Segment.point curve 0.25<parameter> |> Result.defaultWith (failwithf "%A")))

[<Fact>]
let ``subpath self intersections rejects invalid options`` () =
    let value = subpath [ Line(point 0.0 0.0, point 1.0 0.0) ]
    let invalidSeparation: SelfIntersectionOptions =
        { MinimumArcLengthSeparation = 0.0<length>; DistanceTolerance = 1.0e-6<length> }
    let invalidDistance: SelfIntersectionOptions =
        { MinimumArcLengthSeparation = 1.0e-6<length>; DistanceTolerance = 0.0<length> }
    Assert.Equal(Error(InvalidSelfIntersectionMinimumArcLengthSeparation 0.0<length>), Intersections.subpathSelfWith value invalidSeparation)
    Assert.Equal(Error(InvalidSelfIntersectionDistanceTolerance 0.0<length>), Intersections.subpathSelfWith value invalidDistance)

[<Fact>]
let ``path self intersections finds crossing subpaths`` () =
    let horizontal = subpath [ Line(point 0.0 5.0, point 10.0 5.0) ]
    let vertical = subpath [ Line(point 5.0 0.0, point 5.0 10.0) ]
    let intersection = Intersections.pathSelf (Path.ofSubpaths [ horizontal; vertical ]) |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = intersection.Parameters
    Assert.Equal((0, 0), (first.SubpathIndex, first.At.SegmentIndex))
    Assert.Equal((1, 0), (second.SubpathIndex, second.At.SegmentIndex))
    assertParameterNear 0.5<parameter> first.At.T 1.0e-6
    assertParameterNear 0.5<parameter> second.At.T 1.0e-6
    Assert.True(Point.near 1.0e-6<length> intersection.Point (point 5.0 5.0))

[<Fact>]
let ``path self intersections includes single subpath crossings`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 10.0 0.0) ]
    let intersection = Intersections.pathSelf (Path.singleton value) |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let first, second = intersection.Parameters
    Assert.Equal((0, 0), (first.SubpathIndex, first.At.SegmentIndex))
    Assert.Equal((0, 2), (second.SubpathIndex, second.At.SegmentIndex))
    assertParameterNear 0.5<parameter> first.At.T 1.0e-6
    assertParameterNear 0.5<parameter> second.At.T 1.0e-6

[<Fact>]
let ``path self intersections rejects invalid options`` () =
    let options: SelfIntersectionOptions =
        { MinimumArcLengthSeparation = 0.0<length>; DistanceTolerance = 1.0e-6<length> }
    Assert.Equal(
        Error(InvalidSelfIntersectionMinimumArcLengthSeparation 0.0<length>),
        Intersections.pathSelfWith Path.empty options)

[<Fact>]
let ``path self intersections rejects semantic arc overlap`` () =
    let left =
        Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>
              LargeArc = false; Sweep = true; End = point 10.0 0.0 }
    let right =
        Arc { Start = point 0.0 0.0; Radius = point 5.0 5.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 10.0 0.0 }
    let value = Path.ofSubpaths [ subpath [ left ]; subpath [ right ] ]
    Assert.Equal(Error OverlappingSegments, Intersections.pathSelf value)

let private lineSubpath startPoint endPoint = Segment.asSubpath (Line(startPoint, endPoint))
let private at t: SubpathParameter = { SegmentIndex = 0; T = Parameter.fromFloat t }

let private quarterArcCircle center radius startOnRight sweep =
    let right = point (Length.toFloat center.X + radius) (Length.toFloat center.Y)
    let bottom = point (Length.toFloat center.X) (Length.toFloat center.Y + radius)
    let left = point (Length.toFloat center.X - radius) (Length.toFloat center.Y)
    let top = point (Length.toFloat center.X) (Length.toFloat center.Y - radius)
    let points =
        match startOnRight, sweep with
        | true, true -> [ right; bottom; left; top; right ]
        | true, false -> [ right; top; left; bottom; right ]
        | false, true -> [ left; top; right; bottom; left ]
        | false, false -> [ left; bottom; right; top; left ]
    points
    |> List.pairwise
    |> List.map (fun (startPoint, endPoint) ->
        Arc
            { Start = startPoint
              Radius = point radius radius
              XAxisRotation = 0.0<degree>
              LargeArc = false
              Sweep = sweep
              End = endPoint })
    |> Subpath.create
    |> Result.bind (Subpath.setClosed true)
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``transverse lines classify clockwise crossing`` () =
    let left = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    let clockwise = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    match Intersections.classifySubpathIntersection left clockwise (at 0.5) (at 0.5) with
    | Ok(Crossing(Clockwise, apertures)) ->
        Assert.Equal(90.0<degree>, apertures.FirstIncomingToSecondIncoming)
        Assert.Equal(90.0<degree>, apertures.FirstIncomingToSecondOutgoing)
        Assert.Equal(90.0<degree>, apertures.FirstOutgoingToSecondIncoming)
        Assert.Equal(90.0<degree>, apertures.FirstOutgoingToSecondOutgoing)
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``reversing right traversal reverses crossing direction`` () =
    let left = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    let right = lineSubpath (point 0.0 1.0) (point 0.0 -1.0)
    match Intersections.classifySubpathIntersection left right (at 0.5) (at 0.5) with
    | Ok(Crossing(Counterclockwise, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``tangent parabola and line classify touching`` () =
    let parabola =
        subpath [ QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0) ]
    let line = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    match Intersections.classifySubpathIntersection parabola line (at 0.5) (at 0.5) with
    | Ok(Touching(SimilarlyDirected, ClockwiseFromFirstToSecond, ClockwiseFromSecondToFirst, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``tangential cubic crossing has same order on both sides`` () =
    let line = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    let cubic =
        CubicBezier(
            point -1.0 -1.0,
            point -0.3333333333333333 1.0,
            point 0.3333333333333333 -1.0,
            point 1.0 1.0)
        |> Segment.asSubpath
    match Intersections.classifySubpathIntersection line cubic (at 0.5) (at 0.5) with
    | Ok(Touching(SimilarlyDirected, ClockwiseFromFirstToSecond, ClockwiseFromFirstToSecond, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``opposite tangent traversals classify oppositely directed`` () =
    let parabola =
        subpath [ QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0) ]
    let line = lineSubpath (point 1.0 0.0) (point -1.0 0.0)
    match Intersections.classifySubpathIntersection parabola line (at 0.5) (at 0.5) with
    | Ok(Touching(OppositelyDirected, _, _, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``unequal externally kissing quarter arc circles report orders`` () =
    for firstRadius, secondRadius in [ 1.0, 3.0; 3.0, 1.0; 0.25, 8.0 ] do
        let first = quarterArcCircle (point 0.0 0.0) firstRadius true true
        let second = quarterArcCircle (point (firstRadius + secondRadius) 0.0) secondRadius false false
        match Intersections.classifySubpathIntersection first second (at 0.0) (at 0.0) with
        | Ok(Touching(SimilarlyDirected, ClockwiseFromFirstToSecond, ClockwiseFromSecondToFirst, _)) -> ()
        | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``swapping kissing quarter arc arguments reverses orders`` () =
    let firstRadius, secondRadius = 2.0, 5.0
    let first = quarterArcCircle (point 0.0 0.0) firstRadius true true
    let second = quarterArcCircle (point (firstRadius + secondRadius) 0.0) secondRadius false false
    match Intersections.classifySubpathIntersection second first (at 0.0) (at 0.0) with
    | Ok(Touching(_, ClockwiseFromSecondToFirst, ClockwiseFromFirstToSecond, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``oppositely traversed kissing quarter arcs pair geometric sides`` () =
    let firstRadius, secondRadius = 4.0, 1.5
    let first = quarterArcCircle (point 0.0 0.0) firstRadius true true
    let second = quarterArcCircle (point (firstRadius + secondRadius) 0.0) secondRadius false true
    match Intersections.classifySubpathIntersection first second (at 0.0) (at 0.0) with
    | Ok(Touching(OppositelyDirected, ClockwiseFromFirstToSecond, ClockwiseFromSecondToFirst, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``unequal internally kissing quarter arc circles need equal chords`` () =
    for outerRadius, innerRadius in [ 8.0, 1.0; 8.0, 3.0; 100.0, 0.5 ] do
        let outer = quarterArcCircle (point 0.0 0.0) outerRadius true true
        let inner = quarterArcCircle (point (outerRadius - innerRadius) 0.0) innerRadius true true
        match Intersections.classifySubpathIntersection outer inner (at 0.0) (at 0.0) with
        | Ok(Touching(SimilarlyDirected, ClockwiseFromSecondToFirst, ClockwiseFromFirstToSecond, _)) -> ()
        | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``open endpoint to interior is reported before direction topology`` () =
    let first = lineSubpath (point 0.0 0.0) (point 1.0 0.0)
    let second = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    Assert.Equal(
        Ok(EndpointContact(FirstEndpointToSecondInterior StartEndpoint)),
        Intersections.classifySubpathIntersection first second (at 0.0) (at 0.5))

[<Fact>]
let ``directionless interior is indeterminate`` () =
    let origin = point 0.0 0.0
    let left = Subpath.ofSegment (Line(origin, origin))
    let right = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    Assert.Equal(Ok Indeterminate, Intersections.classifySubpathIntersection left right (at 0.5) (at 0.5))

[<Fact>]
let ``grouped intersection expands parameter cartesian product`` () =
    let horizontal = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    let vertical = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    let intersection: SubpathIntersection =
        { Point = point 0.0 0.0
          LeftParameters = [ at 0.25; at 0.75 ]
          RightParameters = [ at 0.25; at 0.75 ] }
    let classified =
        Intersections.classifyGroupedSubpathIntersection horizontal vertical intersection
        |> Result.defaultWith (failwithf "%A")
    Assert.Equal(4, classified.Length)

[<Fact>]
let ``classification rejects out of range angular tolerance`` () =
    let line = lineSubpath (point 0.0 0.0) (point 1.0 0.0)
    let options = { Intersections.defaultClassificationOptions with AngularTolerance = 180.0<degree> }
    Assert.Equal(
        Error(ClassificationError.InvalidAngularTolerance 180.0<degree>),
        Intersections.classifySubpathIntersectionWith line line (at 0.5) (at 0.5) options)

[<Fact>]
let ``subpath intersections canonicalize shared segment boundaries`` () =
    let contact = point 5.0 0.0
    let left =
        subpath [ Line(point 0.0 0.0, contact); Line(contact, point 10.0 0.0) ]
    let right =
        subpath [ Line(point 5.0 -5.0, contact); Line(contact, point 5.0 5.0) ]
    let intersection = Intersections.subpath left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    Assert.True(intersection.LeftParameters = [ { SegmentIndex = 1; T = 0.0<parameter> } ])
    Assert.True(intersection.RightParameters = [ { SegmentIndex = 1; T = 0.0<parameter> } ])

[<Fact>]
let ``path intersections canonicalize near boundary aliases after snapping`` () =
    let middle = point 10.0 0.0
    let left =
        Path.ofSubpaths [ subpath [ Line(point 0.0 0.0, middle); Line(middle, point 20.0 0.0) ] ]
    let right =
        Path.ofSubpaths
            [ subpath [ Line(point 9.9999999999 -5.0, point 9.9999999999 5.0) ]
              subpath [ Line(point 10.0000000001 -5.0, point 10.0000000001 5.0) ] ]
    let result =
        Intersections.pathWith left right
            { Intersections.defaultOptions with
                Tolerance = 1.0e-6<length>
                ParameterSnap = DecimalParameterSnap 7 }
        |> Result.defaultWith (failwithf "%A")
    let intersection = result |> List.exactlyOne
    let expected = [ { SubpathIndex = 0; At = { SegmentIndex = 1; T = 0.0<parameter> } } ]
    Assert.True(intersection.LeftParameters = expected)
    Assert.Equal(2, intersection.RightParameters.Length)
