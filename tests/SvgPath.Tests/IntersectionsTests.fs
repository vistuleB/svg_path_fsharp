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
let ``arc crossing regression is found in either order`` () =
    let left, right = arcPair ()
    let forward = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    let backward = Intersections.segment right left |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear forward.LeftT backward.RightT 1.0e-6
    assertParameterNear forward.RightT backward.LeftT 1.0e-6

[<Fact>]
let ``cubic approximations of the arc regression intersect`` () =
    let leftArc, rightArc = arcPair ()
    let left = Segment.arcsToCubicBeziers leftArc |> List.last
    let right = Segment.arcsToCubicBeziers rightArc |> List.head
    let found = Intersections.segment left right |> Result.defaultWith (failwithf "%A")
    Assert.Single(found) |> ignore

[<Fact>]
let ``symmetric kissing quadratics are found`` () =
    let upper = QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0)
    let lower = QuadraticBezier(point -1.0 -1.0, point 0.0 1.0, point 1.0 -1.0)
    let intersection = Intersections.segment upper lower |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``flat cubic crossing is found`` () =
    let rising = CubicBezier(point 0.0 -0.125, point (1.0 / 3.0) 0.125, point (2.0 / 3.0) -0.125, point 1.0 0.125)
    let falling = CubicBezier(point 0.0 0.125, point (1.0 / 3.0) -0.125, point (2.0 / 3.0) 0.125, point 1.0 -0.125)
    let intersection = Intersections.segment rising falling |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.5<parameter> intersection.LeftT 1.0e-5
    assertParameterNear 0.5<parameter> intersection.RightT 1.0e-5

[<Fact>]
let ``disjoint quadratics return no intersections`` () =
    let upper = QuadraticBezier(point -1.0 2.0, point 0.0 1.0, point 1.0 2.0)
    let lower = QuadraticBezier(point -1.0 -2.0, point 0.0 -1.0, point 1.0 -2.0)
    Assert.Equal(Ok [], Intersections.segment upper lower)

[<Fact>]
let ``off-center kissing quadratics are found`` () =
    let left = QuadraticBezier(point 0.0 0.1369, point 0.5 -0.2331, point 1.0 0.3969)
    let right = QuadraticBezier(point -0.26 -0.3969, point 0.24 0.2331, point 0.74 -0.1369)
    let intersection = Intersections.segment left right |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.37<parameter> intersection.LeftT 2.0e-5
    assertParameterNear 0.63<parameter> intersection.RightT 2.0e-5

[<Fact>]
let ``two close quadratic crossings remain distinct`` () =
    let axis = QuadraticBezier(point 0.0 0.0, point 0.5 0.0, point 1.0 0.0)
    let curve = QuadraticBezier(point 0.0 0.24999999, point 0.5 -0.25000001, point 1.0 0.24999999)
    let found = Intersections.segment axis curve |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``adjacent quadratics include endpoint and crossing`` () =
    let previous = QuadraticBezier(point 0.0 0.0, point 0.5 2.0, point 1.0 0.0)
    let next = QuadraticBezier(point 1.0 0.0, point 0.5 -1.0, point 0.0 1.0)
    let found = Intersections.segment previous next |> Result.defaultWith (failwithf "%A")
    Assert.Equal(2, List.length found)

[<Fact>]
let ``close crossing regression is found`` () =
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
let ``segment self intersection finds cubic crossing and full arc endpoint`` () =
    let curve =
        CubicBezier(
            point 0.0 0.0,
            point -0.2708333333333333 -0.3333333333333333,
            point -0.5416666666666666 -0.3333333333333333,
            point 0.1875 0.0)
    let intersection = Intersections.segmentSelf curve |> Result.defaultWith (failwithf "%A") |> List.exactlyOne
    assertParameterNear 0.25<parameter> intersection.LeftT 1.0e-6
    assertParameterNear 0.75<parameter> intersection.RightT 1.0e-6

    let arc =
        Arc
            { Start = point 0.0 0.0; Radius = point 10.0 10.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 0.0 0.0 }
    Assert.Equal(
        Ok [ { LeftT = 0.0<parameter>; RightT = 1.0<parameter>; Point = point 0.0 0.0 } ],
        Intersections.segmentSelf arc)

[<Fact>]
let ``zero radius closed arc has no self intersection`` () =
    let arc =
        Arc
            { Start = point 0.0 0.0; Radius = point 0.0 10.0; XAxisRotation = 0.0<degree>
              LargeArc = true; Sweep = true; End = point 0.0 0.0 }
    Assert.Equal(Ok [], Intersections.segmentSelf arc)

[<Fact>]
let ``subpath self intersection finds nonadjacent line crossing`` () =
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
let ``subpath self intersection ignores adjacent and closed wrap joins`` () =
    let openValue =
        subpath [ Line(point 0.0 0.0, point 10.0 0.0); Line(point 10.0 0.0, point 10.0 10.0) ]
    Assert.Equal(Ok [], Intersections.subpathSelf openValue)

    let closedValue =
        subpath
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 0.0 0.0) ]
        |> Subpath.assertSetClosed true
    Assert.Equal(Ok [], Intersections.subpathSelf closedValue)

[<Fact>]
let ``subpath self intersection rejects overlapping nonadjacent segments`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 0.0)
              Line(point 10.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 8.0 0.0)
              Line(point 8.0 0.0, point 2.0 0.0) ]
    Assert.Equal(Error OverlappingSegments, Intersections.subpathSelf value)

[<Fact>]
let ``subpath self intersection honors arc length separation`` () =
    let value =
        subpath
            [ Line(point 0.0 0.0, point 10.0 10.0)
              Line(point 10.0 10.0, point 0.0 10.0)
              Line(point 0.0 10.0, point 10.0 0.0) ]
    let options =
        { Intersections.defaultSelfIntersectionOptions with MinimumArcLengthSeparation = 100.0<length> }
    Assert.Equal(Ok [], Intersections.subpathSelfWith value options)

let private lineSubpath startPoint endPoint = Segment.asSubpath (Line(startPoint, endPoint))
let private at t: SubpathParameter = { SegmentIndex = 0; T = Parameter.fromFloat t }

[<Fact>]
let ``transverse line classification preserves crossing direction and apertures`` () =
    let left = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    let clockwise = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    let counterclockwise = lineSubpath (point 0.0 1.0) (point 0.0 -1.0)
    match Intersections.classifySubpathIntersection left clockwise (at 0.5) (at 0.5) with
    | Ok(Crossing(Clockwise, apertures)) ->
        Assert.Equal(90.0<degree>, apertures.FirstIncomingToSecondIncoming)
        Assert.Equal(90.0<degree>, apertures.FirstIncomingToSecondOutgoing)
        Assert.Equal(90.0<degree>, apertures.FirstOutgoingToSecondIncoming)
        Assert.Equal(90.0<degree>, apertures.FirstOutgoingToSecondOutgoing)
    | result -> failwithf "unexpected classification: %A" result
    match Intersections.classifySubpathIntersection left counterclockwise (at 0.5) (at 0.5) with
    | Ok(Crossing(Counterclockwise, _)) -> ()
    | result -> failwithf "unexpected classification: %A" result

[<Fact>]
let ``parabola tangent classification reports touching orders`` () =
    let parabola =
        subpath [ QuadraticBezier(point -1.0 1.0, point 0.0 -1.0, point 1.0 1.0) ]
    let line = lineSubpath (point -1.0 0.0) (point 1.0 0.0)
    match Intersections.classifySubpathIntersection parabola line (at 0.5) (at 0.5) with
    | Ok(Touching(SimilarlyDirected, ClockwiseFromFirstToSecond, ClockwiseFromSecondToFirst, _)) -> ()
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
let ``open endpoint contact precedes direction classification`` () =
    let first = lineSubpath (point 0.0 0.0) (point 1.0 0.0)
    let second = lineSubpath (point 0.0 -1.0) (point 0.0 1.0)
    Assert.Equal(
        Ok(EndpointContact(FirstEndpointToSecondInterior StartEndpoint)),
        Intersections.classifySubpathIntersection first second (at 0.0) (at 0.5))

[<Fact>]
let ``classification rejects invalid angular tolerance`` () =
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
