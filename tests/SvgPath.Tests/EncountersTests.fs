namespace SvgPath.Tests

open SvgPath
open Xunit

module EncountersTests =
    let private point x y = Point.create x y
    let private line x1 y1 x2 y2 = Line(point x1 y1, point x2 y2)
    let private p value = Parameter.fromFloat value
    let private parameterInside value first second = value >= min first second && value <= max first second

    let private segmentIntersectionIsValid left right (intersection: SegmentIntersection) tolerance =
        if tolerance < 0.0<length> || not (parameterInside intersection.LeftT (p 0.0) (p 1.0))
           || not (parameterInside intersection.RightT (p 0.0) (p 1.0)) then false
        else
            let leftPoint = Segment.point left intersection.LeftT |> Result.defaultWith (failwithf "%A")
            let rightPoint = Segment.point right intersection.RightT |> Result.defaultWith (failwithf "%A")
            Point.near tolerance leftPoint intersection.Point && Point.near tolerance rightPoint intersection.Point

    let private segmentOverlapIsValid left right (overlap: SegmentOverlap) tolerance =
        if tolerance < 0.0<length> || overlap.LeftFrom < p 0.0 || overlap.LeftTo > p 1.0
           || overlap.RightFrom < p 0.0 || overlap.RightFrom > p 1.0
           || overlap.RightTo < p 0.0 || overlap.RightTo > p 1.0
           || overlap.LeftFrom >= overlap.LeftTo || overlap.RightFrom = overlap.RightTo then false
        else
            [ 0.0; 0.25; 0.5; 0.75; 1.0 ]
            |> List.forall (fun portion ->
                let leftT = overlap.LeftFrom + portion * (overlap.LeftTo - overlap.LeftFrom)
                let rightT = Overlaps.segmentOverlapRightParameter overlap leftT
                let leftPoint = Segment.point left leftT |> Result.defaultWith (failwithf "%A")
                let rightPoint = Segment.point right rightT |> Result.defaultWith (failwithf "%A")
                Point.near tolerance leftPoint rightPoint)

    let private intersectionContainedInOverlap (intersection: SegmentIntersection) (overlap: SegmentOverlap) =
        parameterInside intersection.LeftT overlap.LeftFrom overlap.LeftTo
        && parameterInside intersection.RightT overlap.RightFrom overlap.RightTo

    let private intersectionConflictsWithOverlap (intersection: SegmentIntersection) (overlap: SegmentOverlap) =
        let leftInside = parameterInside intersection.LeftT overlap.LeftFrom overlap.LeftTo
        let rightInside = parameterInside intersection.RightT overlap.RightFrom overlap.RightTo
        if leftInside <> rightInside then true
        elif not leftInside then false
        else
            let mappedRight = Overlaps.segmentOverlapRightParameter overlap intersection.LeftT
            abs (mappedRight - intersection.RightT) <= p 1.0e-9

    let private segmentEncountersAreValid left right found tolerance =
        found.Overlaps |> List.forall (fun overlap -> segmentOverlapIsValid left right overlap tolerance)
        && found.Intersections |> List.forall (fun intersection -> segmentIntersectionIsValid left right intersection tolerance)
        && found.Intersections
           |> List.forall (fun intersection ->
               found.Overlaps |> List.forall (fun overlap -> not (intersectionConflictsWithOverlap intersection overlap)))

    let private subpath segments = Subpath.create segments |> Result.defaultWith (failwithf "%A")
    let private at index value: SubpathParameter = { SegmentIndex = index; T = p value }

    let private filterRemovesParameterPair leftParameter rightParameter left right overlaps tolerance =
        let intersection: SubpathIntersection =
            { Point = point 0.0<length> 0.0<length>
              LeftParameters = [ leftParameter ]
              RightParameters = [ rightParameter ] }
        let found = { Overlaps = overlaps; Intersections = [ intersection ] }
        Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found left right tolerance
        |> Result.map (fun filtered -> List.isEmpty filtered.Intersections)

    let private segmentSubpathOverlapIsValid segment (subpathValue: Subpath) (overlap: SegmentSubpathOverlap) tolerance =
        not (List.isEmpty overlap.Pieces)
        && overlap.Pieces
           |> List.forall (fun piece ->
               match List.tryItem piece.SubpathSegmentIndex subpathValue.Segments with
               | Some other -> segmentOverlapIsValid segment other piece.Correspondence tolerance
               | None -> false)

    let private selfCrossingCubic () =
        CubicBezier(
            point 0.0<length> 0.0<length>,
            point -0.2708333333333333<length> -0.3333333333333333<length>,
            point -0.5416666666666666<length> -0.3333333333333333<length>,
            point 0.1875<length> 0.0<length>)

    let private segmentOverlap fromValue toValue: SegmentOverlap =
        { Start = point (fromValue * 10.0<length>) 0.0<length>
          Finish = point (toValue * 10.0<length>) 0.0<length>
          LeftFrom = p fromValue; LeftTo = p toValue
          RightFrom = p fromValue; RightTo = p toValue }

    let private intersectionAt value: SegmentIntersection =
        { Point = point (value * 10.0<length>) 0.0<length>
          LeftT = p value; RightT = p value }

    [<Fact>]
    let ``disjoint segments have no encounters`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let right = line 0.0<length> 2.0<length> 10.0<length> 2.0<length>
        Assert.Equal(
            Ok { Overlaps = []; Intersections = [] },
            Encounters.segment left right)

    [<Fact>]
    let ``crossing segments have one point intersection`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 10.0<length>
        let right = line 0.0<length> 10.0<length> 10.0<length> 0.0<length>
        match Encounters.segment left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            Assert.Empty encounters.Overlaps
            let found = encounters.Intersections |> List.exactlyOne
            Assert.InRange(found.LeftT, p (0.5 - 1.0e-12), p (0.5 + 1.0e-12))
            Assert.InRange(found.RightT, p (0.5 - 1.0e-12), p (0.5 + 1.0e-12))
            Assert.True(Point.near 1.0e-9<length> (point 5.0<length> 5.0<length>) found.Point)
            Assert.True(segmentEncountersAreValid left right encounters 1.0e-9<length>)

    [<Fact>]
    let ``endpoint touch is a point intersection`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let right = line 10.0<length> 0.0<length> 10.0<length> 10.0<length>
        let found = Encounters.segment left right |> Result.defaultWith (failwithf "%A")
        Assert.Empty found.Overlaps
        Assert.Single found.Intersections |> ignore
        Assert.True(segmentEncountersAreValid left right found 1.0e-9<length>)

    [<Fact>]
    let ``partial line overlap has overlap and no reported points`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let right = line 5.0<length> 0.0<length> 15.0<length> 0.0<length>
        match Encounters.segment left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            Assert.Single encounters.Overlaps |> ignore
            Assert.Empty encounters.Intersections
            Assert.True(segmentEncountersAreValid left right encounters 1.0e-9<length>)

    [<Fact>]
    let ``overlapping segments still validate intersection options`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let options = { Intersections.defaultOptions with MaxDepth = 0; ParameterSnap = NoParameterSnap }
        Assert.Equal(Error(InvalidIntersectionMaxDepth 0), Encounters.segmentWith segment segment options)

    [<Fact>]
    let ``reversed partial line overlap is valid`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let right = line 15.0<length> 0.0<length> 5.0<length> 0.0<length>
        let found = Encounters.segment left right |> Result.defaultWith (failwithf "%A")
        let overlap = found.Overlaps |> List.exactlyOne
        Assert.True(overlap.RightFrom > overlap.RightTo)
        Assert.Empty found.Intersections
        Assert.True(segmentEncountersAreValid left right found 1.0e-9<length>)

    [<Fact>]
    let ``overlap validator rejects out of range parameters`` () =
        let invalid: SegmentOverlap =
            { LeftFrom = p -0.1; LeftTo = p 1.0; RightFrom = p 0.0; RightTo = p 1.0
              Start = point 0.0<length> 0.0<length>; Finish = point 10.0<length> 0.0<length> }
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        Assert.False(segmentOverlapIsValid segment segment invalid 1.0e-9<length>)

    [<Fact>]
    let ``overlap validator rejects noncoincident interiors`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let right = QuadraticBezier(point 0.0<length> 0.0<length>, point 5.0<length> 5.0<length>, point 10.0<length> 0.0<length>)
        let invalid: SegmentOverlap =
            { LeftFrom = p 0.0; LeftTo = p 1.0; RightFrom = p 0.0; RightTo = p 1.0
              Start = point 0.0<length> 0.0<length>; Finish = point 10.0<length> 0.0<length> }
        Assert.False(segmentOverlapIsValid left right invalid 1.0e-9<length>)

    [<Fact>]
    let ``encounter validator reports intersection contained in overlap`` () =
        let overlap: SegmentOverlap =
            { LeftFrom = p 0.0; LeftTo = p 1.0; RightFrom = p 1.0; RightTo = p 0.0
              Start = point 0.0<length> 0.0<length>; Finish = point 10.0<length> 0.0<length> }
        let intersection: SegmentIntersection =
            { LeftT = p 0.5; RightT = p 0.5; Point = point 5.0<length> 0.0<length> }
        Assert.True(intersectionContainedInOverlap intersection overlap)
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let found = { Overlaps = [ overlap ]; Intersections = [ intersection ] }
        Assert.False(segmentEncountersAreValid segment (Segment.reverse segment) found 1.0e-9<length>)

    [<Fact>]
    let ``intersection validator rejects wrong recorded point`` () =
        let left = line 0.0<length> 0.0<length> 10.0<length> 10.0<length>
        let right = line 0.0<length> 10.0<length> 10.0<length> 0.0<length>
        let recorded = point 6.0<length> 5.0<length>
        let invalid = { LeftT = p 0.5; RightT = p 0.5; Point = recorded }
        Assert.False(segmentIntersectionIsValid left right invalid 1.0e-9<length>)

    [<Fact>]
    let ``subpaths retain overlap and intersections from other segment pairs`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>; line 10.0<length> 0.0<length> 10.0<length> 10.0<length> ]
        let right = subpath [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>; line 5.0<length> 0.0<length> 15.0<length> 10.0<length> ]
        let found = Encounters.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Single found.Overlaps |> ignore
        Assert.Equal(2, found.Intersections.Length)
        Assert.Equal(point 5.0<length> 0.0<length>, found.Intersections[0].Point)
        Assert.Equal(point 10.0<length> 5.0<length>, found.Intersections[1].Point)

    [<Fact>]
    let ``subpath encounters retain piecewise overlap correspondence`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right = subpath [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>; line 5.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let found = Encounters.subpath left right |> Result.defaultWith (failwithf "%A")
        let overlap = found.Overlaps |> List.exactlyOne
        Assert.Equal(2, overlap.Pieces.Length)
        Assert.Equal((0, 0), (overlap.Pieces[0].LeftSegmentIndex, overlap.Pieces[0].RightSegmentIndex))
        Assert.Equal((0, 1), (overlap.Pieces[1].LeftSegmentIndex, overlap.Pieces[1].RightSegmentIndex))
        Assert.Empty found.Intersections

    [<Fact>]
    let ``subpath parameters are complementary through overlap`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right = subpath [ line 2.0<length> 0.0<length> 8.0<length> 0.0<length> ]
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Equal(Ok true, filterRemovesParameterPair (at 0 0.5) (at 0 0.5) left right overlaps 1.0e-9<length>)
        Assert.Equal(Ok false, filterRemovesParameterPair (at 0 0.0) (at 0 0.0) left right overlaps 1.0e-9<length>)

    [<Fact>]
    let ``subpath parameter complementarity clamps by arc length`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right = subpath [ line 2.0<length> 0.0<length> 8.0<length> 0.0<length> ]
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Equal(Ok true, filterRemovesParameterPair (at 0 0.19999999) (at 0 0.0) left right overlaps 1.0e-6<length>)
        Assert.Equal(Ok false, filterRemovesParameterPair (at 0 0.19) (at 0 0.0) left right overlaps 1.0e-6<length>)

    [<Fact>]
    let ``subpath parameter complementarity uses short closed seam motion`` () =
        let left =
            Subpath.polyline [ point 0.0<length> 0.0<length>; point 10.0<length> 0.0<length>; point 10.0<length> 10.0<length>; point 0.0<length> 10.0<length>; point 0.0<length> 0.0<length> ]
            |> Result.bind (Subpath.setClosed true)
            |> Result.defaultWith (failwithf "%A")
        let right = subpath [ line 0.0<length> 5.0<length> 0.0<length> 0.0<length> ]
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Equal(Ok true, filterRemovesParameterPair (at 0 0.00000001) (at 0 1.0) left right overlaps 1.0e-6<length>)

    [<Fact>]
    let ``subpath parameter complementarity searches all overlaps`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right =
            Subpath.polyline [ point 0.0<length> 0.0<length>; point 4.0<length> 0.0<length>; point 4.0<length> 2.0<length>; point 6.0<length> 2.0<length>; point 6.0<length> 0.0<length>; point 10.0<length> 0.0<length> ]
            |> Result.defaultWith (failwithf "%A")
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Equal(2, overlaps.Length)
        Assert.Equal(Ok true, filterRemovesParameterPair (at 0 0.8) (at 4 0.5) left right overlaps 1.0e-9<length>)

    [<Fact>]
    let ``subpath parameter complementarity rejects invalid tolerance`` () =
        let value = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        Assert.Equal(Error(InvalidIntersectionTolerance 0.0<length>), filterRemovesParameterPair (at 0 0.5) (at 0 0.5) value value [] 0.0<length>)

    [<Fact>]
    let ``subpath parameter complementarity rejects infinite tolerance`` () =
        let value = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let infinity = LanguagePrimitives.FloatWithMeasure<length> System.Double.PositiveInfinity
        Assert.Equal(Error(InvalidIntersectionTolerance infinity), filterRemovesParameterPair (at 0 0.5) (at 0 0.5) value value [] infinity)

    [<Fact>]
    let ``subpath encounters merge shared endpoint aliases`` () =
        let left =
            Subpath.create
                [ Line(point 0.0<length> 0.0<length>, point 1.0<length> 0.0<length>)
                  Line(point 1.0<length> 0.0<length>, point 2.0<length> 0.0<length>) ]
        let right = Subpath.ofSegment (Line(point 1.0<length> -1.0<length>, point 1.0<length> 1.0<length>))
        match left with
        | Error error -> failwithf "%A" error
        | Ok left ->
            match Encounters.subpath left right with
            | Error error -> failwithf "%A" error
            | Ok encounters ->
                Assert.Empty encounters.Overlaps
                let intersection = Assert.Single encounters.Intersections
                Assert.Single intersection.LeftParameters |> ignore

    [<Fact>]
    let ``subpath intersection entirely explained by overlap is removed`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right = subpath [ line 2.0<length> 0.0<length> 8.0<length> 0.0<length> ]
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        let intersection: SubpathIntersection =
            { Point = point 5.0<length> 0.0<length>
              LeftParameters = [ at 0 0.5 ]
              RightParameters = [ at 0 0.5 ] }
        let found = { Overlaps = overlaps; Intersections = [ intersection ] }
        let filtered =
            Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found left right 1.0e-9<length>
            |> Result.defaultWith (failwithf "%A")
        Assert.True((overlaps = filtered.Overlaps))
        Assert.Empty filtered.Intersections

    [<Fact>]
    let ``empty filtered encounters still validate tolerance`` () =
        let value = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let found: Encounters<SubpathOverlap, SubpathIntersection> = { Overlaps = []; Intersections = [] }
        Assert.Equal(
            Error(InvalidIntersectionTolerance 0.0<length>),
            Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found value value 0.0<length>)

    [<Fact>]
    let ``subpath intersection retains parameters with non overlap claim`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length> ]
        let right = subpath [ line 2.0<length> 0.0<length> 8.0<length> 0.0<length> ]
        let overlaps = Overlaps.subpath left right |> Result.defaultWith (failwithf "%A")
        let complementaryLeft = at 0 0.5
        let nonComplementaryLeft = at 0 0.8
        let rightParameter = at 0 0.5
        let found: Encounters<SubpathOverlap, SubpathIntersection> =
            { Overlaps = overlaps
              Intersections =
                [ { Point = point 5.0<length> 0.0<length>
                    LeftParameters = [ complementaryLeft; nonComplementaryLeft ]
                    RightParameters = [ rightParameter ] } ] }
        let filtered =
            Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found left right 1.0e-9<length>
            |> Result.defaultWith (failwithf "%A")
        let intersection = filtered.Intersections |> List.exactlyOne
        Assert.True([ nonComplementaryLeft ] = intersection.LeftParameters)
        Assert.True([ rightParameter ] = intersection.RightParameters)

    [<Fact>]
    let ``segment subpath retains addresses for overlap and points`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let subpathValue =
            subpath
                [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>
                  line 5.0<length> 0.0<length> 5.0<length> 5.0<length>
                  line 5.0<length> 5.0<length> 10.0<length> -5.0<length> ]
        let found = Encounters.segmentSubpath segment subpathValue |> Result.defaultWith (failwithf "%A")
        let overlap = found.Overlaps |> List.exactlyOne
        Assert.Equal(point 0.0<length> 0.0<length>, overlap.Start)
        Assert.Equal(point 5.0<length> 0.0<length>, overlap.Finish)
        let piece = overlap.Pieces |> List.exactlyOne
        Assert.Equal(0, piece.SubpathSegmentIndex)
        Assert.Equal((p 0.0, p 0.5, p 0.0, p 1.0),
                     (piece.Correspondence.LeftFrom, piece.Correspondence.LeftTo,
                      piece.Correspondence.RightFrom, piece.Correspondence.RightTo))
        let expected: (Point<length> * float<parameter> * SubpathParameter list) list =
            [ point 5.0<length> 0.0<length>, p 0.5, [ at 1 0.0 ]
              point 7.5<length> 0.0<length>, p 0.75, [ at 2 0.5 ] ]
        Assert.True((expected = found.Intersections))

    [<Fact>]
    let ``path encounters retain subpath and segment addresses`` () =
        let leftSubpath =
            subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>; line 10.0<length> 0.0<length> 10.0<length> 10.0<length> ]
        let rightSubpath =
            subpath [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>; line 5.0<length> 0.0<length> 15.0<length> 10.0<length> ]
        let found = Encounters.path (Path.singleton leftSubpath) (Path.singleton rightSubpath) |> Result.defaultWith (failwithf "%A")
        let overlap = found.Overlaps |> List.exactlyOne
        Assert.Equal((0, 0), (overlap.LeftSubpathIndex, overlap.RightSubpathIndex))
        let piece = overlap.Correspondence.Pieces |> List.exactlyOne
        Assert.Equal((0, 0), (piece.LeftSegmentIndex, piece.RightSegmentIndex))
        Assert.Equal(2, found.Intersections.Length)
        found.Intersections
        |> List.iter (fun intersection ->
            intersection.LeftParameters |> List.iter (fun value -> Assert.Equal(0, value.SubpathIndex))
            intersection.RightParameters |> List.iter (fun value -> Assert.Equal(0, value.SubpathIndex)))

    [<Fact>]
    let ``higher level validators accept pure overlap results`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let subpathValue = subpath [ segment ]
        let segmentSubpathResult = Encounters.segmentSubpath segment subpathValue |> Result.defaultWith (failwithf "%A")
        Assert.True(segmentSubpathResult.Overlaps |> List.forall (fun overlap -> segmentSubpathOverlapIsValid segment subpathValue overlap 1.0e-9<length>))
        Assert.Empty segmentSubpathResult.Intersections
        let subpathResult = Encounters.subpath subpathValue subpathValue |> Result.defaultWith (failwithf "%A")
        Assert.Single subpathResult.Overlaps |> ignore
        Assert.Empty subpathResult.Intersections
        let pathResult = Encounters.path (Path.singleton subpathValue) (Path.singleton subpathValue) |> Result.defaultWith (failwithf "%A")
        Assert.Single pathResult.Overlaps |> ignore
        Assert.Empty pathResult.Intersections

    [<Fact>]
    let ``higher level overlap validators reject invalid segment index`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let subpathValue = subpath [ segment; line 10.0<length> 0.0<length> 20.0<length> 0.0<length> ]
        let correspondence: SegmentOverlap =
            { LeftFrom = p 0.0; LeftTo = p 1.0; RightFrom = p 0.0; RightTo = p 1.0
              Start = point 0.0<length> 0.0<length>; Finish = point 10.0<length> 0.0<length> }
        let invalid: SegmentSubpathOverlap =
            { Start = point 0.0<length> 0.0<length>
              Finish = point 10.0<length> 0.0<length>
              Pieces = [ { SubpathSegmentIndex = 99; Correspondence = correspondence } ] }
        Assert.False(segmentSubpathOverlapIsValid segment subpathValue invalid 1.0e-9<length>)

    [<Fact>]
    let ``point strictly inside overlap fixture`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let overlap = segmentOverlap 0.2 0.8
        let intersection = intersectionAt 0.5
        Assert.True(intersectionContainedInOverlap intersection overlap)
        Assert.False(segmentEncountersAreValid segment segment { Overlaps = [ overlap ]; Intersections = [ intersection ] } 1.0e-6<length>)

    [<Fact>]
    let ``point at overlap boundary with same address fixture`` () =
        let segment = line 0.0<length> 0.0<length> 10.0<length> 0.0<length>
        let overlap = segmentOverlap 0.2 0.8
        let intersection = intersectionAt 0.2
        Assert.True(intersectionContainedInOverlap intersection overlap)
        Assert.False(segmentEncountersAreValid segment segment { Overlaps = [ overlap ]; Intersections = [ intersection ] } 1.0e-6<length>)

    [<Fact>]
    let ``point at overlap boundary through adjacent alias fixture`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>; line 10.0<length> 0.0<length> 10.0<length> 10.0<length> ]
        let right = subpath [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>; line 5.0<length> 0.0<length> 5.0<length> 10.0<length> ]
        let found = Encounters.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Single found.Overlaps |> ignore
        Assert.Single found.Intersections |> ignore
        let filtered = Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found left right 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
        Assert.Empty filtered.Intersections

    [<Fact>]
    let ``isolated intersection elsewhere in same query fixture`` () =
        let left = subpath [ line 0.0<length> 0.0<length> 10.0<length> 0.0<length>; line 10.0<length> 0.0<length> 10.0<length> 10.0<length> ]
        let right = subpath [ line 0.0<length> 0.0<length> 5.0<length> 0.0<length>; line 5.0<length> 0.0<length> 15.0<length> 10.0<length> ]
        let found = Encounters.subpath left right |> Result.defaultWith (failwithf "%A")
        Assert.Single found.Overlaps |> ignore
        Assert.Equal(2, found.Intersections.Length)
        let filtered = Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters found left right 1.0e-6<length> |> Result.defaultWith (failwithf "%A")
        let isolated = filtered.Intersections |> List.exactlyOne
        Assert.Equal(point 10.0<length> 5.0<length>, isolated.Point)

    [<Fact>]
    let ``cubic overlap can mask another isolated intersection fixture`` () =
        let curve = selfCrossingCubic ()
        let found = Encounters.segment curve curve |> Result.defaultWith (failwithf "%A")
        Assert.Single found.Overlaps |> ignore
        Assert.Equal(2, found.Intersections.Length)
        Assert.Contains(found.Intersections, fun value -> abs (value.LeftT - p 0.25) <= p 1.0e-6 && abs (value.RightT - p 0.75) <= p 1.0e-6)
        Assert.Contains(found.Intersections, fun value -> abs (value.LeftT - p 0.75) <= p 1.0e-6 && abs (value.RightT - p 0.25) <= p 1.0e-6)
        Assert.True(segmentEncountersAreValid curve curve found 1.0e-6<length>)

    [<Fact>]
    let ``one sided overlap containment is flagged fixture`` () =
        let whole = selfCrossingCubic ()
        let branch = Segment.betweenInside whole (p 0.1) (p 0.5) |> Result.defaultWith (failwithf "%A")
        let overlap: SegmentOverlap =
            { Start = Segment.start branch; Finish = Segment.finish branch
              LeftFrom = p 0.1; LeftTo = p 0.5; RightFrom = p 0.0; RightTo = p 1.0 }
        let crossing = Segment.point whole (p 0.75) |> Result.defaultWith (failwithf "%A")
        let intersection: SegmentIntersection = { Point = crossing; LeftT = p 0.75; RightT = p 0.375 }
        Assert.True(segmentOverlapIsValid whole branch overlap 1.0e-6<length>)
        Assert.True(segmentIntersectionIsValid whole branch intersection 1.0e-6<length>)
        Assert.False(parameterInside intersection.LeftT overlap.LeftFrom overlap.LeftTo)
        Assert.True(parameterInside intersection.RightT overlap.RightFrom overlap.RightTo)
        Assert.False(segmentEncountersAreValid whole branch { Overlaps = [ overlap ]; Intersections = [ intersection ] } 1.0e-6<length>)
