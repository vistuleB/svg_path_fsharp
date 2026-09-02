namespace SvgPath

[<Struct>]
type Encounters<'overlap, 'intersection> =
    { Overlaps: 'overlap list
      Intersections: 'intersection list }

[<Struct>]
type private ParameterWindow =
    { From: float<parameter>
      To: float<parameter> }

[<RequireQualifiedAccess>]
module Encounters =
    let private ratio (value: float<parameter>) = Parameter.ratio value

    let private interpolate
        (fromValue: float<parameter>)
        (toValue: float<parameter>)
        (portion: float<parameter>)
        : float<parameter> =
        fromValue + ratio portion * (toValue - fromValue)

    let private uniqueParameters parameters =
        parameters
        |> List.sort
        |> List.fold (fun found value ->
            match found with
            | previous :: _ when previous = value -> found
            | _ -> value :: found) []
        |> List.rev

    let private parameterWindows (overlaps: SegmentOverlap list) leftSide : ParameterWindow list =
        overlaps
        |> List.collect (fun overlap ->
            if leftSide then [ overlap.LeftFrom; overlap.LeftTo ]
            else [ overlap.RightFrom; overlap.RightTo ])
        |> fun values -> 0.0<parameter> :: 1.0<parameter> :: values
        |> uniqueParameters
        |> List.pairwise
        |> List.map (fun (fromValue, toValue) -> ({ From = fromValue; To = toValue } : ParameterWindow))

    let private windowsFollowOverlap
        (leftWindow: ParameterWindow)
        (rightWindow: ParameterWindow)
        (overlaps: SegmentOverlap list) =
        overlaps
        |> List.exists (fun overlap ->
            let mappedFrom = Overlaps.segmentOverlapRightParameter overlap leftWindow.From
            let mappedTo = Overlaps.segmentOverlapRightParameter overlap leftWindow.To
            leftWindow.From >= overlap.LeftFrom
            && leftWindow.To <= overlap.LeftTo
            && min mappedFrom mappedTo = rightWindow.From
            && max mappedFrom mappedTo = rightWindow.To
            && rightWindow.From >= min overlap.RightFrom overlap.RightTo
            && rightWindow.To <= max overlap.RightFrom overlap.RightTo)

    let private parameterInside value fromValue toValue =
        value >= min fromValue toValue && value <= max fromValue toValue

    let private segmentParametersAreStalled segment first second tolerance =
        if first = second then Ok true
        else
            Segment.betweenInside segment (min first second) (max first second)
            |> Result.bind Segment.length
            |> Result.map (fun motion -> motion <= tolerance)

    let rec private intersectionFollowsAnOverlap left right tolerance (intersection: SegmentIntersection) (overlaps: SegmentOverlap list) =
        match overlaps with
        | [] -> Ok false
        | overlap :: rest ->
            if not (parameterInside intersection.LeftT overlap.LeftFrom overlap.LeftTo)
               || not (parameterInside intersection.RightT overlap.RightFrom overlap.RightTo) then
                intersectionFollowsAnOverlap left right tolerance intersection rest
            else
                let mappedRight = Overlaps.segmentOverlapRightParameter overlap intersection.LeftT
                let mappedLeft = Overlaps.segmentOverlapLeftParameter overlap intersection.RightT
                segmentParametersAreStalled left intersection.LeftT mappedLeft tolerance
                |> Result.bind (fun leftStalled ->
                    segmentParametersAreStalled right intersection.RightT mappedRight tolerance
                    |> Result.bind (fun rightStalled ->
                        if leftStalled && rightStalled then Ok true
                        else intersectionFollowsAnOverlap left right tolerance intersection rest))

    let private selfIntersectionsThroughLeftOverlap (intersection: SegmentIntersection) (overlap: SegmentOverlap) =
        [ intersection.LeftT, intersection.RightT; intersection.RightT, intersection.LeftT ]
        |> List.choose (fun (throughOverlap, remainingLeft) ->
            if parameterInside throughOverlap overlap.LeftFrom overlap.LeftTo then
                Some
                    { Point = intersection.Point
                      LeftT = remainingLeft
                      RightT = Overlaps.segmentOverlapRightParameter overlap throughOverlap }
            else None)

    let private selfIntersectionsThroughRightOverlap (intersection: SegmentIntersection) (overlap: SegmentOverlap) =
        [ intersection.LeftT, intersection.RightT; intersection.RightT, intersection.LeftT ]
        |> List.choose (fun (throughOverlap, remainingRight) ->
            if parameterInside throughOverlap overlap.RightFrom overlap.RightTo then
                Some
                    { Point = intersection.Point
                      LeftT = Overlaps.segmentOverlapLeftParameter overlap throughOverlap
                      RightT = remainingRight }
            else None)

    let private overlapOffDiagonalSelfIntersections left right (overlaps: SegmentOverlap list) tolerance =
        let options =
            { MinimumArcLengthSeparation = tolerance
              DistanceTolerance = tolerance }
        Intersections.segmentSelfWith left options
        |> Result.bind (fun leftSelf ->
            Intersections.segmentSelfWith right options
            |> Result.map (fun rightSelf ->
                let fromLeft = overlaps |> List.collect (fun overlap -> leftSelf |> List.collect (fun intersection -> selfIntersectionsThroughLeftOverlap intersection overlap))
                let fromRight = overlaps |> List.collect (fun overlap -> rightSelf |> List.collect (fun intersection -> selfIntersectionsThroughRightOverlap intersection overlap))
                fromLeft @ fromRight))

    let rec private hasGeometricDuplicate (candidate: SegmentIntersection) (existing: SegmentIntersection list) left right tolerance =
        match existing with
        | [] -> Ok false
        | intersection :: rest ->
            segmentParametersAreStalled left candidate.LeftT intersection.LeftT tolerance
            |> Result.bind (fun leftStalled ->
                segmentParametersAreStalled right candidate.RightT intersection.RightT tolerance
                |> Result.bind (fun rightStalled ->
                    if leftStalled && rightStalled then Ok true
                    else hasGeometricDuplicate candidate rest left right tolerance))

    let private uniqueSegmentIntersections (intersections: SegmentIntersection list) left right tolerance =
        intersections
        |> List.fold (fun state intersection ->
            state
            |> Result.bind (fun unique ->
                hasGeometricDuplicate intersection unique left right tolerance
                |> Result.map (fun duplicate -> if duplicate then unique else intersection :: unique))) (Ok [])

    let private pointEncounters
        (left: Segment)
        (right: Segment)
        (overlaps: SegmentOverlap list)
        (options: IntersectionOptions) =
        match overlaps with
        | [] -> Intersections.segmentWithoutOverlapPrecheckWith left right options
        | _ ->
            let pairs =
                [ for leftWindow in parameterWindows overlaps true do
                    for rightWindow in parameterWindows overlaps false do
                        yield leftWindow, rightWindow ]
            let inspectPair found (leftWindow: ParameterWindow, rightWindow: ParameterWindow) =
                if windowsFollowOverlap leftWindow rightWindow overlaps then Ok found
                else
                    Segment.betweenInside left leftWindow.From leftWindow.To
                    |> Result.bind (fun leftPortion ->
                        Segment.betweenInside right rightWindow.From rightWindow.To
                        |> Result.bind (fun rightPortion ->
                            Intersections.segmentWithoutOverlapPrecheckWith leftPortion rightPortion options
                            |> Result.map (fun local ->
                                local
                                |> List.fold (fun accumulated (intersection: SegmentIntersection) ->
                                    let mapped =
                                        { intersection with
                                            LeftT = interpolate leftWindow.From leftWindow.To intersection.LeftT
                                            RightT = interpolate rightWindow.From rightWindow.To intersection.RightT }
                                    mapped :: accumulated) found)))
            pairs
            |> List.fold (fun state pair -> state |> Result.bind (fun found -> inspectPair found pair)) (Ok [])
            |> Result.bind (fun windowIntersections ->
                overlapOffDiagonalSelfIntersections left right overlaps options.Tolerance
                |> Result.bind (fun selfIntersections ->
                    (windowIntersections @ selfIntersections)
                    |> List.fold (fun state (intersection: SegmentIntersection) ->
                        state
                        |> Result.bind (fun kept ->
                            intersectionFollowsAnOverlap left right options.Tolerance intersection overlaps
                            |> Result.map (fun follows -> if follows then kept else intersection :: kept))) (Ok [])
                    |> Result.bind (fun candidates -> uniqueSegmentIntersections candidates left right options.Tolerance)
                    |> Result.map (List.sortBy (fun intersection -> intersection.LeftT))))

    let segmentWith left right options =
        Intersections.validateOptions options
        |> Result.bind (fun () ->
            Overlaps.segmentWith left right options.Tolerance
            |> Result.bind (fun overlaps ->
                pointEncounters left right overlaps options
                |> Result.map (fun intersections ->
                    { Overlaps = overlaps
                      Intersections = intersections })))

    let segment left right = segmentWith left right Intersections.defaultOptions

    let subpathWith left right options =
        Intersections.validateOptions options
        |> Result.bind (fun () ->
            Overlaps.subpathWith left right options.Tolerance
            |> Result.bind (fun overlaps ->
                Intersections.subpathWithoutOverlapPrecheckWith left right options
                |> Result.map (fun intersections ->
                    { Overlaps = overlaps
                      Intersections = intersections })))

    let subpath left right = subpathWith left right Intersections.defaultOptions

    let segmentSubpathWith segmentValue subpathValue options =
        Intersections.validateOptions options
        |> Result.bind (fun () ->
            Overlaps.segmentSubpathWith segmentValue subpathValue options.Tolerance
            |> Result.bind (fun overlaps ->
                Intersections.segmentSubpathWithoutOverlapPrecheckWith segmentValue subpathValue options
                |> Result.map (fun intersections ->
                    { Overlaps = overlaps
                      Intersections = intersections })))

    let segmentSubpath segmentValue subpathValue =
        segmentSubpathWith segmentValue subpathValue Intersections.defaultOptions

    let pathWith left right options =
        Intersections.validateOptions options
        |> Result.bind (fun () ->
            Overlaps.pathWith left right options.Tolerance
            |> Result.bind (fun overlaps ->
                Intersections.pathWithoutOverlapPrecheckWith left right options
                |> Result.map (fun intersections ->
                    { Overlaps = overlaps
                      Intersections = intersections })))

    let path left right = pathWith left right Intersections.defaultOptions

    let private segmentLength (_tolerance: float<length>) (segmentValue: Segment) =
        Segment.length segmentValue

    let private segmentMotion
        (tolerance: float<length>)
        (segmentValue: Segment)
        (fromValue: float<parameter>)
        (toValue: float<parameter>) =
        if fromValue = toValue then Ok 0.0<length>
        else
            Segment.betweenInside segmentValue (min fromValue toValue) (max fromValue toValue)
            |> Result.bind (segmentLength tolerance)

    let private canonicalParameter subpathValue parameterValue =
        Subpath.parameterCanonicalize subpathValue parameterValue

    let private compareSubpathParameters left right =
        compare (left.SegmentIndex, left.T) (right.SegmentIndex, right.T)

    let private forwardSubpathMotion
        (tolerance: float<length>)
        (subpathValue: Subpath)
        (fromValue: SubpathParameter)
        (toValue: SubpathParameter) =
        let segments = subpathValue.Segments
        if fromValue = toValue then Ok 0.0<length>
        else
            [ fromValue.SegmentIndex .. toValue.SegmentIndex ]
            |> List.fold (fun state index ->
                state
                |> Result.bind (fun motion ->
                    let fromT = if index = fromValue.SegmentIndex then fromValue.T else 0.0<parameter>
                    let toT = if index = toValue.SegmentIndex then toValue.T else 1.0<parameter>
                    segmentMotion tolerance segments[index] fromT toT
                    |> Result.map ((+) motion))) (Ok 0.0<length>)

    let rec private subpathMotion
        (tolerance: float<length>)
        (subpathValue: Subpath)
        (first: SubpathParameter)
        (second: SubpathParameter) =
        canonicalParameter subpathValue first
        |> Result.bind (fun first ->
            canonicalParameter subpathValue second
            |> Result.bind (fun second ->
                if first = second then Ok 0.0<length>
                elif not subpathValue.Closed then
                    let fromValue, toValue =
                        if compareSubpathParameters first second <= 0 then first, second else second, first
                    forwardSubpathMotion tolerance subpathValue fromValue toValue
                else
                    let direct =
                        if compareSubpathParameters first second <= 0 then
                            forwardSubpathMotion tolerance subpathValue first second
                        else
                            forwardSubpathMotion tolerance subpathValue second first
                    let total =
                        segmentsLength tolerance subpathValue.Segments
                    direct
                    |> Result.bind (fun directMotion ->
                        total |> Result.map (fun totalMotion -> min directMotion (totalMotion - directMotion)))))

    and private segmentsLength (tolerance: float<length>) (segments: Segment list) =
        segments
        |> List.fold (fun state segmentValue ->
            state
            |> Result.bind (fun total ->
                segmentLength tolerance segmentValue |> Result.map ((+) total))) (Ok 0.0<length>)

    let private overlapEndpoints overlap leftSide =
        if leftSide then Overlaps.subpathOverlapLeftStart overlap, Overlaps.subpathOverlapLeftEnd overlap
        else Overlaps.subpathOverlapRightStart overlap, Overlaps.subpathOverlapRightEnd overlap

    let private clampToOverlap tolerance parameterValue subpathValue overlap leftSide otherSubpath =
        let mapped =
            if leftSide then Overlaps.subpathOverlapRightParameter overlap parameterValue subpathValue otherSubpath
            else Overlaps.subpathOverlapLeftParameter overlap parameterValue otherSubpath subpathValue
        mapped
        |> Result.bind (function
            | Some _ -> Ok(Some parameterValue)
            | None ->
                let fromValue, toValue = overlapEndpoints overlap leftSide
                match fromValue, toValue with
                | Some fromValue, Some toValue ->
                    subpathMotion tolerance subpathValue parameterValue fromValue
                    |> Result.bind (fun fromMotion ->
                        subpathMotion tolerance subpathValue parameterValue toValue
                        |> Result.map (fun toMotion ->
                            match fromMotion <= tolerance, toMotion <= tolerance with
                            | false, false -> None
                            | true, false -> Some fromValue
                            | false, true -> Some toValue
                            | true, true -> Some(if fromMotion <= toMotion then fromValue else toValue)))
                | _ -> Ok None)

    let private parametersComplementaryWithOverlap
        tolerance
        leftParameter
        rightParameter
        leftSubpath
        rightSubpath
        overlap =
        clampToOverlap tolerance leftParameter leftSubpath overlap true rightSubpath
        |> Result.bind (fun clampedLeft ->
            clampToOverlap tolerance rightParameter rightSubpath overlap false leftSubpath
            |> Result.bind (fun clampedRight ->
                let leftToRight =
                    match clampedLeft with
                    | None -> Ok false
                    | Some value ->
                        Overlaps.subpathOverlapRightParameter overlap value leftSubpath rightSubpath
                        |> Result.bind (function
                            | None -> Ok false
                            | Some opposite ->
                                subpathMotion tolerance rightSubpath opposite rightParameter
                                |> Result.map (fun motion -> motion <= tolerance))
                let rightToLeft =
                    match clampedRight with
                    | None -> Ok false
                    | Some value ->
                        Overlaps.subpathOverlapLeftParameter overlap value leftSubpath rightSubpath
                        |> Result.bind (function
                            | None -> Ok false
                            | Some opposite ->
                                subpathMotion tolerance leftSubpath opposite leftParameter
                                |> Result.map (fun motion -> motion <= tolerance))
                leftToRight
                |> Result.bind (fun forward ->
                    rightToLeft
                    |> Result.bind (fun reverse ->
                        if forward = reverse then Ok forward
                        else Error InternalOverlapParameterCorrespondenceInconsistency))))

    let private parametersComplementary tolerance leftParameter rightParameter leftSubpath rightSubpath overlaps =
        let rec loop sawInconsistency remaining =
            match remaining with
            | [] when sawInconsistency -> Error InternalOverlapParameterCorrespondenceInconsistency
            | [] -> Ok false
            | overlap :: rest ->
                match parametersComplementaryWithOverlap tolerance leftParameter rightParameter leftSubpath rightSubpath overlap with
                | Ok true -> Ok true
                | Ok false -> loop sawInconsistency rest
                | Error InternalOverlapParameterCorrespondenceInconsistency -> loop true rest
                | Error error -> Error error
        loop false overlaps

    let private filterParameters tolerance parameters opposites subpathValue oppositeSubpath overlaps filteringLeft =
        parameters
        |> List.fold (fun state parameterValue ->
            state
            |> Result.bind (fun kept ->
                opposites
                |> List.fold (fun comparison opposite ->
                    comparison
                    |> Result.bind (fun hasNonComplementary ->
                        if hasNonComplementary then Ok true
                        else
                            let check =
                                if filteringLeft then
                                    parametersComplementary tolerance parameterValue opposite subpathValue oppositeSubpath overlaps
                                else
                                    parametersComplementary tolerance opposite parameterValue oppositeSubpath subpathValue overlaps
                            check |> Result.map not)) (Ok false)
                |> Result.map (fun keep -> if keep then parameterValue :: kept else kept))) (Ok [])
        |> Result.map List.rev

    let filterFullyOverlapExplainedSubpathIntersectionParameters
        encounters
        leftSubpath
        rightSubpath
        tolerance =
        if tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidIntersectionTolerance tolerance)
        else
            encounters.Intersections
            |> List.fold (fun state (intersection: SubpathIntersection) ->
                state
                |> Result.bind (fun filtered ->
                    filterParameters
                        tolerance
                        intersection.LeftParameters
                        intersection.RightParameters
                        leftSubpath
                        rightSubpath
                        encounters.Overlaps
                        true
                    |> Result.bind (fun keptLeft ->
                        filterParameters
                            tolerance
                            intersection.RightParameters
                            intersection.LeftParameters
                            rightSubpath
                            leftSubpath
                            encounters.Overlaps
                            false
                        |> Result.map (fun keptRight ->
                            if List.isEmpty keptLeft && List.isEmpty keptRight then filtered
                            else
                                { intersection with
                                    LeftParameters = keptLeft
                                    RightParameters = keptRight } :: filtered)))) (Ok [])
            |> Result.map (fun intersections ->
                { encounters with Intersections = List.rev intersections })
