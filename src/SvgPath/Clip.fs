namespace SvgPath

[<Struct>]
type ClipOptions =
    { Intersection: IntersectionOptions
      Containment: ContainmentOptions
      Tolerance: float<length> }

[<RequireQualifiedAccess>]
module Clip =
    let defaultOptions =
        { Intersection = Intersections.defaultOptions
          Containment = WindingField.defaultOptions
          Tolerance = 1.0e-6<length> }

    let private validateOptions options =
        Intersections.validateOptions options.Intersection
        |> Result.bind (fun () ->
            WindingField.validateOptions options.Containment
            |> Result.bind (fun () ->
                if options.Tolerance <= 0.0<length>
                   || not (System.Double.IsFinite(float options.Tolerance)) then
                    Error(InvalidIntersectionTolerance options.Tolerance)
                else Ok()))

    let private compareParameters left right =
        compare (left.SegmentIndex, left.T) (right.SegmentIndex, right.T)

    let private intervalSubpath
        (subpathValue: Subpath)
        (fromValue: SubpathParameter)
        (toValue: SubpathParameter) =
        let points = [ fromValue; toValue ] |> List.sortWith compareParameters
        Cut.atParameters subpathValue points
        |> Result.bind (function
            | _ :: middle :: _ when not subpathValue.Closed -> Ok middle
            | first :: _ when subpathValue.Closed -> Ok first
            | _ -> Error(InvalidSubpathInterval(fromValue, toValue)))

    let private parameterSeparation
        (tolerance: float<length>)
        (subpathValue: Subpath)
        (first: SubpathParameter)
        (second: SubpathParameter) =
        if first = second then Ok 0.0<length>
        elif subpathValue.Closed && compareParameters first second > 0 then
            Cut.atParameters subpathValue [ second; first ]
            |> Result.bind (function
                | [ _; wrapped ] -> Subpath.length wrapped
                | _ -> Error(InvalidSubpathInterval(first, second)))
        else
            intervalSubpath subpathValue first second |> Result.bind Subpath.length

    let private uniqueParameters tolerance subpathValue parameters =
        parameters
        |> List.fold (fun state parameterValue ->
            state
            |> Result.bind (fun found ->
                Subpath.parameterCanonicalize subpathValue parameterValue
                |> Result.map (fun canonical -> canonical :: found))) (Ok [])
        |> Result.bind (fun canonical ->
            let sorted = canonical |> List.distinct |> List.sortWith compareParameters
            sorted
            |> List.fold (fun state parameterValue ->
                state
                |> Result.bind (fun kept ->
                    match kept with
                    | previous :: _ ->
                        parameterSeparation tolerance subpathValue previous parameterValue
                        |> Result.map (fun separation ->
                            if separation <= tolerance then kept else parameterValue :: kept)
                    | [] -> Ok [ parameterValue ])) (Ok [])
            |> Result.bind (fun reversed ->
                let kept = List.rev reversed
                if not subpathValue.Closed || List.length kept < 2 then Ok kept
                else
                    parameterSeparation tolerance subpathValue (List.last kept) (List.head kept)
                    |> Result.map (fun seam -> if seam <= tolerance then List.take (List.length kept - 1) kept else kept)))

    let private splitPoints input clipRegion options =
        Encounters.pathWith (Path.singleton input) clipRegion options.Intersection
        |> Result.bind (fun found ->
            let intersections =
                found.Intersections
                |> List.collect (fun intersection -> intersection.LeftParameters)
                |> List.choose (fun parameterValue ->
                    if parameterValue.SubpathIndex = 0 then Some parameterValue.At else None)
            let overlaps =
                found.Overlaps
                |> List.collect (fun overlap ->
                    [ Overlaps.pathOverlapLeftStart overlap
                      Overlaps.pathOverlapLeftEnd overlap ]
                    |> List.choose id)
                |> List.choose (fun parameterValue ->
                    if parameterValue.SubpathIndex = 0 then Some parameterValue.At else None)
            let count = List.length input.Segments
            intersections @ overlaps
            |> List.filter (fun parameterValue ->
                input.Closed
                || (not (parameterValue.SegmentIndex = 0 && parameterValue.T = 0.0<parameter>)
                    && not (parameterValue.SegmentIndex = count - 1 && parameterValue.T = 1.0<parameter>)))
            |> uniqueParameters options.Tolerance input)

    let private samplePoint options (subpathValue: Subpath) =
        Subpath.length subpathValue
        |> Result.bind (fun total ->
            if total <= options.Tolerance then
                Subpath.point subpathValue { SegmentIndex = 0; T = 0.0<parameter> }
            else
                Subpath.pointAtLength subpathValue (total / 2.0))

    let private isInside clipRegion fillRule options subpathValue =
        samplePoint options subpathValue
        |> Result.bind (fun sample ->
            WindingField.pathContainmentWith sample clipRegion fillRule options.Containment
            |> Result.map (function Inside | Boundary -> true | Outside -> false))

    let subpathWith (input: Subpath) clipRegion fillRule options =
        validateOptions options
        |> Result.bind (fun () ->
            match input.Segments with
            | [] -> Ok []
            | _ ->
                splitPoints input clipRegion options
                |> Result.bind (fun points ->
                    let pieces = if List.isEmpty points then Ok [ input ] else Cut.atParameters input points
                    pieces
                    |> Result.bind (fun pieces ->
                        pieces
                        |> List.fold (fun state piece ->
                            state
                            |> Result.bind (fun kept ->
                                isInside clipRegion fillRule options piece
                                |> Result.map (fun keep -> if keep then piece :: kept else kept))) (Ok [])
                        |> Result.map List.rev)))

    let subpath input clipRegion fillRule = subpathWith input clipRegion fillRule defaultOptions

    let pathWith (input: Path) clipRegion fillRule options =
        validateOptions options
        |> Result.bind (fun () ->
            input.Subpaths
            |> List.fold (fun state subpathValue ->
                state
                |> Result.bind (fun kept ->
                    subpathWith subpathValue clipRegion fillRule options
                    |> Result.map (fun clipped -> List.rev clipped @ kept))) (Ok [])
            |> Result.map (List.rev >> Path.ofSubpaths))

    let path input clipRegion fillRule = pathWith input clipRegion fillRule defaultOptions
