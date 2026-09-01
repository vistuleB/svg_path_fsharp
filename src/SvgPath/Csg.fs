namespace SvgPath

[<Struct>]
type CsgOptions =
    { Tolerance: float<length>
      MinimumChord: float<length> }

type BoundaryTopologyFailure =
    | SectorMismatch
    | TraceFailed

type CsgError =
    | CsgArrangementError of ArrangementError
    | CsgPathError of SegmentError
    | InternalBoundaryTopologyError of vertex: int * reason: BoundaryTopologyFailure

type CsgResult =
    { Path: Path
      Build: ArrangementSegmentBuild }

[<RequireQualifiedAccess>]
module Csg =
    let defaultOptions =
        { Tolerance = 1.0e-6<length>
          MinimumChord = 1.0e-5<length> }

    type private BooleanOperation =
        | Union
        | Intersection
        | Difference
        | SymmetricDifference

    type private BoundaryEdge =
        { Id: int
          Layer: int
          Segment: Segment
          StartVertex: int
          EndVertex: int }

    type private BoundaryRay =
        { EdgeId: int
          Starts: bool
          Angle: float<degree> }

    let private build paths options =
        let segments =
            paths
            |> List.collect Path.subpaths
            |> List.collect Subpath.segments
        Arrangement.buildWith segments options.Tolerance options.MinimumChord 0.0<parameter>
        |> Result.mapError CsgArrangementError

    let private filled winding fillRule =
        match fillRule with
        | Nonzero -> winding <> 0
        | EvenOdd -> winding % 2 <> 0

    let private combine operation left right =
        match operation with
        | Union -> left || right
        | Intersection -> left && right
        | Difference -> left && not right
        | SymmetricDifference -> left <> right

    let private classify
        (operation: BooleanOperation)
        (fillRule: FillRule)
        (tolerance: float<length>)
        (leftPath: Path)
        (rightPath: Path)
        (edges: ArrangementEdge list) : Result<BoundaryEdge list, CsgError> =
        edges
        |> List.fold (fun state edge ->
            state
            |> Result.bind (fun boundary ->
                WindingField.segmentSideNonzeroLevels edge.Segment leftPath (tolerance * 16.0) WindingField.defaultOptions
                |> Result.mapError CsgPathError
                |> Result.bind (fun (leftA, rightA) ->
                    WindingField.segmentSideNonzeroLevels edge.Segment rightPath (tolerance * 16.0) WindingField.defaultOptions
                    |> Result.mapError CsgPathError
                    |> Result.map (fun (leftB, rightB) ->
                        let activeLeft = combine operation (filled leftA fillRule) (filled leftB fillRule)
                        let activeRight = combine operation (filled rightA fillRule) (filled rightB fillRule)
                        if activeLeft = activeRight then boundary
                        elif activeLeft then
                            boundary @
                                [ { Id = edge.Id; Layer = 0; Segment = edge.Segment
                                    StartVertex = edge.StartVertex; EndVertex = edge.EndVertex } ]
                        else
                            boundary @
                                [ { Id = edge.Id; Layer = 0; Segment = Segment.reverse edge.Segment
                                    StartVertex = edge.EndVertex; EndVertex = edge.StartVertex } ])))) (Ok [])

    let private ray edge starts =
        let t = if starts then 0.0<parameter> else 1.0<parameter>
        Segment.directions edge.Segment t
        |> Result.mapError CsgPathError
        |> Result.bind (fun directions ->
            let direction = if starts then directions.Outgoing else directions.Incoming
            match direction with
            | None ->
                let vertex = if starts then edge.StartVertex else edge.EndVertex
                Error(InternalBoundaryTopologyError(vertex, SectorMismatch))
            | Some direction ->
                let outward = if starts then direction else Point.scale -1.0 direction
                Ok { EdgeId = edge.Id; Starts = starts; Angle = Point.heading outward })

    let private pairSectors edges =
        edges
        |> List.fold (fun state incoming ->
            state
            |> Result.bind (fun links ->
                let incident =
                    edges
                    |> List.filter (fun edge ->
                        edge.Layer = incoming.Layer &&
                        (edge.StartVertex = incoming.EndVertex || edge.EndVertex = incoming.EndVertex))
                incident
                |> List.fold (fun rayState edge ->
                    rayState
                    |> Result.bind (fun rays ->
                        if edge.StartVertex = incoming.EndVertex then ray edge true |> Result.map (fun value -> value :: rays)
                        else ray edge false |> Result.map (fun value -> value :: rays))) (Ok [])
                |> Result.bind (fun rays ->
                    let ordered = rays |> List.sortBy (fun value -> float value.Angle)
                    match ordered |> List.tryFindIndex (fun value -> value.EdgeId = incoming.Id && not value.Starts) with
                    | None -> Error(InternalBoundaryTopologyError(incoming.EndVertex, SectorMismatch))
                    | Some index when List.isEmpty ordered -> Error(InternalBoundaryTopologyError(incoming.EndVertex, SectorMismatch))
                    | Some index ->
                        let successor = ordered[(index + 1) % ordered.Length]
                        if successor.Starts then Ok(Map.add incoming.Id successor.EdgeId links)
                        else Error(InternalBoundaryTopologyError(incoming.EndVertex, SectorMismatch))))) (Ok Map.empty)

    let private trace edges links tolerance reversePositiveLayers =
        let byId = edges |> List.map (fun edge -> edge.Id, edge) |> Map.ofList
        let rec cycle seed current remaining segments limit =
            if limit <= 0 then Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
            else
                match Map.tryFind current.Id links with
                | None -> Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                | Some nextId when nextId = seed.Id -> Ok(List.rev (current.Segment :: segments), remaining)
                | Some nextId ->
                    match Map.tryFind nextId byId with
                    | None -> Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                    | Some next when not (Set.contains nextId remaining) ->
                        Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                    | Some next -> cycle seed next (Set.remove nextId remaining) (current.Segment :: segments) (limit - 1)
        let rec gather remaining contours =
            if Set.isEmpty remaining then Ok(List.rev contours)
            else
                let seedId = Set.minElement remaining
                let seed = byId[seedId]
                cycle seed seed (Set.remove seedId remaining) [] (Set.count remaining + 1)
                |> Result.bind (fun (segments, remaining) ->
                    Subpath.createWith (WiggleWith tolerance) segments
                    |> Result.bind (Subpath.setClosedWith (WiggleWith tolerance) true)
                    |> Result.mapError CsgPathError
                    |> Result.bind (fun subpath ->
                        let subpath = if reversePositiveLayers && seed.Layer > 0 then Subpath.reverse subpath else subpath
                        gather remaining (subpath :: contours)))
        gather (edges |> List.map _.Id |> Set.ofList) []

    let private booleanFromBuild operation fillRule options left right build =
        classify operation fillRule options.Tolerance left right build.Graph.Edges
        |> Result.bind (fun edges ->
            pairSectors edges
            |> Result.bind (fun links -> trace edges links options.Tolerance false))
        |> Result.map (Path.ofSubpaths >> fun path -> { Path = path; Build = build })

    let private booleanWith operation left right fillRule options =
        build [ left; right ] options
        |> Result.bind (booleanFromBuild operation fillRule options left right)

    let unionWith left right fillRule options = booleanWith Union left right fillRule options
    let union left right fillRule = unionWith left right fillRule defaultOptions

    let intersectionWith left right fillRule options = booleanWith Intersection left right fillRule options
    let intersection left right fillRule = intersectionWith left right fillRule defaultOptions

    let differenceWith left right fillRule options = booleanWith Difference left right fillRule options
    let difference left right fillRule = differenceWith left right fillRule defaultOptions

    let symmetricDifferenceWith left right fillRule options =
        booleanWith SymmetricDifference left right fillRule options
    let symmetricDifference left right fillRule =
        symmetricDifferenceWith left right fillRule defaultOptions

    let nestedContoursWith path options =
        build [ path ] options
        |> Result.bind (fun build ->
            Arrangement.nestedContoursFromGraph build.Graph path options.Tolerance
            |> Result.mapError CsgArrangementError
            |> Result.map (fun contours -> { Path = Path.ofSubpaths contours; Build = build }))

    let nestedContours path = nestedContoursWith path defaultOptions
