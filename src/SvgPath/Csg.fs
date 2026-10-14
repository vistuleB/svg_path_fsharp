namespace SvgPath

/// Boolean operations on filled SVG paths.
/// Open subpaths are implicitly closed by a line for filling; these closing
/// lines are also included in the returned arrangement build.
[<RequireQualifiedAccess>]
module Csg =

    [<Struct>]
    /// Finite positive tolerances. MinimumLength is the
    /// segment-length upper-bound threshold, not endpoint chord distance.
    type Options =
        { Tolerance: float<length>
          MinimumLength: float<length> }

    type BoundaryTopologyFailure =
        | SectorMismatch
        | TraceFailed

    type Error =
        | CsgArrangementError of error: Arrangement.Error
        | CsgPathError of error: SegmentError
        | InternalBoundaryTopologyError of vertex: int * reason: BoundaryTopologyFailure

    /// Binary output endpoints use the shared arrangement vertex positions, moving
    /// each endpoint by at most the build tolerance. Controls and arc parameters
    /// are retained; this does not bound the interior displacement of an arc.
    /// Stored arrangement edges remain unchanged.
    type CsgResult =
        { Path: Path
          Build: Arrangement.ArrangementSegmentBuild }

    let defaultOptions =
        { Tolerance = 1.0e-6<length>
          MinimumLength = 1.0e-5<length> }

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
          Starts: bool }

    let private pathSegments paths =
        paths
        |> List.collect Path.subpaths
        |> List.collect (fun subpath ->
                let segments = Subpath.segments subpath
                let start,finish = Subpath.start subpath,Subpath.finish subpath
                // Include implicit fill closure in the arrangement inventory.
                if List.isEmpty segments || Point.near 0.0<length> start finish then segments
                else segments @ [Line(finish,start)])

    let private build paths options =
        let segments = pathSegments paths
        Arrangement.buildWith segments options.Tolerance options.MinimumLength 0.0<parameter>
        |> Result.mapError (Arrangement.publicError >> CsgArrangementError)

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

    let private operandWindings (built: Arrangement.ArrangementSegmentBuild) dual leftCount isLeft =
        let changes : Arrangement.EdgeWindingChange list =
            built.EdgeImages |> List.map (fun image ->
                { EdgeId = image.EdgeId
                  RightMinusLeft = image.Sources |> List.sumBy (fun source ->
                    if (source.SegmentIndex < leftCount) <> isLeft then 0
                    elif source.Reversed then -1 else 1) })
        Arrangement.faceWindings dual changes
        |> Result.mapError (fun _ -> CsgArrangementError Arrangement.ConstructionFailed)
        |> Result.map (List.map (fun face -> face.FaceId, face.Value) >> Map.ofList)

    let private classify operation fillRule (dual: Arrangement.DualArrangementGraph)
        (left: Map<int,int>) (right: Map<int,int>) (edges: Arrangement.ArrangementEdge list) =
        let faces = dual.EdgeFaces |> List.map (fun face -> face.EdgeId, face) |> Map.ofList
        edges |> List.fold (fun state edge ->
            state |> Result.bind (fun boundary ->
                let invalid = Error(CsgArrangementError Arrangement.ConstructionFailed)
                match Map.tryFind edge.Id faces with
                | None -> invalid
                | Some face ->
                    match Map.tryFind face.LeftFace left, Map.tryFind face.RightFace left,
                          Map.tryFind face.LeftFace right, Map.tryFind face.RightFace right with
                    | Some leftA, Some rightA, Some leftB, Some rightB ->
                        let activeLeft = combine operation (filled leftA fillRule) (filled leftB fillRule)
                        let activeRight = combine operation (filled rightA fillRule) (filled rightB fillRule)
                        if activeLeft = activeRight then Ok boundary
                        elif activeRight then
                            Ok ({ Id=edge.Id; Layer=0; Segment=edge.Segment
                                  StartVertex=edge.StartVertex; EndVertex=edge.EndVertex } :: boundary)
                        else
                            Ok ({ Id=edge.Id; Layer=0; Segment=Segment.reverse edge.Segment
                                  StartVertex=edge.EndVertex; EndVertex=edge.StartVertex } :: boundary)
                    | _ -> invalid)) (Ok []) |> Result.map List.rev

    let private pairSectors (graph: Arrangement.ArrangementGraph) (edges: BoundaryEdge list) =
        let boundary = edges |> List.map (fun edge -> edge.Id, edge) |> Map.ofList
        let orders = Map.ofList graph.CyclicOrders
        edges |> List.fold (fun state incoming ->
            state |> Result.bind (fun links ->
                let vertex = incoming.EndVertex
                let invalid = Error(InternalBoundaryTopologyError(vertex, SectorMismatch))
                match Map.tryFind vertex orders with
                | None -> invalid
                | Some groups ->
                    // Preserve the arrangement's geometric clockwise order, including tangent ties.
                    let ordered = groups |> List.concat |> List.choose (fun incident ->
                        Map.tryFind incident.EdgeId boundary |> Option.bind (fun edge ->
                            if edge.Layer <> incoming.Layer then None
                            else Some {EdgeId=edge.Id; Starts=edge.StartVertex=vertex}))
                    match ordered |> List.tryFindIndex (fun ray -> ray.EdgeId=incoming.Id && not ray.Starts) with
                    | None -> invalid
                    | Some index ->
                        let successor = ordered[(index+1) % ordered.Length]
                        if successor.Starts then Ok(Map.add incoming.Id successor.EdgeId links)
                        else invalid)) (Ok Map.empty)

    let private trace edges links (vertices: Map<int, Point<length>>) reversePositiveLayers =
        // Replace only newly constructed output geometry, after sector pairing.
        let outputSegment edge =
            edge.Segment
            |> Segment.withStart vertices[edge.StartVertex]
            |> Segment.withFinish vertices[edge.EndVertex]
        let byId = edges |> List.map (fun edge -> edge.Id, edge) |> Map.ofList
        let rec cycle seed current remaining segments limit =
            if limit <= 0 then Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
            else
                match Map.tryFind current.Id links with
                | None -> Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                | Some nextId when nextId = seed.Id -> Ok(List.rev (outputSegment current :: segments), remaining)
                | Some nextId ->
                    match Map.tryFind nextId byId with
                    | None -> Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                    | Some next when not (Set.contains nextId remaining) ->
                        Error(InternalBoundaryTopologyError(current.EndVertex, TraceFailed))
                    | Some next -> cycle seed next (Set.remove nextId remaining) (outputSegment current :: segments) (limit - 1)
        let rec gather remaining contours =
            if Set.isEmpty remaining then Ok(List.rev contours)
            else
                let seedId = Set.minElement remaining
                let seed = byId[seedId]
                cycle seed seed (Set.remove seedId remaining) [] (Set.count remaining + 1)
                |> Result.bind (fun (segments, remaining) ->
                    let reduced = segments |> List.fold (fun stack segment ->
                        match stack with
                        | previous :: rest when previous = Segment.reverse segment -> rest
                        | _ -> segment :: stack) []
                    if List.isEmpty reduced then gather remaining contours
                    else
                        Subpath.create segments
                        |> Result.bind (Subpath.close)
                        |> Result.mapError CsgPathError
                        |> Result.bind (fun subpath ->
                            let subpath = if reversePositiveLayers && seed.Layer > 0 then Subpath.reverse subpath else subpath
                            gather remaining (subpath :: contours)))
        gather (edges |> List.map _.Id |> Set.ofList) []

    let private booleanFromBuild operation fillRule leftCount (build: Arrangement.ArrangementSegmentBuild) =
        Arrangement.dual build.Graph |> Result.mapError CsgArrangementError
        |> Result.bind (fun dual ->
            operandWindings build dual leftCount true |> Result.bind (fun left ->
                operandWindings build dual leftCount false |> Result.bind (fun right ->
                    classify operation fillRule dual left right build.Graph.Edges)))
        |> Result.bind (fun edges ->
            pairSectors build.Graph edges
            |> Result.bind (fun links -> trace edges links (build.Graph.Vertices |> List.map (fun vertex -> vertex.Id, vertex.Point) |> Map.ofList) false))
        |> Result.map (Path.ofSubpaths >> fun path -> { Path = path; Build = build })

    let private booleanWith operation left right fillRule options =
        build [ left; right ] options
        |> Result.bind (booleanFromBuild operation fillRule (pathSegments [left] |> List.length))

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
            |> Result.mapError (Arrangement.publicError >> CsgArrangementError)
            |> Result.map (fun contours -> { Path = Path.ofSubpaths contours; Build = build }))

    let nestedContours path = nestedContoursWith path defaultOptions

    /// Return only the reconstructed path. Use union for arrangement/source data.
    let unionPathWith left right fillRule options =
        unionWith left right fillRule options |> Result.map (fun output -> output.Path)

    /// Return only the reconstructed path using default numerical options.
    let unionPath left right fillRule = unionPathWith left right fillRule defaultOptions

    /// Return only the reconstructed path. Use intersection for arrangement/source data.
    let intersectionPathWith left right fillRule options =
        intersectionWith left right fillRule options |> Result.map (fun output -> output.Path)

    /// Return only the reconstructed path using default numerical options.
    let intersectionPath left right fillRule = intersectionPathWith left right fillRule defaultOptions

    /// Return only the reconstructed path. Use difference for arrangement/source data.
    let differencePathWith left right fillRule options =
        differenceWith left right fillRule options |> Result.map (fun output -> output.Path)

    /// Return only the reconstructed path using default numerical options.
    let differencePath left right fillRule = differencePathWith left right fillRule defaultOptions

    /// Return only the reconstructed path. Use symmetricDifference for arrangement/source data.
    let symmetricDifferencePathWith left right fillRule options =
        symmetricDifferenceWith left right fillRule options |> Result.map (fun output -> output.Path)

    /// Return only the reconstructed path using default numerical options.
    let symmetricDifferencePath left right fillRule = symmetricDifferencePathWith left right fillRule defaultOptions

    /// Preserve signed winding levels without retaining the arrangement build.
    let nestedContoursPathWith path options =
        nestedContoursWith path options |> Result.map (fun output -> output.Path)

    let nestedContoursPath path = nestedContoursPathWith path defaultOptions
