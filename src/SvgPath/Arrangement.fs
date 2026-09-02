namespace SvgPath

// Arrangement graph translation.  The graph representation deliberately keeps
// source segment endpoints rather than snapping edge geometry to vertex centres.

[<Struct>]
type ArrangementVertex =
    { Id: int
      Point: Point<length>
      EndpointSamples: Point<length> list }

[<Struct>]
type ArrangementEdge =
    { Id: int
      Segment: Segment
      Bounds: BoundingBox
      StartVertex: int
      EndVertex: int
      ForwardMultiplicity: int
      ReverseMultiplicity: int }

[<Struct>]
type OrientedArrangementEdge = { EdgeId: int; Reversed: bool }

type ArrangementGraph =
    { Vertices: ArrangementVertex list
      Edges: ArrangementEdge list
      CyclicOrders: (int * OrientedArrangementEdge list list) list }

[<Struct>]
type EdgeCapacityAssignment = { EdgeId: int; Capacity: int }

type VertexParityRequest =
    | RequiredVertexParity of vertex: int * parity: int
    | PreferredVertexParity of vertex: int * parity: int

type ForcedParityError =
    | ForcedParityMissingVertex of int
    | ForcedParityDuplicateVertex of int
    | ForcedParityInvalidVertexParity of vertex: int * parity: int
    | ForcedParityMissingEdgeCapacity of int
    | ForcedParityDuplicateEdgeCapacity of int
    | ForcedParityUnknownEdgeCapacity of int
    | ForcedParityInvalidEdgeCapacity of edge: int * capacity: int
    | ForcedParityInfeasible of int
    | ForcedParityAmbiguous of int list

type ArrangementFaceEdge = { EdgeId: int; Left: bool }
type ArrangementFaceWalk = { Outer: bool; Edges: ArrangementFaceEdge list }
type ArrangementFace = { Id: int; Outer: bool; Walks: ArrangementFaceWalk list }
type ArrangementEdgeFaces = { EdgeId: int; LeftFace: int; RightFace: int }
type DualArrangementGraph = { Faces: ArrangementFace list; EdgeFaces: ArrangementEdgeFaces list }

[<Struct>]
type DirectedEdgeReference = { EdgeId: int; Reversed: bool }

type ArrangementSegmentImage =
    { PathIndex: int
      SubpathIndex: int
      SegmentIndex: int
      Edges: DirectedEdgeReference list }

type ArrangementGraphBuild =
    { Graph: ArrangementGraph
      SegmentImages: ArrangementSegmentImage list }

[<Struct>]
type ArrangementSegmentEdgeImage =
    { From: float<parameter>
      To: float<parameter>
      EdgeId: int
      Reversed: bool
      Own: bool }

type ArrangementSourceSegmentImage =
    { SegmentIndex: int
      Edges: ArrangementSegmentEdgeImage list }

[<Struct>]
type ArrangementEdgeSourceImage =
    { SegmentIndex: int
      From: float<parameter>
      To: float<parameter>
      Reversed: bool }

type ArrangementEdgeImage = { EdgeId: int; Sources: ArrangementEdgeSourceImage list }

type ArrangementSegmentBuild =
    { Graph: ArrangementGraph
      Segments: Segment list
      SegmentImages: ArrangementSourceSegmentImage list
      EdgeImages: ArrangementEdgeImage list }

type ArrangementError =
    | ArrangementSegmentError of SegmentError
    | InternalNormalizationError
    | InvalidArrangementTolerance of float<length>
    | InvalidMinimumChord of float<length>
    | InvalidEndpointSliverTolerance of float<parameter>
    | SegmentTooShort of chord: float<length> * minimum: float<length>
    | SegmentCollapsedToVertex of int
    | LoopEdge of int
    | MissingArrangementVertex of int
    | MissingArrangementEdge of int
    | IsolatedVertex of int
    | InvalidMultiplicity of int
    | OddWeightedDegree of vertex: int * degree: int
    | EdgeEndpointMismatch of edge: int * vertex: int * distance: float<length>
    | VertexWithoutEndpointSamples of int
    | VertexCenterMismatch of vertex: int * distanceSquared: float<length^2>
    | VertexSampleOutsideTolerance of vertex: int * distanceSquared: float<length^2> * toleranceSquared: float<length^2>
    | ContourTraceFailed of int
    | CyclicOrderMissingVertex of int
    | CyclicOrderRadiusUnavailable of int
    | InvalidCyclicOrderAttempts of int
    | CyclicOrderCircleIntersectionFailed of vertex: int * edge: int * radius: float<length>
    | DualMissingCyclicOrder of int
    | DualMissingIncidentEdge of vertex: int * edge: int
    | DualWalkDidNotClose of edge: int * left: bool
    | DualFaceSampleUnavailable of edge: int * left: bool
    | DualInvalidOuterWalkCount of int
    | DualMissingEdgeFace of edge: int * left: bool
    | DualInvalidOuterFaceCount of int

[<RequireQualifiedAccess>]
module Arrangement =
    let empty = { Vertices = []; Edges = []; CyclicOrders = [] }

    let private requestData = function
        | RequiredVertexParity(vertex, parity) -> vertex, parity, false
        | PreferredVertexParity(vertex, parity) -> vertex, parity, true

    let private validateParityRequests (graph: ArrangementGraph) requests =
        let rec loop seen = function
            | [] -> Ok ()
            | request :: rest ->
                let vertex, parity, _ = requestData request
                if parity <> 0 && parity <> 1 then Error(ForcedParityInvalidVertexParity(vertex, parity))
                elif Set.contains vertex seen then Error(ForcedParityDuplicateVertex vertex)
                elif not (graph.Vertices |> List.exists (fun candidate -> candidate.Id = vertex)) then
                    Error(ForcedParityMissingVertex vertex)
                else loop (Set.add vertex seen) rest
        loop Set.empty requests

    let private validateCapacities (graph: ArrangementGraph) (assignments: EdgeCapacityAssignment list) =
        let ids = graph.Edges |> List.map _.Id |> Set.ofList
        let rec loop seen = function
            | [] ->
                graph.Edges
                |> List.tryFind (fun edge -> not (Set.contains edge.Id seen))
                |> function
                    | Some edge -> Error(ForcedParityMissingEdgeCapacity edge.Id)
                    | None -> Ok ()
            | assignment :: rest when assignment.Capacity < 0 ->
                Error(ForcedParityInvalidEdgeCapacity(assignment.EdgeId, assignment.Capacity))
            | assignment :: _ when not (Set.contains assignment.EdgeId ids) ->
                Error(ForcedParityUnknownEdgeCapacity assignment.EdgeId)
            | assignment :: _ when Set.contains assignment.EdgeId seen ->
                Error(ForcedParityDuplicateEdgeCapacity assignment.EdgeId)
            | assignment :: rest -> loop (Set.add assignment.EdgeId seen) rest
        loop Set.empty assignments

    let forcedParityCapacitiesWith (graph: ArrangementGraph) (initialCapacities: EdgeCapacityAssignment list) vertexParities =
        validateParityRequests graph vertexParities
        |> Result.bind (fun () -> validateCapacities graph initialCapacities)
        |> Result.bind (fun () ->
            let requestFor vertex =
                vertexParities
                |> List.tryPick (fun request ->
                    let requested, parity, preferred = requestData request
                    if requested = vertex then Some(parity, preferred) else None)
                |> Option.defaultValue (0, false)
            let rec reduce (assignments: EdgeCapacityAssignment list) =
                let capacity edgeId = assignments |> List.find (fun (a: EdgeCapacityAssignment) -> a.EdgeId = edgeId) |> _.Capacity
                let states =
                    graph.Vertices
                    |> List.map (fun vertex ->
                        let parity, preferred = requestFor vertex.Id
                        let incident =
                            graph.Edges
                            |> List.filter (fun edge -> edge.StartVertex = vertex.Id || edge.EndVertex = vertex.Id)
                            |> List.map (fun edge -> edge.Id, capacity edge.Id)
                        let total = incident |> List.sumBy snd
                        vertex.Id, parity, preferred, total, incident |> List.filter (snd >> ((<) 0)) |> List.map fst)
                let mismatched =
                    states
                    |> List.filter (fun (_, parity, preferred, total, _) ->
                        total % 2 <> parity && not (preferred && total = 0))
                match mismatched |> List.tryFind (fun (_, _, _, _, positive) -> List.isEmpty positive) with
                | Some(vertex, _, _, _, _) -> Error(ForcedParityInfeasible vertex)
                | None ->
                    let choose positive =
                        let rec at threshold =
                            match positive |> List.filter (fun edgeId -> capacity edgeId >= threshold) with
                            | [ edgeId ] -> Some edgeId
                            | [] -> None
                            | _ -> at (threshold + 1)
                        at 1
                    match mismatched |> List.tryPick (fun (_, _, _, _, positive) -> choose positive) with
                    | Some edgeId ->
                        assignments
                        |> List.map (fun (assignment: EdgeCapacityAssignment) ->
                            if assignment.EdgeId = edgeId then { assignment with Capacity = assignment.Capacity - 1 }
                            else assignment)
                        |> reduce
                    | None when List.isEmpty mismatched -> Ok assignments
                    | None -> Error(ForcedParityAmbiguous(mismatched |> List.map (fun (vertex, _, _, _, _) -> vertex)))
            reduce initialCapacities)

    let forcedParityCapacities (graph: ArrangementGraph) vertexParities =
        graph.Edges
        |> List.map (fun (edge: ArrangementEdge) ->
            { EdgeId = edge.Id
              Capacity = edge.ForwardMultiplicity + edge.ReverseMultiplicity })
        |> fun capacities -> forcedParityCapacitiesWith graph capacities vertexParities

    let private finite (value: float<length>) = not (System.Double.IsNaN(float value) || System.Double.IsInfinity(float value))

    let private attachVertex tolerance point (vertices: ArrangementVertex list) =
        let candidates =
            vertices
            |> List.choose (fun vertex ->
                let samples = point :: vertex.EndpointSamples
                match SmallestEnclosingCircle.points samples with
                | Ok circle when circle.RadiusSquared <= tolerance * tolerance -> Some(vertex, circle, samples)
                | _ -> None)
        match candidates |> List.sortBy (fun (vertex, circle, _) -> circle.RadiusSquared, vertex.Id) |> List.tryHead with
        | Some(vertex, circle, samples) ->
            vertices
            |> List.map (fun existing ->
                if existing.Id = vertex.Id then { existing with Point = circle.Center; EndpointSamples = samples }
                else existing), vertex.Id
        | None ->
            let id = vertices |> List.fold (fun maximum vertex -> max maximum vertex.Id) -1 |> (+) 1
            vertices @ [ { Id = id; Point = point; EndpointSamples = [ point ] } ], id

    let insertAtomicSegment (graph: ArrangementGraph) segment tolerance minimumChord =
        if tolerance <= 0.0<length> || not (finite tolerance) then Error(InvalidArrangementTolerance tolerance)
        elif minimumChord <= 0.0<length> || not (finite minimumChord) then Error(InvalidMinimumChord minimumChord)
        elif Segment.chordLength segment < minimumChord then Error(SegmentTooShort(Segment.chordLength segment, minimumChord))
        else
            let vertices, startVertex = attachVertex tolerance (Segment.start segment) graph.Vertices
            let vertices, endVertex = attachVertex tolerance (Segment.finish segment) vertices
            if startVertex = endVertex then Error(SegmentCollapsedToVertex startVertex)
            else
                let forward = graph.Edges |> List.tryFind (fun edge -> edge.StartVertex = startVertex && edge.EndVertex = endVertex && edge.Segment = segment)
                let reverseSegment = Segment.reverse segment
                let reverse = graph.Edges |> List.tryFind (fun edge -> edge.StartVertex = endVertex && edge.EndVertex = startVertex && edge.Segment = reverseSegment)
                let edges =
                    match forward, reverse with
                    | Some matching, _ -> graph.Edges |> List.map (fun edge -> if edge.Id = matching.Id then { edge with ForwardMultiplicity = edge.ForwardMultiplicity + 1 } else edge)
                    | None, Some matching -> graph.Edges |> List.map (fun edge -> if edge.Id = matching.Id then { edge with ReverseMultiplicity = edge.ReverseMultiplicity + 1 } else edge)
                    | None, None -> graph.Edges
                match forward, reverse with
                | None, None ->
                    Segment.boundingBox segment
                    |> Result.mapError ArrangementSegmentError
                    |> Result.map (fun bounds ->
                        let id = graph.Edges |> List.fold (fun maximum edge -> max maximum edge.Id) -1 |> (+) 1
                        { Vertices = vertices
                          Edges = edges @ [ { Id = id; Segment = segment; Bounds = bounds; StartVertex = startVertex; EndVertex = endVertex; ForwardMultiplicity = 1; ReverseMultiplicity = 0 } ]
                          CyclicOrders = [] })
                | _ -> Ok { Vertices = vertices; Edges = edges; CyclicOrders = [] }

    let validate (graph: ArrangementGraph) tolerance minimumChord =
        if tolerance <= 0.0<length> then Error(InvalidArrangementTolerance tolerance)
        elif minimumChord <= 0.0<length> then Error(InvalidMinimumChord minimumChord)
        else
            let vertex id = graph.Vertices |> List.tryFind (fun item -> item.Id = id)
            let edgeError =
                graph.Edges
                |> List.tryPick (fun edge ->
                    if edge.ForwardMultiplicity + edge.ReverseMultiplicity <= 0 then Some(InvalidMultiplicity edge.Id)
                    elif edge.StartVertex = edge.EndVertex then Some(LoopEdge edge.StartVertex)
                    elif vertex edge.StartVertex |> Option.isNone then Some(MissingArrangementVertex edge.StartVertex)
                    elif vertex edge.EndVertex |> Option.isNone then Some(MissingArrangementVertex edge.EndVertex)
                    elif Segment.chordLength edge.Segment < minimumChord then Some(SegmentTooShort(Segment.chordLength edge.Segment, minimumChord))
                    else
                        let startDistance = Point.distance (Segment.start edge.Segment) (vertex edge.StartVertex).Value.Point
                        let endDistance = Point.distance (Segment.finish edge.Segment) (vertex edge.EndVertex).Value.Point
                        if startDistance > tolerance then Some(EdgeEndpointMismatch(edge.Id, edge.StartVertex, startDistance))
                        elif endDistance > tolerance then Some(EdgeEndpointMismatch(edge.Id, edge.EndVertex, endDistance))
                        else None)
            match edgeError with
            | Some error -> Error error
            | None ->
                graph.Vertices
                |> List.tryPick (fun vertex ->
                    if List.isEmpty vertex.EndpointSamples then Some(VertexWithoutEndpointSamples vertex.Id)
                    else
                        let degree = graph.Edges |> List.filter (fun edge -> edge.StartVertex = vertex.Id || edge.EndVertex = vertex.Id) |> List.sumBy (fun edge -> edge.ForwardMultiplicity + edge.ReverseMultiplicity)
                        if degree = 0 then Some(IsolatedVertex vertex.Id)
                        elif degree % 2 <> 0 then Some(OddWeightedDegree(vertex.Id, degree))
                        else
                            match SmallestEnclosingCircle.points vertex.EndpointSamples with
                            | Error _ -> Some(VertexWithoutEndpointSamples vertex.Id)
                            | Ok circle when circle.Center <> vertex.Point -> Some(VertexCenterMismatch(vertex.Id, Point.squaredDistance circle.Center vertex.Point))
                            | Ok circle when circle.RadiusSquared > tolerance * tolerance -> Some(VertexSampleOutsideTolerance(vertex.Id, circle.RadiusSquared, tolerance * tolerance))
                            | _ -> None)
                |> function Some error -> Error error | None -> Ok ()

    type private CyclicSample =
        { OrientedEdge: OrientedArrangementEdge
          Point: Point<length>
          Angle: float<degree> }

    let private orientedSegment (graph: ArrangementGraph) (oriented: OrientedArrangementEdge) =
        graph.Edges
        |> List.tryFind (fun (edge: ArrangementEdge) -> edge.Id = oriented.EdgeId)
        |> function
            | None -> Error(MissingArrangementEdge oriented.EdgeId)
            | Some edge -> Ok(if oriented.Reversed then Segment.reverse edge.Segment else edge.Segment)

    let private incidentEdges (graph: ArrangementGraph) vertex : OrientedArrangementEdge list =
        graph.Edges
        |> List.collect (fun (edge: ArrangementEdge) ->
            [ if edge.StartVertex = vertex then yield ({ EdgeId = edge.Id; Reversed = false }: OrientedArrangementEdge)
              if edge.EndVertex = vertex then yield ({ EdgeId = edge.Id; Reversed = true }: OrientedArrangementEdge) ])

    let private circleSample (graph: ArrangementGraph) (vertex: Point<length>) vertexId (radius: float<length>) tolerance (oriented: OrientedArrangementEdge) =
        orientedSegment graph oriented
        |> Result.bind (fun segment ->
            let residual (t: float<parameter>) =
                Segment.point segment t
                |> Result.mapError ArrangementSegmentError
                |> Result.map (fun point -> Point.squaredDistance point vertex - radius * radius)
            let samples = 256
            let rec bracket index (previousT: float<parameter>) previousValue =
                if index > samples then None
                else
                    let t = Parameter.fromFloat (float index / float samples)
                    match residual t with
                    | Error _ -> None
                    | Ok value when value >= 0.0<length^2> -> Some(previousT, t)
                    | Ok value -> bracket (index + 1) t value
            match residual 0.0<parameter> with
            | Error error -> Error error
            | Ok initial ->
                match bracket 1 0.0<parameter> initial with
                | None -> Error(CyclicOrderCircleIntersectionFailed(vertexId, oriented.EdgeId, radius))
                | Some(lower, upper) ->
                    let rec bisect (lower: float<parameter>) (upper: float<parameter>) remaining =
                        if remaining = 0 then upper
                        else
                            let middle = Parameter.fromFloat ((Parameter.ratio lower + Parameter.ratio upper) / 2.0)
                            match residual middle with
                            | Ok value when value >= 0.0<length^2> -> bisect lower middle (remaining - 1)
                            | _ -> bisect middle upper (remaining - 1)
                    let t = bisect lower upper 48
                    Segment.point segment t
                    |> Result.mapError ArrangementSegmentError
                    |> Result.map (fun point ->
                        { OrientedEdge = oriented
                          Point = point
                          Angle = Point.heading (Point.displacement vertex point) }))

    let private separated tolerance (first: CyclicSample) (second: CyclicSample) =
        let rawAngle = abs (Degree.toFloat second.Angle - Degree.toFloat first.Angle)
        let angle = min rawAngle (360.0 - rawAngle)
        Point.distance first.Point second.Point > tolerance || angle >= 0.1

    let private groupSamples tolerance (samples: CyclicSample list) =
        match samples with
        | [] -> []
        | first :: rest ->
            let groups, current, _ =
                rest
                |> List.fold (fun (groups, current, previous) sample ->
                    if separated tolerance previous sample then groups @ [ current ], [ sample ], sample
                    else groups, current @ [ sample ], sample) ([], [ first ], first)
            let groups = groups @ [ current ]
            match groups with
            | firstGroup :: middle when groups.Length > 1 ->
                let lastGroup = List.last groups
                if separated tolerance (List.last lastGroup) first then groups
                else (lastGroup @ firstGroup) :: (middle |> List.take (middle.Length - 1))
            | _ -> groups

    let private clockwisePrecedes left right =
        let raw = Degree.toFloat right - Degree.toFloat left
        let delta = if raw < 0.0 then raw + 360.0 else raw
        delta > 0.0 && delta < 180.0

    let private orderAmbiguousGroups (groups: CyclicSample list list) (samplesByRadius: CyclicSample list list) =
        let angle (samples: CyclicSample list) (edge: OrientedArrangementEdge) = samples |> List.find (fun sample -> sample.OrientedEdge = edge) |> _.Angle
        groups
        |> List.map (fun group ->
            let edges = group |> List.map _.OrientedEdge
            edges
            |> List.sortBy (fun (candidate: OrientedArrangementEdge) ->
                let score =
                    edges
                    |> List.filter ((<>) candidate)
                    |> List.sumBy (fun other ->
                        samplesByRadius
                        |> List.sumBy (fun samples -> if clockwisePrecedes (angle samples candidate) (angle samples other) then 1 else 0))
                -score, candidate.EdgeId, candidate.Reversed))

    let vertexCyclicOrderWith (graph: ArrangementGraph) vertexId tolerance maxAttempts =
        if maxAttempts <= 0 then Error(InvalidCyclicOrderAttempts maxAttempts)
        elif tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then Error(InvalidArrangementTolerance tolerance)
        else
            match graph.Vertices |> List.tryFind (fun vertex -> vertex.Id = vertexId) with
            | None -> Error(CyclicOrderMissingVertex vertexId)
            | Some vertex ->
                let incident: OrientedArrangementEdge list = incidentEdges graph vertexId
                match incident with
                | [] -> Error(IsolatedVertex vertexId)
                | [ only ] -> Ok [ [ only ] ]
                | _ ->
                    incident
                    |> List.fold (fun state oriented ->
                        state
                        |> Result.bind (fun distances ->
                            orientedSegment graph oriented
                            |> Result.map (fun segment -> Point.distance vertex.Point (Segment.finish segment) :: distances))) (Ok [])
                    |> Result.bind (fun distances ->
                        let radius = 0.8 * List.min distances
                        if radius <= 0.0<length> || not (System.Double.IsFinite(float radius)) then Error(CyclicOrderRadiusUnavailable vertexId)
                        else
                            let rec attempts (radius: float<length>) remaining (successes: CyclicSample list list) (previousError: ArrangementError option) =
                                if remaining <= 0 || radius <= tolerance / 2.0 then
                                    match List.rev successes with
                                    | [] -> Error(defaultArg previousError (CyclicOrderRadiusUnavailable vertexId))
                                    | reference :: _ as byRadius ->
                                        reference |> groupSamples tolerance |> fun groups -> Ok(orderAmbiguousGroups groups byRadius)
                                else
                                    incident
                                    |> List.fold (fun state (edge: OrientedArrangementEdge) ->
                                        state
                                        |> Result.bind (fun samples -> circleSample graph vertex.Point vertexId radius tolerance edge |> Result.map (fun sample -> sample :: samples))) (Ok [])
                                    |> function
                                        | Ok samples -> attempts (radius * 0.8) (remaining - 1) ((List.sortBy _.Angle samples) :: successes) previousError
                                        | Error error -> attempts (radius * 0.8) (remaining - 1) successes (Some error)
                            attempts radius maxAttempts [] None)

    /// Compute clockwise SVG-space incident-edge orders by sampling each edge
    /// on common shrinking circles around its vertex.
    let cyclicOrdersWith (graph: ArrangementGraph) tolerance maxAttempts =
        if maxAttempts <= 0 then Error(InvalidCyclicOrderAttempts maxAttempts)
        elif tolerance <= 0.0<length> then Error(InvalidArrangementTolerance tolerance)
        else
            graph.Vertices
            |> List.fold (fun state vertex ->
                state
                |> Result.bind (fun orders ->
                    vertexCyclicOrderWith graph vertex.Id tolerance maxAttempts
                    |> Result.map (fun groups -> orders @ [ vertex.Id, groups ]))) (Ok [])

    let cyclicOrders graph tolerance = cyclicOrdersWith graph tolerance 3

    type private IndexedSegment =
        { FlatIndex: int
          PathIndex: int
          SubpathIndex: int
          SegmentIndex: int
          Segment: Segment }

    let private indexPaths (paths: Path list) =
        paths
        |> List.indexed
        |> List.collect (fun (pathIndex, path) ->
            path.Subpaths
            |> List.indexed
            |> List.collect (fun (subpathIndex, subpath) ->
                subpath.Segments
                |> List.indexed
                |> List.map (fun (segmentIndex, segment) ->
                    pathIndex, subpathIndex, segmentIndex, segment)))
        |> List.indexed
        |> List.map (fun (flatIndex, (pathIndex, subpathIndex, segmentIndex, segment)) ->
            { FlatIndex = flatIndex
              PathIndex = pathIndex
              SubpathIndex = subpathIndex
              SegmentIndex = segmentIndex
              Segment = segment })

    let private addCut value cuts =
        let value = max 0.0<parameter> (min 1.0<parameter> value)
        if cuts |> List.exists (fun existing -> abs (existing - value) <= 1.0e-10<parameter>) then cuts
        else value :: cuts

    let private endpointSides t sliver =
        [ if t <= sliver then yield true
          if 1.0<parameter> - t <= sliver then yield false ]

    let private commonEndpointSliver tolerance sliver left right leftT rightT =
        if sliver <= 0.0<parameter> then false
        else
            let endpoint segment start = if start then Segment.start segment else Segment.finish segment
            endpointSides leftT sliver
            |> List.exists (fun leftStart ->
                endpointSides rightT sliver
                |> List.exists (fun rightStart ->
                    Point.distance (endpoint left leftStart) (endpoint right rightStart) <= tolerance))

    let private collectPairCuts tolerance endpointSliverTolerance left right leftCuts rightCuts =
        match Overlaps.segmentWith left right tolerance with
        | Error error -> Error(ArrangementSegmentError error)
        | Ok (_ :: _ as overlaps) ->
            let leftCuts = overlaps |> List.fold (fun cuts overlap -> cuts |> addCut overlap.LeftFrom |> addCut overlap.LeftTo) leftCuts
            let rightCuts = overlaps |> List.fold (fun cuts overlap -> cuts |> addCut overlap.RightFrom |> addCut overlap.RightTo) rightCuts
            Ok(leftCuts, rightCuts)
        | Ok [] ->
            match Intersections.segmentWith left right { Intersections.defaultOptions with Tolerance = tolerance } with
            | Error error -> Error(ArrangementSegmentError error)
            | Ok intersections ->
                let intersections =
                    intersections
                    |> List.filter (fun (hit: SegmentIntersection) ->
                        not (commonEndpointSliver tolerance endpointSliverTolerance left right hit.LeftT hit.RightT))
                Ok(
                    intersections |> List.fold (fun cuts (hit: SegmentIntersection) -> addCut hit.LeftT cuts) leftCuts,
                    intersections |> List.fold (fun cuts (hit: SegmentIntersection) -> addCut hit.RightT cuts) rightCuts)

    let private endpointCuts tolerance endpointSliverTolerance source target sourceCuts targetCuts =
        [ 0.0<parameter>; 1.0<parameter> ]
        |> List.fold (fun state sourceT ->
            state
            |> Result.bind (fun (sourceCuts, targetCuts) ->
                Segment.point source sourceT
                |> Result.mapError ArrangementSegmentError
                |> Result.bind (fun endpoint ->
                    Segment.projection target endpoint
                    |> Result.mapError ArrangementSegmentError
                    |> Result.map (fun (targetT, _, distance) ->
                        if distance <= tolerance
                           && not (commonEndpointSliver tolerance endpointSliverTolerance source target sourceT targetT) then
                            addCut sourceT sourceCuts, addCut targetT targetCuts
                        else sourceCuts, targetCuts)))) (Ok(sourceCuts, targetCuts))

    let private locateEdge (graph: ArrangementGraph) segment =
        graph.Edges
        |> List.tryPick (fun (edge: ArrangementEdge) ->
            if edge.Segment = segment then Some(edge.Id, false)
            elif edge.Segment = Segment.reverse segment then Some(edge.Id, true)
            else None)

    /// Build directly from a flat segment list without source normalization.
    let buildWith segments vertexTolerance minimumChord (endpointSliverTolerance: float<parameter>) =
        if vertexTolerance <= 0.0<length> then Error(InvalidArrangementTolerance vertexTolerance)
        elif minimumChord <= 0.0<length> then Error(InvalidMinimumChord minimumChord)
        elif endpointSliverTolerance < 0.0<parameter> || System.Double.IsNaN(float endpointSliverTolerance) || System.Double.IsInfinity(float endpointSliverTolerance) then
            Error(InvalidEndpointSliverTolerance endpointSliverTolerance)
        else
            let count = List.length segments
            let cuts = Array.init count (fun _ -> [ 0.0<parameter>; 1.0<parameter> ])
            let segmentArray = List.toArray segments
            let mutable failure = None
            for leftIndex in 0 .. count - 1 do
                for rightIndex in leftIndex + 1 .. count - 1 do
                    if failure.IsNone then
                        match collectPairCuts vertexTolerance endpointSliverTolerance segmentArray[leftIndex] segmentArray[rightIndex] cuts[leftIndex] cuts[rightIndex] with
                        | Error error -> failure <- Some error
                        | Ok(leftCuts, rightCuts) ->
                            match endpointCuts vertexTolerance endpointSliverTolerance segmentArray[leftIndex] segmentArray[rightIndex] leftCuts rightCuts with
                            | Error error -> failure <- Some error
                            | Ok(leftCuts, rightCuts) ->
                                match endpointCuts vertexTolerance endpointSliverTolerance segmentArray[rightIndex] segmentArray[leftIndex] rightCuts leftCuts with
                                | Error error -> failure <- Some error
                                | Ok(rightCuts, leftCuts) -> cuts[leftIndex] <- leftCuts; cuts[rightIndex] <- rightCuts
            match failure with
            | Some error -> Error error
            | None ->
                let mutable graph: ArrangementGraph = empty
                let mutable insertionError: ArrangementError option = None
                let mutable images: ArrangementSourceSegmentImage list = []
                for index in 0 .. count - 1 do
                    let ordered = cuts[index] |> List.sort
                    let mutable edgeImages: ArrangementSegmentEdgeImage list = []
                    for fromValue, toValue in List.pairwise ordered do
                        if insertionError.IsNone then
                            match Segment.betweenInside segmentArray[index] fromValue toValue with
                            | Error error -> insertionError <- Some(ArrangementSegmentError error)
                            | Ok piece when Segment.chordLength piece < minimumChord -> ()
                            | Ok piece ->
                                match insertAtomicSegment graph piece vertexTolerance minimumChord with
                                | Error error -> insertionError <- Some error
                                | Ok nextGraph ->
                                    graph <- nextGraph
                                    match locateEdge graph piece with
                                    | None -> insertionError <- Some(MissingArrangementEdge -1)
                                    | Some(edgeId, reversed) ->
                                        edgeImages <- edgeImages @ [ { From = fromValue; To = toValue; EdgeId = edgeId; Reversed = reversed; Own = false } ]
                    images <- images @ [ { SegmentIndex = index; Edges = edgeImages } ]
                match insertionError with
                | Some error -> Error error
                | None ->
                    cyclicOrders graph vertexTolerance
                    |> Result.map (fun orders ->
                        let graph = { graph with CyclicOrders = orders }
                        let images, _ =
                            images
                            |> List.mapFold (fun owned image ->
                                let edges, owned =
                                    image.Edges
                                    |> List.mapFold (fun owned occurrence ->
                                        let own = not (Set.contains occurrence.EdgeId owned)
                                        { occurrence with Own = own }, Set.add occurrence.EdgeId owned) owned
                                { image with Edges = edges }, owned) Set.empty
                        let edgeImages =
                            graph.Edges
                            |> List.map (fun edge ->
                                { EdgeId = edge.Id
                                  Sources =
                                    images
                                    |> List.collect (fun image ->
                                        image.Edges
                                        |> List.choose (fun occurrence ->
                                            if occurrence.EdgeId = edge.Id then
                                                Some { SegmentIndex = image.SegmentIndex; From = occurrence.From; To = occurrence.To; Reversed = occurrence.Reversed }
                                            else None)) })
                        { Graph = graph; Segments = segments; SegmentImages = images; EdgeImages = edgeImages })

    /// Build an arrangement and preserve each input path segment's edge image.
    let build (paths: Path list) tolerance minimumChord =
        let indexed = indexPaths paths
        let segments = indexed |> List.map _.Segment
        buildWith segments tolerance minimumChord 0.0<parameter>
        |> Result.map (fun built ->
            let images =
                List.zip indexed built.SegmentImages
                |> List.map (fun (source, image) ->
                    { PathIndex = source.PathIndex
                      SubpathIndex = source.SubpathIndex
                      SegmentIndex = source.SegmentIndex
                      Edges = image.Edges |> List.map (fun edge -> { EdgeId = edge.EdgeId; Reversed = edge.Reversed }) })
            { Graph = built.Graph; SegmentImages = images })

    let segmentImageEdges build image =
        image.Edges
        |> List.fold (fun state reference ->
            state
            |> Result.bind (fun edges ->
                match build.Graph.Edges |> List.tryFind (fun edge -> edge.Id = reference.EdgeId) with
                | Some edge -> Ok(edges @ [ edge, reference.Reversed ])
                | None -> Error(MissingArrangementEdge reference.EdgeId))) (Ok [])

    let private faceEdgeEqual (left: ArrangementFaceEdge) (right: ArrangementFaceEdge) = left.EdgeId = right.EdgeId && left.Left = right.Left

    let private faceSuccessor (graph: ArrangementGraph) (current: ArrangementFaceEdge) =
        match graph.Edges |> List.tryFind (fun edge -> edge.Id = current.EdgeId) with
        | None -> Error(MissingArrangementEdge current.EdgeId)
        | Some edge ->
            let arrival = if current.Left then edge.EndVertex else edge.StartVertex
            let incomingReversed = current.Left
            match graph.CyclicOrders |> List.tryFind (fst >> (=) arrival) with
            | None -> Error(DualMissingCyclicOrder arrival)
            | Some(_, groups) ->
                let order = List.concat groups
                match order |> List.tryFindIndex (fun item -> item.EdgeId = current.EdgeId && item.Reversed = incomingReversed) with
                | None -> Error(DualMissingIncidentEdge(arrival, current.EdgeId))
                | Some index ->
                    let next = order[(index + 1) % order.Length]
                    let result: ArrangementFaceEdge = { EdgeId = next.EdgeId; Left = not next.Reversed }
                    Ok result

    let private faceWalk (graph: ArrangementGraph) (start: ArrangementFaceEdge) =
        let rec loop (current: ArrangementFaceEdge) (visited: ArrangementFaceEdge list) remaining =
            if remaining <= 0 then Error(DualWalkDidNotClose(start.EdgeId, start.Left))
            else
                faceSuccessor graph current
                |> Result.bind (fun next ->
                    let visited = current :: visited
                    if faceEdgeEqual next start then Ok(List.rev visited)
                    elif visited |> List.exists (faceEdgeEqual next) then Error(DualWalkDidNotClose(start.EdgeId, start.Left))
                    else loop next visited (remaining - 1))
        loop start [] (graph.Edges.Length * 2 + 1)

    let private walkArea (graph: ArrangementGraph) (walk: ArrangementFaceEdge list) =
        walk
        |> List.sumBy (fun (faceEdge: ArrangementFaceEdge) ->
            let edge = graph.Edges |> List.find (fun (edge: ArrangementEdge) -> edge.Id = faceEdge.EdgeId)
            let segment = if faceEdge.Left then edge.Segment else Segment.reverse edge.Segment
            Area.signedSegment segment)

    let private remapSegmentEndpoints segment newStart newFinish =
        match segment with
        | Line _ -> Ok(Line(newStart, newFinish))
        | _ ->
            Affine.pointPairSimilarity (Segment.start segment) (Segment.finish segment) newStart newFinish
            |> Result.mapError (fun _ -> ArrangementSegmentError SplitOutsideSegment)
            |> Result.bind (fun transform ->
                Transform.segment segment transform
                |> Result.mapError (fun _ -> ArrangementSegmentError CannotMapArcNonlinearly))
            |> Result.map (Segment.withStart newStart >> Segment.withFinish newFinish)

    let private faceEdgeSegment (graph: ArrangementGraph) (reference: ArrangementFaceEdge) =
        match graph.Edges |> List.tryFind (fun edge -> edge.Id = reference.EdgeId) with
        | None -> Error(MissingArrangementEdge reference.EdgeId)
        | Some edge ->
            let segment, startVertex, endVertex =
                if reference.Left then edge.Segment, edge.StartVertex, edge.EndVertex
                else Segment.reverse edge.Segment, edge.EndVertex, edge.StartVertex
            match graph.Vertices |> List.tryFind (fun vertex -> vertex.Id = startVertex),
                  graph.Vertices |> List.tryFind (fun vertex -> vertex.Id = endVertex) with
            | None, _ -> Error(MissingArrangementVertex startVertex)
            | _, None -> Error(MissingArrangementVertex endVertex)
            | Some startPoint, Some endPoint -> remapSegmentEndpoints segment startPoint.Point endPoint.Point

    let private faceWalkSubpath graph edges =
        edges
        |> List.fold (fun state edge ->
            state
            |> Result.bind (fun segments -> faceEdgeSegment graph edge |> Result.map (fun segment -> segments @ [ segment ]))) (Ok [])
        |> Result.bind (fun segments ->
            Subpath.create segments
            |> Result.mapError ArrangementSegmentError
            |> Result.bind (fun subpath -> Subpath.setClosed true subpath |> Result.mapError ArrangementSegmentError))

    let private containmentSignature sample subpaths options =
        subpaths
        |> List.fold (fun state subpath ->
            state
            |> Result.bind (function
                | None -> Ok None
                | Some signature ->
                    WindingField.pathContainmentWith sample (Path.ofSubpaths [ subpath ]) Nonzero options
                    |> Result.mapError ArrangementSegmentError
                    |> Result.map (function
                        | Boundary -> None
                        | Inside -> Some(signature @ [ true ])
                        | Outside -> Some(signature @ [ false ])))) (Ok(Some []))

    let private faceWalkSignature graph allSubpaths edges subpath =
        match edges with
        | [] -> Error(DualFaceSampleUnavailable(-1, true))
        | first :: _ ->
            faceEdgeSegment graph first
            |> Result.bind (fun segment ->
                Segment.point segment 0.5<parameter>
                |> Result.mapError ArrangementSegmentError
                |> Result.bind (fun midpoint ->
                    Segment.derivative segment 0.5<parameter>
                    |> Result.mapError ArrangementSegmentError
                    |> Result.bind (fun derivative ->
                        let direction =
                            Point.normalize derivative
                            |> Option.orElseWith (fun () -> Point.displacement (Segment.start segment) (Segment.finish segment) |> Point.normalize)
                            |> Option.defaultValue (Point.create 1.0 0.0)
                        let normal = Point.rotateCounterclockwise direction
                        let rec sample distance remaining =
                            if remaining <= 0 || distance <= 0.0<length> then Error(DualFaceSampleUnavailable(first.EdgeId, first.Left))
                            else
                                let point = Point.translate (Point.scale distance normal) midpoint
                                let options = { WindingField.defaultOptions with Tolerance = distance * 0.01 }
                                WindingField.pathContainmentWith point (Path.ofSubpaths [ subpath ]) Nonzero options
                                |> Result.mapError ArrangementSegmentError
                                |> Result.bind (function
                                    | Boundary -> sample (distance * 0.5) (remaining - 1)
                                    | _ ->
                                        containmentSignature point allSubpaths options
                                        |> Result.bind (function
                                            | Some signature -> Ok signature
                                            | None -> sample (distance * 0.5) (remaining - 1)))
                        sample (Segment.chordLength segment * 0.0001) 12)))

    type private FaceCandidate =
        { Walk: ArrangementFaceWalk
          Signature: bool list }

    let private facesFromCandidates candidates =
        let groups =
            candidates
            |> List.groupBy _.Signature
            |> List.map snd
        let outerGroups, boundedGroups =
            groups |> List.partition (fun group -> group.Head.Signature |> List.forall not)
        match outerGroups with
        | [ outerGroup ] ->
            (outerGroup :: boundedGroups)
            |> List.indexed
            |> List.fold (fun state (id, group) ->
                state
                |> Result.bind (fun faces ->
                    let enclosing, islands = group |> List.map _.Walk |> List.partition _.Outer
                    let isOuter = group.Head.Signature |> List.forall not
                    match isOuter, enclosing with
                    | true, [] -> Ok(faces @ [ { Id = id; Outer = true; Walks = islands } ])
                    | false, [ enclosing ] -> Ok(faces @ [ { Id = id; Outer = false; Walks = enclosing :: islands } ])
                    | _, walks -> Error(DualInvalidOuterWalkCount walks.Length))) (Ok [])
        | groups -> Error(DualInvalidOuterFaceCount groups.Length)

    /// Derive face boundary walks and the face on each side of every edge.
    /// Walks with the same containment signature are grouped into one face;
    /// the enclosing walk precedes any island walks.
    let dual (graph: ArrangementGraph) =
        if List.isEmpty graph.Edges then
            Ok { Faces = [ { Id = 0; Outer = true; Walks = [] } ]; EdgeFaces = [] }
        else
            let allSides: ArrangementFaceEdge list =
                graph.Edges
                |> List.collect (fun edge ->
                    [ ({ EdgeId = edge.Id; Left = true }: ArrangementFaceEdge)
                      ({ EdgeId = edge.Id; Left = false }: ArrangementFaceEdge) ])
            let rec gather (remaining: ArrangementFaceEdge list) (walks: ArrangementFaceEdge list list) =
                match remaining with
                | [] -> Ok(List.rev walks)
                | start :: _ ->
                    faceWalk graph start
                    |> Result.bind (fun edges ->
                        let remaining = remaining |> List.filter (fun candidate -> edges |> List.exists (faceEdgeEqual candidate) |> not)
                        gather remaining (edges :: walks))
            gather allSides []
            |> Result.bind (fun walks ->
                walks
                |> List.fold (fun state edges ->
                    state
                    |> Result.bind (fun prepared ->
                        faceWalkSubpath graph edges
                        |> Result.map (fun subpath -> prepared @ [ edges, subpath ]))) (Ok []))
            |> Result.bind (fun prepared ->
                let subpaths = prepared |> List.map snd
                prepared
                |> List.fold (fun state (edges, subpath) ->
                    state
                    |> Result.bind (fun candidates ->
                        faceWalkSignature graph subpaths edges subpath
                        |> Result.map (fun signature ->
                            let walk = { Outer = Area.signedSubpath subpath < 0.0<length^2>; Edges = edges }
                            candidates @ [ { Walk = walk; Signature = signature } ]))) (Ok []))
            |> Result.bind facesFromCandidates
            |> Result.bind (fun faces ->
                let findFace edgeId left =
                    faces
                    |> List.tryFind (fun (face: ArrangementFace) -> face.Walks |> List.exists (fun walk -> walk.Edges |> List.exists (fun edge -> edge.EdgeId = edgeId && edge.Left = left)))
                    |> Option.map _.Id
                graph.Edges
                |> List.fold (fun state (edge: ArrangementEdge) ->
                    state
                    |> Result.bind (fun edgeFaces ->
                        match findFace edge.Id true, findFace edge.Id false with
                        | Some left, Some right -> Ok(edgeFaces @ [ { EdgeId = edge.Id; LeftFace = left; RightFace = right } ])
                        | None, _ -> Error(DualMissingEdgeFace(edge.Id, true))
                        | _, None -> Error(DualMissingEdgeFace(edge.Id, false)))) (Ok [])
                |> Result.map (fun edgeFaces -> { Faces = faces; EdgeFaces = edgeFaces }))

    type private NestedContourEdge =
        { Id: int
          Layer: int
          Segment: Segment
          StartVertex: int
          EndVertex: int }

    type private NestedContourRay =
        { EdgeId: int
          Starts: bool
          Angle: float<degree> }

    let private nestedContourEdges
        (graph: ArrangementGraph)
        (path: Path)
        (sideSamplingDistance: float<length>) =
        graph.Edges
        |> List.fold (fun state edge ->
            state
            |> Result.bind (fun classified ->
                WindingField.segmentSideNonzeroLevels
                    edge.Segment
                    path
                    sideSamplingDistance
                    WindingField.defaultOptions
                |> Result.mapError ArrangementSegmentError
                |> Result.map (fun (left, right) -> classified @ [ edge, left, right ]))) (Ok [])
        |> Result.map (fun classified ->
            let maximumLayer =
                classified
                |> List.collect (fun (_, left, right) -> [ abs left; abs right ])
                |> List.fold max 0
            [ for magnitude in 1 .. maximumLayer do
                for sign in [ 1; -1 ] do
                    let layer = sign * magnitude
                    for edge, left, right in classified do
                        let leftActive, rightActive =
                            if layer > 0 then left >= layer, right >= layer
                            else left <= layer, right <= layer
                        if leftActive <> rightActive then
                            if leftActive then
                                yield
                                    { Id = edge.Id
                                      Layer = layer
                                      Segment = edge.Segment
                                      StartVertex = edge.StartVertex
                                      EndVertex = edge.EndVertex }
                            else
                                yield
                                    { Id = edge.Id
                                      Layer = layer
                                      Segment = Segment.reverse edge.Segment
                                      StartVertex = edge.EndVertex
                                      EndVertex = edge.StartVertex } ])

    let private nestedContourSuccessors (edges: NestedContourEdge list) =
        let ray edge starts =
            let parameter = if starts then 0.0<parameter> else 1.0<parameter>
            Segment.directions edge.Segment parameter
            |> Result.mapError ArrangementSegmentError
            |> Result.bind (fun directions ->
                let direction = if starts then directions.Outgoing else directions.Incoming
                match direction with
                | None -> Error(ContourTraceFailed edge.Id)
                | Some direction ->
                    let outward = if starts then direction else Point.scale -1.0 direction
                    Ok { EdgeId = edge.Id; Starts = starts; Angle = Point.heading outward })
        let vertices =
            edges
            |> List.collect (fun edge -> [ edge.Layer, edge.StartVertex; edge.Layer, edge.EndVertex ])
            |> List.distinct
        vertices
        |> List.fold (fun state (layer, vertex) ->
            state
            |> Result.bind (fun successors ->
                let incident =
                    edges
                    |> List.filter (fun edge -> edge.Layer = layer && (edge.StartVertex = vertex || edge.EndVertex = vertex))
                incident
                |> List.fold (fun raysState edge ->
                    raysState
                    |> Result.bind (fun rays ->
                        if edge.StartVertex = vertex then ray edge true |> Result.map (fun item -> item :: rays)
                        else ray edge false |> Result.map (fun item -> item :: rays))) (Ok [])
                |> Result.bind (fun rays ->
                    let ordered = rays |> List.sortBy (fun item -> float item.Angle)
                    let count = ordered.Length
                    ordered
                    |> List.indexed
                    |> List.fold (fun pairState (index, incoming) ->
                        pairState
                        |> Result.bind (fun pairs ->
                            if incoming.Starts then Ok pairs
                            else
                                let successor = ordered[(index + 1) % count]
                                if successor.Starts then Ok(Map.add incoming.EdgeId successor.EdgeId pairs)
                                else Error(ContourTraceFailed incoming.EdgeId))) (Ok successors)))) (Ok Map.empty)

    let private traceNestedContours edges successors tolerance =
        let byId = edges |> List.map (fun edge -> edge.Id, edge) |> Map.ofList
        let rec trace start current visited segments =
            if Set.contains current visited then
                if current = start then Ok(List.rev segments, visited)
                else Error(ContourTraceFailed current)
            else
                match Map.tryFind current byId, Map.tryFind current successors with
                | Some edge, Some next -> trace start next (Set.add current visited) (edge.Segment :: segments)
                | _ -> Error(ContourTraceFailed current)
        let rec gather remaining visited contours =
            match remaining |> List.tryFind (fun edge -> not (Set.contains edge.Id visited)) with
            | None -> Ok(List.rev contours)
            | Some edge ->
                trace edge.Id edge.Id visited []
                |> Result.bind (fun (segments, visited) ->
                    Subpath.createWith (WiggleWith tolerance) segments
                    |> Result.bind (Subpath.setClosedWith (WiggleWith tolerance) true)
                    |> Result.mapError ArrangementSegmentError
                    |> Result.bind (fun contour ->
                        let contour = if edge.Layer > 0 then Subpath.reverse contour else contour
                        gather remaining visited (contour :: contours)))
        gather edges Set.empty []

    /// Reconstruct every nonzero winding layer represented by an arrangement graph.
    /// A region of winding magnitude n contributes n nested closed contours.
    let nestedContoursFromGraph
        (graph: ArrangementGraph)
        (path: Path)
        (tolerance: float<length>) =
        if tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InvalidArrangementTolerance tolerance)
        else
            nestedContourEdges graph path (tolerance * 16.0)
            |> Result.bind (fun edges ->
                nestedContourSuccessors edges
                |> Result.bind (fun successors -> traceNestedContours edges successors tolerance))
