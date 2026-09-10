namespace SvgPath

[<RequireQualifiedAccess>]
/// Construction, validation, source-image lookup, and dualization of planar
/// arrangements formed from SVG path segments.
module Arrangement =

    // The graph representation deliberately keeps source segment endpoints rather
    // than snapping edge geometry to vertex centers.

    [<Struct>]
    /// A topological vertex and the source endpoints represented by it.
    /// Point is the smallest enclosing circle's center for EndpointSamples.
    /// Every sample is within endpoint tolerance. A cluster's center is determined
    /// by its samples, but greedy endpoint-to-cluster assignment can depend on order.
    type ArrangementVertex =
        { Id: int
          Point: Point<length>
          EndpointSamples: Point<length> list }

    [<Struct>]
    /// One atomic geometric edge, including directional source multiplicities.
    /// Its length upper bound is at least the legacy minimumChord size threshold.
    type ArrangementEdge =
        { Id: int
          Segment: Segment
          Bounds: BoundingBox
          StartVertex: int
          EndVertex: int
          ForwardMultiplicity: int
          ReverseMultiplicity: int }

    [<Struct>]
    /// A reference to an arrangement edge in either stored or reversed direction.
    type OrientedArrangementEdge = { EdgeId: int; Reversed: bool }

    /// A noded planar graph. CyclicOrders stores clockwise groups of incident
    /// oriented edges; a group can contain edges whose local order is unresolved.
    type ArrangementGraph =
        { Vertices: ArrangementVertex list
          Edges: ArrangementEdge list
          CyclicOrders: (int * OrientedArrangementEdge list list) list }

    /// One oriented edge occurrence in a face-boundary walk. Left identifies the
    /// face on the visual-left side of the stored edge direction.
    type ArrangementFaceEdge = { EdgeId: int; Left: bool }
    /// One connected boundary walk of an arrangement face.
    type ArrangementFaceWalk = { Outer: bool; Edges: ArrangementFaceEdge list }
    /// A dual face. The unbounded face and each bounded face are explicitly marked.
    type ArrangementFace = { Id: int; Outer: bool; Walks: ArrangementFaceWalk list }
    /// The faces incident to the visual-left and visual-right sides of an edge.
    type ArrangementEdgeFaces = { EdgeId: int; LeftFace: int; RightFace: int }
    /// Face decomposition and edge-to-face incidence for an arrangement graph.
    type DualArrangementGraph = { Faces: ArrangementFace list; EdgeFaces: ArrangementEdgeFaces list }

    /// Signed winding change from stored-edge visual left to visual right.
    /// Describes the winding boundary, not necessarily every geometric preimage.
    type internal EdgeWindingChange = { EdgeId: int; RightMinusLeft: int }
    type internal FaceWinding = { FaceId: int; Value: int }
    type internal WindingPropagationError =
        | InvalidWindingDual
        | InvalidWindingChanges
        | MissingWindingChange of edgeId: int
        | ContradictoryWinding of edgeId: int * faceId: int * assigned: int * required: int
        | UnreachableWindingFace of faceId: int

    [<Struct>]
    /// One atomic graph edge traversed by an input segment.
    type DirectedEdgeReference = { EdgeId: int; Reversed: bool }

    /// Ordered atomic graph-edge image of one source segment.
    type ArrangementSegmentImage =
        { PathIndex: int
          SubpathIndex: int
          SegmentIndex: int
          Edges: DirectedEdgeReference list }

    /// Arrangement graph plus the ordered images of all input segments.
    type ArrangementGraphBuild =
        { Graph: ArrangementGraph
          SegmentImages: ArrangementSegmentImage list }

    [<Struct>]
    /// One atomic edge in the image of a directly supplied source segment.
    /// From <= To are source intervals retained through subdivision, not projection.
    type ArrangementSegmentEdgeImage =
        { From: float<parameter>
          To: float<parameter>
          EdgeId: int
          Reversed: bool
          Own: bool }

    /// Ordered atomic-edge image of one directly supplied source segment.
    type ArrangementSourceSegmentImage =
        { SegmentIndex: int
          Edges: ArrangementSegmentEdgeImage list }

    [<Struct>]
    /// One source occurrence represented by an arrangement edge.
    type ArrangementEdgeSourceImage =
        { SegmentIndex: int
          From: float<parameter>
          To: float<parameter>
          Reversed: bool }

    /// All source occurrences represented by one arrangement edge.
    type ArrangementEdgeImage = { EdgeId: int; Sources: ArrangementEdgeSourceImage list }

    /// Detailed arrangement build for direct segment-list construction.
    /// An image may be empty when every refined piece's length upper bound falls
    /// below the historically named minimumChord threshold.
    type ArrangementSegmentBuild =
        { Graph: ArrangementGraph
          Segments: Segment list
          SegmentImages: ArrangementSourceSegmentImage list
          EdgeImages: ArrangementEdgeImage list }

    /// Errors returned while constructing, validating, or dualizing arrangements.
    type internal ArrangementInternalError =
        | InternalArrangementSegmentError of error: SegmentError
        | InternalNormalizationError
        | InternalSelfIntersectionSubdivisionFailed of sourceIndex: int
        | InternalInvalidArrangementTolerance of tolerance: float<length>
        /// Minimum length-upper-bound threshold must be positive.
        | InternalInvalidMinimumChord of minimumChord: float<length>
        | InternalInvalidEndpointSliverTolerance of tolerance: float<parameter>
        /// Legacy chord label carries the segment length upper bound.
        | InternalSegmentTooShort of chord: float<length> * minimum: float<length>
        | InternalSegmentCollapsedToVertex of vertex: int
        | InternalLoopEdge of vertex: int
        | InternalMissingArrangementVertex of vertex: int
        | InternalMissingArrangementEdge of edge: int
        | InternalIsolatedVertex of vertex: int
        | InternalInvalidMultiplicity of edge: int
        | InternalOddWeightedDegree of vertex: int * degree: int
        | InternalEdgeEndpointMismatch of edge: int * vertex: int * distance: float<length>
        | InternalVertexWithoutEndpointSamples of vertex: int
        | InternalVertexCenterMismatch of vertex: int * distanceSquared: float<length^2>
        | InternalVertexSampleOutsideTolerance of vertex: int * distanceSquared: float<length^2> * toleranceSquared: float<length^2>
        | InternalContourTraceFailed of vertex: int
        | InternalCyclicOrderMissingVertex of vertex: int
        | InternalCyclicOrderRadiusUnavailable of vertex: int
        | InternalInvalidCyclicOrderAttempts of maxAttempts: int
        | InternalCyclicOrderCircleIntersectionFailed of vertex: int * edge: int * radius: float<length>
        | InternalDualMissingCyclicOrder of vertex: int
        | InternalDualMissingIncidentEdge of vertex: int * edge: int
        | InternalDualWalkDidNotClose of edge: int * left: bool
        | InternalDualSweepContradiction of walk: int
        | InternalDualSweepExhausted of unresolved: int
        | InternalDualInvalidOuterWalkCount of count: int
        | InternalDualMissingEdgeFace of edge: int * left: bool
        | InternalDualInvalidOuterFaceCount of count: int


    /// Stable errors returned by arrangement construction and validation.
    type Error =
        | ArrangementSegmentError of error: SegmentError
        | InvalidArrangementTolerance of tolerance: float<length>
        /// Minimum length-upper-bound threshold must be positive.
        | InvalidMinimumChord of minimumChord: float<length>
        | InvalidEndpointSliverTolerance of tolerance: float<parameter>
        /// Legacy chord label carries the segment length upper bound.
        | SegmentTooShort of chord: float<length> * minimum: float<length>
        | ConstructionFailed

    let internal publicError = function
        | InternalArrangementSegmentError error -> ArrangementSegmentError error
        | InternalInvalidArrangementTolerance value -> InvalidArrangementTolerance value
        | InternalInvalidMinimumChord value -> InvalidMinimumChord value
        | InternalInvalidEndpointSliverTolerance value -> InvalidEndpointSliverTolerance value
        | InternalSegmentTooShort(chord, minimum) -> SegmentTooShort(chord, minimum)
        | _ -> ConstructionFailed

    let internal empty = { Vertices = []; Edges = []; CyclicOrders = [] }

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

    let private segmentLengthBound segment =
        Segment.lengthUpperBound segment |> Result.mapError InternalArrangementSegmentError

    let internal insertAtomicSegment (graph: ArrangementGraph) segment tolerance minimumChord =
        if tolerance <= 0.0<length> || not (finite tolerance) then Error(InternalInvalidArrangementTolerance tolerance)
        elif minimumChord <= 0.0<length> || not (finite minimumChord) then Error(InternalInvalidMinimumChord minimumChord)
        else
          segmentLengthBound segment |> Result.bind (fun size ->
            if size < minimumChord then Error(InternalSegmentTooShort(size,minimumChord))
            else
                let vertices, startVertex = attachVertex tolerance (Segment.start segment) graph.Vertices
                let vertices, endVertex = attachVertex tolerance (Segment.finish segment) vertices
                if startVertex = endVertex then Error(InternalSegmentCollapsedToVertex startVertex)
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
                        |> Result.mapError InternalArrangementSegmentError
                        |> Result.map (fun bounds ->
                            let id = graph.Edges |> List.fold (fun maximum edge -> max maximum edge.Id) -1 |> (+) 1
                            { Vertices = vertices
                              Edges = edges @ [ { Id = id; Segment = segment; Bounds = bounds; StartVertex = startVertex; EndVertex = endVertex; ForwardMultiplicity = 1; ReverseMultiplicity = 0 } ]
                              CyclicOrders = [] })
                    | _ -> Ok { Vertices = vertices; Edges = edges; CyclicOrders = [] })

    // Checks edges and endpoint clusters, including closed-boundary degree parity.
    // This does not validate cyclic orders.
    let private validateInternal (graph: ArrangementGraph) tolerance minimumChord =
        if tolerance <= 0.0<length> || not (finite tolerance) then Error(InternalInvalidArrangementTolerance tolerance)
        elif minimumChord <= 0.0<length> || not (finite minimumChord) then Error(InternalInvalidMinimumChord minimumChord)
        else
            let vertex id = graph.Vertices |> List.tryFind (fun item -> item.Id = id)
            let edgeError =
                graph.Edges
                |> List.tryPick (fun edge ->
                    if edge.ForwardMultiplicity + edge.ReverseMultiplicity <= 0 then Some(InternalInvalidMultiplicity edge.Id)
                    elif edge.StartVertex = edge.EndVertex then Some(InternalLoopEdge edge.StartVertex)
                    elif vertex edge.StartVertex |> Option.isNone then Some(InternalMissingArrangementVertex edge.StartVertex)
                    elif vertex edge.EndVertex |> Option.isNone then Some(InternalMissingArrangementVertex edge.EndVertex)
                    else
                        let startDistance = Point.distance (Segment.start edge.Segment) (vertex edge.StartVertex).Value.Point
                        let endDistance = Point.distance (Segment.finish edge.Segment) (vertex edge.EndVertex).Value.Point
                        if startDistance > tolerance then Some(InternalEdgeEndpointMismatch(edge.Id, edge.StartVertex, startDistance))
                        elif endDistance > tolerance then Some(InternalEdgeEndpointMismatch(edge.Id, edge.EndVertex, endDistance))
                        else
                            match segmentLengthBound edge.Segment with
                            | Error error -> Some error
                            | Ok size when size<minimumChord -> Some(InternalSegmentTooShort(size,minimumChord))
                            | Ok _ -> None)
            match edgeError with
            | Some error -> Error error
            | None ->
                graph.Vertices
                |> List.tryPick (fun vertex ->
                    if List.isEmpty vertex.EndpointSamples then Some(InternalVertexWithoutEndpointSamples vertex.Id)
                    else
                        let degree = graph.Edges |> List.filter (fun edge -> edge.StartVertex = vertex.Id || edge.EndVertex = vertex.Id) |> List.sumBy (fun edge -> edge.ForwardMultiplicity + edge.ReverseMultiplicity)
                        if degree = 0 then Some(InternalIsolatedVertex vertex.Id)
                        elif degree % 2 <> 0 then Some(InternalOddWeightedDegree(vertex.Id, degree))
                        else
                            match SmallestEnclosingCircle.points vertex.EndpointSamples with
                            | Error _ -> Some(InternalVertexWithoutEndpointSamples vertex.Id)
                            | Ok circle when circle.Center <> vertex.Point -> Some(InternalVertexCenterMismatch(vertex.Id, Point.squaredDistance circle.Center vertex.Point))
                            | Ok circle when circle.RadiusSquared > tolerance * tolerance -> Some(InternalVertexSampleOutsideTolerance(vertex.Id, circle.RadiusSquared, tolerance * tolerance))
                            | _ -> None)
                |> function Some error -> Error error | None -> Ok ()

    type private CyclicSample =
        { OrientedEdge: OrientedArrangementEdge
          Point: Point<length>
          Angle: float<degree> }

    /// Validates edges, endpoint clusters, and closed-boundary degree parity.
    /// Does not validate cyclic orders; open-boundary graphs may fail parity.
    let validate graph tolerance minimumChord =
        validateInternal graph tolerance minimumChord
        |> Result.mapError publicError

    let private orientedSegment (graph: ArrangementGraph) (oriented: OrientedArrangementEdge) =
        graph.Edges
        |> List.tryFind (fun (edge: ArrangementEdge) -> edge.Id = oriented.EdgeId)
        |> function
            | None -> Error(InternalMissingArrangementEdge oriented.EdgeId)
            | Some edge -> Ok(if oriented.Reversed then Segment.reverse edge.Segment else edge.Segment)

    let private incidentEdges (graph: ArrangementGraph) vertex : OrientedArrangementEdge list =
        graph.Edges
        |> List.collect (fun (edge: ArrangementEdge) ->
            [ if edge.StartVertex = vertex then yield ({ EdgeId = edge.Id; Reversed = false }: OrientedArrangementEdge)
              if edge.EndVertex = vertex then yield ({ EdgeId = edge.Id; Reversed = true }: OrientedArrangementEdge) ])

    let private circleSample (graph: ArrangementGraph) (vertex: Point<length>) vertexId (radius: float<length>) tolerance (oriented: OrientedArrangementEdge) =
        orientedSegment graph oriented
        |> Result.bind (fun segment ->
            // `Segment.crossingsWith` is length-valued in F#, so use radial
            // distance instead of Gleam's equivalent squared-distance residual.
            // The roots are identical and the tolerance remains a radial error.
            Segment.crossingsWith
                segment
                (fun point -> Point.distance point vertex - radius)
                { Samples = 100
                  SignedLineDistanceTolerance = tolerance
                  MaxIterations = 100 }
            |> Result.mapError InternalArrangementSegmentError
            |> Result.bind (fun roots ->
                match roots |> List.tryFind (fun t -> t > 0.0<parameter> && t <= 1.0<parameter>) with
                | None -> Error(InternalCyclicOrderCircleIntersectionFailed(vertexId, oriented.EdgeId, radius))
                | Some t ->
                    Segment.point segment t
                    |> Result.mapError InternalArrangementSegmentError
                    |> Result.map (fun point ->
                        { OrientedEdge = oriented
                          Point = point
                          Angle = Point.heading (Point.displacement vertex point) })))

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

    let internal vertexCyclicOrderWith (graph: ArrangementGraph) vertexId tolerance maxAttempts =
        if tolerance <= 0.0<length> || not (finite tolerance) then Error(InternalInvalidArrangementTolerance tolerance)
        elif maxAttempts <= 0 then Error(InternalInvalidCyclicOrderAttempts maxAttempts)
        else
            match graph.Vertices |> List.tryFind (fun vertex -> vertex.Id = vertexId) with
            | None -> Error(InternalCyclicOrderMissingVertex vertexId)
            | Some vertex ->
                let incident: OrientedArrangementEdge list = incidentEdges graph vertexId
                match incident with
                | [] -> Error(InternalIsolatedVertex vertexId)
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
                        if radius <= 0.0<length> || not (System.Double.IsFinite(float radius)) then Error(InternalCyclicOrderRadiusUnavailable vertexId)
                        else
                            let rec attempts (radius: float<length>) remaining (successes: CyclicSample list list) (previousError: ArrangementInternalError option) =
                                if remaining <= 0 || radius <= tolerance / 2.0 then
                                    match List.rev successes with
                                    | [] -> Error(defaultArg previousError (InternalCyclicOrderRadiusUnavailable vertexId))
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
    /// on common shrinking circles around its vertex. Graph construction uses
    /// this embedding helper.
    let internal cyclicOrdersWith (graph: ArrangementGraph) tolerance maxAttempts =
        if tolerance <= 0.0<length> || not (finite tolerance) then Error(InternalInvalidArrangementTolerance tolerance)
        elif maxAttempts <= 0 then Error(InternalInvalidCyclicOrderAttempts maxAttempts)
        else
            graph.Vertices
            |> List.fold (fun state vertex ->
                state
                |> Result.bind (fun orders ->
                    vertexCyclicOrderWith graph vertex.Id tolerance maxAttempts
                    |> Result.map (fun groups -> orders @ [ vertex.Id, groups ]))) (Ok [])

    let private cyclicOrders graph tolerance = cyclicOrdersWith graph tolerance 3

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

    type private AtomicPiece =
        { SourceIndex: int
          PathIndex: int
          SubpathIndex: int
          SegmentIndex: int
          SourceFrom: float<parameter>
          SourceTo: float<parameter>
          SelfSplitDepth: int
          Segment: Segment }

    type private IncomingContext =
        { Piece: AtomicPiece
          Bounds: BoundingBox
          StartMatch: int option
          EndMatch: int option }

    type private ProgressivePieceResult =
        | ProgressivePieceInserted of ArrangementGraph * ArrangementSourceSegmentImage list
        | ProgressivePieceReplaced of ArrangementGraph * ArrangementSourceSegmentImage list * AtomicPiece list

    type private ProgressiveEdgeStep =
        | ProgressiveContinue of ArrangementGraph * ArrangementSourceSegmentImage list
        | ProgressiveReplaceIncoming of ArrangementGraph * ArrangementSourceSegmentImage list * AtomicPiece list

    let private uniqueVertexForEndpoint vertices endpoint tolerance =
        let matches =
            vertices
            |> List.filter (fun (vertex: ArrangementVertex) -> Point.distance endpoint vertex.Point <= tolerance)
        match matches with
        | [] -> Ok None
        | [ vertex ] -> Ok(Some vertex.Id)
        | _ -> Error InternalNormalizationError

    let private pointInExpandedBox point (bounds: BoundingBox) tolerance =
        point.X >= bounds.Min.X - tolerance
        && point.X <= bounds.Max.X + tolerance
        && point.Y >= bounds.Min.Y - tolerance
        && point.Y <= bounds.Max.Y + tolerance

    let private boundingBoxesOverlap (first: BoundingBox) (second: BoundingBox) tolerance =
        first.Min.X <= second.Max.X + tolerance
        && first.Max.X >= second.Min.X - tolerance
        && first.Min.Y <= second.Max.Y + tolerance
        && first.Max.Y >= second.Min.Y - tolerance

    let private vertexProjectsToLineInterior vertex startPoint finishPoint tolerance =
        let line = Point.displacement startPoint finishPoint
        let lengthSquared = Point.dot line line
        if lengthSquared <= 0.0<length^2> then Error(InternalSegmentTooShort(0.0<length>, tolerance))
        else
            let rawT = Parameter.fromFloat(float (Point.dot (Point.displacement startPoint vertex) line / lengthSquared))
            let projected = Point.translate (Point.scale (Parameter.ratio rawT) line) startPoint
            if Point.distance vertex projected <= tolerance && rawT > 0.0<parameter> && rawT < 1.0<parameter> then Ok(Some rawT)
            else Ok None

    let private vertexProjectsToPieceInterior vertex segment tolerance =
        if Point.distance vertex (Segment.start segment) <= tolerance
           || Point.distance vertex (Segment.finish segment) <= tolerance then Ok None
        else
            match segment with
            | Line(startPoint, finishPoint) -> vertexProjectsToLineInterior vertex startPoint finishPoint tolerance
            | _ ->
                Segment.projection segment vertex
                |> Result.mapError InternalArrangementSegmentError
                |> Result.map (fun (t, _, distance) ->
                    if distance <= tolerance && t > 0.0<parameter> && t < 1.0<parameter> then Some t else None)

    let private distinctParameters segment tolerance parameters =
        let taxicabDiameter segment =
            match segment with
            | Arc endpoint when endpoint.Start = endpoint.End -> Ok 0.0<length>
            | _ ->
                Segment.boundingBox segment
                |> Result.mapError InternalArrangementSegmentError
                |> Result.map BoundingBox.diameter
        let rec loop distinct = function
            | [] -> Ok(List.rev distinct)
            | first :: rest ->
                match distinct with
                | [] -> loop [ first ] rest
                | previous :: _ ->
                    Segment.between segment previous first
                    |> Result.mapError InternalArrangementSegmentError
                    |> Result.bind (fun between ->
                        taxicabDiameter between
                        |> Result.bind (fun motion ->
                            loop (if motion <= tolerance then distinct else first :: distinct) rest))
        loop [] parameters

    let private parameterLengthBoundLongEnough segment fromParameter toParameter minimumChord =
        Segment.between segment fromParameter toParameter
        |> Result.mapError InternalArrangementSegmentError
        |> Result.bind segmentLengthBound
        |> Result.map (fun size -> size>=minimumChord)

    let private retainMinimumLengthCuts segment parameters minimumChord =
        let replaceHead value = function | [] -> [ value ] | _ :: rest -> value :: rest
        let rec loop previous retained = function
            | [] -> Ok(List.rev retained)
            | [ last ] ->
                parameterLengthBoundLongEnough segment previous last minimumChord
                |> Result.map (fun longEnough -> List.rev(if longEnough then last :: retained else replaceHead last retained))
            | candidate :: next :: rest ->
                parameterLengthBoundLongEnough segment previous candidate minimumChord
                |> Result.bind (fun beforeLongEnough ->
                    parameterLengthBoundLongEnough segment candidate next minimumChord
                    |> Result.bind (fun afterLongEnough ->
                        if beforeLongEnough && afterLongEnough then loop candidate (candidate :: retained) (next :: rest)
                        else loop previous retained (next :: rest)))
        match parameters with
        | [] | [ _ ] -> Ok parameters
        | start :: rest -> loop start [ start ] rest

    let private retainedSplitSegments minimumChord segments =
        segments |> List.fold (fun state segment -> state |> Result.bind (fun retained ->
            segmentLengthBound segment |> Result.map (fun size -> if size>=minimumChord then retained @ [segment] else retained))) (Ok [])

    let private effectiveCutParameters segment cuts tolerance minimumChord =
        distinctParameters segment tolerance (List.sort (0.0<parameter> :: 1.0<parameter> :: cuts))
        |> Result.bind (fun parameters -> retainMinimumLengthCuts segment parameters minimumChord)
        |> Result.bind (fun parameters ->
            let interior =
                match parameters with
                | [] | [ _ ] | [ _; _ ] -> []
                | _ :: rest -> rest |> List.rev |> List.tail |> List.rev
            match interior with
            | [] -> Ok []
            | _ ->
                Segment.betweenManyInside segment (List.sort (0.0<parameter> :: 1.0<parameter> :: interior))
                |> Result.mapError InternalArrangementSegmentError
                |> Result.bind (retainedSplitSegments minimumChord)
                |> Result.map (fun retained -> if retained.Length >= 2 then interior else []))

    let private splitAtomicPiece (piece: AtomicPiece) (cuts: float<parameter> list) tolerance minimumChord =
        distinctParameters piece.Segment tolerance (List.sort (0.0<parameter> :: 1.0<parameter> :: cuts))
        |> Result.bind (fun parameters ->
            Segment.betweenManyInside piece.Segment parameters
            |> Result.mapError InternalArrangementSegmentError
            |> Result.bind (fun segments ->
                List.zip segments (List.pairwise parameters)
                |> List.fold (fun state (segment, (fromParameter, toParameter)) -> state |> Result.bind (fun pieces ->
                  segmentLengthBound segment |> Result.map (fun size ->
                    if size < minimumChord then pieces
                    else
                        let interpolate (left: float<parameter>) (right: float<parameter>) (t: float<parameter>) =
                            if t=0.0<parameter> then left elif t=1.0<parameter> then right
                            else left + (right - left) * Parameter.ratio t
                        pieces @ [
                            { piece with
                                SourceFrom = interpolate piece.SourceFrom piece.SourceTo fromParameter
                                SourceTo = interpolate piece.SourceFrom piece.SourceTo toParameter
                                Segment = segment } ]))) (Ok [])))

    let private appendImageReference sourceIndex (reference: ArrangementSegmentEdgeImage) (images: ArrangementSourceSegmentImage list) =
        images |> List.map (fun image -> if image.SegmentIndex = sourceIndex then {image with Edges=image.Edges @ [reference]} else image)

    let private expandEdgeReferences edgeId (replacements: ArrangementSegmentEdgeImage list) (images: ArrangementSourceSegmentImage list) =
        let interpolate (a:float<parameter>) (b:float<parameter>) t =
            if t=0.0<parameter> then a elif t=1.0<parameter> then b
            else a+(b-a)*Parameter.ratio t
        images
        |> List.map (fun image ->
            let expanded = image.Edges |> List.collect (fun reference ->
                if reference.EdgeId <> edgeId then [reference]
                else
                    // Replacement bounds belong to the old stored edge.
                    // Reverse local intervals and order for reversed occurrences.
                    let ordered = if reference.Reversed then List.rev replacements else replacements
                    ordered |> List.map (fun replacement ->
                        let a,b = if reference.Reversed then 1.0<parameter> - replacement.To,1.0<parameter> - replacement.From else replacement.From,replacement.To
                        {EdgeId=replacement.EdgeId;From=interpolate reference.From reference.To a;To=interpolate reference.From reference.To b;Reversed=reference.Reversed<>replacement.Reversed;Own=false}))
            {image with Edges=expanded})

    let private nextEdgeId (edges: ArrangementEdge list) = edges |> List.fold (fun maximum edge -> max maximum (edge.Id + 1)) 0

    let private splitProgressiveGraphEdge (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) edgeId cuts tolerance minimumChord =
        match graph.Edges |> List.tryFind (fun edge -> edge.Id = edgeId) with
        | None -> Error(InternalMissingArrangementEdge edgeId)
        | Some edge ->
            distinctParameters edge.Segment tolerance (List.sort (0.0<parameter> :: 1.0<parameter> :: cuts))
            |> Result.bind (fun parameters ->
                Segment.betweenManyInside edge.Segment parameters
                |> Result.mapError InternalArrangementSegmentError
                |> Result.map (fun segments -> segments,parameters))
            |> Result.bind (fun (segments,parameters) ->
              retainedSplitSegments minimumChord segments |> Result.bind (fun retained ->
                match retained with
                | [] -> Error(InternalSegmentTooShort(0.0<length>, minimumChord))
                | _ ->
                    let firstId = edge.Id
                    let followingId = nextEdgeId graph.Edges
                    let folder (state: Result<ArrangementVertex list * ArrangementEdge list * ArrangementSegmentEdgeImage list, ArrangementInternalError>) (segment,(fromT,toT)) =
                        state
                        |> Result.bind (fun (vertices, replacements, references) ->
                          segmentLengthBound segment |> Result.bind (fun size ->
                            if size<minimumChord then Ok(vertices,replacements,references)
                            else
                                let id = if List.isEmpty replacements then firstId else followingId + replacements.Length - 1
                                let vertices, startVertex = attachVertex tolerance (Segment.start segment) vertices
                                let vertices, endVertex = attachVertex tolerance (Segment.finish segment) vertices
                                if startVertex = endVertex then Ok(vertices, replacements, references)
                                else
                                    Segment.boundingBox segment
                                    |> Result.mapError InternalArrangementSegmentError
                                    |> Result.map (fun bounds ->
                                        let replacement: ArrangementEdge =
                                            { Id = id; Segment = segment; Bounds = bounds
                                              StartVertex = startVertex; EndVertex = endVertex
                                              ForwardMultiplicity = edge.ForwardMultiplicity
                                              ReverseMultiplicity = edge.ReverseMultiplicity }
                                        vertices, replacements @ [ replacement ], references @ [ { EdgeId = id; Reversed = false;From=fromT;To=toT;Own=false } ])))
                    List.zip segments (List.pairwise parameters)
                    |> List.fold folder (Ok(graph.Vertices, [], []))
                    |> Result.map (fun (vertices, replacements, references) ->
                        let edges = graph.Edges |> List.collect (fun candidate -> if candidate.Id = edgeId then replacements else [ candidate ])
                        { Vertices = vertices; Edges = edges; CyclicOrders = [] }, expandEdgeReferences edgeId references images)))

    let private incomingContext (piece: AtomicPiece) (graph: ArrangementGraph) tolerance =
        Segment.boundingBox piece.Segment
        |> Result.mapError InternalArrangementSegmentError
        |> Result.bind (fun bounds ->
            uniqueVertexForEndpoint graph.Vertices (Segment.start piece.Segment) tolerance
            |> Result.bind (fun startMatch ->
                uniqueVertexForEndpoint graph.Vertices (Segment.finish piece.Segment) tolerance
                |> Result.map (fun endMatch -> { Piece = piece; Bounds = bounds; StartMatch = startMatch; EndMatch = endMatch })))

    let private splitPieceAtExistingVertex (piece: AtomicPiece) (graph: ArrangementGraph) tolerance minimumChord =
        Segment.boundingBox piece.Segment
        |> Result.mapError InternalArrangementSegmentError
        |> Result.bind (fun bounds ->
            let rec find (vertices: ArrangementVertex list) =
                match vertices with
                | [] -> Ok None
                | vertex :: rest ->
                    if not (pointInExpandedBox vertex.Point bounds tolerance) then find rest
                    else
                        vertexProjectsToPieceInterior vertex.Point piece.Segment tolerance
                        |> Result.bind (function Some t -> Ok(Some t) | None -> find rest)
            find graph.Vertices)
        |> Result.bind (function
            | None -> Ok None
            | Some cut -> splitAtomicPiece piece [ cut ] tolerance minimumChord |> Result.map Some)

    let private validatePieceEndpointVertices (piece: AtomicPiece) (graph: ArrangementGraph) tolerance =
        uniqueVertexForEndpoint graph.Vertices (Segment.start piece.Segment) tolerance
        |> Result.bind (fun _ -> uniqueVertexForEndpoint graph.Vertices (Segment.finish piece.Segment) tolerance)
        |> Result.map ignore

    let private sharesIncomingEndpoint (edge: ArrangementEdge) startMatch endMatch =
        [ startMatch; endMatch ]
        |> List.choose id
        |> List.exists (fun vertex -> vertex = edge.StartVertex || vertex = edge.EndVertex)

    let private progressiveEndpointSides t sliver =
        [ if t <= sliver then yield true
          if 1.0<parameter> - t <= sliver then yield false ]

    let private commonEndpointSliverByVertices (edge: ArrangementEdge) startMatch endMatch leftT rightT sliver =
        if sliver <= 0.0<parameter> then false
        else
            let vertexForLeft = function true -> Some edge.StartVertex | false -> Some edge.EndVertex
            let vertexForRight = function true -> startMatch | false -> endMatch
            progressiveEndpointSides leftT sliver
            |> List.exists (fun leftSide ->
                progressiveEndpointSides rightT sliver
                |> List.exists (fun rightSide -> vertexForLeft leftSide = vertexForRight rightSide))

    let private pairCuts (context: IncomingContext) (edge: ArrangementEdge) tolerance endpointSliverTolerance =
        let halfTolerance = LanguagePrimitives.FloatWithMeasure<length>(float tolerance / 2.0)
        let options = { Intersections.defaultOptions with Tolerance = halfTolerance }
        match Intersections.segmentWith edge.Segment context.Piece.Segment options with
        | Error OverlappingSegments when sharesIncomingEndpoint edge context.StartMatch context.EndMatch -> Ok([], [])
        | Error error -> Error(InternalArrangementSegmentError error)
        | Ok hits ->
            hits
            |> List.filter (fun hit ->
                not (commonEndpointSliverByVertices edge context.StartMatch context.EndMatch hit.LeftT hit.RightT endpointSliverTolerance))
            |> List.fold (fun (left, right) hit -> hit.LeftT :: left, hit.RightT :: right) ([], [])
            |> Ok

    let private findCorrespondingEdge (context: IncomingContext) (edges: ArrangementEdge list) tolerance =
        match context.StartMatch, context.EndMatch with
        | Some startVertex, Some endVertex ->
            let rec find (edges: ArrangementEdge list) =
                match edges with
                | [] -> Ok None
                | edge :: rest ->
                    let direction =
                        if edge.StartVertex = startVertex && edge.EndVertex = endVertex then Some true
                        elif edge.StartVertex = endVertex && edge.EndVertex = startVertex then Some false
                        else None
                    match direction with
                    | None -> find rest
                    | Some sameDirection ->
                        let rightFrom, rightTo = if sameDirection then 0.0<parameter>, 1.0<parameter> else 1.0<parameter>, 0.0<parameter>
                        Overlaps.checkParameterCorrespondence edge.Segment context.Piece.Segment 0.0<parameter> 1.0<parameter> rightFrom rightTo tolerance 7
                        |> Result.mapError InternalArrangementSegmentError
                        |> Result.bind (function Some _ -> Ok(Some(edge.Id, sameDirection)) | None -> find rest)
            find edges
        | _ -> Ok None

    let private incrementEdge (graph: ArrangementGraph) edgeId forward =
        { graph with
            Edges =
                graph.Edges
                |> List.map (fun edge ->
                    if edge.Id <> edgeId then edge
                    elif forward then { edge with ForwardMultiplicity = edge.ForwardMultiplicity + 1 }
                    else { edge with ReverseMultiplicity = edge.ReverseMultiplicity + 1 }) }

    let private insertCorrespondingPiece (context: IncomingContext) (graph: ArrangementGraph) tolerance minimumChord =
        findCorrespondingEdge context graph.Edges tolerance
        |> Result.bind (function
            | Some(edgeId, sameDirection) -> Ok(incrementEdge graph edgeId sameDirection, edgeId, not sameDirection)
            | None ->
                let edgeId = nextEdgeId graph.Edges
                insertAtomicSegment graph context.Piece.Segment tolerance minimumChord
                |> Result.map (fun next -> next, edgeId, false))

    let private splitExistingEdgeAtEndpoint (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) endpoint tolerance minimumChord =
        let rec find (edges: ArrangementEdge list) =
            match edges with
            | [] -> Ok None
            | edge :: rest when not (pointInExpandedBox endpoint edge.Bounds tolerance) -> find rest
            | edge :: rest ->
                vertexProjectsToPieceInterior endpoint edge.Segment tolerance
                |> Result.bind (function
                    | None -> find rest
                    | Some t ->
                        effectiveCutParameters edge.Segment [ t ] tolerance minimumChord
                        |> Result.bind (function
                            | [] -> find rest
                            | cuts -> splitProgressiveGraphEdge graph images edge.Id cuts tolerance minimumChord |> Result.map Some))
        find graph.Edges

    let private splitExistingEdgeAtIncomingEndpoint (context: IncomingContext) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) tolerance minimumChord =
        splitExistingEdgeAtEndpoint graph images (Segment.start context.Piece.Segment) tolerance minimumChord
        |> Result.bind (function
            | Some result -> Ok(Some result)
            | None -> splitExistingEdgeAtEndpoint graph images (Segment.finish context.Piece.Segment) tolerance minimumChord)

    let private progressiveCompareEdgeCuts (context: IncomingContext) (edge: ArrangementEdge) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) existingCuts incomingCuts tolerance minimumChord =
        effectiveCutParameters edge.Segment existingCuts tolerance minimumChord
        |> Result.bind (fun existingParameters ->
            effectiveCutParameters context.Piece.Segment incomingCuts tolerance minimumChord
            |> Result.bind (fun incomingParameters ->
                match existingParameters, incomingParameters with
                | [], [] -> Ok(ProgressiveContinue(graph, images))
                | _ ->
                    (if List.isEmpty existingParameters then Ok(graph, images)
                     else splitProgressiveGraphEdge graph images edge.Id existingParameters tolerance minimumChord)
                    |> Result.bind (fun (graph, images) ->
                        match incomingParameters with
                        | [] -> Ok(ProgressiveReplaceIncoming(graph, images, [ context.Piece ]))
                        | cuts ->
                            splitAtomicPiece context.Piece cuts tolerance minimumChord
                            |> Result.map (fun replacements -> ProgressiveReplaceIncoming(graph, images, replacements)))))

    let private progressiveInsertDirect (context:IncomingContext) graph images tolerance minimumChord =
        let piece = context.Piece
        // Existing-edge comparisons are finished. Children re-enter ordinary
        // insertion, which will discover their mutual non-endpoint intersections.
        Intersections.segmentSelfWith piece.Segment {MinimumArcLengthSeparation=minimumChord;DistanceTolerance=tolerance/2.0}
        |> Result.mapError InternalArrangementSegmentError
        |> Result.bind (function
            | hit::_ ->
                let middle = hit.LeftT + (hit.RightT-hit.LeftT)/2.0
                let sourceMiddle = piece.SourceFrom + (piece.SourceTo-piece.SourceFrom)*Parameter.ratio middle
                if piece.SelfSplitDepth>=32 || not(middle>0.0<parameter> && middle<1.0<parameter>)
                   || not(sourceMiddle>piece.SourceFrom && sourceMiddle<piece.SourceTo) then
                    Error(InternalSelfIntersectionSubdivisionFailed piece.SourceIndex)
                else
                    Segment.split piece.Segment middle |> Result.mapError InternalArrangementSegmentError
                    |> Result.bind (fun (left,right) ->
                        let depth = piece.SelfSplitDepth+1
                        [{piece with Segment=left;SourceTo=sourceMiddle;SelfSplitDepth=depth};{piece with Segment=right;SourceFrom=sourceMiddle;SelfSplitDepth=depth}]
                        |> List.fold (fun state child -> state |> Result.bind (fun kept ->
                            segmentLengthBound child.Segment |> Result.map (fun size -> if size>=minimumChord then kept @ [child] else kept))) (Ok [])
                        |> Result.map (fun replacements -> ProgressivePieceReplaced(graph,images,replacements)))
            | [] ->
                insertCorrespondingPiece context graph tolerance minimumChord
                |> Result.map (fun (graph,edgeId,reversed) ->
                    let reference:ArrangementSegmentEdgeImage = {EdgeId=edgeId;Reversed=reversed;From=piece.SourceFrom;To=piece.SourceTo;Own=false}
                    ProgressivePieceInserted(graph,appendImageReference piece.SourceIndex reference images))
                |> function
                    | Error(InternalSegmentCollapsedToVertex _) | Error(InternalSegmentTooShort _) -> Ok(ProgressivePieceInserted(graph,images))
                    | result -> result)

    let private progressiveCompareEdges (context: IncomingContext) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) tolerance minimumChord endpointSliverTolerance =
        let rec compare graph images (edges: ArrangementEdge list) =
            match edges with
            | [] ->
                progressiveInsertDirect context graph images tolerance minimumChord
            | edge :: rest when not (boundingBoxesOverlap context.Bounds edge.Bounds tolerance) -> compare graph images rest
            | edge :: rest ->
                // Shared endpoint vertices do not exclude interior intersections.
                pairCuts context edge tolerance endpointSliverTolerance
                |> Result.bind (fun (existingCuts, incomingCuts) ->
                    progressiveCompareEdgeCuts context edge graph images existingCuts incomingCuts tolerance minimumChord)
                |> Result.bind (function
                    | ProgressiveContinue(nextGraph, nextImages) -> compare nextGraph nextImages rest
                    | ProgressiveReplaceIncoming(nextGraph, nextImages, replacements) ->
                        Ok(ProgressivePieceReplaced(nextGraph, nextImages, replacements)))
        compare graph images graph.Edges

    let private progressiveInsertPiece (piece: AtomicPiece) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) tolerance minimumChord endpointSliverTolerance =
        incomingContext piece graph tolerance
        |> Result.bind (fun context ->
            splitExistingEdgeAtIncomingEndpoint context graph images tolerance minimumChord
            |> Result.bind (function
                | Some(graph, images) -> Ok(ProgressivePieceReplaced(graph, images, [ piece ]))
                | None -> progressiveCompareEdges context graph images tolerance minimumChord endpointSliverTolerance))

    let private progressiveInsertPieces (pieces: AtomicPiece list) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) tolerance minimumChord endpointSliverTolerance =
        let rec loop (stack: AtomicPiece list) (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) =
            match stack with
            | [] -> Ok(graph, images)
            | piece :: rest ->
                splitPieceAtExistingVertex piece graph tolerance minimumChord
                |> Result.bind (function
                    | Some replacements -> loop (replacements @ rest) graph images
                    | None ->
                        validatePieceEndpointVertices piece graph tolerance
                        |> Result.bind (fun () -> progressiveInsertPiece piece graph images tolerance minimumChord endpointSliverTolerance)
                        |> Result.bind (function
                            | ProgressivePieceInserted(graph, images) -> loop rest graph images
                            | ProgressivePieceReplaced(graph, images, replacements) -> loop (replacements @ rest) graph images))
        loop pieces graph images

    let private atomicPieces minimumChord (indexed: IndexedSegment list) =
        indexed
        |> List.fold (fun state source -> state |> Result.bind (fun pieces ->
          segmentLengthBound source.Segment |> Result.map (fun size ->
            if size < minimumChord then pieces
            else
                pieces @ [
                    { SourceIndex = source.FlatIndex
                      PathIndex = source.PathIndex
                      SubpathIndex = source.SubpathIndex
                      SegmentIndex = source.SegmentIndex
                      SourceFrom = 0.0<parameter>
                      SourceTo = 1.0<parameter>
                      SelfSplitDepth = 0
                      Segment = source.Segment } ]))) (Ok [])

    let private markOwnership (images: ArrangementSourceSegmentImage list) =
        images
        |> List.mapFold (fun owned image ->
            let edges, owned =
                image.Edges
                |> List.mapFold (fun owned occurrence ->
                    let own = not (Set.contains occurrence.EdgeId owned)
                    { occurrence with Own = own }, Set.add occurrence.EdgeId owned) owned
            { image with Edges = edges }, owned) Set.empty
        |> fst

    let private edgeSourceImages (graph: ArrangementGraph) (images: ArrangementSourceSegmentImage list) =
        graph.Edges
        |> List.map (fun edge ->
            { EdgeId = edge.Id
              Sources =
                images
                |> List.collect (fun image ->
                    image.Edges
                    |> List.choose (fun occurrence ->
                        if occurrence.EdgeId <> edge.Id then None
                        else
                            Some
                                { SegmentIndex = image.SegmentIndex
                                  From = min occurrence.From occurrence.To
                                  To = max occurrence.From occurrence.To
                                  Reversed = occurrence.Reversed })) })

    let private certifySegmentEdgesExist (graph: ArrangementGraph) images =
        images
        |> List.collect (fun (image: ArrangementSourceSegmentImage) -> image.Edges)
        |> List.fold (fun state image ->
            state
            |> Result.bind (fun () ->
                if graph.Edges |> List.exists (fun edge -> edge.Id = image.EdgeId) then Ok()
                else Error(InternalMissingArrangementEdge image.EdgeId))) (Ok())

    let private edgeImagesContainSource (edgeImages: ArrangementEdgeImage list) edgeId segmentIndex fromParameter toParameter reversed =
        edgeImages
        |> List.exists (fun image ->
            image.EdgeId = edgeId
            && (image.Sources
                |> List.exists (fun (source: ArrangementEdgeSourceImage) ->
                    source.SegmentIndex = segmentIndex
                    && source.From = fromParameter
                    && source.To = toParameter
                    && source.Reversed = reversed)))

    let private segmentImagesContainEdge (segmentImages: ArrangementSourceSegmentImage list) segmentIndex edgeId fromParameter toParameter reversed =
        segmentImages
        |> List.exists (fun image ->
            image.SegmentIndex = segmentIndex
            && (image.Edges
                |> List.exists (fun (edge: ArrangementSegmentEdgeImage) ->
                    edge.EdgeId = edgeId
                    && min edge.From edge.To = fromParameter
                    && max edge.From edge.To = toParameter
                    && edge.Reversed = reversed)))

    let private certifyImageCorrespondence (segmentImages: ArrangementSourceSegmentImage list) (edgeImages: ArrangementEdgeImage list) =
        segmentImages
        |> List.fold (fun state (image: ArrangementSourceSegmentImage) ->
            image.Edges
            |> List.fold (fun state (edge: ArrangementSegmentEdgeImage) ->
                state
                |> Result.bind (fun () ->
                    if edgeImagesContainSource edgeImages edge.EdgeId image.SegmentIndex (min edge.From edge.To) (max edge.From edge.To) edge.Reversed then Ok()
                    else Error InternalNormalizationError)) state) (Ok())
        |> Result.bind (fun () ->
            edgeImages
            |> List.fold (fun state (image: ArrangementEdgeImage) ->
                image.Sources
                |> List.fold (fun state (source: ArrangementEdgeSourceImage) ->
                    state
                    |> Result.bind (fun () ->
                        if segmentImagesContainEdge segmentImages source.SegmentIndex image.EdgeId source.From source.To source.Reversed then Ok()
                        else Error InternalNormalizationError)) state) (Ok()))

    let private certifySegmentImageGeometry (graph: ArrangementGraph) segments images tolerance =
        images
        |> List.fold (fun state (image: ArrangementSourceSegmentImage) ->
            state
            |> Result.bind (fun () ->
                match List.tryItem image.SegmentIndex segments with
                | None -> Error InternalNormalizationError
                | Some source ->
                    image.Edges
                    |> List.fold (fun state imageEdge ->
                        state
                        |> Result.bind (fun () ->
                            match graph.Edges |> List.tryFind (fun edge -> edge.Id = imageEdge.EdgeId) with
                            | None -> Error(InternalMissingArrangementEdge imageEdge.EdgeId)
                            | Some edge ->
                                Segment.point source imageEdge.From
                                |> Result.mapError InternalArrangementSegmentError
                                |> Result.bind (fun sourceA ->
                                    Segment.point source imageEdge.To
                                    |> Result.mapError InternalArrangementSegmentError
                                    |> Result.bind (fun sourceB ->
                                        let edgeA, edgeB =
                                            if imageEdge.Reversed then Segment.finish edge.Segment, Segment.start edge.Segment
                                            else Segment.start edge.Segment, Segment.finish edge.Segment
                                        if Point.distance sourceA edgeA <= tolerance && Point.distance sourceB edgeB <= tolerance then Ok()
                                        else Error InternalNormalizationError)))) state)) (Ok())

    let private certifySegmentBuild graph segments segmentImages edgeImages tolerance =
        certifySegmentEdgesExist graph segmentImages
        |> Result.bind (fun () -> certifyImageCorrespondence segmentImages edgeImages)
        |> Result.bind (fun () -> certifySegmentImageGeometry graph segments segmentImages tolerance)

    /// Build directly from a flat segment list without source normalization.
    /// Self-intersections are checked after existing edges. Split children
    /// re-enter ordinary insertion. minimumChord filters by length upper bound,
    /// so zero-chord loops are not discarded merely for coincident endpoints.
    let internal buildWith segments vertexTolerance minimumChord (endpointSliverTolerance: float<parameter>) =
        if vertexTolerance <= 0.0<length> || not (finite vertexTolerance) then Error(InternalInvalidArrangementTolerance vertexTolerance)
        elif minimumChord <= 0.0<length> || not (finite minimumChord) then Error(InternalInvalidMinimumChord minimumChord)
        elif endpointSliverTolerance < 0.0<parameter> || System.Double.IsNaN(float endpointSliverTolerance) || System.Double.IsInfinity(float endpointSliverTolerance) then
            Error(InternalInvalidEndpointSliverTolerance endpointSliverTolerance)
        else
            let indexed =
                segments
                |> List.indexed
                |> List.map (fun (index, segment) ->
                    { FlatIndex = index; PathIndex = 0; SubpathIndex = 0; SegmentIndex = index; Segment = segment })
            let workingImages = indexed |> List.map (fun source -> {SegmentIndex=source.FlatIndex;Edges=[]})
            atomicPieces minimumChord indexed
            |> Result.bind (fun pieces -> progressiveInsertPieces pieces empty workingImages vertexTolerance minimumChord endpointSliverTolerance)
            |> Result.bind (fun (graph, workingImages) ->
                    let images = markOwnership workingImages
                    let edgeImages = edgeSourceImages graph images
                    certifySegmentBuild graph segments images edgeImages vertexTolerance
                    |> Result.bind (fun () ->
                        cyclicOrders graph vertexTolerance
                        |> Result.map (fun orders ->
                            let graph = { graph with CyclicOrders = orders }
                            { Graph = graph; Segments = segments; SegmentImages = images; EdgeImages = edgeImages })))

    /// Build an arrangement and preserve each input path segment's edge image.
    /// Nodes paths into an arrangement and records every source segment image.
    let private buildInternal (paths: Path list) tolerance minimumChord =
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

    /// Builds a planar arrangement and ordered source-segment edge images.
    let build paths tolerance minimumChord =
        buildInternal paths tolerance minimumChord
        |> Result.mapError publicError

    let private segmentImageEdgesInternal (build: ArrangementGraphBuild) (image: ArrangementSegmentImage) =
        image.Edges
        |> List.fold (fun state reference ->
            state
            |> Result.bind (fun edges ->
                match build.Graph.Edges |> List.tryFind (fun edge -> edge.Id = reference.EdgeId) with
                | Some edge -> Ok(edges @ [ edge, reference.Reversed ])
                | None -> Error(InternalMissingArrangementEdge reference.EdgeId))) (Ok [])

    let segmentImageEdges build image =
        segmentImageEdgesInternal build image
        |> Result.mapError publicError

    let private faceEdgeEqual (left: ArrangementFaceEdge) (right: ArrangementFaceEdge) = left.EdgeId = right.EdgeId && left.Left = right.Left

    let private faceSuccessor (graph: ArrangementGraph) (current: ArrangementFaceEdge) =
        match graph.Edges |> List.tryFind (fun edge -> edge.Id = current.EdgeId) with
        | None -> Error(InternalMissingArrangementEdge current.EdgeId)
        | Some edge ->
            let arrival = if current.Left then edge.EndVertex else edge.StartVertex
            let incomingReversed = current.Left
            match graph.CyclicOrders |> List.tryFind (fst >> (=) arrival) with
            | None -> Error(InternalDualMissingCyclicOrder arrival)
            | Some(_, groups) ->
                let order = List.concat groups
                match order |> List.tryFindIndex (fun item -> item.EdgeId = current.EdgeId && item.Reversed = incomingReversed) with
                | None -> Error(InternalDualMissingIncidentEdge(arrival, current.EdgeId))
                | Some index ->
                    let next = order[(index + 1) % order.Length]
                    let result: ArrangementFaceEdge = { EdgeId = next.EdgeId; Left = not next.Reversed }
                    Ok result

    let private faceWalk (graph: ArrangementGraph) (start: ArrangementFaceEdge) =
        let rec loop (current: ArrangementFaceEdge) (visited: ArrangementFaceEdge list) remaining =
            if remaining <= 0 then Error(InternalDualWalkDidNotClose(start.EdgeId, start.Left))
            else
                faceSuccessor graph current
                |> Result.bind (fun next ->
                    let visited = current :: visited
                    if faceEdgeEqual next start then Ok(List.rev visited)
                    elif visited |> List.exists (faceEdgeEqual next) then Error(InternalDualWalkDidNotClose(start.EdgeId, start.Left))
                    else loop next visited (remaining - 1))
        loop start [] (graph.Edges.Length * 2 + 1)

    type private FaceCandidate =
        { Walk: ArrangementFaceWalk
          Signature: bool list }

    // Acceptance and confirmation are separate: repeating a rejected tangent,
    // vertex, overlap or inseparable crossing does not make it usable.
    let private dualSweepConfirmations = 2
    let private dualSweepAttemptsPerQuestion = 64
    let private dualSweepMinimumSine = 0.000001

    type private DualSweepEdge =
        { Id: int; Component: int; LeftWalk: int; RightWalk: int
          Segment: Segment; Bounds: BoundingBox }
    type private DualSweepHit =
        { EdgeId: int; Position: float<length>; Uncertainty: float<length>
          Component: int; Before: int; After: int }
    type private DualExterior = { Component: int; Walk: int; Confirmations: int }
    type private DualPlacement = { Walk: int; Signature: bool list; Confirmations: int }
    type private DualSweepLine = { Origin: Point<length>; Direction: Point<1> }

    let private dualTryMap f values =
        values |> List.fold (fun state value ->
            state |> Result.bind (fun reversed -> f value |> Result.map (fun v -> v::reversed))) (Ok [])
        |> Result.map List.rev

    // Components use graph vertex identity, never geometric proximity.
    let private dualComponents (edges: ArrangementEdge list) =
        let rec grow current remaining =
            let vertices = current |> List.collect (fun (e: ArrangementEdge) -> [e.StartVertex;e.EndVertex])
            let attached, rest = remaining |> List.partition (fun e -> List.contains e.StartVertex vertices || List.contains e.EndVertex vertices)
            if List.isEmpty attached then current, rest else grow (current @ attached) rest
        let rec loop remaining found =
            match remaining with
            | [] -> List.rev found
            | first::rest ->
                let connected, rest = grow [first] rest
                loop rest ((connected |> List.map _.Id)::found)
        loop edges []

    let private dualWalkIndex (walks: ArrangementFaceWalk list) edge left =
        walks |> List.tryFindIndex (fun walk -> walk.Edges |> List.exists (fun e -> e.EdgeId=edge && e.Left=left))
        |> function Some id -> Ok id | None -> Error(InternalDualMissingEdgeFace(edge,left))

    let private dualSweepEdges (edges: ArrangementEdge list) components walks =
        edges |> dualTryMap (fun edge ->
            dualWalkIndex walks edge.Id true |> Result.bind (fun left ->
                dualWalkIndex walks edge.Id false |> Result.bind (fun right ->
                    Segment.boundingBox edge.Segment |> Result.mapError InternalArrangementSegmentError
                    |> Result.map (fun bounds ->
                        { Id=edge.Id; Component=components |> List.findIndex (List.contains edge.Id)
                          LeftWalk=left; RightWalk=right; Segment=edge.Segment; Bounds=bounds }))))

    let private dualSweepTolerance (graph: ArrangementGraph) =
        graph.Vertices |> List.fold (fun tolerance vertex ->
            let rounding = 7.2e-15 * max 1.0<length> (max (abs vertex.Point.X) (abs vertex.Point.Y))
            vertex.EndpointSamples |> List.fold (fun tolerance sample ->
                max tolerance (2.0 * Point.distance vertex.Point sample + rounding)) (max tolerance rounding)) 1e-9<length>

    // Int64 preserves the Park-Miller product, as Gleam's exact integer does.
    let private dualRandom seed = seed * 48271L % 2147483647L
    let private dualRandomPoint (edges: DualSweepEdge list) seed =
        let edge = edges[int(seed % int64 edges.Length)]
        let t = 0.15 + 0.7 * float(dualRandom seed % 10000L) / 10000.0
        Segment.point edge.Segment (Parameter.fromFloat t) |> Result.mapError InternalArrangementSegmentError

    let private dualOuterLine edges componentId seed =
        dualRandomPoint (edges |> List.filter (fun e -> e.Component=componentId)) seed
        |> Result.map (fun origin ->
            { Origin=origin; Direction=Point.direction (Degree.fromFloat(float(dualRandom seed % 360000L)/1000.0)) })

    let private dualPairLine (edges: DualSweepEdge list) walk seed =
        let own = edges |> List.filter (fun e -> e.LeftWalk=walk || e.RightWalk=walk)
        let others = edges |> List.filter (fun e -> e.Component<>own.Head.Component)
        if List.isEmpty others || seed % 3L = 0L then
            dualRandomPoint own seed |> Result.map (fun origin ->
                { Origin=origin; Direction=Point.direction(Degree.fromFloat(float(dualRandom seed % 360000L)/1000.0)) })
        else
            let other = others[int(dualRandom seed % int64 others.Length)]
            let target = others |> List.filter (fun e -> e.LeftWalk=other.LeftWalk || e.RightWalk=other.LeftWalk)
            dualRandomPoint own seed |> Result.bind (fun origin ->
                dualRandomPoint target (dualRandom seed) |> Result.map (fun finish ->
                    { Origin=origin
                      Direction=Point.displacement origin finish |> Point.normalize
                                |> Option.defaultValue (Point.direction 37.0<degree>) }))

    let private dualSignedLineDistance p line =
        let delta = Point.subtract p line.Origin
        line.Direction.X * delta.Y - line.Direction.Y * delta.X

    let private dualSweepEdgeHits edge line tolerance =
        let box = edge.Bounds
        let distances = [box.Min;box.Max;Point.create box.Min.X box.Max.Y;Point.create box.Max.X box.Min.Y]
                        |> List.map (fun p -> dualSignedLineDistance p line)
        if List.forall (fun d -> d > tolerance) distances || List.forall (fun d -> d < -tolerance) distances then Ok []
        else
            Segment.rayCrossingsWith edge.Segment line.Origin line.Direction
                { Segment.defaultCrossingOptions with SignedLineDistanceTolerance=tolerance*0.01 }
            |> Result.mapError (fun _ -> ())
            |> Result.bind (dualTryMap (fun (t, position) ->
                Segment.point edge.Segment t |> Result.mapError (fun _ -> ()) |> Result.bind (fun p ->
                    Segment.derivative edge.Segment t |> Result.mapError (fun _ -> ()) |> Result.bind (fun derivative ->
                        match Point.normalize derivative with
                        | None -> Error()
                        | Some tangent ->
                            let determinant = tangent.X * line.Direction.Y - tangent.Y * line.Direction.X
                            if t <= 1e-9<parameter> || t >= 0.999999999<parameter>
                               || not(System.Double.IsFinite(float position)) || not(System.Double.IsFinite determinant)
                               || abs(dualSignedLineDistance p line)>tolerance || abs determinant<=dualSweepMinimumSine then Error()
                            else
                                let before, after = if determinant>0.0 then edge.LeftWalk,edge.RightWalk else edge.RightWalk,edge.LeftWalk
                                Ok { EdgeId=edge.Id; Position=position; Uncertainty=tolerance/abs determinant
                                     Component=edge.Component; Before=before; After=after }))))

    let private dualCheckHitSeparation (hits: DualSweepHit list) tolerance =
        if hits |> List.pairwise |> List.exists (fun (a,b) -> b.Position-a.Position <= max tolerance (a.Uncertainty+b.Uncertainty)) then Error()
        else Ok()

    // No conclusions escape before the complete infinite line is validated.
    let private dualSweepIntersections edges (vertices: ArrangementVertex list) line tolerance =
        if vertices |> List.exists (fun v -> abs(dualSignedLineDistance v.Point line)<=tolerance) then Error()
        else
            dualTryMap (fun edge -> dualSweepEdgeHits edge line tolerance) edges
            |> Result.bind (fun batches ->
                let hits = List.concat batches |> List.sortBy _.Position
                dualCheckHitSeparation hits tolerance |> Result.map (fun () -> hits))

    let private dualCheckLocalSequence hits current exterior =
        let rec loop hits current =
            match hits with
            | [] -> if current=exterior then Ok() else Error(InternalDualSweepContradiction current)
            | hit::rest -> if hit.Before=current then loop rest hit.After else Error(InternalDualSweepContradiction hit.Before)
        loop hits current

    let rec private dualLineExteriors (hits: DualSweepHit list) =
        match hits with
        | [] -> Ok []
        | first::_ ->
            let own, rest = hits |> List.partition (fun h -> h.Component=first.Component)
            dualCheckLocalSequence own first.Before first.Before |> Result.bind (fun () ->
                dualLineExteriors rest |> Result.map (fun others ->
                    { Component=first.Component; Walk=first.Before; Confirmations=1 }::others))

    let rec private dualMergeExteriors (newClaims: DualExterior list) (old: DualExterior list) =
        match newClaims with
        | [] -> Ok old
        | e::rest ->
            let merged =
                match old |> List.tryFind (fun p -> p.Component=e.Component) with
                | None -> Ok(e::old)
                | Some previous when previous.Walk<>e.Walk -> Error(InternalDualSweepContradiction e.Walk)
                | Some _ -> old |> List.map (fun p -> if p.Component=e.Component then {p with Confirmations=p.Confirmations+1} else p) |> Ok
            merged |> Result.bind (dualMergeExteriors rest)

    let rec private dualFindExteriors edges vertices tolerance count found seed remaining =
        let unresolved = [0..count-1] |> List.filter (fun id -> not(found |> List.exists (fun e -> e.Component=id && e.Confirmations>=dualSweepConfirmations)))
        match unresolved with
        | [] -> Ok found
        | _ when remaining<=0 -> Error(InternalDualSweepExhausted unresolved.Length)
        | id::_ ->
            dualOuterLine edges id seed |> Result.bind (fun line ->
                let next =
                    match dualSweepIntersections edges vertices line tolerance with
                    | Error _ -> Ok found
                    | Ok hits -> dualLineExteriors hits |> Result.bind (fun claims -> dualMergeExteriors claims found)
                next |> Result.bind (fun next -> dualFindExteriors edges vertices tolerance count next (dualRandom seed) (remaining-1)))

    let private dualSignature states (exteriors: DualExterior list) (walks: ArrangementFaceWalk list) =
        walks |> List.mapi (fun id _ -> not(exteriors |> List.exists (fun e -> e.Walk=id)) && List.exists (fun (_,walk) -> walk=id) states)

    let rec private dualMergePlacements (newClaims: DualPlacement list) (old: DualPlacement list) confirm =
        match newClaims with
        | [] -> Ok old
        | p::rest ->
            let merged =
                match old |> List.tryFind (fun q -> q.Walk=p.Walk) with
                | None -> Ok(p::old)
                | Some previous when previous.Signature<>p.Signature -> Error(InternalDualSweepContradiction p.Walk)
                | Some _ -> old |> List.map (fun q -> if q.Walk=p.Walk && confirm then {q with Confirmations=q.Confirmations+1} else q) |> Ok
            merged |> Result.bind (fun merged -> dualMergePlacements rest merged confirm)

    let rec private dualLinePlacements hits states exteriors walks found =
        match hits with
        | [] -> Ok found
        | hit::rest ->
            let before = {Walk=hit.Before; Signature=dualSignature states exteriors walks; Confirmations=1}
            let states = states |> List.map (fun (id,walk) -> id,(if id=hit.Component then hit.After else walk))
            let after = {Walk=hit.After; Signature=dualSignature states exteriors walks; Confirmations=1}
            dualMergePlacements [before;after] found false
            |> Result.bind (dualLinePlacements rest states exteriors walks)

    let rec private dualFindPlacements edges vertices tolerance (walks: ArrangementFaceWalk list) (exteriors: DualExterior list) found seed remaining =
        let unresolved = [0..walks.Length-1] |> List.filter (fun id -> not(found |> List.exists (fun p -> p.Walk=id && p.Confirmations>=dualSweepConfirmations)))
        match unresolved with
        | [] -> Ok found
        | _ when remaining<=0 -> Error(InternalDualSweepExhausted unresolved.Length)
        | walk::_ ->
            dualPairLine edges walk seed |> Result.bind (fun line ->
                let next =
                    match dualSweepIntersections edges vertices line tolerance with
                    | Error _ -> Ok found
                    | Ok hits ->
                        dualLineExteriors hits |> Result.bind (fun claims -> dualMergeExteriors claims exteriors)
                        |> Result.bind (fun _ -> dualLinePlacements hits (exteriors |> List.map (fun e -> e.Component,e.Walk)) exteriors walks [])
                        |> Result.bind (fun placements -> dualMergePlacements placements found true)
                next |> Result.bind (fun next -> dualFindPlacements edges vertices tolerance walks exteriors next (dualRandom seed) (remaining-1)))

    let private dualWalkCandidates (graph: ArrangementGraph) walks =
        let components = dualComponents graph.Edges
        dualSweepEdges graph.Edges components walks |> Result.bind (fun edges ->
            let tolerance = dualSweepTolerance graph
            let budget = dualSweepAttemptsPerQuestion * (List.length walks + components.Length)
            dualFindExteriors edges graph.Vertices tolerance components.Length [] 1729L budget
            |> Result.bind (fun exteriors ->
                dualFindPlacements edges graph.Vertices tolerance walks exteriors [] 7919L budget
                |> Result.map (fun placements ->
                    walks |> List.mapi (fun id walk ->
                        let placement = placements |> List.find (fun p -> p.Walk=id)
                        { Walk={walk with Outer=not(exteriors |> List.exists (fun e -> e.Walk=id))}; Signature=placement.Signature }))))

    let private facesFromCandidates (candidates: FaceCandidate list) =
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
                    | _, walks -> Error(InternalDualInvalidOuterWalkCount walks.Length))) (Ok [])
        | groups -> Error(InternalDualInvalidOuterFaceCount groups.Length)

    /// Derive face boundary walks and the face on each side of every edge.
    /// Accepted infinite-line sweeps group walks; ambiguous vertex, tangent,
    /// overlap and inseparable crossing lines are rejected. Independent lines
    /// must agree. No displaced containment probes are used.
    /// Walks all faces and constructs the dual incidence representation.
    let private dualInternal (graph: ArrangementGraph) =
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
            |> Result.map (List.map (fun edges -> {Outer=false;Edges=edges}))
            |> Result.bind (dualWalkCandidates graph)
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
                        | None, _ -> Error(InternalDualMissingEdgeFace(edge.Id, true))
                        | _, None -> Error(InternalDualMissingEdgeFace(edge.Id, false)))) (Ok [])
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

    let dual graph =
        dualInternal graph
        |> Result.mapError publicError

    type private WindingNeighbor = { EdgeId: int; FaceId: int; Change: int }

    let private addWindingNeighbor neighbors faceId neighbor =
        Map.add faceId (neighbor :: (Map.tryFind faceId neighbors |> Option.defaultValue [])) neighbors

    let private windingNeighbors (edges: ArrangementEdgeFaces list) faceIds changes =
        edges |> List.fold (fun state edge ->
            state |> Result.bind (fun neighbors ->
                if not(Set.contains edge.LeftFace faceIds && Set.contains edge.RightFace faceIds) then Error InvalidWindingDual
                else
                    match Map.tryFind edge.EdgeId changes with
                    | None -> Error(MissingWindingChange edge.EdgeId)
                    | Some change ->
                        let neighbors = addWindingNeighbor neighbors edge.LeftFace {EdgeId=edge.EdgeId;FaceId=edge.RightFace;Change=change}
                        addWindingNeighbor neighbors edge.RightFace {EdgeId=edge.EdgeId;FaceId=edge.LeftFace;Change= -change} |> Ok)) (Ok Map.empty)

    let rec private assignWindingNeighbors neighbors value pending assigned =
        match neighbors with
        | [] -> Ok(pending, assigned)
        | neighbor::rest ->
            let required = value + neighbor.Change
            match Map.tryFind neighbor.FaceId assigned with
            | Some existing when existing<>required -> Error(ContradictoryWinding(neighbor.EdgeId,neighbor.FaceId,existing,required))
            | Some _ -> assignWindingNeighbors rest value pending assigned
            | None -> assignWindingNeighbors rest value (neighbor.FaceId::pending) (Map.add neighbor.FaceId required assigned)

    let rec private propagateFaceWindings pending neighbors assigned =
        match pending with
        | [] -> Ok assigned
        | faceId::rest ->
            let incident = Map.tryFind faceId neighbors |> Option.defaultValue []
            assignWindingNeighbors incident (Map.find faceId assigned) rest assigned
            |> Result.bind (fun (pending,assigned) -> propagateFaceWindings pending neighbors assigned)

    /// Assign integer face windings from infinity=0. Every edge requires one
    /// supplied change, including zero; callers sum opposite contributions.
    /// Separate from dual construction: an open boundary can have a valid dual
    /// without consistent winding. No geometry is sampled or changed.
    let internal faceWindings (dual: DualArrangementGraph) (changes: EdgeWindingChange list) =
        let faceIds = dual.Faces |> List.map _.Id |> Set.ofList
        let changeMap = changes |> List.map (fun c -> c.EdgeId,c.RightMinusLeft) |> Map.ofList
        let edgeIds = dual.EdgeFaces |> List.map _.EdgeId |> Set.ofList
        if faceIds.Count<>dual.Faces.Length || edgeIds.Count<>dual.EdgeFaces.Length then Error InvalidWindingDual
        elif changeMap.Count<>changes.Length || changeMap.Count<>edgeIds.Count then Error InvalidWindingChanges
        else
            match dual.Faces |> List.filter _.Outer with
            | [outer] ->
                windingNeighbors dual.EdgeFaces faceIds changeMap
                |> Result.bind (fun neighbors -> propagateFaceWindings [outer.Id] neighbors (Map.ofList [outer.Id,0]))
                |> Result.bind (fun assigned ->
                    dual.Faces |> dualTryMap (fun face ->
                        match Map.tryFind face.Id assigned with
                        | None -> Error(UnreachableWindingFace face.Id)
                        | Some value -> Ok {FaceId=face.Id;Value=value}))
            | _ -> Error InvalidWindingDual

    let private nestedContourEdges
        (graph: ArrangementGraph)
        (path: Path)
        (tolerance: float<length>) =
        graph.Edges
        |> List.fold (fun state edge ->
            state
            |> Result.bind (fun classified ->
                WindingField.segmentSideNonzeroLevels
                    edge.Segment
                    path
                    (tolerance * 16.0)
                    { WindingField.defaultOptions with Tolerance=tolerance }
                |> Result.mapError InternalArrangementSegmentError
                |> Result.map (fun (left, right) -> classified @ [ edge, left, right ]))) (Ok [])
        |> Result.map (fun classified ->
            classified
            |> List.collect (fun (edge, left, right) ->
                let maximumLayer = max (abs left) (abs right)
                [ for magnitude in 1 .. maximumLayer do
                    for sign in [ 1; -1 ] do
                        let layer = sign * magnitude
                        let leftActive, rightActive =
                            if layer > 0 then left >= layer, right >= layer
                            else left <= layer, right <= layer
                        if leftActive <> rightActive then
                            if leftActive then
                                yield
                                    { Id = 0
                                      Layer = layer
                                      Segment = edge.Segment
                                      StartVertex = edge.StartVertex
                                      EndVertex = edge.EndVertex }
                            else
                                yield
                                    { Id = 0
                                      Layer = layer
                                      Segment = Segment.reverse edge.Segment
                                      StartVertex = edge.EndVertex
                                      EndVertex = edge.StartVertex } ])
            |> List.mapi (fun id edge -> { edge with Id = id }))

    let private nestedContourSuccessors (edges: NestedContourEdge list) =
        let ray edge starts =
            let parameter = if starts then 0.0<parameter> else 1.0<parameter>
            Segment.directions edge.Segment parameter
            |> Result.mapError InternalArrangementSegmentError
            |> Result.bind (fun directions ->
                let direction = if starts then directions.Outgoing else directions.Incoming
                match direction with
                | None -> Error(InternalContourTraceFailed edge.Id)
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
                                else Error(InternalContourTraceFailed incoming.EdgeId))) (Ok successors)))) (Ok Map.empty)

    let private traceNestedContours edges successors tolerance =
        let byId = edges |> List.map (fun edge -> edge.Id, edge) |> Map.ofList
        let rec trace start current visited segments =
            if Set.contains current visited then
                if current = start then Ok(List.rev segments, visited)
                else Error(InternalContourTraceFailed current)
            else
                match Map.tryFind current byId, Map.tryFind current successors with
                | Some edge, Some next -> trace start next (Set.add current visited) (edge.Segment :: segments)
                | _ -> Error(InternalContourTraceFailed current)
        let rec gather remaining visited contours =
            match remaining |> List.tryFind (fun edge -> not (Set.contains edge.Id visited)) with
            | None -> Ok(List.rev contours)
            | Some edge ->
                trace edge.Id edge.Id visited []
                |> Result.bind (fun (segments, visited) ->
                    Subpath.createWith (WiggleWith tolerance) segments
                    |> Result.bind (Subpath.setClosedWith (WiggleWith tolerance) true)
                    |> Result.mapError InternalArrangementSegmentError
                    |> Result.bind (fun contour ->
                        let contour = if edge.Layer > 0 then Subpath.reverse contour else contour
                        gather remaining visited (contour :: contours)))
        gather edges Set.empty []

    /// Reconstruct every nonzero winding layer represented by an arrangement graph.
    /// A region of winding magnitude n contributes n nested closed contours.
    let internal nestedContoursFromGraph
        (graph: ArrangementGraph)
        (path: Path)
        (tolerance: float<length>) =
        if tolerance <= 0.0<length> || not (System.Double.IsFinite(float tolerance)) then
            Error(InternalInvalidArrangementTolerance tolerance)
        else
            nestedContourEdges graph path tolerance
            |> Result.bind (fun edges ->
                nestedContourSuccessors edges
                |> Result.bind (fun successors -> traceNestedContours edges successors tolerance))
