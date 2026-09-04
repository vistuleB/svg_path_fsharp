namespace SvgPath

/// Errors returned by offset and stroke construction.
type Error =
    | PathError of SegmentError
    | ArrangementGraphError of ArrangementError
    | ForcedParityPruningError of ForcedParityError
    | SourceNormalizationError of DegeneracyError
    | InvalidTolerance of tolerance: float<length>
    | InvalidSamples of samples: int
    | InvalidMaxDepth of maxDepth: int
    | InvalidMiterLimit of miterLimit: float
    | InvalidStalledOffsetDiameter of diameter: float<length>
    | InvalidTangentHealAngleDegrees of angle: float<degree>
    | InvalidStrokeWidth of width: float<length>
    | BandSubpathNotClosed
    | DegenerateTangent of t: float<parameter>
    | MaxDepthReached of error: float<length>
    | NonFinite
    | InternalSegmentImageCountMismatch
    | InternalEmptySegmentImage of segmentIndex: int
    | InternalMissingEdgeImage of edgeId: int
    | InternalMissingIndexedSegment of segmentIndex: int
    | InternalMissingWindingOpinion of segmentIndex: int
    | InternalSurvivorCapacityMismatch of edgeId: int * remaining: int
    | InternalForcedParityOpenChain of startVertex: int * endVertex: int
    | InternalIToKSubpathCount of actual: int
    | InternalIToKExpectedClosedSubpath
    | InternalIToKEndpointMismatch of expectedStart: int * actualStart: int * expectedEnd: int * actualEnd: int
    | InternalIToKMissingJPreimage of edgeId: int
    | InconsistentContainment

/// Join geometry inserted between adjacent offset segments.
type Join =
    | Bevel
    | Miter of miterLimit: float
    | Round

/// End-cap geometry used when stroking an open subpath.
type Cap =
    | Butt
    | Square
    | RoundCap

type internal OneSubpathBand =
    | OpenSubpathBand of outline: Subpath
    | ClosedSubpathBand of exterior: Subpath * interior: Subpath

/// Final trimming applied after optional offside trimming of a single offset.
type SingleOffsetFinalTrimming =
    | CuspTrimming
    | InBandTrimming
    | NoTrimming

[<Struct>]
/// Trimming controls for single offsets.
/// Offside trimming applies only to closed source subpaths. FinalTrimming
/// selects cusp-only trimming, complete in-band trimming, or no final pass.
type SingleOffsetTrimming =
    { Offside: bool
      FinalTrimming: SingleOffsetFinalTrimming }

[<Struct>]
/// Trimming controls for a two-sided offset band.
/// InnerCusps and OuterCusps independently trim reversed submerged runs before
/// the sides are assembled. InBand applies the final band-wide trimming pass.
type BandTrimming =
    { InnerCusps: bool
      OuterCusps: bool
      InBand: bool }

[<Struct>]
/// Accuracy and recursion controls for fitting offset curves.
type FittingOptions =
    { Tolerance: float<length>
      Samples: int
      MaxDepth: int }

[<Struct>]
/// Options shared by offset, band, and stroke construction.
type Options =
    { Fitting: FittingOptions
      DistanceOptions: DistanceOptions
      StalledOffsetDiameter: float<length>
      TangentHealAngleDegrees: float<degree>
      Join: Join
      SingleOffsetTrimming: SingleOffsetTrimming
      BandTrimming: BandTrimming }

[<Struct>]
type internal LengthSpan =
    { Segment: Segment
      StartDistance: float<length>
      Length: float<length> }

type internal SegmentEndpoint =
    | SegmentStart
    | SegmentEnd

type internal CubicEndpointFitPolicy =
    | FitPositionOnly
    | FitPositionAndDirection of direction: Point<1>
    | FitPositionAndDirectionWithCollapsedHandle of direction: Point<1>

type internal TangentTurn =
    | Clockwise
    | CounterClockwise
    | Straight
    | CouldNotMeasure

[<Struct>]
type internal ReversalTangentAdjustment =
    { IncomingDegrees: float<degree>
      OutgoingDegrees: float<degree> }

type internal BoundaryKind =
    | Ordinary
    | ReversalBoundary of leftNormalCurvature: float<1 / length> option
    | Inflection
    | NonReversalBoundaryTouch

[<Struct>]
type internal APreparedSegment =
    { SourceSubpathIndex: int
      SourceSegmentIndex: int
      Segment: Segment }

[<Struct>]
type internal CStalledSegment =
    { Prepared: APreparedSegment
      PreparedFrom: float<parameter>
      PreparedTo: float<parameter>
      Segment: Segment }

[<Struct>]
type internal DRefinedSegment =
    { Prepared: APreparedSegment
      PreparedFrom: float<parameter>
      PreparedTo: float<parameter>
      Segment: Segment
      StartBoundary: BoundaryKind
      EndBoundary: BoundaryKind }

[<Struct>]
type internal EJoinFreeSegment =
    { PortionIndex: int
      SegmentIndex: int
      Generation: int
      Refined: DRefinedSegment
      RefinedFrom: float<parameter>
      RefinedTo: float<parameter>
      Segment: Segment
      StartBoundary: BoundaryKind
      EndBoundary: BoundaryKind }

type internal OffsetSegmentSource =
    | OffsetFromJoinFree of EJoinFreeSegment
    | OffsetFromStalledRun of CStalledSegment list

[<Struct>]
type internal FUnhealedOffsetSegment =
    { Segment: Segment
      Source: OffsetSegmentSource
      NudgedStartTangentDirection: Point<1>
      NudgedEndTangentDirection: Point<1> }

[<Struct>]
type internal GHealedOffsetSegment =
    { Segment: Segment
      Source: OffsetSegmentSource
      NudgedStartTangentDirection: Point<1>
      NudgedEndTangentDirection: Point<1> }

type internal OffsetSourceTracePiece =
    | OffsetSourceTraceDRefined of
        sourceSegmentIndex: int *
        refinedPieceIndex: int *
        sourceFrom: float<parameter> *
        sourceTo: float<parameter> *
        segment: Segment *
        startBoundary: BoundaryKind *
        endBoundary: BoundaryKind *
        startIsReversal: bool *
        endIsReversal: bool
    | OffsetSourceTraceStalled of sourceSegmentIndex: int * segment: Segment

[<Struct>]
type internal OffsetSourceTracePortion =
    { Index: int
      Subpath: Subpath
      Pieces: OffsetSourceTracePiece list }

[<Struct>]
type internal SynchronizedOffsetTraceLeaf =
    { SourceSegmentIndex: int
      PreparedFrom: float<parameter>
      PreparedTo: float<parameter>
      Generation: int }

[<Struct>]
type internal SynchronizedOffsetTraceCorrespondence =
    { PortionIndex: int
      CorrespondenceIndex: int
      InnerStalled: bool
      OuterStalled: bool
      InnerLeaves: SynchronizedOffsetTraceLeaf list
      OuterLeaves: SynchronizedOffsetTraceLeaf list }

[<Struct>]
type internal SynchronizedOffsetTraceJoin =
    { AfterPortionIndex: int
      InnerSegments: Segment list
      OuterSegments: Segment list
      InnerReversed: bool
      OuterReversed: bool }

[<Struct>]
type internal SynchronizedOffsetTraceArea =
    { PortionIndex: int
      CorrespondenceIndex: int
      InnerSegments: Segment list
      OuterSegments: Segment list }

[<Struct>]
type internal SingleOffsetContaminationTraceEdge =
    { Id: int
      Segment: Segment
      StartVertex: int
      EndVertex: int
      PreimageFrom: float<parameter>
      PreimageTo: float<parameter>
      Offside: bool
      Survives: bool }

[<Struct>]
type internal BandArrangementTraceEdge =
    { Id: int
      Segment: Segment
      Submerged: bool }

[<Struct>]
type internal CuspTrimmingArrangementTraceEdge =
    { SideIndex: int
      Id: int
      Segment: Segment
      OffsetImage: bool
      Submerged: bool }

type internal BandSide =
    | Inner
    | Outer

type internal HPreimageSource =
    | HealedPreimage of GHealedOffsetSegment
    | JoinPreimage of afterPortionIndex: int * side: BandSide * joinSegmentIndex: int * reversed: bool

[<Struct>]
type internal HPreimageSegment =
    { Segment: Segment
      Source: HPreimageSource }

[<Struct>]
type internal HPreimageSubpath =
    { Segments: HPreimageSegment list
      Closed: bool
      Side: BandSide }

[<Struct>]
type internal ICulledOffsetSegment =
    { Segment: Segment
      Preimage: HPreimageSegment
      PreimageFrom: float<parameter>
      PreimageTo: float<parameter> }

[<Struct>]
type internal ICulledOffsetSubpath =
    { Segments: ICulledOffsetSegment list
      Closed: bool
      Side: BandSide }

[<Struct>]
type internal TracedOffsetSegment =
    { Segment: Segment
      Preimage: HPreimageSegment
      PreimageFrom: float<parameter>
      PreimageTo: float<parameter>
      Reversed: bool }

[<Struct>]
type internal TracedOffsetSubpath =
    { Segments: TracedOffsetSegment list
      Closed: bool
      Side: BandSide
      SourceSubpathIndex: int }

[<Struct>]
type internal ArrangementSplitTracedSegment =
    { Segment: Segment
      Preimage: ICulledOffsetSegment
      PreimageFrom: float<parameter>
      PreimageTo: float<parameter>
      EdgeId: int
      StartVertex: int
      EndVertex: int
      Reversed: bool
      DeletionCandidate: bool }

[<Struct>]
type internal ArrangementSplitTracedSubpath =
    { Segments: ArrangementSplitTracedSegment list
      Closed: bool
      Side: BandSide }

[<Struct>]
type internal ArrangementSplitRun =
    { Segments: ArrangementSplitTracedSegment list
      Submerged: bool }

[<Struct>]
type internal OffsideClosedWalkState =
    { FirstStartVertex: int
      EndVertex: int
      LastIndex: int
      RetainedSpan: float<parameter>
      SkippedRuns: int
      IndicesReversed: int list
      SegmentsReversed: ArrangementSplitTracedSegment list }

[<Struct>]
type internal CuspTrimmedSegment =
    { Segment: Segment
      ArrangementPreimage: ArrangementSplitTracedSegment }

[<Struct>]
type internal CuspTrimmedSubpath =
    { Segments: CuspTrimmedSegment list
      Closed: bool }

[<Struct>]
type internal OffsetArrangementBuild =
    { Graph: ArrangementGraph
      IndexedSegments: IndexedOffsetSegment list
      SegmentImages: ArrangementSourceSegmentImage list
      EdgeImages: ArrangementEdgeImage list }

and internal OffsetArrangementSegmentGroup =
    | UntrimmedOffsetSegment
    | ZeroOffsetSourceSegment

and internal IndexedOffsetSegment =
    { Group: OffsetArrangementSegmentGroup
      SubpathIndex: int
      Segment: Segment
      WindingOpinion: WindingSideOpinion option }

and internal WindingSideOpinion =
    { Left: int
      Right: int }

[<Struct>]
type internal OffsetTrimGraph =
    { Vertices: ArrangementVertex list
      Edges: ArrangementEdge list
      EdgeCapacities: (int * int) list option }

[<Struct>]
type internal OffsetDistances =
    { Inner: float<length>
      Outer: float<length> }

[<Struct>]
type internal BoundaryPair =
    { Inner: BoundaryKind
      Outer: BoundaryKind }

type internal SideStalledStatus =
    | SideStalled
    | SideNotStalled

[<Struct>]
type internal SynchronizedClassifiedSegment =
    { Prepared: APreparedSegment
      InnerStatus: SideStalledStatus
      OuterStatus: SideStalledStatus
      StartBoundary: BoundaryPair
      EndBoundary: BoundaryPair }

[<Struct>]
type internal SynchronizedSourceSegment =
    { Prepared: APreparedSegment
      PreparedFrom: float<parameter>
      PreparedTo: float<parameter>
      Segment: Segment
      InnerStatus: SideStalledStatus
      OuterStatus: SideStalledStatus
      StartBoundary: BoundaryPair
      EndBoundary: BoundaryPair }

type internal SynchronizedSideSource =
    | RefinableSideSource of EJoinFreeSegment
    | StalledSideSource of CStalledSegment list
    | SplitSideSource of left: SynchronizedSideSource * right: SynchronizedSideSource

[<Struct>]
type internal OffsetCorrespondence =
    { PortionIndex: int
      CorrespondenceIndex: int
      Sources: SynchronizedSourceSegment list
      Inner: SynchronizedSideSource
      Outer: SynchronizedSideSource
      InnerOffsetCount: int
      OuterOffsetCount: int }

[<Struct>]
type internal OffsetJoinCorrespondence =
    { AfterPortionIndex: int
      Inner: Segment list
      Outer: Segment list
      InnerReversed: bool
      OuterReversed: bool
      InnerStart: Point<length>
      InnerEnd: Point<length>
      OuterStart: Point<length>
      OuterEnd: Point<length> }

[<Struct>]
type internal SynchronizedHealedPortion =
    { PortionIndex: int
      Inner: GHealedOffsetSegment list
      Outer: GHealedOffsetSegment list }

[<Struct>]
type internal SynchronizedOffsetSegmentsBuild =
    { InnerOffsets: GHealedOffsetSegment list
      OuterOffsets: GHealedOffsetSegment list
      Correspondences: OffsetCorrespondence list
      Portions: SynchronizedHealedPortion list }

[<Struct>]
type internal SynchronizedUntrimmedBuild =
    { Inner: Subpath
      Outer: Subpath
      InnerCulled: ICulledOffsetSubpath
      OuterCulled: ICulledOffsetSubpath
      Correspondences: OffsetCorrespondence list
      Portions: SynchronizedHealedPortion list
      JoinCorrespondences: OffsetJoinCorrespondence list }

[<Struct>]
type internal SingleOffsetUntrimmedBuild =
    { Subpath: Subpath
      ZeroSource: Subpath
      Culled: ICulledOffsetSubpath
      Correspondences: OffsetCorrespondence list
      Portions: SynchronizedHealedPortion list
      JoinCorrespondences: OffsetJoinCorrespondence list }

type internal OffsetAttempt =
    | OffsetAccepted of FUnhealedOffsetSegment
    | OffsetNeedsRefinement of divergence: float<length>

[<Struct>]
type internal SynchronizedUnhealedResult =
    { InnerOffsets: FUnhealedOffsetSegment list
      OuterOffsets: FUnhealedOffsetSegment list
      InnerSource: SynchronizedSideSource
      OuterSource: SynchronizedSideSource }

[<Struct>]
type internal SynchronizedPortionUnhealedBuild =
    { InnerOffsets: FUnhealedOffsetSegment list
      OuterOffsets: FUnhealedOffsetSegment list
      Correspondences: OffsetCorrespondence list }

[<Struct>]
type internal SurvivorEdge =
    { EdgeId: int
      Reversed: bool
      StartVertex: int
      EndVertex: int
      Segment: Segment
      ArrangementPreimage: ArrangementSplitTracedSegment option }

[<Struct>]
type internal SurvivorChain =
    { StartVertex: int
      EndVertex: int
      Edges: SurvivorEdge list
      Closed: bool }

[<Struct>]
type internal AvailableEdgeCapacity =
    { EdgeId: int
      Remaining: int }

[<Struct>]
type internal JoinFreePortion =
    { Index: int
      Subpath: Subpath
      Closed: bool }

type internal CurvatureSplitKind =
    | OrdinarySplit
    | CuspSplit
    | InflectionSplit

[<Struct>]
type internal CurvatureSplitParameter =
    { T: float<parameter>
      Kind: CurvatureSplitKind }

[<Struct>]
type internal CurvatureBoundary =
    { T: float<parameter>
      Boundary: BoundaryKind }

type internal OffsetCurvatureZone =
    | OutsideOffsetRadius
    | InsideOffsetRadius
    | Opposite
    | UnknownCurvatureZone

[<RequireQualifiedAccess>]
/// Construction of signed left-normal offsets, two-sided bands, strokes, and
/// local offset coordinate maps. Positive offsets lie on the visual left of
/// the source traversal; negative offsets lie on its visual right.
module Offset =
    let private defaultTolerance = 0.01<length>
    let private maximumRefinementGeneration = 5
    let private defaultMaxDepth = maximumRefinementGeneration
    let private defaultSamples = 10
    let private defaultTrimmingSamples = 5
    let private defaultMiterLimit = 4.0
    let inline private smallUnitDivisionTolerance<[<Measure>] 'Unit> () : float<'Unit> =
        LanguagePrimitives.FloatWithMeasure<'Unit> 1.0e-6
    let private pointTolerance = 1.0e-9<length>
    let private pointParameterTolerance = 1.0e-9<parameter>
    let private directionDeterminantTolerance = 1.0e-9
    let private angleToleranceDegrees = 1.0e-9<degree>
    let private arrangementTolerance = 2.0e-9<length>
    let private submergedSideSamplingDistance = 5.0e-8<length>
    let private bandOrientationSideSamplingDistance = 1.0e-4<length>
    let private curvatureParameterTolerance = 1.0e-6<parameter>
    let private curvatureValueTolerance = 1.0e-6<1 / length>
    let private curvatureRadiusTolerance = 1.0e-6<length>
    let private defaultTangentHealAngleDegrees = 2.0<degree>
    let private joinFreeTangentAlignmentAngleDegrees = 0.001<degree>
    let private tangentHealAgreementAngleDegrees = 2.0<degree>
    let private sourceTangentColinearizationAngleDegrees = 2.0<degree>
    let private reversalTangentGapDegrees = 1.0<degree>
    let private reversalFitTangentNudgeDegrees = 0.5<degree>
    let private reversalFitLineApertureDegrees = 0.02<degree>
    let private reversalFitMinHandleChordRatio = 0.1
    let private reversalFitMaxHandleChordRatio = 0.9
    let private tangentTurnCurvatureEpsilon = 1.0e-9<1 / length>
    let private tangentTurnAngleEpsilon = 0.001<degree>
    let private stableTangentAssertionDiameter = 0.01<length>
    let private defaultStalledOffsetDiameter = 0.01<length>
    let private adjacentLoopEndpointParameterTolerance = 1.0e-4<parameter>

    let defaultFittingOptions =
        { Tolerance = defaultTolerance
          Samples = defaultSamples
          MaxDepth = defaultMaxDepth }

    let defaultOptions =
        { Fitting = defaultFittingOptions
          DistanceOptions =
            { Segment.defaultDistanceOptions with Samples = defaultTrimmingSamples }
          StalledOffsetDiameter = defaultStalledOffsetDiameter
          TangentHealAngleDegrees = defaultTangentHealAngleDegrees
          Join = Miter defaultMiterLimit
          SingleOffsetTrimming =
            { Offside = true
              FinalTrimming = InBandTrimming }
          BandTrimming =
            { InnerCusps = true
              OuterCusps = true
              InBand = true } }

    let private refinementDepth options = min options.Fitting.MaxDepth maximumRefinementGeneration

    let private validateJoin join =
        match join with
        | Miter miterLimit when miterLimit <= 0.0 || not (System.Double.IsFinite miterLimit) ->
            Error(InvalidMiterLimit miterLimit)
        | _ -> Ok()

    let private validateOptions options =
        if options.Fitting.Tolerance <= 0.0<length>
           || not (System.Double.IsFinite(float options.Fitting.Tolerance)) then
            Error(InvalidTolerance options.Fitting.Tolerance)
        elif options.Fitting.Samples <= 0 then
            Error(InvalidSamples options.Fitting.Samples)
        elif options.Fitting.MaxDepth <= 0 then
            Error(InvalidMaxDepth options.Fitting.MaxDepth)
        elif options.StalledOffsetDiameter < 0.0<length>
             || not (System.Double.IsFinite(float options.StalledOffsetDiameter)) then
            Error(InvalidStalledOffsetDiameter options.StalledOffsetDiameter)
        elif options.TangentHealAngleDegrees < 0.0<degree>
             || not (System.Double.IsFinite(float options.TangentHealAngleDegrees)) then
            Error(InvalidTangentHealAngleDegrees options.TangentHealAngleDegrees)
        else
            validateJoin options.Join

    let private validateStrokeWidth width =
        if width <= 0.0<length> || not (System.Double.IsFinite(float width)) then
            Error(InvalidStrokeWidth width)
        else
            Ok()

    let private requiredDirection t direction =
        match direction with
        | Some value -> Ok value
        | None -> Error(DegenerateTangent t)

    let private directionAgreementAngle (incoming: Point<1>) (outgoing: Point<1>) =
        let clockwise = Point.clockwiseAperture incoming outgoing
        min clockwise (360.0<degree> - clockwise)

    let private interiorUnitTangent t directions =
        match directions.Incoming, directions.Outgoing with
        | Some incoming, Some outgoing ->
            if directionAgreementAngle incoming outgoing > tangentHealAgreementAngleDegrees then
                Error(DegenerateTangent t)
            else
                let sum = Point.add incoming outgoing
                if Point.norm sum > smallUnitDivisionTolerance () then
                    match Point.normalize sum with
                    | Some direction -> Ok direction
                    | None -> Error(DegenerateTangent t)
                else
                    Error(DegenerateTangent t)
        | _ -> Error(DegenerateTangent t)

    let private unitTangent segment t =
        Segment.directions segment t
        |> Result.mapError PathError
        |> Result.bind (fun directions ->
            if t = 0.0<parameter> then requiredDirection t directions.Outgoing
            elif t = 1.0<parameter> then requiredDirection t directions.Incoming
            else interiorUnitTangent t directions)

    let private unitNormal segment t =
        unitTangent segment t |> Result.map Point.rotateCounterclockwise

    let private hPreimageIsReversed preimage =
        match preimage.Source with
        | HealedPreimage healed ->
            match healed.Source with
            | OffsetFromJoinFree source ->
                match unitTangent preimage.Segment 0.5<parameter>, unitTangent source.Segment 0.5<parameter> with
                | Ok offsetTangent, Ok sourceTangent -> Point.dot offsetTangent sourceTangent < 0.0
                | _ -> false
            | OffsetFromStalledRun _ -> false
        | JoinPreimage(_, _, _, reversed) -> reversed

    let private tracedSubpathFromI (subpath: ICulledOffsetSubpath) sourceSubpathIndex : TracedOffsetSubpath =
        { Segments =
            subpath.Segments
            |> List.map (fun segment ->
                { Segment = segment.Segment
                  Preimage = segment.Preimage
                  PreimageFrom = segment.PreimageFrom
                  PreimageTo = segment.PreimageTo
                  Reversed = hPreimageIsReversed segment.Preimage })
          Closed = subpath.Closed
          Side = subpath.Side
          SourceSubpathIndex = sourceSubpathIndex }

    let private tracedSubpathFromSurvivorChain chain side sourceSubpathIndex =
        let rec collect results =
            match results with
            | [] -> Ok []
            | head :: tail ->
                match head, collect tail with
                | Ok value, Ok values -> Ok(value :: values)
                | Error error, _ -> Error error
                | _, Error error -> Error error

        chain.Edges
        |> List.map (fun edge ->
            match edge.ArrangementPreimage with
            | None -> Error(InternalMissingIndexedSegment edge.EdgeId)
            | Some split ->
                Ok
                    ({ Segment = edge.Segment
                       Preimage = split.Preimage.Preimage
                       PreimageFrom = split.PreimageFrom
                       PreimageTo = split.PreimageTo
                       Reversed = split.Reversed }: TracedOffsetSegment))
        |> collect
        |> Result.map (fun segments ->
            { Segments = segments
              Closed = chain.Closed
              Side = side
              SourceSubpathIndex = sourceSubpathIndex })

    let private iSubpathFromTraced (traced: TracedOffsetSubpath) : ICulledOffsetSubpath =
        { Segments =
            traced.Segments
            |> List.map (fun segment ->
                { Segment = segment.Segment
                  Preimage = segment.Preimage
                  PreimageFrom = segment.PreimageFrom
                  PreimageTo = segment.PreimageTo })
          Closed = traced.Closed
          Side = traced.Side }

    let private tracedSubpathFromCuspTrimmed
        (subpath: CuspTrimmedSubpath)
        sourceSubpathIndex
        side
        : TracedOffsetSubpath =
        { Segments =
            subpath.Segments
            |> List.map (fun segment ->
                let split = segment.ArrangementPreimage
                ({ Segment = segment.Segment
                   Preimage = split.Preimage.Preimage
                   PreimageFrom = split.PreimageFrom
                   PreimageTo = split.PreimageTo
                   Reversed = split.Reversed }: TracedOffsetSegment))
          Closed = subpath.Closed
          Side = side
          SourceSubpathIndex = sourceSubpathIndex }

    let private sourceTangentIsMovable segment =
        match segment with
        | QuadraticBezier _
        | CubicBezier _ -> true
        | Line _
        | Arc _ -> false

    let private averagedBoundaryTangent leftTangent rightTangent =
        let sum = Point.add leftTangent rightTangent
        if Point.norm sum > smallUnitDivisionTolerance () then
            Point.normalize sum |> Option.defaultValue leftTangent
        else
            leftTangent

    let private snapSourceEndTangent segment direction =
        match segment with
        | QuadraticBezier(startPoint, control, endPoint) ->
            let control1 =
                Point.displacement startPoint control
                |> Point.scale (2.0 / 3.0)
                |> fun displacement -> Point.translate displacement startPoint
            let control2 =
                Point.displacement endPoint control
                |> Point.scale (2.0 / 3.0)
                |> fun displacement -> Point.translate displacement endPoint
            let handle = Point.distance control2 endPoint
            CubicBezier(startPoint, control1, Point.translate (Point.scale -handle direction) endPoint, endPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let handle = Point.distance control2 endPoint
            CubicBezier(startPoint, control1, Point.translate (Point.scale -handle direction) endPoint, endPoint)
        | _ -> segment

    let private snapSourceStartTangent segment direction =
        match segment with
        | QuadraticBezier(startPoint, control, endPoint) ->
            let control1 =
                Point.displacement startPoint control
                |> Point.scale (2.0 / 3.0)
                |> fun displacement -> Point.translate displacement startPoint
            let control2 =
                Point.displacement endPoint control
                |> Point.scale (2.0 / 3.0)
                |> fun displacement -> Point.translate displacement endPoint
            let handle = Point.distance control1 startPoint
            CubicBezier(startPoint, Point.translate (Point.scale handle direction) startPoint, control2, endPoint)
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            let handle = Point.distance control1 startPoint
            CubicBezier(startPoint, Point.translate (Point.scale handle direction) startPoint, control2, endPoint)
        | _ -> segment

    let private colinearizeSourceTangentBoundaryWithinTolerance left right leftTangent rightTangent =
        match sourceTangentIsMovable left, sourceTangentIsMovable right with
        | true, true ->
            let direction = averagedBoundaryTangent leftTangent rightTangent
            snapSourceEndTangent left direction, snapSourceStartTangent right direction
        | true, false -> snapSourceEndTangent left rightTangent, right
        | false, true -> left, snapSourceStartTangent right leftTangent
        | false, false -> left, right

    let private colinearizeSourceTangentBoundary left right tolerance =
        match unitTangent left 1.0<parameter>, unitTangent right 0.0<parameter> with
        | Ok leftTangent, Ok rightTangent
            when directionAgreementAngle leftTangent rightTangent <= tolerance ->
                colinearizeSourceTangentBoundaryWithinTolerance left right leftTangent rightTangent
        | _ -> left, right

    let private colinearizeSourceTangentPolicy tolerance =
        Custom(fun previous next closing ->
            let previous, next = colinearizeSourceTangentBoundary previous next tolerance
            if closing then [ previous ] else [ previous; next ])

    let private colinearizeOffsetSourceTangents subpath tolerance =
        Subpath.rebuildWith (colinearizeSourceTangentPolicy tolerance) subpath
        |> Result.mapError PathError

    let private segmentDiameter segment =
        Segment.boundingBox segment
        |> Result.mapError PathError
        |> Result.map BoundingBox.diameter

    let private segmentIsShort segment tolerance =
        match segmentDiameter segment with
        | Ok diameter -> diameter < tolerance
        | Error _ -> false

    let private remapSegmentEndpoints segment targetStart targetEnd =
        Segment.remapEndpoints segment targetStart targetEnd
        |> Result.defaultValue (Line(targetStart, targetEnd))

    let private stretchSegmentStart segment targetStart =
        remapSegmentEndpoints segment targetStart (Segment.finish segment)

    let private stretchSegmentEnd segment targetEnd =
        remapSegmentEndpoints segment (Segment.start segment) targetEnd

    let private carryDeletedSmallSegment previous deleted =
        let displacement = Point.displacement (Segment.start deleted) (Segment.finish deleted)
        let target = Point.translate (Point.scale (1.0 / 3.0) displacement) (Segment.finish previous)
        stretchSegmentEnd previous target

    let private bridgeDeletedSmallSegmentGap previous next =
        let target = Point.interpolate (Segment.finish previous) (Segment.start next) 0.25<parameter>
        stretchSegmentEnd previous target, stretchSegmentStart next target

    let private eliminateSmallSegments segments tolerance =
        let rec loop previous rest normalized deletedSinceBridge =
            match rest with
            | [] -> List.rev (previous :: normalized)
            | next :: remaining when not (segmentIsShort next tolerance) ->
                let previous, next =
                    if deletedSinceBridge then bridgeDeletedSmallSegmentGap previous next
                    else previous, next
                loop next remaining (previous :: normalized) false
            | next :: [] -> loop next [] (previous :: normalized) false
            | next :: remaining ->
                loop (carryDeletedSmallSegment previous next) remaining normalized true

        match segments with
        | [] -> []
        | first :: rest -> loop first rest [] false

    let private eliminateSmallOffsetSourceSegments (subpath: Subpath) tolerance =
        match eliminateSmallSegments subpath.Segments tolerance with
        | [] -> Ok subpath
        | normalized ->
            Subpath.createWith (WiggleThenBridgeWith tolerance) normalized
            |> Result.bind (fun normalizedSubpath ->
                Subpath.setClosedWith (WiggleThenBridgeWith tolerance) subpath.Closed normalizedSubpath)
            |> Result.mapError PathError

    let private normalizeSourceSubpath subpath options =
        eliminateSmallOffsetSourceSegments subpath 0.001<length>
        |> Result.bind (fun subpath ->
            Degeneracy.normalizeDegenerateSegments subpath options.Fitting.Tolerance
            |> Result.mapError SourceNormalizationError)
        |> Result.bind (fun subpath ->
            if List.isEmpty subpath.Segments then Ok subpath
            else colinearizeOffsetSourceTangents subpath sourceTangentColinearizationAngleDegrees)

    let private normalizeSourcePath (path: Path) options =
        let rec normalize subpaths =
            match subpaths with
            | [] -> Ok []
            | first :: rest ->
                match normalizeSourceSubpath first options, normalize rest with
                | Ok normalized, Ok normalizedRest -> Ok(normalized :: normalizedRest)
                | Error error, _ -> Error error
                | _, Error error -> Error error

        normalize path.Subpaths
        |> Result.map (List.filter (fun subpath -> not (List.isEmpty subpath.Segments)) >> Path.ofSubpaths)

    let private preparedSegments (subpath: Subpath) sourceSubpathIndex =
        subpath.Segments
        |> List.mapi (fun sourceSegmentIndex segment ->
            { SourceSubpathIndex = sourceSubpathIndex
              SourceSegmentIndex = sourceSegmentIndex
              Segment = segment })

    let private preparedSegmentBetween
        (prepared: APreparedSegment)
        fromParameter
        toParameter =
        Segment.betweenInside prepared.Segment fromParameter toParameter
        |> Result.mapError PathError

    let private segmentIsBezier segment =
        match segment with
        | QuadraticBezier _
        | CubicBezier _ -> true
        | Line _
        | Arc _ -> false

    let private mergeCurvatureSplitKind left right =
        match left, right with
        | InflectionSplit, _
        | _, InflectionSplit -> InflectionSplit
        | CuspSplit, _
        | _, CuspSplit -> CuspSplit
        | _ -> OrdinarySplit

    let private uniqueCurvatureSplitParameters
        tolerance
        (values: CurvatureSplitParameter list)
        : CurvatureSplitParameter list =
        let rec loop (unique: CurvatureSplitParameter list) (remaining: CurvatureSplitParameter list) =
            match unique, remaining with
            | _, [] -> List.rev unique
            | previous :: previousRest, first :: rest
                when abs (first.T - previous.T) <= tolerance ->
                    loop ({ T = previous.T; Kind = mergeCurvatureSplitKind previous.Kind first.Kind } :: previousRest) rest
            | _, first :: rest -> loop (first :: unique) rest
        loop [] values

    let private offsetCurvatureRadiusZone radius offset =
        if offset >= 0.0<length> then
            if radius < -curvatureRadiusTolerance then Opposite
            elif radius < offset - curvatureRadiusTolerance then InsideOffsetRadius
            else OutsideOffsetRadius
        else
            if radius > curvatureRadiusTolerance then Opposite
            elif radius > offset + curvatureRadiusTolerance then InsideOffsetRadius
            else OutsideOffsetRadius

    let private offsetCurvatureZone segment offset t =
        match Curvature.segmentLeftNormalRadius segment t with
        | Ok radius -> offsetCurvatureRadiusZone radius offset
        | Error _ ->
            match Curvature.segmentLeftNormalCurvature segment t with
            | Ok value when value > curvatureValueTolerance -> Opposite
            | Ok value when value < -curvatureValueTolerance -> Opposite
            | Ok _ -> OutsideOffsetRadius
            | Error _ -> UnknownCurvatureZone

    let private offsetCurvatureZonesFormReversalBoundary previous next =
        match previous, next with
        | Some InsideOffsetRadius, Some OutsideOffsetRadius
        | Some InsideOffsetRadius, Some Opposite
        | Some OutsideOffsetRadius, Some InsideOffsetRadius
        | Some Opposite, Some InsideOffsetRadius -> true
        | _ -> false

    let private boundaryIsReversal boundary =
        match boundary with
        | ReversalBoundary _ -> true
        | _ -> false

    let private sourceEndpointCurvature segment t =
        Curvature.segmentLeftNormalCurvature segment t |> Result.toOption

    let private reversalBoundary segment t =
        ReversalBoundary(sourceEndpointCurvature segment t)

    let private curvatureBoundaryBehavior kind previousZone nextZone reversalCurvature =
        match kind with
        | InflectionSplit -> Inflection
        | CuspSplit ->
            if offsetCurvatureZonesFormReversalBoundary previousZone nextZone then
                ReversalBoundary reversalCurvature
            else
                NonReversalBoundaryTouch
        | OrdinarySplit ->
            if offsetCurvatureZonesFormReversalBoundary previousZone nextZone then
                ReversalBoundary reversalCurvature
            else
                Ordinary

    let private curvatureIntervalZones (parameters: CurvatureSplitParameter list) segment offset =
        parameters
        |> List.pairwise
        |> List.map (fun (fromParameter, toParameter) ->
            let midpoint = fromParameter.T + (toParameter.T - fromParameter.T) / 2.0
            offsetCurvatureZone segment offset midpoint)

    let private classifyCurvatureBoundaries
        (parameters: CurvatureSplitParameter list)
        segment
        offset
        : CurvatureBoundary list =
        let zones = curvatureIntervalZones parameters segment offset
        parameters
        |> List.mapi (fun index parameterValue ->
            let previousZone = if index = 0 then None else Some zones[index - 1]
            let nextZone = if index >= List.length zones then None else Some zones[index]
            { T = parameterValue.T
              Boundary =
                curvatureBoundaryBehavior
                    parameterValue.Kind
                    previousZone
                    nextZone
                    (sourceEndpointCurvature segment parameterValue.T) })

    let private sourceSegmentsHaveBoundaryReversal left right offset =
        offsetCurvatureZonesFormReversalBoundary
            (Some(offsetCurvatureZone left offset 1.0<parameter>))
            (Some(offsetCurvatureZone right offset 0.0<parameter>))

    let private applyEndpointBoundaryOverrides boundaries startBoundary endBoundary =
        let withStart =
            match startBoundary, boundaries with
            | Ordinary, _
            | _, [] -> boundaries
            | _, first :: rest -> { first with Boundary = startBoundary } :: rest
        match endBoundary, List.rev withStart with
        | Ordinary, _
        | _, [] -> withStart
        | _, last :: rest -> List.rev ({ last with Boundary = endBoundary } :: rest)

    let private synchronizedCurvatureSplitParameters segment inner outer innerStatus outerStatus =
        let reversalParameters status offset =
            match status with
            | SideStalled -> Ok []
            | SideNotStalled ->
                Curvature.segmentLeftNormalCuspParameters segment offset Curvature.defaultOptions
                |> Result.mapError (fun _ -> NonFinite)

        match reversalParameters innerStatus inner, reversalParameters outerStatus outer,
              Curvature.segmentInflectionParameters segment Curvature.defaultOptions with
        | Ok innerReversals, Ok outerReversals, Ok inflections ->
            let interior values =
                values
                |> List.filter (fun t -> t > pointParameterTolerance && t < 1.0<parameter> - pointParameterTolerance)
            let reversals =
                interior (innerReversals @ outerReversals)
                |> List.map (fun t -> { T = t; Kind = CuspSplit })
            let inflectionSplits =
                interior inflections
                |> List.map (fun t -> { T = t; Kind = InflectionSplit })
            ({ T = 0.0<parameter>; Kind = OrdinarySplit }
             :: { T = 1.0<parameter>; Kind = OrdinarySplit }
             :: (reversals @ inflectionSplits))
            |> List.sortBy (fun value -> value.T)
            |> uniqueCurvatureSplitParameters curvatureParameterTolerance
            |> Ok
        | _ -> Error NonFinite

    let private splitPreparedSegmentForBothOffsets
        (prepared: APreparedSegment)
        (innerBoundaries: CurvatureBoundary list)
        (outerBoundaries: CurvatureBoundary list)
        innerStatus
        outerStatus =
        let rec loop inner outer synchronized =
            match inner, outer with
            | innerFrom :: innerTo :: innerRest, outerFrom :: outerTo :: outerRest ->
                if innerFrom.T <> outerFrom.T || innerTo.T <> outerTo.T then
                    Error NonFinite
                else
                    match loop (innerTo :: innerRest) (outerTo :: outerRest) synchronized with
                    | Error error -> Error error
                    | Ok rest when innerTo.T - innerFrom.T <= pointParameterTolerance -> Ok rest
                    | Ok rest ->
                        preparedSegmentBetween prepared innerFrom.T innerTo.T
                        |> Result.map (fun segment ->
                            ({ Prepared = prepared
                               PreparedFrom = innerFrom.T
                               PreparedTo = innerTo.T
                               Segment = segment
                               InnerStatus = innerStatus
                               OuterStatus = outerStatus
                               StartBoundary = { Inner = innerFrom.Boundary; Outer = outerFrom.Boundary }
                               EndBoundary = { Inner = innerTo.Boundary; Outer = outerTo.Boundary } }: SynchronizedSourceSegment)
                            :: rest)
            | [ _ ], [ _ ]
            | [], [] -> Ok(List.rev synchronized)
            | _ -> Error NonFinite
        loop innerBoundaries outerBoundaries []

    let private markSynchronizedAdjacentReversal
        (left: SynchronizedClassifiedSegment)
        (right: SynchronizedClassifiedSegment)
        (distances: OffsetDistances) =
        let innerReversal =
            sourceSegmentsHaveBoundaryReversal left.Prepared.Segment right.Prepared.Segment distances.Inner
        let outerReversal =
            sourceSegmentsHaveBoundaryReversal left.Prepared.Segment right.Prepared.Segment distances.Outer
        let leftEnd =
            { Inner =
                if innerReversal then reversalBoundary left.Prepared.Segment 1.0<parameter>
                else left.EndBoundary.Inner
              Outer =
                if outerReversal then reversalBoundary left.Prepared.Segment 1.0<parameter>
                else left.EndBoundary.Outer }
        let rightStart =
            { Inner =
                if innerReversal then reversalBoundary right.Prepared.Segment 0.0<parameter>
                else right.StartBoundary.Inner
              Outer =
                if outerReversal then reversalBoundary right.Prepared.Segment 0.0<parameter>
                else right.StartBoundary.Outer }
        { left with EndBoundary = leftEnd }, { right with StartBoundary = rightStart }

    let private markSynchronizedCrossSegmentReversals
        (segments: SynchronizedClassifiedSegment list)
        (distances: OffsetDistances) =
        let rec loop previous rest marked =
            match rest with
            | [] -> List.rev (previous :: marked)
            | next :: remaining ->
                let previous, next = markSynchronizedAdjacentReversal previous next distances
                loop next remaining (previous :: marked)
        match segments with
        | []
        | [ _ ] -> segments
        | first :: rest -> loop first rest []

    let private collectSynchronizedStatusRun
        (first: SynchronizedSourceSegment)
        (rest: SynchronizedSourceSegment list) =
        let rec loop collected remaining =
            match remaining with
            | next :: tail
                when next.InnerStatus = first.InnerStatus
                     && next.OuterStatus = first.OuterStatus ->
                loop (next :: collected) tail
            | _ -> first :: List.rev collected, remaining
        match first.InnerStatus, first.OuterStatus with
        | SideNotStalled, SideNotStalled -> [ first ], rest
        | _ -> loop [] rest

    let private synchronizedRefinedSegment
        (source: SynchronizedSourceSegment)
        side
        : DRefinedSegment =
        let startBoundary, endBoundary =
            match side with
            | Inner -> source.StartBoundary.Inner, source.EndBoundary.Inner
            | Outer -> source.StartBoundary.Outer, source.EndBoundary.Outer
        { Prepared = source.Prepared
          PreparedFrom = source.PreparedFrom
          PreparedTo = source.PreparedTo
          Segment = source.Segment
          StartBoundary = startBoundary
          EndBoundary = endBoundary }

    let private synchronizedESegment
        (refined: DRefinedSegment)
        portionIndex
        segmentIndex
        : EJoinFreeSegment =
        { PortionIndex = portionIndex
          SegmentIndex = segmentIndex
          Generation = 0
          Refined = refined
          RefinedFrom = 0.0<parameter>
          RefinedTo = 1.0<parameter>
          Segment = refined.Segment
          StartBoundary = refined.StartBoundary
          EndBoundary = refined.EndBoundary }

    let private synchronizedStalledSegment (refined: DRefinedSegment) : CStalledSegment =
        { Prepared = refined.Prepared
          PreparedFrom = refined.PreparedFrom
          PreparedTo = refined.PreparedTo
          Segment = refined.Segment }

    let private synchronizedStalledGroup sources side =
        sources
        |> List.map (fun source ->
            synchronizedRefinedSegment source side |> synchronizedStalledSegment)

    let private largestAttemptDivergence inner outer =
        match inner, outer with
        | OffsetNeedsRefinement left, OffsetNeedsRefinement right -> max left right
        | OffsetNeedsRefinement value, _
        | _, OffsetNeedsRefinement value -> value
        | _ -> 0.0<length>

    let private joinSynchronizedUnhealedResults
        (left: SynchronizedUnhealedResult)
        (right: SynchronizedUnhealedResult)
        : SynchronizedUnhealedResult =
        { InnerOffsets = left.InnerOffsets @ right.InnerOffsets
          OuterOffsets = left.OuterOffsets @ right.OuterOffsets
          InnerSource = SplitSideSource(left.InnerSource, right.InnerSource)
          OuterSource = SplitSideSource(left.OuterSource, right.OuterSource) }

    let private splitEJoinFreeSegmentAtMidpoint
        (source: EJoinFreeSegment)
        : Result<EJoinFreeSegment * EJoinFreeSegment, Error> =
        Segment.split source.Segment 0.5<parameter>
        |> Result.mapError PathError
        |> Result.map (fun (left, right) ->
            let sourceMid = source.RefinedFrom + (source.RefinedTo - source.RefinedFrom) / 2.0
            { source with
                Generation = source.Generation + 1
                RefinedTo = sourceMid
                Segment = left
                EndBoundary = Ordinary },
            { source with
                Generation = source.Generation + 1
                RefinedFrom = sourceMid
                Segment = right
                StartBoundary = Ordinary })

    let private pointIsFinite (point: Point<'unit>) =
        System.Double.IsFinite(float point.X) && System.Double.IsFinite(float point.Y)

    let private offsetPoint segment t offset =
        match Segment.point segment t, unitNormal segment t with
        | Ok point, Ok normal ->
            let offsetPoint = Point.translate (Point.scale offset normal) point
            if pointIsFinite offsetPoint then Ok offsetPoint else Error NonFinite
        | Error error, _ -> Error(PathError error)
        | _, Error error -> Error error

    let private circularArcOffsetRadius segment offset =
        match segment with
        | Arc endpoint ->
            Ellipse.endpointToCenter endpoint
            |> Result.mapError (fun _ -> PathError DegenerateArc)
            |> Result.bind (fun center ->
                if abs (center.Radius.X - center.Radius.Y) > pointTolerance then
                    Error NonFinite
                else
                    let signedOffset = if center.DeltaAngle >= 0.0<degree> then offset else -offset
                    Ok(center.Radius.X + signedOffset))
        | _ -> Error NonFinite

    let private offsetCircularArcSegmentRaw segment offset radius =
        match segment with
        | Arc endpoint ->
            match offsetPoint segment 0.0<parameter> offset,
                  offsetPoint segment 1.0<parameter> offset,
                  Ellipse.endpointToCenter endpoint with
            | Ok startPoint, Ok endPoint, Ok center ->
                Ok(
                    Arc
                        { Start = startPoint
                          Radius = Point.create (abs radius) (abs radius)
                          XAxisRotation = center.XAxisRotation
                          LargeArc = abs center.DeltaAngle > 180.0<degree>
                          Sweep = center.DeltaAngle >= 0.0<degree>
                          End = endPoint }
                )
            | Error error, _, _
            | _, Error error, _ -> Error error
            | _, _, Error _ -> Error(PathError DegenerateArc)
        | _ -> Error NonFinite

    let private makeOffsetSegment segment source startTangent endTangent : FUnhealedOffsetSegment =
        { Segment = segment
          Source = source
          NudgedStartTangentDirection = startTangent
          NudgedEndTangentDirection = endTangent }

    let private buildExactArcOffsetSegment arc source =
        match unitTangent arc 0.0<parameter>, unitTangent arc 1.0<parameter> with
        | Ok startTangent, Ok endTangent ->
            Ok(makeOffsetSegment arc source startTangent endTangent)
        | Error error, _
        | _, Error error -> Error error

    let private rawFittingTolerance options = options.Fitting.Tolerance * 0.5

    let private offsetReversalParameters segment offset =
        Curvature.segmentLeftNormalCuspParameters segment offset Curvature.defaultOptions
        |> Result.mapError (fun _ -> NonFinite)

    let private offsetInflectionParameters segment =
        let options: CurvatureOptions =
            { Tolerance = curvatureParameterTolerance
              Samples = 100
              MaxDepth = 32 }
        Curvature.segmentInflectionParameters segment options
        |> Result.mapError (fun _ -> NonFinite)

    let private sourceSegmentOffsetIsStalled
        segment
        (offset: float<length>)
        (threshold: float<length>) =
        match circularArcOffsetRadius segment offset with
        | Ok radius -> abs radius <= threshold
        | Error _ ->
            match offsetPoint segment 0.0<parameter> offset,
                  offsetPoint segment 0.5<parameter> offset,
                  offsetPoint segment 1.0<parameter> offset with
            | Ok startPoint, Ok middlePoint, Ok endPoint ->
                Point.distance startPoint middlePoint + Point.distance middlePoint endPoint <= threshold
            | _ -> false

    let private stalledStatus segment offset threshold =
        if sourceSegmentOffsetIsStalled segment offset threshold then SideStalled
        else SideNotStalled

    let private classifyPreparedSegmentsForBothOffsets
        (segments: APreparedSegment list)
        (distances: OffsetDistances)
        (threshold: float<length>)
        : SynchronizedClassifiedSegment list =
        segments
        |> List.map (fun prepared ->
            { Prepared = prepared
              InnerStatus = stalledStatus prepared.Segment distances.Inner threshold
              OuterStatus = stalledStatus prepared.Segment distances.Outer threshold
              StartBoundary = { Inner = Ordinary; Outer = Ordinary }
              EndBoundary = { Inner = Ordinary; Outer = Ordinary } })
        |> fun classified -> markSynchronizedCrossSegmentReversals classified distances

    let private synchronizedSideBoundary (boundary: BoundaryPair) side =
        match side with
        | Inner -> boundary.Inner
        | Outer -> boundary.Outer

    let private setSynchronizedSideStalled (source: SynchronizedSourceSegment) side =
        match side with
        | Inner -> { source with InnerStatus = SideStalled }
        | Outer -> { source with OuterStatus = SideStalled }

    let rec private synchronizedFirstReversalParameter
        (sources: SynchronizedSourceSegment list)
        side =
        match sources with
        | [] -> None
        | first :: rest ->
            match synchronizedSideBoundary first.EndBoundary side with
            | Ordinary -> synchronizedFirstReversalParameter rest side
            | ReversalBoundary _ -> Some first.PreparedTo
            | Inflection
            | NonReversalBoundaryTouch -> None

    let private synchronizedLastReversalParameter
        (sources: SynchronizedSourceSegment list)
        side =
        sources
        |> List.rev
        |> List.map (fun source ->
            { source with
                PreparedFrom = 1.0<parameter> - source.PreparedTo
                PreparedTo = 1.0<parameter> - source.PreparedFrom
                StartBoundary = source.EndBoundary
                EndBoundary = source.StartBoundary })
        |> fun reversed -> synchronizedFirstReversalParameter reversed side
        |> Option.map (fun t -> 1.0<parameter> - t)

    let private splitSynchronizedSourceAtParameter
        (source: SynchronizedSourceSegment)
        parameterValue =
        let local =
            (parameterValue - source.PreparedFrom)
            / (source.PreparedTo - source.PreparedFrom)
            |> Parameter.fromFloat
        Segment.split source.Segment local
        |> Result.mapError PathError
        |> Result.map (fun (left, right) ->
            let ordinary = { Inner = Ordinary; Outer = Ordinary }
            { source with
                PreparedTo = parameterValue
                Segment = left
                EndBoundary = ordinary },
            { source with
                PreparedFrom = parameterValue
                Segment = right
                StartBoundary = ordinary })

    let private splitSynchronizedSourcesAt sources parameterValue =
        let rec loop remaining =
            match remaining with
            | [] -> Ok []
            | first :: rest
                when parameterValue > first.PreparedFrom + pointParameterTolerance
                     && parameterValue < first.PreparedTo - pointParameterTolerance ->
                splitSynchronizedSourceAtParameter first parameterValue
                |> Result.map (fun (left, right) -> left :: right :: rest)
            | first :: rest -> loop rest |> Result.map (fun splitRest -> first :: splitRest)
        loop sources

    let private synchronizedLateStallNearStart sources side offset stalledThreshold =
        match synchronizedFirstReversalParameter sources side with
        | None -> Ok sources
        | Some rootT ->
            let expandedTo = rootT * 2.0
            if expandedTo >= 1.0<parameter> - pointParameterTolerance then
                Ok sources
            else
                match sources with
                | [] -> Ok sources
                | first :: _ ->
                    preparedSegmentBetween first.Prepared 0.0<parameter> expandedTo
                    |> Result.bind (fun stalledSegment ->
                        if not (sourceSegmentOffsetIsStalled stalledSegment offset stalledThreshold) then
                            Ok sources
                        else
                            splitSynchronizedSourcesAt sources expandedTo
                            |> Result.map (List.map (fun source ->
                                if source.PreparedTo <= expandedTo + pointParameterTolerance then
                                    setSynchronizedSideStalled source side
                                else source)))

    let private synchronizedLateStallNearEnd sources side offset stalledThreshold =
        match synchronizedLastReversalParameter sources side with
        | None -> Ok sources
        | Some rootT ->
            let expandedFrom = rootT * 2.0 - 1.0<parameter>
            if expandedFrom <= pointParameterTolerance then
                Ok sources
            else
                match sources with
                | [] -> Ok sources
                | first :: _ ->
                    preparedSegmentBetween first.Prepared expandedFrom 1.0<parameter>
                    |> Result.bind (fun stalledSegment ->
                        if not (sourceSegmentOffsetIsStalled stalledSegment offset stalledThreshold) then
                            Ok sources
                        else
                            splitSynchronizedSourcesAt sources expandedFrom
                            |> Result.map (List.map (fun source ->
                                if source.PreparedFrom >= expandedFrom - pointParameterTolerance then
                                    setSynchronizedSideStalled source side
                                else source)))

    let private synchronizedLateStalls sources side offset stalledThreshold =
        synchronizedLateStallNearStart sources side offset stalledThreshold
        |> Result.bind (fun sources ->
            synchronizedLateStallNearEnd sources side offset stalledThreshold)

    let private refinePreparedSegmentForBothOffsets
        (prepared: APreparedSegment)
        (distances: OffsetDistances)
        innerStatus
        outerStatus
        innerStartBoundary
        innerEndBoundary
        outerStartBoundary
        outerEndBoundary
        (stalledThreshold: float<length>)
        : Result<SynchronizedSourceSegment list, Error> =
        synchronizedCurvatureSplitParameters
            prepared.Segment
            distances.Inner
            distances.Outer
            innerStatus
            outerStatus
        |> Result.bind (fun parameters ->
            let innerBoundaries =
                classifyCurvatureBoundaries parameters prepared.Segment distances.Inner
                |> fun boundaries ->
                    applyEndpointBoundaryOverrides boundaries innerStartBoundary innerEndBoundary
            let outerBoundaries =
                classifyCurvatureBoundaries parameters prepared.Segment distances.Outer
                |> fun boundaries ->
                    applyEndpointBoundaryOverrides boundaries outerStartBoundary outerEndBoundary
            splitPreparedSegmentForBothOffsets
                prepared
                innerBoundaries
                outerBoundaries
                innerStatus
                outerStatus)
        |> Result.bind (fun sources ->
            synchronizedLateStalls sources Inner distances.Inner stalledThreshold)
        |> Result.bind (fun sources ->
            synchronizedLateStalls sources Outer distances.Outer stalledThreshold)

    let private refineSynchronizedClassifiedSegments
        (segments: SynchronizedClassifiedSegment list)
        (distances: OffsetDistances)
        (stalledThreshold: float<length>) =
        let rec loop
            (remaining: SynchronizedClassifiedSegment list)
            (refined: SynchronizedSourceSegment list) =
            match remaining with
            | [] -> Ok(List.rev refined)
            | first :: rest ->
                refinePreparedSegmentForBothOffsets
                    first.Prepared
                    distances
                    first.InnerStatus
                    first.OuterStatus
                    first.StartBoundary.Inner
                    first.EndBoundary.Inner
                    first.StartBoundary.Outer
                    first.EndBoundary.Outer
                    stalledThreshold
                |> Result.bind (fun next -> loop rest (List.rev next @ refined))
        loop segments []

    let private segmentIsFinite segment =
        match segment with
        | Line(startPoint, endPoint) -> pointIsFinite startPoint && pointIsFinite endPoint
        | QuadraticBezier(startPoint, control, endPoint) ->
            pointIsFinite startPoint && pointIsFinite control && pointIsFinite endPoint
        | CubicBezier(startPoint, control1, control2, endPoint) ->
            pointIsFinite startPoint
            && pointIsFinite control1
            && pointIsFinite control2
            && pointIsFinite endPoint
        | Arc endpoint ->
            pointIsFinite endpoint.Start
            && pointIsFinite endpoint.Radius
            && System.Double.IsFinite(float endpoint.XAxisRotation)
            && pointIsFinite endpoint.End

    let private fittedCurveToSegment curve =
        match curve with
        | CubicBezierData(startPoint, control1, control2, endPoint) ->
            Ok(CubicBezier(startPoint, control1, control2, endPoint))
        | _ -> Error NonFinite

    let private cubicFitError error =
        match error with
        | BezierError.DegenerateTangent -> DegenerateTangent 0.0<parameter>
        | _ -> NonFinite

    let private stalledSegmentOffsetSamples segment offset index count tValues samples =
        let rec loop remaining collected =
            match remaining with
            | [] -> Ok collected
            | localT :: rest ->
                offsetPoint segment localT offset
                |> Result.bind (fun point ->
                    let t = Parameter.fromFloat ((float index + Parameter.ratio localT) / float count)
                    loop rest ((t, point) :: collected))
        loop tValues samples

    let private stalledRunOffsetSamples segments offset =
        let count = List.length segments
        let rec loop index remaining samples =
            match remaining with
            | [] -> Ok(List.rev samples)
            | first :: rest ->
                stalledSegmentOffsetSamples
                    first
                    offset
                    index
                    count
                    [ 0.25<parameter>; 0.5<parameter>; 0.75<parameter> ]
                    samples
                |> Result.bind (fun samples -> loop (index + 1) rest samples)
        loop 0 segments []

    let private stalledRunCollapsed startPoint endPoint = startPoint = endPoint

    let private offsetNonemptyStalledSourceRun
        first
        last
        (run: CStalledSegment list)
        startPoint
        endPoint
        samples =
        match unitTangent first 0.0<parameter>, unitTangent last 1.0<parameter> with
        | Ok startTangent, Ok endTangent ->
            Bezier.fitCubicWithEndpointTangents startPoint endPoint startTangent endTangent samples
            |> Result.mapError cubicFitError
            |> Result.bind (fst >> fittedCurveToSegment)
            |> Result.bind (fun segment ->
                if not (segmentIsFinite segment) then Error NonFinite
                else
                    match unitTangent segment 0.0<parameter>, unitTangent segment 1.0<parameter> with
                    | Ok startDirection, Ok endDirection ->
                        Ok
                            [ makeOffsetSegment
                                segment
                                (OffsetFromStalledRun run)
                                startDirection
                                endDirection ]
                    | Error error, _
                    | _, Error error -> Error error)
        | Error error, _
        | _, Error error -> Error error

    let private offsetCStalledRun (run: CStalledSegment list) offset =
        let segments = run |> List.map (fun stalled -> stalled.Segment)
        match segments with
        | [] -> Ok []
        | first :: _ ->
            let last = List.last segments
            match offsetPoint first 0.0<parameter> offset, offsetPoint last 1.0<parameter> offset with
            | Ok startPoint, Ok endPoint ->
                stalledRunOffsetSamples segments offset
                |> Result.bind (fun samples ->
                    if stalledRunCollapsed startPoint endPoint then Ok []
                    else
                        match segments, circularArcOffsetRadius first offset with
                        | [ _ ], Ok radius ->
                            offsetCircularArcSegmentRaw first offset radius
                            |> Result.bind (fun arc ->
                                buildExactArcOffsetSegment arc (OffsetFromStalledRun run))
                            |> Result.map List.singleton
                        | _ ->
                            offsetNonemptyStalledSourceRun
                                first last run startPoint endPoint samples)
            | Error error, _
            | _, Error error -> Error error

    let private unitVector t point =
        let norm = Point.norm point
        if norm > smallUnitDivisionTolerance () then Ok(Point.scale (1.0 / norm) point)
        else Error(DegenerateTangent t)

    let private signedAngle a b = Trig.atan2Degrees (Point.cross a b) (Point.dot a b)

    let private validateReversalHandleScalar startPoint endPoint value =
        let chord = Point.distance startPoint endPoint
        let minimum = reversalFitMinHandleChordRatio * chord
        let maximum = reversalFitMaxHandleChordRatio * chord
        if System.Double.IsFinite(float value)
           && chord > pointTolerance
           && value >= minimum
           && value <= maximum then Ok()
        else Error NonFinite

    let private fitOneHandle samples fixedPoint column =
        let ata, atb, count =
            samples
            |> List.fold (fun (ata, atb, count) (t, point) ->
                let target = Point.subtract point (fixedPoint t)
                let col = column t
                ata + Point.dot col col,
                atb + Point.dot col target,
                count + 1) (0.0, 0.0<length>, 0)
        if count = 0 || abs ata <= 1.0e-9 then Error NonFinite
        else Ok(atb / ata)

    let private fitStartTangentOneHandle startPoint endPoint direction control2 samples =
        fitOneHandle
            samples
            (fun t ->
                let t = Parameter.ratio t
                let u = 1.0 - t
                Point.add
                    (Point.add
                        (Point.scale (u * u * u + 3.0 * u * u * t) startPoint)
                        (Point.scale (3.0 * u * t * t) control2))
                    (Point.scale (t * t * t) endPoint))
            (fun t ->
                let t = Parameter.ratio t
                let u = 1.0 - t
                Point.scale (3.0 * u * u * t) direction)

    let private fitEndTangentOneHandle startPoint endPoint control1 direction samples =
        fitOneHandle
            samples
            (fun t ->
                let t = Parameter.ratio t
                let u = 1.0 - t
                Point.add
                    (Point.add
                        (Point.scale (u * u * u) startPoint)
                        (Point.scale (3.0 * u * u * t) control1))
                    (Point.scale (3.0 * u * t * t + t * t * t) endPoint))
            (fun t ->
                let t = Parameter.ratio t
                let u = 1.0 - t
                Point.scale (-3.0 * u * t * t) direction)

    let private bisectSignedZero
        (score: float<length> -> float)
        (fromValue: float<length>)
        (toValue: float<length>)
        iterations =
        let rec loop
            (fromValue: float<length>)
            (toValue: float<length>)
            (fromScore: float)
            remaining
            : float<length> =
            if remaining <= 0 then (fromValue + toValue) / 2.0
            else
                let middle = (fromValue + toValue) / 2.0
                let middleScore = score middle
                if middleScore = 0.0 then middle
                elif fromScore * middleScore <= 0.0 then
                    loop fromValue middle fromScore (remaining - 1)
                else loop middle toValue middleScore (remaining - 1)
        let fromScore = score fromValue
        let toScore = score toValue
        if fromScore = 0.0 then Ok fromValue
        elif toScore = 0.0 then Ok toValue
        elif fromScore * toScore > 0.0 then Error NonFinite
        else Ok(loop fromValue toValue fromScore iterations)

    let private directionLineIntersection a aDirection b bDirection =
        match unitVector 0.0<parameter> aDirection, unitVector 0.0<parameter> bDirection with
        | Ok aUnit, Ok bUnit ->
            let determinant = Point.cross aUnit bUnit
            if abs determinant < Trig.sinDegrees reversalFitLineApertureDegrees then Error NonFinite
            else
                let delta = Point.displacement a b
                let scaleA = Point.cross delta bUnit / determinant
                Ok(Point.translate (Point.scale scaleA aUnit) a)
        | Error error, _
        | _, Error error -> Error error

    let private directionsFollowChord startPoint endPoint startDirection endDirection =
        match unitVector 0.5<parameter> (Point.displacement startPoint endPoint) with
        | Error _ -> false
        | Ok chordDirection ->
            abs (signedAngle startDirection chordDirection) <= reversalTangentGapDegrees
            && abs (signedAngle endDirection chordDirection) <= reversalTangentGapDegrees

    let private availableOffsetFitSamples segment offset tValues =
        tValues
        |> List.choose (fun t ->
            match offsetPoint segment t offset with
            | Ok point -> Some(t, point)
            | Error _ -> None)

    let private stalledStartControl2ByBisection
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>) =
        let chord = Point.distance startPoint endPoint
        let fromValue = reversalFitMinHandleChordRatio * chord
        let toValue = reversalFitMaxHandleChordRatio * chord
        let pointFor handle = Point.translate (Point.scale -handle endDirection) endPoint
        let score handle =
            match unitVector 0.0<parameter> (Point.displacement startPoint (pointFor handle)) with
            | Error _ -> 0.0
            | Ok direction -> Point.cross startDirection direction
        bisectSignedZero score fromValue toValue 40 |> Result.map pointFor

    let private stalledEndControl1ByBisection
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>) =
        let chord = Point.distance startPoint endPoint
        let fromValue = reversalFitMinHandleChordRatio * chord
        let toValue = reversalFitMaxHandleChordRatio * chord
        let pointFor handle = Point.translate (Point.scale handle startDirection) startPoint
        let score handle =
            match unitVector 1.0<parameter> (Point.displacement (pointFor handle) endPoint) with
            | Error _ -> 0.0
            | Ok direction -> Point.cross endDirection direction
        bisectSignedZero score fromValue toValue 40 |> Result.map pointFor

    let private stalledStartControl2ParallelOrBisection
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>)
        samples =
        if not (directionsFollowChord startPoint endPoint startDirection endDirection) then
            stalledStartControl2ByBisection startPoint endPoint startDirection endDirection
        else
            unitVector 1.0<parameter> endDirection
            |> Result.bind (fun endDirection ->
                fitEndTangentOneHandle startPoint endPoint startPoint endDirection samples
                |> Result.bind (fun handle ->
                    validateReversalHandleScalar startPoint endPoint handle
                    |> Result.map (fun _ -> Point.translate (Point.scale -handle endDirection) endPoint)))

    let private stalledEndControl1ParallelOrBisection
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>)
        samples =
        if not (directionsFollowChord startPoint endPoint startDirection endDirection) then
            stalledEndControl1ByBisection startPoint endPoint startDirection endDirection
        else
            unitVector 0.0<parameter> startDirection
            |> Result.bind (fun startDirection ->
                fitStartTangentOneHandle startPoint endPoint startDirection endPoint samples
                |> Result.bind (fun handle ->
                    validateReversalHandleScalar startPoint endPoint handle
                    |> Result.map (fun _ -> Point.translate (Point.scale handle startDirection) startPoint)))

    let private stalledStartControl2
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>)
        samples =
        match directionLineIntersection startPoint startDirection endPoint (Point.negate endDirection) with
        | Ok point when Point.distance endPoint point <= 2.0 * Point.distance startPoint endPoint -> Ok point
        | Ok _ -> stalledStartControl2ByBisection startPoint endPoint startDirection endDirection
        | Error _ ->
            stalledStartControl2ParallelOrBisection
                startPoint endPoint startDirection endDirection samples

    let private stalledEndControl1
        (startPoint: Point<length>)
        (endPoint: Point<length>)
        (startDirection: Point<1>)
        (endDirection: Point<1>)
        samples =
        match directionLineIntersection startPoint startDirection endPoint endDirection with
        | Ok point when Point.distance startPoint point <= 2.0 * Point.distance startPoint endPoint -> Ok point
        | Ok _ -> stalledEndControl1ByBisection startPoint endPoint startDirection endDirection
        | Error _ ->
            stalledEndControl1ParallelOrBisection
                startPoint endPoint startDirection endDirection samples

    let private offsetDerivative segment t offset =
        match Segment.derivative segment t, Segment.secondDerivative segment t with
        | Ok derivative, Ok second ->
            let speed = Point.norm derivative
            if speed <= smallUnitDivisionTolerance () then Error(DegenerateTangent t)
            else
                let tangentChange =
                    Point.subtract
                        (Point.scale (1.0 / speed) second)
                        (Point.scale
                            (Point.dot derivative second / (speed * speed * speed))
                            derivative)
                let candidate =
                    Point.add derivative (Point.scale offset (Point.rotateCounterclockwise tangentChange))
                if pointIsFinite candidate then Ok candidate else Error NonFinite
        | Error error, _
        | _, Error error -> Error(PathError error)

    let private tangentTurnFromAperture aperture =
        if aperture <= tangentTurnAngleEpsilon
           || 360.0<degree> - aperture <= tangentTurnAngleEpsilon then Straight
        elif abs (aperture - 180.0<degree>) <= tangentTurnAngleEpsilon then CouldNotMeasure
        elif aperture < 180.0<degree> then Clockwise
        else CounterClockwise

    let private endpointTangentTurnFromChord segment endpoint =
        match unitVector 0.5<parameter> (Point.displacement (Segment.start segment) (Segment.finish segment)) with
        | Error _ -> CouldNotMeasure
        | Ok chordDirection ->
            match endpoint with
            | SegmentStart ->
                match unitTangent segment 0.0<parameter> with
                | Ok tangent -> Point.clockwiseAperture tangent chordDirection |> tangentTurnFromAperture
                | Error _ -> CouldNotMeasure
            | SegmentEnd ->
                match unitTangent segment 1.0<parameter> with
                | Ok tangent -> Point.clockwiseAperture chordDirection tangent |> tangentTurnFromAperture
                | Error _ -> CouldNotMeasure

    let private endpointTangentTurn segment endpoint =
        match segment with
        | Line _ -> Straight
        | _ ->
            let t = if endpoint = SegmentStart then 0.0<parameter> else 1.0<parameter>
            match Curvature.segmentLeftNormalCurvature segment t with
            | Ok value when abs value > tangentTurnCurvatureEpsilon ->
                if value < 0.0<1/length> then Clockwise else CounterClockwise
            | _ -> endpointTangentTurnFromChord segment endpoint

    let private rotateDirection direction degrees =
        Point.direction (Point.heading direction + degrees)

    let private clampedReversalFitNudge reversalDirection oppositeDirection desiredDegrees =
        let room = signedAngle reversalDirection oppositeDirection
        if room * desiredDegrees <= 0.0<degree^2> then 0.0<degree>
        elif abs room <= abs desiredDegrees then room * 0.5
        else desiredDegrees

    let private nudgedReversalFitDirection direction oppositeDirection turn endpoint =
        let sign =
            match turn, endpoint with
            | Clockwise, SegmentStart -> 1.0
            | Clockwise, SegmentEnd -> -1.0
            | CounterClockwise, SegmentStart -> -1.0
            | CounterClockwise, SegmentEnd -> 1.0
            | Straight, _
            | CouldNotMeasure, _ -> 0.0
        let desired = sign * reversalFitTangentNudgeDegrees
        let degrees =
            match oppositeDirection with
            | Ok opposite -> clampedReversalFitNudge direction opposite desired
            | Error _ -> desired
        rotateDirection direction degrees

    let private boundaryReachesOffsetRadius boundary offset =
        match boundary with
        | ReversalBoundary(Some value) when value <> 0.0<1/length> ->
            let radius = 1.0 / value
            System.Double.IsFinite(float radius)
            && abs (radius - offset) <= curvatureRadiusTolerance
        | _ -> false

    let private eJoinFreeEndpointReachesOffsetRadius
        (source: EJoinFreeSegment)
        offset
        endpoint =
        let boundary =
            match endpoint with
            | SegmentStart -> source.StartBoundary
            | SegmentEnd -> source.EndBoundary
        boundaryReachesOffsetRadius boundary offset

    let private eJoinFreeSourceEndpointCurvature (source: EJoinFreeSegment) endpoint =
        let refinedT = if endpoint = SegmentStart then source.RefinedFrom else source.RefinedTo
        let refined = source.Refined
        let preparedT =
            refined.PreparedFrom + (refined.PreparedTo - refined.PreparedFrom) * Parameter.ratio refinedT
        sourceEndpointCurvature refined.Prepared.Segment preparedT

    let private rejectBezierDoubleRadiusReversalESegment (source: EJoinFreeSegment) offset =
        if boundaryIsReversal source.StartBoundary
           && boundaryIsReversal source.EndBoundary
           && eJoinFreeEndpointReachesOffsetRadius source offset SegmentStart
           && eJoinFreeEndpointReachesOffsetRadius source offset SegmentEnd
           && segmentIsBezier source.Segment then Error NonFinite
        else Ok()

    let private eJoinFreeEndpointOffsetDirection
        (source: EJoinFreeSegment)
        offset endpoint isReversal reachesOffsetRadius =
        let endpointT, interiorT =
            match endpoint with
            | SegmentStart -> 0.0<parameter>, curvatureParameterTolerance * 2.0
            | SegmentEnd -> 1.0<parameter>, 1.0<parameter> - curvatureParameterTolerance * 2.0
        let directionAt t =
            offsetDerivative source.Segment t offset
            |> Result.bind (fun derivative ->
                Point.normalize derivative
                |> Option.map Ok
                |> Option.defaultValue (Error(DegenerateTangent t)))
        if isReversal && reachesOffsetRadius then
            match directionAt interiorT with
            | Ok direction -> Ok direction
            | Error _ -> directionAt endpointT
        else directionAt endpointT

    let private eJoinFreeEndpointPolicy (source: EJoinFreeSegment) offset endpoint =
        let boundary = if endpoint = SegmentStart then source.StartBoundary else source.EndBoundary
        let isReversal = boundaryIsReversal boundary
        let reachesOffsetRadius = eJoinFreeEndpointReachesOffsetRadius source offset endpoint
        let oppositeT = if endpoint = SegmentStart then 1.0<parameter> else 0.0<parameter>
        let oppositeDirection =
            offsetDerivative source.Segment oppositeT offset
            |> Result.bind (fun derivative ->
                Point.normalize derivative
                |> Option.map Ok
                |> Option.defaultValue (Error(DegenerateTangent oppositeT)))
        eJoinFreeEndpointOffsetDirection source offset endpoint isReversal reachesOffsetRadius
        |> Result.map (fun direction ->
            if not isReversal then FitPositionAndDirection direction
            else
                let nudged =
                    nudgedReversalFitDirection
                        direction
                        oppositeDirection
                        (endpointTangentTurn source.Segment endpoint)
                        endpoint
                if reachesOffsetRadius then FitPositionAndDirectionWithCollapsedHandle nudged
                else FitPositionAndDirection nudged)

    let private fitOffsetCubicBothStalled startPoint endPoint startDirection endDirection =
        match unitVector 0.5<parameter> (Point.displacement startPoint endPoint) with
        | Error error -> Error error
        | Ok chordDirection ->
            let startAngle = abs (signedAngle startDirection chordDirection)
            let endAngle = abs (signedAngle endDirection chordDirection)
            if startAngle <= reversalTangentGapDegrees && endAngle <= reversalTangentGapDegrees then
                Ok(Line(startPoint, endPoint))
            else Error NonFinite

    let private fitOffsetCubicStartStalledEndTangent startPoint endPoint startDirection endDirection samples =
        stalledStartControl2 startPoint endPoint startDirection endDirection samples
        |> Result.map (fun control2 -> CubicBezierData(startPoint, startPoint, control2, endPoint))

    let private fitOffsetCubicStartTangentEndStalled startPoint endPoint startDirection endDirection samples =
        stalledEndControl1 startPoint endPoint startDirection endDirection samples
        |> Result.map (fun control1 -> CubicBezierData(startPoint, control1, endPoint, endPoint))

    let private fitOffsetCubicStartStalledEndPosition startPoint endPoint startDirection samples =
        unitVector 0.0<parameter> startDirection
        |> Result.bind (fun direction ->
            fitStartTangentOneHandle startPoint endPoint direction endPoint samples
            |> Result.bind (fun handle ->
                validateReversalHandleScalar startPoint endPoint handle
                |> Result.map (fun _ ->
                    CubicBezierData(startPoint, startPoint, Point.translate (Point.scale handle direction) startPoint, endPoint))))

    let private fitOffsetCubicStartPositionEndStalled startPoint endPoint endDirection samples =
        unitVector 1.0<parameter> endDirection
        |> Result.bind (fun direction ->
            fitEndTangentOneHandle startPoint endPoint startPoint direction samples
            |> Result.bind (fun handle ->
                validateReversalHandleScalar startPoint endPoint handle
                |> Result.map (fun _ ->
                    CubicBezierData(startPoint, Point.translate (Point.scale -handle direction) endPoint, endPoint, endPoint))))

    let private fitOffsetCubicStartTangentEndPosition startPoint endPoint startDirection samples =
        unitVector 0.0<parameter> startDirection
        |> Result.bind (fun direction ->
            fitStartTangentOneHandle startPoint endPoint direction endPoint samples
            |> Result.bind (fun handle ->
                validateReversalHandleScalar startPoint endPoint handle
                |> Result.map (fun _ ->
                    CubicBezierData(startPoint, Point.translate (Point.scale handle direction) startPoint, endPoint, endPoint))))

    let private fitOffsetCubicStartPositionEndTangent startPoint endPoint endDirection samples =
        unitVector 1.0<parameter> endDirection
        |> Result.bind (fun direction ->
            fitEndTangentOneHandle startPoint endPoint startPoint direction samples
            |> Result.bind (fun handle ->
                validateReversalHandleScalar startPoint endPoint handle
                |> Result.map (fun _ ->
                    CubicBezierData(startPoint, startPoint, Point.translate (Point.scale -handle direction) endPoint, endPoint))))

    let rec private fitOffsetCubicDataWithEndpointPolicies
        startPoint
        endPoint
        startPolicy
        endPolicy
        samples =
        match startPolicy, endPolicy with
        | FitPositionAndDirection startDirection, FitPositionAndDirection endDirection ->
            Bezier.fitCubicWithEndpointTangents startPoint endPoint startDirection endDirection samples
            |> Result.mapError cubicFitError
            |> Result.bind (fun (curve, report) ->
                recoverCollapsedDirectionFit
                    curve report startPoint endPoint startDirection endDirection samples)
        | FitPositionOnly, FitPositionOnly ->
            Bezier.fitCubicWithEndpoints startPoint endPoint samples
            |> Result.mapError cubicFitError
            |> Result.map fst
        | FitPositionAndDirectionWithCollapsedHandle _,
          FitPositionAndDirectionWithCollapsedHandle _ -> Error NonFinite
        | FitPositionAndDirectionWithCollapsedHandle startDirection,
          FitPositionAndDirection endDirection ->
            fitOffsetCubicStartStalledEndTangent
                startPoint endPoint startDirection endDirection samples
        | FitPositionAndDirection startDirection,
          FitPositionAndDirectionWithCollapsedHandle endDirection ->
            fitOffsetCubicStartTangentEndStalled
                startPoint endPoint startDirection endDirection samples
        | FitPositionAndDirectionWithCollapsedHandle startDirection, FitPositionOnly ->
            fitOffsetCubicStartStalledEndPosition startPoint endPoint startDirection samples
        | FitPositionOnly, FitPositionAndDirectionWithCollapsedHandle endDirection ->
            fitOffsetCubicStartPositionEndStalled startPoint endPoint endDirection samples
        | FitPositionAndDirection startDirection, FitPositionOnly ->
            fitOffsetCubicStartTangentEndPosition startPoint endPoint startDirection samples
        | FitPositionOnly, FitPositionAndDirection endDirection ->
            fitOffsetCubicStartPositionEndTangent startPoint endPoint endDirection samples

    and private recoverCollapsedDirectionFit
        curve
        report
        startPoint
        endPoint
        startDirection
        endDirection
        samples =
        match report.StartHandle, report.EndHandle with
        | CollapsedHandle, CollapsedHandle -> Error NonFinite
        | CollapsedHandle, PositiveHandle ->
            fitOffsetCubicDataWithEndpointPolicies
                startPoint endPoint
                (FitPositionAndDirectionWithCollapsedHandle startDirection)
                (FitPositionAndDirection endDirection)
                samples
        | PositiveHandle, CollapsedHandle ->
            fitOffsetCubicDataWithEndpointPolicies
                startPoint endPoint
                (FitPositionAndDirection startDirection)
                (FitPositionAndDirectionWithCollapsedHandle endDirection)
                samples
        | PositiveHandle, PositiveHandle -> Ok curve
        | UnconstrainedHandle, _
        | _, UnconstrainedHandle -> Error NonFinite

    let private fitOffsetCubicWithNonBothStalledEndpointPolicies
        startPoint endPoint startPolicy endPolicy samples =
        fitOffsetCubicDataWithEndpointPolicies startPoint endPoint startPolicy endPolicy samples
        |> Result.bind fittedCurveToSegment

    let private fitOffsetCubicWithEndpointPolicies
        startPoint endPoint startPolicy endPolicy samples =
        match startPolicy, endPolicy with
        | FitPositionAndDirectionWithCollapsedHandle startDirection,
          FitPositionAndDirectionWithCollapsedHandle endDirection ->
            fitOffsetCubicBothStalled startPoint endPoint startDirection endDirection
        | _ ->
            fitOffsetCubicWithNonBothStalledEndpointPolicies
                startPoint endPoint startPolicy endPolicy samples

    let private unitTangentAtEndpoint segment endpoint =
        match endpoint with
        | SegmentStart -> unitTangent segment 0.0<parameter>
        | SegmentEnd -> unitTangent segment 1.0<parameter>

    let private offsetSegmentNudgedTangentDirection source offsetSegment offset endpoint =
        match source with
        | OffsetFromJoinFree joinFree ->
            eJoinFreeEndpointPolicy joinFree offset endpoint
            |> Result.bind (function
                | FitPositionOnly -> unitTangentAtEndpoint offsetSegment endpoint
                | FitPositionAndDirection direction
                | FitPositionAndDirectionWithCollapsedHandle direction -> Ok direction)
        | OffsetFromStalledRun _ -> unitTangentAtEndpoint offsetSegment endpoint

    let private buildOffsetSegment offset source segment =
        match offsetSegmentNudgedTangentDirection source segment offset SegmentStart,
              offsetSegmentNudgedTangentDirection source segment offset SegmentEnd with
        | Ok startTangent, Ok endTangent ->
            Ok(makeOffsetSegment segment source startTangent endTangent)
        | Error error, _
        | _, Error error -> Error error

    let private smartOffsetDivergence source candidate offset options =
        let rec loop sample best validSamples =
            if sample > options.Fitting.Samples then
                if validSamples = 0 then Error(DegenerateTangent 0.5<parameter>) else Ok best
            else
                let t = Parameter.fromFloat (float sample / float (options.Fitting.Samples + 1))
                match offsetPoint source t offset with
                | Error(DegenerateTangent _) -> loop (sample + 1) best validSamples
                | Error error -> Error error
                | Ok point ->
                    Segment.point candidate t
                    |> Result.mapError PathError
                    |> Result.bind (fun candidatePoint ->
                        let best = max best (Point.distance point candidatePoint)
                        if best > options.Fitting.Tolerance then Ok best
                        else loop (sample + 1) best (validSamples + 1))
        loop 1 0.0<length> 0

    let private offsetDivergence source candidate offset (options: Options) =
        let rec loop sample best =
            if sample > options.Fitting.Samples then Ok best
            else
                let t =
                    Parameter.fromFloat
                        (float sample / float (options.Fitting.Samples + 1))
                offsetPoint source t offset
                |> Result.bind (fun point ->
                    Segment.point candidate t
                    |> Result.mapError PathError
                    |> Result.bind (fun candidatePoint ->
                        let best = max best (Point.distance point candidatePoint)
                        if best > options.Fitting.Tolerance then Ok best
                        else loop (sample + 1) best))
        loop 1 0.0<length>

    let private fitEJoinFreeOffsetSegment
        (source: EJoinFreeSegment)
        offset
        : Result<Segment, Error> =
        match offsetPoint source.Segment 0.0<parameter> offset,
              offsetPoint source.Segment 1.0<parameter> offset with
        | Ok startPoint, Ok endPoint ->
            let samples =
                availableOffsetFitSamples
                    source.Segment
                    offset
                    [ 0.2<parameter>; 0.35<parameter>; 0.5<parameter>; 0.65<parameter>; 0.8<parameter> ]
            match eJoinFreeEndpointPolicy source offset SegmentStart,
                  eJoinFreeEndpointPolicy source offset SegmentEnd,
                  rejectBezierDoubleRadiusReversalESegment source offset with
            | Ok startPolicy, Ok endPolicy, Ok _ ->
                fitOffsetCubicWithEndpointPolicies
                    startPoint endPoint startPolicy endPolicy samples
                |> Result.bind (fun candidate ->
                    if segmentIsFinite candidate then Ok candidate else Error NonFinite)
            | Error error, _, _
            | _, Error error, _
            | _, _, Error error -> Error error
        | Error error, _
        | _, Error error -> Error error

    let private fittedOffsetAttempt
        (source: EJoinFreeSegment)
        offset
        (options: Options)
        : Result<OffsetAttempt, Error> =
        fitEJoinFreeOffsetSegment source offset
        |> Result.bind (fun candidate ->
            smartOffsetDivergence source.Segment candidate offset options
            |> Result.bind (fun divergence ->
                if divergence > rawFittingTolerance options then
                    Ok(OffsetNeedsRefinement divergence)
                else
                    buildOffsetSegment offset (OffsetFromJoinFree source) candidate
                    |> Result.map OffsetAccepted))

    let private offsetEJoinFreeSegmentAttempt
        (source: EJoinFreeSegment)
        offset
        (options: Options)
        : Result<OffsetAttempt, Error> =
        match source.Segment with
        | Line _ ->
            match offsetPoint source.Segment 0.0<parameter> offset,
                  offsetPoint source.Segment 1.0<parameter> offset with
            | Ok startPoint, Ok endPoint ->
                buildOffsetSegment
                    offset
                    (OffsetFromJoinFree source)
                    (Line(startPoint, endPoint))
                |> Result.map OffsetAccepted
            | Error error, _
            | _, Error error -> Error error
        | Arc _ ->
            match circularArcOffsetRadius source.Segment offset with
            | Ok radius when abs radius <= pointTolerance -> Error(DegenerateTangent 0.0<parameter>)
            | Ok radius ->
                offsetCircularArcSegmentRaw source.Segment offset radius
                |> Result.bind (fun arc ->
                    buildExactArcOffsetSegment arc (OffsetFromJoinFree source))
                |> Result.map OffsetAccepted
            | Error _ -> fittedOffsetAttempt source offset options
        | _ -> fittedOffsetAttempt source offset options

    let rec private offsetEWithSourceTree
        (source: EJoinFreeSegment)
        offset
        (options: Options)
        depth
        : Result<FUnhealedOffsetSegment list * SynchronizedSideSource, Error> =
        offsetEJoinFreeSegmentAttempt source offset options
        |> Result.bind (function
            | OffsetAccepted offsetSegment ->
                Ok([ offsetSegment ], RefinableSideSource source)
            | OffsetNeedsRefinement divergence when depth <= 0 ->
                Error(MaxDepthReached divergence)
            | OffsetNeedsRefinement _ ->
                splitEJoinFreeSegmentAtMidpoint source
                |> Result.bind (fun (left, right) ->
                    match offsetEWithSourceTree left offset options (depth - 1),
                          offsetEWithSourceTree right offset options (depth - 1) with
                    | Ok(leftOffsets, leftSource), Ok(rightOffsets, rightSource) ->
                        Ok(leftOffsets @ rightOffsets, SplitSideSource(leftSource, rightSource))
                    | Error error, _
                    | _, Error error -> Error error))

    let rec private offsetSynchronizedEPair
        (innerSource: EJoinFreeSegment)
        (outerSource: EJoinFreeSegment)
        (distances: OffsetDistances)
        (options: Options)
        depth
        : Result<SynchronizedUnhealedResult, Error> =
        match offsetEJoinFreeSegmentAttempt innerSource distances.Inner options,
              offsetEJoinFreeSegmentAttempt outerSource distances.Outer options with
        | Ok(OffsetAccepted innerOffset), Ok(OffsetAccepted outerOffset) ->
            Ok
                { InnerOffsets = [ innerOffset ]
                  OuterOffsets = [ outerOffset ]
                  InnerSource = RefinableSideSource innerSource
                  OuterSource = RefinableSideSource outerSource }
        | Ok innerAttempt, Ok outerAttempt when depth <= 0 ->
            Error(MaxDepthReached(largestAttemptDivergence innerAttempt outerAttempt))
        | Ok _, Ok _ ->
            match splitEJoinFreeSegmentAtMidpoint innerSource,
                  splitEJoinFreeSegmentAtMidpoint outerSource with
            | Ok(innerLeft, innerRight), Ok(outerLeft, outerRight) ->
                match offsetSynchronizedEPair innerLeft outerLeft distances options (depth - 1),
                      offsetSynchronizedEPair innerRight outerRight distances options (depth - 1) with
                | Ok left, Ok right -> Ok(joinSynchronizedUnhealedResults left right)
                | Error error, _
                | _, Error error -> Error error
            | Error error, _
            | _, Error error -> Error error
        | Error error, _
        | _, Error error -> Error error

    let private offsetSynchronizedSourceSegment
        (source: SynchronizedSourceSegment)
        (distances: OffsetDistances)
        (options: Options)
        portionIndex correspondenceIndex =
        let innerRefined = synchronizedRefinedSegment source Inner
        let outerRefined = synchronizedRefinedSegment source Outer
        let innerE = synchronizedESegment innerRefined portionIndex correspondenceIndex
        let outerE = synchronizedESegment outerRefined portionIndex correspondenceIndex
        match source.InnerStatus, source.OuterStatus with
        | SideNotStalled, SideNotStalled ->
            offsetSynchronizedEPair innerE outerE distances options (refinementDepth options)
        | SideStalled, SideNotStalled ->
            let stalled = synchronizedStalledSegment innerRefined
            match offsetCStalledRun [ stalled ] distances.Inner,
                  offsetEWithSourceTree outerE distances.Outer options (refinementDepth options) with
            | Ok innerOffsets, Ok(outerOffsets, outerSource) ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = StalledSideSource [ stalled ]
                      OuterSource = outerSource }
            | Error error, _
            | _, Error error -> Error error
        | SideNotStalled, SideStalled ->
            let stalled = synchronizedStalledSegment outerRefined
            match offsetEWithSourceTree innerE distances.Inner options (refinementDepth options),
                  offsetCStalledRun [ stalled ] distances.Outer with
            | Ok(innerOffsets, innerSource), Ok outerOffsets ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = innerSource
                      OuterSource = StalledSideSource [ stalled ] }
            | Error error, _
            | _, Error error -> Error error
        | SideStalled, SideStalled ->
            let innerStalled = synchronizedStalledSegment innerRefined
            let outerStalled = synchronizedStalledSegment outerRefined
            match offsetCStalledRun [ innerStalled ] distances.Inner,
                  offsetCStalledRun [ outerStalled ] distances.Outer with
            | Ok innerOffsets, Ok outerOffsets ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = StalledSideSource [ innerStalled ]
                      OuterSource = StalledSideSource [ outerStalled ] }
            | Error error, _
            | _, Error error -> Error error

    let rec private offsetRefinableSynchronizedGroup
        sources side offset options portionIndex segmentIndex =
        match sources with
        | [] -> Error NonFinite
        | first :: rest ->
            let refined = synchronizedRefinedSegment first side
            let source = synchronizedESegment refined portionIndex segmentIndex
            offsetEWithSourceTree source offset options (refinementDepth options)
            |> Result.bind (fun (firstOffsets, firstSource) ->
                match rest with
                | [] -> Ok(firstOffsets, firstSource)
                | _ ->
                    offsetRefinableSynchronizedGroup
                        rest side offset options portionIndex (segmentIndex + 1)
                    |> Result.map (fun (restOffsets, restSource) ->
                        firstOffsets @ restOffsets,
                        SplitSideSource(firstSource, restSource)))

    let private offsetSynchronizedStalledGroup
        (sources: SynchronizedSourceSegment list)
        (distances: OffsetDistances)
        (options: Options)
        portionIndex correspondenceIndex innerStatus outerStatus =
        match innerStatus, outerStatus with
        | SideStalled, SideNotStalled ->
            let stalled = synchronizedStalledGroup sources Inner
            match offsetCStalledRun stalled distances.Inner,
                  offsetRefinableSynchronizedGroup
                    sources Outer distances.Outer options portionIndex correspondenceIndex with
            | Ok innerOffsets, Ok(outerOffsets, outerSource) ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = StalledSideSource stalled
                      OuterSource = outerSource }
            | Error error, _
            | _, Error error -> Error error
        | SideNotStalled, SideStalled ->
            let stalled = synchronizedStalledGroup sources Outer
            match offsetRefinableSynchronizedGroup
                    sources Inner distances.Inner options portionIndex correspondenceIndex,
                  offsetCStalledRun stalled distances.Outer with
            | Ok(innerOffsets, innerSource), Ok outerOffsets ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = innerSource
                      OuterSource = StalledSideSource stalled }
            | Error error, _
            | _, Error error -> Error error
        | SideStalled, SideStalled ->
            let innerStalled = synchronizedStalledGroup sources Inner
            let outerStalled = synchronizedStalledGroup sources Outer
            match offsetCStalledRun innerStalled distances.Inner,
                  offsetCStalledRun outerStalled distances.Outer with
            | Ok innerOffsets, Ok outerOffsets ->
                Ok
                    { InnerOffsets = innerOffsets
                      OuterOffsets = outerOffsets
                      InnerSource = StalledSideSource innerStalled
                      OuterSource = StalledSideSource outerStalled }
            | Error error, _
            | _, Error error -> Error error
        | SideNotStalled, SideNotStalled -> Error NonFinite

    let private offsetSynchronizedSourceGroup
        (sources: SynchronizedSourceSegment list)
        distances options portionIndex correspondenceIndex =
        match sources with
        | [] -> Error NonFinite
        | [ source ] ->
            offsetSynchronizedSourceSegment
                source distances options portionIndex correspondenceIndex
        | first :: _ ->
            offsetSynchronizedStalledGroup
                sources distances options portionIndex correspondenceIndex
                first.InnerStatus first.OuterStatus

    let rec private offsetSynchronizedSourceSegmentsLoop
        sources distances options portionIndex correspondenceIndex
        innerOffsets outerOffsets correspondences =
        match sources with
        | [] ->
            Ok
                { InnerOffsets = List.rev innerOffsets
                  OuterOffsets = List.rev outerOffsets
                  Correspondences = List.rev correspondences }
        | first :: rest ->
            let group, remaining = collectSynchronizedStatusRun first rest
            offsetSynchronizedSourceGroup
                group distances options portionIndex correspondenceIndex
            |> Result.bind (fun built ->
                let correspondence =
                    { PortionIndex = portionIndex
                      CorrespondenceIndex = correspondenceIndex
                      Sources = group
                      Inner = built.InnerSource
                      Outer = built.OuterSource
                      InnerOffsetCount = List.length built.InnerOffsets
                      OuterOffsetCount = List.length built.OuterOffsets }
                offsetSynchronizedSourceSegmentsLoop
                    remaining distances options portionIndex (correspondenceIndex + 1)
                    (List.rev built.InnerOffsets @ innerOffsets)
                    (List.rev built.OuterOffsets @ outerOffsets)
                    (correspondence :: correspondences))

    let private offsetSynchronizedSourceSegments sources distances options portionIndex =
        offsetSynchronizedSourceSegmentsLoop
            sources distances options portionIndex 0 [] [] []

    let private sourceBoundaryIsSmooth left right (_options: Options) =
        match unitTangent left 1.0<parameter>, unitTangent right 0.0<parameter> with
        | Ok leftTangent, Ok rightTangent ->
            abs (signedAngle leftTangent rightTangent)
                <= joinFreeTangentAlignmentAngleDegrees
        | _ -> false

    let private prependJoinFreePortion segments closedValue portions =
        match segments with
        | [] -> Ok portions
        | _ ->
            Subpath.create (List.rev segments)
            |> Result.mapError PathError
            |> Result.bind (fun openSubpath ->
                Subpath.setClosed closedValue openSubpath
                |> Result.mapError PathError)
            |> Result.map (fun subpath ->
                { Index = 0; Subpath = subpath; Closed = closedValue } :: portions)

    let rec private splitJoinFreePortionsLoop segments options current portions =
        match segments with
        | [] ->
            prependJoinFreePortion current false portions
            |> Result.map (List.rev)
        | first :: rest ->
            match current with
            | [] -> splitJoinFreePortionsLoop rest options [ first ] portions
            | previous :: _ when sourceBoundaryIsSmooth previous first options ->
                splitJoinFreePortionsLoop rest options (first :: current) portions
            | _ ->
                prependJoinFreePortion current false portions
                |> Result.bind (fun portions ->
                    splitJoinFreePortionsLoop rest options [ first ] portions)

    let private splitJoinFreePortions segments options =
        splitJoinFreePortionsLoop segments options [] []

    let private markClosedJoinFreePortion (portions: JoinFreePortion list) closedValue =
        match closedValue, portions with
        | true, [ portion ] -> [ { portion with Closed = true } ]
        | _ -> portions

    let private indexJoinFreePortions (portions: JoinFreePortion list) =
        portions |> List.mapi (fun index portion -> { portion with Index = index })

    let private fUnhealedToGHealedOffsetSegment (offset: FUnhealedOffsetSegment) =
        { Segment = offset.Segment
          Source = offset.Source
          NudgedStartTangentDirection = offset.NudgedStartTangentDirection
          NudgedEndTangentDirection = offset.NudgedEndTangentDirection }

    let private gHealedToFUnhealedOffsetSegment
        (offset: GHealedOffsetSegment)
        : FUnhealedOffsetSegment =
        { Segment = offset.Segment
          Source = offset.Source
          NudgedStartTangentDirection = offset.NudgedStartTangentDirection
          NudgedEndTangentDirection = offset.NudgedEndTangentDirection }

    let private gHealedToFUnhealedOffsetSegments
        (offsets: GHealedOffsetSegment list)
        : FUnhealedOffsetSegment list =
        offsets |> List.map gHealedToFUnhealedOffsetSegment

    let private translateSegmentStartHandle segment startPoint delta =
        match segment with
        | Line(_, finish) -> Line(startPoint, finish)
        | QuadraticBezier(_, control, finish) ->
            QuadraticBezier(startPoint, Point.add control delta, finish)
        | CubicBezier(_, control1, control2, finish) ->
            CubicBezier(startPoint, Point.add control1 delta, control2, finish)
        | Arc arc -> Arc { arc with Start = startPoint }

    let private translateSegmentEndHandle segment finish delta =
        match segment with
        | Line(startPoint, _) -> Line(startPoint, finish)
        | QuadraticBezier(startPoint, control, _) ->
            QuadraticBezier(startPoint, Point.add control delta, finish)
        | CubicBezier(startPoint, control1, control2, _) ->
            CubicBezier(startPoint, control1, Point.add control2 delta, finish)
        | Arc arc -> Arc { arc with End = finish }

    let private snapOffsetEndPositionOnly (offset: FUnhealedOffsetSegment) finish =
        let delta = Point.subtract finish (Segment.finish offset.Segment)
        { offset with Segment = translateSegmentEndHandle offset.Segment finish delta }

    let private snapOffsetStartPositionOnly (offset: FUnhealedOffsetSegment) startPoint =
        let delta = Point.subtract startPoint (Segment.start offset.Segment)
        { offset with Segment = translateSegmentStartHandle offset.Segment startPoint delta }

    let private offsetSegmentSourceStartIsReversal source =
        match source with
        | OffsetFromJoinFree joinFree -> boundaryIsReversal joinFree.StartBoundary
        | OffsetFromStalledRun _ -> false

    let private offsetSegmentSourceEndIsReversal source =
        match source with
        | OffsetFromJoinFree joinFree -> boundaryIsReversal joinFree.EndBoundary
        | OffsetFromStalledRun _ -> false

    let private offsetBoundaryIsKnownReversal
        (left: FUnhealedOffsetSegment)
        (right: FUnhealedOffsetSegment) =
        offsetSegmentSourceEndIsReversal left.Source
        && offsetSegmentSourceStartIsReversal right.Source

    let private offsetSegmentCertificationSource source =
        match source with
        | OffsetFromJoinFree joinFree -> Some joinFree.Segment
        | OffsetFromStalledRun _ -> None

    let private healedOffsetCertified (healed: FUnhealedOffsetSegment) offset options =
        match offsetSegmentCertificationSource healed.Source with
        | None -> true
        | Some source ->
            match offsetDivergence source healed.Segment offset options with
            | Ok divergence -> divergence <= options.Fitting.Tolerance
            | Error _ -> false

    let private certifiedHealedBoundary left right offset options =
        healedOffsetCertified left offset options
        && healedOffsetCertified right offset options

    let private healSmoothOffsetBoundary
        (left: FUnhealedOffsetSegment)
        (right: FUnhealedOffsetSegment)
        offset options =
        let boundary = Point.midpoint (Segment.finish left.Segment) (Segment.start right.Segment)
        let healedLeft = snapOffsetEndPositionOnly left boundary
        let healedRight = snapOffsetStartPositionOnly right boundary
        if certifiedHealedBoundary healedLeft healedRight offset options then
            Ok(healedLeft, healedRight)
        else Ok(left, right)

    let private healReversalOffsetBoundary
        (left: FUnhealedOffsetSegment)
        (right: FUnhealedOffsetSegment)
        offset options =
        if not (offsetBoundaryIsKnownReversal left right) then Error NonFinite
        else
            let boundary = Point.midpoint (Segment.finish left.Segment) (Segment.start right.Segment)
            let healedLeft = snapOffsetEndPositionOnly left boundary
            let healedRight = snapOffsetStartPositionOnly right boundary
            if certifiedHealedBoundary healedLeft healedRight offset options then
                Ok(healedLeft, healedRight)
            else Error NonFinite

    let private healOffsetBoundary left right offset options =
        match healReversalOffsetBoundary left right offset options with
        | Ok healed -> Ok healed
        | Error _ -> healSmoothOffsetBoundary left right offset options

    let rec private healAdjacentOffsetBoundariesLoop previous rest offset options healed =
        match rest with
        | [] -> Ok(List.rev (previous :: healed))
        | next :: remaining ->
            healOffsetBoundary previous next offset options
            |> Result.bind (fun (previous, next) ->
                healAdjacentOffsetBoundariesLoop
                    next remaining offset options (previous :: healed))

    let private healAdjacentOffsetBoundaries offsets offset options =
        match offsets with
        | []
        | [ _ ] -> Ok offsets
        | first :: second :: rest ->
            healOffsetBoundary first second offset options
            |> Result.bind (fun (first, second) ->
                healAdjacentOffsetBoundariesLoop second rest offset options [ first ])

    let private replaceLastOffset items replacement =
        match List.rev items with
        | [] -> []
        | _ :: rest -> List.rev (replacement :: rest)

    let private healWrappingOffsetBoundary offsets offset options =
        match offsets with
        | []
        | [ _ ] -> Ok offsets
        | first :: rest ->
            let last = List.last rest
            healOffsetBoundary last first offset options
            |> Result.map (fun (last, first) -> first :: replaceLastOffset rest last)

    let private healOffsetBoundaries offsets offset options closedValue =
        healAdjacentOffsetBoundaries offsets offset options
        |> Result.bind (fun healed ->
            if closedValue then healWrappingOffsetBoundary healed offset options
            else Ok healed)
        |> Result.map (List.map fUnhealedToGHealedOffsetSegment)

    let private combineTangentTurns incoming outgoing =
        match incoming, outgoing with
        | Clockwise, Clockwise
        | Clockwise, Straight
        | Straight, Clockwise -> Ok Clockwise
        | CounterClockwise, CounterClockwise
        | CounterClockwise, Straight
        | Straight, CounterClockwise -> Ok CounterClockwise
        | Straight, Straight -> Ok Straight
        | _ -> Error()

    let private localAperture aperture =
        if aperture <= 180.0<degree> then aperture
        else -(360.0<degree> - aperture)

    let private reversalExistingGap incomingDirection outgoingDirection turn =
        let oppositeOutgoing = Point.negate outgoingDirection
        match turn with
        | Clockwise -> Point.clockwiseAperture incomingDirection oppositeOutgoing |> localAperture
        | CounterClockwise -> Point.clockwiseAperture oppositeOutgoing incomingDirection |> localAperture
        | Straight
        | CouldNotMeasure -> 0.0<degree>

    let internal internalReversalTangentAdjustment
        incomingDirection outgoingDirection incomingTurn outgoingTurn
        (incomingChord: float<length>) (outgoingChord: float<length>) requiredGap =
        combineTangentTurns incomingTurn outgoingTurn
        |> Result.bind (fun turn ->
            let totalChord = incomingChord + outgoingChord
            if totalChord <= 0.0<length> || requiredGap < 0.0<degree> then Error()
            else
                let incomingWeight = outgoingChord / totalChord
                let outgoingWeight = incomingChord / totalChord
                let gap = reversalExistingGap incomingDirection outgoingDirection turn
                let missingGap = max 0.0<degree> (requiredGap - gap)
                let incomingMagnitude = incomingWeight * missingGap
                let outgoingMagnitude = outgoingWeight * missingGap
                match turn with
                | Clockwise ->
                    Ok { IncomingDegrees = -incomingMagnitude; OutgoingDegrees = outgoingMagnitude }
                | CounterClockwise ->
                    Ok { IncomingDegrees = incomingMagnitude; OutgoingDegrees = -outgoingMagnitude }
                | Straight
                | CouldNotMeasure -> Error())

    let private offsetSegmentSourceStart source =
        match source with
        | OffsetFromJoinFree joinFree -> Segment.start joinFree.Segment
        | OffsetFromStalledRun run -> Segment.start (List.head run).Segment

    let private offsetSegmentSourceEnd source =
        match source with
        | OffsetFromJoinFree joinFree -> Segment.finish joinFree.Segment
        | OffsetFromStalledRun run -> Segment.finish (List.last run).Segment

    let private directedLineIntersection
        (leftStart: Point<length>) (leftDirection: Point<1>)
        (rightStart: Point<length>) (rightDirection: Point<1>) =
        let delta = Point.displacement leftStart rightStart
        let determinant = Point.cross leftDirection rightDirection
        if abs determinant <= directionDeterminantTolerance then Error()
        else
            let leftT = Point.cross delta rightDirection / determinant
            let rightT = Point.cross delta leftDirection / determinant
            let point = Point.translate (Point.scale leftT leftDirection) leftStart
            if leftT >= 0.0<length> && rightT <= 0.0<length> && pointIsFinite point then Ok point
            else Error()

    let rec private lineSegmentsBetween points =
        match points with
        | []
        | [ _ ] -> []
        | first :: second :: rest ->
            let tail = lineSegmentsBetween (second :: rest)
            if Point.near pointTolerance first second then tail
            else Line(first, second) :: tail

    let private directedMiterJoin
        (left: GHealedOffsetSegment) (right: GHealedOffsetSegment)
        startPoint finish offset miterLimit =
        match directedLineIntersection
                startPoint left.NudgedEndTangentDirection
                finish right.NudgedStartTangentDirection with
        | Error _ -> Ok(lineSegmentsBetween [ startPoint; finish ])
        | Ok apex ->
            let corner = offsetSegmentSourceEnd left.Source
            let miterLength = Point.distance corner apex
            let offsetDistance = abs offset
            let withinLimit =
                offsetDistance <= pointTolerance || miterLength / offsetDistance <= miterLimit
            if withinLimit && pointIsFinite apex then
                Ok(lineSegmentsBetween [ startPoint; apex; finish ])
            else Ok(lineSegmentsBetween [ startPoint; finish ])

    let private roundJoin
        (left: GHealedOffsetSegment) (_right: GHealedOffsetSegment)
        startPoint finish offset =
        let radius = abs offset
        if radius <= pointTolerance then Ok(lineSegmentsBetween [ startPoint; finish ])
        else
            let corner = offsetSegmentSourceEnd left.Source
            let startRadius = Point.displacement corner startPoint
            let endRadius = Point.displacement corner finish
            let angle = signedAngle startRadius endRadius
            if abs angle <= angleToleranceDegrees then
                Ok(lineSegmentsBetween [ startPoint; finish ])
            else
                Ok [ Arc
                    { Start = startPoint
                      Radius = Point.create radius radius
                      XAxisRotation = 0.0<degree>
                      LargeArc = false
                      Sweep = angle > 0.0<degree>
                      End = finish } ]

    let private refinedSourceIntervalZone (source: EJoinFreeSegment) offset =
        let refined = source.Refined
        let preparedSpan = refined.PreparedTo - refined.PreparedFrom
        let fromParameter = refined.PreparedFrom + preparedSpan * Parameter.ratio source.RefinedFrom
        let toParameter = refined.PreparedFrom + preparedSpan * Parameter.ratio source.RefinedTo
        offsetCurvatureZone refined.Prepared.Segment offset ((fromParameter + toParameter) / 2.0)

    let private eSegmentsHaveCrossSourceReversalBoundary
        (left: EJoinFreeSegment) (right: EJoinFreeSegment) offset =
        left.Refined.Prepared.SourceSubpathIndex = right.Refined.Prepared.SourceSubpathIndex
        && left.Refined.Prepared.SourceSegmentIndex <> right.Refined.Prepared.SourceSegmentIndex
        && offsetCurvatureZonesFormReversalBoundary
            (Some(refinedSourceIntervalZone left offset))
            (Some(refinedSourceIntervalZone right offset))

    let private setOffsetSegmentSourceStartReversal (offset: FUnhealedOffsetSegment) =
        match offset.Source with
        | OffsetFromJoinFree source ->
            let boundary = ReversalBoundary(eJoinFreeSourceEndpointCurvature source SegmentStart)
            { offset with Source = OffsetFromJoinFree { source with StartBoundary = boundary } }
        | OffsetFromStalledRun _ -> offset

    let private setOffsetSegmentSourceEndReversal (offset: FUnhealedOffsetSegment) =
        match offset.Source with
        | OffsetFromJoinFree source ->
            let boundary = ReversalBoundary(eJoinFreeSourceEndpointCurvature source SegmentEnd)
            { offset with Source = OffsetFromJoinFree { source with EndBoundary = boundary } }
        | OffsetFromStalledRun _ -> offset

    let private markAdjacentCrossSourceReversalBoundary
        (left: FUnhealedOffsetSegment) (right: FUnhealedOffsetSegment) offset =
        match left.Source, right.Source with
        | OffsetFromJoinFree leftSource, OffsetFromJoinFree rightSource
            when eSegmentsHaveCrossSourceReversalBoundary leftSource rightSource offset ->
            setOffsetSegmentSourceEndReversal left,
            setOffsetSegmentSourceStartReversal right
        | _ -> left, right

    let rec private markLinearCrossSourceReversalBoundariesLoop
        previous rest offset marked =
        match rest with
        | [] -> List.rev (previous :: marked)
        | next :: remaining ->
            let previous, next = markAdjacentCrossSourceReversalBoundary previous next offset
            markLinearCrossSourceReversalBoundariesLoop next remaining offset (previous :: marked)

    let private markLinearCrossSourceReversalBoundaries offsets offset =
        match offsets with
        | []
        | [ _ ] -> offsets
        | first :: rest ->
            markLinearCrossSourceReversalBoundariesLoop first rest offset []

    let private markCrossSourceReversalBoundaries offsets offset closedValue =
        let marked = markLinearCrossSourceReversalBoundaries offsets offset
        match closedValue, marked with
        | true, first :: second :: rest ->
            let last = List.last (first :: second :: rest)
            let last, first = markAdjacentCrossSourceReversalBoundary last first offset
            first :: replaceLastOffset (second :: rest) last
        | _ -> marked

    let private assertContinuousOffsetTangentBoundary left right healAngle =
        match segmentDiameter left, segmentDiameter right with
        | Ok leftDiameter, Ok rightDiameter
            when leftDiameter >= stableTangentAssertionDiameter
              && rightDiameter >= stableTangentAssertionDiameter ->
            match unitTangent left 1.0<parameter>, unitTangent right 0.0<parameter> with
            | Ok leftTangent, Ok rightTangent ->
                let angle = abs (signedAngle leftTangent rightTangent)
                if angle <= healAngle || 180.0<degree> - angle <= healAngle then Ok()
                else Error(DegenerateTangent 1.0<parameter>)
            | Error error, _
            | _, Error error -> Error error
        | Ok _, Ok _ -> Ok()
        | Error error, _
        | _, Error error -> Error error

    let private assertSmoothOffsetBoundary
        (left: FUnhealedOffsetSegment) (right: FUnhealedOffsetSegment) healAngle =
        if Segment.finish left.Segment <> Segment.start right.Segment then Error NonFinite
        elif offsetSegmentSourceEnd left.Source <> offsetSegmentSourceStart right.Source then Ok()
        elif offsetBoundaryIsKnownReversal left right then Ok()
        else assertContinuousOffsetTangentBoundary left.Segment right.Segment healAngle

    let rec private assertSmoothOffsetPostconditions offsets healAngle =
        match offsets with
        | []
        | [ _ ] -> Ok()
        | first :: second :: rest ->
            assertSmoothOffsetBoundary first second healAngle
            |> Result.bind (fun _ -> assertSmoothOffsetPostconditions (second :: rest) healAngle)

    let private splitSynchronizedSourceAtMidpoint (source: SynchronizedSourceSegment) =
        Segment.split source.Segment 0.5<parameter>
        |> Result.mapError PathError
        |> Result.map (fun (left, right) ->
            let midpoint = source.PreparedFrom + (source.PreparedTo - source.PreparedFrom) / 2.0
            let ordinary = { Inner = Ordinary; Outer = Ordinary }
            { source with
                PreparedTo = midpoint
                Segment = left
                EndBoundary = ordinary },
            { source with
                PreparedFrom = midpoint
                Segment = right
                StartBoundary = ordinary })

    let rec private splitSynchronizedDoubleReversalSegmentsLoop
        (sources: SynchronizedSourceSegment list)
        (distances: OffsetDistances)
        (split: SynchronizedSourceSegment list) =
        match sources with
        | [] -> Ok(List.rev split)
        | first :: rest ->
            let innerDouble =
                boundaryReachesOffsetRadius first.StartBoundary.Inner distances.Inner
                && boundaryReachesOffsetRadius first.EndBoundary.Inner distances.Inner
            let outerDouble =
                boundaryReachesOffsetRadius first.StartBoundary.Outer distances.Outer
                && boundaryReachesOffsetRadius first.EndBoundary.Outer distances.Outer
            if segmentIsBezier first.Segment && (innerDouble || outerDouble) then
                splitSynchronizedSourceAtMidpoint first
                |> Result.bind (fun (left, right) ->
                    splitSynchronizedDoubleReversalSegmentsLoop
                        rest distances (right :: left :: split))
            else
                splitSynchronizedDoubleReversalSegmentsLoop rest distances (first :: split)

    let private splitSynchronizedDoubleReversalSegments sources distances =
        splitSynchronizedDoubleReversalSegmentsLoop sources distances []

    let private joinFreePortions subpath options =
        match Subpath.segments subpath with
        | [] -> Ok []
        | segments ->
            splitJoinFreePortions segments options
            |> Result.map (fun portions ->
                markClosedJoinFreePortion portions (Subpath.isClosed subpath)
                |> indexJoinFreePortions)

    let private buildSynchronizedOffsetPortion
        (portion: JoinFreePortion)
        (distances: OffsetDistances)
        (options: Options)
        : Result<SynchronizedOffsetSegmentsBuild, Error> =
        let classified =
            preparedSegments portion.Subpath 0
            |> fun prepared ->
                classifyPreparedSegmentsForBothOffsets
                    prepared distances options.StalledOffsetDiameter
        refineSynchronizedClassifiedSegments classified distances options.StalledOffsetDiameter
        |> Result.bind (fun sources -> splitSynchronizedDoubleReversalSegments sources distances)
        |> Result.bind (fun sources ->
            offsetSynchronizedSourceSegments sources distances options portion.Index)
        |> Result.bind (fun unhealed ->
            let innerUnhealed =
                markCrossSourceReversalBoundaries
                    unhealed.InnerOffsets distances.Inner portion.Closed
            let outerUnhealed =
                markCrossSourceReversalBoundaries
                    unhealed.OuterOffsets distances.Outer portion.Closed
            match healOffsetBoundaries innerUnhealed distances.Inner options portion.Closed,
                  healOffsetBoundaries outerUnhealed distances.Outer options portion.Closed with
            | Ok innerHealed, Ok outerHealed ->
                match assertSmoothOffsetPostconditions
                        (gHealedToFUnhealedOffsetSegments innerHealed)
                        options.TangentHealAngleDegrees,
                      assertSmoothOffsetPostconditions
                        (gHealedToFUnhealedOffsetSegments outerHealed)
                        options.TangentHealAngleDegrees with
                | Ok _, Ok _ ->
                    Ok
                        { InnerOffsets = innerHealed
                          OuterOffsets = outerHealed
                          Correspondences = unhealed.Correspondences
                          Portions =
                            [ { PortionIndex = portion.Index
                                Inner = innerHealed
                                Outer = outerHealed } ] }
                | Error error, _
                | _, Error error -> Error error
            | Error error, _
            | _, Error error -> Error error)

    let rec private buildSynchronizedOffsetPortionsLoop
        portions distances options innerOffsets outerOffsets correspondences healedPortions =
        match portions with
        | [] ->
            Ok
                { InnerOffsets = List.rev innerOffsets
                  OuterOffsets = List.rev outerOffsets
                  Correspondences = List.rev correspondences
                  Portions = List.rev healedPortions }
        | first :: rest ->
            buildSynchronizedOffsetPortion first distances options
            |> Result.bind (fun built ->
                buildSynchronizedOffsetPortionsLoop
                    rest distances options
                    (List.rev built.InnerOffsets @ innerOffsets)
                    (List.rev built.OuterOffsets @ outerOffsets)
                    (List.rev built.Correspondences @ correspondences)
                    (List.rev built.Portions @ healedPortions))

    let private buildSynchronizedOffsetSegments subpath distances options =
        joinFreePortions subpath options
        |> Result.bind (fun portions ->
            buildSynchronizedOffsetPortionsLoop portions distances options [] [] [] [])

    let private parametricJoinSegments
        (left: GHealedOffsetSegment) (right: GHealedOffsetSegment) offset join =
        let startPoint = Segment.finish left.Segment
        let finish = Segment.start right.Segment
        if Point.near pointTolerance startPoint finish then Ok []
        else
            match join with
            | Bevel -> Ok(lineSegmentsBetween [ startPoint; finish ])
            | Miter miterLimit ->
                directedMiterJoin left right startPoint finish offset miterLimit
            | Round -> roundJoin left right startPoint finish offset

    let private offsetSourceEndpointUnitTangent source endpoint =
        match source with
        | OffsetFromJoinFree joinFree -> unitTangentAtEndpoint joinFree.Segment endpoint
        | OffsetFromStalledRun run ->
            match endpoint, run with
            | SegmentStart, first :: _ -> unitTangentAtEndpoint first.Segment endpoint
            | SegmentEnd, _ :: _ -> unitTangentAtEndpoint (List.last run).Segment endpoint
            | _ -> Error(DegenerateTangent 0.0<parameter>)

    let private offsetPortionJoinBoundary
        (left: GHealedOffsetSegment list) (right: GHealedOffsetSegment list) =
        match List.tryLast left, right with
        | Some previous, next :: _ ->
            Ok(Segment.finish previous.Segment, Segment.start next.Segment)
        | _ -> Error(DegenerateTangent 0.0<parameter>)

    let private joinBetweenOffsetPortions
        (left: GHealedOffsetSegment list) (right: GHealedOffsetSegment list)
        offset join =
        match List.tryLast left, right with
        | Some previous, next :: _ -> parametricJoinSegments previous next offset join
        | _ -> Ok []

    let private joinIsGeometricallyReversed
        (left: GHealedOffsetSegment list) (right: GHealedOffsetSegment list)
        joinStart joinEnd =
        match List.tryLast left, right with
        | Some previous, next :: _ ->
            match offsetSourceEndpointUnitTangent previous.Source SegmentEnd,
                  offsetSourceEndpointUnitTangent next.Source SegmentStart with
            | Ok incoming, Ok outgoing ->
                let averageDirection = Point.add incoming outgoing
                let joinChord = Point.displacement joinStart joinEnd
                Ok(Point.dot averageDirection joinChord < 0.0<length>)
            | Error error, _
            | _, Error error -> Error error
        | _ -> Error(DegenerateTangent 0.0<parameter>)

    let private synchronizedJoinCorrespondence
        (left: SynchronizedHealedPortion)
        (right: SynchronizedHealedPortion)
        (distances: OffsetDistances) join =
        match joinBetweenOffsetPortions left.Inner right.Inner distances.Inner join,
              joinBetweenOffsetPortions left.Outer right.Outer distances.Outer join,
              offsetPortionJoinBoundary left.Inner right.Inner,
              offsetPortionJoinBoundary left.Outer right.Outer with
        | Ok innerJoin, Ok outerJoin, Ok(innerStart, innerEnd), Ok(outerStart, outerEnd) ->
            match joinIsGeometricallyReversed left.Inner right.Inner innerStart innerEnd,
                  joinIsGeometricallyReversed left.Outer right.Outer outerStart outerEnd with
            | Ok innerReversed, Ok outerReversed ->
                Ok
                    { AfterPortionIndex = left.PortionIndex
                      Inner = innerJoin
                      Outer = outerJoin
                      InnerReversed = innerReversed
                      OuterReversed = outerReversed
                      InnerStart = innerStart
                      InnerEnd = innerEnd
                      OuterStart = outerStart
                      OuterEnd = outerEnd }
            | Error error, _
            | _, Error error -> Error error
        | Error error, _, _, _
        | _, Error error, _, _
        | _, _, Error error, _
        | _, _, _, Error error -> Error error

    let rec private synchronizedJoinCorrespondencesLoop
        first previous rest distances join closedValue joined =
        match rest with
        | [] ->
            if closedValue then
                synchronizedJoinCorrespondence previous first distances join
                |> Result.map (fun closing -> List.rev (closing :: joined))
            else Ok(List.rev joined)
        | next :: remaining ->
            synchronizedJoinCorrespondence previous next distances join
            |> Result.bind (fun correspondence ->
                synchronizedJoinCorrespondencesLoop
                    first next remaining distances join closedValue
                    (correspondence :: joined))

    let private synchronizedJoinCorrespondences portions distances join closedValue =
        match portions with
        | []
        | [ _ ] -> Ok []
        | first :: second :: rest ->
            synchronizedJoinCorrespondencesLoop
                first first (second :: rest) distances join closedValue []

    let private segmentWithStart segment startPoint = Segment.withStart startPoint segment

    let private segmentWithEnd segment finish = Segment.withFinish finish segment

    let rec private earliestInteriorAdjacentIntersection
        (intersections: SegmentIntersection list)
        (best: SegmentIntersection option) =
        match intersections with
        | [] -> best
        | intersection :: rest ->
            let interior =
                intersection.LeftT > 0.0<parameter>
                && intersection.LeftT < 1.0<parameter> - adjacentLoopEndpointParameterTolerance
                && intersection.RightT > adjacentLoopEndpointParameterTolerance
                && intersection.RightT < 1.0<parameter>
            let nextBest =
                match interior, best with
                | false, _ -> best
                | true, None -> Some intersection
                | true, Some current
                    when intersection.LeftT < current.LeftT
                      || (intersection.LeftT = current.LeftT
                          && intersection.RightT > current.RightT) -> Some intersection
                | _ -> best
            earliestInteriorAdjacentIntersection rest nextBest

    let private shortCircuitAdjacentOffsetSegmentLoopWithParameters left right =
        Intersections.segment left right
        |> Result.mapError PathError
        |> Result.bind (fun intersections ->
            match earliestInteriorAdjacentIntersection intersections None with
            | None -> Ok(left, 1.0<parameter>, right, 0.0<parameter>)
            | Some intersection ->
                match Segment.betweenInside left 0.0<parameter> intersection.LeftT,
                      Segment.betweenInside right intersection.RightT 1.0<parameter> with
                | Ok retainedLeft, Ok retainedRight ->
                    Ok(
                        segmentWithEnd retainedLeft intersection.Point,
                        intersection.LeftT,
                        segmentWithStart retainedRight intersection.Point,
                        intersection.RightT)
                | Error error, _
                | _, Error error -> Error(PathError error))

    let internal internalShortCircuitAdjacentOffsetSegmentLoop left right =
        shortCircuitAdjacentOffsetSegmentLoopWithParameters left right
        |> Result.map (fun (left, _, right, _) -> left, right)

    let rec private synchronizedSideSourceOffsetCount source =
        match source with
        | RefinableSideSource _
        | StalledSideSource _ -> 1
        | SplitSideSource(left, right) ->
            synchronizedSideSourceOffsetCount left + synchronizedSideSourceOffsetCount right

    let rec private synchronizedMaxGranularityTraceAreas
        portionIndex correspondenceIndex innerSource outerSource
        innerSegments outerSegments =
        match innerSource, outerSource with
        | SplitSideSource(innerLeft, innerRight), SplitSideSource(outerLeft, outerRight) ->
            let innerLeftCount = synchronizedSideSourceOffsetCount innerLeft
            let outerLeftCount = synchronizedSideSourceOffsetCount outerLeft
            synchronizedMaxGranularityTraceAreas
                portionIndex correspondenceIndex innerLeft outerLeft
                (List.take innerLeftCount innerSegments)
                (List.take outerLeftCount outerSegments)
            @ synchronizedMaxGranularityTraceAreas
                portionIndex correspondenceIndex innerRight outerRight
                (List.skip innerLeftCount innerSegments)
                (List.skip outerLeftCount outerSegments)
        | _ ->
            [ { PortionIndex = portionIndex
                CorrespondenceIndex = correspondenceIndex
                InnerSegments = innerSegments
                OuterSegments = outerSegments } ]

    let rec private synchronizedOffsetTraceAreas
        (correspondences: OffsetCorrespondence list)
        (innerOffsets: GHealedOffsetSegment list)
        (outerOffsets: GHealedOffsetSegment list)
        (traced: SynchronizedOffsetTraceArea list) =
        match correspondences with
        | [] -> List.rev traced
        | first :: rest ->
            let inner = List.take first.InnerOffsetCount innerOffsets
            let outer = List.take first.OuterOffsetCount outerOffsets
            let areas =
                synchronizedMaxGranularityTraceAreas
                    first.PortionIndex first.CorrespondenceIndex
                    first.Inner first.Outer
                    (inner |> List.map (fun offset -> offset.Segment))
                    (outer |> List.map (fun offset -> offset.Segment))
            synchronizedOffsetTraceAreas
                rest
                (List.skip first.InnerOffsetCount innerOffsets)
                (List.skip first.OuterOffsetCount outerOffsets)
                (List.rev areas @ traced)

    let private synchronizedSideSourceIsStalled source =
        match source with
        | StalledSideSource _ -> true
        | RefinableSideSource _
        | SplitSideSource _ -> false

    let rec private synchronizedSideSourceTraceLeaves source =
        match source with
        | RefinableSideSource joinFree ->
            let refined = joinFree.Refined
            let preparedSpan = refined.PreparedTo - refined.PreparedFrom
            [ { SourceSegmentIndex = refined.Prepared.SourceSegmentIndex
                PreparedFrom =
                    refined.PreparedFrom + preparedSpan * Parameter.ratio joinFree.RefinedFrom
                PreparedTo =
                    refined.PreparedFrom + preparedSpan * Parameter.ratio joinFree.RefinedTo
                Generation = joinFree.Generation } ]
        | StalledSideSource run ->
            run
            |> List.map (fun stalled ->
                { SourceSegmentIndex = stalled.Prepared.SourceSegmentIndex
                  PreparedFrom = stalled.PreparedFrom
                  PreparedTo = stalled.PreparedTo
                  Generation = 0 })
        | SplitSideSource(left, right) ->
            synchronizedSideSourceTraceLeaves left @ synchronizedSideSourceTraceLeaves right

    let private synchronizedOffsetTraceCorrespondence
        (correspondence: OffsetCorrespondence)
        : SynchronizedOffsetTraceCorrespondence =
        { PortionIndex = correspondence.PortionIndex
          CorrespondenceIndex = correspondence.CorrespondenceIndex
          InnerStalled = synchronizedSideSourceIsStalled correspondence.Inner
          OuterStalled = synchronizedSideSourceIsStalled correspondence.Outer
          InnerLeaves = synchronizedSideSourceTraceLeaves correspondence.Inner
          OuterLeaves = synchronizedSideSourceTraceLeaves correspondence.Outer }

    let internal internalSynchronizedOffsetTrace subpath innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedOffsetSegments
                normalized { Inner = innerOffset; Outer = outerOffset } options)
        |> Result.map (fun (build: SynchronizedOffsetSegmentsBuild) ->
            build.Correspondences |> List.map synchronizedOffsetTraceCorrespondence)

    let internal internalSynchronizedOffsetAreaTrace subpath innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedOffsetSegments
                normalized { Inner = innerOffset; Outer = outerOffset } options)
        |> Result.map (fun (build: SynchronizedOffsetSegmentsBuild) ->
            synchronizedOffsetTraceAreas
                build.Correspondences build.InnerOffsets build.OuterOffsets [])

    let private reverseSegments segments =
        segments |> List.rev |> List.map Segment.reverse

    let private strokeCapSegments center (tangent: Point<1>) radius cap atEnd =
        let normal = Point.rotateCounterclockwise tangent
        let positive = Point.translate (Point.scale radius normal) center
        let negative = Point.translate (Point.scale -radius normal) center
        match cap with
        | Butt ->
            if atEnd then Ok(lineSegmentsBetween [ positive; negative ])
            else Ok(lineSegmentsBetween [ negative; positive ])
        | Square ->
            let extension = Point.scale (if atEnd then radius else -radius) tangent
            let positiveExtended = Point.translate extension positive
            let negativeExtended = Point.translate extension negative
            if atEnd then
                Ok(lineSegmentsBetween [ positive; positiveExtended; negativeExtended; negative ])
            else
                Ok(lineSegmentsBetween [ negative; negativeExtended; positiveExtended; positive ])
        | RoundCap ->
            let startPoint, finish = if atEnd then positive, negative else negative, positive
            Ok [ Arc
                { Start = startPoint
                  Radius = Point.create radius radius
                  XAxisRotation = 0.0<degree>
                  LargeArc = false
                  Sweep = true
                  End = finish } ]

    let private strokeEndCap source radius cap =
        match List.tryLast (Subpath.segments source) with
        | None -> Error(PathError EmptySubpath)
        | Some last ->
            unitTangent last 1.0<parameter>
            |> Result.bind (fun tangent ->
                strokeCapSegments (Subpath.finish source) tangent radius cap true)

    let private strokeStartCap source radius cap =
        match Subpath.segments source with
        | [] -> Error(PathError EmptySubpath)
        | first :: _ ->
            unitTangent first 0.0<parameter>
            |> Result.bind (fun tangent ->
                strokeCapSegments (Subpath.start source) tangent radius cap false)

    let private zeroLengthRoundStrokePath center radius =
        let right = Point.translate (Point.create radius 0.0<length>) center
        let left = Point.translate (Point.create -radius 0.0<length>) center
        let radial = Point.create radius radius
        let segments =
            [ Arc { Start = right; Radius = radial; XAxisRotation = 0.0<degree>
                    LargeArc = false; Sweep = true; End = left }
              Arc { Start = left; Radius = radial; XAxisRotation = 0.0<degree>
                    LargeArc = false; Sweep = true; End = right } ]
        Subpath.create segments
        |> Result.mapError PathError
        |> Result.bind (fun outline -> Subpath.setClosed true outline |> Result.mapError PathError)
        |> Result.map Path.singleton

    let private zeroLengthSquareStrokePath center radius =
        let topLeft = Point.translate (Point.create -radius -radius) center
        let topRight = Point.translate (Point.create radius -radius) center
        let bottomRight = Point.translate (Point.create radius radius) center
        let bottomLeft = Point.translate (Point.create -radius radius) center
        lineSegmentsBetween [ topLeft; topRight; bottomRight; bottomLeft; topLeft ]
        |> Subpath.create
        |> Result.mapError PathError
        |> Result.bind (fun outline -> Subpath.setClosed true outline |> Result.mapError PathError)
        |> Result.map Path.singleton

    let private zeroLengthStrokePath subpath radius cap =
        let center = Subpath.start subpath
        match cap with
        | Butt -> Ok Path.empty
        | RoundCap -> zeroLengthRoundStrokePath center radius
        | Square -> zeroLengthSquareStrokePath center radius

    let rec private collectOuterStalledTraceRun pieces prepared stalledTo =
        match pieces with
        | next :: rest
            when next.Prepared = prepared && next.OuterStatus = SideStalled ->
            collectOuterStalledTraceRun rest prepared next.PreparedTo
        | _ -> stalledTo, pieces

    let rec private synchronizedOffsetSourceTracePieces pieces refinedPieceIndex traced =
        match pieces with
        | [] -> List.rev traced
        | first :: rest ->
            let sourceSegmentIndex = first.Prepared.SourceSegmentIndex
            match first.OuterStatus with
            | SideStalled ->
                let stalledTo, remaining =
                    collectOuterStalledTraceRun rest first.Prepared first.PreparedTo
                match preparedSegmentBetween first.Prepared first.PreparedFrom stalledTo with
                | Ok stalledSegment ->
                    synchronizedOffsetSourceTracePieces
                        remaining (refinedPieceIndex + 1)
                        (OffsetSourceTraceStalled(sourceSegmentIndex, stalledSegment) :: traced)
                | Error _ ->
                    synchronizedOffsetSourceTracePieces
                        remaining (refinedPieceIndex + 1) traced
            | SideNotStalled ->
                let startBoundary = first.StartBoundary.Outer
                let endBoundary = first.EndBoundary.Outer
                let trace =
                    OffsetSourceTraceDRefined(
                        sourceSegmentIndex,
                        refinedPieceIndex,
                        first.PreparedFrom,
                        first.PreparedTo,
                        first.Segment,
                        startBoundary,
                        endBoundary,
                        boundaryIsReversal startBoundary,
                        boundaryIsReversal endBoundary)
                synchronizedOffsetSourceTracePieces
                    rest (refinedPieceIndex + 1) (trace :: traced)

    let rec private synchronizedOffsetSourceTracePortions
        (portions: JoinFreePortion list)
        (offset: float<length>)
        (options: Options)
        (traced: OffsetSourceTracePortion list) =
        match portions with
        | [] -> Ok(List.rev traced)
        | first :: rest ->
            let distances: OffsetDistances = { Inner = 0.0<length>; Outer = offset }
            let classified =
                preparedSegments first.Subpath 0
                |> fun prepared ->
                    classifyPreparedSegmentsForBothOffsets
                        prepared distances options.StalledOffsetDiameter
            refineSynchronizedClassifiedSegments classified distances options.StalledOffsetDiameter
            |> Result.bind (fun sources ->
                splitSynchronizedDoubleReversalSegments sources distances)
            |> Result.bind (fun sources ->
                let trace =
                    { Index = first.Index
                      Subpath = first.Subpath
                      Pieces = synchronizedOffsetSourceTracePieces sources 0 [] }
                synchronizedOffsetSourceTracePortions
                    rest offset options (trace :: traced))

    let internal internalOffsetSourceTrace subpath offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized -> joinFreePortions normalized options)
        |> Result.bind (fun portions ->
            synchronizedOffsetSourceTracePortions portions offset options [])

    let private indexedJoinPreimageSegments
        (segments: Segment list) afterPortionIndex side reversedValue
        : HPreimageSegment list =
        segments
        |> List.mapi (fun index segment ->
            { Segment = segment
              Source = JoinPreimage(afterPortionIndex, side, index, reversedValue) })

    let rec private assemblePreimageSegments
        (portions: SynchronizedHealedPortion list)
        (joins: OffsetJoinCorrespondence list)
        side
        : HPreimageSegment list =
        match portions with
        | [] -> []
        | first :: rest ->
            let portion = if side = Inner then first.Inner else first.Outer
            let portionSegments =
                portion
                |> List.map (fun (offset: GHealedOffsetSegment) ->
                    { Segment = offset.Segment; Source = HealedPreimage offset })
            match joins with
            | [] -> portionSegments @ assemblePreimageSegments rest [] side
            | join :: remainingJoins ->
                let rawJoinSegments, joinReversed =
                    if side = Inner then join.Inner, join.InnerReversed
                    else join.Outer, join.OuterReversed
                let joinSegments =
                    indexedJoinPreimageSegments
                        rawJoinSegments join.AfterPortionIndex side joinReversed
                portionSegments
                @ joinSegments
                @ assemblePreimageSegments rest remainingJoins side

    let private assemblePreimageSubpath
        (portions: SynchronizedHealedPortion list)
        (joins: OffsetJoinCorrespondence list)
        side closedValue
        : HPreimageSubpath =
        { Segments = assemblePreimageSegments portions joins side
          Closed = closedValue
          Side = side }

    let private intervalParameter
        (fromParameter: float<parameter>)
        (toParameter: float<parameter>)
        (local: float<parameter>)
        : float<parameter> =
        fromParameter + (toParameter - fromParameter) * Parameter.ratio local

    let private adjacentEndpointOverlap (overlaps: RawOverlap list) =
        overlaps
        |> List.tryFind (fun overlap ->
            let rightStart = min overlap.RightFrom overlap.RightTo
            overlap.LeftFrom < 1.0<parameter>
            && overlap.LeftTo >= 1.0<parameter> - adjacentLoopEndpointParameterTolerance
            && rightStart <= adjacentLoopEndpointParameterTolerance
            && max overlap.RightFrom overlap.RightTo > 0.0<parameter>)

    let private cullAdjacentOffsetSegmentOverlap
        (left: ICulledOffsetSegment)
        (right: ICulledOffsetSegment) =
        OverlapDetection.detect left.Segment right.Segment 1.0e-9<length>
        |> Result.mapError PathError
        |> Result.bind (fun found ->
            match adjacentEndpointOverlap found with
            | None -> Ok(left, Some right)
            | Some overlap ->
                let rightEnd = max overlap.RightFrom overlap.RightTo
                match Segment.point right.Segment rightEnd,
                      Segment.betweenInside left.Segment 0.0<parameter> overlap.LeftFrom with
                | Ok shared, Ok leftSegment ->
                    let retainedLeft =
                        { left with
                            Segment = segmentWithEnd leftSegment shared
                            PreimageTo =
                                intervalParameter
                                    left.PreimageFrom left.PreimageTo overlap.LeftFrom }
                    if rightEnd >= 1.0<parameter> - adjacentLoopEndpointParameterTolerance then
                        Ok(retainedLeft, None)
                    else
                        Segment.betweenInside right.Segment rightEnd 1.0<parameter>
                        |> Result.mapError PathError
                        |> Result.map (fun rightSegment ->
                            retainedLeft,
                            Some
                                { right with
                                    Segment = segmentWithStart rightSegment shared
                                    PreimageFrom =
                                        intervalParameter
                                            right.PreimageFrom right.PreimageTo rightEnd })
                | Error error, _
                | _, Error error -> Error(PathError error))

    let private cullOffsetSegmentLoop
        (left: ICulledOffsetSegment)
        (right: ICulledOffsetSegment) =
        if hPreimageIsReversed left.Preimage = hPreimageIsReversed right.Preimage then
            Ok(left, Some right)
        else
            match shortCircuitAdjacentOffsetSegmentLoopWithParameters left.Segment right.Segment with
            | Error(PathError OverlappingSegments) ->
                cullAdjacentOffsetSegmentOverlap left right
            | Error error -> Error error
            | Ok(leftSegment, leftTo, rightSegment, rightFrom) ->
                Ok(
                    { left with
                        Segment = leftSegment
                        PreimageTo =
                            intervalParameter left.PreimageFrom left.PreimageTo leftTo },
                    Some
                        { right with
                            Segment = rightSegment
                            PreimageFrom =
                                intervalParameter right.PreimageFrom right.PreimageTo rightFrom })

    let rec private cullAdjacentOffsetSegmentLoopsLoop
        (previous: ICulledOffsetSegment)
        (rest: ICulledOffsetSegment list)
        (culled: ICulledOffsetSegment list) =
        match rest with
        | [] -> Ok(List.rev (previous :: culled))
        | next :: remaining ->
            cullOffsetSegmentLoop previous next
            |> Result.bind (fun (previous, next) ->
                match next with
                | Some next ->
                    cullAdjacentOffsetSegmentLoopsLoop
                        next remaining (previous :: culled)
                | None ->
                    cullAdjacentOffsetSegmentLoopsLoop previous remaining culled)

    let private cullAdjacentOffsetSegmentLoops (segments: ICulledOffsetSegment list) =
        match segments with
        | []
        | [ _ ] -> Ok segments
        | first :: second :: rest ->
            cullOffsetSegmentLoop first second
            |> Result.bind (fun (first, second) ->
                match second with
                | Some second ->
                    cullAdjacentOffsetSegmentLoopsLoop second rest [ first ]
                | None -> cullAdjacentOffsetSegmentLoopsLoop first rest [])

    let private replaceLastCulledOffset
        (segments: ICulledOffsetSegment list)
        (replacement: ICulledOffsetSegment) =
        match List.rev segments with
        | [] -> []
        | _ :: rest -> List.rev (replacement :: rest)

    let private cullWrappingOffsetSegmentLoop (segments: ICulledOffsetSegment list) =
        match segments with
        | []
        | [ _ ] -> Ok segments
        | first :: rest ->
            let last = List.last rest
            cullOffsetSegmentLoop last first
            |> Result.map (fun (last, first) ->
                match first with
                | Some first -> first :: replaceLastCulledOffset rest last
                | None -> replaceLastCulledOffset rest last)

    let private cullAdjacentPreimageLoops
        (subpath: HPreimageSubpath)
        : Result<ICulledOffsetSubpath, Error> =
        let segments: ICulledOffsetSegment list =
            subpath.Segments
            |> List.map (fun preimage ->
                { Segment = preimage.Segment
                  Preimage = preimage
                  PreimageFrom = 0.0<parameter>
                  PreimageTo = 1.0<parameter> })
        cullAdjacentOffsetSegmentLoops segments
        |> Result.bind (fun segments ->
            if subpath.Closed then cullWrappingOffsetSegmentLoop segments
            else Ok segments)
        |> Result.map (fun (segments: ICulledOffsetSegment list) ->
            ({ Segments = segments; Closed = subpath.Closed; Side = subpath.Side }
                : ICulledOffsetSubpath))

    let private culledOffsetSubpathSegments (subpath: ICulledOffsetSubpath) =
        subpath.Segments |> List.map (fun segment -> segment.Segment)

    let private subpathFromSynchronizedSegments
        (segments: Segment list) closedValue (tolerance: float<length>) =
        match segments with
        | [] -> Error(DegenerateTangent 0.0<parameter>)
        | _ ->
            let policy = WiggleWith tolerance
            Subpath.createWith policy segments
            |> Result.mapError PathError
            |> Result.bind (fun subpath ->
                Subpath.setClosedWith policy closedValue subpath
                |> Result.mapError PathError)

    let private buildSynchronizedUntrimmed
        (subpath: Subpath)
        (innerOffset: float<length>) (outerOffset: float<length>)
        (options: Options)
        : Result<SynchronizedUntrimmedBuild, Error> =
        let distances: OffsetDistances = { Inner = innerOffset; Outer = outerOffset }
        let closedValue = Subpath.isClosed subpath
        match Subpath.segments subpath with
        | [] ->
            let startPoint = Subpath.start subpath
            Ok
                { Inner = Subpath.empty startPoint
                  Outer = Subpath.empty startPoint
                  InnerCulled = { Segments = []; Closed = closedValue; Side = Inner }
                  OuterCulled = { Segments = []; Closed = closedValue; Side = Outer }
                  Correspondences = []
                  Portions = []
                  JoinCorrespondences = [] }
        | _ ->
            buildSynchronizedOffsetSegments subpath distances options
            |> Result.bind (fun (build: SynchronizedOffsetSegmentsBuild) ->
                synchronizedJoinCorrespondences
                    build.Portions distances options.Join closedValue
                |> Result.bind (fun (joinCorrespondences: OffsetJoinCorrespondence list) ->
                    let innerPreimage =
                        assemblePreimageSubpath
                            build.Portions joinCorrespondences Inner closedValue
                    let outerPreimage =
                        assemblePreimageSubpath
                            build.Portions joinCorrespondences Outer closedValue
                    match cullAdjacentPreimageLoops innerPreimage,
                          cullAdjacentPreimageLoops outerPreimage with
                    | Ok innerCulled, Ok outerCulled ->
                        match subpathFromSynchronizedSegments
                                (culledOffsetSubpathSegments innerCulled)
                                closedValue options.Fitting.Tolerance,
                              subpathFromSynchronizedSegments
                                (culledOffsetSubpathSegments outerCulled)
                                closedValue options.Fitting.Tolerance with
                        | Ok inner, Ok outer ->
                            Ok
                                { Inner = inner
                                  Outer = outer
                                  InnerCulled = innerCulled
                                  OuterCulled = outerCulled
                                  Correspondences = build.Correspondences
                                  Portions = build.Portions
                                  JoinCorrespondences = joinCorrespondences }
                        | Error error, _
                        | _, Error error -> Error error
                    | Error error, _
                    | _, Error error -> Error error))

    let private buildSingleOffsetUntrimmed subpath offset options =
        buildSynchronizedUntrimmed subpath 0.0<length> offset options
        |> Result.map (fun (build: SynchronizedUntrimmedBuild) ->
            { Subpath = build.Outer
              ZeroSource = build.Inner
              Culled = build.OuterCulled
              Correspondences = build.Correspondences
              Portions = build.Portions
              JoinCorrespondences = build.JoinCorrespondences })

    let internal internalSynchronizedJoinTrace subpath innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedUntrimmed normalized innerOffset outerOffset options)
        |> Result.map (fun (build: SynchronizedUntrimmedBuild) ->
            build.JoinCorrespondences
            |> List.map (fun (correspondence: OffsetJoinCorrespondence) ->
                { AfterPortionIndex = correspondence.AfterPortionIndex
                  InnerSegments = correspondence.Inner
                  OuterSegments = correspondence.Outer
                  InnerReversed = correspondence.InnerReversed
                  OuterReversed = correspondence.OuterReversed }))

    let rec private indexedOffsetSegmentsLoop
        subpaths group windingOpinions subpathIndex collected =
        match subpaths with
        | [] -> List.rev collected
        | first :: rest ->
            let windingOpinion, remainingOpinions =
                match windingOpinions with
                | opinion :: remaining -> Some opinion, remaining
                | [] -> None, []
            let collected =
                Subpath.segments first
                |> List.fold (fun accumulated segment ->
                    { Group = group
                      SubpathIndex = subpathIndex
                      Segment = segment
                      WindingOpinion = windingOpinion } :: accumulated) collected
            indexedOffsetSegmentsLoop
                rest group remainingOpinions (subpathIndex + 1) collected

    let private indexedOffsetSegments subpaths group windingOpinions =
        indexedOffsetSegmentsLoop subpaths group windingOpinions 0 []

    let private offsetSegmentArrangement (indexed: IndexedOffsetSegment list) =
        let segments = indexed |> List.map (fun item -> item.Segment)
        Arrangement.buildWith
            segments arrangementTolerance arrangementTolerance 0.0001<parameter>
        |> Result.mapError ArrangementGraphError
        |> Result.map (fun build ->
            { Graph = build.Graph
              IndexedSegments = indexed
              SegmentImages = build.SegmentImages
              EdgeImages = build.EdgeImages })

    let private bandSegmentArrangement untrimmed windingOpinions =
        indexedOffsetSegments
            untrimmed UntrimmedOffsetSegment windingOpinions
        |> offsetSegmentArrangement

    let private singleOffsetSegmentArrangement
        untrimmed zeroSourceSegments offset =
        let offsetOpinion, zeroOpinion =
            if offset >= 0.0<length> then
                { Left = 0; Right = 1 }, { Left = 1; Right = 0 }
            else
                { Left = 1; Right = 0 }, { Left = 0; Right = 1 }
        let indexed =
            indexedOffsetSegments
                untrimmed UntrimmedOffsetSegment
                (List.replicate (List.length untrimmed) offsetOpinion)
            @ (zeroSourceSegments
               |> List.map (fun segment ->
                    { Group = ZeroOffsetSourceSegment
                      SubpathIndex = 0
                      Segment = segment
                      WindingOpinion = Some zeroOpinion }))
        offsetSegmentArrangement indexed

    let rec private arrangementEdgeById
        (edges: ArrangementEdge list) id
        : Result<ArrangementEdge, ArrangementError> =
        match edges with
        | [] -> Error(MissingArrangementEdge id)
        | first :: rest ->
            if first.Id = id then Ok first
            else arrangementEdgeById rest id

    let private sourceSegmentImageEdges
        (build: OffsetArrangementBuild)
        (image: ArrangementSourceSegmentImage) =
        image.Edges
        |> List.fold (fun state reference ->
            state
            |> Result.bind (fun found ->
                arrangementEdgeById build.Graph.Edges reference.EdgeId
                |> Result.mapError ArrangementGraphError
                |> Result.map (fun edge -> (edge, reference.Reversed) :: found))) (Ok [])
        |> Result.map List.rev

    let private offsetIndexedSegmentAt segments index =
        if index < 0 then None else List.tryItem index segments

    let private offsetSegmentIndexHasGroup
        (build: OffsetArrangementBuild) segmentIndex group =
        match offsetIndexedSegmentAt build.IndexedSegments segmentIndex with
        | Some indexed -> indexed.Group = group
        | None -> false

    let private arrangementEdgeImageById
        (images: ArrangementEdgeImage list) edgeId =
        images |> List.tryFind (fun image -> image.EdgeId = edgeId)

    let private arrangementEdgeHasGroup
        (build: OffsetArrangementBuild) edgeId group =
        match arrangementEdgeImageById build.EdgeImages edgeId with
        | None -> false
        | Some image ->
            image.Sources
            |> List.exists (fun source ->
                offsetSegmentIndexHasGroup build source.SegmentIndex group)

    let private retainOffsetImageEdges
        (graph: ArrangementGraph)
        (build: OffsetArrangementBuild)
        : OffsetTrimGraph =
        { Vertices = graph.Vertices
          Edges =
            graph.Edges
            |> List.filter (fun edge ->
                arrangementEdgeHasGroup build edge.Id UntrimmedOffsetSegment)
          EdgeCapacities = None }

    let private offsetTrimGraph (graph: ArrangementGraph) : OffsetTrimGraph =
        { Vertices = graph.Vertices
          Edges = graph.Edges
          EdgeCapacities = None }

    let rec private bandSubpathWindingOpinions bands =
        match bands with
        | [] -> []
        | first :: rest ->
            let opinions =
                match first with
                | OpenSubpathBand _ -> [ { Left = 0; Right = 1 } ]
                | ClosedSubpathBand _ ->
                    [ { Left = 0; Right = 1 }; { Left = 1; Right = 0 } ]
            opinions @ bandSubpathWindingOpinions rest

    /// Constructs one untrimmed offset of a subpath with explicit options.
    let subpathUntrimmedWith subpath offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized -> buildSingleOffsetUntrimmed normalized offset options)
        |> Result.map (fun build -> build.Subpath)

    /// Constructs one untrimmed offset of a subpath with default options.
    let subpathUntrimmed subpath offset =
        subpathUntrimmedWith subpath offset defaultOptions

    /// Constructs synchronized untrimmed inner and outer offsets.
    let subpathBandUntrimmedWith subpath innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedUntrimmed normalized innerOffset outerOffset options)
        |> Result.map (fun build -> Path.ofSubpaths [ build.Inner; build.Outer ])

    /// Constructs synchronized untrimmed inner and outer offsets with default options.
    let subpathBandUntrimmed subpath innerOffset outerOffset =
        subpathBandUntrimmedWith subpath innerOffset outerOffset defaultOptions

    let rec private untrimmedOffsetPathSubpaths subpaths offset options converted =
        match subpaths with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            subpathUntrimmedWith first offset options
            |> Result.bind (fun offsetSubpath ->
                untrimmedOffsetPathSubpaths
                    rest offset options (offsetSubpath :: converted))

    /// Constructs an untrimmed offset independently for every source subpath.
    let pathUntrimmedWith path offset options =
        validateOptions options
        |> Result.bind (fun _ ->
            untrimmedOffsetPathSubpaths (Path.subpaths path) offset options [])
        |> Result.map Path.ofSubpaths

    /// Constructs untrimmed offsets for a path with default options.
    let pathUntrimmed path offset =
        pathUntrimmedWith path offset defaultOptions

    let rec private singleOffsetUntrimmedPathBuilds subpaths offset options converted =
        match subpaths with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            buildSingleOffsetUntrimmed first offset options
            |> Result.bind (fun build ->
                singleOffsetUntrimmedPathBuilds rest offset options (build :: converted))

    let rec private untrimmedBandPathSubpaths
        subpaths innerOffset outerOffset options converted =
        match subpaths with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            subpathBandUntrimmedWith first innerOffset outerOffset options
            |> Result.bind (fun band ->
                untrimmedBandPathSubpaths
                    rest innerOffset outerOffset options
                    (List.rev (Path.subpaths band) @ converted))

    /// Constructs synchronized untrimmed bands independently for every subpath.
    let pathBandUntrimmedWith path innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ ->
            untrimmedBandPathSubpaths
                (Path.subpaths path) innerOffset outerOffset options [])
        |> Result.map Path.ofSubpaths

    /// Constructs untrimmed bands for a path with default options.
    let pathBandUntrimmed path innerOffset outerOffset =
        pathBandUntrimmedWith path innerOffset outerOffset defaultOptions

    let private arrangementEdgeCapacities
        (graph: OffsetTrimGraph)
        : AvailableEdgeCapacity list =
        match graph.EdgeCapacities with
        | None ->
            graph.Edges |> List.map (fun edge -> { EdgeId = edge.Id; Remaining = 1 })
        | Some capacities ->
            capacities
            |> List.choose (fun (edgeId, remaining) ->
                if remaining > 0 then Some { EdgeId = edgeId; Remaining = remaining }
                else None)

    let rec private takeEdgeCapacity
        id (capacities: AvailableEdgeCapacity list)
        : AvailableEdgeCapacity list option =
        match capacities with
        | [] -> None
        | first :: rest when first.EdgeId = id ->
            if first.Remaining = 1 then Some rest
            else Some({ first with Remaining = first.Remaining - 1 } :: rest)
        | first :: rest ->
            takeEdgeCapacity id rest |> Option.map (fun rest -> first :: rest)

    let private reverseSurvivorEdge (edge: SurvivorEdge) : SurvivorEdge =
        { edge with
            Reversed = not edge.Reversed
            StartVertex = edge.EndVertex
            EndVertex = edge.StartVertex
            Segment = Segment.reverse edge.Segment }

    let private reverseSurvivorChain (chain: SurvivorChain) : SurvivorChain =
        { chain with
            StartVertex = chain.EndVertex
            EndVertex = chain.StartVertex
            Edges = chain.Edges |> List.rev |> List.map reverseSurvivorEdge }

    let private markSurvivorChainClosed (chain: SurvivorChain) : SurvivorChain =
        { chain with Closed = chain.StartVertex = chain.EndVertex }

    let rec private mergeSurvivorChains
        (incoming: SurvivorChain)
        (candidate: SurvivorChain)
        : SurvivorChain option =
        if candidate.EndVertex = incoming.StartVertex then
            Some
                { StartVertex = candidate.StartVertex
                  EndVertex = incoming.EndVertex
                  Edges = candidate.Edges @ incoming.Edges
                  Closed = candidate.StartVertex = incoming.EndVertex }
        elif incoming.EndVertex = candidate.StartVertex then
            Some
                { StartVertex = incoming.StartVertex
                  EndVertex = candidate.EndVertex
                  Edges = incoming.Edges @ candidate.Edges
                  Closed = incoming.StartVertex = candidate.EndVertex }
        elif incoming.EndVertex = candidate.EndVertex then
            mergeSurvivorChains (reverseSurvivorChain incoming) candidate
        elif incoming.StartVertex = candidate.StartVertex then
            mergeSurvivorChains (reverseSurvivorChain incoming) candidate
        else None

    let rec private insertSurvivorChain
        (chain: SurvivorChain)
        (openChains: SurvivorChain list)
        (skipped: SurvivorChain list) =
        match openChains with
        | [] -> markSurvivorChainClosed chain :: List.rev skipped
        | candidate :: rest ->
            match mergeSurvivorChains chain candidate with
            | Some merged ->
                insertSurvivorChain
                    (markSurvivorChainClosed merged)
                    (List.rev skipped @ rest) []
            | None -> insertSurvivorChain chain rest (candidate :: skipped)

    let private appendSourceOrderEdge
        (edge: SurvivorEdge)
        (openChains: SurvivorChain list) =
        let chain =
            { StartVertex = edge.StartVertex
              EndVertex = edge.EndVertex
              Edges = [ edge ]
              Closed = edge.StartVertex = edge.EndVertex }
        insertSurvivorChain chain openChains []

    let private appendSourceOrderEdges
        (edges: SurvivorEdge list)
        (openChains: SurvivorChain list) =
        edges |> List.fold (fun openChains edge -> appendSourceOrderEdge edge openChains) openChains

    let rec private sourceOrderSurvivorDirectedEdges
        (directedEdges: (ArrangementEdge * bool) list)
        (available: AvailableEdgeCapacity list)
        (edges: SurvivorEdge list) =
        match directedEdges with
        | [] -> List.rev edges, available
        | (edge: ArrangementEdge, reversedValue) :: rest ->
            match takeEdgeCapacity edge.Id available with
            | None -> sourceOrderSurvivorDirectedEdges rest available edges
            | Some available ->
                let startVertex, endVertex, segment =
                    if reversedValue then
                        edge.EndVertex, edge.StartVertex, Segment.reverse edge.Segment
                    else edge.StartVertex, edge.EndVertex, edge.Segment
                let survivor =
                    { EdgeId = edge.Id
                      Reversed = reversedValue
                      StartVertex = startVertex
                      EndVertex = endVertex
                      Segment = segment
                      ArrangementPreimage = None }
                sourceOrderSurvivorDirectedEdges rest available (survivor :: edges)

    let private sourceOrderSurvivorImageEdges
        (build: OffsetArrangementBuild)
        (image: ArrangementSourceSegmentImage)
        (available: AvailableEdgeCapacity list) =
        sourceSegmentImageEdges build image
        |> Result.map (fun directed ->
            sourceOrderSurvivorDirectedEdges directed available [])

    let rec private sourceOrderSurvivorChainsLoop
        (build: OffsetArrangementBuild)
        (images: ArrangementSourceSegmentImage list)
        (available: AvailableEdgeCapacity list)
        (openChains: SurvivorChain list) =
        match images with
        | [] -> Ok(List.rev openChains, available)
        | image :: rest ->
            sourceOrderSurvivorImageEdges build image available
            |> Result.bind (fun (imageEdges, available) ->
                sourceOrderSurvivorChainsLoop
                    build rest available
                    (appendSourceOrderEdges imageEdges openChains))

    let private assertCapacitiesConsumed (capacities: AvailableEdgeCapacity list) =
        match capacities with
        | [] -> Ok()
        | first :: _ ->
            Error(InternalSurvivorCapacityMismatch(first.EdgeId, first.Remaining))

    let private filterBareSurvivorChains
        (chains: SurvivorChain list) protectedVertices =
        chains
        |> List.filter (fun chain ->
            chain.Closed
            || (List.contains chain.StartVertex protectedVertices
                && List.contains chain.EndVertex protectedVertices))

    let rec private survivorChainsToSubpaths
        (chains: SurvivorChain list)
        (tolerance: float<length>)
        (subpaths: Subpath list) =
        match chains with
        | [] -> Ok(List.rev subpaths)
        | first :: rest ->
            let policy = WiggleThenBridgeWith tolerance
            let segments = first.Edges |> List.map (fun edge -> edge.Segment)
            Subpath.createWith policy segments
            |> Result.mapError PathError
            |> Result.bind (fun subpath ->
                Subpath.setClosedWith policy first.Closed subpath
                |> Result.mapError PathError)
            |> Result.bind (fun subpath ->
                survivorChainsToSubpaths rest tolerance (subpath :: subpaths))

    let private closeSurvivorSubpath subpath tolerance =
        if Subpath.isClosed subpath then Ok subpath
        elif Point.distance (Subpath.start subpath) (Subpath.finish subpath) <= tolerance then
            Subpath.setClosedWith (WiggleThenBridgeWith tolerance) true subpath
            |> Result.mapError PathError
        else Ok subpath

    let rec private closeSurvivorSubpaths subpaths tolerance =
        match subpaths with
        | [] -> Ok []
        | first :: rest ->
            match closeSurvivorSubpath first tolerance,
                  closeSurvivorSubpaths rest tolerance with
            | Ok first, Ok rest -> Ok(first :: rest)
            | Error error, _
            | _, Error error -> Error error

    let rec private arrangementSourceWindingOpinions
        (build: OffsetArrangementBuild)
        (sources: ArrangementEdgeSourceImage list)
        (opinion: WindingSideOpinion)
        : Result<WindingSideOpinion, Error> =
        match sources with
        | [] -> Ok opinion
        | first :: rest ->
            match offsetIndexedSegmentAt build.IndexedSegments first.SegmentIndex with
            | None -> Error(InternalMissingIndexedSegment first.SegmentIndex)
            | Some (indexed: IndexedOffsetSegment) ->
                match indexed.WindingOpinion with
                | None -> Error(InternalMissingWindingOpinion first.SegmentIndex)
                | Some sourceOpinion ->
                    let sourceOpinion =
                        if first.Reversed then
                            { Left = sourceOpinion.Right; Right = sourceOpinion.Left }
                        else sourceOpinion
                    arrangementSourceWindingOpinions build rest
                        { Left = opinion.Left + sourceOpinion.Left
                          Right = opinion.Right + sourceOpinion.Right }

    let private arrangementEdgeWindingOpinion
        (build: OffsetArrangementBuild) edgeId =
        match arrangementEdgeImageById build.EdgeImages edgeId with
        | None -> Error(InternalMissingEdgeImage edgeId)
        | Some (image: ArrangementEdgeImage) ->
            arrangementSourceWindingOpinions build image.Sources
                { Left = 0; Right = 0 }

    let private arrangementEdgeWindingMatchesOpinion
        (build: OffsetArrangementBuild)
        (edge: ArrangementEdge)
        (winding: Point<length> -> Result<int, Error>)
        (sideSamplingDistance: float<length>) =
        arrangementEdgeWindingOpinion build edge.Id
        |> Result.bind (fun expected ->
            Segment.point edge.Segment 0.5<parameter>
            |> Result.mapError PathError
            |> Result.bind (fun point ->
                unitNormal edge.Segment 0.5<parameter>
                |> Result.bind (fun normal ->
                    let leftPoint = Point.add point (Point.scale sideSamplingDistance normal)
                    let rightPoint = Point.add point (Point.scale -sideSamplingDistance normal)
                    match winding leftPoint, winding rightPoint with
                    | Ok left, Ok right ->
                        let commonShift = expected.Left - left
                        Ok(commonShift >= 0
                           && expected.Right - right = commonShift
                           && left + commonShift >= 0
                           && right + commonShift >= 0)
                    | Error error, _
                    | _, Error error -> Error error)))

    let rec private deleteWindingMismatchedEdgesLoop
        (build: OffsetArrangementBuild)
        (edges: ArrangementEdge list)
        winding sideSamplingDistance
        (retained: ArrangementEdge list) =
        match edges with
        | [] -> Ok(List.rev retained)
        | edge :: rest ->
            arrangementEdgeWindingMatchesOpinion build edge winding sideSamplingDistance
            |> Result.bind (fun matches ->
                deleteWindingMismatchedEdgesLoop
                    build rest winding sideSamplingDistance
                    (if matches then edge :: retained else retained))

    let private deleteWindingMismatchedEdges
        (build: OffsetArrangementBuild)
        (graph: OffsetTrimGraph)
        winding sideSamplingDistance =
        deleteWindingMismatchedEdgesLoop
            build graph.Edges winding sideSamplingDistance []
        |> Result.map (fun retained -> { graph with Edges = retained })

    let private protectedVertexParities vertices =
        vertices
        |> List.distinct
        |> List.choose (fun vertex ->
            let occurrences = vertices |> List.filter ((=) vertex) |> List.length
            if occurrences % 2 = 1 then Some(PreferredVertexParity(vertex, 1))
            else None)

    let private forcedParityReduceTrimGraph graph protectedVertices =
        let arrangement =
            { Vertices = graph.Vertices
              Edges = graph.Edges
              CyclicOrders = [] }
        Arrangement.forcedParityCapacities
            arrangement (protectedVertexParities protectedVertices)
        |> Result.mapError ForcedParityPruningError
        |> Result.map (fun assignments ->
            let edgeCapacities =
                assignments |> List.map (fun assignment -> assignment.EdgeId, assignment.Capacity)
            let edges =
                graph.Edges
                |> List.filter (fun edge ->
                    assignments
                    |> List.exists (fun assignment ->
                        assignment.EdgeId = edge.Id && assignment.Capacity > 0))
            { Vertices = graph.Vertices
              Edges = edges
              EdgeCapacities = Some edgeCapacities })

    let rec private takeSegmentImages images count =
        if count <= 0 then Ok([], images)
        else
            match images with
            | [] -> Error InternalSegmentImageCountMismatch
            | first :: rest ->
                takeSegmentImages rest (count - 1)
                |> Result.map (fun (taken, remaining) -> first :: taken, remaining)

    let private segmentImageStartVertex build image =
        sourceSegmentImageEdges build image
        |> Result.bind (fun edges ->
            match edges with
            | [] -> Error(InternalEmptySegmentImage image.SegmentIndex)
            | (edge, reversedValue) :: _ ->
                Ok(if reversedValue then edge.EndVertex else edge.StartVertex))

    let private segmentImageEndVertex build image =
        sourceSegmentImageEdges build image
        |> Result.bind (fun edges ->
            match List.tryLast edges with
            | None -> Error(InternalEmptySegmentImage image.SegmentIndex)
            | Some(edge, reversedValue) ->
                Ok(if reversedValue then edge.StartVertex else edge.EndVertex))

    let rec private untrimmedOpenEndpointVerticesLoop
        build untrimmed images protectedVertices =
        match untrimmed with
        | [] -> Ok(List.rev protectedVertices)
        | first :: rest ->
            takeSegmentImages images (List.length (Subpath.segments first))
            |> Result.bind (fun (subpathImages, remainingImages) ->
                if Subpath.isClosed first || List.isEmpty subpathImages then
                    untrimmedOpenEndpointVerticesLoop
                        build rest remainingImages protectedVertices
                else
                    let firstImage = List.head subpathImages
                    let lastImage = List.last subpathImages
                    match segmentImageStartVertex build firstImage,
                          segmentImageEndVertex build lastImage with
                    | Ok startVertex, Ok endVertex ->
                        untrimmedOpenEndpointVerticesLoop
                            build rest remainingImages
                            (endVertex :: startVertex :: protectedVertices)
                    | Error error, _
                    | _, Error error -> Error error)

    let private untrimmedOpenEndpointVertices build untrimmed =
        untrimmedOpenEndpointVerticesLoop
            build untrimmed build.SegmentImages []

    let private assertForcedParityChainsValid
        (graph: OffsetTrimGraph)
        (chains: SurvivorChain list)
        protectedVertices =
        match graph.EdgeCapacities with
        | None -> Ok()
        | Some _ ->
            match chains |> List.tryFind (fun chain ->
                not chain.Closed
                && not (List.contains chain.StartVertex protectedVertices
                        && List.contains chain.EndVertex protectedVertices)) with
            | Some chain ->
                Error(InternalForcedParityOpenChain(chain.StartVertex, chain.EndVertex))
            | None -> Ok()

    let private sourceOrderSurvivorSubpaths
        (build: OffsetArrangementBuild)
        (graph: OffsetTrimGraph)
        protectedVertices
        (tolerance: float<length>) =
        let segmentImages =
            build.SegmentImages
            |> List.filter (fun image ->
                offsetSegmentIndexHasGroup
                    build image.SegmentIndex UntrimmedOffsetSegment)
        let available = arrangementEdgeCapacities graph
        sourceOrderSurvivorChainsLoop build segmentImages available []
        |> Result.bind (fun (chains, remaining) ->
            match assertCapacitiesConsumed remaining,
                  assertForcedParityChainsValid graph chains protectedVertices with
            | Ok _, Ok _ ->
                filterBareSurvivorChains chains protectedVertices
                |> fun chains -> survivorChainsToSubpaths chains tolerance []
            | Error error, _
            | _, Error error -> Error error)

    let private openButtBandEndCap sideA sideB =
        lineSegmentsBetween [ Subpath.finish sideA; Subpath.finish sideB ]

    let private openButtBandStartCap sideA sideB =
        lineSegmentsBetween [ Subpath.start sideB; Subpath.start sideA ]

    let private openButtBandOutline sideA sideB =
        let segments =
            Subpath.segments sideA
            @ openButtBandEndCap sideA sideB
            @ reverseSegments (Subpath.segments sideB)
            @ openButtBandStartCap sideA sideB
        Subpath.createWith Wiggle segments
        |> Result.mapError PathError
        |> Result.bind (fun outline ->
            Subpath.setClosedWith Wiggle true outline |> Result.mapError PathError)

    let private bandFromSides sideA innerOffset sideB outerOffset =
        let exterior, interior =
            if innerOffset >= outerOffset then sideA, sideB else sideB, sideA
        if Subpath.isClosed sideA then Ok(ClosedSubpathBand(exterior, interior))
        else openButtBandOutline exterior interior |> Result.map OpenSubpathBand

    let private closedUntrimmedSide source offset options =
        subpathUntrimmedWith source offset options
        |> Result.bind (fun side ->
            if Subpath.isClosed side then Ok side
            else
                Subpath.setClosedWith
                    (WiggleWith options.Fitting.Tolerance) true side
                |> Result.mapError PathError)

    let private untrimmedStrokeOutline source radius cap options =
        match subpathUntrimmedWith source radius options,
              subpathUntrimmedWith source -radius options,
              strokeEndCap source radius cap,
              strokeStartCap source radius cap with
        | Ok positive, Ok negative, Ok endCap, Ok startCap ->
            let segments =
                Subpath.segments positive
                @ endCap
                @ reverseSegments (Subpath.segments negative)
                @ startCap
            Subpath.createWith Wiggle segments
            |> Result.mapError PathError
            |> Result.bind (fun candidate ->
                Subpath.setClosedWith Wiggle true candidate |> Result.mapError PathError)
        | Error error, _, _, _
        | _, Error error, _, _
        | _, _, Error error, _
        | _, _, _, Error error -> Error error

    let private untrimmedStrokeBand
        (source: Subpath) (width: float<length>) cap (options: Options) =
        let radius = width / 2.0
        if Subpath.isClosed source then
            match closedUntrimmedSide source -radius options,
                  closedUntrimmedSide source radius options with
            | Ok interior, Ok exterior -> Ok(ClosedSubpathBand(exterior, interior))
            | Error error, _
            | _, Error error -> Error error
        else
            untrimmedStrokeOutline source radius cap options
            |> Result.map OpenSubpathBand

    let private requireClosedBandSubpath subpath =
        if Subpath.isClosed subpath then Ok()
        else Error BandSubpathNotClosed

    let private oneSubpathBandSemanticPath band =
        match band with
        | OpenSubpathBand outline ->
            requireClosedBandSubpath outline
            |> Result.map (fun _ -> Path.ofSubpaths [ outline ])
        | ClosedSubpathBand(sideA, sideB) ->
            match requireClosedBandSubpath sideA, requireClosedBandSubpath sideB with
            | Ok _, Ok _ -> Ok(Path.ofSubpaths [ sideA; Subpath.reverse sideB ])
            | Error error, _
            | _, Error error -> Error error

    let rec private oneSubpathBandSemanticPaths bands paths =
        match bands with
        | [] -> Ok(List.rev paths)
        | first :: rest ->
            oneSubpathBandSemanticPath first
            |> Result.bind (fun path -> oneSubpathBandSemanticPaths rest (path :: paths))

    let private internalBandWindingFunction bands =
        oneSubpathBandSemanticPaths bands []
        |> Result.map (fun semanticPaths ->
            let path =
                semanticPaths
                |> List.collect Path.subpaths
                |> Path.ofSubpaths
            fun point ->
                WindingField.pathWinding point path
                |> Result.mapError PathError
                |> Result.bind (function
                    | Winding value -> Ok value
                    | BoundaryWinding -> Error InconsistentContainment))

    let private arrangementSplitSurvivorEdge
        (segment: ArrangementSplitTracedSegment) : SurvivorEdge =
        { EdgeId = segment.EdgeId
          Reversed = false
          StartVertex = segment.StartVertex
          EndVertex = segment.EndVertex
          Segment = segment.Segment
          ArrangementPreimage = Some segment }

    let private arrangementSplitWalkToSurvivorChain
        (walk: ArrangementSplitTracedSegment list) =
        match walk with
        | [] -> failwith "empty arrangement split walk"
        | first :: rest ->
            rest
            |> List.fold (fun chain segment ->
                { chain with
                    EndVertex = segment.EndVertex
                    Edges = arrangementSplitSurvivorEdge segment :: chain.Edges
                    Closed = chain.StartVertex = segment.EndVertex })
                { StartVertex = first.StartVertex
                  EndVertex = first.EndVertex
                  Edges = [ arrangementSplitSurvivorEdge first ]
                  Closed = first.StartVertex = first.EndVertex }
            |> fun chain -> { chain with Edges = List.rev chain.Edges }

    let private cuspTrimExpectedEndpoints
        (segments: ArrangementSplitTracedSegment list) closedValue =
        match segments with
        | [] -> Error InternalSegmentImageCountMismatch
        | first :: _ ->
            let last = List.last segments
            if closedValue then Ok(first.StartVertex, first.StartVertex)
            else Ok(first.StartVertex, last.EndVertex)

    let private arrangementSplitEdgeCapacities
        (graph: ArrangementGraph)
        (retained: ArrangementSplitTracedSegment list) =
        graph.Edges
        |> List.map (fun edge ->
            { EdgeId = edge.Id
              Capacity =
                retained
                |> List.filter (fun segment -> segment.EdgeId = edge.Id)
                |> List.length })

    let rec private arrangementSplitSourceOrderSurvivorChains
        (segments: ArrangementSplitTracedSegment list)
        (available: AvailableEdgeCapacity list)
        (openChains: SurvivorChain list) =
        match segments with
        | [] -> Ok(List.rev openChains, available)
        | segment :: rest ->
            match takeEdgeCapacity segment.EdgeId available with
            | None -> arrangementSplitSourceOrderSurvivorChains rest available openChains
            | Some available ->
                arrangementSplitSourceOrderSurvivorChains
                    rest available
                    (appendSourceOrderEdge (arrangementSplitSurvivorEdge segment) openChains)

    let private arrangementSplitParityChainsFromAssignments
        (retained: ArrangementSplitTracedSegment list)
        (assignments: EdgeCapacityAssignment list) =
        let available =
            assignments
            |> List.choose (fun assignment ->
                if assignment.Capacity > 0 then
                    Some { EdgeId = assignment.EdgeId; Remaining = assignment.Capacity }
                else None)
        arrangementSplitSourceOrderSurvivorChains retained available []
        |> Result.bind (fun (chains, remaining) ->
            assertCapacitiesConsumed remaining |> Result.map (fun _ -> chains))

    let private cuspTrimParitySurvivorChains
        (segments: ArrangementSplitTracedSegment list)
        (graph: ArrangementGraph)
        protectedVertices =
        let retained = segments |> List.filter (fun segment -> not segment.DeletionCandidate)
        match retained with
        | [] -> Ok []
        | _ ->
            Arrangement.forcedParityCapacitiesWith
                graph
                (arrangementSplitEdgeCapacities graph retained)
                (protectedVertexParities protectedVertices)
            |> Result.mapError ForcedParityPruningError
            |> Result.bind (arrangementSplitParityChainsFromAssignments retained)

    let private cuspTrimSubpathFromChain
        (chain: SurvivorChain)
        expectedClosed expectedStart expectedEnd =
        let checkedChain =
            if expectedClosed then
                if chain.Closed then Ok chain
                else Error InternalIToKExpectedClosedSubpath
            elif chain.StartVertex = expectedStart && chain.EndVertex = expectedEnd then
                Ok chain
            elif chain.StartVertex = expectedEnd && chain.EndVertex = expectedStart then
                Ok(reverseSurvivorChain chain)
            else
                Error(InternalIToKEndpointMismatch(
                    expectedStart, chain.StartVertex, expectedEnd, chain.EndVertex))
        checkedChain
        |> Result.bind (fun chain ->
            chain.Edges
            |> List.fold (fun state edge ->
                state |> Result.bind (fun segments ->
                    match edge.ArrangementPreimage with
                    | None -> Error(InternalIToKMissingJPreimage edge.EdgeId)
                    | Some preimage ->
                        Ok({ Segment = edge.Segment
                             ArrangementPreimage = preimage } :: segments))) (Ok [])
            |> Result.map (fun segments ->
                { Segments = List.rev segments; Closed = expectedClosed }))

    let private finishCuspTrimWithParity
        (split: ArrangementSplitTracedSubpath)
        (rescued: ArrangementSplitTracedSegment list)
        (build: OffsetArrangementBuild) =
        let retained = rescued |> List.filter (fun segment -> not segment.DeletionCandidate)
        match retained with
        | [] -> Ok None
        | _ ->
            cuspTrimExpectedEndpoints split.Segments split.Closed
            |> Result.bind (fun (expectedStart, expectedEnd) ->
                let protectedVertices =
                    if split.Closed then [] else [ expectedStart; expectedEnd ]
                cuspTrimParitySurvivorChains retained build.Graph protectedVertices
                |> Result.bind (function
                    | [ chain ] ->
                        cuspTrimSubpathFromChain
                            chain split.Closed expectedStart expectedEnd
                        |> Result.map Some
                    | chains -> Error(InternalIToKSubpathCount(List.length chains))))

    let rec private arrangementSplitSegmentsFromIEdgeImages
        (source: ICulledOffsetSegment)
        (images: ArrangementSegmentEdgeImage list)
        (build: OffsetArrangementBuild)
        winding
        (split: ArrangementSplitTracedSegment list) =
        match images with
        | [] -> Ok(List.rev split)
        | image :: rest ->
            arrangementEdgeById build.Graph.Edges image.EdgeId
            |> Result.mapError ArrangementGraphError
            |> Result.bind (fun edge ->
                arrangementEdgeWindingMatchesOpinion
                    build edge winding submergedSideSamplingDistance
                |> Result.bind (fun matches ->
                    let segment, startVertex, endVertex =
                        if image.Reversed then
                            Segment.reverse edge.Segment, edge.EndVertex, edge.StartVertex
                        else edge.Segment, edge.StartVertex, edge.EndVertex
                    let item =
                        { Segment = segment
                          Preimage = source
                          PreimageFrom = intervalParameter source.PreimageFrom source.PreimageTo image.From
                          PreimageTo = intervalParameter source.PreimageFrom source.PreimageTo image.To
                          EdgeId = image.EdgeId
                          StartVertex = startVertex
                          EndVertex = endVertex
                          Reversed = hPreimageIsReversed source.Preimage
                          DeletionCandidate = not matches }
                    arrangementSplitSegmentsFromIEdgeImages
                        source rest build winding (item :: split)))

    let private arrangementSplitSegmentsFromIImage
        source (image: ArrangementSourceSegmentImage) build winding =
        arrangementSplitSegmentsFromIEdgeImages source image.Edges build winding []

    let rec private arrangementSplitSegmentsFromIImages
        (segments: ICulledOffsetSegment list)
        (images: ArrangementSourceSegmentImage list)
        build winding
        (split: ArrangementSplitTracedSegment list) =
        match segments, images with
        | [], [] -> Ok(List.rev split)
        | segment :: remainingSegments, image :: remainingImages ->
            arrangementSplitSegmentsFromIImage segment image build winding
            |> Result.bind (fun pieces ->
                arrangementSplitSegmentsFromIImages
                    remainingSegments remainingImages build winding
                    (List.rev pieces @ split))
        | _ -> Error InternalSegmentImageCountMismatch

    let private arrangementSplitSubpathFromIArrangement
        (subpath: ICulledOffsetSubpath)
        (build: OffsetArrangementBuild)
        winding =
        takeSegmentImages build.SegmentImages (List.length subpath.Segments)
        |> Result.bind (fun (images, _) ->
            arrangementSplitSegmentsFromIImages
                subpath.Segments images build winding []
            |> Result.map (fun segments ->
                { Segments = segments; Closed = subpath.Closed; Side = subpath.Side }))

    let rec private takeArrangementSplitRun
        (segments: ArrangementSplitTracedSegment list)
        submerged
        (taken: ArrangementSplitTracedSegment list) =
        match segments with
        | first :: rest when first.DeletionCandidate = submerged ->
            takeArrangementSplitRun rest submerged (first :: taken)
        | _ -> List.rev taken, segments

    let rec private arrangementSplitRuns
        (segments: ArrangementSplitTracedSegment list)
        (runs: ArrangementSplitRun list) =
        match segments with
        | [] -> List.rev runs
        | first :: rest ->
            let same, remaining =
                takeArrangementSplitRun rest first.DeletionCandidate [ first ]
            arrangementSplitRuns remaining
                ({ Segments = same; Submerged = first.DeletionCandidate } :: runs)

    let private arrangementSplitRunContainsReversed (run: ArrangementSplitRun) =
        run.Segments
        |> List.exists (fun (segment: ArrangementSplitTracedSegment) -> segment.Reversed)

    let private setArrangementSplitSegmentsSubmerged
        submerged (segments: ArrangementSplitTracedSegment list) =
        segments |> List.map (fun segment -> { segment with DeletionCandidate = submerged })

    let private rescueArrangementSplitRun (run: ArrangementSplitRun) =
        if run.Submerged && not (arrangementSplitRunContainsReversed run) then
            { run with Segments = setArrangementSplitSegmentsSubmerged false run.Segments }
        else run

    let private replaceFirstArrangementSplitRun
        (replacement: ArrangementSplitRun) (runs: ArrangementSplitRun list) =
        match runs with | [] -> [] | _ :: rest -> replacement :: rest

    let private replaceLastArrangementSplitRun
        (replacement: ArrangementSplitRun) (runs: ArrangementSplitRun list) =
        match List.rev runs with | [] -> [] | _ :: rest -> List.rev (replacement :: rest)

    let private rescueArrangementSplitSubmergedRuns
        (subpath: ArrangementSplitTracedSubpath) =
        let runs = arrangementSplitRuns subpath.Segments []
        let rescued = List.map rescueArrangementSplitRun runs
        let rescued =
            match subpath.Closed, runs with
            | true, first :: _ :: _ ->
                let last = List.last runs
                if first.Submerged && last.Submerged then
                    let wrappingReversed =
                        arrangementSplitRunContainsReversed first
                        || arrangementSplitRunContainsReversed last
                    rescued
                    |> replaceFirstArrangementSplitRun
                        { first with
                            Segments = setArrangementSplitSegmentsSubmerged
                                wrappingReversed first.Segments }
                    |> replaceLastArrangementSplitRun
                        { last with
                            Segments = setArrangementSplitSegmentsSubmerged
                                wrappingReversed last.Segments }
                else rescued
            | _ -> rescued
        rescued |> List.collect (fun run -> run.Segments)

    let private cuspTrimISubpath
        (subpath: ICulledOffsetSubpath)
        (zeroSource: Subpath)
        (offset: float<length>)
        (options: Options) =
        match subpath.Segments with
        | [] -> Ok None
        | _ ->
            subpathFromSynchronizedSegments
                (subpath.Segments |> List.map (fun segment -> segment.Segment))
                subpath.Closed options.Fitting.Tolerance
            |> Result.bind (fun geometry ->
                bandFromSides zeroSource 0.0<length> geometry offset
                |> Result.bind (fun band ->
                    internalBandWindingFunction [ band ]
                    |> Result.bind (fun winding ->
                        singleOffsetSegmentArrangement
                            [ geometry ] (Subpath.segments zeroSource) offset
                        |> Result.bind (fun build ->
                            arrangementSplitSubpathFromIArrangement
                                subpath build winding
                            |> Result.bind (fun split ->
                                finishCuspTrimWithParity
                                    split
                                    (rescueArrangementSplitSubmergedRuns split)
                                    build)))))

    let private offsideSegmentSpan (segment: ArrangementSplitTracedSegment) =
        abs (segment.PreimageTo - segment.PreimageFrom)

    let private offsideClosedWalkStart
        (segment: ArrangementSplitTracedSegment) index =
        { FirstStartVertex = segment.StartVertex
          EndVertex = segment.EndVertex
          LastIndex = index
          RetainedSpan = offsideSegmentSpan segment
          SkippedRuns = 0
          IndicesReversed = [ index ]
          SegmentsReversed = [ segment ] }

    let private offsideClosedWalkExtend
        (state: OffsideClosedWalkState)
        (segment: ArrangementSplitTracedSegment) index =
        { state with
            EndVertex = segment.EndVertex
            LastIndex = index
            RetainedSpan = state.RetainedSpan + offsideSegmentSpan segment
            SkippedRuns = state.SkippedRuns + (if index = state.LastIndex + 1 then 0 else 1)
            IndicesReversed = index :: state.IndicesReversed
            SegmentsReversed = segment :: state.SegmentsReversed }

    let rec private intListLexicographicallyBefore left right =
        match left, right with
        | [], [] -> false
        | [], _ :: _ -> true
        | _ :: _, [] -> false
        | leftFirst :: leftRest, rightFirst :: rightRest ->
            if leftFirst < rightFirst then true
            elif leftFirst > rightFirst then false
            else intListLexicographicallyBefore leftRest rightRest

    let private offsideClosedWalkIsBetter
        (candidate: OffsideClosedWalkState)
        (current: OffsideClosedWalkState) =
        if candidate.RetainedSpan > current.RetainedSpan then true
        elif candidate.RetainedSpan < current.RetainedSpan then false
        elif candidate.SkippedRuns < current.SkippedRuns then true
        elif candidate.SkippedRuns > current.SkippedRuns then false
        else
            intListLexicographicallyBefore
                (List.rev candidate.IndicesReversed)
                (List.rev current.IndicesReversed)

    let private insertBetterOffsideClosedWalkState
        (candidate: OffsideClosedWalkState)
        (states: OffsideClosedWalkState list) =
        let rec loop prefix remaining =
            match remaining with
            | [] -> List.rev (candidate :: prefix)
            | first :: rest ->
                let sameState =
                    first.FirstStartVertex = candidate.FirstStartVertex
                    && first.EndVertex = candidate.EndVertex
                    && first.LastIndex = candidate.LastIndex
                if sameState then
                    if offsideClosedWalkIsBetter candidate first then
                        List.rev prefix @ (candidate :: rest)
                    else
                        states
                else
                    loop (first :: prefix) rest
        loop [] states

    let rec private offsideClosedWalkLoop
        (segments: (ArrangementSplitTracedSegment * int) list)
        (states: OffsideClosedWalkState list)
        (best: OffsideClosedWalkState option) =
        match segments with
        | [] -> states, best
        | (segment, index) :: rest ->
            let starting = offsideClosedWalkStart segment index
            let extended =
                states
                |> List.filter (fun state -> state.EndVertex = segment.StartVertex)
                |> List.map (fun state -> offsideClosedWalkExtend state segment index)
            let newStates = starting :: extended
            let states =
                newStates
                |> List.fold (fun states candidate ->
                    insertBetterOffsideClosedWalkState candidate states) states
            let best =
                newStates
                |> List.filter (fun state -> state.EndVertex = state.FirstStartVertex)
                |> List.fold (fun best candidate ->
                    match best with
                    | None -> Some candidate
                    | Some current when offsideClosedWalkIsBetter candidate current ->
                        Some candidate
                    | _ -> best) best
            offsideClosedWalkLoop rest states best

    let private offsideClosedWalk (segments: ArrangementSplitTracedSegment list) =
        let available =
            segments
            |> List.indexed
            |> List.choose (fun (index, segment) ->
                if segment.DeletionCandidate then None else Some(segment, index))
        match offsideClosedWalkLoop available [] None |> snd with
        | None -> []
        | Some state -> List.rev state.SegmentsReversed

    let rec private offsideClosedWalkDecomposition
        (segments: ArrangementSplitTracedSegment list) =
        match offsideClosedWalk segments with
        | [] -> []
        | walk ->
            let remaining =
                segments |> List.filter (fun segment -> not (List.contains segment walk))
            walk :: offsideClosedWalkDecomposition remaining

    let private offsideSurvivorChains segments =
        offsideClosedWalkDecomposition segments
        |> List.map arrangementSplitWalkToSurvivorChain

    let private cuspTrimmedSubpathGeometry
        (subpath: CuspTrimmedSubpath)
        tolerance =
        subpathFromSynchronizedSegments
            (subpath.Segments |> List.map (fun segment -> segment.Segment))
            subpath.Closed tolerance

    let rec private orientBandSubpath
        (subpath: Subpath)
        (segments: Segment list)
        (winding: Point<length> -> Result<int, Error>) =
        match segments with
        | [] -> Ok subpath
        | first :: rest ->
            Segment.point first 0.5<parameter>
            |> Result.mapError PathError
            |> Result.bind (fun point ->
                match unitNormal first 0.5<parameter> with
                | Error _ -> orientBandSubpath subpath rest winding
                | Ok normal ->
                    let leftPoint =
                        Point.add point
                            (Point.scale bandOrientationSideSamplingDistance normal)
                    let rightPoint =
                        Point.add point
                            (Point.scale -bandOrientationSideSamplingDistance normal)
                    match winding leftPoint, winding rightPoint with
                    | Ok left, Ok right when right > left -> Ok subpath
                    | Ok left, Ok right when left > right -> Ok(Subpath.reverse subpath)
                    | Ok _, Ok _ -> orientBandSubpath subpath rest winding
                    | Error error, _
                    | _, Error error -> Error error)

    let rec private orientBandSubpaths subpaths winding oriented =
        match subpaths with
        | [] -> Ok(List.rev oriented)
        | first :: rest ->
            let orientedFirst =
                if Subpath.isClosed first then
                    orientBandSubpath first (Subpath.segments first) winding
                else Ok first
            orientedFirst
            |> Result.bind (fun first ->
                orientBandSubpaths rest winding (first :: oriented))

    let private orientBandPath path winding =
        orientBandSubpaths (Path.subpaths path) winding []
        |> Result.map Path.ofSubpaths

    let private trimSingleOffsetArrangement
        (build: OffsetArrangementBuild)
        untrimmed winding
        (options: Options) =
        let trimGraph = retainOffsetImageEdges build.Graph build
        untrimmedOpenEndpointVertices build untrimmed
        |> Result.bind (fun protectedVertices ->
            deleteWindingMismatchedEdges
                build trimGraph winding submergedSideSamplingDistance
            |> Result.bind (fun withoutSubmerged ->
                forcedParityReduceTrimGraph withoutSubmerged protectedVertices
                |> Result.bind (fun parityReduced ->
                    sourceOrderSurvivorSubpaths
                        build parityReduced protectedVertices options.Fitting.Tolerance
                    |> Result.bind (fun subpaths ->
                        closeSurvivorSubpaths subpaths options.Fitting.Tolerance))))

    let private trimBandArrangement
        untrimmed winding windingOpinions
        (options: Options) =
        bandSegmentArrangement untrimmed windingOpinions
        |> Result.bind (fun build ->
            untrimmedOpenEndpointVertices build untrimmed
            |> Result.bind (fun protectedVertices ->
                deleteWindingMismatchedEdges
                    build (offsetTrimGraph build.Graph)
                    winding submergedSideSamplingDistance
                |> Result.bind (fun withoutSubmerged ->
                    forcedParityReduceTrimGraph withoutSubmerged protectedVertices
                    |> Result.bind (fun parityReduced ->
                        sourceOrderSurvivorSubpaths
                            build parityReduced protectedVertices options.Fitting.Tolerance
                        |> Result.bind (fun subpaths ->
                            closeSurvivorSubpaths subpaths options.Fitting.Tolerance)))))

    let internal internalTopologicalBandLoops untrimmed bands options =
        internalBandWindingFunction bands
        |> Result.bind (fun winding ->
            trimBandArrangement
                untrimmed winding (bandSubpathWindingOpinions bands) options)

    let private topologicalBandPathWithOpinions
        untrimmed bands windingOpinions options =
        internalBandWindingFunction bands
        |> Result.bind (fun winding ->
            trimBandArrangement untrimmed winding windingOpinions options
            |> Result.bind (fun loops ->
                orientBandPath (Path.ofSubpaths loops) winding))

    let private topologicalBandPath untrimmed bands options =
        internalTopologicalBandLoops untrimmed bands options
        |> Result.bind (fun loops ->
            internalBandWindingFunction bands
            |> Result.bind (fun winding ->
                orientBandPath (Path.ofSubpaths loops) winding))

    let private trimBandSideCusps
        (subpath: ICulledOffsetSubpath)
        zeroSource offset
        (options: Options)
        enabled =
        if enabled then
            cuspTrimISubpath subpath zeroSource offset options
            |> Result.bind (function
                | None -> Ok None
                | Some trimmed ->
                    cuspTrimmedSubpathGeometry trimmed options.Fitting.Tolerance
                    |> Result.map Some)
        else
            tracedSubpathFromI subpath 0
            |> fun traced ->
                subpathFromSynchronizedSegments
                    (traced.Segments |> List.map (fun segment -> segment.Segment))
                    traced.Closed options.Fitting.Tolerance
            |> Result.map Some

    let rec private bandArrangementTraceEdges
        (edges: ArrangementEdge list)
        build winding
        (traced: BandArrangementTraceEdge list) =
        match edges with
        | [] -> Ok(List.rev traced)
        | edge :: rest ->
            arrangementEdgeWindingMatchesOpinion
                build edge winding submergedSideSamplingDistance
            |> Result.bind (fun matches ->
                bandArrangementTraceEdges rest build winding
                    ({ Id = edge.Id
                       Segment = edge.Segment
                       Submerged = not matches } :: traced))

    let internal internalSubpathBandArrangementTrace
        subpath innerOffset outerOffset (options: Options) =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedUntrimmed normalized innerOffset outerOffset options
            |> Result.bind (fun synchronized ->
                match cuspTrimISubpath
                          synchronized.InnerCulled normalized innerOffset options,
                      cuspTrimISubpath
                          synchronized.OuterCulled normalized outerOffset options with
                | Ok(Some innerTrimmed), Ok(Some outerTrimmed) ->
                    match cuspTrimmedSubpathGeometry
                              innerTrimmed options.Fitting.Tolerance,
                          cuspTrimmedSubpathGeometry
                              outerTrimmed options.Fitting.Tolerance with
                    | Ok inner, Ok outer ->
                        bandFromSides inner innerOffset outer outerOffset
                        |> Result.bind (fun band ->
                            let opinions =
                                if innerOffset >= outerOffset then
                                    [ { Left = 0; Right = 1 }
                                      { Left = 1; Right = 0 } ]
                                else
                                    [ { Left = 1; Right = 0 }
                                      { Left = 0; Right = 1 } ]
                            internalBandWindingFunction [ band ]
                            |> Result.bind (fun winding ->
                                bandSegmentArrangement [ inner; outer ] opinions
                                |> Result.bind (fun arrangement ->
                                    bandArrangementTraceEdges
                                        arrangement.Graph.Edges
                                        arrangement winding [])))
                    | Error error, _
                    | _, Error error -> Error error
                | Ok None, _
                | _, Ok None -> Ok []
                | Error error, _
                | _, Error error -> Error error))

    let rec private cuspTrimmingArrangementTraceEdges
        (edges: ArrangementEdge list)
        build winding sideIndex
        (traced: CuspTrimmingArrangementTraceEdge list) =
        match edges with
        | [] -> Ok(List.rev traced)
        | edge :: rest ->
            arrangementEdgeWindingMatchesOpinion
                build edge winding submergedSideSamplingDistance
            |> Result.bind (fun matches ->
                cuspTrimmingArrangementTraceEdges
                    rest build winding sideIndex
                    ({ SideIndex = sideIndex
                       Id = edge.Id
                       Segment = edge.Segment
                       OffsetImage =
                           arrangementEdgeHasGroup
                               build edge.Id UntrimmedOffsetSegment
                       Submerged = not matches } :: traced))

    let private cuspTrimmingArrangementTraceForSide
        (subpath: ICulledOffsetSubpath)
        zeroSource offset sideIndex
        (options: Options) =
        subpathFromSynchronizedSegments
            (subpath.Segments |> List.map (fun segment -> segment.Segment))
            subpath.Closed options.Fitting.Tolerance
        |> Result.bind (fun geometry ->
            bandFromSides zeroSource 0.0<length> geometry offset
            |> Result.bind (fun band ->
                internalBandWindingFunction [ band ]
                |> Result.bind (fun winding ->
                    singleOffsetSegmentArrangement
                        [ geometry ] (Subpath.segments zeroSource) offset
                    |> Result.bind (fun arrangement ->
                        cuspTrimmingArrangementTraceEdges
                            arrangement.Graph.Edges arrangement winding sideIndex []))))

    let internal internalSubpathBandCuspTrimmingArrangementTrace
        subpath innerOffset outerOffset (options: Options) =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedUntrimmed normalized innerOffset outerOffset options
            |> Result.bind (fun build ->
                match cuspTrimmingArrangementTraceForSide
                          build.InnerCulled normalized innerOffset 0 options,
                      cuspTrimmingArrangementTraceForSide
                          build.OuterCulled normalized outerOffset 1 options with
                | Ok first, Ok second -> Ok(first @ second)
                | Error error, _
                | _, Error error -> Error error))

    let private tracedSubpathGeometry
        (traced: TracedOffsetSubpath)
        tolerance =
        subpathFromSynchronizedSegments
            (traced.Segments |> List.map (fun segment -> segment.Segment))
            traced.Closed tolerance

    let rec private uniqueInts values unique =
        match values with
        | [] -> List.rev unique
        | first :: rest when List.contains first unique -> uniqueInts rest unique
        | first :: rest -> uniqueInts rest (first :: unique)

    let rec private segmentImageEdgeIds
        (images: ArrangementSourceSegmentImage list)
        ids =
        match images with
        | [] -> ids
        | first :: rest ->
            let ids =
                first.Edges
                |> List.fold (fun ids image ->
                    if List.contains image.EdgeId ids then ids
                    else image.EdgeId :: ids) ids
            segmentImageEdgeIds rest ids

    let private dualEdgeFaces (dual: DualArrangementGraph) edgeId =
        dual.EdgeFaces
        |> List.tryFind (fun edge -> edge.EdgeId = edgeId)
        |> function
            | Some edge -> Ok edge
            | None -> Error(ArrangementGraphError(MissingArrangementEdge edgeId))

    let rec private contaminationSeedFaces
        (images: ArrangementSourceSegmentImage list)
        (dual: DualArrangementGraph)
        (offset: float<length>)
        seeded =
        match images with
        | [] -> Ok seeded
        | first :: rest ->
            first.Edges
            |> List.fold (fun state image ->
                state
                |> Result.bind (fun seeded ->
                    dualEdgeFaces dual image.EdgeId
                    |> Result.map (fun faces ->
                        let sourceLeft, sourceRight =
                            if image.Reversed then faces.RightFace, faces.LeftFace
                            else faces.LeftFace, faces.RightFace
                        let face = if offset > 0.0<length> then sourceLeft else sourceRight
                        if List.contains face seeded then seeded else face :: seeded))) (Ok seeded)
            |> Result.bind (fun seeded ->
                contaminationSeedFaces rest dual offset seeded)

    let rec private propagateContaminatedFaces
        (dual: DualArrangementGraph)
        barriers contaminated =
        let expanded =
            dual.EdgeFaces
            |> List.fold (fun contaminated edge ->
                if List.contains edge.EdgeId barriers then contaminated
                else
                    let left = List.contains edge.LeftFace contaminated
                    let right = List.contains edge.RightFace contaminated
                    match left, right with
                    | true, false -> edge.RightFace :: contaminated
                    | false, true -> edge.LeftFace :: contaminated
                    | _ -> contaminated) contaminated
            |> fun values -> uniqueInts values []
        if List.length expanded = List.length contaminated then expanded
        else propagateContaminatedFaces dual barriers expanded

    let rec private arrangementSplitSegmentsFromIContaminationEdges
        (source: ICulledOffsetSegment)
        (images: ArrangementSegmentEdgeImage list)
        (build: OffsetArrangementBuild)
        (dual: DualArrangementGraph)
        contaminated
        (split: ArrangementSplitTracedSegment list) =
        match images with
        | [] -> Ok(List.rev split)
        | image :: rest ->
            match arrangementEdgeById build.Graph.Edges image.EdgeId,
                  dualEdgeFaces dual image.EdgeId with
            | Ok edge, Ok faces ->
                let offside =
                    not (List.contains faces.LeftFace contaminated)
                    && not (List.contains faces.RightFace contaminated)
                let segment, startVertex, endVertex =
                    if image.Reversed then
                        Segment.reverse edge.Segment, edge.EndVertex, edge.StartVertex
                    else edge.Segment, edge.StartVertex, edge.EndVertex
                let item =
                    { Segment = segment
                      Preimage = source
                      PreimageFrom = intervalParameter source.PreimageFrom source.PreimageTo image.From
                      PreimageTo = intervalParameter source.PreimageFrom source.PreimageTo image.To
                      EdgeId = image.EdgeId
                      StartVertex = startVertex
                      EndVertex = endVertex
                      Reversed = hPreimageIsReversed source.Preimage
                      DeletionCandidate = offside }
                arrangementSplitSegmentsFromIContaminationEdges
                    source rest build dual contaminated (item :: split)
            | Error error, _ -> Error(ArrangementGraphError error)
            | _, Error error -> Error error

    let private arrangementSplitSegmentsFromIContaminationImage
        source (image: ArrangementSourceSegmentImage)
        build dual contaminated =
        arrangementSplitSegmentsFromIContaminationEdges
            source image.Edges build dual contaminated []

    let rec private arrangementSplitSegmentsFromIContaminationImages
        (segments: ICulledOffsetSegment list)
        (images: ArrangementSourceSegmentImage list)
        build dual contaminated
        (split: ArrangementSplitTracedSegment list) =
        match segments, images with
        | [], [] -> Ok(List.rev split)
        | segment :: remainingSegments, image :: remainingImages ->
            arrangementSplitSegmentsFromIContaminationImage
                segment image build dual contaminated
            |> Result.bind (fun pieces ->
                arrangementSplitSegmentsFromIContaminationImages
                    remainingSegments remainingImages build dual contaminated
                    (List.rev pieces @ split))
        | _ -> Error InternalSegmentImageCountMismatch

    let private arrangementSplitSubpathFromIContamination
        (subpath: ICulledOffsetSubpath)
        images build dual contaminated =
        arrangementSplitSegmentsFromIContaminationImages
            subpath.Segments images build dual contaminated []
        |> Result.map (fun segments ->
            { Segments = segments; Closed = subpath.Closed; Side = subpath.Side })

    let private offsideTrimmedSingleOffsetSubpath
        (build: SingleOffsetUntrimmedBuild)
        offsetImages zeroImages arrangement dual offset
        (_options: Options)
        sourceSubpathIndex =
        if not (Subpath.isClosed build.Subpath) || offset = 0.0<length> then
            Ok [ tracedSubpathFromI build.Culled sourceSubpathIndex ]
        else
            let barriers = segmentImageEdgeIds zeroImages []
            contaminationSeedFaces zeroImages dual offset []
            |> Result.bind (fun seeds ->
                let contaminated = propagateContaminatedFaces dual barriers seeds
                arrangementSplitSubpathFromIContamination
                    build.Culled offsetImages arrangement dual contaminated
                |> Result.bind (fun split ->
                    offsideSurvivorChains split.Segments
                    |> List.fold (fun state chain ->
                        state |> Result.bind (fun traced ->
                            tracedSubpathFromSurvivorChain
                                chain build.Culled.Side sourceSubpathIndex
                            |> Result.map (fun item -> item :: traced))) (Ok [])
                    |> Result.map List.rev))

    let rec private offsideTrimmedSingleOffsetSubpathsLoop
        builds offsetImages zeroImages arrangement dual offset options
        sourceSubpathIndex
        (trimmed: TracedOffsetSubpath list) =
        match builds with
        | [] -> Ok(List.rev trimmed)
        | (build: SingleOffsetUntrimmedBuild) :: rest ->
            match takeSegmentImages offsetImages (List.length (Subpath.segments build.Subpath)),
                  takeSegmentImages zeroImages (List.length (Subpath.segments build.ZeroSource)) with
            | Ok(buildOffsetImages, remainingOffsetImages),
              Ok(buildZeroImages, remainingZeroImages) ->
                offsideTrimmedSingleOffsetSubpath
                    build buildOffsetImages buildZeroImages arrangement dual
                    offset options sourceSubpathIndex
                |> Result.bind (fun buildTrimmed ->
                    offsideTrimmedSingleOffsetSubpathsLoop
                        rest remainingOffsetImages remainingZeroImages
                        arrangement dual offset options (sourceSubpathIndex + 1)
                        (List.rev buildTrimmed @ trimmed))
            | Error error, _
            | _, Error error -> Error error

    let private offsideTrimmedSingleOffsetSubpathsEnabled
        (builds: SingleOffsetUntrimmedBuild list)
        arrangement offset options =
        let offsetCount =
            builds
            |> List.sumBy (fun build -> List.length (Subpath.segments build.Subpath))
        takeSegmentImages arrangement.SegmentImages offsetCount
        |> Result.bind (fun (offsetImages, zeroImages) ->
            Arrangement.dual arrangement.Graph
            |> Result.mapError ArrangementGraphError
            |> Result.bind (fun dual ->
                offsideTrimmedSingleOffsetSubpathsLoop
                    builds offsetImages zeroImages arrangement dual offset options 0 []))

    let private offsideTrimmedSingleOffsetSubpaths
        (builds: SingleOffsetUntrimmedBuild list)
        arrangement offset options enabled =
        if enabled then
            offsideTrimmedSingleOffsetSubpathsEnabled builds arrangement offset options
        else
            builds
            |> List.mapi (fun index build -> tracedSubpathFromI build.Culled index)
            |> Ok

    let rec private offsideTrimmedSingleOffsetWindingSubpaths
        (builds: SingleOffsetUntrimmedBuild list)
        (trimmed: TracedOffsetSubpath list)
        offset bands tolerance sourceSubpathIndex collected =
        match builds, bands with
        | [], _ -> Ok(List.rev collected)
        | build :: rest, band :: remainingBands ->
            let offsetResults =
                trimmed
                |> List.filter (fun item -> item.SourceSubpathIndex = sourceSubpathIndex)
                |> List.map (fun item -> tracedSubpathGeometry item tolerance)
            offsetResults
            |> List.fold (fun state item ->
                match state, item with
                | Ok values, Ok value -> Ok(value :: values)
                | Error error, _
                | _, Error error -> Error error) (Ok [])
            |> Result.bind (fun reversedOffsetSubpaths ->
                let offsetSubpaths = List.rev reversedOffsetSubpaths
                let semanticSubpaths =
                    if not (Subpath.isClosed build.Subpath) then
                        match band with
                        | OpenSubpathBand outline -> [ outline ]
                        | ClosedSubpathBand(exterior, interior) ->
                            [ exterior; Subpath.reverse interior ]
                    else
                        match offsetSubpaths with
                        | [] -> []
                        | _ when offset >= 0.0<length> ->
                            offsetSubpaths @ [ Subpath.reverse build.ZeroSource ]
                        | _ ->
                            build.ZeroSource :: List.map Subpath.reverse offsetSubpaths
                offsideTrimmedSingleOffsetWindingSubpaths
                    rest trimmed offset remainingBands tolerance
                    (sourceSubpathIndex + 1)
                    (List.rev semanticSubpaths @ collected))
        | _ :: _, [] -> Ok(List.rev collected)

    let private offsideTrimmedSingleOffsetWindingFunction
        builds trimmed offset bands tolerance =
        offsideTrimmedSingleOffsetWindingSubpaths
            builds trimmed offset bands tolerance 0 []
        |> Result.map (fun subpaths ->
            let path = Path.ofSubpaths subpaths
            fun point ->
                WindingField.pathWinding point path
                |> Result.mapError PathError
                |> Result.bind (function
                    | Winding value -> Ok value
                    | BoundaryWinding -> Error InconsistentContainment))

    let private cuspTrimTracedSubpath
        (traced: TracedOffsetSubpath)
        zeroSource offset options =
        cuspTrimISubpath (iSubpathFromTraced traced) zeroSource offset options
        |> Result.map (Option.map (fun subpath ->
            tracedSubpathFromCuspTrimmed
                subpath traced.SourceSubpathIndex traced.Side))

    let rec private cuspTrimmedSingleOffsetSubpaths
        (subpaths: TracedOffsetSubpath list)
        (builds: SingleOffsetUntrimmedBuild list)
        offset options
        (trimmed: TracedOffsetSubpath list) =
        match subpaths with
        | [] -> Ok(List.rev trimmed)
        | traced :: rest ->
            match List.tryItem traced.SourceSubpathIndex builds with
            | None -> Error InternalSegmentImageCountMismatch
            | Some build ->
                cuspTrimTracedSubpath traced build.ZeroSource offset options
                |> Result.bind (fun result ->
                    cuspTrimmedSingleOffsetSubpaths
                        rest builds offset options
                        (match result with
                         | Some subpath -> subpath :: trimmed
                         | None -> trimmed))

    let private cuspTrimmedSingleOffsetSubpathsResult
        offsideTrimmed builds offset (options: Options) =
        cuspTrimmedSingleOffsetSubpaths
            offsideTrimmed builds offset options []
        |> Result.bind (fun traced ->
            traced
            |> List.fold (fun state subpath ->
                state |> Result.bind (fun converted ->
                    tracedSubpathGeometry subpath options.Fitting.Tolerance
                    |> Result.map (fun geometry -> geometry :: converted))) (Ok [])
            |> Result.map List.rev)

    let private submergedTrimmedSingleOffsetSubpaths
        offsideTrimmed builds originalArrangement zeroSourceSegments
        offset bands (options: Options) offside =
        offsideTrimmed
        |> List.fold (fun state subpath ->
            state |> Result.bind (fun converted ->
                tracedSubpathGeometry subpath options.Fitting.Tolerance
                |> Result.map (fun geometry -> geometry :: converted))) (Ok [])
        |> Result.map List.rev
        |> Result.bind (fun untrimmed ->
            let windingResult =
                if offside then
                    offsideTrimmedSingleOffsetWindingFunction
                        builds offsideTrimmed offset bands options.Fitting.Tolerance
                else internalBandWindingFunction bands
            windingResult
            |> Result.bind (fun winding ->
                let arrangementResult =
                    if offside then
                        singleOffsetSegmentArrangement
                            untrimmed zeroSourceSegments offset
                    else Ok originalArrangement
                arrangementResult
                |> Result.bind (fun arrangement ->
                    trimSingleOffsetArrangement
                        arrangement untrimmed winding options)))

    let private finalSingleOffsetSubpaths
        (builds: SingleOffsetUntrimmedBuild list)
        offset bands (options: Options) offside finalTrimming =
        let originalUntrimmed = builds |> List.map (fun build -> build.Subpath)
        let zeroSourceSegments =
            builds |> List.collect (fun build -> Subpath.segments build.ZeroSource)
        singleOffsetSegmentArrangement originalUntrimmed zeroSourceSegments offset
        |> Result.bind (fun originalArrangement ->
            offsideTrimmedSingleOffsetSubpaths
                builds originalArrangement offset options offside
            |> Result.bind (fun offsideTrimmed ->
                match finalTrimming with
                | CuspTrimming ->
                    cuspTrimmedSingleOffsetSubpathsResult
                        offsideTrimmed builds offset options
                | InBandTrimming ->
                    submergedTrimmedSingleOffsetSubpaths
                        offsideTrimmed builds originalArrangement
                        zeroSourceSegments offset bands options offside
                | NoTrimming ->
                    offsideTrimmed
                    |> List.fold (fun state subpath ->
                        state |> Result.bind (fun converted ->
                            tracedSubpathGeometry subpath options.Fitting.Tolerance
                            |> Result.map (fun geometry -> geometry :: converted))) (Ok [])
                    |> Result.map List.rev))

    let private orientOutlineSubpath subpath clockwise =
        let isClockwise = Area.signedSubpath subpath >= 0.0<length^2>
        if isClockwise = clockwise then subpath else Subpath.reverse subpath

    let rec private outlineContourProbeSegments
        subpath (segments: Segment list) =
        match segments with
        | [] -> Error(PathError EmptySubpath)
        | first :: rest ->
            Segment.point first 0.5<parameter>
            |> Result.mapError PathError
            |> Result.bind (fun point ->
                match unitNormal first 0.5<parameter> with
                | Error _ -> outlineContourProbeSegments subpath rest
                | Ok leftNormal ->
                    let distance =
                        max (pointTolerance * 10.0)
                            (Segment.chordLength first * 0.0001)
                    let left = Point.add point (Point.scale distance leftNormal)
                    let right = Point.add point (Point.scale -distance leftNormal)
                    match WindingField.pathContainment
                              left (Path.ofSubpaths [ subpath ]) Nonzero,
                          WindingField.pathContainment
                              right (Path.ofSubpaths [ subpath ]) Nonzero with
                    | Ok Inside, Ok Outside -> Ok left
                    | Ok Outside, Ok Inside -> Ok right
                    | Ok _, Ok _ -> outlineContourProbeSegments subpath rest
                    | Error error, _
                    | _, Error error -> Error(PathError error))

    let private outlineContourProbe subpath =
        outlineContourProbeSegments subpath (Subpath.segments subpath)

    let rec private outlineContourDepthLoop probe subpaths depth =
        match subpaths with
        | [] -> Ok depth
        | first :: rest ->
            WindingField.pathContainment
                probe (Path.ofSubpaths [ first ]) Nonzero
            |> Result.mapError PathError
            |> Result.bind (fun containment ->
                outlineContourDepthLoop probe rest
                    (if containment = Inside then depth + 1 else depth))

    let private outlineContourDepth subpath all =
        outlineContourProbe subpath
        |> Result.bind (fun probe ->
            outlineContourDepthLoop probe all 0
            |> Result.map (fun count -> max 0 (count - 1)))

    let private orientOutlineSubpathFromDepth subpath all =
        if not (Subpath.isClosed subpath) then Ok subpath
        else
            outlineContourDepth subpath all
            |> Result.map (fun depth ->
                orientOutlineSubpath subpath (depth % 2 = 0))

    let rec private orientOutlineSubpaths subpaths all oriented =
        match subpaths with
        | [] -> Ok(List.rev oriented)
        | first :: rest ->
            orientOutlineSubpathFromDepth first all
            |> Result.bind (fun orientedFirst ->
                orientOutlineSubpaths rest all (orientedFirst :: oriented))

    let private orientOutlinePath path =
        let subpaths = Path.subpaths path
        orientOutlineSubpaths subpaths subpaths []
        |> Result.map Path.ofSubpaths

    let private trimSingleOffsetBuilds
        builds offset bands (options: Options) =
        finalSingleOffsetSubpaths
            builds offset bands options
            options.SingleOffsetTrimming.Offside
            options.SingleOffsetTrimming.FinalTrimming
        |> Result.map (List.filter (fun subpath -> not (List.isEmpty (Subpath.segments subpath))))
        |> Result.bind (Path.ofSubpaths >> orientOutlinePath)

    let internal internalSingleOffsetBandCandidate source offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath source options)
        |> Result.bind (fun normalized ->
            buildSingleOffsetUntrimmed normalized offset options)
        |> Result.bind (fun build ->
            bandFromSides build.ZeroSource 0.0<length> build.Subpath offset)

    /// Offsets one segment without topological trimming.
    let segmentWith segment offset options =
        Subpath.createWith Strict [ segment ]
        |> Result.mapError PathError
        |> Result.bind (fun source -> subpathUntrimmedWith source offset options)
        |> function
            | Error(PathError EmptySubpath) -> Error(DegenerateTangent 0.0<parameter>)
            | result -> result

    /// Offsets one segment with default options and without topological trimming.
    let segment segment offset = segmentWith segment offset defaultOptions

    /// Constructs and trims one signed offset of a subpath.
    let subpathWith subpath offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSingleOffsetUntrimmed normalized offset options)
        |> Result.bind (fun untrimmedBuild ->
            bandFromSides
                untrimmedBuild.ZeroSource 0.0<length>
                untrimmedBuild.Subpath offset
            |> Result.bind (fun band ->
                trimSingleOffsetBuilds
                    [ untrimmedBuild ] offset [ band ] options))

    /// Constructs and trims one signed offset with default options.
    let subpath subpath offset = subpathWith subpath offset defaultOptions

    /// Constructs the trimmed region between two signed offsets of a subpath.
    /// Either offset ordering is accepted; exchanging them reverses the result.
    let subpathBandWith
        subpath innerOffset outerOffset (options: Options) =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourceSubpath subpath options)
        |> Result.bind (fun normalized ->
            buildSynchronizedUntrimmed normalized innerOffset outerOffset options
            |> Result.bind (fun build ->
                match trimBandSideCusps
                          build.InnerCulled normalized innerOffset options
                          options.BandTrimming.InnerCusps,
                      trimBandSideCusps
                          build.OuterCulled normalized outerOffset options
                          options.BandTrimming.OuterCusps with
                | Ok(Some inner), Ok(Some outer) ->
                    bandFromSides inner innerOffset outer outerOffset
                    |> Result.bind (fun band ->
                        let opinions =
                            if innerOffset >= outerOffset then
                                [ { Left = 0; Right = 1 }
                                  { Left = 1; Right = 0 } ]
                            else
                                [ { Left = 1; Right = 0 }
                                  { Left = 0; Right = 1 } ]
                        let path =
                            if options.BandTrimming.InBand then
                                topologicalBandPathWithOpinions
                                    [ inner; outer ] [ band ] opinions options
                            else oneSubpathBandSemanticPath band
                        path
                        |> Result.map (fun path ->
                            if innerOffset > outerOffset then Path.reverse path
                            else path))
                | Ok None, _
                | _, Ok None -> Ok Path.empty
                | Error error, _
                | _, Error error -> Error error))

    /// Constructs an offset band with default options.
    let subpathBand subpath innerOffset outerOffset =
        subpathBandWith subpath innerOffset outerOffset defaultOptions

    let rec private singleOffsetBandsFromBuilds
        (builds: SingleOffsetUntrimmedBuild list)
        offset converted =
        match builds with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            bandFromSides first.ZeroSource 0.0<length> first.Subpath offset
            |> Result.bind (fun band ->
                singleOffsetBandsFromBuilds rest offset (band :: converted))

    /// Constructs and trims an offset independently for each path subpath.
    let pathWith (path: Path) offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourcePath path options)
        |> Result.bind (fun normalized ->
            singleOffsetUntrimmedPathBuilds
                (Path.subpaths normalized) offset options []
            |> Result.bind (fun builds ->
                singleOffsetBandsFromBuilds builds offset []
                |> Result.bind (fun bands ->
                    trimSingleOffsetBuilds builds offset bands options)))

    /// Constructs trimmed path offsets with default options.
    let path (path: Path) offset = pathWith path offset defaultOptions

    let rec private bandPathSubpaths
        subpaths innerOffset outerOffset options converted =
        match subpaths with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            subpathBandWith first innerOffset outerOffset options
            |> Result.bind (fun band ->
                bandPathSubpaths rest innerOffset outerOffset options
                    (List.rev (Path.subpaths band) @ converted))

    /// Constructs a trimmed offset band independently for each path subpath.
    let pathBandWith (path: Path) innerOffset outerOffset options =
        validateOptions options
        |> Result.bind (fun _ ->
            bandPathSubpaths
                (Path.subpaths path) innerOffset outerOffset options [])
        |> Result.map Path.ofSubpaths

    /// Constructs path offset bands with default options.
    let pathBand (path: Path) innerOffset outerOffset =
        pathBandWith path innerOffset outerOffset defaultOptions

    let private closedStrokePath
        source (radius: float<length>) (options: Options) =
        untrimmedStrokeBand source (radius * 2.0) Butt options
        |> Result.bind (function
            | OpenSubpathBand _ -> Error BandSubpathNotClosed
            | ClosedSubpathBand(exterior, interior) ->
                topologicalBandPathWithOpinions
                    [ interior; exterior ]
                    [ ClosedSubpathBand(exterior, interior) ]
                    [ { Left = 1; Right = 0 }; { Left = 0; Right = 1 } ]
                    options)

    /// Converts a subpath stroke to filled outline geometry.
    let subpathStrokeWith subpath width cap (options: Options) =
        match validateStrokeWidth width, validateOptions options with
        | Error error, _
        | _, Error error -> Error error
        | Ok _, Ok _ ->
            let radius = width / 2.0
            match Subpath.segments subpath with
            | [] -> Ok Path.empty
            | _ ->
                Subpath.length subpath
                |> Result.mapError PathError
                |> Result.bind (fun sourceLength ->
                    if sourceLength <= pointTolerance then
                        zeroLengthStrokePath subpath radius cap
                    elif Subpath.isClosed subpath then
                        closedStrokePath subpath radius options
                        |> Result.bind orientOutlinePath
                    else
                        untrimmedStrokeOutline subpath radius cap options
                        |> Result.bind (fun untrimmed ->
                            topologicalBandPath
                                [ untrimmed ] [ OpenSubpathBand untrimmed ] options)
                        |> Result.bind orientOutlinePath)

    /// Strokes a subpath with a butt cap and default options.
    let subpathStroke subpath width =
        subpathStrokeWith subpath width Butt defaultOptions

    let rec private strokePathSubpaths subpaths width cap options converted =
        match subpaths with
        | [] -> Ok(List.rev converted)
        | first :: rest ->
            subpathStrokeWith first width cap options
            |> Result.bind (fun stroke ->
                strokePathSubpaths rest width cap options
                    (List.rev (Path.subpaths stroke) @ converted))

    /// Converts every subpath stroke to filled outline geometry.
    let pathStrokeWith (path: Path) width cap options =
        match validateStrokeWidth width, validateOptions options with
        | Error error, _
        | _, Error error -> Error error
        | Ok _, Ok _ ->
            strokePathSubpaths (Path.subpaths path) width cap options []
            |> Result.map Path.ofSubpaths

    /// Strokes a path with butt caps and default options.
    let pathStroke (path: Path) width =
        pathStrokeWith path width Butt defaultOptions

    let internal internalUntrimmedStrokeBand source width cap options =
        validateStrokeWidth width
        |> Result.bind (fun _ -> untrimmedStrokeBand source width cap options)

    let rec private contaminationArrangementTraceBuilds
        (builds: SingleOffsetUntrimmedBuild list)
        offsetImages zeroImages arrangement dual offset
        (traced: SingleOffsetContaminationTraceEdge list) =
        match builds with
        | [] -> Ok(List.rev traced)
        | build :: rest ->
            match takeSegmentImages offsetImages (List.length (Subpath.segments build.Subpath)),
                  takeSegmentImages zeroImages (List.length (Subpath.segments build.ZeroSource)) with
            | Ok(buildOffsetImages, remainingOffsetImages),
              Ok(buildZeroImages, remainingZeroImages) ->
                let barriers = segmentImageEdgeIds buildZeroImages []
                contaminationSeedFaces buildZeroImages dual offset []
                |> Result.bind (fun seeds ->
                    let contaminated = propagateContaminatedFaces dual barriers seeds
                    arrangementSplitSubpathFromIContamination
                        build.Culled buildOffsetImages arrangement dual contaminated
                    |> Result.bind (fun split ->
                        let survivors =
                            if Subpath.isClosed build.Subpath then
                                offsideSurvivorChains split.Segments
                                |> List.collect (fun chain ->
                                    chain.Edges
                                    |> List.choose (fun edge -> edge.ArrangementPreimage))
                            else split.Segments
                        let survivorIds = survivors |> List.map (fun item -> item.EdgeId)
                        let traced =
                            split.Segments
                            |> List.fold (fun traced segment ->
                                { Id = segment.EdgeId
                                  Segment = segment.Segment
                                  StartVertex = segment.StartVertex
                                  EndVertex = segment.EndVertex
                                  PreimageFrom = segment.PreimageFrom
                                  PreimageTo = segment.PreimageTo
                                  Offside = segment.DeletionCandidate
                                  Survives = List.contains segment.EdgeId survivorIds }
                                :: traced) traced
                        contaminationArrangementTraceBuilds
                            rest remainingOffsetImages remainingZeroImages
                            arrangement dual offset traced))
            | Error error, _
            | _, Error error -> Error error

    let internal internalPathSingleOffsetContaminationArrangementTrace
        (source: Path) offset options =
        validateOptions options
        |> Result.bind (fun _ -> normalizeSourcePath source options)
        |> Result.bind (fun normalized ->
            singleOffsetUntrimmedPathBuilds
                (Path.subpaths normalized) offset options []
            |> Result.bind (fun builds ->
                let offsetCount =
                    builds
                    |> List.sumBy (fun build -> List.length (Subpath.segments build.Subpath))
                let untrimmed = builds |> List.map (fun build -> build.Subpath)
                let zeroSegments =
                    builds |> List.collect (fun build -> Subpath.segments build.ZeroSource)
                singleOffsetSegmentArrangement untrimmed zeroSegments offset
                |> Result.bind (fun arrangement ->
                    takeSegmentImages arrangement.SegmentImages offsetCount
                    |> Result.bind (fun (offsetImages, zeroImages) ->
                        Arrangement.dual arrangement.Graph
                        |> Result.mapError ArrangementGraphError
                        |> Result.bind (fun dual ->
                            contaminationArrangementTraceBuilds
                                builds offsetImages zeroImages
                                arrangement dual offset [])))))

    let rec private pointInsideAnySemanticBand point paths =
        match paths with
        | [] -> Ok false
        | first :: rest ->
            WindingField.pathContainment point first Nonzero
            |> Result.mapError PathError
            |> Result.bind (function
                | Inside -> Ok true
                | Outside
                | Boundary -> pointInsideAnySemanticBand point rest)

    let internal internalBandInsideFunction bands =
        oneSubpathBandSemanticPaths bands []
        |> Result.map (fun paths -> fun point -> pointInsideAnySemanticBand point paths)

    let private submergedSegment
        segment
        (inside: Point<length> -> Result<bool, Error>)
        sideSamplingDistance =
        Segment.point segment 0.5<parameter>
        |> Result.mapError PathError
        |> Result.bind (fun point ->
            unitNormal segment 0.5<parameter>
            |> Result.bind (fun normal ->
                let first = Point.add point (Point.scale sideSamplingDistance normal)
                let second = Point.add point (Point.scale -sideSamplingDistance normal)
                match inside first, inside second with
                | Ok firstInside, Ok secondInside -> Ok(firstInside && secondInside)
                | Error error, _
                | _, Error error -> Error error))

    let internal internalSegmentIsSubmerged segment inside sideSamplingDistance =
        submergedSegment segment inside sideSamplingDistance

    let rec private lengthSpans
        (segments: Segment list)
        (options: LengthOptions)
        startDistance
        (spans: LengthSpan list) =
        match segments with
        | [] -> Ok(List.rev spans)
        | first :: rest ->
            Segment.lengthWith first options
            |> Result.mapError PathError
            |> Result.bind (fun segmentLength ->
                let spans =
                    if segmentLength > 0.0<length> then
                        { Segment = first
                          StartDistance = startDistance
                          Length = segmentLength } :: spans
                    else spans
                lengthSpans rest options (startDistance + segmentLength) spans)

    let private lengthSpansTotal spans =
        match spans with
        | [] -> 0.0<length>
        | first :: rest ->
            rest
            |> List.fold (fun total span ->
                max total (span.StartDistance + span.Length))
                (first.StartDistance + first.Length)

    let private positiveRemainder
        (value: float<length>)
        (modulus: float<length>) =
        let turns = floor (value / modulus)
        let remainder = value - turns * modulus
        if remainder < 0.0<length> then remainder + modulus
        elif remainder >= modulus then remainder - modulus
        else remainder

    let private offsetMapDistance
        (distance: float<length>)
        (totalLength: float<length>)
        closedValue =
        if closedValue then Ok(positiveRemainder distance totalLength)
        elif distance < 0.0<length> || distance > totalLength then
            Error(PathError(InvalidLengthDistance(distance, totalLength)))
        else Ok distance

    let rec private lengthSpanAt spans distance =
        match spans with
        | [] -> Error(DegenerateTangent 0.0<parameter>)
        | [ first ] -> Ok first
        | first :: rest ->
            if distance <= first.StartDistance + first.Length then Ok first
            else lengthSpanAt rest distance

    let private offsetMapPoint
        spans totalLength closedValue
        (options: LengthOptions)
        (local: Point<length>) =
        if not (pointIsFinite local) then Error NonFinite
        else
            offsetMapDistance local.X totalLength closedValue
            |> Result.bind (fun distance ->
                lengthSpanAt spans distance
                |> Result.bind (fun span ->
                    let localDistance = distance - span.StartDistance
                    Segment.parameterAtLengthWith span.Segment localDistance options
                    |> Result.mapError PathError
                    |> Result.bind (fun t ->
                        match Segment.point span.Segment t |> Result.mapError PathError,
                              unitNormal span.Segment t with
                        | Ok point, Ok normal ->
                            let mapped = Point.add point (Point.scale local.Y normal)
                            if pointIsFinite mapped then Ok mapped else Error NonFinite
                        | Error error, _
                        | _, Error error -> Error error)))

    /// Builds a local-coordinate map whose x coordinate follows source arc
    /// length and whose y coordinate is signed visual-left normal distance.
    let subpathOffsetMapWith subpath (options: LengthOptions) =
        lengthSpans (Subpath.segments subpath) options 0.0<length> []
        |> Result.bind (fun spans ->
            let totalLength = lengthSpansTotal spans
            if totalLength <= 0.0<length> then
                Error(DegenerateTangent 0.0<parameter>)
            else
                let closedValue = Subpath.isClosed subpath
                Ok(fun local ->
                    offsetMapPoint spans totalLength closedValue options local))

    /// Builds a local offset-coordinate map with default length options.
    let subpathOffsetMap subpath =
        subpathOffsetMapWith subpath Segment.defaultLengthOptions
