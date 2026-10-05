namespace SvgPath

/// Point projections and closest pairs between geometry.
[<RequireQualifiedAccess>]
module Distance =

    let defaultDistanceOptions = Segment.defaultDistanceOptions

    let segmentDistance = Segment.distance

    let segmentDistanceWith = Segment.distanceWith

    let segmentProjection = Segment.projection

    let segmentProjectionWith = Segment.projectionWith

    let subpathProjection = Subpath.projection

    let subpathProjectionWith = Subpath.projectionWith

    let subpathDistance = Subpath.distance

    let subpathDistanceWith = Subpath.distanceWith

    let pathDistance = Path.distance

    let pathDistanceWith = Path.distanceWith

    let pathProjection = Path.projection

    let pathProjectionWith = Path.projectionWith

    /// Controls for closest-pair searches. Intersection parameter snapping does not apply.
    /// Invalid values retain InvalidIntersectionTolerance and InvalidIntersectionMaxDepth.
    [<Struct>]
    type ClosestPairOptions =
        { /// Finite positive geometric tolerance, not a certified global error bound.
          Tolerance: float<length>
          /// Positive subdivision limit, also used for boundary projection refinement.
          /// Analytic line-line searches need no subdivision.
          MaxDepth: int }

    /// Default tolerance is 1e-9 length units; maximum subdivision depth is 48.
    let defaultClosestPairOptions: ClosestPairOptions =
        { Tolerance = Intersections.defaultOptions.Tolerance
          MaxDepth = Intersections.defaultOptions.MaxDepth }

    let private pairSearchOptions (options: ClosestPairOptions): Intersections.IntersectionOptions =
        { Tolerance = options.Tolerance
          MaxDepth = options.MaxDepth
          ParameterSnap = Intersections.NoParameterSnap }

    let segmentSegmentClosestPair = Intersections.segmentSegmentClosestPair

    let segmentSegmentClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.segmentSegmentClosestPairWith left right (pairSearchOptions options)

    let segmentSubpathClosestPair = Intersections.segmentSubpathClosestPair

    let segmentSubpathClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.segmentSubpathClosestPairWith left right (pairSearchOptions options)

    let segmentPathClosestPair = Intersections.segmentPathClosestPair

    let segmentPathClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.segmentPathClosestPairWith left right (pairSearchOptions options)

    let subpathSubpathClosestPair = Intersections.subpathSubpathClosestPair

    let subpathSubpathClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.subpathSubpathClosestPairWith left right (pairSearchOptions options)

    let subpathPathClosestPair = Intersections.subpathPathClosestPair

    let subpathPathClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.subpathPathClosestPairWith left right (pairSearchOptions options)

    let pathPathClosestPair = Intersections.pathPathClosestPair

    let pathPathClosestPairWith left right (options: ClosestPairOptions) =
        Intersections.pathPathClosestPairWith left right (pairSearchOptions options)
