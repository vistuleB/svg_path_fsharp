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

    let defaultClosestPairOptions = Intersections.defaultOptions

    let segmentSegmentClosestPair = Intersections.segmentSegmentClosestPair

    let segmentSegmentClosestPairWith = Intersections.segmentSegmentClosestPairWith

    let segmentSubpathClosestPair = Intersections.segmentSubpathClosestPair

    let segmentSubpathClosestPairWith = Intersections.segmentSubpathClosestPairWith

    let segmentPathClosestPair = Intersections.segmentPathClosestPair

    let segmentPathClosestPairWith = Intersections.segmentPathClosestPairWith

    let subpathSubpathClosestPair = Intersections.subpathSubpathClosestPair

    let subpathSubpathClosestPairWith = Intersections.subpathSubpathClosestPairWith

    let subpathPathClosestPair = Intersections.subpathPathClosestPair

    let subpathPathClosestPairWith = Intersections.subpathPathClosestPairWith

    let pathPathClosestPair = Intersections.pathPathClosestPair

    let pathPathClosestPairWith = Intersections.pathPathClosestPairWith
