namespace SvgPath

/// Fill containment, winding, and crossing queries.
[<RequireQualifiedAccess>]
module Containment =

    let defaultCrossingOptions = Segment.defaultCrossingOptions

    let defaultContainmentOptions = WindingField.defaultOptions

    let segmentCrossings = Segment.crossings

    let segmentCrossingsWith = Segment.crossingsWith

    let segmentRayCrossings = Segment.rayCrossings

    let segmentRayCrossingsWith = Segment.rayCrossingsWith

    let subpathContainment = WindingField.subpathContainment

    let subpathContainmentWith = WindingField.subpathContainmentWith

    let pathContainment = WindingField.pathContainment

    let pathContainmentWith = WindingField.pathContainmentWith

    let pathWinding = WindingField.pathWinding

    let pathWindingWith = WindingField.pathWindingWith
