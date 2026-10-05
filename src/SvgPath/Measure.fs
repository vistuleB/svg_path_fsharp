namespace SvgPath

/// Arc lengths, distance-addressed evaluation, and subdivision.
[<RequireQualifiedAccess>]
module Measure =

    let defaultLengthOptions = Segment.defaultLengthOptions

    let segmentChordLength = Segment.chordLength

    let segmentChordLengthSquared = Segment.chordLengthSquared

    let segmentLength = Segment.length

    let segmentLengthUpperBound = Segment.lengthUpperBound

    let segmentLengthWith = Segment.lengthWith

    let segmentIsZeroLength = Segment.isZeroLength

    let segmentParameterAtLength = Segment.parameterAtLength

    let segmentParameterAtLengthWith = Segment.parameterAtLengthWith

    let segmentPointAtLength = Segment.pointAtLength

    let segmentPointAtLengthWith = Segment.pointAtLengthWith

    let segmentDerivativeAtLength = Segment.derivativeAtLength

    let segmentDerivativeAtLengthWith = Segment.derivativeAtLengthWith

    let segmentBetweenLengths = Segment.betweenLengths

    let segmentBetweenLengthsWith = Segment.betweenLengthsWith

    let segmentBetweenLengthsMany = Segment.betweenLengthsMany

    let segmentBetweenLengthsManyWith = Segment.betweenLengthsManyWith

    let segmentSubdivideToMaxLength = Segment.subdivideToMaxLength

    let segmentSubdivideToMaxLengthWith = Segment.subdivideToMaxLengthWith

    let subpathSubdivideToMaxLength = Subpath.subdivideToMaxLength

    let subpathSubdivideToMaxLengthWith = Subpath.subdivideToMaxLengthWith

    let pathSubdivideToMaxLength = Path.subdivideToMaxLength

    let pathSubdivideToMaxLengthWith = Path.subdivideToMaxLengthWith

    let subpathLength = Subpath.length

    let subpathLengthUpperBound = Subpath.lengthUpperBound

    let subpathLengthWith = Subpath.lengthWith

    let subpathIsZeroLength = Subpath.isZeroLength

    let subpathParameterAtLength = Subpath.parameterAtLength

    let subpathParameterAtLengthWith = Subpath.parameterAtLengthWith

    let subpathPointAtLength = Subpath.pointAtLength

    let subpathPointAtLengthWith = Subpath.pointAtLengthWith

    let subpathDerivativeAtLength = Subpath.derivativeAtLength

    let subpathDerivativeAtLengthWith = Subpath.derivativeAtLengthWith

    let subpathBetweenLengths = Subpath.betweenLengths

    let subpathBetweenLengthsWith = Subpath.betweenLengthsWith

    let subpathSplitAtLengths = Subpath.splitAtLengths

    let subpathSplitAtLengthsWith = Subpath.splitAtLengthsWith

    let pathLength = Path.length

    let pathLengthUpperBound = Path.lengthUpperBound

    let pathLengthWith = Path.lengthWith

    let pathParameterAtLength = Path.parameterAtLength

    let pathParameterAtLengthWith = Path.parameterAtLengthWith

    let pathPointAtLength = Path.pointAtLength

    let pathPointAtLengthWith = Path.pointAtLengthWith

    let pathDerivativeAtLength = Path.derivativeAtLength

    let pathDerivativeAtLengthWith = Path.derivativeAtLengthWith
