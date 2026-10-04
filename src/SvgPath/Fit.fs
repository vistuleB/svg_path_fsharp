namespace SvgPath

/// Parametric curve construction and constrained fitting.
[<RequireQualifiedAccess>]
module Fit =

    let defaultMinimizeOptions = Segment.defaultMinimizeOptions

    let defaultParametricOptions<[<Measure>] 'Param> : ParametricOptions<'Param> = Subpath.defaultParametricOptions<'Param>

    let subpathFromParametric = Subpath.fromParametric

    let subpathFromParametricWith = Subpath.fromParametricWith

    let fitCubicWithEndpointTangents = Bezier.fitCubicWithEndpointTangents

    let fitCubicWithEndpoints = Bezier.fitCubicWithEndpoints

    let segmentMinimize = Segment.minimize

    let segmentMinimizeWith = Segment.minimizeWith
