# SvgPath

[![NuGet Version](https://img.shields.io/nuget/v/SvgPath)](https://www.nuget.org/packages/SvgPath)
[![.NET](https://img.shields.io/badge/.NET-9.0-512bd4)](https://dotnet.microsoft.com/)

`SvgPath` is a geometry library for SVG paths in F#. It parses and serializes
SVG `d` and `transform` attributes and works directly with paths, subpaths,
lines, quadratic and cubic Beziers, and elliptical arcs. Operations preserve the
original curve types where possible rather than flattening them into polygons.

The package includes:

- construction, editing, evaluation, differentiation, splitting, and
  singularity-safe curve directions;
- isolated intersections, continuous overlaps, combined encounter queries, and
  closest-point pair projections;
- fill-rule-aware union, intersection, difference, and symmetric difference
  under SVG `nonzero` and `evenodd` rules;
- clipping, cutting, one- and two-sided offsets, stroke outlines, dashes, and
  marker layout;
- bounding boxes, convex hulls, containment, area, transforms, curve fitting,
  basic-shape conversion, and path effects;
- decimal-aware relative serialization that compensates for accumulated
  rounding drift.

Topology-sensitive operations use planar arrangements of the original curves.
Segments are progressively split at intersections, endpoint contacts, and
overlap boundaries; coincident portions retain directional multiplicities and
source correspondence. This supports reconstruction without replacing the input
geometry with a polygonal approximation.

`SvgPath` targets .NET 9. It is a behavior-preserving port of the Gleam
[`svg_path`](https://github.com/vistuleB/svg_path) package, with F# units of
measure added to audit scalar usage.

```shell
dotnet add package SvgPath --version 0.1.0
```

```fsharp
open SvgPath

let tidyPathData input =
    Parse.path input
    |> Result.map (fun path ->
        Serialize.pathWith path (Serialize.decimalOptions 2))
```

```fsharp
open SvgPath

let prepareForArcAverseConsumer input =
    Parse.path input
    |> Result.map (fun path ->
        path
        |> Path.arcsToCubicBeziers
        |> Serialize.path)
```

## Module Map

- `SvgPath`: core `Path`, `Subpath`, `Segment`, `Point`, `FillRule`, and shared
  option and error types.
- `Point`: vector-style helpers for `Point<length>` and other measured points.
- `Parse` and `Serialize`: SVG path-data parsing and serialization.
- `Affine`: raw six-value affine matrices, composition, and point mapping.
- `Transform`: applying affine transforms to SVG path geometry.
- `TransformParse` and `TransformSerialize`: SVG `transform` attribute parsing
  and serialization.
- `Trig`: degree-based trigonometry helpers for SVG-facing angles.
- `Ellipse`: endpoint and center arc data, arc conversion, evaluation,
  splitting, bounding boxes, and cubic approximation.
- `Congruency`: ordered congruency checks under translation, rotation, and
  uniform scale.
- `Area`: signed area and SVG fill-rule area for subpaths and paths.
- `Clip`: curve clipping that keeps original geometry inside a filled clipping
  region without adding closure bridges.
- `Intersections`: segment, subpath, and path point-intersection queries, plus
  closest-point pair projections.
- `Overlaps`: continuous coincident intervals between segments, subpaths, and
  paths.
- `Encounters`: combined continuous-overlap and isolated point-intersection
  queries.
- `Arrangement`: planar arrangements built by progressively noding path
  segments, including endpoint clusters and coincident-edge multiplicities.
- `ArrangementDrawing`: drawing primitives for inspecting an arrangement graph.
- `Csg`: Boolean union, intersection, difference, symmetric difference, and
  nested contour reconstruction for filled paths.
- `Cut`: split subpaths and paths at intersections with cutter geometry.
- `Offset`: one-sided offsets, two-sided bands, and offset-map helpers.
- `Stroke`: SVG-style stroke outlines, caps, joins, dash extraction, and dashed
  stroke geometry.
- `Marker`: marker pose computation and marker layout transforms.
- `Effects`: one-off artistic path effects such as corner rounding.
- `Degeneracy`: normalization of near-degenerate geometry into simpler segments.
- `Curvature`: signed curvature and visual-left-normal radius helpers for
  segments.
- `ConvexHull`: convex hulls for segments, subpaths, paths, and point lists.
- `Bezier`: Bezier fitting and low-level Bezier geometry helpers.
- `BasicShapes`: conversions from SVG basic shapes to paths.
- `Svg`: small debugging helper for writing complete SVG documents.
- `Inspect`: stable, non-SVG inspection strings for debugging and tests.

## Numeric Model

Public geometry uses SVG user-space coordinates, where positive y points down.
Positive signed offsets use the visual-left normal. Clockwise and
counterclockwise APIs likewise refer to visual SVG orientation.

F# units of measure annotate the scalar quantities that were plain `Float`
values in the Gleam package:

- `float<length>` represents coordinates, distances, radii, and tolerances.
- `float<length^2>` represents squared lengths and areas.
- `float<parameter>` represents normalized curve parameters.
- `float<degree>` and `float<radian>` represent angles.

Curve parameters are dimensionless mathematically, but nominally measured in
F#. Derivatives retain parameter powers such as `Point<length / parameter>` and
`Point<length / parameter^2>`.

Most geometric operations return `Result`, keeping invalid geometry, numerical
failures, and violated topology assumptions explicit.

## Core Model

The root namespace represents SVG path data with `Path` and `Subpath` types,
supported by lower-level `Segment` and `Point` primitives.

### Points

A point stores measured `X` and `Y` coordinates:

```fsharp
open SvgPath

let p = Point.create 10.0<length> 20.0<length>
let q = Point.create 13.0<length> 24.0<length>
let d = Point.distance p q
```

Use `Point` for vector-style helpers such as `Point.dot`, `Point.norm`,
`Point.project`, `Point.right`, and `Point.direction`.

### Segments

A `Segment` is one SVG path segment, expressed in absolute coordinates, not
relative to a previous current point:

```fsharp
type Segment =
    | Line of startPoint: Point<length> * endPoint: Point<length>
    | QuadraticBezier of startPoint: Point<length> * control: Point<length> * endPoint: Point<length>
    | CubicBezier of startPoint: Point<length> * control1: Point<length> * control2: Point<length> * endPoint: Point<length>
    | Arc of EndpointArcData
```

For `Arc`, `XAxisRotation` is measured in degrees, matching SVG path data.

Segments can be evaluated, differentiated, and split by their local parameter
`t`, where `0.0<parameter>` is the segment start and `1.0<parameter>` is the
segment end:

```fsharp
Segment.point segment 0.5<parameter>
Segment.derivative segment 0.5<parameter>
Segment.split segment 0.5<parameter>
Segment.between segment 0.25<parameter> 0.75<parameter>
Segment.betweenMany segment [ 0.25<parameter>; 0.75<parameter>; 0.5<parameter> ]
```

Values outside `0.0<parameter>..1.0<parameter>` lead to silent extrapolation
along the same algebraic parameterization. Use `Segment.splitInside` and
`Segment.betweenInside` to surface parameter-domain errors instead.

For unit traversal directions that remain meaningful when the ordinary
derivative collapses to zero, use `Segment.directions`. It returns incoming and
outgoing directions separately, since a cusp can have two different one-sided
directions. `Subpath.directions` and `Path.directions` apply the same operation
at their respective parameter types; the `With` variants accept
`DirectionOptions` for controlling when a derivative candidate is treated as
collapsed.

### Subpaths

A `Subpath` is opaque. It internally consists of a start point, a list of
end-to-end segments, and a flag indicating topological closure.

```fsharp
type Subpath =
    private
        { startPoint: Point<length>
          segmentList: Segment list
          isClosed: bool }
```

The public properties are `Start`, `Segments`, and `Closed`. The library
guarantees that the first segment, when present, starts at `Start`, and that
the last segment of a topologically closed subpath, when present, likewise ends
at `Start`.

Subpaths with `Segments = []` can have any value of `Closed`. A subpath's
serialization ends in `Z` if and only if `Closed = true`.

Subpaths can be split by local segment addresses:

```fsharp
let at = { SegmentIndex = 1; T = 0.5<parameter> }

Subpath.split subpath at
Subpath.between subpath { SegmentIndex = 0; T = 0.5<parameter> } { SegmentIndex = 2; T = 0.25<parameter> }
Subpath.betweenMany subpath [ { SegmentIndex = 0; T = 0.5<parameter> }; { SegmentIndex = 2; T = 0.25<parameter> } ]
Subpath.point subpath at
Subpath.derivative subpath at
```

Subpath parameters are strict: `SegmentIndex` must address a real segment and
`T` must be inside `0.0<parameter>..1.0<parameter>`. Unlike segment parameters,
subpath parameters do not extrapolate beyond a segment. The split helpers only
return positive-length pieces.

Use `Subpath.create` to construct an open subpath from a nonempty list of
contiguous segments, and `Subpath.setClosed` to change whether a subpath is
topologically closed. `Subpath.setClosed true` may return an error, but
`Subpath.setClosed false` cannot.

```fsharp
let closedTriangle () =
    let a = Point.create 0.0<length> 0.0<length>
    let b = Point.create 10.0<length> 0.0<length>
    let c = Point.create 5.0<length> 10.0<length>

    Subpath.create [ Line(a, b); Line(b, c); Line(c, a) ]
    |> Result.bind (Subpath.setClosed true)
    |> Result.map Serialize.subpath
```

Construction succeeds when required segment endpoints meet. Construct empty
move-only subpaths with `Subpath.empty startPoint`, where `startPoint` gives the
subpath's move point.

Use `Subpath.normalizeZeroLengthLines` to remove zero-length line segments from
a subpath. It preserves at least one zero-length segment of a nonempty subpath,
though it does not add any new segments when the input has no segments.

### Paths

A `Path` is an ordered collection of independent subpaths.

```fsharp
let path = Path.ofSubpaths [ subpath ]
let subpaths = Path.subpaths path
```

The total widening conversions are `Segment.asSubpath`, `Segment.asPath`, and
`Subpath.asPath`. The narrowing `Path.asSubpath` succeeds when a path has at
most one nonempty subpath; empty subpaths are ignored unless they are all the
path contains.

Use `Path.mapSubpaths` and `Path.filterSubpaths` to transform or filter a path's
subpaths. Use `Path.combine` to assemble one path from a list of paths.

Use `Path.start` and `Path.finish` to get the endpoints of a full path. Empty
paths return `Error EmptyPath`; paths with subpaths use the first subpath's
start and the last subpath's end, including empty subpaths.

## Subpath Building

Helper functions let users employ an `EndpointPolicy` option to specify how
adjacent endpoints should be reconciled:

```fsharp
type EndpointPolicy =
    | Strict
    | Wiggle
    | WiggleWith of float<length>
    | Bridge
    | WiggleThenBridge
    | WiggleThenBridgeWith of float<length>
    | Custom of (Segment -> Segment -> bool -> Segment list)
```

`Strict` is the behavior of `Subpath.create`, requiring exact endpoint equality.
`Wiggle` moves nearby endpoints together within the package's default wiggle
tolerance of `1e-9<length>` while respecting the horizontality and verticality
of `Line` segments: horizontal and vertical lines stay horizontal and vertical.
If adjacent horizontal/horizontal or vertical/vertical lines are misaligned, a
bridge is inserted regardless of endpoint distance.

`WiggleWith tolerance` provides the same policy with an explicit tolerance.
`Bridge` keeps existing endpoints in place and inserts a straight line segment
when needed. `WiggleThenBridge` applies the pair-local wiggle behavior when
adjacent endpoints are within tolerance, and otherwise bridges that pair.
`WiggleThenBridgeWith tolerance` is its configurable counterpart.

`Custom` gives callers a hook for bespoke endpoint reconciliation. Its third
callback argument is `true` only for the closing join from the last segment back
to the first segment of a closed subpath.

Functions that accept an `EndpointPolicy` end in `With`:

```fsharp
Subpath.createWith Wiggle segments
Subpath.joinWith Bridge [ firstSubpath; secondSubpath ]
Subpath.spliceWith Wiggle startIndex deleteCount replacementSegments subpath
Subpath.setClosedWith Bridge true subpath
Subpath.rebuildWith WiggleThenBridge subpath
Path.rebuildWith Wiggle path
```

Failure to reconcile segment endpoints under a given policy results in a
`Discontinuous` `SegmentError` variant carrying the adjacent segment indices,
the expected and actual points, and their distance.

For hand-authored static geometry where invalid continuity is a programmer
error, use normal F# pattern matching or `Result.defaultWith (failwithf "%A")`
at the call site.

`Custom` receives adjacent segments as `previous` and `next`. For ordinary
adjacent pairs, its returned list replaces the pair. For the closing join from
the last segment back to the first segment of a closed subpath, the returned
list replaces only the last segment. An empty list deletes the replaced segment
or pair. If the returned list is nonempty, its first segment must start where
`previous` started; the constructor verifies the final subpath afterward. A
custom policy can adjust, delete, replace, or insert bridge-like segments. It
may be called even when the original adjacent endpoints already match, so it can
also perform coalescing or cleanup effects.

### Joining Subpaths

`Subpath.join` combines open subpaths into one open subpath. With the default
`Strict` policy, each subpath's end point must exactly equal the next subpath's
start point. Empty open subpaths can act as identity values when their start
points line up. `Subpath.join []` returns `Error EmptySubpath`.

Closed subpaths are rejected rather than implicitly opened. This keeps
closedness as explicit topology: if you want to discard it, use
`Subpath.setClosed false subpath` first.

### Splicing Subpaths

`Subpath.splice` replaces a range of segments while preserving the subpath
invariant. `startIndex` is a zero-based segment index, `deleteCount` is the
number of segments to remove, and the replacement list is inserted in its place.

```fsharp
Subpath.splice 2 1 replacementSegments subpath
```

If `startIndex + deleteCount` extends past the end of the subpath, everything
from `startIndex` onward is deleted. Negative `startIndex`, negative
`deleteCount`, and a start index greater than the subpath length return
`InvalidSplice`.

With the default `Strict` policy, the edited subpath must still be continuous.
Closed subpaths preserve their closed state. If the splice result is nonempty,
the subpath start is updated to the first resulting segment's start point. If
the splice result is empty, the previous start point is preserved.

### Opening and Reversing Subpaths

`Subpath.openAt` breaks open a closed subpath at a subpath parameter and returns
a single open subpath. The result traverses the whole loop from that point back
to itself. Use `T = 0.0<parameter>` to open at a segment boundary.

```fsharp
Subpath.openAt closedSubpath { SegmentIndex = 2; T = 0.5<parameter> }
Subpath.reverse subpath
Segment.reverse segment
Path.reverse path
```

## Converting Arcs and Curves

Some SVG consumers and geometry workflows prefer to avoid elliptical `Arc`
segments. Use the `arcsToCubicBeziers` family to replace arcs with cubic Bezier
curves while preserving lines, quadratic Beziers, and existing cubics:

```fsharp
Segment.arcsToCubicBeziers segment
Subpath.arcsToCubicBeziers subpath
Path.arcsToCubicBeziers path
```

Elliptical arcs are approximated with one or more cubic Beziers, split into
chunks of at most a quarter turn. The conversion preserves subpath closed/open
state. If an arc is degenerate, it falls back to the straight-line cubic Bezier
between the arc endpoints.

There is no tolerance option for this conversion. The approximation policy is
deterministic: each arc chunk spans no more than 90 degrees. This is the common
practical SVG arc-to-cubic approximation and is usually more than adequate for
rendering and interchange.

If you want every segment represented as cubic Bezier curves, use the stricter
helpers instead. Lines and quadratic Beziers are converted exactly.

```fsharp
Segment.toCubicBeziers segment
Subpath.toCubicBeziers subpath
Path.toCubicBeziers path
```

Use the `toLines` family to approximate every segment with straight lines:

```fsharp
Segment.toLines segment
Subpath.toLines subpath
Path.toLines path
```

The `With` variants accept `LinearizeOptions`. The default tolerance is
`0.01<length>` and the default recursion limit is 20. Beziers are adaptively
subdivided using their control points' distance from each chord. Arcs use a
conservative bound based on their radius and angular span. Degenerate arcs
become lines between their endpoints.

## Arcs and the Ellipse Module

`Arc` uses SVG's endpoint arc representation: an explicit `Start`, `End`, two
semi-axis radii, `XAxisRotation`, and the SVG `LargeArc` and `Sweep` flags.
This matches the information carried by an SVG `A` path command, with the
current point made explicit as `Start`.

Endpoint arcs are compact, but they are awkward for evaluation and splitting.
The lower-level `Ellipse` module exposes the two arc representations used by
the SVG implementation notes:

```fsharp
type EndpointArcData =
    { Start: Point<length>
      Radius: Point<length>
      XAxisRotation: float<degree>
      LargeArc: bool
      Sweep: bool
      End: Point<length> }

type CenterArcData =
    { Center: Point<length>
      Radius: Point<length>
      XAxisRotation: float<degree>
      StartAngle: float<degree>
      DeltaAngle: float<degree> }
```

`Ellipse.endpointToCenter` converts SVG-style endpoint data into center data.
During that conversion, radii follow SVG's forgiving rules: negative radii are
made positive, and radii that are too small to connect the endpoints are scaled
up uniformly. `CenterArcData.Radius` is therefore the corrected radius.

Public arc angles are in degrees. `StartAngle` and `DeltaAngle` are measured in
the ellipse's own coordinate system before stretching and rotation; `DeltaAngle`
is signed, and determines the sweep direction.

Use `Segment.arcCenterData` to convert a root `Arc` segment to
`CenterArcData`, and `Segment.arcFromCenterData` to come back to an `Arc`. For
common evaluation tasks, use `Segment.arcPoint`, `Segment.arcDerivative`, and
`Segment.arcPointAtAngle`; these keep the ordinary `SegmentError` type.

## Geometry Helpers

The core modules expose common geometry helpers directly on `Segment`,
`Subpath`, and `Path`. The XML docs contain the full option and error details;
this section maps the available families.

### Bounding Boxes

Use `Segment.boundingBox`, `Subpath.boundingBox`, and `Path.boundingBox` for
axis-aligned bounds. Line, Bezier, and arc extrema are included. Measure a box
with `BoundingBox.width`, `BoundingBox.height`, `BoundingBox.center`, and
`BoundingBox.diameter`; the diameter is width plus height.

### Optimization Over Segments

Use `Segment.minimize` to find the segment parameter where a scalar function of
the segment point is minimized:

```fsharp
let lowestPoint segment =
    Segment.minimize segment (fun point -> float point.Y)
```

The returned value is a segment parameter in `0.0<parameter>..1.0<parameter>`.
You can pass it to `Segment.point` or `Segment.split`.

Minimization is numerical and does not require a derivative. Use
`Segment.minimizeWith` when the default sampling and tolerance are not
appropriate.

### Segment and Subpath Lengths

Use `Segment.length`, `Subpath.length`, or `Path.length` to measure geometry.
Lines are exact. Beziers and arcs use adaptive integration. Distances are true
path-coordinate lengths, not normalized fractions.

Length-address helpers convert traveled distances back to ordinary parameters
and evaluated geometry:

```fsharp
Segment.parameterAtLength segment 12.0<length>
Segment.pointAtLength segment 12.0<length>
Segment.derivativeAtLength segment 12.0<length>
Segment.betweenLengths segment 12.0<length> 30.0<length>
Segment.betweenLengthsMany segment [ 12.0<length>; 20.0<length>; 30.0<length> ]

Subpath.parameterAtLength subpath 25.0<length>
Subpath.pointAtLength subpath 25.0<length>
Subpath.derivativeAtLength subpath 25.0<length>
Subpath.betweenLengths subpath 25.0<length> 60.0<length>
Subpath.betweenLengthsMany subpath [ 25.0<length>; 40.0<length>; 60.0<length> ]

Path.parameterAtLength path 40.0<length>
Path.pointAtLength path 40.0<length>
Path.derivativeAtLength path 40.0<length>
```

### Distances and Projections

Use `Segment.distance` to measure the shortest distance from a point to a
segment. Use `Segment.projection` when you also need the nearest segment
parameter and point:

```fsharp
let distanceToSegment point segment =
    Segment.distance point segment

let nearestOnSegment point segment =
    Segment.projection point segment

let nearestOnPath point path =
    Path.projection path point
```

`Subpath.projection` and `Path.projection` lift the same idea to larger
structures and return public parameters. Move-only subpaths are skipped.

### Point Containment

Use containment helpers to classify a point relative to SVG fill geometry:

```fsharp
WindingField.subpathContainment point subpath Nonzero
WindingField.pathContainment point path EvenOdd
```

The result and fill-rule types are:

```fsharp
type PointContainment =
    | Inside
    | Outside
    | Boundary

type FillRule =
    | Nonzero
    | EvenOdd
```

`Boundary` is reported independently of the fill rule. Otherwise, `Nonzero` or
`EvenOdd` determines whether the result is `Inside` or `Outside`.

Fill geometry implicitly closes every nonempty subpath with a straight line from
its end to its start. This happens whether `Subpath.Closed` is `true` or
`false`. Consequently, changing only the `Closed` field does not change the
result of containment testing. The `Closed` field still matters for
serialization and stroke semantics.

A move-only subpath has no segments, fill area, or boundary. It is always
`Outside`, even when the tested point equals its move point. An empty path and a
path containing only move-only subpaths are also `Outside`.

`Nonzero` is SVG's default fill rule. A directed crossing contributes `+1` or
`-1` to the winding number. The point is inside when the total winding number is
not zero. For a path, winding numbers are summed across all subpaths, so
oppositely directed loops can cancel and equally directed loops reinforce one
another.

`EvenOdd` ignores crossing direction. The point is inside when the total number
of crossings across all subpaths is odd. Passing through another enclosed loop
therefore toggles inside/outside regardless of that loop's direction.

For a point inside both an outer loop and a nested inner loop:

| Inner loop direction | `Nonzero` | `EvenOdd` |
| --- | --- | --- |
| Same as outer loop | `Inside` (winding magnitude 2) | `Outside` (two crossings) |
| Opposite to outer loop | `Outside` (windings cancel) | `Outside` (two crossings) |

This aggregation is why path containment cannot be implemented as "inside any
subpath". Self-intersecting subpaths and paths that revisit an area use the
same winding and crossing rules.

Before applying a fill rule, containment checks the original geometry and
implicit closing lines for boundary hits. A boundary match takes precedence over
both fill rules. Use `With` variants to choose the coordinate-space boundary
tolerance and numerical options.

## Areas

Use `Area` for signed area, SVG fill-rule area, and absolute winding area:

```fsharp
let filledArea path = Area.path path Nonzero
let signedArea path = Area.signedPath path
let windingArea path = Area.absolutePath path
```

There are three area notions here. `Area.signedSubpath` and `Area.signedPath`
return algebraic area. `Area.subpath` and `Area.path` return unsigned filled
area under `Nonzero` or `EvenOdd`. `Area.absoluteSubpath` and
`Area.absolutePath` integrate `abs(windingNumber)`, so repeated same-direction
loops count with multiplicity. `ConvexHull` is a separate geometry operation; a
hull area can be larger than the filled area of a concave or self-intersecting
shape.

Signed area is computed from line integrals. Lines, quadratic Beziers, cubic
Beziers, and elliptical arcs are handled directly. The sign depends on drawing
direction: reversing a simple loop reverses the sign. Self-intersections and
oppositely directed loops can cancel, while repeated loops can multiply the
result.

Fill-rule area follows SVG fill semantics. Every nonempty subpath is implicitly
closed with a straight line from its end to its start, regardless of the
`Subpath.Closed` field. Move-only subpaths contribute zero area. For a path, all
subpaths are considered together, so overlapping and nested subpaths are not
measured independently and then added.

The difference matters for repeated or nested loops:

| Shape | Signed area | `Nonzero` area | `EvenOdd` area |
| --- | --- | --- | --- |
| One simple loop | `+A` or `-A` | `A` | `A` |
| Same loop twice, same direction | `+2A` or `-2A` | `A` | `0` |
| Same loop twice, opposite directions | `0` | `0` | `0` |

For those three rows, `Area.absolutePath` returns `A`, `2A`, and `0`,
respectively.

`Area.subpath`, `Area.path`, `Area.absoluteSubpath`, and `Area.absolutePath`
first linearize curves and then integrate slabs of the resulting line
arrangement. The `With` variants accept `LinearizeOptions`; `Tolerance`
controls curve-to-line approximation in coordinate units, not a direct bound on
final area error. The arrangement step compares every pair of linearized edges,
so these arrangement-based areas are quadratic in the number of generated line
edges.

## Crossings, Intersections, and Overlaps

Use `Segment.crossings` to find parameter values where a scalar predicate
changes sign along a segment:

```fsharp
let horizontalCrossings segment y =
    Segment.crossings segment (fun point -> point.Y - y)
```

The returned values are segment parameters in `0.0<parameter>..1.0<parameter>`.
Crossing detection is numerical and sampling-based; use `Segment.crossingsWith`
to tune it.

Use `Intersections.segment` to find point intersections between two segments:

```fsharp
let crossings left right = Intersections.segment left right
```

Each `SegmentIntersection` contains the intersection point plus the local
parameters on both segments:

```fsharp
type SegmentIntersection =
    { LeftT: float<parameter>
      RightT: float<parameter>
      Point: Point<length> }
```

The result represents finite point intersections only; segment overlaps return
`OverlappingSegments`. The same operation is lifted to larger structures:

```fsharp
Intersections.segmentSubpath segment subpath
Intersections.subpath leftSubpath rightSubpath
Intersections.path leftPath rightPath
```

Self-intersections use parallel names:

```fsharp
Intersections.segmentSelf segment
Intersections.subpathSelf subpath
Intersections.pathSelf path
```

Results are ordered by parameter, and boundary aliases are canonicalized. Use
`With` variants to supply `IntersectionOptions` or `SelfIntersectionOptions`.

Known subpath intersection addresses can be classified afterward with
`Intersections.classifySubpathIntersection` as crossings, nontransverse
contacts, endpoint contacts, or indeterminate cases. Contact order uses
outward-pointing rays sampled at equal arc lengths; the accompanying aperture
angles instead use the incoming and outgoing traversal directions directly.

Point-intersection queries deliberately cannot represent a continuous shared
interval. Use `Overlaps` when coincident geometry is the expected result:

```fsharp
Overlaps.segment leftSegment rightSegment
Overlaps.subpath leftSubpath rightSubpath
Overlaps.path leftPath rightPath
```

A `SegmentOverlap` gives the interval parameters and geometric endpoints on
both segments. Its left parameters are canonicalized into increasing order; the
right parameters may decrease when the two segments traverse the overlap in
opposite directions. The endpoint parameters define an affine, monotone
correspondence throughout the overlap. Coincident geometry that cannot satisfy
that contract returns `NonAffineOverlapCorrespondence`; normalize or linearize
such segments before overlap detection. At a matching tolerance,
`Intersections.segment` returns `OverlappingSegments` exactly when
`Overlaps.segment` reports an overlap.

The overlap detector is intended for non-degenerate segments whose overlap
boundaries occur at an endpoint of at least one input segment. Arrangement
construction establishes that working model through progressive endpoint,
intersection, and overlap-boundary splitting. Subpath and path overlap values
retain their constituent piecewise-affine segment correspondences, and the
module provides helpers for mapping exact parameters from either traversal to
the other.

Use `Encounters` when both continuous overlaps and isolated point intersections
are required from one query. Its segment, segment-subpath, subpath, and path
functions return both lists without changing the underlying payload types.
Subpath encounters retain overlap-boundary intersections by default; the
explicitly named `filterFullyOverlapExplainedSubpathIntersectionParameters`
helper derives a view with parameters fully explained by overlaps removed.

## Convex Hulls

The `ConvexHull` module computes closed convex hull subpaths for segments,
subpaths, paths, and point lists.

```fsharp
let hull segment = ConvexHull.segmentHull segment
```

Lines, quadratic Beziers, and ordinary arcs are handled semantically. Lines
produce a two-line closed hull, while quadratic Beziers and arcs produce the
original primitive plus the chord joining its endpoints. Cubic Beziers use a
cubic-specific numerical solver.

Use `ConvexHull.subpathHull`, `ConvexHull.pathHull`, and
`ConvexHull.pointsHull` for larger inputs. Move-only subpaths contribute their
start points.

`ConvexHull.segmentMinimumWidth`, `ConvexHull.subpathMinimumWidth`, and
`ConvexHull.pathMinimumWidth` estimate the thinnest direction of a convex hull.
The corresponding `segmentDiameter`, `subpathDiameter`, and `pathDiameter`
functions measure the widest direction.

## Congruency

The `Congruency` module finds a translation, rotation, and uniform scale mapping
one ordered piece of geometry to another:

```fsharp
let mapped source target =
    Congruency.path source target 0.000001<length>
    |> Result.bind (fun transform -> Transform.path source transform)
```

This is semantic congruency, not rendered-shape equivalence. Segment
constructors must match, so a line and a visually identical degenerate curve do
not match. Arc field details are checked after the point cloud transform is
found.

`Congruency.subpath` and `Congruency.path` compare ordered structure only. They
ignore the subpath `Closed` field, but they do not rotate or cycle closed
subpaths, choose alternate starting segments, or reorder subpaths. If two closed
loops start at different places, open or rebuild them with matching segment
order before calling congruency.

The same module also exposes `fitPoints`, `fitSegment`, `fitSubpath`, and
`fitPath` for best-fit matching. Pass `Similar` for translation, rotation, and
uniform scale, or `Affine` for a general affine matrix. These helpers return a
`Fit` value containing the transform and RMS error.

## Parsing

`Parse.path` accepts normal SVG path data syntax, including:

- comma separators;
- SVG whitespace separators, including form feed;
- compact signed numbers such as `M0-1`;
- compact arc flags such as `A10 10 0 0110 20`;
- implicit line commands after `M`;
- repeated command argument groups;
- relative and absolute commands;
- closepath commands `Z` and `z`.

```fsharp
let canonicalize () =
    Parse.path "M0,0 10,10z"
    |> Result.map Serialize.path
```

The parsed object is not just a token stream. It is normalized into this
package's path model. For example, an implicit line after `M` becomes a `Line`
segment internally.

The parser follows the SVG path-data grammar for number consumption,
comma/whitespace placement, command repetition, and arc flags. Its conformance
suite includes cases adapted from Web Platform Tests and the W3C SVG 1.1 Second
Edition test suite. Unlike a browser renderer, `Parse.path` is strict: invalid
trailing data returns `Error` for the whole input instead of returning or
rendering the valid prefix.

Parser errors have the form `ParseError(reason, remaining)`. `remaining` is the
exact suffix of the original input beginning at the failure location and is
empty for a failure at end of input.

Closepath is also represented semantically. If parsing `Z` needs a straight line
back to the subpath start, the parser inserts that line and marks the subpath
closed. If the subpath is already back at its start, no extra line is inserted;
the subpath is just marked closed.

## Serialization

`Serialize` emits SVG path data from `Path`, `Subpath`, and `Segment` values.

By default it uses:

- absolute commands;
- up to 5 decimal places;
- stripped trailing decimal zeroes;
- readable whitespace;
- repeated command letters;
- one-line path data;
- `H` and `V` for horizontal and vertical lines when possible;
- `S` and `T` for smooth curves when possible;
- `Z` for closed subpaths.

Serialization options can use relative commands, commas inside coordinate pairs,
smaller whitespace, rounded numbers, fixed decimal places, omitted repeated
command letters, line breaks, left-padded numbers for visual alignment,
explicit line commands instead of `H`/`V`, and explicit curve commands instead
of `S`/`T`.

When `Relative = true`, the serializer compensates for accumulated drift caused
by decimal rounding.

```fsharp
let compactPathData input =
    Parse.path input
    |> Result.map (fun path ->
        Serialize.pathWith path (Serialize.minifyingOptions 2))
```

`Serialize.minifyingOptions` is a deterministic small-output preset. It uses the
serializer's normal `H`/`V` and `S`/`T` discovery, but it does not try every SVG
spelling and prove that the result is globally shortest.

If you want a complete SVG document for debugging or examples, use `Svg` with a
view box, per-path style strings, and optional text labels. It is a small
drawing helper, not a rendering framework.

### Move-Only Subpaths, Zero-Length Segments, and Closure

SVG distinguishes move-only subpaths from zero-length drawing subpaths. The
subpath consisting only of the command `M 50,0` has a current point but no
drawing segment, whereas `M 50,0 L 50,0` has a zero-length line segment. User
agents can render these differently: with `stroke-linecap:round` or
`stroke-linecap:square`, for example, the zero-length line can produce a visible
mark while the move-only subpath remains invisible. SVG 2 describes this in its
notes on [zero-length path segments](https://www.w3.org/TR/SVG2/paths.html#PathElementImplementationNotes)
and [stroke line caps](https://www.w3.org/TR/SVG2/painting.html#LineCaps).
There is a similar difference between `M 0,0` and `M 0,0 Z`, with the `Z`
command supplying a zero-length line segment to the subpath:

![Zero-length closepath behavior](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/zero_length_closepath_probe.svg)

```xml
<path d="M 90,50" style="fill:none;stroke:blue;stroke-width:24;stroke-linecap:round;" />
<path d="M 260,50 L 260,50" style="fill:none;stroke:blue;stroke-width:24;stroke-linecap:round;" />
<path d="M 90,230" style="fill:none;stroke:black;stroke-width:24;stroke-linecap:round;" />
<path d="M 260,230 Z" style="fill:none;stroke:black;stroke-width:24;stroke-linecap:round;" />
```

For that reason, `Subpath.normalizeZeroLengthLines` keeps one zero-length line
if a subpath consists only of zero-length lines, preserving the difference
between a zero-length subpath and a move-only subpath. It does this even for
closed subpaths, where the choice is mainly about preserving internal
representation consistency.

Concerning the detailed mechanics of subpath closure, a literal read of the
[SVG 2 specification](https://www.w3.org/TR/SVG2/paths.html#PathDataClosePathCommand)
plausibly suggests that `Z` means "draw a final line from the current point to
the starting point, even if this final line has length 0, and then mark
topological closure". The observable behavior of user agents, however, suggests
that `Z` is commonly interpreted as meaning "draw a final line to the starting
point only if necessary to bridge a gap or when no segments have been added to
the subpath yet, and then mark topological closure". This library follows the
latter interpretation.

Under this interpretation, a final nonzero-jump line that geometrically closes a
topologically closed subpath can be elided in the representation of the subpath,
shortening `M0,0 L10,10 0,0 Z` to `M0,0 L10,10 Z`. A final zero-length jump
followed by `Z` cannot be dropped without losing information, so the serializer
never drops zero-length lines, including immediately prior to `Z`.

## Transforming Paths

`Transform` applies SVG-style affine transforms to segments, subpaths, and
paths.

```fsharp
let movePathData input =
    Parse.path input
    |> Result.bind (fun path ->
        Transform.path path (Transform.translate 10.0<length> 20.0<length>))
    |> Result.map Serialize.path
```

Transforms use the SVG six-value affine matrix:

```text
matrix(a b c d e f)
```

which corresponds to:

```text
x' = a*x + c*y + e
y' = b*x + d*y + f
```

The ordinary `segment`, `subpath`, and `path` transform functions preserve
segment types and return `DegenerateArcTransform` when an affine transform
collapses an arc into line geometry. Use `segmentGracefully`,
`segmentToSubpathGracefully`, `subpathGracefully`, or `pathGracefully` when
collapsed arcs should instead become one or more line segments.

Matrix values can be constructed and inspected as tuples:

```fsharp
let inspectTransform () =
    Transform.rotate 30.0<degree>
    |> Transform.toTuple
```

Use `Transform.chain first second` when thinking in application order. Use
`Transform.multiply left right` when thinking in matrix multiplication order.

```fsharp
let scaleThenMove () =
    let scale = Transform.scale 2.0
    let move = Transform.translate 10.0<length> 20.0<length>

    Transform.chain scale move
```

Transforms can also be applied about a point, or about one of the nine anchor
points on a segment, subpath, or path bounding box:

```text
TopLeft      TopCenter      TopRight
CenterLeft   Center         CenterRight
BottomLeft   BottomCenter   BottomRight
```

```fsharp
let flipPathHorizontally path =
    Transform.pathAboutAnchor path (Transform.scaleXY -1.0 1.0) Center
```

## Transform Attributes

SVG transform attributes can be parsed and serialized separately from paths.

```fsharp
let tidyTransformAttribute input =
    TransformParse.attribute input
    |> Result.map TransformSerialize.toString
```

The transform parser accepts normal SVG transform syntax, including compound
attributes such as:

```text
translate(10) scale(2) skewX(3)
```

Its errors use the same `ParseError(reason, remaining)` convention as path-data
parsing.

Transform serialization prefers readable SVG forms when the matrix can be
recognized clearly:

```text
translate(10 20)
translate(10 20) scale(2)
rotate(30)
translate(10 20) rotate(30) scale(2 3)
```

If no clearer representation is available, it falls back to:

```text
matrix(a b c d e f)
```

Use `TransformSerialize.forceMatrix` when you want the raw matrix form even if a
shorter transform expression could be detected.

```fsharp
let rawTransformAttribute () =
    Transform.translate 10.0<length> 20.0<length>
    |> TransformSerialize.toStringWith (TransformSerialize.defaultOptions () |> TransformSerialize.forceMatrix)
```

## Inspecting Paths

`Inspect` prints path data structures for debugging and tests. It is not the SVG
`d` serializer. Use `Inspect.segment`, `Inspect.subpath`, and `Inspect.path` for
readable structural output:

```fsharp
let inspectLine () =
    Line(Point.create 0.0<length> 0.0<length>, Point.create 12.0<length> 10.0<length>)
    |> Inspect.segment
```

Example output:

```text
Line(start=0,0 end=12,10)
```

Use the `Code` functions when you want copy-pasteable F#:

```fsharp
let inspectCode path = Inspect.pathCode path
```

Inspection options mirror the serializer's decimal controls: rounding, fixed
decimal places, and left padding are available through the `With` functions.

## Curve Clipping

`Clip` clips drawn geometry to a filled clipping region. This is not a filled
Boolean operation: the input path is treated as curves, and the clipping path is
treated as a filled region.

```fsharp
Clip.subpath input clipRegion Nonzero
Clip.path input clipRegion Nonzero
```

The returned subpaths contain only pieces of the original input geometry.
Boundary pieces from the clipping region are not inserted. If an open subpath
enters, exits, and re-enters the clipping region, the result contains multiple
open subpaths. If a closed circle is clipped by a rectangle, the result is the
visible arc fragments as open subpaths, not a closed rectangle-and-arc outline.

Closed inputs stay closed only when the whole subpath survives without being cut
by the clipping boundary. Pieces whose sample point is inside or on the boundary
of the clipping region are retained. Segment types are preserved where possible:
lines remain lines, Beziers remain Beziers, and arcs remain arcs after
splitting.

## Offsets, Bands, and Stroke Outlines

`Offset` constructs offsets from the original curve types. Lines and circular
arcs are offset exactly. Quadratic and cubic Beziers and non-circular arcs are
represented by fitted cubic Beziers, checked against the source's true normal
displacement, and subdivided when the fit exceeds the requested tolerance.

Positive offsets point along the visual left normal in SVG screen coordinates;
negative offsets point along the visual right normal. For example, a positive
offset of a horizontal line directed from left to right appears above that line.

```fsharp
Offset.segment segment 12.0<length>
// Result<Subpath, Offset.Error>

Offset.subpath subpath 12.0<length>
Offset.path path 12.0<length>
// Result<Path, Offset.Error>
```

A segment offset returns a `Subpath` because one source curve may require
several fitted pieces. Subpath and path offsets return a `Path`: trimming may
split one offset walk into multiple subpaths or remove it entirely.

The `With` variants accept `Offset.Options`. The join can be `Bevel`,
`Miter miterLimit`, or `Round`. `Options.Fitting` controls fitted-curve accuracy
and maximum subdivision depth. `Options.DistanceOptions` controls the projection
and root-finding tolerances used during trimming; it is not a trimming-policy
switch.

Use `Offset.subpathUntrimmed`, `Offset.pathUntrimmed`, or their `With` variants
to obtain the connected offset walks before topological trimming. These are
useful for inspection or for callers implementing a different trimming policy,
but they may retain self-intersections, reversal folds, and regions lying on the
wrong side of a closed source contour.

### Single-Offset Trimming

`SingleOffsetTrimming` controls two consecutive stages:

```fsharp
let options =
    { Offset.defaultOptions with
        SingleOffsetTrimming =
            { Offside = true
               FinalTrimming = InBandTrimming } }
```

`Offside` applies only to closed source subpaths. The source and its offset
define a signed intervening region. Face-contamination through the arrangement
graph removes offset portions lying on the wrong side of that region. Open
source subpaths have no closed source face, so this stage has no effect on them.

The final stage is selected independently:

| `SingleOffsetFinalTrimming` | Behavior |
| --- | --- |
| `NoTrimming` | Return the walks surviving the optional offside stage. |
| `CuspTrimming` | Remove side-local submerged folds whose run contains reversed offset geometry. |
| `InBandTrimming` | Apply the complete source-to-offset winding classification and parity-capacity reconstruction. This includes the effect of cusp trimming. |

The following open source has no offside stage, so the panels isolate the three
final-trimming choices:

![Single offset with no final trimming, cusp trimming, and in-band trimming](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/single_offset_final_trimming.svg)

For closed contours, `Offside` is an additional and independent operation. In
this example the source contains oppositely oriented concentric rectangles; the
final trimming mode is `NoTrimming` in both panels:

![Single offset of concentric rectangles with offside trimming disabled and enabled](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/single_offset_offside_trimming.svg)

The defaults are `Offside = true` and `FinalTrimming = InBandTrimming`.
Adjacent reversed/non-reversed loops created locally during offset assembly are
conservatively collapsed before these public trimming stages; that construction
cleanup is intentionally not a public switch.

### Two-Sided Bands

`Offset.subpathBand` and `Offset.pathBand` construct and jointly trim two signed
offsets:

```fsharp
Offset.subpathBand subpath 18.0<length> 34.0<length>
Offset.pathBand path 18.0<length> 34.0<length>
```

`inner` and `outer` are caller-assigned roles, not a numeric-order restriction.
Either ordering is accepted. Exchanging the values reverses the orientation of
the resulting band. Bands do not add endpoint caps; use `Offset.subpathStroke`
or `Stroke.subpath` when an open source needs `Butt`, `Square`, or `RoundCap`
endpoints.

Band trimming has three independent Boolean controls:

```fsharp
let options =
    { Offset.defaultOptions with
        BandTrimming =
            { InnerCusps = true
               OuterCusps = true
               InBand = true } }
```

- `InnerCusps` applies side-local cusp trimming to the caller-designated inner
  offset.
- `OuterCusps` applies the same operation to the caller-designated outer offset.
- `InBand` performs the final joint winding classification and parity-capacity
  reconstruction after the two sides are assembled.

The cusp switches act before joint band trimming. The four-concave-corner
example below holds `InBand = true` while changing the two side-local switches:

![Band trimming with both, one, and neither side-local cusp pass enabled](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/band_cusp_trimming.svg)

The figure-eight below holds both cusp switches at `true` and changes only the
final joint pass:

![Figure-eight band with in-band trimming disabled and enabled](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/band_in_band_trimming.svg)

All three band switches default to `true`. Turning a stage off is useful for
inspection and for specialized callers that want to preserve intermediate
geometry, but the result can retain reversal folds, self-intersections, or
disconnected loops that the default pipeline removes.

`Offset.subpathBandUntrimmed`, `Offset.pathBandUntrimmed`, and their `With`
variants return the two synchronized offset sides without side-local or joint
trimming. They preserve inner-then-outer ordering and add no caps or bridges.

## Stroke Outlines and Dashes

`Stroke` is a small public wrapper over the offset stroke-outline machinery. It
uses `StrokeOptions`, `StrokeCap`, and dash options rather than exposing every
offset-specific detail at the top level.

```fsharp
Stroke.segment (Line(a, b)) 2.0<length>
Stroke.subpath subpath 2.0<length>
Stroke.path path 2.0<length>

let roundStroke =
    { Stroke.defaultOptions with
        Width = 2.0<length>
        Cap = StrokeRound
        Offset = { Offset.defaultOptions with Join = Round } }

Stroke.subpathWith subpath roundStroke
```

Dash extraction uses SVG dash semantics: odd-length dash arrays are duplicated,
zero patterns produce no dashes, negative dash lengths are rejected, and the dash
offset is normalized around the total pattern length.

## Arrangement Graphs

`Arrangement` constructs a planar arrangement from one or more source paths.
Construction preserves the caller's segment geometry while progressively
splitting segments at intersections, endpoint contacts, and overlap boundaries.
The resulting atomic edges do not intersect except at endpoint clusters.
Coincident edges are stored once with forward and reverse multiplicities.

For two overlapping squares, the input boundaries cross at two points. Those
crossings become vertices, and the four original sides that pass through them
are split into atomic edges. The left panel uses one color per source subpath;
the right panel shows the resulting vertices, directed edges, winding levels,
and directional multiplicities.

![Two overlapping square subpaths and their arrangement graph](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/arrangement_graph_overlapping_squares.svg)

```fsharp
Arrangement.build [ left; right ] 0.000001<length> 0.00001<length>
```

`ArrangementGraphBuild` contains the graph and `SegmentImages`. Each segment
image records, in original path, subpath, and segment order, the graph-edge
identifiers produced from one source segment and whether each traversal reverses
the stored edge direction. An image can be empty when all pieces of an input
segment are shorter than `MinimumChord`.

The graph, vertex, and edge representations are transparent for inspection.
Vertices retain their clustered source endpoints and use the center of the
smallest circle enclosing those endpoints as their representative point. Edges
retain their segment geometry, endpoint vertex identifiers, directional
multiplicities, and bounds. Cyclic edge order around a vertex is derived from
geometry and stored as ordered groups.

Arrangement construction compares segment geometry rather than requiring
structurally equal segment values. In the following case, two equal circles run
in opposite directions. Each consists of two 180-degree arcs, but the second
circle's subdivision is shifted by 45 degrees. The graph splits the common
circle at all four source endpoints and represents each geometric edge once,
with one occurrence in each direction.

![Oppositely directed equal circles with phase-shifted arc subdivisions and their arrangement graph](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/arrangement_graph_semantic_circle_overlap.svg)

`Arrangement.build` is the supported constructor. Direct construction remains
possible for inspection, serialization, and tests, but callers then assume
responsibility for the documented graph invariants. `Arrangement.validate`
checks local representation and closed-boundary invariants that do not require
pairwise intersection tests.

`ArrangementDrawing` provides reusable drawing primitives for the transparent
graph representation. `ArrangementDrawing.drawing` shows vertices, edges, and
directional multiplicities. `ArrangementDrawing.annotatedDrawing` additionally
shows winding levels on both sides of every edge relative to a compatible source
path; it trusts that the supplied source corresponds to the graph.

## Path CSG

`Csg` performs operations on the filled point-sets represented by SVG paths. It
builds an arrangement graph, measures the winding field on either side of its
edges, applies the requested fill rule, and reconstructs the necessary boundary
cycles.

```fsharp
Csg.union left right Nonzero
Csg.intersection left right Nonzero
Csg.difference left right Nonzero
Csg.symmetricDifference left right Nonzero
```

Each function returns `Result<CsgResult, CsgError>`. For example, both products
of a union remain available without rebuilding the arrangement:

```fsharp
let output = Csg.union left right Nonzero

// output.Path is the reconstructed result.
// output.Build is the exact ArrangementGraphBuild used to compute it.
```

`CsgResult.Path` is the reconstructed output path. `CsgResult.Build` exposes the
arrangement graph and source-segment images for inspection or drawing. This
matters because endpoint clustering and segment refinement make the
arrangement's geometry the source of truth for the returned path.

Boolean operations can produce no components, one component, multiple
components, holes, or islands inside holes. Multiple subpaths in each operand
are evaluated globally. Open subpaths follow SVG fill semantics and are
implicitly closed for filling. The fill rule is part of the operation: repeated
loops, self-intersections, and nested subpaths can produce different results
under `Nonzero` and `EvenOdd`.

The following worked example uses two paths containing two rectangles each.
Every panel retains the same coordinate system: the first row shows the source
paths, their arrangement graph, union, and intersection; the second shows both
orders of difference, symmetric difference, and rounded nested contours. The
arrangement is constructed once from geometry, while each binary result
classifies its edge sectors under the selected fill rule.

Under `Nonzero`, any nonzero winding level is filled. The arrangement panel's
black numbers are the winding levels immediately to the left and right of each
directed edge; its red numbers are forward and reverse source multiplicities.

![Eight-panel ArrangementGraph CSG example using the Nonzero fill rule](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/arrangement_csg_nonzero.svg)

The same inputs and arrangement produce different Boolean boundaries under
`EvenOdd`, where winding parity determines whether a sector is filled. The final
`nestedContours` panel is unchanged because that unary operation preserves the
complete signed winding field and does not take a fill rule.

![Eight-panel ArrangementGraph CSG example using the EvenOdd fill rule](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/arrangement_csg_evenodd.svg)

For points away from a boundary:

| Operation | The point is inside the result when |
| --- | --- |
| `union(left, right)` | it is inside `left` or `right` |
| `intersection(left, right)` | it is inside both operands |
| `difference(left, right)` | it is inside `left` but not `right` |
| `symmetricDifference(left, right)` | it is inside exactly one operand |

Use the `With` variants with `CsgOptions` to choose the endpoint tolerance and
minimum atomic-edge chord. Returned segments retain their source type where
possible: lines remain lines, Beziers remain Beziers, and arcs remain arcs after
splitting.

The unary `Csg.nestedContours` operation takes no fill rule. It reconstructs
nested or disjoint unit-level contours that preserve a path's complete signed
integer winding field, rather than reducing that field to filled/unfilled
values.

## README Figures

README figures are generated through the public F# API by a non-packable console
project:

```shell
scripts/generate-readme-figures
scripts/generate-readme-figures --check
```

The source SVGs live under `docs/readme` on `main`. For package README rendering,
the same generated SVGs are copied to the repository's `markdown-assets` branch
under `figures/`, so NuGet can fetch them as public raw GitHub assets. NuGet
renders externally hosted images from an allow-list that includes
`raw.githubusercontent.com`; it does not render images by reading relative paths
inside the `.nupkg`.

## Development

```shell
scripts/test-fast          # ordinary tests, excluding the slow convex-hull suite
scripts/test-slow          # the slow convex-hull suite only
scripts/test-all           # full suite (fast then slow)
scripts/test-release       # canonical pre-release verification (full suite)
scripts/generate-readme-figures
scripts/generate-readme-figures --check
```

Without the scripts, the test project can be run directly:

```shell
dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj --filter "Category!=Slow"
```

The generated README SVGs are not part of the `SvgPath` NuGet package. The
package includes the Markdown README itself, the compiled library, XML docs,
license metadata, repository metadata, and the `FSharp.Core` dependency.
