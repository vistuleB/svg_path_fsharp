# SvgPath

`SvgPath` is an F# library for SVG path parsing, serialization, transforms,
curve geometry, intersections, arrangements, Boolean operations, offsets,
bands, and strokes. It is a behavior-preserving port of the Gleam
[`svg_path`](https://github.com/vistuleB/svg_path), with F# units of measure
added to audit scalar usage.

The package targets .NET 9. Its public API may change while the mechanically
faithful port is refined into a more idiomatic F# library.

## Install

```shell
dotnet add package SvgPath --version 0.1.0
```

```fsharp
open SvgPath

let result =
    Parse.path "M 0 0 L 20 0 L 20 20 Z"
    |> Result.mapError (sprintf "parse error: %A")
    |> Result.bind (fun path ->
        Offset.path path 2.0<length>
        |> Result.mapError (sprintf "offset error: %A"))
    |> Result.map Serialize.path
```

Most geometric operations return `Result`, keeping invalid geometry,
numerical failures, and violated topology assumptions explicit.

## Numeric and path model

Public geometry uses SVG user-space coordinates, where positive y points down.
Positive signed offsets use the visual-left normal. Clockwise and
counterclockwise APIs likewise refer to visual SVG orientation.

- `float<length>` represents coordinates, distances, radii, and tolerances.
- `float<length^2>` represents squared lengths and areas.
- `float<parameter>` represents normalized curve parameters.
- `float<degree>` and `float<radian>` represent angles.

Curve parameters are dimensionless mathematically, but nominally measured in
F#. Derivatives retain parameter powers such as
`Point<length / parameter>` and `Point<length / parameter^2>`.

The core model is `Point`, `Segment`, `Subpath`, and `Path`. Segments may be
lines, quadratic or cubic Béziers, or endpoint-form elliptical arcs. Subpaths
retain an explicit start and closure flag. Construction checks continuity
according to an `EndpointPolicy` rather than silently changing disconnected
geometry.

`Parse.path` reads SVG path data. `Serialize.path` writes it, with options for
relative commands, precision, whitespace, horizontal/vertical and smooth
commands, and command repetition. Empty and zero-length subpaths remain
representable; rendering depends on closure and line-cap semantics.

![Zero-length closepath behavior](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/zero_length_closepath_probe.svg)

## Geometry

`Segment`, `Subpath`, and `Path` expose evaluation, directions, derivatives,
bounding boxes, length, splitting, splicing, reversal, linearization,
projection, and similarity remapping. `Intersections` computes intersections
and closest-point pairs. `Overlaps` handles parameter correspondences for
coincident portions; `Encounters` and `Cut` expose ordered encounters and cuts.

Supporting modules include `Bezier`, `Ellipse`, `Curvature`, `Root`, `Area`,
`WindingField`, `ConvexHull`, `SmallestEnclosingCircle`, `Congruency`,
`BasicShapes`, `Transform`, `Degeneracy`, `Effects`, `Marker`, and `Clip`.

## Arrangements and CSG

`Arrangement.build` nodes paths into a planar graph while preserving
source-to-edge images, directional multiplicities, and cyclic edge orders.
`ArrangementDrawing` provides diagnostics.

![Arrangement of overlapping squares](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/arrangement_graph_overlapping_squares.svg)

![Coincident circle geometry](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/arrangement_graph_semantic_circle_overlap.svg)

`Csg` computes union, intersection, difference, symmetric difference, and
nested contours under either SVG fill rule.

![Nonzero CSG](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/arrangement_csg_nonzero.svg)

![Even-odd CSG](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/arrangement_csg_evenodd.svg)

## Offsets, bands, and strokes

`Offset.path` constructs a signed single offset. `Offset.pathBand` constructs
the region between two signed offsets and accepts either inner/outer ordering.
`Offset.pathStroke` constructs a stroke through the same curve-fitting and
arrangement machinery. Subpath and configurable `With` variants are provided.

`Offset.Options` controls fitting tolerance, sampling and refinement depth,
joins, tangent healing, stalled-offset handling, and trimming.
`Offset.defaultOptions` supplies the normal policy.

### Single-offset trimming

`SingleOffsetTrimming.Offside` enables closed-subpath trimming by signed offset
side. `FinalTrimming` selects `CuspTrimming`, `InBandTrimming`, or
`NoTrimming`. In-band trimming is the default and includes cusp cases.

![Single-offset final trimming](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/single_offset_final_trimming.svg)

![Single-offset offside trimming](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/single_offset_offside_trimming.svg)

### Band trimming

`BandTrimming` independently controls `InnerCusps`, `OuterCusps`, and the final
band-wide `InBand` pass.

![Band cusp trimming](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/band_cusp_trimming.svg)

![Band in-band trimming](https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/v0.1.0/docs/readme/band_in_band_trimming.svg)

## Module map

| Area | Modules |
| --- | --- |
| Core | `Point`, `SvgPath`, `BasicShapes`, `Inspect` |
| Curves | `Bezier`, `Ellipse`, `Curvature`, `Root`, `Trig` |
| Input/output | `Parse`, `Serialize`, `Svg`, `Format` |
| Editing | `Affine`, `Transform`, `TransformParse`, `TransformSerialize`, `Degeneracy`, `Effects`, `Marker`, `Clip` |
| Relationships | `Intersections`, `Overlaps`, `OverlapDetection`, `Encounters`, `Cut`, `Congruency` |
| Topology | `Area`, `WindingField`, `Arrangement`, `ArrangementDrawing`, `Csg` |
| Enclosures | `ConvexHull`, `SmallestEnclosingCircle` |
| Derived paths | `Offset`, `Stroke` |

## Development

```shell
dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj
scripts/generate-readme-figures
scripts/generate-readme-figures --check
```

README figures are generated through the public F# API by a non-packable
console project. The generated SVGs live under `docs/readme`; neither the tool
nor those assets are part of the `SvgPath` NuGet package. Every generated SVG
starts with a white rectangle covering its complete view box.
