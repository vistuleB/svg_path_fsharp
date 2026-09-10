# F# public API / error-contract review — 2026-09-10

This is an F#-first review, separate from the join parity changes. The public
type-ownership finding has since been resolved as recorded below; the remaining
findings have not changed behavior. This is a first pass, not a completed
whole-library algorithm audit.

## Scope and reproducibility

- Reflection over every exported error union: 37 unions. All payload fields
  have names; no Item/ItemN payloads found.
- Focused inspection of Offset/Stroke join validation and public error mapping,
  Affine/Transform point-correspondence construction, TransformParse/Parse
  errors, NumberFormat/Serialize options, Curvature options, and internal Root
  as a dependency of the new joins. Not every function in these files was
  re-audited numerically.
- `dotnet fsi examples/debug/public_api_audit.fsx` records the reflection and
  runtime probes below. It uses the ordinary built production assembly.
- Fast profile after join changes: 1827 passed. See GLEAM_SYNC.md for exact
  generation/test commands and the source commit order.

## 1. Point-correspondence wrappers erase useful errors

`Transform.pointPairSimilarity` and `Transform.pointTripleMap` delegate to
Affine, then map every construction error to unit. Their tolerance failure
also returns unit. In contrast, Affine supplies DegenerateSourcePair,
DegenerateSourceTriple and NonFiniteTransform.

Confirmed with coincident source points: Affine returns
`Error DegenerateSourcePair`, Transform returns `Error ()`. Negative tolerance
also gives `Error ()`. A caller cannot distinguish invalid tolerance,
degeneracy, non-finite construction, or excessive residual.

Recommendation: agree on a typed wrapper error retaining Affine.Error plus
invalid tolerance / residual failure before changing it. This is not a port
mistake: Gleam transform.gleam also explicitly returns Result(Matrix, Nil).
Do not silently change only F# if parallel contracts remain a goal.

## 2. Namespace-wide operation names — resolved

The user selected symmetry with Gleam: operation-owned types now live inside
their modules, including `Offset.Error`, `Offset.Options`, `Stroke.Error`,
`Stroke.Options`, `Arrangement.Error`, and `Affine.Error`. Shared geometry types
remain namespace-level. No compatibility aliases preserve the former root names.
PublicTypeLayoutTests checks module ownership and the absence of obsolete aliases.

## 3. Internal polynomial root units are too narrow for the new caller

Root.quadratic returns float<parameter>, while ArcsJoin solves for a radius
adjustment with length units. The port explicitly expresses coefficients in
one user-space length unit and restores the result's length. Same coefficients,
same solver, same root selection as Gleam.

Root is internal (confirmed by reflection); this is not a public API leak.
Recommendation: a future scalar-unknown root API could avoid the conversion,
but it does not require an immediate change to Gleam's unmeasured Float API.

## Checks without an issue found

- MiterClip and Arcs reject zero/negative limits in the 23 source parity tests.
  Runtime probes additionally reject NaN and infinity even for an empty Path,
  preserving StrokeOffsetError(InvalidMiterLimit value).
- Arcs construction failure remains a private detail mapped to public
  ConstructionFailed, as in Gleam. No new internal type is exported.
- Stroke's documented trimming override matches the implementation: inner and
  outer cusp trimming disabled, final in-band trimming enabled. Band owns caps.
- Named error payload convention holds across all exported error unions.
- NumberFormat clamps decimal-place settings rather than exposing unchecked
  .NET format precision; it uses invariant culture for numeric output.

No new ordinary-geometry failure was confirmed by this first audit pass.
