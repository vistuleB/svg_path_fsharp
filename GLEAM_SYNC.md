# Gleam commit-by-commit synchronization

Baseline: F# `c612b7b`, corresponding to Gleam `29ce1db` (curvature public
diagnostics/docs). Work proceeds in `git log --reverse 29ce1db..a251698` order:
109 source commits. Uncommitted Elizabeth experiments are not in this queue.
Do not substitute the final Gleam tree for the historical source commit.

## Progress

- `87e7e75` — signed-zero predicates: explicit zero-vector heading; unit-aware
  `InternalNumber.isZero`; eight one-to-one regression tests. .NET comparisons
  already equate both zero signs, so the other predicate replacements require
  no behavioral change. Focused `dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj
  --filter FullyQualifiedName~SignedZeroTests`: 8 passed. `scripts/test-fast`:
  1568 passed (baseline 1560).

- `0bdac79` — minimum-width strip `Direction` → `Normal`, including its unit
  normal contract. `WidthExtremum.Direction` remains unchanged, as in Gleam.
  Validation: `scripts/test-fast`: 1568 passed.

- `2d71317` — source-parameter-ordered longitudinal protrusions, preservation of
  endpoint anchors, and removal of hull piece compaction. Added all 12 source
  regression tests one-to-one. `scripts/test-fast`: 1560 passed, 20 failed.
  This is an intentionally failing historical checkpoint: Gleam recorded 1370
  passed, 18 failed. Failures include hull assembly and its downstream offset
  callers; do not invent repairs here. The committed repair is `21c8b7f`.

- `1584d30` — vector support with search rescaling and raw dot-product output;
  roundoff-adjusted lower bounds applied before irreversible interval pruning.
  All eight source regressions added one-to-one. `scripts/test-fast`: 1568
  passed, the same 20 historical hull/downstream failures remain.

- `c40509a` — source-geometry strip seed using three farthest-point passes,
  raw normal support queries, and actual segment widths; wired into thin-prefix
  decisions. All five source regressions added one-to-one. `scripts/test-fast`:
  1573 passed, the same 20 historical hull/downstream failures remain.

- `21c8b7f` — distinguish FullLoop, OnePoint, and Portion; reuse SubpathParameter
  and normalize exact endpoint aliases without swallowing short portions.
  All five source regressions added one-to-one. `scripts/test-fast`: 1598 passed;
  `scripts/test-slow`: 26 passed. All 20 intermediate failures are resolved.

- `8f8173d` — exact endpoint branches in Point.interpolate, with three one-to-one
  regressions (signed-zero coordinates checked by bits on .NET).
  `scripts/test-fast`: 1601 passed.

- `2854585` — canonicalize rounded full turns after heading/aperture arithmetic;
  three source regressions added one-to-one. `scripts/test-fast`: 1604 passed.

- `dd7362f` — public EndpointPolicyContext, forward input flags, repeated
  closure policy application, adapted Effects/Offset callers and README.
  Nine source regressions added one-to-one. `scripts/test-fast`: 1613 passed.

- `9215e47` — discard only exact constant segments in hull/tangent reconstruction;
  retain short nonconstant curves and arc errors. Three source regressions added
  one-to-one. `scripts/test-fast`: 1616 passed.

- `0590b19` — README traversal flags, deletion behavior, and immediate replacement
  validation clarified. Documentation-only; `git diff --check` passes.

- `297fe29` — rename WiggleThenBridge and its configurable constructor to
  WiggleElseBridge throughout source, tests, and README. `scripts/test-fast`:
  1616 passed.

- `ba7159d` — cheap segment/subpath/path length upper bounds, README docs, and
  seven source regressions one-to-one. `scripts/test-fast`: 1623 passed.

- `e341c79` — replace unbounded displacement swallowing with balanced runs
  bounded by three tolerances; preserve source/portion endpoints. Eight source
  regressions added one-to-one. `scripts/test-fast`: 1631 passed.

- `3c072a9` — checked product/sum validate actual finite results instead of
  rounded preflight bounds. The Erlang-only exception wrapper has no .NET role.
  Five source regressions added one-to-one. `scripts/test-fast`: 1636 passed.

- `ada3613` — rescale vectors before normalization and divide coordinates
  directly. Three source regressions added one-to-one. `scripts/test-fast`:
  1639 passed.

- `7dbfee4` — stable distributed projection, tolerance-scaled proximity, and
  opposite-sign weighted interpolation; checked sums preserve F# units.
  Five source regressions added one-to-one. `scripts/test-fast`: 1644 passed.

- `f8922a9` — signed remainder before degree trigonometry; precomputed conversion
  factors avoid intermediate overflow. Replaced the old large-angle source test
  and added its three new regressions. `scripts/test-fast`: 1647 passed.

- `7227f07` — carry vanishing-derivative evidence with root candidates, preserve
  it during deduplication, and accept exact zeros on the final bisection step.
  Six source regressions added one-to-one. `scripts/test-fast`: 1653 passed.

- `72add87` — exact Bezier evaluation and split endpoints, with two source
  regressions one-to-one. Bezier now retains its own interpolation helper, as
  Gleam does, instead of inheriting Point's separate arithmetic policy.
  `scripts/test-fast`: 1655 passed.

- `1d32ade` — canonical signed-zero helper and Bezier split deduplication.
  Three source regressions added one-to-one. `scripts/test-fast`: 1658 passed.

- `ba24ca4` — canonical signed-zero segment parameters and subpath addresses.
  Seven source regressions added one-to-one. `scripts/test-fast`: 1665 passed.

- `8d3dd27` — either signed-zero dash pattern is continuous; one source
  regression added. `scripts/test-fast`: 1666 passed.

- `ac66040` — ellipse split parameter canonicalization and zero-direction
  predicates. Two source regressions added. `scripts/test-fast`: 1668 passed.

- `5f6eddf` — offset tangent, score, boundary, and offside predicates explicitly
  accept either zero sign. One source regression added. `scripts/test-fast`:
  1669 passed.

- `a4a80c0` — zero length requests, arc radii, and scalar guards accept both
  zero signs. Subpath length lookup delegates to the same segment guard.
  Two source regressions added. `scripts/test-fast`: 1671 passed.

- `e69b71b` — either signed-zero primitive size disables rendering. One source
  regression added. `scripts/test-fast`: 1672 passed.

- `22b8376` — curvature and bisection zero predicates; one source regression
  added. `scripts/test-fast`: 1673 passed.

- `7d735a0` — canonical transform entries before compact serialization.
  One source regression added. `scripts/test-fast`: 1674 passed.

- `e5c1399` — verified Bezier quadratic-extrema denominator and cubic
  self-intersection cross determinant already accept both signs through .NET
  equality. Semantic no-op; no source tests added. Last unchanged-code
  `scripts/test-fast`: 1674 passed; `git diff --check` passes.

- `b6655e5` — Area.edgeYAt already detects either signed-zero denominator
  through .NET equality. Semantic no-op; source adds no tests. Last
  unchanged-code `scripts/test-fast`: 1674 passed; `git diff --check` passes.

- `aed3d23` — circumcircle determinant equality already accepts both zero
  signs on .NET. Semantic no-op; no source tests added. Last unchanged-code
  `scripts/test-fast`: 1674 passed; `git diff --check` passes.

- `5a1a9da` — hull arriving/leaving tangent and curvature quadratic-coefficient
  predicates already use .NET signed-zero equality. Semantic no-op; no source
  tests added. Last unchanged-code `scripts/test-fast`: 1674 passed;
  `git diff --check` passes.

- `75175fe` — Cut.atParameters already canonicalizes every address through
  Subpath.parameterCanonicalize before endpoint filtering, so the source's
  additional local normalization is redundant here. Verified call chain;
  semantic no-op. Last unchanged-code `scripts/test-fast`: 1674 passed;
  `git diff --check` passes.

- `078868f` — Clip's early endpoint filter uses .NET equality, which already
  equates both zero signs. Clip.uniqueParameters and cut reconstruction also
  canonicalize through Subpath.parameterCanonicalize afterward. Source's local
  normalization is semantically redundant. Last unchanged-code
  `scripts/test-fast`: 1674 passed;
  `git diff --check` passes.

- `c01da6a` — normalize snap candidates before endpoint-ranking, and accept
  either zero sign in arc-radius guards. Source adds no tests.
  `scripts/test-fast`: 1674 passed.

- `89a4617` — canonical signed-zero overlap endpoint aliases; one source
  regression added. `scripts/test-fast`: 1675 passed.

- `838b78d` — closed endpoint and endpoint/interior cubic self-intersections
  preserve known boundary parameters, validate coordinate roots geometrically,
  and deduplicate validated pairs. Five source regressions added one-to-one.
  `scripts/test-fast`: 1680 passed.

- `fd10a21` — reject endpoint-only tangent fits without handle information;
  document the contract and add the source regression. `scripts/test-fast`:
  1681 passed.

- `fddb38e` — preserve collapsed-line traversal and exact monotone endpoints;
  enumerate every multi-turn projection extremum. Rescaled eigenvector
  normalization is already supplied by Point.normalize. Five source regressions
  added; two source expectations and one additional F# expectation updated for
  the new traversal contract. `scripts/test-fast`: 1686 passed.

- `25c25d9` — replace sampled cusp discovery with polynomial curvature extrema,
  stationary-velocity partitions, touching-root checks, and crossing bisection.
  Geometry units are retained in polynomial coefficients; derivative parameter
  units are restored when evaluating the normalized residual. All eight source
  regressions added one-to-one. `scripts/test-fast`: 1694 passed.

- `2b9eda3` — explicit remaining-bracket depth error, checking exact-root and
  interval success first. Three source regressions added. `scripts/test-fast`:
  1696 passed, one failed: the historical zero-tolerance success expectation.
  This same checkpoint is corrected by source `1045251`; do not alter it early.

- `a2caa2d` — remove sampled radius bands, their type, Samples option, and
  validation error. Remove corresponding assertions from the existing bundled
  F# tests; source's removed test was not a standalone F# test. Offset already
  uses defaultOptions and needs no constructor change. `scripts/test-fast`:
  1696 passed, the same historical zero-tolerance expectation fails.

- `1045251` — replace zero-tolerance success expectation with validation of the
  unresolved sign-changing bracket. `scripts/test-fast`: 1697 passed; the
  historical intermediate failure is resolved.

- `eda3e14` — document visual signed-area orientation and separate internal
  arrangement-merging tolerance. Documentation-only; `git diff --check` passes.

- `e6eae1b` — fixed supports remain boundary supports in enclosing-circle
  construction; computed radii include every support's rounded distance.
  Two source regressions added. `scripts/test-fast`: 1699 passed.

- `6082627` — retain better projection candidates at isolation endpoints when
  there is no sign bracket. Both source regressions added.
  `scripts/test-fast`: 1701 passed.

- `d4aaffd` — validate both supplied overlap endpoint pairs before interior
  samples. Both source regressions added. `scripts/test-fast`: 1703 passed.

- `602d2f0` — retain endpoint alternatives and reject non-affine candidates
  only when accepted correspondences do not cover their domains. Both source
  regressions added; corrected the old F# full-overlap assertion helper to use
  actual endpoints and the source's exact assertions. `scripts/test-fast`:
  1705 passed.

- `2fd732c` — gather geometrically verified coordinate-root addresses and
  explicit endpoints, retaining projection for approximate matches. Preserve
  failures unless a coordinate exhausts its degree bound; require reciprocal
  containment for rejected affine candidates. Source regression added.
  `scripts/test-fast`: 1706 passed.

- `cf70b23` — retain arc midpoint transverse extent in fitting clouds; pass
  transform family through container clouds and allow reflected sweep for
  affine fitting only. Documentation and both source regressions ported.
  `scripts/test-fast`: 1708 passed.

- `b5a6cab` — preserve directly transformed endpoints in collapsed arc
  subpaths, reconstructing only interior extrema trigonometrically. Ported
  documentation and source regression. `scripts/test-fast`: 1709 passed.

- `d92b75e` — document overlap-window partitioning, off-diagonal pairs, and
  encounter filtering/order. Documentation-only; `git diff --check` passes.

- `918e167` — compute the minor eigenvalue from the axis determinant and
  reject exactly singular input maps before rounded axis extraction. Both
  source regressions ported. `scripts/test-fast`: 1711 passed.

- `7345278` — include implicit fill-closing lines in CSG arrangements and
  pass arrangement tolerance into winding queries, including nested contours.
  Three source regressions added. `scripts/test-fast`: 1714 passed.

- `3351438` — start unrounded rectangles at (x,y) when either effective
  radius is zero. Source regression and docs ported. `scripts/test-fast`:
  1715 passed.

- `3c419ab` — separate numeric groups by their final-token-safe rule,
  allowing only a leading sign to omit whitespace. Source regression added.
  `scripts/test-fast`: 1716 passed.

- `3f1a135` — separate repeated numeric arguments under AtSubpaths newline
  mode. Source regression added. `scripts/test-fast`: 1717 passed.

- `0b605ab` — serialize coincident-endpoint arcs directly rather than
  inventing full-loop geometry. Updated source regression and added zero-radius
  termination regression. `scripts/test-fast`: 1718 passed.

- `61bbbe3` — leave untouched segments out of corner-overlap resolution and
  inverse-length reconstruction. Source regression added. `scripts/test-fast`:
  1719 passed.

- `fd9d26a` — return usable empty Lines for endpoint Arc splits, retaining
  the original Arc. Source regression and docs added. `scripts/test-fast`:
  1720 passed.

- `1752c36` — subtract ellipse-axis rotation before recovering circular-arc
  intersection parameters. Source regression added. `scripts/test-fast`:
  1721 passed.

- `787c6d7` — close clipping-region subpaths with Bridge for encounter
  discovery, leaving subject curves open. Source regression and docs ported.
  `scripts/test-fast`: 1722 passed.

- `ed500a2` — restore original subpaths if all pieces survive incidental
  boundary cuts. Source regression added. `scripts/test-fast`: 1723 passed.

- `842ca9a` — document nonnegative distance tolerance and one-pass radius
  adaptation. Documentation-only; `git diff --check` passes.

- `93b5880` — clarify greedy clustering's insertion-order dependence. F# had
  no misplaced parity docs on atomic insertion. `git diff --check` passes.

Next: `ddad3c4` (offset provenance documentation).
