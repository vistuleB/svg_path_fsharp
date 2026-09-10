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

Next: `7dbfee4` (projection/proximity/interpolation arithmetic).
