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

Next: `0bdac79` (minimum-width strip `direction` → `normal`).
