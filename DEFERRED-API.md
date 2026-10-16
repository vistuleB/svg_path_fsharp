# Deferred API changes for 4.0.0

Review this register before every release and before adding a public type or
error variant. Keep 3.x compatible; accumulate deliberate breaking changes here.
This is a release reminder, not a claim that all audits or 4.0.0 design are complete.

| ID | Change | Status | Languages |
| --- | --- | --- | --- |
| API-001 | Public refined-boundary CSG diagnostic | Deferred to 4.0.0 | Gleam and F# |

## API-001: refined-boundary CSG error

The proposed `RefinedBoundaryNotClosed(path_index, vertex, net_flow)` (F# uses
`pathIndex`, `vertex`, `netFlow`) adds a case to the public CSG error union.
It can break exhaustive matches, so it must not ship in a 3.x patch.

The 3.x implementation **retains boundary validation** and the detailed private
failure. At the public boundary it maps this to the existing
`ArrangementGraphError(ConstructionFailed)` in Gleam or
`CsgArrangementError Arrangement.ConstructionFailed` in F#.
No caller tolerance or minimum-length setting is silently adjusted. The reduced
FL004 regression still requires an error at default minimum length and a correct
intersection when the caller explicitly retains shorter pieces.

### Preserved implementation

- Local branch in each repository: `codex/v4-api`.
- The focused commit on that branch restores the public case, its documentation,
  and the regression assertions checking operand index and nonzero net flow.
- Original implementation history: Gleam `bf59110d`; F# `c7a6e52`.
- Corresponding register in the sibling repository: `../svg_path/DEFERRED-API.md`.

The branch is a shelf for deferred commits, not a parallel release line. It does
not receive ordinary fixes. Do not merge its old base wholesale into a future
release: cherry-pick/reapply each approved focused commit onto the current main
branch and resolve any drift. Neither branch is pushed automatically.

### 4.0.0 adoption checklist

1. Reconsider the public diagnostic alongside other deferred API changes.
2. Apply API-001 in both languages; restore operand/vertex/net-flow documentation
   and the diagnostic assertions. Keep the existing boundary guard.
3. Add the case to the 4.0.0 migration guide and changelog, explaining zero-based
   operand indices and incoming-minus-outgoing flow. Remove the 3.x deferral note.
4. Run both canonical release suites and check affected public documentation.
5. Mark this register implemented in both repositories, with commit references.

Until then, release preparation must confirm that the public error union still
matches 3.1.3. Other audit coverage gaps remain tracked in the audit records.

## Compatibility validation (2026-10-16)

- Public CSG error union compared with the 3.1.3 release preparation: unchanged.
- Gleam: `gleam test` passed 2,038 main tests.
- F#: `dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj --filter
  'FullyQualifiedName~Csg|FullyQualifiedName~FlattenBoundary|FullyQualifiedName~FlattenArcOverlap'`
  passed 54 targeted tests.
- These checks validate the compatibility adjustment. Canonical full release
  verification remains part of the subsequent release-preparation step.
