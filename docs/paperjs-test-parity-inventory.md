# Paper.js test coverage: Gleam and F#

## Follow-up implementation

The four focused gaps below now have ports in
`tests/SvgPath.Tests/PaperCrossings1409SlowTests.fs` and
`tests/SvgPath.Tests/PaperIntersectionAddressesSlowTests.fs`, each marked
`Category=Slow`. Test names are `Paper 1409 assertion 2`,
`Paper 1409 assertion 3`, `Paper address 12` and `Paper address 19`.
`Paper Boolean fixture 107 geometry` now includes the same 7×7 containment
grid as Gleam and remains in the fast profile. All 151 coordinate pairs in
the four new tests were mechanically compared with the Gleam sources and
match exactly, including the expected intersection points.

The inventory below records the **pre-port baseline**. Its missing-coverage
statuses are historical; the broader fast-case mapping remains outstanding.
No production code changes are part of this follow-up.

Validation: the following targeted command passed all 10 tests (four new slow
tests and six Boolean cases, including the expanded fixture 107):

```shell
dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj --no-build --filter 'FullyQualifiedName~PaperCrossings1409SlowTests|FullyQualifiedName~PaperIntersectionAddressesSlowTests|FullyQualifiedName~PaperBooleanRelease310Tests'
```

The build succeeded; the initial sandboxed test launch could not bind the
runner's local socket, so the successful test run used approved execution
outside the sandbox. README recipe consistency and whitespace checks passed.
`scripts/test-slow` also passed all 30 tests (26 existing convex-hull cases plus
the four new Paper.js cases), with no skips. The full fast suite and figure
generation were not rerun for this test-only change.

Source review: 2026-10-15. Gleam HEAD `d6c7b631`, F# HEAD `31d9ad09`,
including the working trees used for the timing baseline. This review does not
run new fixtures or establish implementation failures.

## Scope and conclusion

The five Paper.js adaptations added to Gleam's slow suite are **not five
equivalent tests moved into F# fast**. One has the full input geometry and most
assertions in F# fast; four lack their fixture-specific checks. The latter have
related coverage, described below, which must not be counted as equivalent.

This is a focused assertion-level inventory of those five cases, plus a broader
inventory of explicitly identified Paper.js ports. It is not an assertion-by-
assertion comparison of all native tests or all 232 Gleam Paper.js adaptations.

Both ports use Paper.js source revision
`92775f5279c05fb7f0a743e9e7fa02cd40ec1e70`; the Gleam extraction runtime was
`paper@0.12.18`. Exact download dates were not recorded. The upstream archive
was consolidated on 2026-10-14. See the neighboring Gleam repository's
`scripts/paperjs/catalog.json` and `INDEX.md` for provenance and the full index.

## The five slow cases

Gleam filenames below are under `test_slow/`; F# filenames are under
`tests/SvgPath.Tests/`. References use test names rather than line numbers.

| Gleam file and test | F# coverage | Disposition |
| --- | --- | --- |
| `svg_path_paper_boolean_full_overlap_slow_test.gleam`: `paper_boolean_full_overlap_107_test` | `PaperBooleanRelease310Tests.fs`: `Paper Boolean fixture 107 geometry` | Same input cubics, Nonzero union, expected SVG, area tolerance and bidirectional boundary sampling. F# omits the 7×7 containment grid. Runs in fast. |
| `svg_path_paper_crossings_1409_slow_test.gleam`: `paper_1409_assertion_2_test` | Related overlap-removal and classification unit tests only | Missing combined test on identical closed squares represented by endpoint-repeated cubics. |
| Same file: `paper_1409_assertion_3_test` | Related overlap-removal and classification unit tests only | Missing combined test on identical four-cubic circles. |
| `svg_path_paper_intersection_addresses_slow_test.gleam`: `paper_address_12_test` | Related encounter/address unit tests only | Missing this shared-arc fixture's four expected addresses and classifications. |
| Same file: `paper_address_19_test` | `PaperBooleanRelease310Tests.fs`: `Paper Boolean fixture 79 geometry` uses the same two input contours | Geometry is present, but CSG output checks do not assert this fixture's seven intersection addresses or classifications. Those checks are missing. |

### What the missing assertions protect

Both #1409 cases require all of the following together:

- The point-only intersection query reports `OverlappingSegments`.
- Encounters returns nonempty overlap and intersection lists.
- Every grouped intersection classification is `Touching`; `Indeterminate`
  is not accepted as a non-crossing result.
- Removing redundant intersections preserves the overlaps and removes all
  reported point intersections.

Address fixtures 12 and 19 require overlap-aware encounters, expected point and
left-segment/parameter matches, and crossing versus non-crossing classification.
They also reconstruct every returned left/right address, checking agreement
within the default tolerance, and reject indeterminate classifications.
Fixture 12 deliberately adapts shared-arc endpoints to our local Touching
contract. Neither a successful union nor generic overlap tests establish these
properties on the same inputs.

Fixture 107's omitted grid compares Nonzero containment on 49 candidate points,
skipping points within 0.001 of either output boundary. Area and sampled boundary
agreement are useful checks, but do not replace this filled-region check.

### Related coverage that is not a substitute

`EncountersTests.fs` includes:

- `subpath intersection entirely explained by overlap is removed`: straight
  segments and an explicitly constructed intersection record.
- `point at overlap boundary through adjacent alias fixture`: two-segment line
  paths and removal of their overlap-covered endpoint.
- `isolated intersection elsewhere in same query fixture`: line paths retaining
  an isolated intersection while removing an overlap-covered one.

`IntersectionsTests.fs` includes
`grouped intersection expands parameter cartesian product`, which checks four
expanded combinations for horizontal/vertical lines. It does not perform the
#1409 cubic-contour classification check.

`Release310RegressionTests.fs` explicitly describes its tests as reduced
witnesses. For example, `Paper projected cubic witnesses retain geometric
crossings` checks five segment/ray cases, root parameters and residuals. Those
are valuable bug regressions, not replacements for full path encounters.

## Broader inventory and limits

Counts are test functions, not assertion counts or coverage percentages.

| Upstream family | Gleam fast / slow | Explicit F# port evidence |
| --- | ---: | --- |
| `Matrix.js` | 52 / 0 | 52 named cases, IDs 0–51, in `AffineDecompositionTests.fs`; documented ±180-degree adaptation. |
| `Path_Boolean.js` | 34 / 1 | Six full-input geometry cases: 79, 113, 107, 122, 123, 172 in `PaperBooleanRelease310Tests.fs`; native reduced witnesses separately. Full assertion equivalence is not claimed. |
| `Path_Intersections.js` | 50 / 4 | Four slow checks assessed above; no matching fixture-specific assertion coverage. Fast cases not exhaustively mapped here. |
| `Curve.js` | 58 / 0 | Not exhaustively mapped against native F# tests in this review. |
| `Path.js` | 20 / 0 | Not exhaustively mapped against native F# tests in this review. |
| `PathItem_Contains.js` | 13 / 0 | Not exhaustively mapped against native F# tests in this review. |

Do not subtract the 58 explicitly named F# cases from 232 and report the
remainder as missing: native F# tests may cover some of those assertions, and
the Boolean and matrix adaptations do not necessarily have identical assertion
sets. The existing [migration report](release-3.1.0-migration.md) already states
that F# does not execute the entire upstream audit inventory.

## Method and benchmark interpretation

Reviewed input geometry, operations and helper assertions in the five Gleam
tests; searched F# test sources by upstream identifiers, distinctive coordinates
and the classification/removal APIs; inspected candidate counterparts and their
helpers. In particular, address 19's coordinate match led to Boolean fixture
79, rather than an incorrect claim that its geometry was entirely absent.

The relevant F# files are included by `SvgPath.Tests.fsproj`. Only
`ConvexHullSlowParityTests.fs` has the `Category=Slow` trait; the scripts select
`Category=Slow` and `Category!=Slow`. Thus the slow-suite counts (Gleam 31,
F# 26) reflect both profile choices and missing fixture-specific coverage.

The recorded 3.1.0 median for F# Boolean 107 is about 7.185 seconds, charged to
fast. Gleam's five added slow tests total about 24.661 seconds of individual
medians. Neither number is a cross-language performance comparison on identical
assertions. No test or implementation changes were made for this inventory.

## Recommended follow-up

1. Port the two #1409 and two address tests with their complete assertions into
   dedicated F# Paper.js files; initially classify those files as slow.
2. Add fixture 107's containment grid. Keep its profile decision separate from
   whether its assertions match.
3. Then map the remaining fast Paper.js adaptations by input, operation and
   assertions, retaining separate statuses for full cases, reduced witnesses,
   related generic coverage and confirmed omissions.

If a newly ported test fails, inventory and investigate it before changing the
implementation. This review itself has found no new runtime failure.
