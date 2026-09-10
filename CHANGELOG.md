# Changelog

## Unreleased

- Synchronized core geometry with Gleam `svg_path` v0.46.0 (`ed94708`),
  preserving commit-by-commit traceability in `GLEAM_SYNC.md`.
- General curve intersections now use the bounded Elizabeth beam with endpoint
  discovery, polygonal exclusion, cached scoring, spatial selection and terminal
  Newton refinement. Removed superseded intersection routes; distance projection
  retains its separate minimizer. Numerical candidates are not root certificates.
- Added `Segment.boundingPolygon` and `Segment.boundingPolygonBetween`, returning
  visually clockwise convex enclosures, including corrected elliptical arcs.
- Offset classification uses signed dual-face winding propagation. Final band
  contours follow filled-face boundaries and preserve kissing seams/retraces.
  Single offsets no longer receive a nesting-based final orientation pass.
- Stroke construction delegates to symmetric bands. Open bands retain caps when
  final trimming is disabled; explicit untrimmed-band helpers return uncapped sides.
- Removed unused `Options.DistanceOptions`; replaced internal string hull-repair
  modes with typed cases and removed obsolete cusp/containment plumbing.
- Hardened arrangement self-intersections, same-source comparisons, source-interval
  tracking and sweep-based dual construction; preserved normalization extrema,
  source endpoints and explicit full-loop/point/portion hull semantics.
- Ported scalar/parameter, root, curvature, overlap, projection, parsing,
  serialization, clipping and transformation fixes with their source regressions.
- Updated endpoint policies to richer Custom context and `WiggleElseBridge` names;
  short-source normalization uses bounded balanced runs and length upper bounds.
- Gallery generation now runs isolated workers concurrently, reports failures and
  timings, and captures the second-offset arrangement from production data.
- Refreshed documentation and locally generated README/Gallery assets. Asset-tag
  publication and a new F# package version remain release-preparation steps.

- Cusp discovery now partitions at curvature extrema and reports depth
  exhaustion with its remaining parameter bracket.
- Removed unused sampled near-radius band discovery, `CurvatureBand`,
  `CurvatureOptions.Samples`, and its validation error. Pointwise radius
  proximity and cusp discovery remain available.

## 0.5.0

- Allowed `0.0` tolerance for degenerate-line normalization, collapsing only
  exactly-collinear windows; negative and non-finite tolerances remain
  rejected.
- Allowed `0.0` for `RoundCornerOptions.DistanceTolerance`, so a zero
  tolerance rounds corners exactly and drops only corners whose radius or trim
  is exactly zero; negative and non-finite tolerances remain rejected.
- Rebased `AdaptRadius` corner rounding onto a single feasibility-bounded
  scale pass instead of a convergence check, removing the epsilon comparison
  and iteration limit.
- Aligned too-small corner trims with the Gleam implementation: under
  `ErrorOnFailure`, a corner whose radius or trim is at or below the distance
  tolerance now fails with `CannotRoundCorner` instead of being silently
  skipped.
- Added regression tests for exact zero-tolerance normalization, zero-
  tolerance corner rounding, exact trim-consume overlap failures, and an
  inward square-spiral adapt-radius collapse.

## 0.4.0

- Oriented Boolean boundary output so outer contours are traced clockwise with
  filled space on the right of each directed boundary edge for `union`,
  `intersection`, `difference`, and `symmetricDifference`.
- Collapsed corner-pinch unions (two filled regions meeting at exactly one
  point, such as squares touching at a corner) into a single self-touching loop
  instead of two degenerate loops, following the same filled-sector pairing.
- Added corner-pinch union regression tests covering reversed operand
  orientations, the opposite touch diagonal, and both combined.

## 0.3.0

- Hardened endpoint reconciliation across degeneracy, convex-hull, and offset
  survivor-chain rebuilds, using strict joins where replacement geometry is
  exact and tolerant joins where evaluation noise remains.
- Reclassified post-repair residual gaps as distinct construction failures
  (`HullPiecesDiscontinuous`, `InternalSurvivorChainDiscontinuous`) instead of
  surfacing ordinary `Discontinuous` subpath errors, so callers can tell
  internal construction exhaustion from user-facing segment mistakes.

## 0.2.0

- Port the complete project README with generated API figures.
- Add the slow convex-hull stress suite as an isolated test profile
  (`scripts/test-slow`), with `scripts/test-release` as the canonical
  pre-release verification (fast then slow profiles).
- Document the release and asset-tag workflow (`RELEASING.md`,
  `COMMIT_CYCLE.md`).

## 0.1.0

- Port the complete `svg_path` geometry implementation to F#.
- Distinguish lengths, areas, curve parameters, derivatives, and angles with
  units of measure.
- Cover parsing, serialization, intersections, overlaps, arrangements, CSG,
  convex hulls, transforms, clipping, effects, offsets, bands, and strokes.
- Establish test-for-test behavioral parity with the Gleam implementation.
