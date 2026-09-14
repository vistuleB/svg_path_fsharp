# Changelog

## 1.1.0 - 2026-09-14

- Aligned with Gleam `svg_path` v1.1.0. Parsing now preserves signed/zero arc
  radii and coincident endpoints instead of applying SVG interpretation rules.
  Existing function signatures remain unchanged.
- Added `Segment.normalizeSvgArc`, `Subpath.normalizeSvgArcs`, and
  `Path.normalizeSvgArcs` for explicit SVG arc omission, zero-radius line
  replacement, and absolute-radius normalization, without a tolerance.
- Degeneracy normalization and line conversion now report undefined ellipse
  geometry through their existing error types rather than substituting chords.
- Added `Segment.arcsToCubicBeziersStrict`, `Segment.toCubicBeziersStrict`,
  `Subpath.toCubicBeziersStrict`, and `Path.toCubicBeziersStrict`. These return
  `Result` without straight-cubic recovery. Existing forgiving converters remain
  available, using the same approximation for valid arcs.
- Corrected arc-to-cubic endpoint reconciliation to match Gleam: the first and
  last cubic retain the exact supplied arc endpoints.
- Ported all 19 new Gleam arc regressions and updated related existing tests.
- Documented explicit SVG normalization separately from geometric simplification
  and pinned README figures to `assets-v1.1.0`.
- Release verification: `scripts/test-release` passed 1,862 fast and 26 slow
  tests; README and Gallery generator checks passed for all 13 and 33 figures.

## 1.0.0 - 2026-09-12

- Established the v0.8.0 public API as the stable baseline, aligned with Gleam
  `svg_path` v1.0.0. No geometry algorithms or public signatures changed.
- Future breaking public API changes require a major version increase.
  Existing numerical tolerances and bounded intersection-search limitations
  remain part of the documented contracts.
- Updated the independently generated README band figures: square offsets
  1.6/1.8, larger square legends, and thinner figure-eight band strokes.
- Pinned README images to the immutable `assets-v1.0.0` tag.
- Release verification: `scripts/test-release` passed 1,843 fast tests and
  26 slow tests. `scripts/generate-readme-figures --check` and
  `scripts/generate-gallery-figures --check` passed for 13 README and 33
  Gallery figures.

## 0.8.0 - 2026-09-11

- Aligned the public API changes with Gleam `svg_path` v0.48.0 (`fa075de`).
- Breaking: replaced `Subpath.setClosed`, `setClosedWith`, and their assert
  variants with `close`, `closeWith`, `assertClose`, `assertCloseWith`, and
  the infallible opening function (`open` requires double backticks in F#
  source). Opening changes only the closed flag; closing applies the endpoint
  policy. No Boolean setter remains.
- Breaking: ported Gleam's public naming cleanup: explicit taxicab diameter,
  absolute-winding area, 90-degree rotations, arc-collapse transforms,
  closest-pair queries, consistent curve splitting and parameter comparison,
  and shorter convex-hull constructor names. Updated callers and examples;
  removed obsolete left-padding aliases.
- Breaking: moved conditional-linearization operations into `Degeneracy`
  as `segmentLinearizeIfDegenerate` and `subpathLinearizeIfDegenerate`, with
  `Degeneracy.Error` and corresponding negative-tolerance regressions.
- Made `SmallestEnclosingCircle` and NumberFormat implementation helpers/types
  internal. The three formatting option types used by public inspection and
  serialization options remain public. Added assembly-export regression checks.
- Clarified constructor, assertion, endpoint-policy, parametric fitting, and
  arc-conversion contracts. Remaining documentation suggestions are recorded
  separately in `FSharp_DOCSTRING_REVIEW.md`.
- Release verification: `scripts/test-release` passed 1,843 fast tests and
  26 slow convex-hull tests. `scripts/generate-readme-figures --check` and
  `scripts/generate-gallery-figures --check` passed for all 13 README and 33
  Gallery figures. README images are pinned to `assets-v0.8.0`.

## 0.7.0 - 2026-09-10

- Aligned with Gleam `svg_path` v0.47.0 (`f543511`).
- Breaking: Transform correspondence helpers now return `Transform.Error`,
  preserving Affine errors and distinguishing invalid tolerance from failed
  correspondence checks (with mapped point, target, and tolerance).
- Internal quadratic solving supports arbitrary root units; Arcs radius
  calculations no longer convert through curve parameters. Numerical solving
  and existing curve-polynomial behavior are unchanged.

- Added `Offset.InnerJoin` and the optional `Offset.Options.InnerJoin` override,
  matching Gleam's local inner-corner policy: Round for Round joins, Bevel for
  every other style by default. Explicit InnerRound/InnerBevel overrides apply
  to both band sides and single offsets, including through stroke options.

- Breaking: moved operation-specific public types into their owning modules,
  following Gleam's qualification style (`Offset.Error`, `Stroke.Options`,
  `Arrangement.Error`, etc.). Shared geometry types remain in `SvgPath`.
  Updated tests, README examples, and figure generators; no compatibility aliases
  retain the old root-level operation names.

- Added `MiterClip` and `Arcs` joins, matching Gleam commits `411c34a` and
  `71b05d1`, including their signed-offset fallback and clipping contracts.
- Added the 23 corresponding regression tests one-to-one and four independently
  generated README comparison strips (`9789684`).
- Added four independently generated Gallery overlays of historical SVG 2
  join illustrations. README images are pinned to `assets-v0.7.0`.
- Release verification: `scripts/test-release` passed 1,840 fast tests and
  26 slow convex-hull tests; `scripts/generate-readme-figures --check` passed.
  `scripts/generate-gallery-figures` regenerated all 33 figures successfully.

## 0.6.0

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
- Refreshed documentation and independently generated README/Gallery assets;
  README images are pinned to the immutable `assets-v0.6.0` asset tag.

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
  `WORKFLOW.md`).

## 0.1.0

- Port the complete `svg_path` geometry implementation to F#.
- Distinguish lengths, areas, curve parameters, derivatives, and angles with
  units of measure.
- Cover parsing, serialization, intersections, overlaps, arrangements, CSG,
  convex hulls, transforms, clipping, effects, offsets, bands, and strokes.
- Establish test-for-test behavioral parity with the Gleam implementation.
