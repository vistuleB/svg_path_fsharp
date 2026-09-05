# Changelog

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
