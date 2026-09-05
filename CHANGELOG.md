# Changelog

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
