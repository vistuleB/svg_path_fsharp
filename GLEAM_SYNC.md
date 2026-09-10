# Gleam commit-by-commit synchronization

Baseline: F# `c612b7b`, corresponding to Gleam `29ce1db` (curvature public
diagnostics/docs). Work proceeds in `git log --reverse 29ce1db..a251698` order:
109 source commits, followed by subsequent committed Gleam history as authorized.
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

- `ddad3c4` — clarify Stroke ownership, cusp-trimmer adaptation, projection
  sampling budget, and interval mapping. `git diff --check` passes.

- `7d37623` — remove the line-run shortcut, using support extrema for all
  thin windows. Three source regressions added; the F#-only local-reversal test
  now reflects the explicit global-extrema contract. `scripts/test-fast`:
  1726 passed.

- `126e79c` — remove shared-endpoint intersection skipping. All three source
  regressions ported. `scripts/test-fast`: 1729 passed.

- `8b43c68` — use length bounds instead of chords for size filters; compare
  same-source pieces; split self-intersections only after existing-edge checks,
  reinserting both children with bounded subdivision depth. Four source
  regressions ported. `scripts/test-fast`: 1733 passed.

- `af898f9` — retain source intervals on progressive images and compose them
  through edge cuts (including reversed occurrences), replacing endpoint
  projection bookkeeping. Four source regressions added. `scripts/test-fast`:
  1737 passed.

- `5e34eaa` — historical residual-window experiment, including its two new
  crossing tests. `scripts/test-fast`: 1729 passed, 10 failed (1739 total).
  This is an intentionally unsuccessful upstream checkpoint, immediately
  reverted by the next source commit; no independent repair is invented here.

- `e5f4a66` — revert the preceding unsuccessful experiment. Runtime and tests
  exactly match the pre-experiment F# commit `59374d6` (1737 fast tests passed
  there); verified by `git diff 59374d6 -- src tests` producing no changes.

- `46b3cd6` — validate crossing brackets and reject unmatched clamped endpoint
  roots. Source regression ported. `scripts/test-fast`: 1738 passed.

- `d49e170` — conservative polygon enclosures, original-ellipse evaluation
  for arc windows, private bounds switch, and empty/unchanged child guards.
  Source regression ported. `scripts/test-fast`: 1739 passed.
  `dotnet fsi examples/debug/intersection_enclosures.fsx`: 2127 helper checks
  passed. This ports the source diagnostic's enclosure assertions; its
  JavaScript module-rewriting comparison harness is not a library component.

- `71f18fe` — retain the original Gleam experimental patch explicitly labeled
  as a historical Gleam artifact, with F# checkpoint links and diagnostic
  instructions. No production changes; preceding fast result: 1739 passed.

- `6679aa6` — retain retraced closed contours when no interior probe exists.
  Both exact C-shape source regressions ported. `scripts/test-fast`: 1741 passed.

- `c2e8865` — annotation containment uses its supplied tolerance, matching
  side-probe displacement. Source regression ported. `scripts/test-fast`:
  1742 passed.

- `3bf9b2a` — parity-erased cusp results return None like initially empty
  classifications. `scripts/test-fast`: 1742 passed.

- `5ec5fd2` — column-scale reconstruction checks preserve small shear during
  transform serialization. Source regression ported. `scripts/test-fast`:
  1743 passed.

- `c91e527` — remove unreachable position-only fit policy, its match cases,
  and four unused fitting helpers. `scripts/test-fast`: 1743 passed.

- `4ee430e` — source-only illustrated audit ledger (`REMAINING_ISSUES.md`),
  not production behavior. Its resolved code changes have their own entries
  above. The document's Gleam-specific reproducers and image links remain in
  the source repository; it is not presented as an F# audit. No F# code change.

- `cbc802b` — resume drawing at the retained current point after closepath;
  repeated closepath is a no-op. Three source regressions ported.
  `scripts/test-fast`: 1746 passed.

- `0d8f613` — reverse survivor segment, vertices, and directed H interval
  together; immutable source and REVERSED label remain unchanged.
  `scripts/test-fast`: 1746 passed.

- `316f9dd` — inspect the wraparound seam before marking a sole join-free
  portion closed; construct the closing correspondence even with one open
  portion. Exact source regression ported. `scripts/test-fast`: 1747 passed.

- `f57dd01` — apply the conservative width lower-bound allowance before
  pruning optimization intervals. `scripts/test-fast`: 1747 passed.

- `786d993` — reparameterize an increasing neighborhood for extrapolated
  direction queries. All three source regressions and their coordinatewise
  tolerances ported. `scripts/test-fast`: 1750 passed.

- `2c09bb6` — align the closed source seam explicitly before rebuilding,
  retaining both control-handle edits, including on a single cubic. Both
  source regressions ported. `scripts/test-fast`: 1752 passed.

- `80809a9` — clamp selected-span local distances without weakening global
  validation. Source regression ported. `scripts/test-fast`: 1753 passed.
  Replaced both text-map Gallery snapshots with the original fixture algorithms
  and repository-local source SVG. `dotnet run --project
  tools/GalleryFigures/GalleryFigures.fsproj -- gallery-lazy-dog-offset-coil.svg
  gallery-lazy-dog-offset-decaying-spiral.svg` completed successfully; both
  generated SVGs, generator, and documentation committed together.

- `5a5261d` — Gallery refresh attempted using `scripts/generate-gallery-figures`.
  Exit status 1: 25 calculated figures succeeded, three failed, and the one
  still-archived arrangement SVG was explicitly reported/copied. Successful
  changed assets are committed; failed figures retain their previous SVGs and
  are **not** verified against this source checkpoint.
  - `gallery-recursive-dashes.svg`: second dash stroke,
    `StrokeOffsetError ConstructionFailed`.
  - `gallery-intersection-circle-rectangle.svg`: `CsgArrangementError`.
  - `gallery-difference-circle-rectangle.svg`: `CsgArrangementError`.
  All nine successive package-title offsets completed. Full log:
  `/tmp/fsharp-sync-gallery-refresh.log` (local diagnostic, not committed).

- `95a29d2` — user approved a diagnostic-only instrumented build in place of
  Erlang runtime tracing. Captures actual private production classification and
  parity calls; no copied solver/pipeline. Observer storage/calls and friend
  visibility are compiled out of normal builds; diagnostic output/intermediates
  are isolated and packaging is rejected. Old SVG moved into the archive.
  `scripts/generate-gallery-figures
  gallery-package-title-second-offset-arrangement.svg`: succeeded, 907 vertices,
  1181 graph edges, 783 eligible edges, 471 initially submerged, 312 retained,
  185 positive final capacities. Ordinary `scripts/test-fast`: 1753 passed.
  Diagnostic packaging guard tested with `dotnet msbuild
  src/SvgPath/SvgPath.fsproj -t:RejectDiagnosticPackage
  -p:GalleryDiagnostics=true -nologo` (expected rejection).
  The three earlier full-Gallery failures remain recorded, not independently
  repaired. Resume instruction: continue past non-catastrophic historical
  failures toward the latest committed Gleam changes.

- `527da80` — offset capacities count precisely the source-order reconstruction
  preimages, excluding zero-source companions; parity reduction preserves those
  explicit capacities. Added the three original zero-offset regressions.
  `scripts/test-fast`: 1756 passed.

- `0181593` — validate actual one-sided endpoint directions after either
  tangent-line or bisection collapsed-control fitting; reject reversed rays.
  `dotnet fsi examples/debug/collapsed_handle_rays.fsx`: the original 12 ray
  cases passed. `scripts/test-fast`: 1756 passed.

- `33f11eb` — deduplicate intersections by the pair of parameter addresses,
  never position alone; remove the geometric deduplication tolerance. Both
  original closed-cubic/retraced-quadratic regressions ported one-to-one.
  `scripts/test-fast`: 1758 passed.

- `11f9598` — classify equal sampled outward-ray orders as crossings only
  for smooth, aligned tangent branches; port the original swapped/reversed
  regression assertions. `scripts/test-fast`: 1758 passed.

- `410ed0f` — retain visible zero-length dash entries and source addresses;
  generate their Round/Square caps, orienting squares from source tangents.
  Ported the four new source tests and updated its existing zero-entry test.
  `scripts/test-fast`: 1762 passed.

- `4b11243` — propagate tangent polynomial isolation/geometric refinement
  failures through hull construction; internal diagnostics now return Result.
  Ported the source exhaustion regression and updated existing callers/tests.
  Also restored the source's line-adjacent refinement calls before endpoint
  synchronization rather than silently skipping their failures.
  `scripts/test-fast`: 1763 passed.

- `7a41f3e` — dual construction now uses the original deterministic line
  proposals, complete-line validation, two confirmations, exterior/placement
  propagation, contradiction and exhaustion errors. Removed displaced probes.
  Ported all four source regressions. `scripts/test-fast`: 1767 passed.
  `dotnet fsi examples/debug/dual_sweep_checks.fsx`: source diagnostic checks
  for acceptance, confirmations, contradictions, exhaustion, vertex/touching
  rejection and inseparable hits passed. Park-Miller uses Int64 so its product
  remains exact instead of overflowing F# Int32.

- `8e3a44e` — remove final nesting-based single-offset orientation; port the
  source preservation regression and regenerate autonomous README figures.
  `scripts/test-fast`: 1767 passed, 1 failed. Failure is the F#-specific legacy
  `OffsetTests.path offset orients nested closed contours by depth`, whose
  expected orientation contradicts this deliberate upstream contract change.
  Recorded for final test reconciliation, not repaired out of sequence.
  `scripts/generate-readme-figures`: all nine figures generated successfully.

- `506be97` — stroke returns the band's orientation without an additional
  nesting-based pass. `scripts/test-fast`: 1767 passed, the same one obsolete
  single-offset orientation expectation failed; no additional failures.

- `1f6c347` — delete unused outline nesting probes/depth/orientation helpers.
  `dotnet build src/SvgPath/SvgPath.fsproj --no-restore`: succeeded, zero
  warnings/errors. No behavioral change from the preceding tested checkpoint.

- `ac5feb9` — signed winding propagation over an existing dual, with supplied
  per-edge changes and validation/contradiction/unreachable errors. All five
  source regressions ported. `scripts/test-fast`: 1772 passed, the same one
  obsolete single-offset orientation expectation failed.

- `0ec158b` — include exact winding occurrences/closure caps in the AG,
  propagate signed face windings for classification, retain only eligible
  reconstruction capacities, make open bands capped in both modes, and delegate
  nonzero strokes to symmetric bands. Small-loop shared-endpoint rejection now
  requires both parameters near the seam. README contracts and all three new
  source regressions plus changed existing assertions ported.
  `scripts/test-fast`: 1775 passed, the same one obsolete F# nesting-orientation
  test failed; no additional failures. Diagnostic Gallery capture still observes
  the actual production classification, not a parallel display construction.

- `f3e79ab` — regenerated autonomous Gallery assets after the unified band
  construction. Historical Erlang diagnostic captures remain in the Gleam
  repository; no copied SVG is used as a substitute for F# computation.
  `scripts/generate-gallery-figures`: 27 generated, two existing Boolean
  circle/rectangle cases failed with `CsgArrangementError`. Recursive dashes
  now generate successfully. The actual second-offset AG capture remains
  907 vertices, 1181 edges, 783 eligible, 471 submerged, 185 final positive
  capacities. Log: `/tmp/fsharp-sync-unified-gallery.log`.

- `211076f` — private before/inside-cusp small-loop placement switch and
  source-image-based loop-edge selection. Default remains before cusp trimming.
  All four source regressions ported; no extra geometric intersection pass.
  Initial validation exposed a test-helper value-restriction compilation error.
  The helper is now explicitly a function. Completed `scripts/test-fast`:
  1779 passed, the same one obsolete nesting-orientation expectation failed.

- `c45b249` — documentation-only analytic endpoint investigation. The saved
  Gleam join/line pair reaches the analytic arc-ray path, not a window solver.
  Reconstructed angle candidates can fall beyond an exact stored endpoint.
  No algorithm change in this checkpoint; the subsequent `6362abe` ports the
  endpoint candidate fix and exact source regression geometry.

- `76ad87a` — replace displaced final-orientation probes with deterministic
  signed-unit dual propagation; retain same-loop retraces as unconstrained and
  reject unsupported ownership/conflicting face values. All nine source tests
  and README contracts ported. `scripts/test-fast`: 1788 passed, the same one
  obsolete single-offset nesting-orientation expectation failed.

- `6362abe` — independently seed both stored curve endpoints against the finite
  line, retaining analytic candidates and endpoint-preferring deduplication.
  Exact recursive-dash arc/line regression covers both reversals and swaps.
  `scripts/test-fast`: 1789 passed, one unchanged obsolete nesting expectation
  failed. No additional failures.

- `a251698` — default final band enumeration follows filled-face boundary walks
  with material on the right, preserving kissing seams and all residual even
  multiplicity as retraces. Older orientator retained behind the private switch.
  Nine source tests and closed-walk rotation-aware comparison helper ported.
  `scripts/test-fast`: 1798 passed, one unchanged obsolete single-offset
  nesting-orientation expectation failed. `scripts/generate-readme-figures`:
  all nine generated. This completes the initial 109-commit source queue.

- `f571cd7` — concurrent Gallery scheduling with per-file progress, timings,
  immediate writes, failure reports and success-only generated index. Workers
  are isolated processes, preserving the diagnostic tracer isolation. .NET
  elapsed time is reported; Erlang reduction counts have no fabricated analogue.
  `scripts/generate-gallery-figures gallery-dashed-strokes.svg
  gallery-stroke-caps.svg`: 2 generated, 0 failed. `dotnet fsi
  examples/debug/gallery_runner_checks.fsx`: concurrent start, immediate writes,
  exception/error isolation and report checks passed (intentional failures).

- `fac1bc2` — production Elizabeth beam, endpoint-on-segment inventory, terminal
  Newton/experimental alternating refinement, fixed-representative selection,
  exact evaluation cache and depth-window errors. All 15 source tests ported.
  Aligned inherited window order/cut arithmetic and Edward-only circular-arc
  dispatch to the source; traversal helpers remain stack-safe on .NET.
  `scripts/test-fast`: 1809 passed, 5 failed: the known obsolete orientation
  test and four numerical-candidate count assumptions (flat cubic, off-center
  kissing, shared endpoint, flat beam). Later source `bf5341d` addresses those
  intersection contracts; no tolerances or assertions changed here.
  `scripts/generate-gallery-figures gallery-intersection-circle-rectangle.svg
  gallery-difference-circle-rectangle.svg gallery-symmetric-figure-eight-bands.svg`:
  all three generated. The two previously failing Boolean figures now succeed.

- `78c263b` — Elizabeth rejects using enclosing polygons directly, without
  constructing axis-aligned boxes or extrema. `scripts/test-fast`: 1808 passed,
  6 failed. The five preceding assertions remain; the strict translated
  depth-first experiment now exhausts 10000 windows. The following upstream
  commit removes that experimental solver/test rather than changing production.

- `f730fc1` — remove depth-first Elizabeth and alternating terminal refinement;
  retain the production beam and its regressions. `scripts/test-fast`: 1806
  passed, 5 failed, 1811 total. The four historical candidate-count assertions
  and obsolete single-offset nesting assertion remain; no additional failures.

- `5d6e8bc` — source audit/Elizabeth status documentation only. No production
  change; F# verification remains recorded above rather than copying Gleam's
  successful test claims into this repository.

- `caab926` — remove superseded whole-contour orientator, its errors/switch and
  nine obsolete tests. Filled-face tests now exercise re-enumeration directly.
  `scripts/test-fast`: 1797 passed, the same 5 historical assertions failed,
  1802 total. No new failures.

- `5f8609c` — classification requires a complete dual-face winding map, computed
  once per pass. Removed optional cache, sampled fallback and winding callbacks.
  Diagnostic Gallery observations still capture actual production classification.
  `scripts/test-fast`: 1797 passed, the same 5 historical assertions failed,
  1802 total.

- `6db0bf7` — remove unused cusp cap arguments and obsolete semantic containment
  helpers; retain the open-payload rejection regression through topological band
  construction. Ported pipeline/small-loop switch comments. `scripts/test-fast`:
  1795 passed, the same 5 historical assertions failed, 1800 total.

- `44db918` — stroke entry points validate once; internal subpath/dash traversal
  reuses that validation. Zero-length helpers return path errors directly. Dash
  pattern normalization already used the simplified value shape. `scripts/test-fast`:
  1795 passed, the same 5 historical assertions failed, 1800 total.

- `1bd85ab` — typed PointRepair/LoopRepair/NoRepair policies replace strings.
  The F# prefilter was already unconditional; pairwise unions already used the
  no-repair policy directly. Fast and slow test call sites use the typed cases.
  `scripts/test-fast`: 1795 passed, the same 5 historical assertions failed,
  1800 total. Slow profile not rerun at this checkpoint.

- `18fc6f3` — remove Henry/Edward intersection routes and comparison switches;
  retain projection's shared descent and terminal-grid helpers. Rename bounded
  curve errors to CurveSolverError. `scripts/test-fast`: 1795 passed, the same
  5 historical assertions failed, 1800 total.

- `8ea88ff` — clarify that cyclic embedding is used by graph construction and
  describe the established sampled overlap detector without experimental wording.
  Comments only; last completed fast results remain recorded above.

- `bc2396e` — remove unused Offset.Options.DistanceOptions, fixture settings and
  README claims. Remove the upstream-deleted projection-default test; correct
  default_distance_options_test to the source's trimming-default assertions,
  consolidating the existing duplicate F# case. `scripts/test-fast`: 1793 passed,
  the same 5 historical assertions failed, 1798 total.

- `7f7f241` — README navigation, numerical-intersection contract, segment-size
  threshold and dual-face semantics aligned to source. Existing F# development
  commands already name the fast/slow/full/release profiles. Documentation only.

- `a78128f` — port current geometry contracts into F# docstrings/comments:
  visual rotations, numerical root/intersection limits, arc conversion policy,
  support-based normalization, capped/untrimmed bands, stroke overrides and
  winding sampling. Gallery links its actual runner workflow. Gleam-specific
  disabled-test paths do not apply to .NET trait-based test selection.
  Documentation only; no additional tests run for this checkpoint.

- `19d2949` — separate current Elizabeth notes from historical experiments.
  The large Gleam review/archive documents were never copied into this repo;
  their resolved findings are preserved by this commit ledger instead. No
  production changes or additional tests at this documentation checkpoint.

- `bf5341d` — port candidate geometry/separation/known-contact assertions and
  exact numerical count snapshots, including endpoint preference after snapping.
  `scripts/test-fast`: 1797 passed, 1 failed, 1798 total. All four historical
  intersection assertions now pass with the exact Gleam snapshots (6/9/4/4).
  The sole failure remains the obsolete F#-only single-offset nesting assertion.

- `785df2f` — public Segment.boundingPolygon/Between faithfully use Bezier
  control hulls and corrected-ellipse tangent triangles, returning visual
  clockwise hulls with source-start preference. All five source tests ported.
  `scripts/test-fast`: 1802 passed, the obsolete F# orientation assertion failed,
  1803 total; all bounding-polygon regressions pass.

- `55b738f` — Elizabeth uses public ordered bounding polygons and boundary
  normals, retaining along-line axes for two-point hulls. Removed duplicate
  enclosing-point construction; ported the collinear-degeneracy regression.
  `scripts/test-fast`: 1803 passed, the obsolete F# orientation assertion failed,
  1804 total. Candidate-count snapshots remain unchanged and passing.

Next: `ed94708` (Gleam 0.46.0 release/docs/assets); authorized F# obsolete-test
cleanup precedes final verification. No later/uncommitted Gleam work is in scope.
