# Elizabeth synchronization notes

## Current contract

Elizabeth is the sole production general curve-pair search. It is bounded and
heuristic, not a mathematical completeness certificate. Analytic Line handling
and overlap prechecks remain outside this solver. Projection continues to use
separate distance minimization. Endpoint-on-segment candidates are collected
before beam selection; terminal windows use Newton refinement. Historical
experimental alternatives below are no longer executable policies.

## Historical checkpoints

Update at Gleam `18fc6f3`: Henry and Edward and their comparison switches have
also been removed. Elizabeth is the sole general curve-pair intersection route;
distance projection retains its separate minimization machinery. Bounded solver
errors are named `CurveSolverError`. Descriptions below are historical only.

Update at Gleam `f730fc1`: depth-first Elizabeth, its total-window budget, and
alternating terminal refinement have been removed. Production tests now use
the beam route directly. The historical description below records `fac1bc2`.

This ports Gleam `fac1bc2`, not the later cleanup commits. Production curve
pairs use the breadth-first beam; Henry and Edward and depth-first Elizabeth
remain internal comparisons at this checkpoint. Analytic Line dispatch and
overlap prechecks remain outside the curve solver. There is no silent fallback
from production Elizabeth errors.

Beam residual tolerance is `min(caller tolerance, 1e-13)`; depth-first comparison
uses `min(caller tolerance, 1e-12)`. Terminal parameter widths are `1e-9` on both
axes. Final square-distance deduplication uses `1e-7`. Endpoint-on-segment
candidates are collected independently before beam selection.

An 8 by 8 initial grid undergoes enclosure rejection and three-by-three
refinement. Crossing and other windows have separate, loanable budgets, initially
500 each. Decay starts at generation 5 or the first generation exceeding 1000
survivors, reaching 12 per bucket at generation 12. Best-of-corners/center and
chord-crossing residuals rank windows, with coarse-to-fine spatial diversity.
Exact parameter-keyed caches only affect repeated ranking evaluation.

Terminal windows try corners, center, and chord crossing/closest seeds, with
eight window-confined Newton iterations. The depth-first comparison also tries
alternating tangent/secant steps. Candidates are ranked with exact endpoints
first, then residual and parameters; representatives are fixed before deduping
neighbors. Degree-product bounds limit the final spatially diverse selection.

This is intentionally an incomplete search. Discard counters are diagnostics,
not certificates of root completeness. Max depth exhaustion is an explicit
error carrying the unresolved parameter window. No new geometry tolerance or
fallback policy was introduced by the F# port.
