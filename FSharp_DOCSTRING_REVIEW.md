# F# public docstring review

Reviewed 2026-09-11, after porting Gleam's public naming, conditional-linearization
move, and construction/conversion documentation changes. This is a separate
documentation audit, not a numerical correctness audit or a release verification.

## Resolved during the port

- Added explicit failure conditions and Result-returning counterparts to
  Subpath assertion helpers. F# uses `System.ArgumentException`, not a Gleam panic.
- Documented policy-taking construction/editing variants, parametric interval
  restrictions, and ParametricOptions field constraints.
- Documented arc-conversion rejection conditions and the distinction between
  single-line and multi-line collapsed-arc transforms.
- Documented zero-extent and negative-dimension outcomes for basic shapes.
- The documentation comparison exposed a behavioral parity discrepancy:
  `Subpath.setClosedWith policy false subpath` validated an unused policy.
  It now opens directly, as Gleam does. A regression exercises a negative
  WiggleWith tolerance that must be ignored when opening.

  Subsequent API change: the Boolean setters have been replaced by
  `Subpath.close`, `Subpath.closeWith`, and the infallible opening function
  (whose `open` identifier must be backtick-escaped in F# source). Opening no longer accepts a policy at all;
  the obsolete policy-input regression was removed and opening idempotence is tested.

## Remaining findings

These findings are not implemented in this audit. They need no API redesign.

### 1. Intersections module overstates its overlap-error rule

Location: `src/SvgPath/Intersections.fs`, module introduction.

“Continuous overlaps are errors here” sounds universal. Intersection queries
reject overlaps, but `segmentSegmentClosestPairWith` explicitly accepts an
overlap and returns a coincident pair. The simple closest-pair wrapper documents
this correctly. Restrict the introduction's claim to intersection queries and
link both operation families.

### 2. Point.near does not explain what “rejected” means

Location: `src/SvgPath/Point.fs`, `near`.

Negative, infinite, and NaN tolerances are described as rejected. The return
type is Bool and the implementation returns false; it neither throws nor
returns an Error. Say “returns false.” Also state that the distance comparison
includes equality and zero tolerance requests coordinate equality.

### 3. Curvature.Options omits its validation boundaries

Location: `src/SvgPath/Curvature.fs`, `Options` and `defaultOptions`.

“Every field is validated” leaves callers guessing. Tolerance must be finite
and non-negative; MaxDepth must be positive. Explain that Tolerance measures
parameter width, and that zero tolerance can exhaust MaxDepth rather than
guaranteeing convergence. Put the constraints on the fields, where IDE help
can expose them.

### 4. Splitting helpers lack parameter and boundary contracts

Locations: `src/SvgPath/Bezier.fs`, `split`, `splitInside`, `splitMany`,
`splitManyInside`; `src/SvgPath/Ellipse.fs`, corresponding `arcSplit*` helpers.

These functions generally have no attached docstrings. Document checked versus
unchecked parameter ranges, endpoint handling, sorting/deduplication of supplied
cuts, and the difference between pair splitting and many-way splitting. Port
the corresponding Gleam contracts, checking each against F# rather than
inferring behavior from “Inside” or “Many.”

### 5. Area semantics are not attached to the public functions

Location: `src/SvgPath/Area.fs`, `absoluteWindingPath*`,
`absoluteWindingSubpath*`, `path*`, and `subpath*`.

The module introduction explains area notions, but these functions have no
attached docstrings. IDE users need to see whether an operation integrates
absolute winding multiplicity or computes unsigned area under a chosen fill
rule. The option-taking forms should link the linearization/error contract;
the simpler forms should name their default options.

### 6. Conditional linearization omits its Option cases locally

Location: `src/SvgPath/Degeneracy.fs`, `segmentLinearizeIfDegenerate` and
`subpathLinearizeIfDegenerate`.

Their docstrings describe replacement and tolerance, but not the meanings of
`None`, `Some []`, and `Some lines`, nor the segment-level choice to return None
for a Line. The README now explains part of this, but those details belong on
the functions too. Match the Gleam function documentation, including the empty
subpath case and the owning error type.

## Recommendation

Address the six groups with documentation changes. The main F# documentation
gap is missing function-level contracts, not just vague adjectives. Do not
make a blanket promise that every float-valued input is validated: the public
geometry constructors and individual operations have different policies.
