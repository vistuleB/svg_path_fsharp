# Offset Error Migration

This is the ordered migration plan copied from the Gleam Offset refactor. Keep
each batch small, compile after each batch, run `scripts/test-fast`, and commit
each successful step.

## Naming Rule

The migration has two distinct error domains:

- `InternalError` contains the complete implementation error inventory. Its
  constructors are prefixed with `Internal`.
- Public `Error` contains only stable caller-facing variants. Its constructors
  are not prefixed with `Internal`.
- `publicError : InternalError -> Error` removes the internal boundary only for
  selected stable variants. All other internal failures become `ConstructionFailed`.

Do not mechanically prefix public constructors. Internal constructors gain the
`Internal` prefix; stable public constructors remain unprefixed.

## Gleam Commit Sequence

### 1. Split the Error Domain

Gleam commit `bd3cbc0`, `split offset internal error type`:

- Renamed the detailed error domain to `InternalError`.
- Added `Internal` prefixes to detailed constructors.
- Added public `Error` and `public_error`.
- Immediately changed the whole implementation to use `InternalError`,
  including the public API functions whose later batches add the mapping.
- Did not map any public boundaries yet.

F# action:

- Replace `SvgPath.Error` with `SvgPath.InternalError` (every constructor
  prefixed `Internal`) plus a public `SvgPath.Error` carrying the same
  inventory unprefixed.
- Add `Offset.publicError : InternalError -> Error` inside `module Offset`
  (F# namespaces cannot hold values). It is 1:1 in this batch; later batches
  narrow it.
- Switch the entire implementation to `InternalError`, temporarily including
  the public API signatures. `InternalError` stays publicly accessible while
  public functions return it.
- Stroke `StrokeError.StrokeOffsetError` temporarily wraps `InternalError`
  (Gleam did the same with `OffsetError(offset.InternalError)`).
- Run `scripts/test-fast`; all 1547 tests must pass, with error-case tests
  asserting `InternalError` cases.

### 2. Group Internal Helpers

Gleam commit `b89ad64`, `group offset internals after public pipeline`:

- Moved helpers that do not depend on public functions, directly or indirectly,
  to the bottom of `offset.gleam`.
- Left the public-facing pipeline and its downstream helpers together near the
  top.

F# action:

- Move pure/internal Offset helpers below the public pipeline.
- Keep each downstream dependency below or adjacent to the public boundary it
  serves.
- Do not change behavior in this batch.

### 3. Offset Maps and Segment APIs

Gleam commit `0042187`, `map segment offset errors at public boundary`:

- Migrated `subpath_offset_map_with` and `subpath_offset_map`.
- Migrated `segment_with` and `segment`.
- Mapped their internal results with `public_error`.
- Moved now-internal downstream helpers out of the public section.

F# functions:

- `subpathOffsetMapWith` (maps `lengthSpans` and per-point map results via `publicError`)
- `subpathOffsetMap`
- `segmentWith` (maps `Subpath.createWith`, `subpathUntrimmedWith`, and the `DegenerateTangent` fallback through `publicError`)
- `segment`

This batch also reverts the corresponding test assertions to the public
`Error` cases.

### 4. Historical Default-Wrapper Checkpoint

Gleam commit `507f75d`, `map default offset boundaries to public errors`:

- Temporarily mapped default wrappers whose explicit variants were already
  public: `subpath`, `subpath_band`, `subpath_stroke`, `path`, and `path_band`.
- Later commits moved the durable mapping to the explicit `*_with` boundaries.

F# action:

- Treat this as historical context, not a separate F# batch.
- Keep default wrappers thin and migrate them only as delegates after their
  explicit public functions have been migrated.
- Do not add Offset stroke work here; stroke ownership is already complete in
  `Stroke.fs`.

### 5. Untrimmed Band APIs

Gleam commit `be276c6`, `map untrimmed band errors at public boundaries`:

- Migrated `subpath_band_untrimmed_with` and its default wrapper.
- Migrated `path_band_untrimmed_with` and its default wrapper.
- Migrated the downstream untrimmed-band path helper as needed.

F# functions:

- `subpathBandUntrimmedWith` (maps `validateOptions`, `validateJoin`, `normalizeSourceSubpath`, and `buildSynchronizedUntrimmed` through `publicError`)
- `subpathBandUntrimmed`
- `pathBandUntrimmedWith` (maps `validateOptions` and `validateJoin` through `publicError`)
- `pathBandUntrimmed`
- `untrimmedBandPathSubpaths` (delegates to the now-public `subpathBandUntrimmedWith`, so no mapping changes are needed in the helper itself)

### 6. Trimmed Band APIs

Gleam commit `b9db16b`, `map band errors at public boundaries`:

- Migrated `subpath_band_with` and `path_band_with`.
- Added boundary mapping for option validation, join validation, band
  construction, trimming, and arrangement reconstruction.
- Migrated the downstream band path helper.

F# functions:

- `subpathBandWith` (maps `validateOptions`, `validateJoin`, `normalizeSourceSubpath`, `buildSynchronizedUntrimmed`, both `trimBandSideCusps`, `bandFromSides`, and the in-band/one-subpath path construction through `publicError`)
- `subpathBand`
- `pathBandWith` (maps `validateOptions` and `validateJoin` through `publicError`)
- `pathBand`
- `bandPathSubpaths` (delegates to the now-public `subpathBandWith`, no mapping changes needed in the helper)

This batch also flips `StrokeError.StrokeOffsetError` back from `InternalError`
to the public `Error`, because `Stroke.subpathWith` consumes `subpathBandWith`
which now returns public `Error`. Update the two Stroke tests that assert
offset error cases to use the unprefixed constructors.

Note: in the same commit, keep `path`/`pathUntrimmed` assertions on `Internal*`
constructors; they are migrated in their own later batches. `pathBand` and the
Stroke assertions flip to public `Error` now.

### 7. Stroke APIs (Already Complete)

Gleam commit `60dfbca`, `map stroke errors at public boundaries`:

- Migrated `subpath_stroke_with` and `path_stroke_with`.
- Mapped stroke-width validation and downstream offset failures.
- Migrated the downstream stroke path helper.

This item is already complete in F#: stroke construction and cap handling now
live in `Stroke.fs`, and Offset no longer exports stroke constructors. Do not
reintroduce stroke constructors into Offset. The only stroke work that remains
is the `StrokeOffsetError` flip back to public `Error`, covered in batch 6
where its last Offset consumer is migrated.

### 8. Trimmed Subpath APIs

Gleam commit `dcf5215`, `map trimmed subpath errors at public boundary`:

- Migrated `subpath_with`.
- Mapped validation, normalization, band construction, trimming, and final
  orientation at that public boundary.
- Kept `subpath` as a thin public delegate.

F# functions:

- `subpathWith`
- `subpath`

### 9. Untrimmed Offset APIs

Gleam commit `c2c1578`, `map untrimmed offset errors at public boundaries`:

- Migrated `subpath_untrimmed_with` and its default wrapper.
- Migrated downstream untrimmed path construction.

F# functions:

- `subpathUntrimmedWith`
- `subpathUntrimmed`
- `untrimmedOffsetPathSubpaths`

### 10. Path APIs

Gleam commit `76f12a5`, `map path offset errors at public boundary`:

- Migrated `path_with`.
- Mapped validation, normalization, per-subpath construction, and final path
  assembly.

Gleam commit `81cf9e3`, `map path offset errors at public boundaries`:

- Removed the remaining internal error cases from public `Error`.
- Mapped all remaining unstable failures to `ConstructionFailed`.
- Completed the public API cleanup.

F# functions:

- `pathWith`
- `path`

## Checkpoint Rules

- Do not change more than one batch per commit.
- Run `dotnet build` or `scripts/test-fast` after every batch.
- Update tests in the same batch as the boundary they exercise.
- Internal tests should assert `InternalError` cases.
- Public tests should assert only stable `Error` cases or `ConstructionFailed`.
- Do not run the slow profile until all batches are complete.

## Public API Audit

The current public Result-returning Offset API is fully covered by the batches
above:

- Untrimmed offsets: `subpathUntrimmedWith`, `subpathUntrimmed`,
  `pathUntrimmedWith`, `pathUntrimmed`.
- Untrimmed bands: `subpathBandUntrimmedWith`, `subpathBandUntrimmed`,
  `pathBandUntrimmedWith`, `pathBandUntrimmed`.
- Trimmed offsets: `segmentWith`, `segment`, `subpathWith`, `subpath`,
  `pathWith`, `path`.
- Trimmed bands: `subpathBandWith`, `subpathBand`, `pathBandWith`, `pathBand`.
- Offset maps: `subpathOffsetMapWith`, `subpathOffsetMap`.

The public non-Result values `defaultMiterLimit`, `defaultFittingOptions`, and
`defaultOptions` do not need error-boundary migration. Internal `let internal`
functions are included in the initial InternalError conversion, not in the
public-boundary batches.
