# F# Parity Plan

The F# implementation is backfilled against the current Gleam implementation in
small, behavior-preserving steps.

## Divergence

- Forced-parity ownership: Gleam owns the types and reduction implementation in `Offset`; F# still owns them in `Arrangement`.
- Arrangement error boundary: Gleam separates internal errors from stable public errors; F# still exposes one monolithic `ArrangementError`.
- Offset error boundary: Gleam separates detailed internal failures from public `Error`; F# still exposes internal failures directly.
- Convex-hull error naming and visibility: Gleam uses prefixed internal errors and a narrow public error type; F# exposes construction errors separately without the same naming convention.
- Degeneracy error boundary: Gleam keeps its public error surface narrow; F# exposes `DegeneracyError` directly.
- Stroke ownership: Gleam owns stroke construction in `Stroke`; F# still contains stroke construction wrappers and implementation in `Offset`.
- Consumer and test layout: F# consumers and forced-parity tests still reference `Arrangement` ownership.

## Order

1. Move forced-parity types, implementation, and tests from `Arrangement` to `Offset`.
2. Refactor convex-hull and degeneracy error types and public boundaries.
3. Refactor `Arrangement` errors into internal and stable public domains.
4. Refactor `Offset` errors into internal and stable public domains.
5. Move stroke implementation ownership from `Offset` to `Stroke`.
6. Run fast parity verification, then the slow profile after fast parity is complete.

## Verification

- Use `scripts/test-fast` for normal iteration.
- Do not run the slow profile until the fast parity work is complete.
- Commit each focused backfill step without pushing.
