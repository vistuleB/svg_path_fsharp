# Agent Instructions

## Public Type Ownership

- Follow Gleam's module qualification for operation-specific public types:
  `Offset.Error`, `Stroke.Options`, `Arrangement.Error`, etc. Declare these
  types inside their owning F# module, rather than as namespace-wide prefixed
  types or generic root-level `Error` / `Options`.
- Keep shared foundational geometry and core path types in the `SvgPath`
  namespace. Do not add compatibility aliases for the removed operation names.

## Error Payload Style

- Name every error-union payload field, including single fields. Match the
  corresponding Gleam payload meaning and preserve F# units of measure.
- Labels describe the carried value: for example, `divergence`, not `depth`,
  when reporting fitting error remaining at a recursion limit.
