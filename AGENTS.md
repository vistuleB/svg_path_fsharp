# Agent Instructions

## Error Payload Style

- Name every error-union payload field, including single fields. Match the
  corresponding Gleam payload meaning and preserve F# units of measure.
- Labels describe the carried value: for example, `divergence`, not `depth`,
  when reporting fitting error remaining at a recursion limit.
