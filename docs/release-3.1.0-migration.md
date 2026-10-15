# 3.1.0 migration from Gleam

Gleam source baseline: `ce48851` (2026-10-14). The affine factorization API was
already ported in `eb6e62e`; the final preparation additionally mirrors these
post-3.0.1 fixes:

| Gleam change | F# implementation | Regression coverage |
| --- | --- | --- |
| `a130b86` tangent boundary ordering | `Csg.fs` uses arrangement cyclic orders | `Release310RegressionTests.fs`: tangent cubic difference; `PaperBooleanRelease310Tests.fs`: fixture 172 |
| `1b78ea6` signed-zero root candidates | `Root.fs` normalizes root-isolation coordinates | `Release310RegressionTests.fs`: signed-zero recovered crossing |
| `911ba22` Bernstein crossing fallback | `Root.fs`, `SvgPath.fs` | `Release310RegressionTests.fs`: projected cubic witnesses, three roots/tangent, close roots at three scales |
| `d2f949a` per-operand face windings | `Arrangement.fs`, `Csg.fs` | `Release310RegressionTests.fs`: narrow faces, thin triangles, multiplicities; `PaperBooleanRelease310Tests.fs`: 79 and 113 |
| `8d4eba6` certified full overlaps | `OverlapDetection.fs`; exact retrace removal in `Csg.fs` | `Release310RegressionTests.fs`: translated/reversed overlaps; `PaperBooleanRelease310Tests.fs`: 107, 122, 123 |
| `a6741a8` zero-length lines beside curves | `Offset.fs` | `Release310RegressionTests.fs`: arc strokes, maximum-radius rectangle seams, degenerate-input errors |
| `6e1f95d`, `7ddb2fc`, `256c5e1` stack safety | Direct recursion in construction/tokenization; parser handlers return remaining tokens and state to one tail-recursive driver | `Release310RegressionTests.fs`: long construction/closure, explicit/implicit/mixed commands, exact error suffix |

F# uses its existing `List.tryLast`-based closure implementation, so it does not
need Gleam's replacement for the recursive `split_last` helper. The .NET parser
command-dispatch overflow was reproduced before applying the migration.

The six full Boolean fixtures are adaptations of Paper.js `Path_Boolean.js`,
revision `92775f5279c05fb7f0a743e9e7fa02cd40ec1e70`, extracted in the Gleam audit.
They check filled area and sampled boundary distances against upstream output.
Native regressions separately protect exact API contracts. The Paper.js MIT
notice is retained in `paperjs-matrix-LICENSE.txt` and applies to these fixtures
as well. The complete upstream audit catalog remains in the Gleam repository at
`scripts/paperjs/INDEX.md`; F# does not claim to execute that entire inventory.

The [2026-10-15 focused parity inventory](paperjs-test-parity-inventory.md)
records the four originally missing slow-fixture checks and the containment-grid
assertions omitted from the fixture 107 port, along with their subsequent test
ports, related coverage and scope limits.
