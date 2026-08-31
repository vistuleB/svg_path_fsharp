# svg_path_fsharp experiment

This private experiment tests whether an F# port can audit scalar usage in
`svg_path` through units of measure before any public package API is designed.

The initial scalar distinctions are:

- `float<length>` for SVG user-space lengths and coordinates;
- `float<length^2>` for squared lengths and areas;
- `float<parameter>` for curve parameters;
- `float<degree>` and `float<radian>` for angles.

Curve parameters are dimensionless mathematically, but F# treats
`float<parameter>` as a nominal measure. Code must therefore cross an explicit
`Parameter.ratio` boundary before using a parameter as an interpolation
coefficient. This is intentional: those boundaries identify places where the
Gleam implementation may be mixing scalar roles.

The experiment also distinguishes `Point` from `Vector`. The Gleam package
currently uses `Point` for both roles, so this distinction may prove useful or
may prove too costly for a faithful public port.

The current code translates the complete behavior of `svg_path/point.gleam`:
SVG directions and headings, vector arithmetic, dot and cross products,
overflow-resistant norms and distances, interpolation, normalization,
projection, rotation, and tolerance-based point comparison. Location-only
operations live under `Point`; vector-only operations live under the
measure-polymorphic `Vector<'Unit>` type.

Run the current tests with:

```shell
dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj
```
