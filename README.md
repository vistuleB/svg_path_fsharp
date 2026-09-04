# svg_path_fsharp

This private project is a behavior-preserving F# port of `svg_path`. It audits
scalar usage through units of measure and provides a second implementation
against which the Gleam package can be checked. Its public package API is still
subject to change.

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

The port covers the full Gleam implementation: parsing and serialization,
segments and paths, Bézier and ellipse geometry, intersections and overlaps,
arrangements and CSG, convex hulls, transforms, clipping, effects, offsets,
bands, and strokes. The test suite follows the Gleam suite test-for-test, with
additional F# checks where units of measure enforce contracts at compile time.

Public geometry uses `Point<length>`. Derivatives retain powers of the nominal
curve parameter, such as `Point<length / parameter>` and
`Point<length / parameter^2>`. Arbitrary parametric subpath construction is
generic in the caller's parameter measure; an optional tangent callback must
therefore return `Point<length / 'Param>`.

Positive signed offsets use the visual-left normal in SVG coordinates. Visual
clockwise rotation and winding conventions are used consistently rather than
the vertically reflected conventions customary in Cartesian plots.

## Package

The first package version is `0.1.0` and targets .NET 9:

```shell
dotnet add package SvgPath --version 0.1.0
```

The API is expected to change while the mechanically faithful port is refined
into a more idiomatic F# library.

## Example

```fsharp
open SvgPath

let result =
    Parse.path "M 0 0 L 20 0 L 20 20 Z"
    |> Result.mapError (sprintf "parse error: %A")
    |> Result.bind (fun path ->
        Offset.path path 2.0<length>
        |> Result.mapError (sprintf "offset error: %A"))
    |> Result.map Serialize.path
```

Most operations return `Result` so invalid input geometry, numerical failures,
and violated topology assumptions remain explicit.

## Development

Run the tests with:

```shell
dotnet test tests/SvgPath.Tests/SvgPath.Tests.fsproj
```
