# Contributor Workflow

This file describes coding conventions, verification, generated figures, and
release preparation for contributors, whether working manually or with an agent.

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

## Test Profiles And Reporting

- `scripts/test-fast`: all tests except the slow convex-hull suite.
- `scripts/test-slow`: convex-hull stress tests only.
- `scripts/test-all`: both profiles.
- `scripts/test-release`: canonical pre-release verification, including both
  profiles; fast tests alone do not verify a release.

In review notes and reports, record the exact completed command and test count.
Reserve claims that the full suite passes for a successful `scripts/test-all`
or `scripts/test-release` run in the current worktree.

## Figure Layout

- Use `xml`, not `svg`, as the Markdown code-fence language for SVG examples.
- When comparing opposite orientations, keep each direction arrow at the same
  visual location and only reverse its direction.
- Compute each panel's actual geometry bounds and recenter the geometry in its
  panel rather than relying on hand-tuned translations when practical.

## Relationship To The Gleam Project

This `svg_path_fsharp` project is a port of the neighboring Gleam
[`svg_path`](https://github.com/vistuleB/svg_path) package. The release and
asset workflow here mirrors that project's `WORKFLOW.md`; where the master
copies of scripts live in the Gleam project, this file notes them so the two
projects can be kept in step as the workflow evolves.

There is a dedicated audience distinction:

- `README.md` is rendered by NuGet, so its figures need externally hosted
  absolute URLs.
- `GALLERY.md` is browsed in the repository and links directly to `docs/gallery`.
  Those SVGs are committed on `main`, not the README asset branch.

## Gallery Figures

Run `scripts/generate-gallery-figures` to regenerate all Gallery figures.
This includes the four W3C join comparisons, bringing the registry to 33.
The README generator also includes four join strips (13 figures total), with
the same source geometry as Gleam but computed by F# public stroke calls.
The build completes once, then figure jobs run concurrently in isolated worker
processes. START/DONE/FAILED and ten-second RUNNING messages identify each file.
`docs/gallery/timings.tsv` records status and elapsed milliseconds; .NET has no
Erlang reductions counter, so that metric is not fabricated. Per-file logs and
error reports are local diagnostics. The generated README links only successes
from this run, so stale failed SVGs are not presented as fresh results.
It enables `GalleryDiagnostics=true` for compile-time-only capture of actual
private calls used by the second-offset fixture. Diagnostic outputs and
intermediates are isolated under `bin/gallery-diagnostics` and
`obj/gallery-diagnostics`; packaging such builds is prohibited.
The self-contained, non-packable generator lives in `tools/GalleryFigures`.
Use `--check` to compare outputs without rewriting them, or pass SVG filenames
to run selected figures. Commit the generator, `GALLERY.md`, and `docs/gallery`
changes together. See the generator README for its Gleam source mapping and
the diagnostic capture contract and historical archive.

## Local Previews

For chat/debug previews, write SVGs under `docs/readme` (or a scratch directory)
and, for interactive viewing, reference them with absolute local Markdown image
paths. The normal README figures are regenerated from the fixtures and written
to `docs/readme`.

Do not use GUI commands such as `open`, Chrome, Inkscape, or Preview for this
workflow. Do not generate PNG fallbacks unless specifically requested.

## Regenerating the Published README Figures

Run the canonical generator from the repository root:

```sh
scripts/generate-readme-figures
```

It regenerates the thirteen README figures in `docs/readme` from the F# fixtures in
`tools/ReadmeFigures`. It does not promote them anywhere: the promotion step for
NuGet rendering happens on the `markdown-assets` branch described below.

Use `--check` to verify that the committed SVGs match the fixtures without
writing anything:

```sh
scripts/generate-readme-figures --check
```

This is used in CI and pre-release verification to ensure committed figures are
in sync with the fixtures.

## README Figures During Feature Work

README figures use the `markdown-assets` branch while work is in progress. It is
an orphan branch holding only the published SVGs (and a short `README.md`
describing the branch), never the package source.

The mutable preview URL shape is:

```text
https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets/figures/name.svg
```

Workflow:

1. Regenerate the source outputs in `docs/readme` on `main`:
   `scripts/generate-readme-figures`.
2. Copy the selected README-facing SVGs into the `figures/` directory of the
   worktree checked out on the orphan `markdown-assets` branch.
3. Commit those figure changes on `markdown-assets` and push it.
4. Point the temporary README URLs at the mutable `markdown-assets` branch.
5. Commit the README/source changes on `main`.

Generated README figures should not be referenced through local
package-relative paths. NuGet will not reliably render those paths for the
package README.

## README Figures During Release Prep

For a release, `README.md` should not point at the mutable `markdown-assets`
branch. It should point at an immutable asset tag.

Release asset URL shape:

```text
https://raw.githubusercontent.com/vistuleB/svg_path_fsharp/assets-vX.Y.Z/figures/name.svg
```

Release workflow:

1. Run the canonical pre-release verification command:

   ```sh
   scripts/test-release
   ```

   This is the full suite (fast and slow profiles); `dotnet test` alone is not
   full release verification.
2. Ensure the `markdown-assets` worktree contains the final README-facing SVGs
   for the release.
3. Commit and push `markdown-assets`.
4. Tag that exact `markdown-assets` commit:

   ```sh
   git tag assets-vX.Y.Z
   git push origin assets-vX.Y.Z
   ```

5. On `main`, rewrite README image URLs from `markdown-assets` to
   `assets-vX.Y.Z`.
6. Verify the release README no longer points at the mutable branch:

   ```sh
   rg 'raw.githubusercontent.com/vistuleB/svg_path_fsharp/markdown-assets' README.md
   ```

   For a release commit, this should print nothing.
7. Commit release prep on `main`, including:

   - `README.md` asset URL rewrites,
   - `CHANGELOG.md`,
   - the version bump in `src/SvgPath/SvgPath.fsproj` (and any package version
     metadata).

8. Tag the release commit on `main` as `vX.Y.Z`.
9. Generate the `.nupkg` in `artifacts/package/` from that exact release commit:

   ```sh
   dotnet pack src/SvgPath/SvgPath.fsproj -c Release -o artifacts/package
   ```

10. Let the user run the final NuGet upload of that package.

## Practical Notes

- Use `markdown-assets` only as the mutable branch name.
- Use `assets-vX.Y.Z` only as release asset tag names.
- Do not create a branch and a tag with the same name.
- Do not rewrite or delete old asset tags.
- If a NuGet release is replaced, move both relevant tags deliberately:
  `vX.Y.Z` on `main`, and `assets-vX.Y.Z` on `markdown-assets` if README
  figures changed.
