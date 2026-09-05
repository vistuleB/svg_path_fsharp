# Commit Cycle

This file describes the intended workflow for generated README figures and the
release asset branch during normal feature work and release prep.

This `svg_path_fsharp` project is a port of the neighboring Gleam
[`svg_path`](https://github.com/vistuleB/svg_path) package. The release and
asset workflow here mirrors that project's `COMMIT_CYCLE.md`; where the master
copies of scripts live in the Gleam project, this file notes them so the two
projects can be kept in step as the workflow evolves.

There is a dedicated audience distinction:

- `README.md` is rendered by NuGet, so its figures need externally hosted
  absolute URLs.
- This project currently has no repository-browsed `GALLERY.md`, so there is no
  gallery figure promotion step here. The Gleam project also has a gallery
  workflow; if a gallery is added to this port, copy that section of the Gleam
  `COMMIT_CYCLE.md`.

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

It regenerates the nine README figures in `docs/readme` from the F# fixtures in
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
