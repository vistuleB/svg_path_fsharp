# Releasing

This checklist supplements the figure and asset-tag workflow in
`COMMIT_CYCLE.md`.

The master copy of this release sequence lives in the neighboring Gleam
[`svg_path`](https://github.com/vistuleB/svg_path) project
(`RELEASING.md` and `scripts/test-*`). This F# port is kept in step; where a
step exists in both, the meaning is identical, only the tooling differs (`dotnet`
here, `gleam` there).

## Required verification

1. Run the canonical pre-release test command:

   ```sh
   scripts/test-release
   ```

   This runs both the fast profile and the slow convex-hull profile. Neither
   `dotnet test` nor `scripts/test-fast` alone is full release verification.

2. Report the command precisely. The phrase "full suite passes" is reserved
   for a successful `scripts/test-all` or `scripts/test-release` run.

3. Verify the README figures are in sync with the fixtures:

   ```sh
   scripts/generate-readme-figures --check
   ```

   This should print nothing and exit zero.

4. Confirm the `markdown-assets` worktree holds the final README-facing SVGs for
   this release before tagging them (see `COMMIT_CYCLE.md`).

5. Complete the README figure, changelog, version, asset-tag, release-tag, and
   publication steps in `COMMIT_CYCLE.md`.

## Version and package metadata

- The package version lives in `<Version>` in `src/SvgPath/SvgPath.fsproj`.
- Keep `CHANGELOG.md` in sync with the release.
- Do not bump the version and tag the release until `scripts/test-release` and
  `scripts/generate-readme-figures --check` both pass.
