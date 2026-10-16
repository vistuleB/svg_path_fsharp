# F# 3.1.4 release validation

Validated on 2026-10-16 after the FL002–FL006 ports and API-001 compatibility
adjustment. The public CSG error union matches 3.1.3; the boundary guard remains
active and reports the existing construction error. The detailed diagnostic is
reserved for 4.0.0; see [the register](../DEFERRED-API.md).

- `scripts/test-release`: passed 2,036 main and 30 slow tests, no failures or
  skips. Reported durations were 1m26s and 28s. These are single validation-run
  observations, not comparative benchmarks.
- `scripts/generate-readme-figures --check`: all 13 figures passed.
- `scripts/generate-gallery-figures --check`: all 33 figures passed.
- All 13 README assets match `assets-v3.1.3` byte for byte; `assets-v3.1.4`
  reuses that immutable asset commit. The new asset tag is pushed.
- `git diff --check`: passed. README references use `assets-v3.1.4`.

Package generation from the release commit:
`dotnet pack src/SvgPath/SvgPath.fsproj -c Release -o artifacts/package`.
The package is `artifacts/package/SvgPath.3.1.4.nupkg`; verify its nuspec version,
embedded README, release assembly, and repository commit before uploading.

This release includes the recorded fixes, not a claim of exhaustive Flatten.js
assertion parity. Broader coverage gaps remain in the Gleam audit records.
Publishing to NuGet remains a separate user action. Unrelated local workflow
edits and untracked diagnostic files are excluded from the release commit.
