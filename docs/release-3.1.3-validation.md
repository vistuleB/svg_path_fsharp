# F# 3.1.3 release validation

Validated on 2026-10-15 after closest-pair fix `8914885` and Paper.js test
coverage commit `3f8a97d`.

- `scripts/test-release`: passed 2,014 fast tests and 30 slow tests, no skips.
  Reported test durations were 1m29s and 28s respectively; these are single
  release-verification observations, not comparative benchmarks.
- `scripts/generate-gallery-figures --check`: all 33 figures passed.
- `scripts/generate-readme-figures --check`: all 13 figures passed.
- All 13 README assets match `assets-v3.1.2` byte for byte; `assets-v3.1.3`
  reuses that asset commit. The README uses the new immutable tag.
- Public signatures and options are unchanged; this is a patch release.

The focused Paper.js additions close the five recorded assertion gaps; they
are not a claim of exhaustive F#/Gleam test parity. See
`paperjs-test-parity-inventory.md` for scope.

Package build after the release commit:
`dotnet pack src/SvgPath/SvgPath.fsproj -c Release -o artifacts/package`.
Publishing to NuGet remains a separate user action.
