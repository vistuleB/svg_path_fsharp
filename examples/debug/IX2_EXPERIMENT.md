# Historical residual-window experiment

`ix2-residual-gleam-experiment.patch` is the original Gleam diagnostic snapshot
from `71f18fe`, against Gleam `af898f9`. It is retained unchanged for provenance;
it is not an F# patch and must not be applied to either current main branch.
The snapshot combines residual retention, partial-result fallback merging,
polygon enclosures, subdivision guards, and bracket validation. Its `1e-15`
polishing experiment had been removed; the source reported 1539 passing tests
and nine failures. Remaining problems included near-root multiplicity and
search exhaustion. It was not promoted into production as a combined change.

The corresponding F# sequential history records the initial experiment at
`b65b65d` and its reversal at `0812eb0`. Bracket validation (`b28b974`) and
enclosures (`a6566a9`) are separate ports of independently retained changes.
Reverting the residual search does not fix IX2's missed nearby roots.

Run the F# private enclosure checks after building the library:

```shell
dotnet fsi examples/debug/intersection_enclosures.fsx
```
