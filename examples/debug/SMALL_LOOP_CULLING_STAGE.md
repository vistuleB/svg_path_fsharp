# Small-loop culling placement

Port of Gleam `211076f`. The private `smallLoopCullingStage` in `Offset.fs`
selects the established `BeforeCuspTrimming` or experimental
`InsideCuspTrimming`. The latter runs only when cusp trimming runs, never
independently during offside or final trimming.

The embedded culler walks source-ordered arrangement images of adjacent
opposite-REVERSED preimages. The earliest previous start matching the latest
next end selects graph edges after submerged-run rescue. All occurrences of
those edges become deletion candidates. The arrangement owns geometry, cuts,
and endpoint/sliver handling; this operation does not recut geometry.

Original previous starts and next ends are excluded. Wraparound adjacency is
considered only for closed inputs. There is no iteration over newly adjacent
preimages after deletion. Four one-to-one source tests cover these contracts.

The default remains `BeforeCuspTrimming`. Upstream's experimental embedded
policy intentionally changes default single offsets, because their final
trimming does not invoke cusp trimming. That historical policy decision is
not changed by the F# port.
