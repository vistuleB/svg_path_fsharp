# Gallery

These figures parallel the Gleam Gallery, using the F# library and the same
source geometry. Run `scripts/generate-gallery-figures` from this repository.
The generator is a non-packable project under `tools/GalleryFigures`; it requires
no neighboring Gleam checkout. Panel framing is computed from F# geometry bounds.
Production arrangement capture and concurrent worker details are in
[COMMIT_CYCLE.md](COMMIT_CYCLE.md).

All twenty-nine figures are calculated by F#. The second-offset arrangement
captures production calls in a diagnostic-only build; no alternate solver is used.

### Rounded Rectangle Union

![Rounded rectangle union](docs/gallery/gallery-rounded-rectangle-union.svg)

Shows a pile of overlapping rectangles, their raw `Nonzero` union, and the same
result after `Effects.roundCornersWith`.

### ArrangementGraph Intersection Studies

Each sheet shows the source operands, their shared arrangement graph, and the
reconstructed intersection boundary.

![Overlapping rectangle intersection](docs/gallery/gallery-intersection-rectangles.svg)

![Circle and rectangle intersection](docs/gallery/gallery-intersection-circle-rectangle.svg)

![Edge-tangent rectangle intersection](docs/gallery/gallery-intersection-edge-tangent.svg)

![Nested Nonzero intersection](docs/gallery/gallery-intersection-nested-nonzero.svg)

![Nested EvenOdd intersection](docs/gallery/gallery-intersection-nested-evenodd.svg)

![Bowtie and rectangle intersection](docs/gallery/gallery-intersection-bowtie-rectangle.svg)

### ArrangementGraph Difference Studies

These use the same three-panel format for subtraction, including holes,
nested fill-rule cases, mixed curves, and self-crossing input.

![Overlapping rectangle difference](docs/gallery/gallery-difference-rectangles.svg)

![Circle and rectangle difference](docs/gallery/gallery-difference-circle-rectangle.svg)

![Contained rectangle cutout](docs/gallery/gallery-difference-hole.svg)

![Nested Nonzero difference](docs/gallery/gallery-difference-nested-nonzero.svg)

![Nested EvenOdd difference](docs/gallery/gallery-difference-nested-evenodd.svg)

![Bowtie and rectangle difference](docs/gallery/gallery-difference-bowtie-rectangle.svg)

### Stroke Caps

![Stroke caps](docs/gallery/gallery-stroke-caps.svg)

Shows the same open cubic stroked with butt, square, and round caps.

### Dashed Strokes

![Dashed strokes](docs/gallery/gallery-dashed-strokes.svg)

Shows SVG-style dash extraction followed by geometric stroking with round caps.

### Recursive Dashes

![Recursive dashes](docs/gallery/gallery-recursive-dashes.svg)

Shows a dashed stroke whose individual dash outlines are dashed and stroked
again at a smaller scale.

### Figure-Eight Band

![Figure-eight band](docs/gallery/gallery-figure-eight-band.svg)

Shows an asymmetric two-sided band around a closed figure-eight.

### Stretched Figure-Eight Bands

![Stretched figure-eight bands](docs/gallery/gallery-symmetric-figure-eight-bands.svg)

Compares a narrower band lying entirely on its positive-offset side (`+10` to
`+20`, left) with a wide band crossing both sides of the source (`−5` to `+25`,
right).

### Figure-Eight Correspondence Blocks

![Figure-eight correspondence blocks](docs/gallery/gallery-figure-eight-correspondence-blocks.svg)

Shows the per-segment correspondence blocks between the untrimmed inner and
outer offset walks of the asymmetric figure-eight band.
This ports the current Gleam display fixture's endpoint-pairing heuristic;
these colored regions are not a certified provenance mapping.

### Figure-Eight Convex Hulls

![Figure-eight convex hulls](docs/gallery/gallery-figure-eight-convex-hulls.svg)

Compares the convex hulls of the Gallery figure-eight, its asymmetric band,
and the source and band taken together. These three cases are also numerical
regression fixtures for the convex-hull implementation.

### Stroke Offset Tracks

![Stroke offset tracks](docs/gallery/gallery-stroke-offset-tracks.svg)

Shows one open subpath and several one-sided untrimmed offsets in different
colors.

### Earth-Tone Offsets

![Earth-tone offsets](docs/gallery/gallery-earth-tone-offsets.svg)

Shows three one-sided offset studies with each panel centered from the computed
geometry bounds.

### Package Title First Offset

![Package title first offset](docs/gallery/gallery-package-title-first-offset.svg)

Shows the package title outline, its untrimmed single offset, and the trimmed
single offset used to stress arrangement-based offset pruning.

### Package Title Second Offset Arrangement

![Package title second offset arrangement](docs/gallery/gallery-package-title-second-offset-arrangement.svg)

Shows the full arrangement from two successive `1.05` offsets using `Miter(4)`.
Only the second offset disables offside trimming. A diagnostic-only build
captures the actual classification and parity-reduction calls.

Red marks initially submerged offset edges; purple marks positive final
capacity. Yellow marks deleted edges that were initially degree-one after
submerged deletion. Green marks other initially retained edges later deleted;
pale gray marks source-only graph edges. Original lettering is pale gray and
its first offset is blue. The historical snapshot is retained under `archive`.

### Offset Text

![Lazy dog offset coil](docs/gallery/gallery-lazy-dog-offset-coil.svg)

An included SVG text sample mapped into `(distance, offset)` space and then
onto a fixed-radius coil. The F# generator recomputes this geometry.

![Lazy dog offset decaying spiral](docs/gallery/gallery-lazy-dog-offset-decaying-spiral.svg)

Uses the same text sample on a decaying spiral, with both radius and local
offset shrinking by the same factor per turn.

The F# generator recomputes this geometry from the same included text sample.

### Crescent Hull

![Crescent hull](docs/gallery/gallery-crescent-hull.svg)

Shows a crescent-shaped point cloud, a reference arc and chord, and the computed
convex hull.

### Package Title Nine Offsets

![Package title nine offsets](docs/gallery/gallery-package-title-nine-offsets.svg)

Nine successive `Offset.path` rings, each spaced 1.04 from the previous ring,
pushed outward from the `SVG_PATH` text outline.

### Cut Radiator

![Cut radiator](docs/gallery/gallery-cut-radiator.svg)

Shows a dense snaking subpath cut by a text outline, with the pieces inside the
outline removed.
