# W3C join comparison references

These SVG files are unchanged reference illustrations downloaded from
https://www.w3.org/TR/SVG2/images/painting/ on 2026-09-10.
They are W3C specification material, not library-generated expected outputs.
See [W3C document licensing](https://www.w3.org/copyright/document-license/).

Run `scripts/generate-gallery-figures` (optionally selecting the four
`w3c-*.svg` filenames). `W3cJoins.fs` uses public F# `Stroke.subpath`
calls for every blue outline; no Gleam build or Node runtime is needed.
It removes the reference sources' zero-displacement moveto between two
coincident endpoints so that those two fragments actually receive a join.
It does not change their coordinates, radii, or arc flags.
Original reference viewports are retained for registration of the overlays.

## Results

- **MiterClip:** stroke width 35, limit 3, pivot x=175. The expected clip
  plane is x=227.5. The computed maximum x is exactly 227.5; this is asserted
  by the generator. The Miter(3) companion uses the bevel fallback.
- **Arcs, nested circles:** successful public stroke, one closed subpath;
  maximum x=351.48504256809167. The reference pink path reaches x=352.03.
  This is not an exact-coordinate regression fixture: its pink outline uses
  rounded Béziers and arc radii, and its construction-circle guides also
  have rounded centers and radii. The overlay exposes the difference rather
  than adjusting our output to match the illustration.
- **Arcs, disjoint circles:** successful public stroke, one closed subpath;
  tip x=324.63583607610025. The reference pink tip is x=323.85, while its
  green tangent-circle guides touch at x=325. Those reference elements are
  themselves inconsistent by 1.15 units. Use the drawing to compare the
  construction, not as an exact numerical oracle.
- **Arcs, parallel tangents:** deliberate contract difference. The reference
  rectangle ends at x=350 (limit 5, half-width 20). Our Round fallback ends
  at x=270. This is already documented in the README; the comparison is not
  a conformance claim for this case.

This first batch covers the miter-limit illustration and all three arcs
fallback illustrations. It does not yet reproduce the principal
`linejoin-construction-arcs.svg` illustration or establish exhaustive SVG
join conformance. No production geometry was changed for these comparisons.

## Historical status

Arcs' extrapolated joins were adopted on
[19 September 2012](https://www.w3.org/2012/09/19-svg-minutes.html#action10).
Miter-clip was adopted on
[12 February 2015](https://www.w3.org/2015/02/12-svg-minutes.html#action02).
Both were removed from the editor's draft in
[March 2026](https://w3c.github.io/svgwg/svg2-draft/changes.html#painting),
but remain in the older published SVG 2 specification used for these figures.
