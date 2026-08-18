# Hex sticker for ggRandomForests

Date: 2026-08-17
Status: design approved, not yet implemented

## Purpose

The package has no logo. This adds one: a hex sticker in the R community format,
used as the pkgdown site logo, in the README header, and printed as a physical
sticker.

## What it depicts

A fan of four survival curves, as `gg_rfsrc()` draws for a survival forest.

Chosen over a variable-importance plot, a literal forest of decision trees, and
an abstract branching motif. The reasoning: survival forests are what this
package supports that generic random forest tooling does not, so the curves say
something specific about it rather than restating the method. Tree clip art is
what most random forest packages reach for, and fine branches print badly at two
inches.

All four curves start from a single point. Every stratum is at survival = 1 when
t = 0, so a fan from a common origin is what the real plot looks like, and it
gives the composition a focal point in the upper left.

## Layout

Wordmark on the baseline, horizontal, with the curves filling the space above.

Considered and rejected: running the wordmark along the lower-right edge, which
frees the whole face for the mark but drops the name from 19 units to 15.5 and
makes it the first thing to fail in print; and filling the strata as bands rather
than drawing them as lines, which is bolder at small sizes but reads as a stacked
area chart, since filled bands imply summing.

The line treatment is the honest depiction of survival strata, and the baseline
wordmark is the most legible arrangement at the size that matters.

## Geometry

Standard R hex, so it tiles with every other sticker on a laptop lid:

| Property | Value |
|---|---|
| Height, point to point | 50.8 mm (2 in) |
| Width, flat side to flat side | 43.9 mm |
| Ratio | 1.732 : 2 |
| Orientation | pointy top and bottom, flat left and right |

The border stroke sits inside the hex path rather than centred on it, so the cut
line does not clip it. The screen version has no bleed; the print master adds
1/8 in.

## The mark

- Four curves, stroke weights tapering 5.5, 5, 4.5, 4 as they descend. The taper
  reads as depth without adding elements.
- A filled dot at the shared origin.
- Curves are clipped to the hex and run off the right edge rather than stopping
  short, which implies the plot continues past the frame.
- The lower band of the hex is solid field colour. The wordmark sits on flat
  colour, never on top of curves, which is the difference between a name that is
  readable at two inches and one that is not.

## Typography

`ggRandomForests`, Outfit SemiBold, 18 units in the hex's 173-unit width,
tracking -0.3, centred, baseline at y = 147.

**The wordmark must sit above y = 148.5.** That is where the hexagon stops being
full width and the bottom triangle begins closing, and that triangle is 24% of
the total height. A fifteen-character name at 18 units is about 140 wide, and at
y = 176 the hex is only about 71 across, so a wordmark placed low does not merely
look cramped, it runs past the outline on both sides. The first draft of this
design did exactly that.

The consequence is that the lower triangle stays empty field colour. That is
deliberate. It is also why the curves end at y = 123 rather than running to the
bottom: they clear the wordmark band instead of colliding with the type.

Outfit is already the pkgdown base font, so the sticker and the site share a
voice rather than colliding. At 2 in the wordmark renders around 13 pt.

**The wordmark is converted to SVG paths, not left as a `<text>` element.**

A logo that references a font by name renders in whatever the viewer happens to
have installed. Outfit is a Google font and is not present on a stock macOS or
Linux machine, so `font-family="Outfit"` silently falls back: on this machine it
fell back to SF, and the first mockup of this design was reviewed in the wrong
typeface without anyone noticing until it was pointed out. The same failure would
reach pkgdown, the printer, and anyone opening the SVG.

Converting to paths makes the file self-contained and byte-identical everywhere,
at the cost of the text no longer being editable as text. For an asset that
changes roughly never, that is the right trade. Keep the live-text version
alongside as `logo-editable.svg` so the wordmark can be re-set if the name or
font ever changes.

## Palette

| Role | Hex |
|---|---|
| Field | `#12332A` |
| Border | `#4EA97C` |
| Top curve | `#F2E9DC` |
| Strata, descending | `#9FD9B4`, `#5FB183`, `#38855F` |

Deep green says forest without drawing one. The strata darken monotonically as
they descend, which encodes their ordering rather than merely decorating them,
and means **the design survives greyscale conversion** for a one-colour print run
or a black and white figure.

Single variant only. A deep green hex with a light border reads on both the light
and dark pkgdown themes, so a second light-background version would be carried
and maintained for no current use.

## How it is built

Hand-authored SVG, **not** the `hexSticker` package.

`hexSticker` composes a ggplot and re-renders the sticker on every build. A
static SVG gives exact control over the curve geometry, produces identical bytes
every time, and adds no dependency to a package that is deliberately careful
about them. The cost is that edits are made in the SVG rather than in R, which is
the right trade for an asset that changes roughly never.

## Deliverables

| File | Purpose | Committed |
|---|---|---|
| `man/figures/logo.svg` | source of truth, wordmark as paths | yes |
| `man/figures/logo-editable.svg` | same art, wordmark as live text | yes |
| `man/figures/logo.png` | 240 px wide, used by pkgdown and the README | yes |
| print master, 2 in at 1200 dpi with bleed | physical stickers | no |

The print master stays out of the repository on purpose. It would add hundreds of
kilobytes to a 2.38 MB tarball against CRAN's 5 MB limit, for a file nobody
installing the package needs. Regenerate it from the SVG when ordering stickers.

`man/figures/` already exists and already ships (it holds `README-overview.png`),
so no `.Rbuildignore` change is needed.

## Integration

- **README**: logo in the header, via `usethis::use_logo()`. This changes the
  current layout, which opens with a plain `#` title followed by a badge block.
- **pkgdown**: picked up automatically from `man/figures/logo.png`. No
  `_pkgdown.yml` change required.

## Out of scope

- A light-background variant.
- Animated or interactive versions.
- Restyling the pkgdown theme to match. The palette was chosen to work with the
  existing zephyr theme, not to replace it.

## Verification

- Render at 240 px and confirm the wordmark is legible; render at 60 px, the
  pkgdown navbar size, and confirm the **mark** is still identifiable. The
  wordmark will not be readable at 60 px and is not expected to be: at that size
  it renders around 6 px tall. A hex is identified in a navbar by its shape and
  colour, not by reading it. Stating the criterion the other way round would
  guarantee a failure and then invite someone to weaken the design to pass it.
- Convert to greyscale and confirm the strata remain distinguishable in order.
- Confirm `R CMD check --as-cran` stays at its current single NOTE and that the
  tarball stays well under 5 MB.
