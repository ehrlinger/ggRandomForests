# Design: per-panel scale control for `plot.gg_partial_varpro()`

**Date:** 2026-08-31
**Issue:** not yet filed
**Status:** proposed, not implemented
**Target version:** 4.0.0 (unreleased development line)

## Problem

`plot.gg_partial_varpro()` renders its panels with
`facet_wrap(~name, scales = "free_x")`. That gives each variable its own x
*range*, which is the right default and covers most exploratory use. It does not
give each variable its own x *scale*: `facet_wrap()` has no per-panel `breaks`,
`limits` or axis title, because a facetted plot has one x scale by construction.

For a manuscript figure that is the binding constraint. Four partial dependence
curves on VIS, diastolic BP, lactate and flow rate need four different tick
spacings (10, 20, 2, 1), four different clipped ranges, and four different axis
titles carrying units. None of that is reachable from the facet, so the analyst
drops out of the package and hand-builds one `ggplot()` per variable, then
recombines with `ggpubr::ggarrange()`:

```r
vplt_vis <- pro_part_cont %>%
  filter(name == "vis_last") %>%
  ggplot(aes(x = variable, y = (1 - prob) * 100, group = model, color = model)) +
  geom_point(col = "red") +
  geom_smooth(alpha = 0.3, se = FALSE, span = .6, col = "blue", linewidth = 1.0) +
  labs(x = "VIS", y = "Successfully Weaned (%)") +
  scale_x_continuous(breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 100))
## ... three more, near-identical, then ggarrange(ncol = 2, nrow = 2)
```

Four blocks differing in five values each. The cost is not the typing; it is that
the figure now has no relationship to the package. The label map is re-joined by
hand, the log-odds back-transform is re-derived by hand, and the panel grid is
rebuilt by hand — three things `plot.gg_partial_varpro()` already does.

Two of those three are the analyst re-implementing working code:

- **The label map.** `.forest_labels()` (`R/utils.R:173`) already accepts a
  two-column `key`/`label` frame — a shape added for
  `hvtiRutilities::label_map()`. `plot(x, labels = dta_label_map)` works today;
  the `left_join(dta_label_map, by = join_by(name == key))` is redundant.
- **The back-transform.** `gg_partial_varpro(scale = "prob")` applies
  `plogis()` per observation and then averages
  (`colMeans(.scale_transform(...))`, `R/gg_partial_varpro.R:757`). The
  hand-rolled `mutate(odds = exp(parametric), prob = odds / (1 + odds))`
  averages first and transforms after. By Jensen those are different curves,
  and the difference is largest exactly where the per-subject log-odds spread is
  widest — the tails, which is where these figures are read.

Only the third, per-panel scale control, is a genuine gap.

There is also a smaller defect the same work should close. When both the
`continuous` and `categorical` frames are non-empty the method returns
`patchwork::wrap_plots(gg_cont, gg_cat, ncol = 1)`. On a patchwork, `+` adds to
the *last* plot, so the natural cleanup silently themes the categorical panel
alone:

```r
plot(pro_part) + scale_colour_brewer(palette = "Set1")  # continuous unchanged
```

Nothing warns. There is currently no way to ask the method for one frame, so the
workaround is to blank the other element on the object before calling `plot()`.

## Decisions

1. **Extend the existing method; do not add an export.** A new
   `gg_partial_panels()` would be a plot-only function in a package whose public
   vocabulary is `gg_*` extractors plus their `plot()`/`autoplot()` methods. The
   panel grid is a rendering concern and belongs on the renderer.
2. **The per-panel specification is a tidy frame, not a nested list.** Keyed by
   `name`, one row per panel. The analyst already maintains exactly this shape
   (`dta_label_map`); the feature asks them to add columns to a table they have
   rather than learn a new nested structure.
3. **The spec frame's presence is the routing switch.** `panels = NULL` (the
   default) renders today's `facet_wrap()`, byte-identical. A supplied frame
   routes to `patchwork`, where per-panel scales are expressible. No new
   dependency: `patchwork` is already an `Import`, and the method's `@return`
   already documents "a `ggplot` (or `patchwork`) object".
4. **Reuse `points` / `smooth` as the mark vocabulary.** `plot.gg_variable()`
   already declares `points = TRUE, smooth = TRUE`. Inventing `geom = "point_smooth"`
   would give the package two names for one idea.
5. **Palettes go through `ggplot2::scale_colour_brewer()`, not `RColorBrewer`.**
   ggplot2 exports the brewer scales and resolves them through `scales`, which
   vendors the ColorBrewer ramps. `RColorBrewer` stays in `Suggests` where it is
   today; calling `RColorBrewer::brewer.pal()` directly is the version that would
   cost an `Import`.
6. **New formals go after `...`.** R partial-matches argument names only before
   the dots. That is the 2026-08-31 lesson recorded at
   `R/plot.gg_variable.R:147`, where `time_units` ahead of the dots silently
   captured a caller's retired `time =` and died on a type check instead of
   reaching the guard that would have named it.

## Design

### Signature

Six formals, all additive, all defaulting to current behaviour, all past the dots:

```r
plot.gg_partial_varpro <- function(x,
                                   type   = c("parametric", "nonparametric",
                                              "causal"),
                                   labels = NULL,
                                   ...,
                                   which   = c("both", "continuous",
                                               "categorical"),
                                   panels  = NULL,
                                   points  = FALSE,
                                   smooth  = FALSE,
                                   palette = NULL,
                                   ncol    = NULL)
```

### The `panels` frame

A data frame keyed by `name`, matched against the `name` column of
`x$continuous`. Every column except `name` is optional; an absent column means
"let ggplot2 decide", which is what the facet does today.

| Column | Type | Meaning |
|---|---|---|
| `name` | character | variable, matched against `x$continuous$name`. Required. |
| `xlab` | character | panel x axis title. Defaults to the resolved `labels` value, then to `name`. |
| `xmin`, `xmax` | numeric | clipped range, applied through `coord_cartesian()` so points outside are dropped from view, not from the smooth. |
| `xby` | numeric | tick spacing, expanded to `seq(xmin, xmax, xby)`. |
| `span` | numeric | per-panel `geom_smooth()` span. |

Scalar `xmin`/`xmax`/`xby` columns rather than a list-column `xlim`, so the frame
survives `tibble::tribble()` and `read.csv()` alike.

Rows are rendered in frame order, which makes panel order explicit and
reproducible — the facet's importance ordering is the right default but is not
what a figure legend wants pinned. A `name` in `panels` that is absent from
`x$continuous` is an error naming the missing variable; a variable in
`x$continuous` absent from `panels` is dropped without comment, since selecting a
subset is the common reason to supply the frame at all.

Supplying `panels` implies `which = "continuous"`: the frame's vocabulary
(`xmin`, `xby`, `span`) is continuous-only, and the categorical frame is drawn
with boxplots on a discrete axis.

### Routing

```
panels = NULL  ->  facet_wrap(~name, scales = "free_x")     [today, unchanged]
panels = <df>  ->  patchwork::wrap_plots(panels, ncol) +
                     plot_layout(axis_titles = "collect")
```

`axis_titles = "collect"` (patchwork >= 1.2, installed 1.3.2) collapses repeated
axis titles to one, which is what the hand-built version was faking with
`labs(y = "")` on the right-hand panels.

### The patchwork theming wart

A patchwork takes `&`, not `+`, to reach every panel. This is inherent to
patchwork, not something the design can hide, so it is documented on the
`panels` parameter and shown in the example:

```r
plot(pro_part, panels = panel_spec, points = TRUE, smooth = TRUE) &
  hvtiPlotR::theme_manuscript()
```

The single-dependency alternative that avoids it, `ggh4x::facetted_pos_scales()`,
keeps the result one `ggplot` and would take `+`. It was rejected: AGENTS.md
requires asking before adding a dependency, and one operator is not worth a CRAN
`Import` when `patchwork` is already present and already returned by this method.

### `which`

`which = "continuous"` / `"categorical"` returns a bare `ggplot` for that frame;
`"both"` (the default) preserves today's behaviour including the patchwork
return. This closes the silent `+`-on-patchwork trap by giving callers a
supported way to get a single plot, replacing the `x$categorical <- NULL`
workaround.

## What this does not do

- **No general y-`transform` argument.** The manuscript figure's
  `y = (1 - prob) * 100` is one `aes()` expression in the caller. A transform
  argument would be speculative API on a method that already carries a `scale`
  vocabulary in its extractor.
- **No per-panel y scales.** A shared y is what makes a partial dependence panel
  grid comparable; free y invites reading four unrelated ranges as one story.
- **No change to any returned object.** `gg_partial_varpro()` is untouched. No
  class, element name or column name moves, so there is nothing here for a
  reverse dependency to notice.

## Testing

Definition of done is AGENTS.md's, in order: `document()`, then
`lintr::lint_package()` at zero, then
`NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()`.

New tests in `tests/testthat/test_gg_partial_varpro.R`:

- `panels = NULL` returns a `ggplot` whose build is unchanged from current output
  (the non-regression that protects every existing caller).
- A `panels` frame returns a `patchwork` with `nrow(panels)` panels.
- A `name` in `panels` absent from `x$continuous` errors and names it.
- `which = "continuous"` on an object with both frames returns a bare `ggplot`,
  not a patchwork.
- `panels` with only a `name` column renders — every other column optional.
- `palette` reaches the built scale.

New vdiffr baselines in `tests/testthat/test_snapshots.R`, joining the four that
exist (`gg-partial-varpro-{both,categorical,continuous,mortality}.svg`):

- `gg-partial-varpro-panels.svg` — the patchwork route with per-panel breaks.
- `gg-partial-varpro-points-smooth.svg` — the mark grammar.

Per AGENTS.md, regenerate baselines **last**, and re-check `main` for baselines
added since this branch started before merging.

## Release timing

**This belongs in 4.0.0.** v4 is the methods-extension line, and per-panel scale
control on a `plot()` method is a methods extension. It is not a candidate for
deferral.

4.0.0 is unreleased and under internal review toward a future CRAN submission;
`v4.0.0-rc3` is an internal review artifact, not a submission. That matters two
ways:

- **No deprecation cycle is owed.** Nothing here has ever shipped, and the
  package has zero CRAN reverse dependencies, so new formals on an unreleased
  method cost nothing to add. This is the same reasoning the
  `plot-gg-variable-time-args` spec used to remove two formals outright.
- **Deferring would be the actual versioning error.** `4.0.1` is a patch digit,
  and new public API does not belong in a patch release. It would also split one
  methods-extension story across two CRAN submissions, when the standing rule is
  to accumulate work under the current version and let one release carry the
  whole feature set.

**No version bump.** `DESCRIPTION` line 4 is already `4.0.0` and `NEWS.md` line 2
already reads `Version: 4.0.0` under an open `v4.0.0 (development)` section, so
this needs a NEWS bullet and nothing else. The test that greps `NEWS.md` for the
exact `DESCRIPTION` version stays satisfied.

**Consequence to accept, not hide:** rc3's release-gate evidence describes a tree
without this change. Landing it makes that evidence stale, so the gate is re-run
and an rc4 cut before submission. That is the cost of putting a methods extension
in the methods-extension release, and it is the right trade.

Check-time budget is not at risk: the new render paths add two vdiffr snapshots
to a file that runs in about 17 seconds, and no forest fit.

## Prior art

The two script chunks this design generalises are the reference implementation.
The spec-frame-plus-builder shape below was prototyped and rendered before this
document was written, and is what `panels` internalises:

```r
panel_spec <- tibble::tribble(
  ~name,      ~xlab,                       ~xmin, ~xmax, ~xby, ~span,
  "vis_last", "VIS",                           0,    50,   10,  0.6,
  "bpd_last", "Diastolic BP (mmHg)",          20,   100,   20,  0.6,
  "lac_last", "Lactate (mmol/L)",              0,    10,    2,  0.7,
  "fli_last", "Flow rate (L/min/m²)",     0,     3,    1,  0.6
)
```
