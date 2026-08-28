# Labelled, importance-ordered forest plots

**Date:** 2026-08-28
**Repo:** ggRandomForests (CRAN, v4.0.0 development line)
**Branch:** `feat/forest-plot-labels`
**Ships in:** v4.0.0 — **no version bump.** `v4.0.0-rc1` is a git tag, not a
DESCRIPTION version; RC2 is cut from `main` once this and the parallel RHF
session's work land.

## Problem

`plot()` on a `gg_partial_varpro` object produces a **methods** figure where the
default should be a **deliverable** figure. The current output is not wrong — the
three-series overlay on an additive scale is a legitimate diagnostic, and the
plot method's own documentation describes reading where the parametric,
nonparametric and causal curves fan apart. That view earns its keep. It is simply
not the one you want by default, and getting the deliverable view currently means
rebuilding the figure by hand.

Underneath that framing sit **two genuine bugs** and **one default problem**:

| # | Symptom | Root cause | Site |
|---|---|---|---|
| 1 | Facets sorted alphabetically | `plt.df$name <- feat_name` writes a **character**; `facet_wrap()` re-sorts it, discarding `part_dta` order | `.build_varpro_dfs()` |
| 2 | `nvars` returns the wrong variables | Slices `seq(nvars)` — the **first** n list elements, before any ranking | `.build_varpro_dfs()` |
| 3 | y-axis reads "Partial Effect" (**not a bug**) | `object=` absent → `family` is `NA` → `.resolve_varpro_scale()` returns `"generic"`. The label is *honest* — the scale genuinely could not be determined. The problem is that this path is silent and easy to fall into. | `.resolve_varpro_scale()` |

Separately, every forest plot that draws variable names draws the **raw column
name** (`bpd_last`, `vis_last`) where the deliverable needs a human label
("BP Diastole", "VIS").

Rows 1 and 2 are defects in **both** views — importance order and top-n selection
are wrong whatever scale you draw on. **Row 3 is not a defect**; it is a default
that makes the generic path easier to reach than the probability path CORR reads.
Its fix is a warning, not a changed label.

Row 2 is the serious one: it silently substitutes an arbitrary subset for the
top-n and there is no symptom in the output.

## Constraints established by investigation

These were verified, not assumed. They shape the design.

1. **`rfsrc` strips `attr(x, "label")`.** A `data.frame` with labelled columns
   passed to `rfsrc()` comes back with `f$xvar$age` label `NULL`.
2. **`varpro` strips them too.** `v$x$age` label is `NULL`.
   ⇒ Labels **cannot** be recovered from a fit. The caller must supply them.
3. **`varpro` carries `$xvar.org.names` and `$xvar.names`** — the original-vs-one-hot
   mapping is on the object. Preferred over `varpro_feature_names()`'s
   character-stripping heuristic for resolving `sex0`/`sex1` back to `sex`.
4. **`get.topvars()` returns far fewer names than `xvar.names`.** On a 3-variable
   test fit it returned one. Variables with no importance rank are the **common
   case**, not an edge case.
5. **`hvtiRutilities` cannot be depended on.** It is GitHub-only; ggRandomForests
   is CRAN. Its label API is also pure — `label_map(data)`, `get_labels(map, vars)`
   — so a `Suggests:` + `requireNamespace()` gate would buy zero capability, since
   the caller must supply the data or the map either way.

## Design

### Component 1 — `.forest_labels(labels)`, shared internal

Resolves a name → label lookup. **Base R only; no new dependency.**

| Input shape | Behavior |
|---|---|
| Labelled data frame | Read `attr(col, "label")` per column |
| Named character vector | `c(bpd_last = "BP Diastole")` — format-agnostic |
| Two-column `key`/`label` data frame | `hvtiRutilities::label_map()` output, consumed by shape |
| `NULL` (default) | Raw variable names — current behavior, unchanged |

Rules:
- A variable with no label falls back to its raw name. Never blank, never an error.
- A lookup that resolves **nothing** warns once, naming the likely cause. (Labels
  lost in a parquet round-trip present exactly this way.)
- Ambiguity between the labelled-data-frame and `key`/`label` shapes is resolved by
  checking for exactly the columns `key` and `label` first.

The two packages couple through a **data shape**, not a linkage. ggRandomForests
never names an internal package in its DESCRIPTION.

### Component 2 — `.varpro_importance_order(part_dta, object)`

Returns `names(part_dta)` reordered by importance.

- **With `object`:** rank against `varPro::get.topvars(object)`, resolving one-hot
  names via `$xvar.org.names` / `$xvar.names`. Where a variable has several one-hot
  levels, it takes its **best** rank across them.
- **Unranked variables:** keep `part_dta` order, appended after the ranked block.
  Per constraint 4 this carries most of the weight. **Nothing is dropped.**
- **Without `object`:** return `names(part_dta)` unchanged — list order, still
  better than alphabetical.

`.build_varpro_dfs()` then sets `name` as a **factor with those levels**, and
`nvars` slices **after** ranking. `facet_wrap()` inherits factor level order, so
the plot method needs no change for ordering.

### Known limitation — ordering does not cross the categorical/continuous split

`plot.gg_partial_varpro()` builds **two** ggplots and staples them with
`patchwork::wrap_plots(ncol = 1)`. Component 2 therefore orders correctly *within*
the continuous block and *within* the categorical block, but the blocks cannot
interleave: if importance ranks `female` third, it still draws in the lower block.

This is structural, not an oversight. Verified 2026-08-28: ggplot2 rejects a
numeric x in one panel and a discrete x in another within one `facet_wrap` —
*"Discrete value supplied to a continuous scale."* Scale **type** is per-plot;
`scales = "free_x"` frees the range, not the type.

Two further consequences of the split, also left in place here: `plot()` returns a
**patchwork**, not a ggplot, so `+ theme_hv_*()` does not compose as callers
expect; and the two blocks get independent y-scales, which is why the categorical
panel appears on a different range despite carrying the same units.

Unifying this is **follow-up work**, tracked separately. The candidate paths are:
map categorical levels onto numeric x positions (one facet, plain ggplot, per-panel
tick labels become the open problem); `ggh4x::facetted_pos_scales()` (new
dependency); or keep patchwork and fix only its symptoms (shared y-limits,
`heights=` proportional to panel counts).

### Component 3 — apply at four sites

**Where each concern lives.** Ordering is a **data** concern and belongs in the
constructor (`name`'s factor levels are set there). Labels are a **presentation**
concern and belong in the **plot method**: a new `labels =` argument on each
`plot.*` method, applied at draw time via ggplot2's `labeller=` and scale labels.

The returned object's columns keep **raw** variable names. This is deliberate —
per the family rule, changing a returned object's column names is a breaking
change for downstream consumers, and nothing downstream should have to know about
display labels.

| Surface | File | Gets |
|---|---|---|
| varPro partial | `plot.gg_partial_varpro.R` | labels + ordering |
| rfsrc partial | `plot.gg_partial.R` | labels |
| rfsrc VIMP | `plot.gg_vimp.R` | labels |
| varPro importance | `plot.gg_varpro.R` | labels |

Labels apply to facet strips and to the categorical axis.

`plot.gg_partialpro()` is a deprecated class shim that re-dispatches to
`plot.gg_partial_varpro()`; it must forward `labels=` so the deprecated path
behaves identically. Same for `autoplot.gg_partialpro()`.

**Excluded: RHF** (`gg_rhf_importance.R`, `plot.gg_rhf_importance.R`). A parallel
session owns those files. Follow-up once it lands.

### Component 4 — loud scale fallback

`.resolve_varpro_scale()` warns once when it resolves to `"generic"` for want of a
fit, naming the cause and the remedy (`pass object=`). **Defaults unchanged:**
`scale = "auto"` with a classification fit already resolves to `"prob"` and labels
the axis `P(Y = target)`.

## Out of scope

Deliberately not addressed, each separable:

- The three-series overlay (`parametric` / `nonparametric` / `causal`) drawn
  without a legend or annotation. **Deliberately retained** — it is the diagnostic
  view, and the docs teach reading it. Annotating the series (house style: annotate,
  never a legend) is a separate improvement, not part of this change.
- The categorical panel's `patchwork` layout and default fills, and the
  categorical/continuous split itself — see **Known limitation** above.
- RHF importance sites (above).

## Testing

`test_*.R` convention, data-carrying assertions — not `expect_s3_class(p, "ggplot")`.

**Labels**
- each of the three `labels=` shapes resolves correctly
- unlabelled variables fall back to their raw name
- a lookup resolving nothing warns exactly once
- `labels = NULL` reproduces current output
- the returned object's columns still carry **raw** names after plotting with labels
- `plot.gg_partialpro()` (deprecated shim) forwards `labels=` identically

**Ordering**
- factor levels of `name` match `get.topvars()` order for a mocked fit
- unranked variables append after the ranked block; **none are dropped**
- one-hot names resolve to the original variable via `$xvar.org.names`
- `nvars = 3` returns the top 3 **by importance**, not the first 3 — regression
  test for defect 2
- no `object` → `part_dta` list order preserved

**Scale**
- no `object` → warning emitted, `prov$scale == "generic"`
- `family == "class"` → `prov$scale == "prob"`, axis label `P(Y = ...)`

## Mechanics

- Git worktree at `~/Documents/GitHub/ggRandomForests-labels`, branch
  `feat/forest-plot-labels`, off `origin/main` — the parallel RHF session's
  checkout is untouched.
- **No DESCRIPTION version change.** NEWS bullets appended to the **end** of the
  `v4.0.0 (development)` section, so the parallel session's concurrent NEWS edit
  is a one-line conflict rather than a tangled one.
- `devtools::document()` run; `man/` committed with the source change.
- No `_pkgdown.yml` change — no new exports; all new functions are internal.
- `lintr::lint_package()` clean before pushing.
- PR into `main`; maintainer merges.

## Downstream

`hvti_graphics` chapters `varpro_partial.qmd` and `rf_vimp.qmd` render from
committed `_freeze/` and CI never re-executes R, so they will keep showing the
**unfixed** figures until deliberately re-rendered. Sequence that after this ships.
See `[[Claude/Tasks/recipes-labelled-forest-plots]]`.
