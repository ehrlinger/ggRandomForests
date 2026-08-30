# Make the survival time unit settable, not assumed — design (v1)

**Date:** 2026-08-29
**Branch:** `claude/sweet-williams-2c2479` (worktree)
**Status:** design approved in brainstorming; pending spec review

## Goal

Three survival axis labels in this package name a time unit the package was
never told. Stop asserting the unit by default, and give callers a supported
way to supply the one their fit actually uses.

## The bug

`plot.gg_variable()` builds its survival y-axis title by pasting the literal
string `"year"` onto a time value:

```r
y = paste("Survival at", gg_dta$time[1], "year")   # R/plot.gg_variable.R:345, :524
```

Nothing in that expression consults the data. `randomForestSRC::pbc` — this
package's own canonical survival example — records follow-up in **days**, so

```r
plot(gg_variable(rf), time = median(pbc$days), panel = TRUE)
```

renders `Survival at 1191 year` for a horizon of 1191 days. The label is not
merely uninformative; it is wrong by a factor of 365.

`plot.gg_rfsrc()` has the same defect on its x-axis:

```r
ggplot2::labs(x = "time (years)", y = "Survival (%)")   # R/plot.gg_rfsrc.R:290
```

Found while rendering the HVTI Recipes book (`hvtiGraphics`, branch
`codex/hvti-recipes-reader-alignment`), whose `rf_dependence.qmd` figure
published the wrong label and now carries an explicit `labs(y = ...)` to
suppress it. This package's own survival vignette already does the same thing
at `vignettes/ggRandomForests-survival.qmd:517` and `:537`. Two independent
downstream overrides is the evidence that the default is wrong rather than
just terse.

## Design

### API

One new formal on each affected method, placed immediately before `...`,
matching where `labels = NULL` already sits in `plot.gg_variable()`:

```r
plot.gg_variable(x, xvar, time, time_labels, panel, oob, points, smooth,
                 labels = NULL, time_units = NULL, ...)
plot.gg_rfsrc(x, notch = TRUE, time_units = NULL, ...)
```

`autoplot.gg_variable()` and `autoplot.gg_rfsrc()` are thin
`plot(object, ...)` forwarders, so the argument reaches `autoplot()` callers
with no parallel edit.

### Behaviour

The caller supplies the exact noun. No pluralisation, no inference.

| | `time_units = NULL` (default) | `time_units = "days"` |
|---|---|---|
| `plot.gg_variable()` y-axis | `Survival at 1191` | `Survival at 1191 days` |
| `plot.gg_rfsrc()` x-axis | `time` | `time (days)` |

The default is the bare, always-correct form. A unit appears only when the
caller states one.

### Validation

`time_units` must be `NULL` or a length-1 character vector. Anything else
errors, rather than pasting a mangled label into the plot.

### Rejected alternative: read a units attribute

`gg_variable()` builds its `time` column from `rf$time.interest`, and
`randomForestSRC` carries no units metadata on the fit. There is nothing to
read. Supporting it would mean inventing a propagation path through
`gg_variable()` for a value the fit never had.

## Scope

Three call sites: `R/plot.gg_variable.R:345`, `R/plot.gg_variable.R:524`,
`R/plot.gg_rfsrc.R:290`.

The remaining `year` hits in `R/` are `Surv(years, status)` formulas inside
`@examples` and prose about `partial.type = "years.lost"`. Both are correct as
written and are not touched.

### Explicitly not in scope

`plot.gg_variable()` declares a `time_labels` formal (line 137) that is
documented but never referenced in the body. It is a pre-existing dead
parameter, unrelated to units, and removing a documented parameter from an
exported method is a breaking change that deserves its own decision. It stays
as it is.

## Testing

The label *is* the contract, so assert it directly on the built object's
`$labels$x` / `$labels$y` rather than through an SVG diff. Each block carries
its own `set.seed()` per the determinism rule in `AGENTS.md`:

1. survival, `panel = TRUE`, default — y is `Survival at <t>`, contains no `year`
2. survival, `panel = TRUE`, `time_units = "days"` — y is `Survival at <t> days`
3. survival, `panel = FALSE`, default
4. survival, `panel = FALSE`, `time_units = "days"`
5. `plot.gg_rfsrc()` survival, default and `time_units = "days"`
6. a regression-branch label, asserting the edit did not reach past survival

Plus a validation test that a non-character `time_units` errors.

## vdiffr baselines

Only `gg-rfsrc-survival-no-ci.svg` and `gg-rfsrc-survival-bootstrap-ci.svg`
contain the string; the existing `gg-variable-*` baselines are all regression
and classification, so the `plot.gg_variable()` fix touches none of them.

Both are regenerated **last**, after the final full-suite run, per the
ordering rule in `AGENTS.md`. Every suite run in this work uses
`NOT_CRAN=true VDIFFR_RUN_TESTS=true`.

## Version

No bump. 4.0.0 is an unreleased development line, so this lands as a bullet
under the existing `ggRandomForests v4.0.0 (development)` heading in
`NEWS.md`. `DESCRIPTION` and the `NEWS.md` `Version:` line both stay at
4.0.0, and the version-grep test stays green without an edit.

## Done

```
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'                                  # 0 lints
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'    # 0 failures, 0 errors
R CMD check --as-cran                                               # from a clean git archive export
```
