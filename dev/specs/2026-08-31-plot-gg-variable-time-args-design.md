# Design: retire `plot.gg_variable()`'s dead time formals, guard what replaces them

**Date:** 2026-08-31
**Issue:** [#251](https://github.com/ehrlinger/ggRandomForests/issues/251)
**Status:** implemented in [#260](https://github.com/ehrlinger/ggRandomForests/pull/260)
**Target version:** 4.0.0 (unreleased development line)

## Problem

`plot.gg_variable()` declares and documents two formals its body never reads:

```r
plot.gg_variable <- function(x, xvar, time, time_labels, panel = FALSE,
                             oob = TRUE, points = TRUE, smooth = TRUE,
                             labels = NULL, time_units = NULL, ...)
```

`time` and `time_labels` are real parameters of `gg_variable()`, the extractor,
which reads them out of `...` and uses them to select the horizon and label the
resulting `time` column. By the time `plot()` runs, the horizon is already baked
into `gg_dta$time` and the method reads only that column. Searching the body for a
bare `time` or `time_labels` returns nothing: every occurrence is `gg_dta$time`, a
column-name string, `time_units`, or a comment.

`man/plot.gg_variable.Rd` advertises both on the method, so `?plot.gg_variable`
tells a user they can select a horizon at plot time. They cannot:

```r
gg_dta <- gg_variable(rf, time = 90)
plot(gg_dta, xvar = "age", time = 1191)   # still the 90 plot, no error, no warning
```

The failure is silent because the arguments match named formals rather than
falling into `...`, so neither R nor the method complains.

A second, related gap arrived with [#250](https://github.com/ehrlinger/ggRandomForests/pull/250).
`time_units` is validated only as "NULL or a single non-empty string"
(`.check_time_units()`, `R/utils.R`), never against the data it describes, so a
wrong unit produces a confidently wrong axis title:

```r
plot(gg_dta, xvar = "age", time_units = "years")  # "Survival at 1191 years"
```

Both are the same defect: an argument accepted, never validated against the data,
and reported as neither applied nor rejected. The same class was fixed four times
in this package during the week of 2026-08-29 (PRs 240, 242, 244) and is filed
upstream as [kogalur/varPro#7](https://github.com/kogalur/varPro/issues/7).

## Decisions

Four maintainer decisions, 2026-08-31:

1. **Horizon selection is the extractor's job, and only the extractor's.**
   `plot()` renders whatever the object already contains. Wiring `plot()` up to
   re-slice was rejected: it duplicates extractor logic in a plot method and lets
   two places disagree about what a horizon means.
2. **Remove the formals now, no deprecation cycle.** Not a breaking change to any
   released version: 4.0.0 is unreleased, there are zero CRAN reverse dependencies,
   and no caller in `R/`, `tests/` or `vignettes/` passes `time=` to `plot()`.
3. **Catch the retired names in `...` and redirect.** Removal alone would let
   `time=` fall into `...` and trip ggplot2's generic `Ignoring unknown parameters`,
   which points at a geom rather than at the call that works.
4. **Give `time_units` a narrow, one-directional plausibility check.** Only the
   case that actually occurred.

## Changes

### 1. Remove `time` and `time_labels`

Delete both formals from the signature and both `@param` entries from the roxygen
block. Regenerate `man/plot.gg_variable.Rd`.

### 2. Retired-name guard

A new internal helper in `R/utils.R`, called once near the top of
`plot.gg_variable()`:

```r
.check_retired_time_args(...)
```

It inspects `names(list(...))` for `time` and `time_labels` and warns, naming the
call that works:

> `plot.gg_variable(): 'time' selects a horizon at extraction time, not at plot`
> `time. Use gg_variable(rf, time = ...) instead.`

A **warning, not an error.** The plot is still correct for the horizon the object
holds, so aborting would be worse than drawing it with a correction.

The helper must not consume anything from `...`. `plot.gg_variable()` forwards
`...` to ggplot2 layers, and swallowing an argument it was meant to relay would
reintroduce the exact defect this change removes.

### 3. `time_units` plausibility check

`.check_time_units()` gains a second, **optional** argument carrying the time
values:

```r
.check_time_units(time_units, time_values = NULL)
```

It warns when a year-like unit (`year`, `years`, `yr`, `yrs`, matched
case-insensitively) is supplied against values exceeding **150**.

Three details the design review surfaced, each of which would otherwise bite
during implementation:

- **There are two callers, not one.** `plot.gg_variable()` (`R/plot.gg_variable.R:154`)
  and `plot.gg_rfsrc()` (`R/plot.gg_rfsrc.R:167`). The new argument defaults to
  `NULL` so the existing type validation is unchanged for both and neither call
  site is forced to change.
- **The time column does not always exist.** `plot.gg_variable()` calls this
  helper before it branches on family, and only the survival branch has a `time`
  column. Pass `gg_dta[["time"]]`, which is `NULL` for regression and
  classification, and skip the plausibility check whenever the values are absent,
  empty, or not numeric-coercible. The check is opportunistic, never required.
- **`gg_dta$time` is a factor.** `.survival_at_label()` already calls
  `as.character()` on it precisely because the integer codes are not the labels.
  Any magnitude comparison must go through
  `suppressWarnings(as.numeric(as.character(x)))` and ignore `NA` results, or it
  will compare factor codes and silently test the wrong thing.

150 sits above any plausible human survival horizon measured in years, with
margin. It fires on the observed case (a day-scale horizon labelled `"years"`) and
stays silent on real clinical data.

Warning, never an error: the package cannot know the unit, and a genuinely long
horizon is conceivable.

**The reverse direction is deliberately not checked.** Small values with `"days"`
is normal, so there is no reliable signal and a check would be noise. Likewise no
unit-to-magnitude table beyond the year case; that is speculative until a second
case appears.

## Testing

Data-carrying assertions, per the repo convention. No `expect_s3_class(p, "ggplot")`
as a substitute for checking behaviour.

**Retired-name guard** (`tests/testthat/test_gg_variable.R`)

- `plot(gg_dta, xvar = "age", time = 1191)` warns, and the message names `gg_variable(`
- the same for `time_labels`
- the returned object is still a valid plot: the warning corrects, it does not abort
- **a normal call warns about neither.** A guard that fires on everything is as
  useless as one that fires on nothing, and `...` is rarely empty in a plot method
- **other `...` arguments still reach the geoms.** `alpha = 0.3` lands in
  `aes_params`, proving the new inspection does not consume what it inspects

**`time_units` plausibility** (`tests/testthat/test_utils.R`)

- values around 1191 with `"years"` warns
- values around 1191 with `"days"` is silent
- values around 3 with `"years"` is silent, the reverse direction we do not check
- `"Yrs"` and `"YEAR"` warn, confirming case-insensitive matching
- `time_units = NULL` is silent and unchanged
- the existing type validation still errors on `character(0)`, `NA` and `""`

## Non-goals

Stated so they do not creep in during implementation:

- No re-slicing in `plot()`. Horizon stays extractor-only.
- No reverse-direction unit check.
- No unit-to-magnitude table beyond the year case.
- `facet_wrap(~time)` and the `gg_dta$time` column are untouched. That facet faces
  time rather than variables and is not part of this change.

## Definition of done

The repo's standard gate, in order:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'                                  # 0 lints
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'    # 0 failures
```

Then `R CMD check --as-cran` with the manual, from a clean `git archive` export,
once per PR.

No version bump. 4.0.0 is an unreleased line, so these land as bullets under its
open NEWS section.
