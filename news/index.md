# Changelog

## ggRandomForests v2.8.0 (in development)

- New
  [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  extractor and
  [`plot.gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md)
  method for time-resolved Brier scores and CRPS on survival forests
  (issue [\#9](https://github.com/ehrlinger/ggRandomForests/issues/9)).
  Wraps
  [`randomForestSRC::get.brier.survival()`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)
  and adds the mortality-quartile decomposition, a 15-85 percent
  per-subject envelope, and running CRPS via trapezoidal integration.
  Supports `cens.model = c("km", "rfsrc")`, `type = c("brier", "crps")`,
  and `envelope` (overall line + 15-85% ribbon). Multi-model comparison
  is left to
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on multiple `gg_brier` outputs — see
  [`?gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  for an example.
- First-class `varPro` integration to restore the variable-selection
  workflow that disappeared with `randomForestSRC::var.select.rfsrc()`.
  Planned: `varPro` in `Suggests:` (or `Imports:`),
  `gg_partialpro.varpro` S3 method, and a dedicated “Variable selection
  with varPro” vignette. See
  `Claude/Tasks/ggRandomForests v2.8.0 plan.md` (vault) for details.

## ggRandomForests v2.7.2

CRAN release: 2026-05-02

- Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and Ishwaran
    et al. 2008, with `<doi:...>` auto-links) per CRAN cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace [`cat()`](https://rdrr.io/r/base/cat.html) in
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    with [`message()`](https://rdrr.io/r/base/message.html) so progress
    output is suppressible
    ([`suppressMessages()`](https://rdrr.io/r/base/message.html)) and
    plays nicely inside notebooks / Shiny / quarto.
  - Restore the user’s [`par()`](https://rdrr.io/r/graphics/par.html)
    settings in the
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

## ggRandomForests v2.7.1

- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests:
  [`partial.rfsrc()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  was being called without `partial.type`, causing a zero-length
  comparison (`if (partial.type == "rel.freq") ...`) inside the C-level
  prediction routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new
  `partial.type` argument accepting `"surv"`, `"chf"`, or `"mort"`).
  This unblocks the `partial-dep` chunk in the survival vignette.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests with multiple `partial.time` values:
  [`get.partial.plot.data()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  returns yhat as an `[length(partial.values) x length(partial.time)]`
  matrix, but the previous code assumed a vector and crashed on
  column-mismatch when assigning `time`. The result is now reshaped to
  long form so each `(x, time)` pair is a single row.
- Improve
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  survival layout: predictor value is now on the x-axis with one curve
  per (rounded) time point coloured by `Time`, faceted by variable name.
  The previous default put time on the x-axis and one curve per
  predictor value, producing a saturated legend with dozens of
  nearly-identical lines.
- Add `tests/testthat/test_plot_layer_data.R`: regression suite that
  uses
  [`ggplot2::layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  to verify each `plot.gg_*()` method renders non-empty layers for every
  supported forest family. Catches the empty-figure class of bug
  (transform/plot column-name mismatch) without requiring visual
  inspection.
- [`ggrandomforests.news()`](https://ehrlinger.github.io/ggRandomForests/reference/ggrandomforests.news.md)
  now reads `NEWS.md` (the canonical change log R also surfaces via
  [`utils::news()`](https://rdrr.io/r/utils/news.html)). The legacy
  hand-maintained `inst/NEWS` has been removed — it had silently drifted
  to v2.4.0 (June 2025) across three releases, so users running the
  helper saw stale version info. One source of truth, no more drift
  window.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  legend duplication: the bar geom mapped both `fill` and `color` to the
  `positive` column, but only the fill legend was titled “VIMP \> 0”,
  leaving a redundant second legend titled “positive”. Both aesthetics
  now share the “VIMP \> 0” title so ggplot merges them into a single
  legend by default.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  for forests with all-positive VIMP: the bar geom previously mapped
  only `color` (no `fill`), producing hollow / outline- only bars and an
  “Ignoring unknown labels: fill” warning whenever `labs(fill = ...)`
  was applied. Both `fill` and `color` are now mapped unconditionally,
  so bars render filled in every case.
- Add `@examples` blocks to
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partialpro.md).
  The latter uses a self-contained mock of the `varpro::partialpro()`
  output structure so the example runs without pulling in `varpro` as a
  dependency.

## ggRandomForests v2.7.0

- S3 design overhaul:
  [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md),
  and
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  now stamp their return values with S3 classes (`gg_partial`,
  `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatch
  without any boilerplate.
- Add
  [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md),
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md),
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partialpro.md)
  S3 methods; continuous predictors render as line plots, categorical as
  bar charts, faceted by variable name. Survival forests produce curves
  over time; two-variable surface plots group by `xvar2.name`.
- Convert
  [`gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  to an S3 generic dispatching on the class of its first argument. New
  [`gg_survival.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  method extracts the survival response directly from the fitted forest
  (no separate data argument needed);
  [`gg_survival.default()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  preserves the existing interface.
- Fix
  [`plot.gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)
  auto-coercion: previously called `gg_survival(rfsrc_obj)` treating the
  forest as the `interval` string argument, causing a latent crash;
  replaced with [`inherits()`](https://rdrr.io/r/base/class.html) guard.
- Deprecate
  [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
  via [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) with a
  pointer to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md);
  all package tests updated to suppress the warning.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  — `make_eval_grid()` used `unlist(dplyr::select())` which coerced
  factor columns to integer codes; now uses `newx[[xname]]` to preserve
  column class. Categorical detection extended to cover
  [`is.factor()`](https://rdrr.io/r/base/factor.html) and
  [`is.character()`](https://rdrr.io/r/base/character.html) in addition
  to the cardinality check.
- Add guards to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md):
  all-NA `xval` after NA removal now emits a warning and skips the
  variable; all-NA grouping variable (`xvar2`) calls
  [`stop()`](https://rdrr.io/r/base/stop.html); `n_eval` and `cat_limit`
  are validated as single integers \>= 2 near function entry.
- Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
- Fix `@param partial.time` documentation: “see the section above”
  corrected to “see the section below”.
- Replace deprecated
  [`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html)
  with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  in
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partialpro.md).
- Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
- Update tests: add `expect_s3_class()` checks for all new classes; add
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) smoke tests
  for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`; add
  `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
- Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and
  `plot.gg_partialpro` to `_pkgdown.yml` reference index.

## ggRandomForests v2.7.0

- Fix critical visual bug in `plot.gg_rfsrc`: all
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) calls used
  bare string literals instead of `.data[[col]]`, causing every
  aesthetic to map to a constant string rather than the underlying data
  column. All plot types (regression, classification, survival) were
  affected.
- Fix [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  bare-string literals in `plot.gg_roc` multi-class branch; remove
  unreachable `if (crv < 2)` dead-code branch.
- Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative
  index computed via
  [`colnames()`](https://rdrr.io/r/base/colnames.html) was a no-op on
  large datasets and a latent crash for data with ≤ 2 unique event
  times.
- Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing
  columns; replaced with `!col %in% colnames()` guard.
- Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`;
  now recovers the training frame via `.rf_recover_model_frame()`.
- Fix legend suppression in `plot.gg_error` for single-outcome forests
  where the data frame has no `variable` column.
- Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with
  `seq_len(nvar)` in both S3 methods; `1:0` silently returned `c(1, 0)`
  instead of `integer(0)` when `nvar == 0`.
- Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all
  `context()` calls removed; testthat 1.x `expect_that` /
  `is_identical_to` removed.
- Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
- Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
- Add `covr` and `vdiffr` to `Suggests`.

## ggRandomForests v2.6.1

- Fix model-label assignment in `gg_partial` for categorical variable
  data
- Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalisation and categorical data handling

## ggRandomForests v2.6.0

- Add and export new plotting functions; update existing plot
  documentation
- Improve unit and integration tests; overall coverage raised to 83%
- Remove `hvtiRutilities` internal dependency; clean up associated
  imports
- Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr`
  calls

## ggRandomForests v2.5.0

- Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via
  [`randomForestSRC::partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
  without requiring a separate `plot.variable` call
- Add support for a grouping variable (`xvar2.name`) in
  `gg_partial_rfsrc`
- Improved vignette formatting and namespace usage

## ggRandomForests v2.4.0

- Updating to latest ggplot2 functions
- Utilize some namespace referencing
- Added pkgdown documentation
- Minor testing improvements

## ggRandomForests v2.3.0

- Knocking the dust off this.
- Fix the ROC curves
- Fix the colors on VIMP plot

## ggRandomForests v2.2.1

CRAN release: 2022-09-01

- Fix docs for HTML5/Roxygen update

## ggRandomForests v2.2.0

CRAN release: 2022-05-09

- Bring back the regression vignette
- Improve package tests and code coverage
- Clean up code with lintr

## ggRandomForests v2.1.0

CRAN release: 2022-04-26

To pull this out of archive on randomForestSRC 3.1 build release. Fixed
a plot bug for gg_error to show the actual curve (issue 35)

## ggRandomForests v2.0.1

CRAN release: 2016-09-07

- Correct a bug in survival plots when predicting on future data without
  a known outcome.
- All Vignettes are now at <https://github.com/ehrlinger/ggRFVignette>
- All tests are being moved to
  <https://github.com/ehrlinger/ggRFVignette>
- Begin work on rewriting all checks to not use cached data. This will
  require more runtime, and hence we will run fewer of them on CRAN
  release.
- Minor bug and documentation fixes.

## ggRandomForests v2.0.0

CRAN release: 2016-06-11

- Added initial support for the randomForest package
- Updated cache files for randomForestSRC 2.2.0 release.
- Remove regression vignettes to meet CRAN size limits. These remain
  available at the package source
  <https://github.com/ehrlinger/ggRandomForests>
- Minor bug and documentation fixes.

## ggRandomForests v1.2.1

CRAN release: 2015-12-12

- Update cached datasets for randomForestSRC 2.0.0 release.
- Correct some vignette formatting errors (thanks Joe Smith)

## ggRandomForests v1.2.0

CRAN release: 2015-11-15

- Convert to semantic versioning <http://semver.org/>
- Updates for release of ggplot2 2.0.0
- Change from reshape2::melt dependence to tidyr::gather
- Optimize tests for CRAN to optimize R CMD CHECK times.

## ggRandomForests v1.1.4

CRAN release: 2015-03-29

- `combine.gg_partial` bug when giving a single variable plot.variable
  object.

- Remove `dplyr` depends to transitions from “Imports” to “Suggests”.

- Argument for single outcome `gg_vimp` plot for classification forests.

- Improvements to `gg_vimp` arguments for consistency.

- Add bootstrap confidence intervals to `gg_rfsrc` function.

- Initial `partial.rfsrc` function to replace the
  [`randomForestSRC::plot.variable`](https://www.randomforestsrc.org//reference/plot.variable.rfsrc.html)
  function.

- Move cache data to `randomForestSRC` v1.6.1 to take advantage of
  `rfsrc` version checking between function calls.

- Vignette updates for JSS submission of “ggRandomForests: Exploring
  Random Forest Survival”.

- Vignette updates for arXiv submission of ggRandomForests: Random
  Forests for Regression

- Some optimizations to reduce package size.

- Remove all tests from CRAN build to optimise R CMD CHECK times.

- Remove pdf vignette figure from CRAN build.

- Return S3method calls to NAMESPACE for “S3 methods exported but not
  registered” for R V3.2+.

- Misc Bug Fixes.

## ggRandomForests v1.1.3

CRAN release: 2015-01-08

- Update “ggRandomForests: Visually Exploring a Random Forest for
  Regression” vignette.
- Further development of draft package vignette “Survival with Random
  Forests”.
- Rename vignettes to align with randomForestSRC package usage.
- Add more tests and example functions.
- Refactor `gg_` functions into S3 methods to allow future
  implementation for other random forest packages.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.2

CRAN release: 2014-12-25

- Add package vignette “ggRandomForests: Visually Exploring a Random
  Forest for Regression”
- Add gg_partial_coplot, quantile_cuts and surface_matrix functions
- export the calc_roc and calc_auc functions.
- replace tidyr function dependency with reshape2 (melt instead of
  gather) due to lazy eval issues.
- reduce dplyr dependencies (remove select and %\>% usage for base
  equivalents, I still use tbl_df for printing)
- Further development of package vignette “Survival with Random Forests”
- Refactor cached example datasets for better documentation, estimates
  and examples.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.1

CRAN release: 2014-12-13

Maintenance release, mostly to fix gg_survival and gg_partial plots. \*
Fix the gg_survival functions to plot kaplan-meier estimates. \* Fix the
gg_partial functions for categorical variables. \* Add some more S3
print functions. \* Try to make gg_functions more consistent. \* Further
development of package vignette “Survival with Random Forests” \* Modify
the example cached datasets for better estimates and examples. \*
Improve help files. \* Misc Bug Fixes.

## ggRandomForests v1.1.0

CRAN release: 2014-12-05

- Add panel option for gg_variable and gg_partial
- Rework interactions plot
- add gg_coplot functions
- Imports instead of depends
- Add version dependencies for randomForestSRC
- Include package vignette “Random Forests for Survival”
- Misc Bug Fixes

## ggRandomForests v1.0.0

CRAN release: 2014-10-15

- First CRAN release.

## ggRandomForests v0.2

- Initial useR!2014 release.
