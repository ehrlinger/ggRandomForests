Package: ggRandomForests
Version: 2.7.3.9006

ggRandomForests v2.8.0 (development) — continued
=================================================
* **`gg_roc`: per-class one-vs-rest ROC curves (#88, closes #72).**
  - `gg_roc()` gains a `per_class = FALSE` argument.  When `per_class = TRUE`
    and the forest has more than two classes, returns a long-format `gg_roc`
    data frame with a `class` factor column and a named AUC vector attribute
    (one entry per class, ordered by descending AUC).
  - `plot.gg_roc()` gains a `panel = c("overlay", "facet")` argument.  When the
    `gg_roc` object contains a `class` column, the overlay path colours curves
    by class; the facet path wraps each class into its own panel.
  - `summary.gg_roc()` now prints named per-class AUC values when the `class`
    column is present.
  - Binary forests: `per_class = TRUE` is a silent no-op (single-curve result
    returned unchanged).
  - ROC confidence intervals are deferred to v2.9.0 (issue #7 / #72-CIs).
* **varPro variable dependency: `gg_udependent()` (Phase 3).**
  - `gg_udependent()` extracts cross-variable dependency scores from a
    `uvarpro` fit using `varPro::get.beta.entropy()` +
    `varPro::sdependent()`, and returns a tidy list with `$edges`
    (variable_from, variable_to, weight), `$nodes` (variable, degree,
    selected), and `$graph` (igraph object).
  - `plot.gg_udependent()` renders the dependency network using ggraph
    with edge width/opacity scaled by dependency strength and node colour
    by signal-variable status.  Layout is configurable (`"fr"`, `"kk"`,
    `"stress"`, etc.).
  - `ggraph` added to `Suggests:`.
* **varPro variable importance: `gg_varpro()` (#85).**
  - `gg_varpro()` extracts per-tree importance scores from a fitted
    `varpro` object and renders an honest boxplot — hinges at the
    15th/85th percentile, whiskers at the 5th/95th — of the per-tree
    z-score distribution per variable.  Variables whose aggregate
    z > `cutoff` (default 0.79) are colour-highlighted.
  - `faithful = TRUE` overlays individual per-tree z-scores as jittered
    semi-transparent points with a white-outlined mean dot, reproducing
    the distributional view from varPro's internal `bxp` output.
  - `conditional = TRUE` (classification forests only) extracts
    `$conditional.z` and renders class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - `local.std = FALSE` enables `plot(..., type = "raw")` to display
    raw per-tree importance instead of z-normalised values.

ggRandomForests v2.8.0 (development)
====================================
* **`gg_variable.randomForest` classification fix (#87).**
  - `gg_variable.randomForest()` for classification forests now stores
    per-class OOB vote fractions as `yhat.<classname>` columns (from
    `object$votes`), matching the `rfsrc` path.  Previously a single
    `yhat` factor column (class labels from `object$predicted`) was
    stored, which prevented the multi-class pivot in `plot.gg_variable`
    from firing.  Vote fractions are row-normalised to `[0, 1]` even
    when the forest was fit with `norm.votes = FALSE`.
  - `plot.gg_variable` binary classification: `smooth = TRUE` now
    correctly maps x/y aesthetics onto the smooth layer.
  - `plot.gg_variable` multi-class numeric path: `smooth = TRUE` now
    adds a smooth layer (was silently skipped).
  - Closes stale issues #81 (fixed in PR #83) and #82.
* **varPro partial dependence: `gg_partial_varpro()` (#84).**
  - `gg_partial_varpro()` replaces `gg_partialpro()` as the primary entry
    point for varPro partial dependence plots.  The new extractor accepts
    an optional `object` argument (the originating `varpro` fit) for
    provenance-aware axis labeling and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - **Ensemble mortality labeling** (Ishwaran et al. 2008): when
    `scale = "mortality"` (or `scale = "auto"` with a survival forest),
    the y-axis is labeled "Ensemble mortality (expected events)" — an
    unbounded relative-risk score, not a survival probability.  The
    documentation explicitly warns against misinterpretation.
  - **Survival path C:** `scale = "surv"` or `scale = "chf"` extracts
    `object$rf` (the embedded rfsrc forest) and returns true S(t)/CHF
    partial curves via `gg_partial_rfsrc` infrastructure.
  - `varPro` is now a hard dependency (`Imports:`).
  - `gg_partialpro()` is soft-deprecated: it emits a deprecation warning
    and delegates to `gg_partial_varpro()`.  Removal is planned for the
    release after v2.8.0.
* **randomForest engine validation & repair (#82).** Fixes #80, #81
  and a `plot.gg_error` label wart; adds full randomForest regression
  coverage. See sub-items below.
  - `plot.gg_variable()` now always returns a single `ggplot` (one
    variable) or a `patchwork` composite (multiple variables / default),
    never a bare list — consistent with the v2.7.3 `plot.gg_partial*`
    change. Previously a list was returned for multiple `xvar`, which
    broke `patchwork` / `autoplot()` / `layer_data()` composition (#80).
  - `gg_roc()` / `calc_roc()` for `randomForest` now compute a correct
    ROC from class probabilities (OOB votes by default, honoring `oob`)
    instead of a degenerate ~3-point curve; `which_outcome = "all"` (the
    default for `gg_roc(rf)`) returns a macro-averaged one-vs-rest ROC
    with no warning. The shared `.validate_which_outcome` helper and
    `calc_roc.rfsrc` are byte-unchanged — rfsrc behavior is preserved
    (#81).
* **Dependency modernization (breaking for scripts that relied on
  attachment).** `randomForestSRC` and `randomForest` moved from
  `Depends:` to `Imports:`; `igraph`, `callr`, and `varPro` added to
  `Suggests:` (`varPro` graduates to `Imports:` later in the v2.8.0
  development series, with the first varPro-integration component).
  `library(ggRandomForests)` no longer attaches
  `randomForestSRC`/`randomForest` to the search path. User scripts
  that called `rfsrc()`/`randomForest()` unqualified after only
  `library(ggRandomForests)` must now also `library(randomForestSRC)`
  / `library(randomForest)` (or qualify the calls). All ggRandomForests
  functions are unaffected — they fully qualify their dependencies.

ggRandomForests v2.7.3
======================
* `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and `plot.gg_partialpro()`
  now always return a single `ggplot`/`patchwork` object. Previously, when
  both continuous and categorical predictors were present, they returned a
  named list `list(continuous=, categorical=)`, which surprised users and
  made `autoplot()` dispatch ambiguous. The two panels are now combined
  vertically via `patchwork::wrap_plots()` (patchwork moved from `Suggests`
  to `Imports`). Closes #77.
* `autoplot()` S3 methods for all 10 `gg_*` classes, delegating to the
  corresponding `plot.gg_*()` method so objects work in `|>` pipelines,
  `patchwork`, and `cowplot` compositions via `ggplot2::autoplot()`.
* `print()` and `summary()` S3 methods for every `gg_*` data object
  (gg_error, gg_vimp, gg_rfsrc, gg_variable, gg_partial,
  gg_partial_rfsrc, gg_partialpro, gg_roc, gg_survival, gg_brier).
  `print()` is header-only — use `head()` for rows. `summary()`
  returns a printable `summary.gg` object with per-class diagnostics.
  Each `gg_*` constructor now attaches a `"provenance"` attribute
  (source, family, ntree, n, xvar.names) consumed by the new methods.
* New `gg_brier()` extractor and `plot.gg_brier()` method for time-resolved
  Brier scores and CRPS on survival forests (issue #9). Wraps
  `randomForestSRC::get.brier.survival()` and adds the mortality-quartile
  decomposition, a 15-85 percent per-subject envelope, and running CRPS
  via trapezoidal integration. Supports `cens.model = c("km", "rfsrc")`,
  `type = c("brier", "crps")`, and `envelope` (overall line + 15-85%
  ribbon). Multi-model comparison is left to `dplyr::bind_rows()` on
  multiple `gg_brier` outputs — see `?gg_brier` for an example.
* Visual unification of ribbon overlays across plot methods. All
  ribbons now use a shared alpha (`.gg_ribbon_alpha = 0.2`) and a
  shared fill (`.gg_ribbon_fill = "steelblue"`) for single-series
  cases (KM/NA CIs, bootstrap CIs, `gg_brier` envelope); group-stratified
  ribbons keep their group-coloured fill. Statistical bounds unchanged —
  only styling.
ggRandomForests v2.7.2
=====================
* Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and
    Ishwaran et al. 2008, with `<doi:...>` auto-links) per CRAN
    cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace `cat()` in `surv_partial.rfsrc()` with `message()` so
    progress output is suppressible (`suppressMessages()`) and plays
    nicely inside notebooks / Shiny / quarto.
  - Restore the user's `par()` settings in the
    `surv_partial.rfsrc()` example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

ggRandomForests v2.7.1
=====================
* Fix `gg_partial_rfsrc()` for survival forests: `partial.rfsrc()` was being
  called without `partial.type`, causing a zero-length comparison
  (`if (partial.type == "rel.freq") ...`) inside the C-level prediction
  routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new `partial.type`
  argument accepting `"surv"`, `"chf"`, or `"mort"`). This unblocks the
  `partial-dep` chunk in the survival vignette.
* Fix `gg_partial_rfsrc()` for survival forests with multiple
  `partial.time` values: `get.partial.plot.data()` returns yhat as an
  `[length(partial.values) x length(partial.time)]` matrix, but the previous
  code assumed a vector and crashed on column-mismatch when assigning
  `time`. The result is now reshaped to long form so each `(x, time)` pair
  is a single row.
* Improve `plot.gg_partial_rfsrc()` survival layout: predictor value is now
  on the x-axis with one curve per (rounded) time point coloured by `Time`,
  faceted by variable name. The previous default put time on the x-axis
  and one curve per predictor value, producing a saturated legend with
  dozens of nearly-identical lines.
* Add `tests/testthat/test_plot_layer_data.R`: regression suite that uses
  `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders
  non-empty layers for every supported forest family. Catches the
  empty-figure class of bug (transform/plot column-name mismatch) without
  requiring visual inspection.
* `ggrandomforests.news()` now reads `NEWS.md` (the canonical change log
  R also surfaces via `utils::news()`). The legacy hand-maintained
  `inst/NEWS` has been removed — it had silently drifted to v2.4.0
  (June 2025) across three releases, so users running the helper saw
  stale version info. One source of truth, no more drift window.
* Fix `plot.gg_vimp()` legend duplication: the bar geom mapped both
  `fill` and `color` to the `positive` column, but only the fill legend
  was titled "VIMP > 0", leaving a redundant second legend titled
  "positive". Both aesthetics now share the "VIMP > 0" title so ggplot
  merges them into a single legend by default.
* Fix `plot.gg_vimp()` for forests with all-positive VIMP: the bar geom
  previously mapped only `color` (no `fill`), producing hollow / outline-
  only bars and an "Ignoring unknown labels: fill" warning whenever
  `labs(fill = ...)` was applied. Both `fill` and `color` are now mapped
  unconditionally, so bars render filled in every case.
* Add `@examples` blocks to `plot.gg_partial_rfsrc()` and
  `plot.gg_partialpro()`. The latter uses a self-contained mock of the
  `varpro::partialpro()` output structure so the example runs without
  pulling in `varpro` as a dependency.

ggRandomForests v2.7.0
=====================
* S3 design overhaul: `gg_partial()`, `gg_partialpro()`, and
  `gg_partial_rfsrc()` now stamp their return values with S3 classes
  (`gg_partial`, `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  `plot()` dispatch without any boilerplate.
* Add `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and
  `plot.gg_partialpro()` S3 methods; continuous predictors render as line
  plots, categorical as bar charts, faceted by variable name.  Survival
  forests produce curves over time; two-variable surface plots group by
  `xvar2.name`.
* Convert `gg_survival()` to an S3 generic dispatching on the class of its
  first argument.  New `gg_survival.rfsrc()` method extracts the survival
  response directly from the fitted forest (no separate data argument
  needed); `gg_survival.default()` preserves the existing interface.
* Fix `plot.gg_survival()` auto-coercion: previously called
  `gg_survival(rfsrc_obj)` treating the forest as the `interval` string
  argument, causing a latent crash; replaced with `inherits()` guard.
* Deprecate `surv_partial.rfsrc()` via `.Deprecated()` with a pointer to
  `gg_partial_rfsrc()`; all package tests updated to suppress the warning.
* Fix `gg_partial_rfsrc()` — `make_eval_grid()` used `unlist(dplyr::select())`
  which coerced factor columns to integer codes; now uses `newx[[xname]]` to
  preserve column class.  Categorical detection extended to cover
  `is.factor()` and `is.character()` in addition to the cardinality check.
* Add guards to `gg_partial_rfsrc()`: all-NA `xval` after NA removal now
  emits a warning and skips the variable; all-NA grouping variable (`xvar2`)
  calls `stop()`; `n_eval` and `cat_limit` are validated as single integers
  >= 2 near function entry.
* Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
* Fix `@param partial.time` documentation: "see the section above" corrected
  to "see the section below".
* Replace deprecated `tidyr::gather()` with `tidyr::pivot_longer()` in
  `plot.gg_vimp()` and `plot.gg_partialpro()`.
* Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
* Update tests: add `expect_s3_class()` checks for all new classes; add
  `plot()` smoke tests for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`;
  add `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
* Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and `plot.gg_partialpro`
  to `_pkgdown.yml` reference index.

ggRandomForests v2.7.0
=====================
* Fix critical visual bug in `plot.gg_rfsrc`: all `aes()` calls used bare
  string literals instead of `.data[[col]]`, causing every aesthetic to map
  to a constant string rather than the underlying data column. All plot
  types (regression, classification, survival) were affected.
* Fix `aes()` bare-string literals in `plot.gg_roc` multi-class branch;
  remove unreachable `if (crv < 2)` dead-code branch.
* Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative index
  computed via `colnames()` was a no-op on large datasets and a latent crash
  for data with ≤ 2 unique event times.
* Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing columns;
  replaced with `!col %in% colnames()` guard.
* Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`; now
  recovers the training frame via `.rf_recover_model_frame()`.
* Fix legend suppression in `plot.gg_error` for single-outcome forests where
  the data frame has no `variable` column.
* Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with `seq_len(nvar)`
  in both S3 methods; `1:0` silently returned `c(1, 0)` instead of
  `integer(0)` when `nvar == 0`.
* Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all `context()`
  calls removed; testthat 1.x `expect_that` / `is_identical_to` removed.
* Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
* Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
* Add `covr` and `vdiffr` to `Suggests`.

ggRandomForests v2.6.1
=====================
* Fix model-label assignment in `gg_partial` for categorical variable data
* Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalisation and categorical data handling

ggRandomForests v2.6.0
=====================
* Add and export new plotting functions; update existing plot documentation
* Improve unit and integration tests; overall coverage raised to 83%
* Remove `hvtiRutilities` internal dependency; clean up associated imports
* Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr` calls

ggRandomForests v2.5.0
=====================
* Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via `randomForestSRC::partial.rfsrc`, without
  requiring a separate `plot.variable` call
* Add support for a grouping variable (`xvar2.name`) in `gg_partial_rfsrc`
* Improved vignette formatting and namespace usage

ggRandomForests v2.4.0
=====================
* Updating to latest ggplot2 functions
* Utilize some namespace referencing
* Added pkgdown documentation
* Minor testing improvements

ggRandomForests v2.3.0
=====================
* Knocking the dust off this.
* Fix the ROC curves
* Fix the colors on VIMP plot

ggRandomForests v2.2.1
=====================
* Fix docs for HTML5/Roxygen update

ggRandomForests v2.2.0
=====================
* Bring back the regression vignette
* Improve package tests and code coverage
* Clean up code with lintr

ggRandomForests v2.1.0
=====================
To pull this out of archive on randomForestSRC 3.1 build release.
Fixed a plot bug for gg_error to show the actual curve (issue 35)

ggRandomForests v2.0.1
======================
* Correct a bug in survival plots when predicting on future data without a known outcome.
* All Vignettes are now at https://github.com/ehrlinger/ggRFVignette
* All tests are being moved to https://github.com/ehrlinger/ggRFVignette
* Begin work on rewriting all checks to not use cached data. 
  This will require more runtime, and hence we will run fewer of them on CRAN release. 
* Minor bug and documentation fixes.

ggRandomForests v2.0.0
======================
* Added initial support for the randomForest package
* Updated cache files for randomForestSRC 2.2.0 release.
* Remove regression vignettes to meet CRAN size limits. These remain available at the package source https://github.com/ehrlinger/ggRandomForests
* Minor bug and documentation fixes.

ggRandomForests v1.2.1
======================
* Update cached datasets for randomForestSRC 2.0.0 release. 
* Correct some vignette formatting errors (thanks Joe Smith)

ggRandomForests v1.2.0
======================
* Convert to semantic versioning http://semver.org/
* Updates for release of ggplot2 2.0.0
* Change from reshape2::melt dependence to tidyr::gather
* Optimize tests for CRAN to optimize R CMD CHECK times.


ggRandomForests v1.1.4
======================
* `combine.gg_partial` bug when giving a single variable plot.variable object.
* Remove `dplyr` depends to transitions from "Imports" to "Suggests".
* Argument for single outcome `gg_vimp` plot for classification forests.
* Improvements to `gg_vimp` arguments for consistency.
* Add bootstrap confidence intervals to `gg_rfsrc` function.
* Initial `partial.rfsrc` function to replace the `randomForestSRC::plot.variable` function.
* Move cache data to `randomForestSRC` v1.6.1 to take advantage of `rfsrc` version checking between function calls.

* Vignette updates for JSS submission of "ggRandomForests: Exploring Random Forest Survival".
* Vignette updates for arXiv submission of ggRandomForests: Random Forests for Regression

* Some optimizations to reduce package size.
* Remove all tests from CRAN build to optimise R CMD CHECK times.
* Remove pdf vignette figure from CRAN build.
* Return S3method calls to NAMESPACE for "S3 methods exported but not registered" for R V3.2+.
  
* Misc Bug Fixes.

ggRandomForests v1.1.3
======================
* Update "ggRandomForests: Visually Exploring a Random Forest for Regression" vignette.
* Further development of draft package vignette "Survival with Random Forests". 
* Rename vignettes to align with randomForestSRC package usage.
* Add more tests and example functions.
* Refactor `gg_` functions into S3 methods to allow future implementation for other random forest packages.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.

ggRandomForests v1.1.2
======================
* Add package vignette "ggRandomForests: Visually Exploring a Random Forest for Regression"
* Add gg_partial_coplot, quantile_cuts and surface_matrix functions
* export the calc_roc and calc_auc functions.
* replace tidyr function dependency with reshape2 (melt instead of gather) due to lazy eval issues.
* reduce dplyr dependencies (remove select and %>% usage for base equivalents, I still use tbl_df for printing)
* Further development of package vignette "Survival with Random Forests" 
* Refactor cached example datasets for better documentation, estimates and examples.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.


ggRandomForests v1.1.1
======================
Maintenance release, mostly to fix gg_survival and gg_partial plots.
* Fix the gg_survival functions to plot kaplan-meier estimates.
* Fix the gg_partial functions for categorical variables.
* Add some more S3 print functions.
* Try to make gg_functions more consistent.
* Further development of package vignette "Survival with Random Forests" 
* Modify the example cached datasets for better estimates and examples.
* Improve help files.
* Misc Bug Fixes.


ggRandomForests v1.1.0
======================
* Add panel option for gg_variable and gg_partial
* Rework interactions plot
* add gg_coplot functions
* Imports instead of depends
* Add version dependencies for randomForestSRC
* Include package vignette "Random Forests for Survival" 
* Misc Bug Fixes

ggRandomForests v1.0.0
======================
* First CRAN release.

ggRandomForests v0.2
======================
* Initial useR!2014 release. 
