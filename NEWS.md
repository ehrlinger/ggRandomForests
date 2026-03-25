Package: ggRandomForests
Version: 2.7.0

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
