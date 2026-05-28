## v3.0.0 — varPro integration (major release)

This is a major release following v2.7.3 (current on CRAN). The version
jumps to 3.0.0 because it adds a substantial new feature layer and
soft-deprecates one user-facing function.

### Major changes in v3.0.0

- **New varPro wrapper family.** Tidy extractors and `plot()` methods for
  the `varPro` package: `gg_partial_varpro()` (partial dependence),
  `gg_varpro()` (variable importance), `gg_udependent()` (cross-variable
  dependency graph), `gg_isopro()` (isolation-forest anomaly scores),
  `gg_beta_varpro()` (per-rule beta importance), and `gg_ivarpro()`
  (individual / local importance), each with `print` / `summary` /
  `autoplot` companions and a dedicated "varpro" vignette.
- **Soft-deprecation.** `gg_partialpro()` now warns and forwards to
  `gg_partial_varpro()`; it will be removed in the release after v3.0.0.
- **randomForest engine fixes.** `gg_variable.randomForest()`
  classification, `gg_roc()` / `calc_roc()` for `randomForest` (true
  probability-based, macro-averaged ROC), per-class one-vs-rest ROC
  (`per_class = TRUE`), and `plot.gg_variable()` now returns a single
  `ggplot` / `patchwork` object rather than a bare list.
- **Importance-plot ordering.** All importance plots now place the
  most-important variable at the top, matching the `gg_vimp` convention.

### Dependency change (reverse-dependency safe)

`randomForestSRC` and `randomForest` move from `Depends:` to `Imports:`,
and `varPro` is added to `Imports:`. `library(ggRandomForests)` no longer
attaches the forest packages to the search path. There are **0 reverse
dependencies** on CRAN, so no downstream packages are affected.

## Test environments

* **Local:** R 4.5.3 on macOS (aarch64-apple-darwin20).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 0 notes.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release) —
  all green on the head commit.
* **Reverse-dependency check:** `tools::package_dependencies(reverse =
  TRUE)` returns 0.
* **URLs:** `urlchecker::url_check()` clean.

## NOTE disposition

No notes in local `R CMD check --as-cran`. Examples that fit survival
forests are wrapped in `\donttest` and run in a few seconds each.
