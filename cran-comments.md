## v3.1.0 — varPro integration + bug fix (major release)

### Resubmission

This resubmission addresses the one issue raised on the previous 3.1.0
submission: the overall check time exceeded 10 minutes on
r-devel-windows-x86_64, driven by the vignette rebuild (~331 s) and the
tests (~209 s). We have reduced both, per the three levers suggested
(small toy data, fewer iterations, precomputed results for the lengthiest
parts):

- The regression and survival vignettes use lighter forests (`ntree`
  200 / 150, imputation `ntree` 100) and coarser partial-dependence grids.
- The varpro vignette's three `gg_partial_varpro()` calls and the Boston
  `beta.varpro()` fit — together ~34 s, the bulk of that vignette's
  rebuild — are now precomputed offline (`vignettes/precompute_varpro.R`)
  and loaded from a 167 KB `vignettes/varpro_precomputed.rds`, with an
  automatic live-computation fallback if the file is absent.
- The `gg_udependent()` tests previously recomputed the same per-fit
  entropy matrix (~1.5 s) once per test; they now memoise it.

Locally the full vignette rebuild drops from ~80 s to ~38 s and the test
suite from ~44 s to ~36 s (R 4.6.0, aarch64-apple-darwin23), with no
change in test coverage or vignette content.

### Release context

v2.7.3 is the version currently published on CRAN. A v3.0.0 submission
(2026-05-28) cleared the Windows and Debian pretests (0/0/0) but did not
complete the review cycle; this 3.1.0 release supersedes it, carrying the
full v3.0.0 feature layer plus the v3.1.0 bug fix and documentation work.
The version is in 3.x territory because it adds a substantial new feature
layer and soft-deprecates one user-facing function.

### Changes in v3.1.0

- **Bug fix.** `gg_vimp()` on single-outcome `rfsrc` forests now correctly
  flags variables with non-positive VIMP in the `positive` column (used for
  plot colouring). The single-outcome fit names the column `VIMP`
  (uppercase) while the flag check read `$vimp` (lowercase), so `positive`
  stayed `TRUE` for every variable. Added a regression test.
- **Documentation.** Deepened the varPro-family and rfsrc importance /
  partial / survival help pages, made the `gg_vimp()` (permutation
  importance) vs `gg_varpro()` (release-rule importance) distinction
  explicit and cross-linked, and fixed a stale `@return` in `gg_roc()`. No
  user-facing behaviour change beyond the bug fix above.

### Major changes carried from v3.0.0

- **New varPro wrapper family.** Tidy extractors and `plot()` methods for
  the `varPro` package: `gg_partial_varpro()` (partial dependence),
  `gg_varpro()` (variable importance), `gg_udependent()` (cross-variable
  dependency graph), `gg_isopro()` (isolation-forest anomaly scores),
  `gg_beta_varpro()` (per-rule beta importance), and `gg_ivarpro()`
  (individual / local importance), each with `print` / `summary` /
  `autoplot` companions and a dedicated "varpro" vignette.
- **Soft-deprecation.** `gg_partialpro()` now warns and forwards to
  `gg_partial_varpro()`; it will be removed in a future release.
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

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 0 notes.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release),
  all green on the head commit.
* **Reverse-dependency check:** `tools::package_dependencies(reverse =
  TRUE)` returns 0.
* **URLs:** `urlchecker::url_check()` clean.

## NOTE disposition

No notes in local `R CMD check --as-cran`. Examples that fit survival
forests are wrapped in `\donttest` and run in a few seconds each.
