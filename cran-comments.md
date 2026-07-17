## v3.5.0 — minor release (SHAP explanations; default S3 methods; survival partial-dependence labelling; randomForest VIMP fixes)

This is a minor feature-and-fix release. It consolidates the work developed
since the CRAN 3.4.0 release (the 3.4.1 and 3.5.0 development cycles) into a
single submission.

### What's new / fixed

* **SHAP explanations.** New `gg_shap()` with `plot`/`autoplot`/`print`/
  `summary` methods, plus `shap_importance()`, `shap_beeswarm()` and
  `shap_dependence()`, giving SHAP explanations of regression and
  classification forests by wrapping `kernelshap` (Suggests). `gg_shap()`
  enforces the documented integer contract on `bg_n` and `which.class` rather
  than silently coercing them: `bg_n = 1.9` was truncated to 1, `bg_n = Inf`
  became `NA`, and `which.class = 2.9` passed the range check and then indexed
  column 2 — returning SHAP values for a class the caller never asked for.
  Non-whole, non-finite, out-of-range and non-scalar values now raise a clear
  error; valid input is unaffected.
* **Default S3 methods for the classic wrappers.** The remaining
  `rfsrc`/`randomForest` wrappers — `gg_error()`, `gg_vimp()` and others —
  gained `default` methods, so an unsupported object now produces an
  informative error instead of dispatching somewhere unhelpful.
* **Bug fix: `gg_vimp()` reports the importance a `randomForest` fit actually
  computed.** A `randomForest` importance matrix stores permutation importance
  and node impurity side by side on incommensurable scales; `gg_vimp()` ranked
  them together, so node impurity (thousands) swept the top and the permutation
  values the caller asked for by passing `importance = TRUE` (tens) were
  truncated away — `randomForest(medv ~ ., Boston, importance = TRUE)` showed
  `lstat = 12576.7` where the permutation value is `62.4`. The permutation
  measure is now reported and node impurity left out of the ranking, for both
  regression and classification. Alongside: `which.outcome` resolves the class
  column by name (a `randomForest` matrix has no overall-first column, so `0`
  had returned the first class mislabelled as overall), `nvar` counts variables
  and ranks before trimming (it had been keeping the least-important variables),
  and the selected measure is named in the `set` column rather than the
  literal `"vimp"`.
* **Bug fix: survival partial dependence is no longer mistakable for a
  probability.** `randomForestSRC::plot.variable()` defaults to
  `surv.type = "mort"`, so `gg_partial()`'s `yhat` is *mortality* — an expected
  event count, not a value on [0, 1] — and it only superficially resembles a
  percentage. `yhat` is passed through unscaled (rescaling it would corrupt the
  quantity); instead the label describing what was plotted is carried on the
  object as `attr(x, "ylabel")` and used as the y-axis title by
  `plot.gg_partial()`. Note that `gg_partial_rfsrc()` defaults to
  `partial.type = "surv"` and so does report survival probabilities: the two
  entry points report different quantities by default. (#15)
* **Smaller tarball.** The vignettes now render figures with `ragg` and quantise
  them to a 256-colour palette, taking the source tarball from 4.7 MB to 2.3 MB
  and `inst/doc` from 5.3 MB to 1.9 MB (the installed-size INFO is gone). The
  vignettes had never set a graphics device and so fell through to the default
  `png()`, which writes RGBA truecolor. Figures are visually unchanged (mean
  pixel difference 1.55 on a 0-255 scale). Both steps are guarded by
  `requireNamespace()` and degrade to no-ops, so a vignette rebuild on a machine
  without `ragg` or ImageMagick still succeeds.
* Documentation: the package help page (`?ggRandomForests`) now describes the
  whole current surface — the SHAP, Brier, varPro and unsupervised-varPro
  families were missing — and no longer claims that `plot()` methods may return
  a *list* of `ggplot2` objects; each returns a single plottable object (a
  `ggplot`, or a `patchwork` composite for the multi-panel methods).

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` (with the manual) returns 0 errors, 0 warnings,
  0 notes; overall check time under 4 minutes.
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.
* **URL check:** `urlchecker::url_check()` reports all URLs correct.

### NOTE disposition

`R CMD check --as-cran` is clean (0/0/0) locally.

The note expected at incoming feasibility is "Days since last update: 14"
(3.4.0 was published 2026-07-02). The submission is deliberate rather than a
correction to 3.4.0: it lands the SHAP family, a self-contained feature set
developed and reviewed as a unit, together with the `gg_partial()` fix in #15,
where the plotted quantity could be read as a probability when it is an
expected event count, and the `gg_vimp()` fixes, where a `randomForest` fit
reported node impurity in place of the permutation importance the caller
requested. I am happy to hold this release and resubmit later if the cadence is
unwelcome.

The gcc-UBSAN guard from v3.1.1/v3.1.2 is unchanged: the single unsupervised
`varPro::isopro(method = "unsupv")` test still calls `skip_on_cran()` to avoid
an upstream `randomForestSRC` sanitizer report (`rfsrcGrow`, `entry.c:184`);
all other varPro tests run. This is the only grow that trips the report (its
`yvar.wt` is length-0); `uvarpro()` and the other varPro grows are
synthetic-supervised and sanitizer-clean. ggRandomForests remains a pure-R
package (`NeedsCompilation: no`).
