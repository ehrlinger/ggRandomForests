# ggRandomForests: Visually Exploring Random Forests

`ggRandomForests` provides `ggplot2` (Wickham 2009) diagnostic and
exploration figures for fits from
[`randomForestSRC::rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html)
(`rfsrc` class; randomForestSRC \>= 3.4.0),
[`randomForestRHF::rhf()`](https://www.randomforestsrc.org//reference/rhf.html)
(`rhf` class; randomForestRHF \>= 2.0.0),
[`varPro::varpro()`](https://www.randomforestsrc.org/reference/varpro.html)
(`varpro` class; varPro \>= 3.1.0), or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html).

`randomForestSRC` gives a unified treatment of Breiman's (2001) random
forests across data settings: regression and classification forests when
the response is numeric or categorical, survival and competing-risk
forests (Ishwaran et al. 2008) for right-censored data.

## Details

The package is built on one decision: keep the data step and the figure
step apart. A `gg_*` function pulls a tidy data object out of the
forest; its [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method turns that object into a figure. Two things follow.

The data object stands on its own. It carries everything its plot needs,
so you can save it, inspect it, or come back to it later without keeping
the original forest – which can be large – in memory.

You are never locked into the default figure. Each
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method returns
a single plottable object: a `ggplot` you extend with `+`, or a
`patchwork` composite for the multi-panel methods. Add layers, swap
scales, apply a theme – or ignore the default entirely and build the
figure from the tidy data yourself. Every `gg_*` object also carries
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods:
[`print()`](https://rdrr.io/r/base/print.html) shows a short header
rather than dumping every row, and
[`summary()`](https://rdrr.io/r/base/summary.html) returns a diagnostics
object.

**Forest diagnostics**

- [`gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md):
  predicted versus observed values.

- [`gg_error`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md):
  OOB error against the number of trees.

- [`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md):
  variable importance ranking (Ishwaran et al. 2010).

- [`gg_variable`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md):
  marginal variable dependence.

- [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md):
  ROC curves for classification forests (see also
  [`calc_roc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
  and
  [`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)).

- [`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md):
  Kaplan-Meier / Nelson-Aalen estimates.

- [`gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md):
  time-resolved Brier score and CRPS for survival forests.

**Partial dependence**

- [`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md):
  tidies the output of `randomForestSRC::plot.variable(partial = TRUE)`.

- [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md):
  computes partial dependence from the fitted forest directly, via
  `partial.rfsrc`.

**SHAP explanations**

- [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md):
  SHAP values for regression and classification forests, wrapping
  [`kernelshap`](https://rdrr.io/pkg/kernelshap/man/kernelshap.html),
  with
  [`shap_importance`](https://ehrlinger.github.io/ggRandomForests/reference/shap_importance.md),
  [`shap_beeswarm`](https://ehrlinger.github.io/ggRandomForests/reference/shap_beeswarm.md)
  and
  [`shap_dependence`](https://ehrlinger.github.io/ggRandomForests/reference/shap_dependence.md)
  figures.

**varPro rule-based variable selection**

- [`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md):
  variable importance from a `varpro` fit.

- [`gg_partial_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  (alias
  [`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)):
  partial effects on an interpretable scale.

- [`gg_beta_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md):
  per-region lasso coefficients.

- [`gg_isopro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md):
  isolation-forest outlier scores.

- [`gg_ivarpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md):
  individual (per-observation) importance.

**Unsupervised varPro**

- [`gg_udependent`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md):
  variable dependency graph.

- [`gg_beta_uvarpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md):
  entropy-based variable ranking.

- [`gg_sdependent`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md):
  signal-variable detection.

**Random Hazard Forests** These functions are for time-to-event data
whose predictors can change during follow-up. They show how event risk,
time-varying discrimination, and variable priority change over time, and
let you inspect a saved tree-size tuning search.

- [`gg_rhf`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf.md):
  per-case event-risk curves over time.

- [`gg_auct`](https://ehrlinger.github.io/ggRandomForests/reference/gg_auct.md):
  AUC curves over follow-up time.

- [`gg_rhf_importance`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf_importance.md):
  variable priority by time window.

- [`gg_tune_rhf`](https://ehrlinger.github.io/ggRandomForests/reference/gg_tune_rhf.md):
  saved tree-size tuning paths by OOB risk or OOB iAUC.

See
[`vignette("rhf", package = "ggRandomForests")`](https://ehrlinger.github.io/ggRandomForests/articles/rhf.md)
for a longitudinal workflow that uses all four RHF families.

varPro is a required dependency (`Imports`), so the varPro families are
always available. randomForestRHF and kernelshap are in `Suggests`:
their functions check for the package and fail with a clear message when
it is not installed.

## References

Breiman, L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H, Kogalur U (2026). Fast Unified Random Forests for Survival,
Regression, and Classification (RF-SRC). R package version 3.6.2.
<https://cran.r-project.org/package=randomForestSRC>

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R
News 7(2), 25–31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random
survival forests. Ann. Appl. Statist. 2(3), 841–860.

Ishwaran, H., U. B. Kogalur, E. Z. Gorodeski, A. J. Minn, and M. S.
Lauer (2010). High-dimensional variable selection for survival data. J.
Amer. Statist. Assoc. 105, 205-217.

Ishwaran, H. (2007). Variable importance in binary regression trees and
forests. Electronic J. Statist., 1, 519-537.

Lu M, Ishwaran H (2024). Model-independent variable selection via the
rule-based variable priority framework. arXiv:2409.09003.
<https://arxiv.org/abs/2409.09003>

Ishwaran H, Kogalur U (2026). Model-Independent Variable Selection via
the Rule-Based Variable Priority. R package version 3.2.0.
<https://CRAN.R-project.org/package=varPro>

Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
arXiv:2608.21597.
[doi:10.48550/arXiv.2608.21597](https://doi.org/10.48550/arXiv.2608.21597)
.

Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
R package version 2.0.3.
<https://CRAN.R-project.org/package=randomForestRHF>

Wickham, H. ggplot2: elegant graphics for data analysis. Springer New
York, 2009.
