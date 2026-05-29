# ggRandomForests: Visually Exploring Random Forests

[![active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/badges/latest/active.svg)

[![R-CMD-check](https://github.com/ehrlinger/ggRandomForests/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ehrlinger/ggRandomForests/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/ehrlinger/ggRandomForests/actions/workflows/lint.yaml/badge.svg)](https://github.com/ehrlinger/ggRandomForests/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/ehrlinger/ggRandomForests/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ehrlinger/ggRandomForests/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ehrlinger/ggRandomForests/graph/badge.svg)](https://app.codecov.io/gh/ehrlinger/ggRandomForests)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11526.svg)](https://doi.org/10.5281/zenodo.11526)

![ggRandomForests overview: predicted survival, variable importance, OOB
error, and partial dependence](reference/figures/README-overview.png)

`ggRandomForests` provides `ggplot2`-based diagnostic and exploration
plots for random forests fit with
[randomForestSRC](https://cran.r-project.org/package=randomForestSRC)
(\>= 3.4.0) or
[randomForest](https://cran.r-project.org/package=randomForest). It
separates data extraction from plotting so the intermediate tidy objects
can be inspected, saved, or used for custom analyses. Listed in the
[ggplot2 extensions gallery](https://exts.ggplot2.tidyverse.org/).

## Installation

``` r

# CRAN (stable)
install.packages("ggRandomForests")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("ehrlinger/ggRandomForests")
```

## Quick start

``` r

library(randomForestSRC)
library(ggRandomForests)

# 1. Fit a forest (regression)
rf <- rfsrc(medv ~ ., data = MASS::Boston, importance = TRUE)

# 2. Check convergence: did the forest grow enough trees?
plot(gg_error(rf))

# 3. Rank predictors by importance
plot(gg_vimp(rf))

# 4. Marginal dependence for top variables
gg_v <- gg_variable(rf)
plot(gg_v, xvar = "lstat")
plot(gg_v, xvar = rf$xvar.names, panel = TRUE, se = FALSE)

# 5. Partial dependence for a single predictor
pv <- plot.variable(rf, xvar.names = "lstat", partial = TRUE, show.plots = FALSE)
pd <- gg_partial(pv)
plot(pd)
```

For survival forests, see the package vignette:

``` r

vignette("ggRandomForests")
```

For variable importance with varPro — partial dependence, importance
z-scores, beta importance, individual/local importance, and isolation
forests — see the dedicated vignette:

``` r

vignette("varpro", package = "ggRandomForests")
```

## Function reference

| Function | Input | What you get |
|----|----|----|
| [`gg_error()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md) | `rfsrc` / `randomForest` | OOB error vs. number of trees |
| [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md) | `rfsrc` / `randomForest` | Variable importance ranking |
| [`gg_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md) | `rfsrc` / `randomForest` | Predicted vs. observed values |
| [`gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md) | `rfsrc` / `randomForest` | Marginal dependence data frame |
| [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md) | `plot.variable` output | Partial dependence (continuous + categorical) |
| [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md) | `rfsrc` model | Partial dependence via `partial.rfsrc` |
| [`gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md) | `rfsrc` survival forest | Kaplan–Meier / Nelson–Aalen estimates |
| [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md) | `rfsrc` / `randomForest` (class) | ROC curve data |
| [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md) | `rfsrc` (survival) | Time-resolved Brier score and CRPS |

Each `gg_*` function has a matching
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) S3 method that
hands back a single plottable object — a `ggplot`, or a `patchwork`
composite when the method lays out multiple panels — so you can keep
adding layers, scales, or a theme. Every `gg_*` object also has
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods:
[`print()`](https://rdrr.io/r/base/print.html) shows a short header at
the REPL rather than dumping every row (use
[`head()`](https://rdrr.io/r/utils/head.html) when you want the rows),
and [`summary()`](https://rdrr.io/r/base/summary.html) gives you a
diagnostics object you can print or keep.

## Why ggRandomForests?

The package is built on one decision: keep the data step and the figure
step apart. The `gg_*` functions pull a tidy data object out of the
forest; the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
methods turn that object into a `ggplot2` figure. Two things follow from
that split.

First, the data object stands on its own. It carries everything its plot
needs, so you can save it, inspect it, or come back to it later without
keeping the original forest — which can be large — in memory.

Second, you are never locked into the default figure. Because a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method returns
a single plottable object (a `ggplot`, or a `patchwork` composite for
the multi-panel methods), you can add layers, swap scales, or apply a
theme; and if the default is not what you want, you can ignore it
entirely and build the figure from the tidy data yourself.

## Recent changes

See [NEWS.md](https://ehrlinger.github.io/ggRandomForests/NEWS.md) for
the full changelog. Highlights since v2.4.0:

- **v2.6.1** Fix factor-level assignment in `gg_partial` for categorical
  variables.
- **v2.6.0** New plotting functions exported; test coverage raised to
  83%; removed internal dependency on `hvtiRutilities`.
- **v2.5.0** New
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  computes partial dependence directly from an `rfsrc` model without a
  separate `plot.variable` call; supports a grouping variable via
  `xvar2.name`.

## References

Breiman, L. (2001). Random forests, *Machine Learning*, 45:5–32.

Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for
Survival, Regression and Classification. R package version \>= 3.4.0.
<https://cran.r-project.org/package=randomForestSRC>

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. *R
News* 7(2), 25–31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random
survival forests. *Ann. Appl. Statist.* 2(3), 841–860.

Liaw A. and Wiener M. (2002). Classification and Regression by
randomForest. *R News* 2(3), 18–22.

Wickham H. (2009). *ggplot2: Elegant Graphics for Data Analysis*.
Springer New York.
