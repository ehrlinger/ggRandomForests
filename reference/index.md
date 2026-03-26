# Package index

## Forest Objects

Extract and display data from random forest objects.

- [`gg_rfsrc(`*`<rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md)
  : Predicted response data object

- [`plot(`*`<gg_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md)
  :

  Predicted response plot from a `gg_rfsrc` object.

## Training Error

Visualize forest convergence and training error.

- [`gg_error()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md)
  : Random forest error trajectory data object

- [`plot(`*`<gg_error>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_error.md)
  :

  Plot a `gg_error` object

## Variable Importance

Assess and plot variable importance (VIMP).

- [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  : Variable Importance (VIMP) data object

- [`plot(`*`<gg_vimp>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  :

  Plot a `gg_vimp` object, extracted variable importance of a
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object

## Variable Dependence

Marginal variable dependence plots.

- [`gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md)
  : Marginal variable dependence data object.

- [`plot(`*`<gg_variable>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md)
  :

  Plot a `gg_variable` object,

## Partial Dependence

Partial dependence plots for individual variables.

- [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)
  : Split partial dependence data into continuous or categorical
  datasets
- [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  : Partial dependence data from an rfsrc model
- [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md)
  : Split varpro partial dependence data into continuous or categorical
  datasets

## Survival Analysis

Survival curves, ROC, and related diagnostics.

- [`gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  : Nonparametric survival estimates.

- [`plot(`*`<gg_survival>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)
  :

  Plot a `gg_survival` object.

- [`gg_roc(`*`<rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  : ROC (Receiver Operating Characteristic) curve data from a
  classification forest.

- [`plot(`*`<gg_roc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)
  :

  ROC plot generic function for a `gg_roc` object.

- [`calc_roc(`*`<rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
  : Receiver Operator Characteristic calculator

- [`calc_auc()`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)
  : Area Under the ROC Curve calculator

- [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
  : Survival partial dependence data for one or more predictors

- [`kaplan()`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
  : nonparametric Kaplan-Meier estimates

- [`nelson()`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md)
  : nonparametric Nelson-Aalen estimates

## Utilities

Helper functions used internally and by users.

- [`quantile_pts()`](https://ehrlinger.github.io/ggRandomForests/reference/quantile_pts.md)
  : Quantile-based cut points for coplots
- [`varpro_feature_names()`](https://ehrlinger.github.io/ggRandomForests/reference/varpro_feature_names.md)
  : Recover original variable names from varpro one-hot encoded feature
  names

## Package

- [`ggRandomForests-package`](https://ehrlinger.github.io/ggRandomForests/reference/ggRandomForests-package.md)
  : ggRandomForests: Visually Exploring Random Forests
