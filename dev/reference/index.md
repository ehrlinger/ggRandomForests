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

- [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
  : Variable importance data from a varPro model

- [`plot(`*`<gg_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_varpro.md)
  :

  Plot a `gg_varpro` variable importance object

- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
  : Per-variable lasso-beta importance from a varPro fit

- [`plot(`*`<gg_beta_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_beta_varpro.md)
  :

  Plot a `gg_beta_varpro` object

- [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
  : Variable dependency graph from a uvarpro model

- [`plot(`*`<gg_udependent>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_udependent.md)
  :

  Plot a `gg_udependent` variable dependency graph

- [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md)
  : Individual (local) variable importance from a varPro fit

- [`plot(`*`<gg_ivarpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_ivarpro.md)
  :

  Plot a `gg_ivarpro` object

## Anomaly Detection

Score and plot per-observation anomaly scores.

- [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  : Tidy data from a varPro isolation-forest fit
- [`plot(`*`<gg_isopro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_isopro.md)
  : Plot a varPro isolation-forest anomaly score

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

- [`plot(`*`<gg_partial>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md)
  :

  Plot a `gg_partial` object

- [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  : Partial dependence data from an rfsrc model

- [`plot(`*`<gg_partial_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  :

  Plot a `gg_partial_rfsrc` object

- [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  : Partial dependence data from a varPro model

- [`plot(`*`<gg_partialpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  [`plot(`*`<gg_partial_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  :

  Plot a `gg_partial_varpro` object

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

- [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  : Brier score and CRPS for survival forests

- [`plot(`*`<gg_brier>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md)
  :

  Plot a `gg_brier` object

- [`gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf.md)
  : Tidy hazard and cumulative-hazard curves from a Random Hazard Forest

- [`plot(`*`<gg_rhf>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rhf.md)
  : Plot Random Hazard Forest hazard / cumulative-hazard curves

## S3 Methods

Standard R generics implemented for all gg\_\* data objects.

- [`autoplot(`*`<gg_error>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_vimp>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_variable>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_partial>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_partial_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_partialpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_partial_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_roc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_survival>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_brier>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_rhf>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_udependent>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  [`autoplot(`*`<gg_isopro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)
  :

  `autoplot` methods for ggRandomForests data objects

- [`print(`*`<gg_error>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_vimp>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_variable>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_partial>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_partial_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_partialpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_partial_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_roc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_survival>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_brier>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_rhf>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_udependent>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<summary.gg_udependent>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_isopro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_beta_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  [`print(`*`<gg_ivarpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  : Print methods for gg\_\* data objects

- [`print(`*`<summary.gg>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_error>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_vimp>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_variable>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_partial>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_partial_rfsrc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_partialpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_partial_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_roc>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_survival>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_udependent>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_brier>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_rhf>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_isopro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_beta_varpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  [`summary(`*`<gg_ivarpro>`*`)`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
  : Summary methods for gg\_\* data objects

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
