# Partial dependence data from an rfsrc model

Computes partial dependence for one or more predictors by calling
[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
internally, then splits the results into separate data frames for
continuous and categorical variables. Unlike
[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
no separate `plot.variable` call is required — supply the fitted `rfsrc`
object directly.

## Usage

``` r
gg_partial_rfsrc(
  rf_model,
  xvar.names = NULL,
  xvar2.name = NULL,
  newx = NULL,
  cat_limit = 10
)
```

## Arguments

- rf_model:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object.

- xvar.names:

  Character vector of predictor names for which partial dependence
  should be computed. Must be a subset of `rf_model$xvar.names`.

- xvar2.name:

  Optional single character name of a grouping variable in `newx`. When
  supplied, partial dependence is computed separately for each unique
  level of this variable and a `grp` column is appended.

- newx:

  Optional `data.frame` of predictor values to evaluate partial effects
  at. Defaults to the training data stored in `rf_model$xvar`. All
  column names must match `rf_model$xvar.names`.

- cat_limit:

  Variables with fewer than `cat_limit` unique values in `newx` are
  treated as categorical; all others are continuous. Defaults to 10.

## Value

A named list with two elements:

- continuous:

  A `data.frame` with columns `x` (numeric), `yhat`, `name` (variable
  name), and optionally `grp` (the level of `xvar2.name`) and `time`
  (survival forests only) for all continuous predictors.

- categorical:

  A `data.frame` with the same columns but `x` kept as character, for
  low-cardinality predictors.

## See also

[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
[`get.partial.plot.data`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)

## Examples

``` r
## ------------------------------------------------------------
##
## regression
##
## ------------------------------------------------------------

airq.obj <- rfsrc(Ozone ~ ., data = airquality)

## partial effect for wind
prt_dta <- gg_partial_rfsrc(airq.obj,
                       xvar.names = c("Wind"))
```
