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
  partial.time = NULL,
  partial.type = c("surv", "chf", "mort"),
  cat_limit = 10,
  n_eval = 25
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

- partial.time:

  Numeric vector of desired time points for survival forests (ignored
  for regression/classification). Values are automatically snapped to
  the nearest entry in `rf_model$time.interest` — see the **Survival
  forests** section below. When `NULL` (default), three quartile points
  of `time.interest` are used.

- partial.type:

  Character; type of predicted value for survival forests, passed
  through to
  [`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html).
  One of `"surv"` (default), `"chf"`, or `"mort"`. Ignored for
  non-survival forests.
  [`partial.rfsrc()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  requires a non-`NULL` value for survival families; supplying it here
  avoids a cryptic “argument is of length zero” error from the
  underlying C code.

- cat_limit:

  Variables with fewer than `cat_limit` unique values in `newx` are
  treated as categorical; all others are continuous. Defaults to 10.

- n_eval:

  Number of evaluation points for continuous variables. Instead of
  passing all observed values (which can be slow, especially for
  survival forests), continuous predictors are evaluated on a quantile
  grid of this many points. Categorical variables always use all unique
  levels. Defaults to 25.

## Value

A named list with two elements:

- continuous:

  A `data.frame` with columns `x` (numeric), `yhat`, `name` (variable
  name), and optionally `grp` (the level of `xvar2.name`) and `time`
  (survival forests only) for all continuous predictors.

- categorical:

  A `data.frame` with the same columns but `x` kept as character, for
  low-cardinality predictors.

## Survival forests and `partial.time`

[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
requires that every value in `partial.time` be an exact member of the
model's `time.interest` vector (the unique observed event times stored
in the fitted object). Passing arbitrary time values — even plausible
ones such as `c(1, 3)` for a study measured in years — causes a C-level
prediction error inside `partial.rfsrc`.

`gg_partial_rfsrc` handles this automatically: every element of
`partial.time` is silently snapped to its nearest `time.interest` value
before the call is made. To target a specific follow-up horizon, find
the closest grid point yourself and pass it explicitly:

    ti  <- rf_model$time.interest
    t1  <- ti[which.min(abs(ti - 1))]   # nearest to 1 year
    pd  <- gg_partial_rfsrc(rf_model, xvar.names = "x", partial.time = t1)

## Logical predictor columns

[`partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
does not handle `logical` predictor columns correctly in survival
forests (randomForestSRC \<= 3.5.1). If your training data contains
binary 0/1 columns, convert them to
[`factor`](https://rdrr.io/r/base/factor.html) rather than `logical`
before fitting the model.

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
