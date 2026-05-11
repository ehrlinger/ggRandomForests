# Receiver Operator Characteristic calculator

Receiver Operator Characteristic calculator

## Usage

``` r
# S3 method for class 'rfsrc'
calc_roc(object, dta, which_outcome = "all", oob = TRUE, ...)
```

## Arguments

- object:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html),
  [`predict.rfsrc`](https://www.randomforestsrc.org//reference/predict.rfsrc.html),
  or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  classification object containing predicted class probabilities.

- dta:

  A factor (or coercible to factor) of the true observed class labels,
  one per observation. Typically `object$yvar` for rfsrc or `object$y`
  for randomForest.

- which_outcome:

  Integer index of the class for which the ROC curve is computed (e.g.
  `1` for the first class, `2` for the second). Use `"all"` to request
  all classes (currently falls back to class 1 with a warning).

- oob:

  Logical; if `TRUE` (default for rfsrc) use OOB predicted
  probabilities. Forced to `FALSE` for `randomForest` objects.

- ...:

  Extra arguments passed to helper functions (currently unused).

## Value

A `gg_roc` `data.frame` with columns `sens` (sensitivity), `spec`
(specificity), and `pct` (the probability threshold), with one row per
unique prediction value. Suitable for passing to
[`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)
or
[`plot.gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md).

## Details

For a randomForestSRC prediction and the actual response value,
calculate the specificity (1-False Positive Rate) and sensitivity (True
Positive Rate) of a predictor.

This is a helper function for the
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
functions, and not intended for use by the end user.

## See also

[`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)

[`plot.gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)

## Examples

``` r
## Taken from the gg_roc example
rfsrc_iris <- rfsrc(Species ~ ., data = iris)

gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
  which_outcome = 1, oob = TRUE
)
gg_dta <- calc_roc(rfsrc_iris, rfsrc_iris$yvar,
  which_outcome = 1, oob = FALSE
)

rf_iris <- randomForest(Species ~ ., data = iris)
gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
  which_outcome = 1
)
#> Warning: number of rows of result is not a multiple of vector length (arg 2)
gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
  which_outcome = 2
)
```
