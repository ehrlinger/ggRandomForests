# Receiver Operator Characteristic calculator

Receiver Operator Characteristic calculator

## Usage

``` r
# S3 method for class 'rfsrc'
calc_roc(object, dta, which_outcome = "all", oob = TRUE, ...)
```

## Arguments

- object:

  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`predict.rfsrc`](https://www.randomforestsrc.org//reference/predict.rfsrc.html)
  object containing predicted response

- dta:

  True response variable

- which_outcome:

  If defined, only show ROC for this response.

- oob:

  Use OOB estimates, the normal validation method (TRUE)

- ...:

  extra arguments passed to helper functions

## Value

A `gg_roc` object

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
gg_dta <- calc_roc(rf_iris, rf_iris$yvar,
  which_outcome = 2
)
```
