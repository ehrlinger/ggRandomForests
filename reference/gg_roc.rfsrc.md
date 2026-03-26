# ROC (Receiver Operating Characteristic) curve data from a classification forest.

Computes sensitivity (true positive rate) and specificity (1 - false
positive rate) across all prediction thresholds for one class of a
classification
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
object.

## Usage

``` r
# S3 method for class 'rfsrc'
gg_roc(object, which_outcome, oob = TRUE, ...)
```

## Arguments

- object:

  A classification
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  object. Only forests with `family == "class"` (rfsrc) or
  `type == "classification"` (randomForest) are supported.

- which_outcome:

  Integer index or character name of the class for which the ROC curve
  is computed. For binary forests this is typically `1` or `2`; for
  multi-class forests any valid class index. Use `which_outcome = 0` to
  obtain the overall (averaged) ROC.

- oob:

  Logical; if `TRUE` (default) use out-of-bag predicted probabilities
  for the curve. Set to `FALSE` to use full in-bag predictions.

- ...:

  Extra arguments (currently unused).

## Value

A `gg_roc` `data.frame` with one row per unique prediction threshold and
columns:

- sens:

  Sensitivity (true positive rate) at each threshold.

- spec:

  Specificity (true negative rate) at each threshold.

- yvar:

  The observed class label for each observation.

Pass to
[`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)
for the area under the curve.

## See also

[`plot.gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md),
[`calc_roc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md),
[`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md),
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html),
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ ., data = iris)

# ROC for setosa
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)
plot(gg_dta)


# ROC for versicolor
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
plot(gg_dta)


# ROC for virginica
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 3)
plot(gg_dta)


## -------- iris data
rf_iris <- randomForest::randomForest(Species ~ ., data = iris)

# ROC for setosa
gg_dta <- gg_roc(rf_iris, which_outcome = 1)
plot(gg_dta)


# ROC for versicolor
gg_dta <- gg_roc(rf_iris, which_outcome = 2)
plot(gg_dta)


# ROC for virginica
gg_dta <- gg_roc(rf_iris, which_outcome = 3)
plot(gg_dta)

```
