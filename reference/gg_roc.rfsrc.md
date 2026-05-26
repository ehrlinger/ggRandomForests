# ROC (Receiver Operating Characteristic) curve data from a classification forest.

A classifier does not hand you a class; it hands you a predicted
probability, and you pick a threshold. Slide that threshold from 0 to 1
and the trade-off between catching the positives and crying wolf shifts
the whole way. The ROC curve traces that trade-off. For one class of a
classification
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
forest, `gg_roc` walks every threshold and records sensitivity (the true
positive rate) against specificity (1 minus the false positive rate).

## Usage

``` r
# S3 method for class 'rfsrc'
gg_roc(object, which_outcome, oob = TRUE, per_class = FALSE, ...)
```

## Arguments

- object:

  A classification
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  object. Only forests with `family == "class"` (rfsrc) or
  `type == "classification"` (randomForest) are supported.

- which_outcome:

  Integer index or character name of the class to score. For binary
  forests this is usually `1` or `2`; for multi-class forests, any valid
  class index or level name. `which_outcome = "all"` or `0` behaves
  differently by engine:

  `randomForest` method

  :   Returns a macro-averaged one-vs-rest ROC computed over the
      per-class probabilities.

  `rfsrc` method

  :   Warns and falls back to class 1. The macro-average and per-class
      faceting for the `rfsrc` path are tracked separately under issue
      \#72.

- oob:

  Logical; if `TRUE` (default), build the curve from out-of-bag
  predicted probabilities, otherwise from full in-bag predictions. For
  `randomForest`, `TRUE` uses the out-of-bag vote probabilities in
  `object$votes`; `FALSE` uses in-bag `predict(type = "prob")`.

- per_class:

  Logical; if `TRUE` and the forest has more than two classes, return
  one ROC curve per class, each class scored against all the others. The
  result is a long-format `data.frame` with a `class` factor column and
  a named AUC vector attribute, ordered by descending AUC. Binary
  forests treat `per_class = TRUE` as a no-op. Honoured by the
  `randomForest` method only.

- ...:

  Extra arguments (currently unused).

## Value

A `gg_roc` `data.frame`, one row per unique prediction threshold, with
columns:

- sens:

  Sensitivity (true positive rate) at the threshold.

- spec:

  Specificity (true negative rate) at the threshold.

- yvar:

  The observed class label for each observation.

Pass it to
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
rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris)

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
