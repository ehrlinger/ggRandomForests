# Predicted response data object

Extracts the predicted response values from the
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) object,
and formats data for plotting the response using
[`plot.gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md).

## Usage

``` r
# S3 method for class 'rfsrc'
gg_rfsrc(object, oob = TRUE, by, ...)
```

## Arguments

- object:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  object.

- oob:

  Logical; if `TRUE` (default) return out-of-bag predictions. Set to
  `FALSE` to use full in-bag (training) predictions. Forced to `FALSE`
  automatically for `predict.rfsrc` objects, which carry no OOB
  estimates.

- by:

  Optional stratifying variable. Either a character column name present
  in the training data, or a vector/factor of the same length as the
  training set. When supplied, a `group` column is added to the returned
  data and bootstrap CI bands (survival) are computed per group. Omit or
  leave missing to return an unstratified result.

- ...:

  Additional arguments controlling output for specific forest families:

  surv_type

  :   Character; one of `"surv"` (default), `"chf"`, or `"mortality"`
      for survival forests.

  conf.int

  :   Numeric coverage probability (e.g. `0.95`) to request bootstrap
      pointwise confidence bands for survival forests. Triggers
      wide-format output with `lower`, `upper`, `median`, and `mean`
      columns.

  bs.sample

  :   Integer; number of bootstrap resamples when `conf.int` is set.
      Defaults to the number of observations.

## Value

A `gg_rfsrc` object (a classed `data.frame`) whose structure depends on
the forest family:

- regression:

  Columns `yhat` and the response name; optionally a `group` column when
  `by` is supplied.

- classification:

  One column per class with predicted probabilities; a `y` column with
  observed class labels; optionally `group`.

- survival (no CI / grouping):

  Long-format with columns `variable` (event time), `value` (survival
  probability), `obs_id`, and `event`.

- survival (with `conf.int` or `by`):

  Wide-format with pointwise bootstrap CI columns (`lower`, `upper`,
  `median`, `mean`) per time point; a `group` column when `by` is
  supplied.

The object carries class attributes for the forest family so that
[`plot.gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md)
dispatches correctly.

## Details

For survival forests, use the `surv_type` argument (`"surv"`, `"chf"`,
or `"mortality"`) to select the predicted quantity. Bootstrap confidence
bands are requested by passing `conf.int` (e.g. `conf.int = 0.95`); the
number of resamples is controlled by `bs.sample`.

## See also

[`plot.gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md),
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html),
[`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)

## Examples

``` r
## ------------------------------------------------------------
## classification example (small, runs on CRAN)
## ------------------------------------------------------------
## -------- iris data
set.seed(42)
rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)
gg_dta <- gg_rfsrc(rfsrc_iris)
plot(gg_dta)


# \donttest{
## ------------------------------------------------------------
## Additional regression / survival examples are guarded with
## \donttest because the cumulative example time exceeds the
## 10-second CRAN budget. Run locally with `R CMD check --run-donttest`
## (or `devtools::check(run_dont_test = TRUE)`) to exercise them.
## ------------------------------------------------------------

## -------- air quality data (regression)
rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality,
                    na.action = "na.impute", ntree = 50)
plot(gg_rfsrc(rfsrc_airq))


## -------- Boston data (rfsrc + randomForest)
if (requireNamespace("MASS", quietly = TRUE)) {
  data(Boston, package = "MASS")
  Boston$chas <- as.logical(Boston$chas)
  rfsrc_boston <- rfsrc(medv ~ ., data = Boston, ntree = 50,
                        forest = TRUE, importance = TRUE,
                        tree.err = TRUE, save.memory = TRUE)
  plot(gg_rfsrc(rfsrc_boston))

  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston,
                                          ntree = 50)
  plot(gg_rfsrc(rf_boston))
}


## -------- mtcars data
rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ntree = 50)
plot(gg_rfsrc(rfsrc_mtcars))


## -------- veteran data (survival; with CI and group-by)
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran,
                       ntree = 50)
plot(gg_rfsrc(rfsrc_veteran))

plot(gg_rfsrc(rfsrc_veteran, conf.int = .95))

plot(gg_rfsrc(rfsrc_veteran, by = "trt"))

# }
```
