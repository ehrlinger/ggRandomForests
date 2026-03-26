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
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ ., data = iris)
gg_dta<- gg_rfsrc(rfsrc_iris)

plot(gg_dta)


## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------

## -------- air quality data
rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
gg_dta<- gg_rfsrc(rfsrc_airq)

plot(gg_dta)



## -------- Boston data
data(Boston, package = "MASS")
Boston$chas <- as.logical(Boston$chas)
rfsrc_boston <- rfsrc(medv ~ .,
   data = Boston,
   forest = TRUE,
   importance = TRUE,
   tree.err = TRUE,
   save.memory = TRUE)

plot(gg_rfsrc(rfsrc_boston))


### randomForest example
data(Boston, package="MASS")
rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
plot(gg_rfsrc(rf_boston))



## -------- mtcars data
rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
gg_dta<- gg_rfsrc(rfsrc_mtcars)

plot(gg_dta)


## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## -------- veteran data
## randomized trial of two treatment regimens for lung cancer
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)

gg_dta <- gg_rfsrc(rfsrc_veteran)
plot(gg_dta)


gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
plot(gg_dta)


gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
plot(gg_dta)



## -------- pbc data
## We don't run this because of bootstrap confidence limits
# We need to create this dataset
data(pbc, package = "randomForestSRC",)
#> Warning: data set ‘’ not found
# For whatever reason, the age variable is in days... makes no sense to me
#Convert age to years
pbc$age <- pbc$age / 364.24

pbc$years <- pbc$days / 364.24
pbc <- pbc[, -which(colnames(pbc) == "days")]
pbc$treatment <- as.numeric(pbc$treatment)
pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
pbc$treatment <- factor(pbc$treatment)
dta_train <- pbc[-which(is.na(pbc$treatment)), ]
# Create a test set from the remaining patients
 pbc_test <- pbc[which(is.na(pbc$treatment)), ]

#========
# build the forest:
rfsrc_pbc <- randomForestSRC::rfsrc(
  Surv(years, status) ~ .,
 dta_train,
 nsplit = 10,
 na.action = "na.impute",
 forest = TRUE,
 importance = TRUE,
 save.memory = TRUE
)
gg_dta <- gg_rfsrc(rfsrc_pbc)
plot(gg_dta)


gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
plot(gg_dta)



gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
plot(gg_dta)


```
