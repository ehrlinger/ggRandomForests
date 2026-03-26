# Random forest error trajectory data object

Extract the cumulative out-of-bag (OOB) or in-bag training error rate
from `randomForestSRC` and `randomForest` fits as a function of the
number of grown trees.

## Usage

``` r
gg_error(object, ...)
```

## Arguments

- object:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  object.

- ...:

  Optional arguments passed to the methods. Set `training = TRUE` to
  append the in-bag error trajectory when supported.

## Value

A `gg_error` `data.frame` containing at least the cumulative OOB error
columns and an `ntree` counter. When `training = TRUE` is honored an
additional `train` column is included.

## Details

For `randomForestSRC` objects the function reshapes the
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)`$err.rate`
matrix and annotates it with the tree index required by
[`plot.gg_error`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_error.md).
When supplied a
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
object, the method inspects either the `$mse` or `$err.rate` component
and, when `training = TRUE` is requested, reconstructs the original
training set via the model call to compute an in-bag error curve using
per-tree predictions. Training curves are only available when the forest
was stored (`keep.forest = TRUE`) and the original data can be
recovered.

## References

Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for
Survival, Regression and Classification. R package version \>= 3.4.0.
<https://cran.r-project.org/package=randomForestSRC>

## See also

[`plot.gg_error`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_error.md),
[`gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md),
[`gg_variable`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md),
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html),
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html),
[`plot.rfsrc`](https://www.randomforestsrc.org//reference/plot.rfsrc.html)

## Examples

``` r
## Examples from RFSRC package...
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## ------------- iris data
## You can build a randomForest
rfsrc_iris <- rfsrc(Species ~ ., data = iris, tree.err = TRUE)

# Get a data.frame containing error rates
gg_dta <- gg_error(rfsrc_iris)

# Plot the gg_error object
plot(gg_dta)


## RandomForest example
rf_iris <- randomForest::randomForest(Species ~ .,
  data = iris,
  tree.err = TRUE,
)
gg_dta <- gg_error(rf_iris)
plot(gg_dta)


gg_dta <- gg_error(rf_iris, training = TRUE)
plot(gg_dta)

## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------

## ------------- airq data
rfsrc_airq <- rfsrc(Ozone ~ .,
  data = airquality,
  na.action = "na.impute", tree.err = TRUE,
)

# Get a data.frame containing error rates
gg_dta <- gg_error(rfsrc_airq)

# Plot the gg_error object
plot(gg_dta)
#> Ignoring unknown labels:
#> • colour : "Outcome"



## ------------- Boston data
data(Boston, package = "MASS")
Boston$chas <- as.logical(Boston$chas)
rfsrc_boston <- rfsrc(medv ~ .,
  data = Boston,
  forest = TRUE,
  importance = TRUE,
  tree.err = TRUE,
  save.memory = TRUE
)

# Get a data.frame containing error rates
gg_dta <- gg_error(rfsrc_boston)

# Plot the gg_error object
plot(gg_dta)
#> Ignoring unknown labels:
#> • colour : "Outcome"



## ------------- mtcars data
rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, tree.err = TRUE)

# Get a data.frame containing error rates
gg_dta<- gg_error(rfsrc_mtcars)

# Plot the gg_error object
plot(gg_dta)
#> Ignoring unknown labels:
#> • colour : "Outcome"



## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## ------------- veteran data
## randomized trial of two treatment regimens for lung cancer
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran,
                       tree.err = TRUE)

gg_dta <- gg_error(rfsrc_veteran)
plot(gg_dta)
#> Ignoring unknown labels:
#> • colour : "Outcome"


## ------------- pbc data
# Load a cached randomForestSRC object
# We need to create this dataset
data(pbc, package = "randomForestSRC",)
#> Warning: data set ‘’ not found
# For whatever reason, the age variable is in days... makes no sense to me
for (ind in seq_len(dim(pbc)[2])) {
 if (!is.factor(pbc[, ind])) {
   if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
     if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
       pbc[, ind] <- as.logical(pbc[, ind])
     }
   }
 } else {
   if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
     if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
       pbc[, ind] <- as.logical(pbc[, ind])
     }
     if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
       pbc[, ind] <- as.logical(pbc[, ind])
     }
   }
 }
 if (!is.logical(pbc[, ind]) &
     length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
   pbc[, ind] <- factor(pbc[, ind])
 }
}
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
 tree.err = TRUE,
 forest = TRUE,
 importance = TRUE,
 save.memory = TRUE
)


gg_dta <- gg_error(rfsrc_pbc)
plot(gg_dta)
#> Ignoring unknown labels:
#> • colour : "Outcome"

```
