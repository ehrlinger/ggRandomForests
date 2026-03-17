# Variable Importance (VIMP) data object

`gg_vimp` Extracts the variable importance (VIMP) information from a
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
object and reshapes it into a tidy data set.

## Usage

``` r
gg_vimp(object, nvar, ...)
```

## Arguments

- object:

  A [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object, the output from
  [`vimp`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html),
  or a fitted
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html).

- nvar:

  argument to control the number of variables included in the output.

- ...:

  arguments passed to the
  [`vimp.rfsrc`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html)
  function if the
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object does not contain importance information.

## Value

`gg_vimp` object. A `data.frame` of VIMP measures, in rank order,
optionally containing class-specific scores and a relative importance
column. When `randomForest` objects lack stored importance values a
warning is issued and `NA` placeholders are returned so plots remain
reproducible.

## References

Ishwaran H. (2007). Variable importance in binary regression trees and
forests, *Electronic J. Statist.*, 1:519-537.

## See also

[`plot.gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)

[`vimp`](https://www.randomforestsrc.org//reference/vimp.rfsrc.html)

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ .,
  data = iris,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_iris)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 16 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## ------------------------------------------------------------
## regression example
## ------------------------------------------------------------

## -------- air quality data
rfsrc_airq <- rfsrc(Ozone ~ ., airquality,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_airq)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 5 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



## -------- Boston data
data(Boston, package = "MASS")
rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_boston)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 13 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## -------- Boston data
rf_boston <- randomForest::randomForest(medv ~ ., Boston)
gg_dta <- gg_vimp(rf_boston)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 13 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



## -------- mtcars data
rfsrc_mtcars <- rfsrc(mpg ~ .,
  data = mtcars,
  importance = TRUE
)
gg_dta <- gg_vimp(rfsrc_mtcars)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 10 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## ------------------------------------------------------------
## survival example
## ------------------------------------------------------------

## -------- veteran data
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- rfsrc(Surv(time, status) ~ .,
  data = veteran,
  ntree = 100,
  importance = TRUE
)

gg_dta <- gg_vimp(rfsrc_veteran)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 6 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## -------- pbc data
# We need to create this dataset
data(pbc, package = "randomForestSRC", )
#> Warning: data set ‘’ not found
# For whatever reason, the age variable is in days...
# makes no sense to me
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
# Convert age to years
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

# ========
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

gg_dta <- gg_vimp(rfsrc_pbc)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 17 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


# Restrict to only the top 10.
gg_dta <- gg_vimp(rfsrc_pbc, nvar = 10)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 10 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.

```
