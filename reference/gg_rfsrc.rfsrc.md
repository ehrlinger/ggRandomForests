# Predicted response data object

Extracts the predicted response values from the
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) object,
and formats data for plotting the response using
[`plot.gg_rfsrc`](http://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md).

## Usage

``` r
# S3 method for class 'rfsrc'
gg_rfsrc(object, oob = TRUE, by, ...)
```

## Arguments

- object:

  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object

- oob:

  boolean, should we return the oob prediction , or the full forest
  prediction.

- by:

  stratifying variable in the training dataset, defaults to NULL

- ...:

  extra arguments

## Value

`gg_rfsrc` object

## Details

`surv_type` ("surv", "chf", "mortality", "hazard") for survival forests

`oob` boolean, should we return the oob prediction , or the full forest
prediction.

## See also

[`plot.gg_rfsrc`](http://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rfsrc.md)
`rfsrc` `plot.rfsrc`
[`gg_survival`](http://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
rfsrc_iris <- rfsrc(Species ~ ., data = iris)
gg_dta<- gg_rfsrc(rfsrc_iris)

plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 450 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## ------------------------------------------------------------
## Regression example
## ------------------------------------------------------------

## -------- air quality data
rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
gg_dta<- gg_rfsrc(rfsrc_airq)

plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 153 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 153 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



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
#> Warning: All aesthetics have length 1, but the data has 506 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 506 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


### randomForest example
data(Boston, package="MASS")
rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
plot(gg_rfsrc(rf_boston))
#> Warning: All aesthetics have length 1, but the data has 506 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 506 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



## -------- mtcars data
rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
gg_dta<- gg_rfsrc(rfsrc_mtcars)

plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 32 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 32 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


## ------------------------------------------------------------
## Survival example
## ------------------------------------------------------------
## -------- veteran data
## randomized trial of two treatment regimens for lung cancer
data(veteran, package = "randomForestSRC")
rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)

gg_dta <- gg_rfsrc(rfsrc_veteran)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 13289 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 97 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 97 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 194 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 194 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



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
#> Warning: All aesthetics have length 1, but the data has 38064 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 122 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 122 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.



gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
plot(gg_dta)
#> Warning: All aesthetics have length 1, but the data has 244 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 244 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.


```
