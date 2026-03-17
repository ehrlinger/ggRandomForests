# ROC (Receiver operator curve) data from a classification random forest.

The sensitivity and specificity of a randomForest classification object.

## Usage

``` r
# S3 method for class 'rfsrc'
gg_roc(object, which_outcome, oob, ...)
```

## Arguments

- object:

  an [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  classification object

- which_outcome:

  select the classification outcome of interest.

- oob:

  use oob estimates (default TRUE)

- ...:

  extra arguments (not used)

## Value

`gg_roc` `data.frame` for plotting ROC curves.

## See also

[`plot.gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
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
