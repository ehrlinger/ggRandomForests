# ROC plot generic function for a [`gg_roc`](http://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md) object.

ROC plot generic function for a
[`gg_roc`](http://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
object.

## Usage

``` r
# S3 method for class 'gg_roc'
plot(x, which_outcome = NULL, ...)
```

## Arguments

- x:

  [`gg_roc`](http://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  object created from a classification forest

- which_outcome:

  for multiclass problems, choose the class for plotting

- ...:

  arguments passed to the
  [`gg_roc`](http://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  function

## Value

`ggplot` object of the ROC curve

## References

Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival,
Regression and Classification (RF-SRC), R package version 1.4.

## See also

[`gg_roc`](http://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
rfsrc

## Examples

``` r
## ------------------------------------------------------------
## classification example
## ------------------------------------------------------------
## -------- iris data
# Build a small classification forest (ntree=50 keeps example fast)
set.seed(42)
rfsrc_iris <- rfsrc(Species ~ ., data = iris, ntree = 50)

# ROC for setosa (outcome index 1)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)
plot.gg_roc(gg_dta)


# ROC for versicolor (outcome index 2)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
plot.gg_roc(gg_dta)


# ROC for virginica (outcome index 3)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 3)
plot.gg_roc(gg_dta)


# Alternatively, pass the forest directly to plot all three ROC curves
plot.gg_roc(rfsrc_iris)

```
