# ROC plot generic function for a [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md) object.

ROC plot generic function for a
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
object.

## Usage

``` r
# S3 method for class 'gg_roc'
plot(x, which_outcome = NULL, ...)
```

## Arguments

- x:

  A
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  object, or a raw
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  classification forest or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  classification object. When a forest is supplied,
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  is called automatically.

- which_outcome:

  Integer; for multi-class problems, the index of the class whose ROC
  curve should be plotted. When `NULL` (default) and the forest has more
  than two classes, ROC curves for all classes are overlaid in a single
  plot. For binary forests `NULL` defaults to class index 2.

- ...:

  Additional arguments forwarded to
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  when `x` is a raw forest object (e.g. `oob = FALSE`).

## Value

A `ggplot` object. The x-axis shows 1 - Specificity (FPR) and the y-axis
shows Sensitivity (TPR). A dashed red diagonal reference line marks the
random-classifier baseline. The AUC value is annotated on the plot for
single-class curves. Multi-class plots colour and style each class curve
distinctly.

## References

Breiman L. (2001). Random forests, Machine Learning, 45:5-32.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R,
Rnews, 7(2):25-31.

Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for
Survival, Regression and Classification. R package version \>= 3.4.0.
<https://cran.r-project.org/package=randomForestSRC>

## See also

[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
[`calc_roc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
[`calc_auc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_auc.md)
[`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
[`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)

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
plot(gg_dta)


# ROC for versicolor (outcome index 2)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)
plot(gg_dta)


# ROC for virginica (outcome index 3)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 3)
plot(gg_dta)


# Plot all three ROC curves in one call by iterating over outcome indices
n_cls <- ncol(rfsrc_iris$predicted)
for (i in seq_len(n_cls)) print(plot(gg_roc(rfsrc_iris, which_outcome = i)))



```
