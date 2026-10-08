# ROC plot generic function for a [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md) object.

ROC plot generic function for a
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
object.

## Usage

``` r
# S3 method for class 'gg_roc'
plot(x, which_outcome = NULL, ..., panel = c("overlay", "facet"))
```

## Arguments

- x:

  A
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  object, or a raw
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  classification forest.

  A raw forest is accepted, but plain `plot(forest)` does not arrive
  here. Both `randomForestSRC` and `randomForest` register their own
  `plot` methods, so S3 dispatch sends a raw forest to
  [`plot.rfsrc`](https://www.randomforestsrc.org//reference/plot.rfsrc.html)
  or `plot.randomForest` instead. This branch is reached only by naming
  the method outright, as `plot.gg_roc(forest)`.

  That branch also does not use `gg_roc`'s own default for
  `which_outcome`: given a multi-class forest and `which_outcome = NULL`
  it calls
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  once per class and overlays the one-vs-rest curves, where `gg_roc(x)`
  alone returns a single curve – a macro-average for `randomForest`, or
  class 1 with a warning for `rfsrc`. Prefer
  `plot(gg_roc(x, which_outcome))`, which is explicit about both the
  class and the engine. Issue \#72 tracks reconciling the entry points.

- which_outcome:

  Integer; for multi-class problems, the index of the class to plot.
  When `NULL` (default) and the forest has more than two classes, the
  curves for all classes are overlaid in one plot. For binary forests,
  `NULL` defaults to class index 2.

- ...:

  Additional arguments passed to
  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  when `x` is a raw forest (e.g. `oob = FALSE`).

- panel:

  Character; layout for per-class ROC objects, the ones from
  `gg_roc(..., per_class = TRUE)`. `"overlay"` (default) draws every
  class curve in one panel, colored by class; `"facet"` gives each class
  its own panel. Ignored for single-class `gg_roc` objects.

## Value

A `ggplot` object. The x-axis is 1 - Specificity (FPR), the y-axis is
Sensitivity (TPR), and a dashed red diagonal marks the random-classifier
baseline. Single-class curves carry the AUC as an annotation;
multi-class plots color and style each class curve distinctly.

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
rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)

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
