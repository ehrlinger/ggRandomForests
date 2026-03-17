# Area Under the ROC Curve calculator

Area Under the ROC Curve calculator

## Usage

``` r
calc_auc(x)
```

## Arguments

- x:

  [`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  object

## Value

AUC. 50% is random guessing, higher is better.

## Details

calc_auc uses the trapezoidal rule to calculate the area under the ROC
curve.

This is a helper function for the
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
functions.

## See also

[`calc_roc`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
[`gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)

[`plot.gg_roc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)

## Examples

``` r
##
## Taken from the gg_roc example
rfsrc_iris <- rfsrc(Species ~ ., data = iris)

gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1)

calc_auc(gg_dta)
#> [1] 1

gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)

calc_auc(gg_dta)
#> [1] 1

## randomForest tests
rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
gg_dta <- gg_roc(rfsrc_iris, which_outcome = 2)

calc_auc(gg_dta)
#> [1] 1
```
