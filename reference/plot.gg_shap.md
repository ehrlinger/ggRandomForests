# Plot a [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md) object

Routes to one of three SHAP views. `type = "beeswarm"` (default) draws
the signature SHAP summary; `"importance"` draws a mean-absolute-SHAP
bar chart; `"dependence"` draws SHAP value against a single feature's
value.

## Usage

``` r
# S3 method for class 'gg_shap'
plot(x, type = c("beeswarm", "importance", "dependence"), xvar = NULL, ...)
```

## Arguments

- x:

  A
  [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  object.

- type:

  One of `"beeswarm"`, `"importance"`, or `"dependence"`.

- xvar:

  For `type = "dependence"`, the variable to plot. When `NULL`, the
  top-ranked variable is used.

- ...:

  Passed to the underlying builder.

## Value

A `ggplot` object.

## See also

[`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
[`shap_importance`](https://ehrlinger.github.io/ggRandomForests/reference/shap_importance.md)

## Examples

``` r
# \donttest{
if (requireNamespace("kernelshap", quietly = TRUE)) {
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  gg_dta <- gg_shap(rf, bg_n = 20)
  plot(gg_dta, type = "importance")
}

# }
```
