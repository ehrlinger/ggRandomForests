# SHAP global importance bar chart

Bar chart of mean absolute SHAP value per variable – the SHAP analog of
[`plot.gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md).

## Usage

``` r
shap_importance(x, ...)
```

## Arguments

- x:

  A
  [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  object.

- ...:

  Unused.

## Value

A `ggplot` object.

## See also

[`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
[`plot.gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_shap.md)
