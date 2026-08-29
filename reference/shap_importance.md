# SHAP global importance bar chart

Bar chart of mean absolute SHAP value per variable – the SHAP analog of
[`plot.gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md).

## Usage

``` r
shap_importance(x, labels = NULL, ...)
```

## Arguments

- x:

  A
  [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  object.

- labels:

  Optional variable labels. One of: a named character vector
  (`c(Temp = "Temperature")`); a labelled data frame, whose
  `attr(col, "label")` values are read; or a two-column `key`/`label`
  data frame. Variables with no label keep their raw name. Defaults to
  `NULL` (raw names).

- ...:

  Unused.

## Value

A `ggplot` object.

## See also

[`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
[`plot.gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_shap.md)
