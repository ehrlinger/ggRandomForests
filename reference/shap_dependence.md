# SHAP dependence plot

SHAP value against the value of a single feature — the SHAP analog of a
partial-dependence plot. Numeric features use a continuous x-axis;
factor or character features fall back to their labels on a discrete
axis.

## Usage

``` r
shap_dependence(x, xvar = NULL, labels = NULL, ...)
```

## Arguments

- x:

  A
  [`gg_shap`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  object.

- xvar:

  The variable to plot. When `NULL`, the top-ranked variable (largest
  mean absolute SHAP) is used.

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
