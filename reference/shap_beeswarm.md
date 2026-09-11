# SHAP beeswarm summary plot

The signature SHAP summary: one jittered point per (observation,
variable), positioned by SHAP value and colored by the feature value,
min-max scaled to `[0, 1]` within each variable so every variable's own
range maps to the full color gradient. Categorical features have no
numeric value and render as a neutral gray (no numeric value to scale).

## Usage

``` r
shap_beeswarm(x, labels = NULL, ...)
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
