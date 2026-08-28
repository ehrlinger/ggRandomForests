# Plot a Random Hazard Forest tuning path

Draws the saved evaluated metric at each tree size, highlighting the
upstream selected size. OOB risk paths show the criterion minimized by
upstream tuning; OOB iAUC paths show the criterion it maximizes. An iAUC
path includes a standard-error band only when finite supplied standard
errors are available.

## Usage

``` r
# S3 method for class 'gg_tune_rhf'
plot(x, se_band = TRUE, se_mult = 1, ...)
```

## Arguments

- x:

  A `gg_tune_rhf` object from
  [`gg_tune_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_tune_rhf.md).

- se_band:

  Logical; draw an iAUC standard-error band when available.

- se_mult:

  Positive finite multiplier for the standard-error band.

- ...:

  Additional arguments passed to the evaluated-point layer.

## Value

A `ggplot` object.

## See also

[`gg_tune_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_tune_rhf.md).
