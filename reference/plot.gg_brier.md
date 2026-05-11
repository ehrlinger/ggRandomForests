# Plot a [`gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md) object

Plot the time-resolved Brier score (default) or running CRPS for a
survival forest, optionally overlaid with a 15-85 percent per-subject
envelope.

## Usage

``` r
# S3 method for class 'gg_brier'
plot(x, type = c("brier", "crps"), envelope = FALSE, ...)
```

## Arguments

- x:

  A
  [`gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  object.

- type:

  Which series to plot: `"brier"` (default) or `"crps"`.

- envelope:

  Logical. When `TRUE`, overlays a ribbon spanning the 15th-85th
  percentile of per-subject Brier (or running CRPS) contributions at
  each time, around the overall line. When `FALSE` (default), draws the
  overall series only.

- ...:

  Extra arguments forwarded to
  [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Value

A `ggplot` object.

## See also

[`gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md),
[`get.brier.survival`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data(pbc, package = "randomForestSRC")
rf <- randomForestSRC::rfsrc(Surv(days, status) ~ ., data = pbc,
                             nsplit = 10)
gg_dta <- gg_brier(rf)
plot(gg_dta)
plot(gg_dta, type = "crps")
plot(gg_dta, envelope = TRUE)   # adds 15-85% envelope
} # }
```
