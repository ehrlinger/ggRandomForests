# Plot a `gg_varpro` variable importance object

Renders a horizontal boxplot of per-tree importance z-scores (or raw
importances) with optional per-tree jitter overlay (`faithful=TRUE`) or
class-conditional facets (`conditional=TRUE`).

## Usage

``` r
# S3 method for class 'gg_varpro'
plot(x, type, ...)
```

## Arguments

- x:

  A `gg_varpro` object from
  [`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md).

- type:

  Character; controls the display scale. When omitted, auto-detected
  from `provenance$local.std`: `"z"` if `local.std = TRUE` (the
  default), `"raw"` if `local.std = FALSE`. Explicitly supplying a value
  that conflicts with the extract-time setting raises an error.

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Details

\*\*Honest boxplot geometry:\*\* Hinges are the 15th and 85th
percentiles of the per-tree z-distribution; whiskers extend to the 5th
and 95th percentiles. This is **not** a Tukey boxplot. A mandatory plot
caption states this explicitly.

\*\*`faithful = TRUE`:\*\* Per-tree values are overlaid as jittered
semi-transparent points on the same scale as the boxplot (z-scale when
`local.std = TRUE`, raw-scale when `local.std = FALSE`); the box is
drawn at reduced opacity; a white-outlined filled dot marks the mean.

\*\*`conditional = TRUE`:\*\* The conditional class-importance scores
(`$conditional`) are shown as a faceted bar chart
(`facet_wrap(~class, nrow = 1)`); variable sort order follows the
unconditional median z from `$stats`.

## See also

[`gg_varpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)

## Examples

``` r
# \donttest{
set.seed(42)
vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
plot(gg_varpro(vp))

plot(gg_varpro(vp, faithful = TRUE))

# }
```
