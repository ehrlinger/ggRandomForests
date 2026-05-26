# Plot a `gg_varpro` variable importance object

Draws a horizontal boxplot of the per-tree importance z-scores, or of
the raw importances if you asked for those. Set `faithful = TRUE` at
extract time and the per-tree points are scattered over the box; for a
classification forest, `conditional = TRUE` splits the plot into one
facet per class.

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

  Character; the display scale. Leave it off and it is read from
  `provenance$local.std`: `"z"` when `local.std = TRUE` (the default),
  `"raw"` when `local.std = FALSE`. Asking for a scale that the extract
  step did not prepare raises an error.

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Details

\*\*Boxplot geometry:\*\* the hinges are the 15th and 85th percentiles
of the per-tree z-distribution, and the whiskers run to the 5th and
95th. This is **not** a Tukey boxplot, and the plot carries a caption
that says so.

\*\*`faithful = TRUE`:\*\* the per-tree values are jittered over the box
as semi-transparent points, on the same scale as the box itself (z when
`local.std = TRUE`, raw when `local.std = FALSE`). The box is drawn
faint to let the points show through, and a white-outlined dot marks the
mean.

\*\*`conditional = TRUE`:\*\* the class-conditional importances
(`$conditional`) are shown as a faceted bar chart
(`facet_wrap(~class, nrow = 1)`). Variables keep the sort order set by
the unconditional median z in `$stats`, so the facets line up.

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
