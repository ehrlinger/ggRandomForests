# Plot a [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md) object

Produces ggplot2 partial dependence curves from the named list returned
by
[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md).

## Usage

``` r
# S3 method for class 'gg_partial_rfsrc'
plot(x, ...)
```

## Arguments

- x:

  A
  [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  object.

- ...:

  Not currently used.

## Value

A single `ggplot` object, or a named list with elements `continuous` and
`categorical` when both types are present.

## Details

For standard (non-survival) forests: continuous predictors are line
plots, categorical predictors are bar charts, both faceted by variable
name.

For survival forests (when a `time` column is present): each predictor
value is a separate curve over time, faceted by variable name.

For two-variable surface plots (when a `grp` column is present): each
group level is a separate line, faceted by primary predictor name.

## See also

[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md),
[`plot.gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md)
