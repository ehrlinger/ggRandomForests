# Plot a [`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md) object

Produces ggplot2 partial dependence curves from the named list returned
by
[`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md),
which wraps `varpro::partialpro` output.

## Usage

``` r
# S3 method for class 'gg_partialpro'
plot(x, type = c("parametric", "nonparametric", "causal"), ...)
```

## Arguments

- x:

  A
  [`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md)
  object.

- type:

  Character vector; one or more of `"parametric"`, `"nonparametric"`,
  `"causal"`. Defaults to all three.

- ...:

  Not currently used.

## Value

A single `ggplot` or a named list with `continuous` and `categorical`
elements when both types of predictors are present.

## Details

Each variable produces up to three effect curves: parametric,
nonparametric, and causal. The `type` argument controls which are shown.

## See also

[`gg_partialpro`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partialpro.md)
