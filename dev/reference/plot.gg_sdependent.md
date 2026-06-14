# Plot a `gg_sdependent` object

Ranked lollipop of the per-variable `sdependent()` signal score, sorted
descending so the strongest signal lands at the top. Points are coloured
blue for variables flagged as signal (`signal == TRUE`) and grey
otherwise.

## Usage

``` r
# S3 method for class 'gg_sdependent'
plot(x, ...)
```

## Arguments

- x:

  A `gg_sdependent` object from
  [`gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md).

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Reading the chart

Each lollipop is a variable's signal score from
[`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html),
computed on the unsupervised entropy-region lasso structure of a
[`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html)
fit. Blue variables cleared the detection threshold and are reported in
`sdependent()$signal.vars`; grey ones did not. Pair with
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
(the dependency graph) to see *how* the signal variables connect, and
with
[`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md)
for the lasso-importance ranking.

## See also

[`gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md),
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md),
[`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 50)
  plot(gg_sdependent(o))
}

# }
```
