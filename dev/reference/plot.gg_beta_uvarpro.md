# Plot a `gg_beta_uvarpro` object

Horizontal bar chart of the mean absolute lasso coefficient
\\\mathrm{mean}(\|\hat{\beta}\|)\\ per variable from an unsupervised
varPro fit, sorted descending so the eye lands on the top variable
first. Bars are filled blue above the selection cutoff, grey otherwise,
with a dashed red line at the cutoff.

## Usage

``` r
# S3 method for class 'gg_beta_uvarpro'
plot(x, ...)
```

## Arguments

- x:

  A `gg_beta_uvarpro` object from
  [`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md).

- ...:

  Not currently used.

## Value

A `ggplot` object.

## Reading the chart

Each bar is the average magnitude of a per-region lasso coefficient for
that variable, computed by
[`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
over the unsupervised entropy regions of a
[`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html)
fit. There is no response: the score measures how strongly a variable is
reconstructed by the others within released regions, i.e. an
unsupervised importance / redundancy signal rather than a predictive
one. As with
[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md),
the numeric scale carries the predictors' units, so bar lengths are
comparable within a data set but not blindly across variables on very
different scales.

## See also

[`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md),
[`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md),
[`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md).

## Examples

``` r
# \donttest{
if (requireNamespace("varPro", quietly = TRUE)) {
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 50)
  plot(gg_beta_uvarpro(o))
}

# }
```
