# Plot Random Hazard Forest hazard / cumulative-hazard curves

Draws case-specific ensemble curves from a
[`gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf.md)
object: hazard (default) or cumulative hazard, one line per case
selected by `idx`.

## Usage

``` r
# S3 method for class 'gg_rhf'
plot(x, idx = NULL, hazard.only = TRUE, ...)
```

## Arguments

- x:

  A `gg_rhf` object from
  [`gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf.md).

- idx:

  Integer vector of case ids (matched against the `id` column) to draw.
  `NULL` (default) draws every case.

- hazard.only:

  Logical; `TRUE` (default) plots the hazard, `FALSE` plots the
  cumulative hazard.

- ...:

  Not currently used.

## Value

A `ggplot` object.

## See also

[`gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rhf.md).

## Examples

``` r
# \donttest{
if (requireNamespace("randomForestRHF", quietly = TRUE)) {
  data(pbc, package = "randomForestSRC")
  d <- randomForestRHF::convert.counting(
    survival::Surv(days, status) ~ ., na.omit(pbc))
  o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
  plot(gg_rhf(o), idx = c(1, 5, 10))
}

# }
```
