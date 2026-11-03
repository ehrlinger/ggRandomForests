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

## Details

Cells the forest left undefined are dropped before drawing. From
randomForestRHF 2.0.0 the hazard is `NA` outside each case's observed
`(start, stop]` path, so a hazard curve ends with that case's follow-up,
and a case whose hazard is masked throughout is left out of the legend
rather than shown as an empty one. From 2.0.3 the cumulative hazard is
masked too, though on a different rule: it is `NA` after each case's
final stop. It is documented upstream as flat through an internal gap,
but randomForestRHF 2.1.0 returns `NA` from a case's first gap onward,
so on 2.1.0 the `hazard.only = FALSE` curve of such a case ends where
the gap begins. Either curve breaks at a masked run instead of being
joined across it, so a gap shows as a gap.

Each case gets its own colour and legend key only while there are at
most 10 of them. Beyond that the colours stop being distinguishable and
the legend would list every id, so the curves are drawn in a single
translucent colour with no legend. To pick out individual cases, pass a
short `idx`.

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
