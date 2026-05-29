# Tidy time-varying AUC from a Random Hazard Forest

Extracts the time-dependent AUC curve from
[`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)
into a tidy long data frame, one row per time point, with bootstrap
confidence bounds when available and the integrated AUC (iAUC) summary
attached as an attribute.

## Usage

``` r
gg_auct(object, ...)

# S3 method for class 'rhf'
gg_auct(object, marker = c("chf", "haz"), auct_fit = NULL, ...)
```

## Arguments

- object:

  A fitted `rhf` object from randomForestRHF.

- ...:

  Not currently used.

- marker:

  Risk marker for the AUC: `"chf"` (cumulative hazard, default) or
  `"haz"` (hazard). Ignored when `auct_fit` is supplied.

- auct_fit:

  Optional precomputed
  [`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)
  result (class `"auct.rhf"`) for the same `object`. `NULL` (default)
  computes it. Supply it to reuse an expensive bootstrap run.

## Value

A `data.frame` of class `c("gg_auct", "data.frame")` with columns
`time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
bootstrap), an `iauc` attribute (a list with `uno`, `std`, `uno.se`,
`std.se`, `conf.level`), and a `provenance` attribute derived from
`object` (source, family, ntree, n).

## See also

[`plot.gg_auct()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_auct.md),
[`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)

## Examples

``` r
# \donttest{
if (requireNamespace("randomForestRHF", quietly = TRUE)) {
  data(pbc, package = "randomForestSRC")
  d <- randomForestRHF::convert.counting(
    survival::Surv(days, status) ~ ., na.omit(pbc))
  o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
  plot(gg_auct(o, marker = "chf"))
}
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).

# }
```
