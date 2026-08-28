# Plot a time-varying AUC curve

Draws AUC(t) from a
[`gg_auct()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_auct.md)
object: a line over time, a bootstrap confidence ribbon when available,
and a dashed reference line at 0.5 (chance). The integrated AUC (iAUC)
appears in the caption. Times with a non-finite AUC are dropped before
drawing: from randomForestRHF 2.0.0 the final grid time can be `NA`,
where the censoring-weight denominator is undefined once the control set
is nearly exhausted.

## Usage

``` r
# S3 method for class 'gg_auct'
plot(x, ...)
```

## Arguments

- x:

  A `gg_auct` object from
  [`gg_auct()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_auct.md).

- ...:

  Not currently used.

## Value

A `ggplot` object.

## See also

[`gg_auct()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_auct.md).

## Examples

``` r
# \donttest{
if (requireNamespace("randomForestRHF", quietly = TRUE)) {
  data(pbc, package = "randomForestSRC")
  d <- randomForestRHF::convert.counting(
    survival::Surv(days, status) ~ ., na.omit(pbc))
  o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
  plot(gg_auct(o))
}

# }
```
