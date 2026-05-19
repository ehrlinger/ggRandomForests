# Print methods for gg\_\* data objects

Each `print.gg_*()` method emits a single-line header containing the
class label and, when available, forest provenance metadata (source
package, family, ntree, n). The object is returned invisibly so
[`print()`](https://rdrr.io/r/base/print.html) calls chain cleanly in
pipes.

## Usage

``` r
# S3 method for class 'gg_error'
print(x, ...)

# S3 method for class 'gg_vimp'
print(x, ...)

# S3 method for class 'gg_rfsrc'
print(x, ...)

# S3 method for class 'gg_variable'
print(x, ...)

# S3 method for class 'gg_partial'
print(x, ...)

# S3 method for class 'gg_partial_rfsrc'
print(x, ...)

# S3 method for class 'gg_partialpro'
print(x, ...)

# S3 method for class 'gg_roc'
print(x, ...)

# S3 method for class 'gg_survival'
print(x, ...)

# S3 method for class 'gg_brier'
print(x, ...)
```

## Arguments

- x:

  A `gg_*` data object.

- ...:

  Not currently used.

## Value

The object `x`, invisibly.

## Details

To inspect rows use [`head()`](https://rdrr.io/r/utils/head.html). To
retrieve per-class diagnostics use
[`summary.gg`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md).

## See also

[`summary.gg`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md),
[`autoplot.gg`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)

## Examples

``` r
set.seed(42)
airq <- na.omit(airquality)
rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
print(gg_error(rf))
#> <gg_error>  from randomForestSRC  |  family: regr  |  ntree: 50  |  n: 111
print(gg_vimp(rf))
#> Warning: rfsrc object does not contain VIMP information. Calculating...
#> <gg_vimp>  from randomForestSRC  |  family: regr  |  ntree: 50  |  n: 111  |  variables: 5
```
