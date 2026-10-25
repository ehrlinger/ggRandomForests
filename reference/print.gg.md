# Print methods for gg\_\* data objects

Each `print.gg_*()` method prints a one-line header: the class label
and, where the forest recorded it, provenance (source package, family,
ntree, n). It returns the object invisibly, so
[`print()`](https://rdrr.io/r/base/print.html) sits cleanly in a pipe.

## Usage

``` r
# S3 method for class 'gg_beta_uvarpro'
print(x, ...)

# S3 method for class 'gg_sdependent'
print(x, ...)

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

# S3 method for class 'gg_partial_varpro'
print(x, ...)

# S3 method for class 'gg_roc'
print(x, ...)

# S3 method for class 'gg_survival'
print(x, ...)

# S3 method for class 'gg_brier'
print(x, ...)

# S3 method for class 'gg_udependent'
print(x, ...)

# S3 method for class 'summary.gg_udependent'
print(x, ...)

# S3 method for class 'gg_varpro'
print(x, ...)

# S3 method for class 'gg_isopro'
print(x, ...)

# S3 method for class 'gg_beta_varpro'
print(x, ...)

# S3 method for class 'gg_ivarpro'
print(x, ...)

# S3 method for class 'gg_shap'
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

To see the rows themselves, use
[`head()`](https://rdrr.io/r/utils/head.html); for per-class
diagnostics, use
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
