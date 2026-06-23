# Summary methods for gg\_\* data objects

Where `print` gives you a one-line header, `summary` digs a level
deeper. Each `summary.gg_*()` method returns a `summary.gg` object: a
header line plus a few diagnostic statistics for that object type (the
OOB error curve, the top VIMP variables, a time range, the integrated
CRPS, and so on). `print.summary.gg()` renders it to the console.

## Usage

``` r
# S3 method for class 'summary.gg'
print(x, ...)

# S3 method for class 'gg_error'
summary(object, ...)

# S3 method for class 'gg_vimp'
summary(object, ...)

# S3 method for class 'gg_rfsrc'
summary(object, ...)

# S3 method for class 'gg_variable'
summary(object, ...)

# S3 method for class 'gg_partial'
summary(object, ...)

# S3 method for class 'gg_partial_rfsrc'
summary(object, ...)

# S3 method for class 'gg_partialpro'
summary(object, ...)

# S3 method for class 'gg_partial_varpro'
summary(object, ...)

# S3 method for class 'gg_roc'
summary(object, ...)

# S3 method for class 'gg_survival'
summary(object, ...)

# S3 method for class 'gg_varpro'
summary(object, ...)

# S3 method for class 'gg_udependent'
summary(object, ...)

# S3 method for class 'gg_brier'
summary(object, ...)

# S3 method for class 'gg_isopro'
summary(object, ...)

# S3 method for class 'gg_beta_varpro'
summary(object, ...)

# S3 method for class 'gg_ivarpro'
summary(object, ...)
```

## Arguments

- x:

  A `summary.gg` object (for `print.summary.gg`).

- ...:

  Not currently used.

- object:

  A `gg_*` data object.

## Value

A `summary.gg` object: a list with `header` and `body` character
vectors. `print.summary.gg` returns it invisibly.

## See also

[`print.gg`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md),
[`autoplot.gg`](https://ehrlinger.github.io/ggRandomForests/reference/autoplot.gg.md)

## Examples

``` r
set.seed(42)
airq <- na.omit(airquality)
rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
summary(gg_error(rf))
#> <gg_error>  from randomForestSRC  |  family: regr  |  ntree: 50  |  n: 111
#>   ntree: 50
#>   final OOB error: error=338.7
#>   min OOB error:   error=338.7
summary(gg_vimp(rf))
#> Warning: rfsrc object does not contain VIMP information. Calculating...
#> <gg_vimp>  from randomForestSRC  |  family: regr  |  ntree: 50  |  n: 111
#>   variables: 5
#>   positive VIMP: 5 / negative: 0
#>   top 5: Wind (1749), Temp (1332), Solar.R (270.5), Day (219.9), Month (77.35)
```
