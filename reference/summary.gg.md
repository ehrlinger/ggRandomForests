# Summary methods for gg\_\* data objects

Each `summary.gg_*()` method returns a `summary.gg` object containing a
header line and per-class diagnostic statistics (OOB error curve, top
VIMP variables, time range, integrated CRPS, etc.). `print.summary.gg()`
renders the object to the console.

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

# S3 method for class 'gg_roc'
summary(object, ...)

# S3 method for class 'gg_survival'
summary(object, ...)

# S3 method for class 'gg_brier'
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

A `summary.gg` object (a list with `header` and `body` character
vectors), returned invisibly from `print.summary.gg`.

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
#>   top 5: Wind (1770), Temp (1370), Solar.R (298.2), Day (202.5), Month (67.97)
```
