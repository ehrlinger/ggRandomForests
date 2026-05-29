# Quantile-based cut points for coplots

This helper wraps [`quantile`](https://rdrr.io/r/stats/quantile.html) to
create well-spaced cut points for conditioning plots. When
`intervals = TRUE` the lower boundary is nudged down so that
[`cut()`](https://rdrr.io/r/base/cut.html) treats the minimum value as a
valid observation.

The output can be passed directly into the breaks argument of the `cut`
function for creating groups for coplots.

## Usage

``` r
quantile_pts(object, groups, intervals = FALSE)
```

## Arguments

- object:

  Numeric vector of predictor values.

- groups:

  Number of quantile points (or intervals) to compute.

- intervals:

  Logical indicating whether to return interval boundaries suitable for
  [`cut()`](https://rdrr.io/r/base/cut.html) (length `groups + 1`) or
  the interior quantile points (length `groups`).

## Value

Numeric vector of quantile points. When `intervals = TRUE` the result is
strictly increasing and can be supplied to
[`cut()`](https://rdrr.io/r/base/cut.html) to produce `groups` balanced
strata.

## See also

`cut`

## Examples

``` r
data(Boston, package = "MASS")
rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., Boston)

# To create 6 intervals, we want 7 points.
# quantile_pts will find balanced intervals
rm_pts <- quantile_pts(rfsrc_boston$xvar$rm, groups = 6, intervals = TRUE)

# Use cut to create the intervals
rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)

summary(rm_grp)
#> (3.56,5.76] (5.76,5.99] (5.99,6.21] (6.21,6.44] (6.44,6.85] (6.85,8.78] 
#>          85          84          84          85          84          84 
```
