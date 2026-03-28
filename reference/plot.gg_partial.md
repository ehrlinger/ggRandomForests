# Plot a [`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md) object

Produces ggplot2 partial dependence curves from the named list returned
by
[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md).
Continuous predictors are shown as line plots; categorical predictors
are shown as bar charts. Both panels are faceted by variable name so
multiple predictors can be compared at a glance.

## Usage

``` r
# S3 method for class 'gg_partial'
plot(x, ...)
```

## Arguments

- x:

  A
  [`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)
  object (output of
  [`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)).

- ...:

  Not currently used; reserved for future arguments.

## Value

When only continuous or only categorical variables are present, a single
`ggplot` object. When both are present, a named list with elements
`continuous` and `categorical`, each a `ggplot`.

## See also

[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
[`plot.gg_variable`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md)

## Examples

``` r
set.seed(42)
airq <- na.omit(airquality)
rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
pv <- randomForestSRC::plot.variable(rf, partial = TRUE, show.plots = FALSE)
pd <- gg_partial(pv)
plot(pd)
#> $continuous

#> 
#> $categorical

#> 
```
