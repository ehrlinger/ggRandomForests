# Plot a [`gg_ale_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ale_rfsrc.md) interaction object

Renders the second-order (interaction) ALE surface from
[`gg_ale_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ale_rfsrc.md)
(when called with `xvar2.name`) as a ggplot2 heatmap. Values near zero
mean the two predictors act additively over that region of their joint
range; large positive or negative values mark where their combined
effect departs from the sum of their individual main effects.

## Usage

``` r
# S3 method for class 'gg_ale_interaction'
plot(x, ...)
```

## Arguments

- x:

  A `gg_ale_interaction` object.

- ...:

  Not currently used.

## Value

A `ggplot` object.

## See also

[`gg_ale_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ale_rfsrc.md),
[`plot.gg_ale_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_ale_rfsrc.md)

## Examples

``` r
# \donttest{
airq.obj <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                                    ntree = 50)
ale_int <- gg_ale_rfsrc(airq.obj, xvar.names = "Wind",
                         xvar2.name = "Temp", n_eval = 12)
plot(ale_int)

# }
```
