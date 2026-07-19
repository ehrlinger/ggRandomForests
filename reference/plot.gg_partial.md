# Plot a [`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md) object

Turns a
[`gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)
object into a ggplot2 figure. Each curve is a partial dependence trace –
the forest's average prediction as one predictor is swept across its
range while the rest are marginalized over the training data. Continuous
predictors appear as line plots; categorical predictors appear as bar
charts. Both panels are faceted by variable name so you can compare the
shape and scale of each variable's effect at a glance.

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

A `ggplot` (or `patchwork`) object. When only one variable type is
present a single `ggplot` is returned. When both continuous and
categorical variables are present the two panels are combined vertically
via
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
which also satisfies `inherits(p, "ggplot")`.

## Details

When a `model` label was attached in
[`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
lines are colored by model – handy for overlaying results from two
forests (e.g., one tuned, one default) in the same figure.

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

```
