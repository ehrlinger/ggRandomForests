# Plot a [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md) object

Produces ggplot2 partial dependence curves from the named list returned
by
[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md).

## Usage

``` r
# S3 method for class 'gg_partial_rfsrc'
plot(x, ...)
```

## Arguments

- x:

  A
  [`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  object.

- ...:

  Not currently used.

## Value

A `ggplot` (or `patchwork`) object. When both continuous and categorical
variables are present the two panels are combined vertically via
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Details

For standard (non-survival) forests: continuous predictors are line
plots, categorical predictors are bar charts, both faceted by variable
name.

For survival forests (when a `time` column is present): each evaluation
time point is a separate curve over the predictor's value, faceted by
variable name. The y-axis label adapts to the `partial.type` stored on
the object (“Predicted Survival”, “Predicted CHF”, or “Predicted
Mortality”).

For two-variable surface plots (when a `grp` column is present): each
group level is a separate line, faceted by primary predictor name.

## See also

[`gg_partial_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md),
[`plot.gg_partial`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md)

## Examples

``` r
## ------------------------------------------------------------
## Regression forest -- one continuous curve per variable
## ------------------------------------------------------------
set.seed(42)
airq <- na.omit(airquality)
rfsrc_airq <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)

pd <- gg_partial_rfsrc(rfsrc_airq, xvar.names = c("Wind", "Temp"),
                       n_eval = 10)
plot(pd)


# \donttest{
## ------------------------------------------------------------
## Survival forest -- one curve per requested time horizon,
## faceted by variable. Y-axis label tracks `partial.type`.
## ------------------------------------------------------------
# randomForestSRC's formula parser requires the unqualified Surv() symbol;
# it Depends on `survival`, so Surv is on the search path once
# randomForestSRC is loaded.
data(veteran, package = "randomForestSRC")
set.seed(42)
rfsrc_v <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
                                  data = veteran, ntree = 50)
ti  <- rfsrc_v$time.interest
t30 <- ti[which.min(abs(ti - 30))]
t90 <- ti[which.min(abs(ti - 90))]

# Default partial.type = "surv" -> y-axis "Predicted Survival"
pd_s <- gg_partial_rfsrc(rfsrc_v, xvar.names = "age",
                         partial.time = c(t30, t90), n_eval = 8)
plot(pd_s)


# partial.type = "chf" -> y-axis "Predicted CHF"
pd_c <- gg_partial_rfsrc(rfsrc_v, xvar.names = "age",
                         partial.time = c(t30, t90),
                         partial.type = "chf", n_eval = 8)
plot(pd_c)

# }
```
