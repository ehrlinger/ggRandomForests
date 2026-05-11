# `autoplot` methods for ggRandomForests data objects

These methods let you use
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
on any `gg_*` object returned by ggRandomForests. They are thin wrappers
around the corresponding `plot.gg_*()` S3 methods, so all arguments
accepted by those methods are forwarded via `...`.

## Usage

``` r
# S3 method for class 'gg_error'
autoplot(object, ...)

# S3 method for class 'gg_vimp'
autoplot(object, ...)

# S3 method for class 'gg_rfsrc'
autoplot(object, ...)

# S3 method for class 'gg_variable'
autoplot(object, ...)

# S3 method for class 'gg_partial'
autoplot(object, ...)

# S3 method for class 'gg_partial_rfsrc'
autoplot(object, ...)

# S3 method for class 'gg_partialpro'
autoplot(object, ...)

# S3 method for class 'gg_roc'
autoplot(object, ...)

# S3 method for class 'gg_survival'
autoplot(object, ...)

# S3 method for class 'gg_brier'
autoplot(object, ...)
```

## Arguments

- object:

  A `gg_*` data object (see Details).

- ...:

  Additional arguments forwarded to the underlying `plot.gg_*()` method.

## Value

A `ggplot` object.

## Details

The following `gg_*` classes are supported:

- `gg_error`:

  OOB error vs. number of trees

- `gg_vimp`:

  Variable importance ranking

- `gg_rfsrc`:

  Predicted vs. observed values

- `gg_variable`:

  Marginal dependence

- `gg_partial`:

  Partial dependence (via `plot.variable`)

- `gg_partial_rfsrc`:

  Partial dependence (via `partial.rfsrc`)

- `gg_partialpro`:

  Partial dependence (via `varPro`)

- `gg_roc`:

  ROC curve

- `gg_survival`:

  Survival / cumulative hazard curves

- `gg_brier`:

  Time-resolved Brier score and CRPS

## Examples

``` r
# \donttest{
library(ggplot2)
#> 
#> Attaching package: ‘ggplot2’
#> The following object is masked from ‘package:randomForest’:
#> 
#>     margin
set.seed(42)
rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                              ntree = 50, importance = TRUE,
                              tree.err = TRUE)
autoplot(gg_error(rf))
#> Ignoring unknown labels:
#> • colour : "Outcome"

autoplot(gg_vimp(rf))

# }
```
