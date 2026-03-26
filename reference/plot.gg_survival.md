# Plot a [`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md) object.

Plot a
[`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
object.

## Usage

``` r
# S3 method for class 'gg_survival'
plot(
  x,
  type = c("surv", "cum_haz", "hazard", "density", "mid_int", "life", "proplife"),
  error = c("shade", "bars", "lines", "none"),
  label = NULL,
  ...
)
```

## Arguments

- x:

  [`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  or a survival
  [`gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md)
  object created from a
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  object

- type:

  "surv", "cum_haz", "hazard", "density", "mid_int", "life", "proplife"

- error:

  "shade", "bars", "lines" or "none"

- label:

  Modify the legend label when gg_survival has stratified samples

- ...:

  not used

## Value

A `ggplot` object. The y-axis shows the chosen `type` (e.g. survival
probability for `"surv"`) and the x-axis shows time. Confidence shading,
bars, or lines are added when the input object carries
confidence-interval columns.

## See also

[`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md),
[`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md),
[`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md),
[`gg_rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md)

## Examples

``` r
## -------- pbc data
data(pbc, package = "randomForestSRC")
pbc$time <- pbc$days / 364.25

# This is the same as kaplan
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc
)

plot(gg_dta, error = "none")

plot(gg_dta)


# Stratified on treatment variable.
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "treatment"
)

plot(gg_dta, error = "none")

plot(gg_dta)

plot(gg_dta, label = "treatment")


# ...with smaller confidence limits.
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "treatment", conf.int = .68
)

plot(gg_dta, error = "lines")

plot(gg_dta, label = "treatment", error = "lines")
#> Ignoring unknown labels:
#> • fill : "treatment"


# ...with smaller confidence limits.
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "sex", conf.int = .68
)

plot(gg_dta, error = "lines")

plot(gg_dta, label = "sex", error = "lines")
#> Ignoring unknown labels:
#> • fill : "sex"

```
