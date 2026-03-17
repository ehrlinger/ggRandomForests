# Nonparametric survival estimates.

Nonparametric survival estimates.

## Usage

``` r
gg_survival(
  interval = NULL,
  censor = NULL,
  by = NULL,
  data,
  type = c("kaplan", "nelson"),
  ...
)
```

## Arguments

- interval:

  name of the interval variable in the training dataset.

- censor:

  name of the censoring variable in the training dataset.

- by:

  stratifying variable in the training dataset, defaults to NULL

- data:

  name of the training data.frame

- type:

  one of ("kaplan","nelson"), defaults to Kaplan-Meier

- ...:

  extra arguments passed to Kaplan or Nelson functions.

## Value

A `gg_survival` object created using the non-parametric Kaplan-Meier or
Nelson-Aalen estimators.

## Details

`gg_survival` is a wrapper function for generating nonparametric
survival estimates using either
[`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md)-Aalen
or
[`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)-Meier
estimates.

## See also

[`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
[`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md)

[`plot.gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)

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


# ...with smaller confidence limits.
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "treatment", conf.int = .68
)

plot(gg_dta, error = "lines")

```
