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

  Character; name of the time-to-event column in `data`.

- censor:

  Character; name of the event-indicator column in `data` (1 = event
  occurred, 0 = censored).

- by:

  Optional character; name of a grouping column in `data` for stratified
  estimates. Defaults to `NULL` (unstratified).

- data:

  A `data.frame` containing the survival data.

- type:

  One of `"kaplan"` (Kaplan-Meier, default) or `"nelson"` (Nelson-Aalen
  cumulative hazard).

- ...:

  Additional arguments passed to
  [`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
  or
  [`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md)
  (e.g. `conf.int` to change the CI width).

## Value

A `gg_survival` `data.frame` with columns `time`, `surv` (or `cum_haz`
for Nelson-Aalen), `lower`, `upper` (confidence limits), and `n.risk`. A
`strata` column is added when `by` is supplied.

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
