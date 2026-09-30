# nonparametric Nelson-Aalen estimates

`cum_haz` is the Nelson-Aalen estimate of the cumulative hazard: at each
event time the number of events is divided by the number at risk, and
the ratios are summed. The `surv` column, its standard error and its
confidence limits are the Kaplan-Meier estimates, as
[`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
returns them, and `hazard`, `density`, `life` and `proplife` are derived
from that `surv`. The two functions therefore differ only in `cum_haz`,
where `kaplan` reports \\-\log S(t)\\. The two agree closely while the
risk set is large and diverge in the tail; when the last observation is
an event, \\-\log S(t)\\ is infinite there and the Nelson-Aalen sum is
not.

## Usage

``` r
nelson(interval, censor, data, by = NULL, weight = NULL, ...)
```

## Arguments

- interval:

  name of the interval variable in the training dataset.

- censor:

  name of the censoring variable in the training dataset.

- data:

  name of the survival training data.frame

- by:

  stratifying variable in the training dataset, defaults to NULL

- weight:

  optional numeric vector of event weights, one per row of `data`
  (default `NULL`, every event counts once). The weights apply to events
  only: each increment of `cum_haz` is the summed weight of the events
  at that time over the unweighted number at risk, so a censored
  observation's weight has no effect. Use it for severity-weighted
  events. The Kaplan-Meier columns are not weighted.

- ...:

  arguments passed to the `survfit` function

## Value

[`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
object

## See also

[`gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
`nelson`
[`plot.gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)

## Examples

``` r
# These get run through the gg_survival examples.
data(pbc, package = "randomForestSRC")
pbc$time <- pbc$days / 364.25

# This is the same as gg_survival
gg_dta <- nelson(
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

plot(gg_dta, error = "lines")

plot(gg_dta)


gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "treatment",
  type = "nelson"
)

plot(gg_dta, error = "bars")

plot(gg_dta)

```
