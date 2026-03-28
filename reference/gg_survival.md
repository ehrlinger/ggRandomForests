# Nonparametric survival estimates.

Nonparametric survival estimates.

## Usage

``` r
gg_survival(
  object = NULL,
  interval = NULL,
  censor = NULL,
  by = NULL,
  data = NULL,
  type = c("kaplan", "nelson"),
  ...
)

# S3 method for class 'rfsrc'
gg_survival(
  object,
  interval = NULL,
  censor = NULL,
  by = NULL,
  data = NULL,
  type = c("kaplan", "nelson"),
  ...
)

# Default S3 method
gg_survival(
  object = NULL,
  interval = NULL,
  censor = NULL,
  by = NULL,
  data = NULL,
  type = c("kaplan", "nelson"),
  ...
)
```

## Arguments

- object:

  For the `rfsrc` method: a fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  survival forest. For the default method: pass `NULL` (or omit) and
  supply `interval`, `censor`, and `data` instead.

- interval:

  Character; name of the time-to-event column in `data` (default method
  only).

- censor:

  Character; name of the event-indicator column in `data` (1 = event, 0
  = censored; default method only).

- by:

  Optional character; name of a grouping column for stratified
  estimates. For the `rfsrc` method, `by` must be a column in
  `object$xvar`.

- data:

  A `data.frame` containing survival data (default method only).

- type:

  One of `"kaplan"` (Kaplan-Meier, default) or `"nelson"` (Nelson-Aalen
  cumulative hazard). Default method only.

- ...:

  Additional arguments passed to
  [`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
  or
  [`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md).

## Value

A `gg_survival` `data.frame` with columns `time`, `surv`, `cum_haz`,
`lower`, `upper`, `n.risk`, and optionally `groups` when `by` is
supplied.

## Details

`gg_survival` is an S3 generic for generating nonparametric survival
estimates. It dispatches on the class of its first argument:

- `rfsrc`:

  Extracts the response data from the fitted forest and delegates to
  [`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md).
  Use the `by` argument to stratify on a predictor stored in the model's
  `xvar` slot.

- default:

  Accepts raw survival data columns via the `interval`, `censor`, `by`,
  and `data` arguments, delegating to either
  [`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
  (default) or
  [`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md).

## See also

[`kaplan`](https://ehrlinger.github.io/ggRandomForests/reference/kaplan.md)
[`nelson`](https://ehrlinger.github.io/ggRandomForests/reference/nelson.md)

[`plot.gg_survival`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)

## Examples

``` r
## -------- pbc data (default method — raw data columns)
data(pbc, package = "randomForestSRC")
pbc$time <- pbc$days / 364.25

gg_dta <- gg_survival(interval = "time", censor = "status", data = pbc)
plot(gg_dta, error = "none")


# Stratified
gg_dta <- gg_survival(
  interval = "time", censor = "status",
  data = pbc, by = "treatment"
)
plot(gg_dta)

```
