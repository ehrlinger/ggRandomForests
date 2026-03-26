# Bootstrap pointwise confidence bands for a mean survival curve

Draws `bs_samples` bootstrap resamples (with replacement) of the
per-observation survival curves stored in `gg_dta`, computes the column
means to obtain a bootstrapped mean curve per resample, then returns the
pointwise quantiles at `level_set` and the overall mean across
resamples.

## Usage

``` r
bootstrap_survival(gg_dta, bs_samples, level_set)
```

## Arguments

- gg_dta:

  A wide `data.frame` of survival probabilities as returned by the
  survival branch of
  [`gg_rfsrc.rfsrc`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md),
  before the optional pivot to long form. Columns `obs_id`, `event`, and
  `group` (if present) are excluded from the resampling.

- bs_samples:

  Integer; number of bootstrap resamples.

- level_set:

  Numeric vector of length 2 giving the lower and upper quantile
  probabilities for the confidence band (e.g. `c(0.025, 0.975)` for a
  95% CI).

## Value

A `data.frame` with one row per unique event time and columns `value`
(time), `lower`, `upper`, `median`, and `mean`.
