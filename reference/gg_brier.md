# Brier score and CRPS for survival forests

Extract the time-resolved Brier score and continuous ranked probability
score (CRPS) for a survival forest grown with `randomForestSRC`. The
Brier score is computed at each time on `object$time.interest`, both
overall and stratified by mortality-risk quartile. CRPS is the running
trapezoidal integral of the Brier score, normalised by elapsed time, and
is computed within each quartile and overall.

## Usage

``` r
gg_brier(object, ...)
```

## Arguments

- object:

  A fitted
  [`rfsrc`](https://www.randomforestsrc.org//reference/rfsrc.html)
  survival forest (`object$family == "surv"`).

- ...:

  Currently unused; accepted for S3 dispatch compatibility.

## Value

A `gg_brier` `data.frame` with columns

- time:

  event time grid (`object$time.interest`).

- brier:

  overall Brier score at each time.

- bs.q25, bs.q50, bs.q75, bs.q100:

  Brier score within each mortality-risk quartile (lowest to highest
  risk).

- bs.lower, bs.upper:

  15th and 85th percentile of per-subject Brier contributions at each
  time. Used by `plot.gg_brier(by_quartile = TRUE)` to draw an envelope
  around the overall curve.

- crps:

  running CRPS (overall) at each time, normalised by elapsed time.

- crps.q25, crps.q50, crps.q75, crps.q100:

  running CRPS within each mortality-risk quartile.

- crps.lower, crps.upper:

  running CRPS of the 15th / 85th per-subject Brier percentile,
  normalised by elapsed time.

The integrated CRPS (a single scalar matching
`get.brier.survival()$crps`) is attached as
`attr(., "crps_integrated")`.

## Details

Wraps
[`get.brier.survival`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)
and rebuilds the quartile decomposition + running CRPS from the returned
`brier.matx` and `mort` components, mirroring the computation in
`randomForestSRC:::plot.survival`. The Brier score uses
inverse-probability-of-censoring weighting; the censoring distribution
is estimated either by Kaplan-Meier (`cens.model = "km"`, the default)
or by a separate censoring forest (`cens.model = "rfsrc"`).

## References

Graf E., Schmoor C., Sauerbrei W., Schumacher M. (1999). Assessment and
comparison of prognostic classification schemes for survival data.
Statistics in Medicine, 18(17-18):2529-2545.

Gerds T.A., Schumacher M. (2006). Consistent estimation of the expected
Brier score in general survival models with right-censored event times.
Biometrical Journal, 48(6):1029-1040.

## See also

[`plot.gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md),
[`get.brier.survival`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html),
[`gg_error`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(pbc, package = "randomForestSRC")
rfsrc_pbc <- randomForestSRC::rfsrc(
  Surv(days, status) ~ ., data = pbc, nsplit = 10
)
gg_dta <- gg_brier(rfsrc_pbc)
plot(gg_dta)
plot(gg_dta, type = "crps")
plot(gg_dta, envelope = TRUE)   # overall line + 15-85% envelope

# Multi-model comparison: stack gg_brier outputs and plot with ggplot2.
rf2 <- randomForestSRC::rfsrc(
  Surv(days, status) ~ ., data = pbc, nsplit = 10, mtry = 4
)
compare_dta <- dplyr::bind_rows(
  dplyr::mutate(gg_brier(rfsrc_pbc), model = "default"),
  dplyr::mutate(gg_brier(rf2),       model = "mtry=4")
)
ggplot2::ggplot(compare_dta,
  ggplot2::aes(x = time, y = brier, colour = model)) +
  ggplot2::geom_line()
} # }
```
