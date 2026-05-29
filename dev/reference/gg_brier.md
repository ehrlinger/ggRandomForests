# Brier score and CRPS for survival forests

The Brier score asks a familiar question of any probabilistic forecast:
how far did the predicted probability sit from what actually happened?
For a survival forest the forecast is the predicted survival
probability, and the score is computed at each event time, so the result
is a curve rather than a single number – lower is better, at every time.

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

This function extracts that time-resolved Brier score for a survival
forest grown with `randomForestSRC`, both overall and split by
mortality-risk quartile. It also returns the continuous ranked
probability score (CRPS), which is the Brier score integrated over time
and divided by elapsed time – a running average of the curve so far.

This wraps
[`get.brier.survival`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)
and rebuilds the quartile decomposition and running CRPS from the
returned `brier.matx` and `mort` components, following the computation
in the internal `plot.survival` function of randomForestSRC.
Right-censored data make a plain Brier score biased, so the score uses
inverse-probability-of-censoring weighting. The censoring distribution
is estimated either by Kaplan-Meier (`cens.model = "km"`, the default)
or by a separate censoring forest (`cens.model = "rfsrc"`).

## Note

Brier score / CRPS is `randomForestSRC` survival-only; there is no
`randomForest` method.

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
# \donttest{
library(survival)   # Surv() must be on the search path for rfsrc()
#> 
#> Attaching package: ‘survival’
#> The following object is masked _by_ ‘.GlobalEnv’:
#> 
#>     pbc
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

# }
```
