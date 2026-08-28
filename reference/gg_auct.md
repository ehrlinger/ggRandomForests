# Tidy time-varying AUC from a Random Hazard Forest

Extracts the time-dependent AUC curve from
[`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)
into a tidy long data frame, one row per time point, with bootstrap
confidence bounds when available and the integrated AUC (iAUC) summary
attached as an attribute.

## Usage

``` r
gg_auct(object, ...)

# S3 method for class 'rhf'
gg_auct(
  object,
  marker = c("chf", "haz"),
  auct_fit = NULL,
  method = c("cumulative", "incident"),
  ...
)
```

## Arguments

- object:

  A fitted `rhf` object from randomForestRHF.

- ...:

  Further arguments passed to
  [`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html),
  for example `bootstrap.rep` to request confidence bounds, or `riskset`
  for the incident definition. Ignored when `auct_fit` is supplied.

- marker:

  Risk marker for the AUC: `"chf"` (cumulative hazard, default) or
  `"haz"` (hazard). Not used when `auct_fit` is supplied, though the
  value is still validated.

- auct_fit:

  Optional precomputed
  [`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)
  result (class `"auct.rhf"`) for the same `object`. `NULL` (default)
  computes it. Supply it to reuse an expensive bootstrap run.

- method:

  Which time-dependent AUC definition to compute, passed to
  [`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html).
  `"cumulative"` (default) ranks accumulated risk through a horizon;
  `"incident"` ranks local failures within the risk set at each time.
  See the note below before relying on the default. Not used when
  `auct_fit` is supplied, though the value is still validated.

## Value

A `data.frame` of class `c("gg_auct", "data.frame")` with columns
`time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
bootstrap), an `iauc` attribute (a list with `uno`, `std`, `uno.se`,
`std.se`, `conf.level`), and a `provenance` attribute derived from
`object` (source, family, ntree, n).

## Note

Cumulative/dynamic AUC is unreliable under randomForestRHF 2.0.0, so
treat the `method = "cumulative"` default with care. That release holds
the in-sample cumulative hazard flat once a subject's supplied records
end, which
[`?randomForestRHF::rhf`](https://www.randomforestsrc.org//reference/rhf.html)
documents. At a fixed grid point the marker then reflects how long a
subject was observed as well as how much risk they carried, and the
cumulative/dynamic definition compares subjects who have already failed
against subjects still under follow-up. The curve can fall below the 0.5
chance line on data the forest fits well.

The incident/dynamic definition does not inherit this, because it
compares subjects within a risk set at each time, before any of them has
left follow-up. It answers a different question rather than a better
version of the same one, so reach for `method = "incident"` where that
question is the one you are asking. The behavior is upstream, reported
at <https://github.com/kogalur/randomForestRHF/issues/1>; `gg_auct()`
passes the values through unchanged in every case.

## References

Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
arXiv:2608.21597.
[doi:10.48550/arXiv.2608.21597](https://doi.org/10.48550/arXiv.2608.21597)
.

Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
R package version 2.0.0.
<https://CRAN.R-project.org/package=randomForestRHF>.

## See also

[`plot.gg_auct()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_auct.md),
[`randomForestRHF::auct.rhf()`](https://www.randomforestsrc.org//reference/auct.rhf.html)

## Examples

``` r
# \donttest{
if (requireNamespace("randomForestRHF", quietly = TRUE)) {
  data(pbc, package = "randomForestSRC")
  d <- randomForestRHF::convert.counting(
    survival::Surv(days, status) ~ ., na.omit(pbc))
  o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
  plot(gg_auct(o, marker = "chf"))
}

# }
```
