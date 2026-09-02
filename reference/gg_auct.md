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
  See the note below on choosing between them. Not used when `auct_fit`
  is supplied, though the value is still validated.

## Value

A `data.frame` of class `c("gg_auct", "data.frame")` with columns
`time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
bootstrap), an `iauc` attribute (a list with `uno`, `std`, `uno.se`,
`std.se`, `conf.level`), and a `provenance` attribute derived from
`object` (source, family, ntree, n).

## Note

The two definitions answer different questions rather than better and
worse versions of the same one. Cumulative/dynamic AUC ranks accumulated
risk through a horizon, comparing subjects who have failed by that
horizon against subjects still event-free at it. Incident/dynamic AUC
ranks local failures within the risk set at each time. Pick the one that
matches the question you are asking, and read the two curves as separate
estimands rather than as a check on each other.

Cumulative/dynamic AUC was unreliable under randomForestRHF 2.0.0, which
could push the curve below the 0.5 chance line on data the forest fits
well. That was an upstream problem, fixed in 2.0.3. R does not enforce a
`Suggests` version at run time, so `gg_auct()` checks the installed
version itself and errors rather than compute a cumulative/dynamic curve
it knows to be wrong. The check applies only when `gg_auct()` does the
computation: `method = "incident"` is unaffected by the upstream problem
and is never gated, and a supplied `auct_fit` is taken as given, since
an `auct.rhf` object records no version and may have been read from a
file. `gg_auct()` passes the values through unchanged in every case.

## References

Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
arXiv:2608.21597.
[doi:10.48550/arXiv.2608.21597](https://doi.org/10.48550/arXiv.2608.21597)
.

Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
R package version 2.0.3.
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
