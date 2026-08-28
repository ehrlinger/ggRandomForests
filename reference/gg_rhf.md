# Tidy hazard and cumulative-hazard curves from a Random Hazard Forest

Extracts case-specific ensemble hazard and cumulative-hazard estimates
from a fitted
[`randomForestRHF::rhf()`](https://www.randomforestsrc.org//reference/rhf.html)
object into a tidy long data frame, one row per (case, time) pair on the
forest's `time.interest` grid.

## Usage

``` r
gg_rhf(object, ...)

# S3 method for class 'rhf'
gg_rhf(object, source = c("oob", "inbag"), ...)
```

## Arguments

- object:

  A fitted `rhf` object from randomForestRHF.

- ...:

  Not currently used.

- source:

  Which ensemble estimate to extract: `"oob"` (default, out-of-bag) or
  `"inbag"`. Falls back to the other when the requested one is absent
  (e.g. `bootstrap = "none"` has no OOB estimate).

## Value

A `data.frame` of class `c("gg_rhf", "data.frame")` with columns `id`,
`time`, `hazard`, `chf`, `source`, an integer `ntime` attribute (number
of grid points), and a `provenance` attribute.

The frame always has `ntime` rows per case. `hazard` may be `NA`: from
randomForestRHF 2.0.0 the pointwise hazard is defined only where a grid
point falls inside one of the case's supplied `(start, stop]` intervals,
and is `NA` in gaps and after the final stop time. `chf` is not masked.
It accumulates the exact interval overlap, so it stays flat across those
regions. The values are passed through unchanged; use `na.rm = TRUE`
when summarising `hazard`, or see
[`plot.gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rhf.md),
which drops the masked cells before drawing.

## References

Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
arXiv:2608.21597.
[doi:10.48550/arXiv.2608.21597](https://doi.org/10.48550/arXiv.2608.21597)
.

Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
R package version 2.0.0.
<https://CRAN.R-project.org/package=randomForestRHF>.

## See also

[`plot.gg_rhf()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_rhf.md),
[`randomForestRHF::rhf()`](https://www.randomforestsrc.org//reference/rhf.html)

## Examples

``` r
# \donttest{
if (requireNamespace("randomForestRHF", quietly = TRUE)) {
  data(pbc, package = "randomForestSRC")
  d <- randomForestRHF::convert.counting(
    survival::Surv(days, status) ~ ., na.omit(pbc))
  o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
  gg <- gg_rhf(o)
  plot(gg, idx = c(1, 5, 10))
}

# }
```
