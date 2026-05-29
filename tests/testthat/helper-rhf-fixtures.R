# Session-memoised randomForestRHF fixtures. rhf() fits are slow; compute
# once per R session and reuse. In-memory only — no disk cache.

.rhf_cache <- new.env(parent = emptyenv())

# Static-covariate pbc fit in counting-process form.
.rhf_pbc <- function() {
  if (is.null(.rhf_cache$pbc)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      testthat::skip("randomForestRHF not installed")
    }
    if (!requireNamespace("randomForestSRC", quietly = TRUE)) {
      testthat::skip("randomForestSRC not installed")
    }
    data(pbc, package = "randomForestSRC")
    d <- randomForestRHF::convert.counting(
      survival::Surv(days, status) ~ ., stats::na.omit(pbc)
    )
    .rhf_cache$pbc <- randomForestRHF::rhf(
      "Surv(id, start, stop, event) ~ .", d, ntree = 30, seed = -1L
    )
  }
  .rhf_cache$pbc
}
