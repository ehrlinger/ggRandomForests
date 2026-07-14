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
    # envir = environment() is required, not stylistic: data() defaults to
    # .GlobalEnv, which is not on this function's lexical chain under
    # devtools::test() (testthat roots the helper env in the attached search
    # path), so a bare data(pbc) binds a pbc this function cannot see.
    utils::data("pbc", package = "randomForestSRC", envir = environment())

    d <- randomForestRHF::convert.counting(
      survival::Surv(days, status) ~ ., stats::na.omit(pbc)
    )
    # seed = -1L is randomForestRHF's documented reproducibility seed (a
    # negative integer fixes the forest's RNG); the set.seed() guards any
    # R-level RNG so the fixture — and the vdiffr snapshots built from it —
    # are deterministic across sessions.
    set.seed(20260529L)
    .rhf_cache$pbc <- randomForestRHF::rhf(
      "Surv(id, start, stop, event) ~ .", d, ntree = 30, seed = -1L
    )
  }
  .rhf_cache$pbc
}

# auct.rhf on the pbc fit — with bootstrap (CI ribbon) and without (NA CI).
.auct_pbc_boot <- function() {
  if (is.null(.rhf_cache$auct_boot)) {
    o <- .rhf_pbc()
    set.seed(20260529L)
    .rhf_cache$auct_boot <- randomForestRHF::auct.rhf(
      o, marker = "chf", bootstrap.rep = 20L
    )
  }
  .rhf_cache$auct_boot
}

.auct_pbc_noboot <- function() {
  if (is.null(.rhf_cache$auct_noboot)) {
    o <- .rhf_pbc()
    .rhf_cache$auct_noboot <- randomForestRHF::auct.rhf(o, marker = "chf")
  }
  .rhf_cache$auct_noboot
}
