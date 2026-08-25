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

.rhf_importance_cache_pbc <- function() {
  if (is.null(.rhf_cache$importance_cache)) {
    o <- .rhf_pbc()
    set.seed(20260825L)
    .rhf_cache$importance_cache <- randomForestRHF::varpro.cache.rhf(
      o, max.rules.tree = 30L, max.tree = 12L
    )
  }
  .rhf_cache$importance_cache
}

.rhf_importance_indices <- function() {
  cache <- .rhf_importance_cache_pbc()
  unique(pmax(1L, round(seq(1L, cache$K, length.out = 4L))))
}

.rhf_importance_pbc <- function() {
  if (is.null(.rhf_cache$importance)) {
    o <- .rhf_pbc()
    cache <- .rhf_importance_cache_pbc()
    set.seed(20260825L)
    .rhf_cache$importance <- randomForestRHF::importance.rhf(
      o,
      cache = cache,
      time.index = .rhf_importance_indices()
    )
  }
  .rhf_cache$importance
}

.fake_rhf_importance <- function() {
  object <- structure(list(
    xvar.names = c("x1", "x2", "x3"),
    time.interest = c(1, 2),
    family = "surv", ntree = 10L, n = 12L
  ), class = "rhf")
  mat <- matrix(c(0.2, 0.8, 0.1, 1.2, 0.4, 0.3), nrow = 3L,
                dimnames = list(object$xvar.names, c("1", "2")))
  win <- data.frame(
    index = 1:2, time = 1:2, start = c(0, 1), stop = 1:2,
    midpoint = c(0.5, 1.5), n.risk = c(12L, 8L),
    n.rules = c(20L, 15L), label = c("(0, 1]", "(1, 2]")
  )
  long <- data.frame(
    variable = rep(rownames(mat), times = 2L),
    time = rep(win$time, each = 3L),
    time.index = rep(win$index, each = 3L),
    window = rep(win$label, each = 3L),
    start = rep(win$start, each = 3L),
    stop = rep(win$stop, each = 3L),
    midpoint = rep(win$midpoint, each = 3L),
    n.risk = rep(win$n.risk, each = 3L),
    n.rules = rep(win$n.rules, each = 3L),
    importance = as.vector(mat)
  )
  long <- long[order(long$time.index, -long$importance, long$variable), ]
  fit <- structure(list(
    xvar.names = object$xvar.names,
    importance.matrix = mat,
    importance.long = long,
    window.info = win,
    y.source = "int.haz.oob",
    trim = 0.1
  ), class = "importance.rhf")
  list(object = object, fit = fit)
}
