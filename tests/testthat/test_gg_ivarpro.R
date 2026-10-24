# ---- Dispatch --------------------------------------------------------------

test_that("gg_ivarpro: non-varpro input errors via the default method", {
  # No varPro fit needed — the default method fires before any varpro work.
  expect_error(gg_ivarpro(data.frame(a = 1:3)),
               "expected a 'varpro' object")
  expect_error(gg_ivarpro(1:3), "got an object of class integer")
})

# ---- Shape ----------------------------------------------------------------

test_that("gg_ivarpro regression returns long-format tidy frame", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)

  expect_s3_class(out, "gg_ivarpro")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("obs", "variable", "local_imp", "selected"))
  expect_true(all(!is.na(out$local_imp)))  # NA cells filtered out
  expect_true(is.factor(out$variable))
})

test_that("gg_ivarpro classification adds class column", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)

  expect_s3_class(out, "gg_ivarpro")
  expect_true("class" %in% names(out))
  expect_setequal(as.character(unique(out$class)), levels(iris$Species))
})

test_that("gg_ivarpro variable factor levels ordered by descending mean(|local_imp|)", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_true(is.factor(out$variable))

  expected <- tapply(abs(out$local_imp), out$variable, mean, na.rm = TRUE)
  # Reversed-descending: most-important variable is the LAST level (top of
  # the plot after coord_flip).
  expected_order <- rev(names(sort(expected, decreasing = TRUE)))
  expect_equal(levels(out$variable), expected_order)
})

# ---- which_obs ------------------------------------------------------------

test_that("gg_ivarpro which_obs filters to a single observation", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L)
  expect_true(all(out$obs == 1L))
  expect_equal(attr(out, "provenance")$which_obs, 1L)
})

test_that("gg_ivarpro which_obs out of range errors with valid range", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  n  <- nrow(MASS::Boston)
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv, which_obs = n + 1L),
    "out of range"
  )
})

# ---- which_class ----------------------------------------------------------

test_that("gg_ivarpro binary classification which_class = NULL defaults to last factor level", {
  v  <- .varpro_iris_binary_for_ivarpro()
  iv <- .ivarpro_iris_binary()
  # Which cells ivarpro() leaves finite depends on the upstream RNG, so force
  # them finite: this test is about the default class, not varPro's output.
  iv[] <- lapply(iv, function(x) seq_along(x) / length(x))
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_equal(prov$which_class, "virginica")
  expect_setequal(as.character(unique(out$class)), "virginica")
})

# A class whose every local-importance cell is nonfinite yields a 0-row
# frame; tagging it with its class name must not fail R's row-count check.
test_that("gg_ivarpro binary: all-nonfinite profile returns an empty gg_ivarpro", {
  v  <- .varpro_iris_binary_for_ivarpro()
  iv <- .ivarpro_iris_binary()
  iv[] <- lapply(iv, function(x) rep(NaN, length(x)))
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_s3_class(out, "gg_ivarpro")
  expect_equal(nrow(out), 0L)
  expect_true("class" %in% names(out))
})

test_that("gg_ivarpro multiclass: one all-nonfinite class drops out, the rest survive", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  iv[["setosa"]][] <- lapply(iv[["setosa"]], function(x) rep(NaN, length(x)))
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_gt(nrow(out), 0L)
  expect_false("setosa" %in% as.character(out$class))
})

# Which cells ivarpro() leaves finite depends on the upstream RNG: on
# randomForestSRC 3.9.3 the binary fixture comes back all NA. The binary
# tests check ggRF's labelling and mirroring, not varPro's slopes, so they
# fill every cell with a finite value and keep varPro's attributes (target,
# ivarpro.path, data).
.finite_ivarpro <- function(iv) {
  iv[] <- lapply(seq_along(iv), function(j) (seq_len(nrow(iv)) + j) / nrow(iv) - 0.5)
  iv
}

# varPro explains the FIRST level on a binary fit (attr "target"); the last
# level is derived from it. Look up varPro's raw value for each output row.
.raw_ivarpro_at <- function(iv, out) {
  m <- as.matrix(iv)
  m[cbind(out$obs, match(as.character(out$variable), colnames(m)))]
}

test_that("gg_ivarpro binary: first level is varPro's own target profile", {
  v  <- .varpro_iris_binary_for_ivarpro()
  iv <- .ivarpro_iris_binary()
  iv <- .finite_ivarpro(iv)
  expect_equal(attr(iv, "target"), "versicolor")
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "versicolor")
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
})

test_that("gg_ivarpro binary: last level is the target's negated slopes", {
  v  <- .varpro_iris_binary_for_ivarpro()
  iv <- .ivarpro_iris_binary()
  iv <- .finite_ivarpro(iv)
  out <- gg_ivarpro(v, ivarpro_fit = iv)   # default: virginica
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, -.raw_ivarpro_at(iv, out))
})

test_that("gg_ivarpro binary: use.abs profiles are not negated", {
  v  <- .varpro_iris_binary_for_ivarpro()
  set.seed(20261005L)
  iv <- varPro::ivarpro(v, use.abs = TRUE)
  iv <- .finite_ivarpro(iv)
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
})

test_that("gg_ivarpro binary: a y.external target that is not a class is not mirrored", {
  v <- .varpro_iris_binary_for_ivarpro()
  p <- v$rf$predicted.oob
  set.seed(20261005L)
  iv <- varPro::ivarpro(v, y.external = matrix(p[, 2L], dimnames = list(NULL, "score")))
  iv <- .finite_ivarpro(iv)
  expect_equal(attr(iv, "target"), "score")
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_gt(nrow(out), 0L)
  expect_setequal(as.character(unique(out$class)), "virginica")
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
})

# A y.external target may carry a class's name and still be any numeric
# response, so it must not be mirrored into a second, complementary class.
test_that("gg_ivarpro binary: y.external named after a class is not mirrored", {
  v <- .varpro_iris_binary_for_ivarpro()
  ext <- matrix(v$x[, 1L], dimnames = list(NULL, "versicolor"))
  ## The default view is the last level whether or not iv was mirrored, so
  ## probe the first level: a mirrored profile would have rows there.
  ## gg_ivarpro() runs ivarpro() itself, so it sees y.external in '...'
  set.seed(20261023L)
  expect_equal(nrow(gg_ivarpro(v, y.external = ext, which_class = "versicolor")), 0L)
  ## precomputed with save.data = TRUE: the saved response is not the OOB
  ## probability of versicolor
  set.seed(20261023L)
  iv <- varPro::ivarpro(v, y.external = ext, save.data = TRUE)
  iv <- .finite_ivarpro(iv)
  expect_equal(nrow(gg_ivarpro(v, ivarpro_fit = iv, which_class = "versicolor")), 0L)
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
  ## precomputed, named after the second level: never what ivarpro() explains
  set.seed(20261023L)
  iv <- varPro::ivarpro(v, y.external = matrix(v$x[, 1L],
                                               dimnames = list(NULL, "virginica")),
                        save.data = FALSE)
  iv <- .finite_ivarpro(iv)
  expect_equal(nrow(gg_ivarpro(v, ivarpro_fit = iv, which_class = "versicolor")), 0L)
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
})

test_that("gg_ivarpro binary: a saved internal target is still mirrored", {
  v <- .varpro_iris_binary_for_ivarpro()
  set.seed(20261023L)
  iv <- varPro::ivarpro(v, save.data = TRUE)
  iv <- .finite_ivarpro(iv)
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "versicolor")
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
  ## saved without its data: nothing refutes the internal target, so mirrored
  set.seed(20261023L)
  iv <- varPro::ivarpro(v, save.data = FALSE)
  expect_null(attr(iv, "data"))
  iv <- .finite_ivarpro(iv)
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "versicolor")
  expect_gt(nrow(out), 0L)
  expect_equal(out$local_imp, .raw_ivarpro_at(iv, out))
})

test_that("gg_ivarpro which_class explicit returns single class", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "setosa")
  expect_equal(as.character(unique(out$class)), "setosa")
})

test_that("gg_ivarpro which_class not in levels errors", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv, which_class = "bogus"),
    "is not a level of the response"
  )
})

test_that("gg_ivarpro which_class on regression warns and is ignored", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  expect_warning(
    out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "anything"),
    "ignored for regression family"
  )
  expect_false("class" %in% names(out))
})

# ---- cutoff polymorphism --------------------------------------------------

test_that("gg_ivarpro cutoff = NULL is per-class mean(|local_imp|) (named vector)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, levels(iris$Species))
  for (cls in levels(iris$Species)) {
    expect_equal(
      prov$cutoff[[cls]],
      mean(abs(out$local_imp[out$class == cls])),
      tolerance = 1e-10
    )
  }
})

test_that("gg_ivarpro scalar cutoff broadcasts across classes", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv, cutoff = 0.5)
  prov <- attr(out, "provenance")
  expect_equal(unname(prov$cutoff), rep(0.5, 3))
})

test_that("gg_ivarpro regression cutoff is length-1 named numeric", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, "regr")
  expect_length(prov$cutoff, 1L)
})

# ---- ivarpro_fit shape guard ----------------------------------------------

test_that("gg_ivarpro rejects malformed ivarpro_fit", {
  v <- .varpro_boston()
  expect_error(gg_ivarpro(v, ivarpro_fit = list()),
               "does not look like a varPro::ivarpro\\(\\) result")
})

test_that("gg_ivarpro classification ivarpro_fit must be list of K named frames", {
  v <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  iv_bad <- iv[c("setosa", "versicolor")]   # drop one class
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv_bad),
    "class"
  )
})

test_that("gg_ivarpro shape guard catches mismatched row counts (PR #99 Copilot)", {
  # Regression: rows mismatch
  v_regr  <- .varpro_boston()
  iv_regr <- .ivarpro_boston()
  iv_short <- iv_regr[seq_len(nrow(iv_regr) - 1L), , drop = FALSE]
  expect_error(
    gg_ivarpro(v_regr, ivarpro_fit = iv_short),
    "rows but object trained"
  )

  # Classification: per-class row count mismatch
  v_cls  <- .varpro_iris_multiclass_for_ivarpro()
  iv_cls <- .ivarpro_iris_multiclass()
  iv_cls_bad <- iv_cls
  iv_cls_bad$setosa <- iv_cls_bad$setosa[seq_len(nrow(iv_cls_bad$setosa) - 1L), ,
                                         drop = FALSE]
  expect_error(
    gg_ivarpro(v_cls, ivarpro_fit = iv_cls_bad),
    "rows but object trained"
  )

  # Classification: a per-class element is not a data.frame
  iv_cls_bad2 <- iv_cls
  iv_cls_bad2$setosa <- as.matrix(iv_cls_bad2$setosa)
  expect_error(
    gg_ivarpro(v_cls, ivarpro_fit = iv_cls_bad2),
    "is not a data.frame"
  )
})

# ---- ... + ivarpro_fit warning --------------------------------------------

test_that("gg_ivarpro warns when ... is supplied alongside ivarpro_fit", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  expect_warning(
    out <- gg_ivarpro(v, ivarpro_fit = iv, use.loo = TRUE),
    "ignored because ivarpro_fit is supplied"
  )
  expect_s3_class(out, "gg_ivarpro")
})

# ---- family guard ---------------------------------------------------------

test_that("gg_ivarpro errors on regr+ / surv families", {
  v <- .varpro_boston()
  v_fake <- v
  v_fake$family <- "surv"
  expect_error(gg_ivarpro(v_fake), "family = 'surv'")
})

# ---- plot dispatch matrix --------------------------------------------------

test_that("plot.gg_ivarpro regression distribution builds", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_ivarpro regression which_obs builds (single panel)", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_ivarpro classification distribution builds (faceted)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(length(unique(built$layout$layout$PANEL)) >= 2L)
})

test_that("plot.gg_ivarpro classification which_obs builds (faceted)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

# ---- print + summary ------------------------------------------------------

test_that("print.gg_ivarpro prints invisibly with header", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  pr <- withVisible(print(out))
  expect_false(pr$visible)
  expect_identical(pr$value, out)
})

test_that("summary.gg_ivarpro regression returns descending named numeric", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_ivarpro")
  vals <- as.numeric(unclass(s))
  expect_equal(vals, sort(vals, decreasing = TRUE))
})

test_that("summary.gg_ivarpro classification returns per-class list", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_ivarpro")
  expect_true(is.list(unclass(s)))
  expect_setequal(names(unclass(s)), levels(iris$Species))
})

test_that("autoplot.gg_ivarpro matches plot", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  # The aggregate (no which_obs) regression view uses geom_jitter, whose
  # Stat re-randomises on every ggplot_build(). Seed both builds so the
  # comparison is deterministic — the test is checking that autoplot
  # delegates to plot, not testing jitter stability.
  set.seed(1L)
  d1 <- ggplot2::ggplot_build(plot(out))$data
  set.seed(1L)
  d2 <- ggplot2::ggplot_build(ggplot2::autoplot(out))$data
  expect_equal(d1, d2)
})

# ---- slow: cache equivalence ----------------------------------------------

test_that("gg_ivarpro cached and uncached paths agree (slow)", {
  testthat::skip_on_cran()
  if (!identical(Sys.getenv("GG_IVARPRO_SLOW_TESTS", "false"), "true")) {
    skip("Slow test - set GG_IVARPRO_SLOW_TESTS=true to run")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()

  set.seed(20260526L)
  uncached <- gg_ivarpro(v)
  cached   <- gg_ivarpro(v, ivarpro_fit = iv)

  strip <- function(df) {
    attr(df, "provenance") <- NULL
    df
  }
  expect_equal(strip(as.data.frame(uncached)),
               strip(as.data.frame(cached)),
               tolerance = 1e-6)
  expect_false(attr(uncached, "provenance")$precomputed)
  expect_true(attr(cached,   "provenance")$precomputed)
})
