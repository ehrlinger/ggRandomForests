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
  expected_order <- names(sort(expected, decreasing = TRUE))
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
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_equal(prov$which_class, "virginica")
  expect_setequal(as.character(unique(out$class)), "virginica")
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
