test_that("gg_rhf_importance tidies a supplied priority result", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)

  expect_s3_class(x, "gg_rhf_importance")
  expect_identical(names(x), c(
    "variable", "time_window", "time", "time_index", "start", "stop",
    "midpoint", "n_risk", "n_rules", "priority"
  ))
  expect_equal(x$priority, f$fit$importance.long$importance)
  expect_false(any(c("z", "selected") %in% names(x)))
  expect_equal(tail(levels(x$variable), 1L), "x1")
  expect_true(attr(x, "provenance")$precomputed)
})

test_that("precomputed and calculation-only arguments cannot be mixed", {
  f <- .fake_rhf_importance()

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit, time.index = 1L),
    "calculation arguments"
  )
  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit, trim = 0.2),
    "calculation arguments"
  )
})

test_that("gg_rhf_importance validates the supplied result class", {
  f <- .fake_rhf_importance()
  class(f$fit) <- NULL

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "inherit from 'importance.rhf'"
  )
})

test_that("gg_rhf_importance rejects mismatched variables", {
  f <- .fake_rhf_importance()
  f$fit$xvar.names[1L] <- "other"

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "xvar.names.*do not match"
  )
})

test_that("gg_rhf_importance rejects a malformed priority matrix", {
  f <- .fake_rhf_importance()
  f$fit$importance.matrix <- as.data.frame(f$fit$importance.matrix)

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "importance.matrix.*malformed"
  )
})

test_that("gg_rhf_importance rejects misaligned window metadata", {
  f <- .fake_rhf_importance()
  f$fit$window.info <- f$fit$window.info[-1L, ]

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "window.info.*align"
  )
})

test_that("gg_rhf_importance checks windows against the RHF time grid", {
  f <- .fake_rhf_importance()
  f$fit$window.info$time[1L] <- 1.5

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "window.info.*time.interest"
  )
})

test_that("gg_rhf_importance rejects malformed long priority data", {
  f <- .fake_rhf_importance()
  f$fit$importance.long <- f$fit$importance.long[-1L, ]

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "importance.long.*malformed"
  )
})

test_that("gg_rhf_importance rejects unknown or duplicate long-data keys", {
  f <- .fake_rhf_importance()
  f$fit$importance.long$variable[1L] <- "other"

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "unknown variables or windows"
  )

  f <- .fake_rhf_importance()
  f$fit$importance.long$variable[2L] <- f$fit$importance.long$variable[1L]
  f$fit$importance.long$time.index[2L] <-
    f$fit$importance.long$time.index[1L]

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "unknown variables or windows"
  )
})

test_that("gg_rhf_importance checks long values against the matrix", {
  f <- .fake_rhf_importance()
  f$fit$importance.long$importance[1L] <- 9

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "importance.long.*importance.matrix"
  )
})

test_that("gg_rhf_importance checks long metadata against window metadata", {
  f <- .fake_rhf_importance()
  f$fit$importance.long$midpoint[1L] <- 9

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "importance.long.*window.info"
  )
})

test_that("gg_rhf_importance rejects negative priority values", {
  f <- .fake_rhf_importance()
  f$fit$importance.matrix["x1", "1"] <- -0.1
  row <- with(f$fit$importance.long, variable == "x1" & time.index == 1L)
  f$fit$importance.long$importance[row] <- -0.1

  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit),
    "negative priority"
  )
})

test_that("computed and precomputed paths return the same priority frame", {
  skip_on_cran()
  skip_if_not_installed("randomForestRHF")
  set.seed(20260825L)
  o <- .rhf_pbc()
  fit <- .rhf_importance_pbc()
  cached <- gg_rhf_importance(o, importance_fit = fit)
  computed <- gg_rhf_importance(
    o,
    cache = .rhf_importance_cache_pbc(),
    time.index = .rhf_importance_indices()
  )

  expect_equal(lapply(computed, identity), lapply(cached, identity))
  expect_false(attr(computed, "provenance")$precomputed)
})

test_that("gg_rhf_importance print reports its priority context", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)

  expect_output(print(x), "variables: 3")
  expect_output(print(x), "windows: 2")
  expect_output(print(x), "precomputed: TRUE")
  expect_output(print(x), "rank: q90")
  expect_output(print(x), "y_source: int.haz.oob")
  expect_invisible(print(x))
})

test_that("gg_rhf_importance summary ranks variables by q90 priority", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)
  s <- summary(x)

  expect_identical(names(s), c(
    "variable", "q90", "median", "mean", "max", "n_windows", "n_finite"
  ))
  expect_identical(s$variable, c("x1", "x2", "x3"))
  expect_equal(s$q90, c(1.1, 0.76, 0.28))
  expect_equal(s$median, c(0.7, 0.6, 0.2))
  expect_equal(s$max, c(1.2, 0.8, 0.3))
  expect_identical(s$n_windows, c(2L, 2L, 2L))
  expect_identical(s$n_finite, c(2L, 2L, 2L))
  expect_true(all(diff(s$q90) <= 0))
})

test_that("gg_rhf_importance autoplot forwards variable filters", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)
  p <- ggplot2::autoplot(x, vars = "x2")

  expect_s3_class(p, "ggplot")
  expect_identical(unique(as.character(p$data$variable)), "x2")
})
