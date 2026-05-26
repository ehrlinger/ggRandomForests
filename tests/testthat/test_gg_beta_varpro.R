test_that("gg_beta_varpro returns the expected tidy shape", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)

  expect_s3_class(out, "gg_beta_varpro")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out),
                  c("variable", "beta_mean", "n_rules", "selected"))
  released <- unique(b$results$variable[is.finite(b$results$imp)])
  expect_equal(nrow(out), length(released))
})

test_that("gg_beta_varpro aggregation equals mean(|imp|) per variable", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  for (i in seq_len(min(2L, nrow(out)))) {
    var_name <- as.character(out$variable[i])
    var_idx  <- match(var_name, b$xvar.names)
    expected <- mean(abs(res$imp[res$variable == var_idx]))
    expect_equal(out$beta_mean[i], expected, tolerance = 1e-10,
                 label = paste0("beta_mean for ", var_name))
  }
})

test_that("gg_beta_varpro errors on non-regression family", {
  if (!requireNamespace("varPro", quietly = TRUE)) skip("varPro not installed")
  set.seed(20260526L)
  iris2 <- iris
  vc <- varPro::varpro(Species ~ ., data = iris2, ntree = 30)
  expect_error(
    gg_beta_varpro(vc),
    "family = 'class'"
  )
})

test_that("gg_beta_varpro cutoff = 0 selects everything, Inf selects nothing", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  zero_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = 0)
  expect_true(all(zero_cut$selected))

  inf_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = Inf)
  expect_true(!any(inf_cut$selected))
})

test_that("gg_beta_varpro default cutoff is mean(beta_mean), provenance flagged", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)
  prov <- attr(out, "provenance")

  expect_equal(prov$cutoff, mean(out$beta_mean), tolerance = 1e-10)
  expect_true(prov$cutoff_default)

  explicit <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.123)
  expect_false(attr(explicit, "provenance")$cutoff_default)
  expect_equal(attr(explicit, "provenance")$cutoff, 0.123)
})

test_that("gg_beta_varpro rejects malformed beta_fit", {
  v <- .varpro_mtcars()
  expect_error(
    gg_beta_varpro(v, beta_fit = list()),
    "beta_fit does not look like a varPro::beta.varpro\\(\\) result"
  )

  b <- .beta_fit_mtcars()
  b_bad <- b
  b_bad$results <- b_bad$results[, setdiff(names(b_bad$results), "imp"), drop = FALSE]
  expect_error(
    gg_beta_varpro(v, beta_fit = b_bad),
    "imp"
  )
})

test_that("gg_beta_varpro warns when ... is supplied alongside beta_fit", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  expect_warning(
    out <- gg_beta_varpro(v, use.cv = TRUE, beta_fit = b),
    "ignored because beta_fit is supplied"
  )
  expect_s3_class(out, "gg_beta_varpro")
})

test_that("gg_beta_varpro returns empty frame when beta.varpro yields no rules", {
  v <- .varpro_mtcars()
  empty <- structure(
    list(results = data.frame(tree = integer(0), branch = integer(0),
                              variable = integer(0), n.oob = integer(0),
                              imp = numeric(0)),
         xvar.names = v$xvar.names),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = empty)
  expect_s3_class(out, "gg_beta_varpro")
  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), c("variable", "beta_mean", "n_rules", "selected"))
  expect_equal(attr(out, "provenance")$n_rules_total, 0L)
})

test_that("gg_beta_varpro counts lasso-shrunk-to-zero rules in n_rules", {
  v <- .varpro_mtcars()
  fake <- structure(
    list(
      results = data.frame(
        tree     = c(1L, 1L, 2L, 2L),
        branch   = c(1L, 2L, 1L, 2L),
        variable = c(1L, 1L, 1L, 1L),
        n.oob    = c(5L, 5L, 5L, 5L),
        imp      = c(0.0, 0.0, 1.5, -2.5)
      ),
      xvar.names = v$xvar.names
    ),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = fake)
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_rules, 4L)
  # mean(|0|, |0|, |1.5|, |-2.5|) = 1.0
  expect_equal(out$beta_mean, 1.0, tolerance = 1e-10)
})

test_that("gg_beta_varpro cached and uncached paths agree", {
  if (!identical(Sys.getenv("GG_BETA_VARPRO_SLOW_TESTS", "true"), "true")) {
    skip("Slow test — set GG_BETA_VARPRO_SLOW_TESTS=true to run")
  }
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()

  set.seed(20260526L)
  uncached <- gg_beta_varpro(v)
  cached   <- gg_beta_varpro(v, beta_fit = b)

  # Strip provenance before comparing the tidy-frame content — `precomputed`
  # differs (TRUE vs FALSE) between the two paths by design, and that flag is
  # checked separately below.
  strip <- function(x) {
    attr(x, "provenance") <- NULL
    as.data.frame(x)
  }
  expect_equal(
    strip(uncached),
    strip(cached),
    tolerance = 1e-10
  )
  expect_false(attr(uncached, "provenance")$precomputed)
  expect_true(attr(cached, "provenance")$precomputed)
})

test_that("plot.gg_beta_varpro returns a ggplot that builds", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  p <- plot(out)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_true(length(built$data) >= 1L)
})

test_that("gg_beta_varpro S3 companions return the expected shapes", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  # print invisibly returns x
  pr <- withVisible(print(out))
  expect_false(pr$visible)
  expect_identical(pr$value, out)

  # summary returns a named numeric, sorted descending, attribute n_rules present
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_varpro")
  expect_type(unclass(s), "double")
  # as.numeric() strips both names and the n_rules attr so the comparison is
  # value-only; sort() drops the n_rules attr and would otherwise cause a
  # spurious attribute-mismatch failure.
  vals <- as.numeric(unclass(s))
  expect_equal(vals, sort(vals, decreasing = TRUE))
  expect_equal(length(s), nrow(out))
  expect_true(!is.null(attr(s, "n_rules")))

  # autoplot delegates to plot
  p1 <- plot(out)
  p2 <- ggplot2::autoplot(out)
  expect_equal(
    ggplot2::ggplot_build(p1)$data,
    ggplot2::ggplot_build(p2)$data
  )
})
