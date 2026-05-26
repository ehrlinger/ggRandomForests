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
