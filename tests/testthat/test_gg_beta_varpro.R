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
