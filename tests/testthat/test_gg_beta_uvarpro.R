# Tests for gg_beta_uvarpro() — the unsupervised (uvarpro) lasso-beta wrapper.
#
# The deterministic logic is tested through the `beta_fit` (precomputed)
# path with a hand-built matrix + a stub uvarpro object, so these run on CRAN
# with no live varPro grow (which would reach the randomForestSRC rule-grow
# path). One live integration test is skip_on_cran().

# A get.beta.entropy()-shaped matrix: rows = released variables, cols = all
# variables, cells = |lasso beta|. colMeans(na.rm=TRUE) is the importance.
.mock_beta_entropy <- function() {
  m <- matrix(
    c(NA, 0.2, 0.8, 0.1,
      0.5, NA, 0.6, 0.3,
      0.4, 0.4, NA, 0.2),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c", "d"))
  )
  m
}
.stub_uvarpro <- function(ntree = 50L) {
  structure(list(ntree = ntree), class = "uvarpro")
}

test_that("gg_beta_uvarpro returns the expected tidy shape", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  expect_s3_class(out, "gg_beta_uvarpro")
  expect_s3_class(out, "data.frame")
  expect_named(out, c("variable", "beta_mean", "n_released", "selected"))
  expect_s3_class(out$variable, "factor")
  expect_type(out$beta_mean, "double")
  expect_type(out$n_released, "integer")
  expect_type(out$selected, "logical")
  expect_equal(nrow(out), 4L)
})

test_that("beta_mean equals colMeans(|beta|) per variable", {
  m <- .mock_beta_entropy()
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = m)
  expected <- colMeans(m, na.rm = TRUE)
  # align by variable name (out is sorted descending)
  got <- stats::setNames(out$beta_mean, as.character(out$variable))
  expect_equal(got[names(expected)], expected[names(expected)])
})

test_that("rows are most-important first; factor levels reversed for coord_flip", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  # row order strictly non-increasing in beta_mean
  expect_false(is.unsorted(rev(out$beta_mean)))
  # the most-important variable is the LAST factor level (top after flip)
  top <- as.character(out$variable[which.max(out$beta_mean)])
  expect_equal(tail(levels(out$variable), 1), top)
})

test_that("n_released counts non-NA contributing regions", {
  m <- .mock_beta_entropy()
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = m)
  got <- stats::setNames(out$n_released, as.character(out$variable))
  expect_equal(unname(got["a"]), sum(!is.na(m[, "a"])))
  expect_equal(unname(got["b"]), sum(!is.na(m[, "b"])))
})

test_that("cutoff = 0 selects everything, Inf selects nothing; default is mean", {
  m <- .mock_beta_entropy()
  expect_true(all(gg_beta_uvarpro(.stub_uvarpro(), beta_fit = m, cutoff = 0)$selected))
  expect_false(any(gg_beta_uvarpro(.stub_uvarpro(), beta_fit = m, cutoff = Inf)$selected))

  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = m)
  prov <- attr(out, "provenance")
  expect_equal(unname(prov$cutoff[["unsupv"]]), mean(colMeans(m, na.rm = TRUE)))
  expect_true(prov$cutoff_default)
  expect_false(prov$precomputed == FALSE)  # beta_fit supplied -> precomputed TRUE
})

test_that("provenance records the unsupervised source and dimensions", {
  m <- .mock_beta_entropy()
  prov <- attr(gg_beta_uvarpro(.stub_uvarpro(77L), beta_fit = m), "provenance")
  expect_equal(prov$source, "varPro::get.beta.entropy")
  expect_equal(prov$family, "unsupv")
  expect_equal(prov$ntree, 77L)
  expect_equal(prov$n_var, ncol(m))
  expect_equal(prov$n_released_regions, nrow(m))
})

test_that("gg_beta_uvarpro warns when ... is supplied alongside beta_fit", {
  expect_warning(
    out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy(),
                           pre.filter = FALSE),
    "ignored because beta_fit is supplied"
  )
  expect_s3_class(out, "gg_beta_uvarpro")
})

test_that("malformed beta_fit is rejected with a clear message", {
  expect_error(
    gg_beta_uvarpro(.stub_uvarpro(), beta_fit = list()),
    "does not look like a varPro::get.beta.entropy"
  )
  unnamed <- matrix(1:6, nrow = 2)
  expect_error(
    gg_beta_uvarpro(.stub_uvarpro(), beta_fit = unnamed),
    "must have column names"
  )
})

test_that("empty beta matrix yields an empty gg_beta_uvarpro frame", {
  empty <- matrix(numeric(0), nrow = 0, ncol = 0)
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = empty)
  expect_s3_class(out, "gg_beta_uvarpro")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("variable", "beta_mean", "n_released", "selected"))
  expect_error(plot(out), "nothing to plot")
})

test_that("plot returns a ggplot", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  p <- plot(out)
  expect_s3_class(p, "ggplot")
})

test_that("print returns invisibly and shows a header line", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  expect_output(print(out), "gg_beta_uvarpro")
  expect_output(print(out), "variables selected")
  expect_identical(withVisible(print(out))$visible, FALSE)
})

test_that("summary returns a summary.gg object with the top variables", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_uvarpro")
  expect_s3_class(s, "summary.gg")
  expect_true(any(grepl("top variables", s$body)))
})

test_that("autoplot dispatches to plot and returns a ggplot", {
  out <- gg_beta_uvarpro(.stub_uvarpro(), beta_fit = .mock_beta_entropy())
  expect_s3_class(ggplot2::autoplot(out), "ggplot")
})

test_that("non-uvarpro input is rejected", {
  expect_error(
    gg_beta_uvarpro(structure(list(), class = "varpro")),
    "expected a 'uvarpro' object"
  )
})

# ---- live integration (skipped on CRAN: uvarpro grows a forest) -----------
test_that("gg_beta_uvarpro agrees with a live get.beta.entropy() fit", {
  skip_on_cran()
  skip_if_not_installed("varPro")
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 30)
  b <- varPro::get.beta.entropy(o)
  cached   <- gg_beta_uvarpro(o, beta_fit = b)
  expect_s3_class(cached, "gg_beta_uvarpro")
  expect_equal(nrow(cached), ncol(b))
  got <- stats::setNames(cached$beta_mean, as.character(cached$variable))
  exp <- colMeans(b, na.rm = TRUE)
  expect_equal(got[names(exp)], exp[names(exp)])
})
