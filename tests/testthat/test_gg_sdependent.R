# Tests for gg_sdependent() — the sdependent() signal-detection wrapper.
#
# gg_sdependent() calls varPro::sdependent() on a get.beta.entropy() matrix.
# sdependent() does NOT grow a forest (no UBSAN path), so the matrix-driven
# tests run on CRAN with skip_if_not_installed("varPro"); only the live
# uvarpro() grow is skip_on_cran().

# A get.beta.entropy()-shaped matrix: square, named, NA diagonal.
.mock_entropy_sq <- function() {
  v <- c("a", "b", "c", "d")
  matrix(c(NA, .6, .1, .7,
           .5, NA, .2, .8,
           .1, .3, NA, .2,
           .6, .7, .1, NA),
         nrow = 4, byrow = TRUE, dimnames = list(v, v))
}
.stub_uvarpro <- function(ntree = 50L) {
  structure(list(ntree = ntree), class = "uvarpro")
}

test_that("gg_sdependent returns the expected tidy shape", {
  skip_if_not_installed("varPro")
  out <- gg_sdependent(.stub_uvarpro(), beta_fit = .mock_entropy_sq())
  expect_s3_class(out, "gg_sdependent")
  expect_named(out, c("variable", "imp_score", "degree", "signal"))
  expect_s3_class(out$variable, "factor")
  expect_type(out$imp_score, "double")
  expect_type(out$signal, "logical")
})

test_that("rows are ranked by imp_score; top is last factor level", {
  skip_if_not_installed("varPro")
  out <- gg_sdependent(.stub_uvarpro(), beta_fit = .mock_entropy_sq())
  expect_false(is.unsorted(rev(out$imp_score)))
  top <- as.character(out$variable[which.max(out$imp_score)])
  expect_equal(tail(levels(out$variable), 1), top)
})

test_that("signal flag matches sdependent()$signal.vars", {
  skip_if_not_installed("varPro")
  m <- .mock_entropy_sq()
  s <- varPro::sdependent(m, plot = FALSE)
  out <- gg_sdependent(.stub_uvarpro(), beta_fit = m)
  flagged <- as.character(out$variable[out$signal])
  expect_setequal(flagged, s$signal.vars)
})

test_that("provenance records the sdependent source and parameters", {
  skip_if_not_installed("varPro")
  prov <- attr(gg_sdependent(.stub_uvarpro(60L), beta_fit = .mock_entropy_sq(),
                             threshold = 0.3),
               "provenance")
  expect_equal(prov$source, "varPro::sdependent")
  expect_equal(prov$family, "unsupv")
  expect_equal(prov$ntree, 60L)
  expect_equal(prov$threshold, 0.3)
  expect_true(prov$precomputed)
})

test_that("... is ignored (with a warning) when beta_fit is supplied", {
  skip_if_not_installed("varPro")
  expect_warning(
    gg_sdependent(.stub_uvarpro(), beta_fit = .mock_entropy_sq(),
                  pre.filter = FALSE),
    "ignored because beta_fit is supplied"
  )
})

test_that("malformed beta_fit and non-uvarpro input are rejected", {
  expect_error(
    gg_sdependent(.stub_uvarpro(), beta_fit = list()),
    "does not look like a varPro::get.beta.entropy"
  )
  expect_error(
    gg_sdependent(structure(list(), class = "varpro")),
    "expected a 'uvarpro' object"
  )
})

test_that("empty beta matrix yields an empty gg_sdependent frame", {
  out <- gg_sdependent(.stub_uvarpro(), beta_fit = matrix(numeric(0), 0, 0))
  expect_s3_class(out, "gg_sdependent")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("variable", "imp_score", "degree", "signal"))
  expect_error(plot(out), "nothing to plot")
})

test_that("print / summary / autoplot companions work", {
  skip_if_not_installed("varPro")
  out <- gg_sdependent(.stub_uvarpro(), beta_fit = .mock_entropy_sq())
  expect_output(print(out), "gg_sdependent")
  expect_output(print(out), "flagged as signal")
  s <- summary(out)
  expect_s3_class(s, "summary.gg_sdependent")
  expect_true(any(grepl("signal variables", s$body)))
  expect_s3_class(ggplot2::autoplot(out), "ggplot")
})

# ---- live integration (skipped on CRAN: uvarpro grows a forest) -----------
test_that("gg_sdependent agrees with a live uvarpro + sdependent fit", {
  skip_on_cran()
  skip_if_not_installed("varPro")
  set.seed(1)
  o <- varPro::uvarpro(mtcars, ntree = 30)
  out <- gg_sdependent(o)
  expect_s3_class(out, "gg_sdependent")
  b <- varPro::get.beta.entropy(o)
  s <- varPro::sdependent(b, plot = FALSE)
  expect_setequal(as.character(out$variable[out$signal]), s$signal.vars)
})
