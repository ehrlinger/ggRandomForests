# Categorical partials must be labelled in the order varPro::partialpro()
# returns its columns ($xvirtual: sorted values, or factor levels), not in the
# order the values first appear in the sampled cases. With the old labelling,
# a 0/1 variable whose first sampled case was 1 had "0" and "1" swapped, and a
# multi-level variable had its levels shifted.

.cat_label_fit <- function() {
  if (is.null(.varpro_cache$cat_labels)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20261015L)
    v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50, split.weight = FALSE)
    set.seed(20261015L)
    ## method = "rnd": partialpro()'s default ("unsupv") reaches an unsupervised
    ## rfsrc() grow, the known gcc-UBSAN path (entry.c:184; see
    ## helper-varpro-fixtures.R). The supervised route keeps CRAN's sanitizer
    ## builds clean and changes nothing these tests check.
    pp <- varPro::partialpro(v, xvar.names = c("am", "vs", "cyl"), method = "rnd")
    .varpro_cache$cat_labels <- list(v = v, pp = pp)
  }
  .varpro_cache$cat_labels
}

test_that("each categorical level is labelled with varPro's own column for it", {
  fx <- .cat_label_fit()
  pp <- fx$pp
  g <- gg_partial_varpro(part_dta = pp, object = fx$v)
  ct <- g$categorical
  for (nm in c("am", "vs", "cyl")) {
    xv <- pp[[nm]]$xvirtual
    for (j in seq_along(xv)) {
      got <- ct$parametric[ct$name == nm & as.character(ct$variable) == as.character(xv[j])]
      expect_equal(sort(got[!is.na(got)]), sort(pp[[nm]]$yhat.par[, j][!is.na(pp[[nm]]$yhat.par[, j])]),
                   label = sprintf("%s = %s", nm, xv[j]))
    }
  }
})

test_that("the fixture exercises the bug: some first-seen order differs from xvirtual", {
  pp <- .cat_label_fit()$pp
  differs <- vapply(c("am", "vs", "cyl"), function(nm) {
    !identical(as.character(unique(pp[[nm]]$xorg)), as.character(pp[[nm]]$xvirtual))
  }, logical(1))
  expect_true(any(differs))
})

test_that("a factor-valued categorical keeps its level order and ordered status", {
  x <- ordered(c("medium", "low", "high"), levels = c("medium", "low", "high"))
  f <- list(xorg = x, xvirtual = levels(x),
            yhat.par = matrix(1:9, 3), yhat.nonpar = matrix(1:9, 3),
            yhat.causal = matrix(1:9, 3))
  g <- gg_partial_varpro(list(stage = f))
  v <- g$categorical$variable
  expect_true(is.ordered(v))
  expect_identical(levels(v), c("medium", "low", "high"))
  ## column j of yhat.* still belongs to xvirtual[j]
  for (j in seq_along(f$xvirtual)) {
    expect_equal(g$categorical$parametric[as.character(v) == f$xvirtual[j]],
                 f$yhat.par[, j])
  }
  built <- ggplot2::ggplot_build(plot(g))
  expect_identical(built$layout$panel_params[[1L]]$x$get_labels(),
                   c("medium", "low", "high"))
})
