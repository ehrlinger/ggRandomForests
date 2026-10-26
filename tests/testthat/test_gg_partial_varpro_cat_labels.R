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
  expect_identical(as.character(built$layout$panel_params[[1L]]$x$get_labels()),
                   c("medium", "low", "high"))
})

test_that("categoricals of mixed factor types bind, each in its own level order", {
  mk <- function(x) {
    list(xorg = x, xvirtual = levels(x) %||% sort(unique(x)),
         yhat.par = matrix(1:9, 3), yhat.nonpar = matrix(1:9, 3),
         yhat.causal = matrix(1:9, 3))
  }
  stage <- mk(ordered(c("m", "l", "h"), levels = c("m", "l", "h")))
  grp   <- mk(factor(c("z", "y", "x"), levels = c("z", "y", "x")))
  code  <- mk(c(10, 2, 1))
  g <- gg_partial_varpro(list(stage = stage, grp = grp, code = code))
  v <- g$categorical$variable
  ## an ordered factor cannot bind with an unordered one; this used to error
  expect_s3_class(v, "factor")
  expect_false(is.ordered(v))
  built <- ggplot2::ggplot_build(plot(g))
  labs <- lapply(built$layout$panel_params, function(p) as.character(p$x$get_labels()))
  expect_identical(labs, list(c("m", "l", "h"), c("z", "y", "x"),
                              c("1", "2", "10")))
  ## two ordered factors stay ordered
  g2 <- gg_partial_varpro(list(stage = stage,
                               s2 = mk(ordered(c("b", "a", "c"),
                                               levels = c("b", "a", "c")))))
  expect_true(is.ordered(g2$categorical$variable))
})

test_that("a continuous variable ahead of ordered categoricals keeps them ordered", {
  cont <- list(xorg = seq(0, 1, length.out = 20), xvirtual = seq(0, 1, length.out = 20),
               yhat.par = matrix(1:60, 3), yhat.nonpar = matrix(1:60, 3),
               yhat.causal = matrix(1:60, 3))
  stage <- list(xorg = ordered(c("m", "l", "h"), levels = c("m", "l", "h")),
                xvirtual = c("m", "l", "h"),
                yhat.par = matrix(1:9, 3), yhat.nonpar = matrix(1:9, 3),
                yhat.causal = matrix(1:9, 3))
  g <- gg_partial_varpro(list(age = cont, stage = stage))
  expect_gt(nrow(g$continuous), 0L)
  expect_true(is.ordered(g$categorical$variable))
  expect_identical(levels(g$categorical$variable), c("m", "l", "h"))
})

test_that("overlapping categorical labels keep each panel's own order", {
  mk <- function(x, lv) {
    list(xorg = factor(x, levels = lv), xvirtual = lv,
         yhat.par = matrix(1:6, 3), yhat.nonpar = matrix(1:6, 3),
         yhat.causal = matrix(1:6, 3))
  }
  ## pooled by first occurrence these would be 2, 3, 1, and the second panel
  ## would read 2, 1; a conflicting factor order would also be overridden
  g <- gg_partial_varpro(list(a = mk(c(2, 3, 2), c("2", "3")),
                              b = mk(c(1, 2, 1), c("1", "2")),
                              c = mk(c("y", "x", "y"), c("y", "x")),
                              d = mk(c("x", "y", "x"), c("x", "y"))))
  built <- ggplot2::ggplot_build(plot(g))
  labs <- lapply(built$layout$panel_params, function(p) as.character(p$x$get_labels()))
  expect_identical(unname(labs), list(c("2", "3"), c("1", "2"), c("y", "x"),
                                      c("x", "y")))
})
