# Extracted from test_gg_partial_varpro_cat_labels.R:119

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
mk <- function(x, lv) {
    list(xorg = factor(x, levels = lv), xvirtual = lv,
         yhat.par = matrix(1:6, 3), yhat.nonpar = matrix(1:6, 3),
         yhat.causal = matrix(1:6, 3))
  }
g <- gg_partial_varpro(list(a = mk(c(2, 3, 2), c("2", "3")),
                              b = mk(c(1, 2, 1), c("1", "2")),
                              c = mk(c("y", "x", "y"), c("y", "x")),
                              d = mk(c("x", "y", "x"), c("x", "y"))))
built <- ggplot2::ggplot_build(plot(g))
labs <- lapply(built$layout$panel_params, function(p) unname(p$x$get_labels()))
expect_identical(unname(labs), list(c("2", "3"), c("1", "2"), c("y", "x"),
                                      c("x", "y")))
