# Extracted from test_gg_partial_varpro_cat_labels.R:82

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
expect_s3_class(v, "factor")
expect_false(is.ordered(v))
built <- ggplot2::ggplot_build(plot(g))
labs <- lapply(built$layout$panel_params, function(p) unname(p$x$get_labels()))
expect_identical(labs, list(c("m", "l", "h"), c("z", "y", "x"),
                              c("1", "2", "10")))
