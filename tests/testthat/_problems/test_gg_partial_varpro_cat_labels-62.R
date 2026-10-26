# Extracted from test_gg_partial_varpro_cat_labels.R:62

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
x <- ordered(c("medium", "low", "high"), levels = c("medium", "low", "high"))
f <- list(xorg = x, xvirtual = levels(x),
            yhat.par = matrix(1:9, 3), yhat.nonpar = matrix(1:9, 3),
            yhat.causal = matrix(1:9, 3))
g <- gg_partial_varpro(list(stage = f))
v <- g$categorical$variable
expect_true(is.ordered(v))
expect_identical(levels(v), c("medium", "low", "high"))
for (j in seq_along(f$xvirtual)) {
    expect_equal(g$categorical$parametric[as.character(v) == f$xvirtual[j]],
                 f$yhat.par[, j])
  }
built <- ggplot2::ggplot_build(plot(g))
expect_identical(unname(built$layout$panel_params[[1L]]$x$get_labels()),
                   c("medium", "low", "high"))
