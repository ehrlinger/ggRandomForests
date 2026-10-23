# Tests for gg_varpro (Phase 2: importance extractor)

## ── Helpers ──────────────────────────────────────────────────────────────────

# Regression fit — fast, always available (mtcars is base R)
make_vp_regr <- function(ntree = 25L) {
  # Supervised varpro grow (real Y) — UBSAN-clean; runs on CRAN. See
  # helper-varpro-fixtures.R for which varPro calls reach rfsrc without a
  # formula and so trip entry.c:184.
  set.seed(42L)
  varPro::varpro(mpg ~ ., data = mtcars, ntree = ntree)
}

# Classification fit — iris, always available
make_vp_class <- function(ntree = 25L) {
  set.seed(42L)
  varPro::varpro(Species ~ ., data = iris, ntree = ntree)
}

## ── Input validation ─────────────────────────────────────────────────────────

test_that("gg_varpro: missing object -> stop", {
  expect_error(gg_varpro(), regexp = "object")
})

test_that("gg_varpro: non-varpro object -> stop", {
  expect_error(gg_varpro(list(x = 1)), regexp = "varpro")
})

test_that("gg_varpro: conditional=TRUE on regression -> stop", {
  skip_on_cran()
  vp <- make_vp_regr()
  expect_error(gg_varpro(vp, conditional = TRUE),
               regexp = "classification")
})

test_that("gg_varpro: faithful=TRUE + local.std=TRUE is valid, records local.std=TRUE", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE, local.std = TRUE)
  expect_true(is.matrix(gg$imp.tree))
  expect_true(attr(gg, "provenance")$local.std)
})

## ── Class & structure ────────────────────────────────────────────────────────

test_that("gg_varpro returns gg_varpro class", {
  skip_on_cran()
  vp <- make_vp_regr()
  expect_s3_class(gg_varpro(vp), "gg_varpro")
})

test_that("gg_varpro$imp has variable, z, selected columns", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_named(gg$imp, c("variable", "z", "selected"))
  expect_s3_class(gg$imp$variable, "factor")
  expect_type(gg$imp$z, "double")
  expect_type(gg$imp$selected, "logical")
})

test_that("gg_varpro$stats has expected columns", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_true(all(c("variable", "median", "q05", "q15", "q85", "q95", "mean")
                  %in% names(gg$stats)))
})

test_that("gg_varpro$imp.tree is NULL when faithful=FALSE", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = FALSE)
  expect_null(gg$imp.tree)
})

test_that("gg_varpro$imp.tree is a matrix when faithful=TRUE", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  expect_true(is.matrix(gg$imp.tree))
})

test_that("gg_varpro$conditional is NULL when conditional=FALSE", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, conditional = FALSE)
  expect_null(gg$conditional)
})

test_that("gg_varpro$conditional has variable, class, z when conditional=TRUE", {
  skip_on_cran()
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  expect_false(is.null(gg$conditional))
  expect_true(all(c("variable", "class", "z") %in% names(gg$conditional)))
})

# varPro codes by frequency (minority = 1; a tie makes the first level the
# minority), not by level position. Check every class's z against the
# upstream column whose code the data pair with that class.
.expect_conditional_matches_codes <- function(v, gg) {
  z <- varPro::importance(v, local.std = FALSE)$conditional.z
  for (lab in levels(v$y.org)) {
    code <- unique(as.character(v$y[as.character(v$y.org) == lab]))
    expect_length(code, 1L)
    rows <- gg$conditional[gg$conditional$class == lab, ]
    expect_gt(nrow(rows), 0L)
    expect_equal(rows$z, unname(z[as.character(rows$variable), code]))
  }
}

test_that("gg_varpro conditional names binary classes, not varPro's 0/1 codes", {
  skip_on_cran()
  vb <- .varpro_iris_binary()
  # varPro recodes a binary outcome internally; y.org keeps the names.
  expect_identical(levels(vb$y), c("0", "1"))
  gg <- gg_varpro(vb, conditional = TRUE)
  expect_identical(levels(gg$conditional$class), levels(vb$y.org))
  .expect_conditional_matches_codes(vb, gg)
})


test_that("gg_varpro conditional binary: a tie codes the first level as 1", {
  skip_on_cran()
  vb <- .varpro_iris_binary()
  expect_equal(as.vector(table(vb$y.org)), c(50L, 50L))
  expect_identical(unique(as.character(vb$y[vb$y.org == "versicolor"])), "1")
  .expect_conditional_matches_codes(vb, gg_varpro(vb, conditional = TRUE))
})

test_that("gg_varpro conditional binary: a minority first level is coded 1", {
  skip_on_cran()
  set.seed(20261023L)
  d <- iris[iris$Species != "setosa", ]
  d$Species <- droplevels(d$Species)
  d <- d[c(which(d$Species == "versicolor")[1:20], which(d$Species == "virginica")), ]
  v <- varPro::varpro(Species ~ ., data = d, ntree = 30)
  expect_identical(unique(as.character(v$y[v$y.org == "versicolor"])), "1")
  .expect_conditional_matches_codes(v, gg_varpro(v, conditional = TRUE))
})

test_that("gg_varpro conditional binary: no pairing keeps varPro's codes", {
  skip_on_cran()
  vb <- .varpro_iris_binary()
  vb$y.org <- NULL
  gg <- gg_varpro(vb, conditional = TRUE)
  expect_setequal(as.character(unique(gg$conditional$class)), c("0", "1"))
})

test_that("gg_varpro conditional keeps multiclass names unchanged", {
  skip_on_cran()
  vm <- .varpro_iris_multiclass()
  gg <- gg_varpro(vm, conditional = TRUE)
  expect_identical(levels(gg$conditional$class), levels(iris$Species))
})

test_that("gg_varpro conditional leaves multiclass labels that look like codes", {
  skip_on_cran()
  skip_if_not_installed("varPro")
  set.seed(20261005L)
  d <- iris
  d$Species <- factor(as.integer(d$Species) - 1L, levels = c(2, 0, 1))
  v <- varPro::varpro(Species ~ ., data = d, ntree = 30)
  gg <- gg_varpro(v, conditional = TRUE)
  z <- varPro::importance(v, local.std = FALSE)$conditional.z
  for (k in c("2", "0", "1")) {
    rows <- gg$conditional[gg$conditional$class == k, ]
    expect_equal(rows$z, unname(z[as.character(rows$variable), k]))
  }
})

## ── Provenance attribute ─────────────────────────────────────────────────────

test_that("gg_varpro provenance has all expected fields", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("family", "local.std", "cutoff", "faithful",
                    "conditional", "xvar.names", "n") %in% names(prov)))
})

test_that("gg_varpro provenance cutoff matches argument", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 1.2)
  expect_equal(attr(gg, "provenance")$cutoff, 1.2)
})

## ── cutoff and nvar ──────────────────────────────────────────────────────────

test_that("gg_varpro$imp$selected reflects z > cutoff", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 0.79)
  expect_true(all(gg$imp$z[gg$imp$selected]  >  0.79))
  expect_true(all(gg$imp$z[!gg$imp$selected] <= 0.79))
})

test_that("gg_varpro nvar=3 returns exactly 3 variables", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, nvar = 3L)
  expect_equal(nrow(gg$imp), 3L)
  expect_equal(nrow(gg$stats), 3L)
})

test_that("gg_varpro variable factor runs least- to most-important median z", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  # Factor levels run least-important first so the most-important variable is
  # the last level (top of the plot after coord_flip). Median z therefore
  # increases along the factor levels.
  med_vals <- gg$stats$median[order(match(gg$stats$variable,
                                           levels(gg$imp$variable)))]
  expect_true(all(diff(med_vals) >= 0))
})

test_that("gg_varpro nvar larger than p returns all variables", {
  skip_on_cran()
  vp <- make_vp_regr()
  n_all <- nrow(gg_varpro(vp)$imp)
  gg <- gg_varpro(vp, nvar = 999L)
  expect_equal(nrow(gg$imp), n_all)
})

test_that("gg_varpro local.std=FALSE stats equal raw-column medians", {
  skip_on_cran()
  ## Mechanistic check: verify each path against the actual column computation.
  ## faithful=TRUE forces local.std=FALSE (coercion tested above); imp.tree
  ## gives the ground-truth raw importance matrix for both assertions.
  vp <- make_vp_regr(ntree = 50L)
  suppressMessages(gg_raw <- gg_varpro(vp, local.std = FALSE, faithful = TRUE))
  mat   <- gg_raw$imp.tree          # ntree x p raw importance matrix
  vname <- colnames(mat)[[1L]]      # pick any variable by name, not position

  ## local.std = FALSE: stats$median == median of raw column
  expect_equal(
    gg_raw$stats$median[as.character(gg_raw$stats$variable) == vname],
    stats::median(mat[, vname], na.rm = TRUE),
    tolerance = 1e-10
  )

  ## local.std = TRUE: stats$median == median of z-normalized column
  gg_z <- gg_varpro(vp, local.std = TRUE)
  sd_j <- apply(mat, 2L, stats::sd, na.rm = TRUE)
  sd_j[sd_j < .Machine$double.eps] <- 1
  z_med <- stats::median(mat[, vname] / sd_j[[vname]], na.rm = TRUE)
  expect_equal(
    gg_z$stats$median[as.character(gg_z$stats$variable) == vname],
    z_med,
    tolerance = 1e-10
  )
})

## ── Z-scale alignment ─────────────────────────────────────────────────────────

test_that("plot.gg_varpro conditional: an undefined z gets no bar, not a zero bar (#344)", {
  skip_on_cran()
  set.seed(20261014L)
  d <- iris
  for (j in 1:4) d[[paste0("noise", j)]] <- sample(d$Sepal.Width)
  set.seed(20261014L)
  vp <- varPro::varpro(Species ~ ., data = d, ntree = 30, split.weight = FALSE, mtry = 2)
  ## local.std = FALSE with a high trim leaves some class-conditional z at 0/0
  gg <- gg_varpro(vp, conditional = TRUE, local.std = FALSE, trim = 0.3)
  defined <- is.finite(gg$conditional$z)
  expect_true(any(!defined))
  ## 3.5.x plot() resolves type = "raw" for a local.std = FALSE extract and then
  ## says the conditional view ignores it; that message is not this fix.
  expect_no_warning(p <- suppressMessages(plot(gg)))
  built <- ggplot2::ggplot_build(p)
  ## one drawn bar per defined z, and no zero-height bar standing in for NA
  y <- built$data[[1L]]$y
  expect_equal(sum(is.finite(y)), sum(defined))
  expect_false(any(y[is.finite(y)] == 0))
  expect_equal(sum(built$data[[1L]]$ymax != 0, na.rm = TRUE), sum(defined))
  ## every displayed variable keeps its row, including those varPro left out
  expect_setequal(built$layout$panel_params[[1L]]$y$get_labels(),
                  levels(gg$imp$variable))
  expect_match(p$labels$caption, "z undefined")
})

test_that("plot.gg_varpro conditional: a class with no defined z keeps its facet", {
  skip_on_cran()
  set.seed(42L)
  vi <- varPro::varpro(Species ~ ., data = iris, ntree = 3, split.weight = FALSE)
  gg <- gg_varpro(vi, conditional = TRUE, local.std = FALSE, nvar = 1)
  ## Make one class's z undefined by hand. Which class a seeded ntree = 3 fit
  ## leaves undefined, if any, changes with the rfsrc RNG.
  gg$conditional$z[gg$conditional$class == "virginica"] <- NaN
  built <- ggplot2::ggplot_build(plot(gg))
  expect_setequal(as.character(built$layout$layout$class), levels(iris$Species))
})

test_that("plot.gg_varpro conditional: no undefined-z caption when every z is defined", {
  skip_on_cran()
  set.seed(42L)
  ## gg_varpro() always asks importance() for local.std = FALSE, so on this fit
  ## Sepal.Width is undefined for every class; nvar = 2 shows only the two
  ## petal variables, whose z are all defined.
  gg <- gg_varpro(make_vp_class(), conditional = TRUE, nvar = 2)
  expect_true(all(is.finite(gg$conditional$z[gg$conditional$variable %in%
                                                levels(gg$imp$variable)])))
  expect_false(grepl("undefined", plot(gg)$labels$caption))
})

test_that("gg_varpro: z-normalisation mean(z_ij) == aggregate z_j", {
  skip_on_cran()
  # varPro's importance() computes z_j = mean(imp_ij) / sd_j (no sqrt(ntree)).
  # So .varpro_imp_stats uses z_ij = imp_ij / sd_j, giving mean(z_ij) == z_j.
  # Verify by recomputing from imp.tree (faithful=TRUE).
  vp <- make_vp_regr(ntree = 50L)
  gg <- gg_varpro(vp, faithful = TRUE)
  mat <- gg$imp.tree  # ntree x p raw importance matrix
  sd_j <- apply(mat, 2L, stats::sd, na.rm = TRUE)
  sd_j[sd_j < .Machine$double.eps] <- 1
  z_mat <- sweep(mat, 2L, sd_j, FUN = "/")  # z_ij = imp_ij / sd_j
  z_means <- colMeans(z_mat, na.rm = TRUE)
  # Column means should equal aggregate z from importance()
  agg_z <- setNames(gg$imp$z, as.character(gg$imp$variable))
  common <- intersect(names(z_means), names(agg_z))
  expect_gt(length(common), 0L)
  expect_equal(z_means[common], agg_z[common], tolerance = 0.1,
               info = "mean per-tree z should approximate aggregate z within 0.1")
})

## ── Plot smoke tests ─────────────────────────────────────────────────────────

test_that("plot.gg_varpro default returns a ggplot", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='z' returns a ggplot", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)  # local.std=TRUE default
  p  <- plot(gg, type = "z")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=FALSE returns a ggplot", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = FALSE)
  p  <- plot(gg, type = "raw")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=TRUE -> stop", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = TRUE)
  expect_error(plot(gg, type = "raw"), regexp = "local\\.std")
})

## ── Regression: no phantom "NA" category when nvar < p ───────────────────────
## gg_varpro(nvar = k) truncates $imp/$stats to k variables, but $imp.tree (the
## per-tree matrix) and $conditional still carry every variable. Re-levelling
## those to the truncated $imp levels orphaned the extras to NA, which rendered
## as an empty "NA" box/bar. plot.gg_varpro must drop them.

## The `variable` scale is mapped to x (aes(x = variable)); coord_flip() flips
## the rendering, not the aesthetic-to-scale mapping, so the variable categories
## are always the trained x-scale range. Reading them there needs no guessing
## about which rendered axis is discrete, and the phantom NA appears directly as
## an NA category in that range.
.varpro_variable_cats <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
}

test_that("plot.gg_varpro faithful: no phantom 'NA' variable when nvar < p", {
  skip_on_cran()
  # imp.tree keeps every variable, so nvar = 3 (< 10 predictors) orphans the
  # rest -- they would collapse into an NA category without the fix.
  vp   <- make_vp_regr()
  cats <- .varpro_variable_cats(plot(gg_varpro(vp, faithful = TRUE, nvar = 3L)))
  expect_gt(length(cats), 0L)
  expect_false(anyNA(cats))
})

test_that("plot.gg_varpro conditional: no phantom 'NA' variable when nvar < p", {
  skip_on_cran()
  # nvar = 1 truncates below the conditional table's variable count, which is
  # what orphans a variable to NA in the buggy path.
  vp   <- make_vp_class()
  cats <- .varpro_variable_cats(plot(gg_varpro(vp, conditional = TRUE, nvar = 1L)))
  expect_gt(length(cats), 0L)
  expect_false(anyNA(cats))
})

test_that("plot.gg_varpro type='z' with local.std=FALSE -> stop", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = FALSE)
  expect_error(plot(gg, type = "z"), regexp = "local\\.std")
})

test_that("plot.gg_varpro faithful=TRUE returns a ggplot", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro conditional=TRUE returns ggplot with FacetWrap", {
  skip_on_cran()
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})

## ── S3 companions ────────────────────────────────────────────────────────────

test_that("autoplot.gg_varpro returns a ggplot", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})

test_that("print.gg_varpro returns object invisibly", {
  skip_on_cran()
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp)
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_varpro", out)))
})

test_that("print.gg_varpro output contains selected/total counts", {
  skip_on_cran()
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp, cutoff = 0.79)
  out <- capture.output(print(gg))
  expect_true(any(grepl("selected", out, ignore.case = TRUE)))
})

test_that("summary.gg_varpro returns summary.gg_varpro class", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_varpro")
})

test_that(".varpro_binary_code_map returns NULL unless the pairing is one-to-one", {
  map <- ggRandomForests:::.varpro_binary_code_map
  ok <- list(y.org = factor(c("a", "b", "b")), y = factor(c("1", "0", "0")))
  expect_identical(map(ok), c("0" = "b", "1" = "a"))
  ## a code paired with two labels
  expect_null(map(list(y.org = factor(c("a", "b", "a")),
                       y     = factor(c("1", "1", "0")))))
  ## two codes paired with one label
  expect_null(map(list(y.org = factor(c("a", "a")), y = factor(c("0", "1")))))
  ## lengths differ, or a side is missing
  expect_null(map(list(y.org = factor("a"), y = factor(c("0", "1")))))
  expect_null(map(list(y = factor(c("0", "1")))))
})
