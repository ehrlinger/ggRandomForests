# Tests for gg_varpro (Phase 2: importance extractor)

## ── Helpers ──────────────────────────────────────────────────────────────────

# Regression fit — fast, always available (mtcars is base R)
make_vp_regr <- function(ntree = 25L) {
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
  vp <- make_vp_regr()
  expect_error(gg_varpro(vp, conditional = TRUE),
               regexp = "classification")
})

test_that("gg_varpro: survival fit -> clear 'not supported yet' stop", {
  # gg_varpro extracts regr/class families; a survival fit must fail with a
  # clear message rather than the cryptic "differing number of rows" cbind
  # error from the downstream importance reshape. (varPro survival support is
  # deferred; for survival importance use gg_vimp() on an rfsrc forest.)
  skip_if_not_installed("survival")
  # bare Surv(); parseFormula rejects the survival::Surv(...) namespace form
  Surv <- survival::Surv # nolint: object_name_linter
  data(pbc, package = "randomForestSRC")
  set.seed(42L)
  vp_surv <- varPro::varpro(Surv(days, status) ~ .,
                            data = na.omit(pbc), ntree = 25L)
  expect_error(gg_varpro(vp_surv), regexp = "survival")
})

test_that("gg_varpro: faithful=TRUE + local.std=TRUE is valid, records local.std=TRUE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE, local.std = TRUE)
  expect_true(is.matrix(gg$imp.tree))
  expect_true(attr(gg, "provenance")$local.std)
})

## ── Class & structure ────────────────────────────────────────────────────────

test_that("gg_varpro returns gg_varpro class", {
  vp <- make_vp_regr()
  expect_s3_class(gg_varpro(vp), "gg_varpro")
})

test_that("gg_varpro$imp has variable, z, selected columns", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_named(gg$imp, c("variable", "z", "selected"))
  expect_s3_class(gg$imp$variable, "factor")
  expect_type(gg$imp$z, "double")
  expect_type(gg$imp$selected, "logical")
})

test_that("gg_varpro$stats has expected columns", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_true(all(c("variable", "median", "q05", "q15", "q85", "q95", "mean")
                  %in% names(gg$stats)))
})

test_that("gg_varpro$imp.tree is NULL when faithful=FALSE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = FALSE)
  expect_null(gg$imp.tree)
})

test_that("gg_varpro$imp.tree is a matrix when faithful=TRUE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  expect_true(is.matrix(gg$imp.tree))
})

test_that("gg_varpro$conditional is NULL when conditional=FALSE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, conditional = FALSE)
  expect_null(gg$conditional)
})

test_that("gg_varpro$conditional has variable, class, z when conditional=TRUE", {
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  expect_false(is.null(gg$conditional))
  expect_true(all(c("variable", "class", "z") %in% names(gg$conditional)))
})

## ── Provenance attribute ─────────────────────────────────────────────────────

test_that("gg_varpro provenance has all expected fields", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("family", "local.std", "cutoff", "faithful",
                    "conditional", "xvar.names", "n") %in% names(prov)))
})

test_that("gg_varpro provenance cutoff matches argument", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 1.2)
  expect_equal(attr(gg, "provenance")$cutoff, 1.2)
})

## ── cutoff and nvar ──────────────────────────────────────────────────────────

test_that("gg_varpro$imp$selected reflects z > cutoff", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 0.79)
  expect_true(all(gg$imp$z[gg$imp$selected]  >  0.79))
  expect_true(all(gg$imp$z[!gg$imp$selected] <= 0.79))
})

test_that("gg_varpro nvar=3 returns exactly 3 variables", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, nvar = 3L)
  expect_equal(nrow(gg$imp), 3L)
  expect_equal(nrow(gg$stats), 3L)
})

test_that("gg_varpro variable factor runs least- to most-important median z", {
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
  vp <- make_vp_regr()
  n_all <- nrow(gg_varpro(vp)$imp)
  gg <- gg_varpro(vp, nvar = 999L)
  expect_equal(nrow(gg$imp), n_all)
})

test_that("gg_varpro local.std=FALSE stats equal raw-column medians", {
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

  ## local.std = TRUE: stats$median == median of z-normalised column
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

test_that("gg_varpro: z-normalisation mean(z_ij) == aggregate z_j", {
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
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='z' returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)  # local.std=TRUE default
  p  <- plot(gg, type = "z")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=FALSE returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = FALSE)
  p  <- plot(gg, type = "raw")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=TRUE -> stop", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = TRUE)
  expect_error(plot(gg, type = "raw"), regexp = "local\\.std")
})

test_that("plot.gg_varpro type='z' with local.std=FALSE -> stop", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = FALSE)
  expect_error(plot(gg, type = "z"), regexp = "local\\.std")
})

test_that("plot.gg_varpro faithful=TRUE returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro conditional=TRUE returns ggplot with FacetWrap", {
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})

## ── S3 companions ────────────────────────────────────────────────────────────

test_that("autoplot.gg_varpro returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})

test_that("print.gg_varpro returns object invisibly", {
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp)
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_varpro", out)))
})

test_that("print.gg_varpro output contains selected/total counts", {
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp, cutoff = 0.79)
  out <- capture.output(print(gg))
  expect_true(any(grepl("selected", out, ignore.case = TRUE)))
})

test_that("summary.gg_varpro returns summary.gg_varpro class", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_varpro")
})
