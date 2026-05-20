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

test_that("gg_varpro: faithful=TRUE + local.std=TRUE coerces local.std with message", {
  vp <- make_vp_regr()
  expect_message(
    gg <- gg_varpro(vp, faithful = TRUE, local.std = TRUE),
    regexp = "local.std"
  )
  # provenance must record resolved FALSE
  expect_false(attr(gg, "provenance")$local.std)
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

test_that("gg_varpro variable factor ordered by descending median z", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  med_vals <- gg$stats$median[order(match(gg$stats$variable,
                                           levels(gg$imp$variable)))]
  expect_true(all(diff(med_vals) <= 0))
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
  expect_equal(z_means[common], agg_z[common], tolerance = 0.5,
               info = "mean per-tree z should approximate aggregate z within 0.5")
})
