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

test_that("gg_varpro$imp has variable, z, selected, coverage, tested columns", {
  skip_on_cran()
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_named(gg$imp, c("variable", "z", "selected", "coverage", "tested"))
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

test_that("gg_varpro conditional names binary classes, not varPro's 0/1 codes", {
  skip_on_cran()
  vb <- .varpro_iris_binary()
  # varPro recodes a binary outcome internally; y.org keeps the names.
  expect_identical(levels(vb$y), c("0", "1"))
  gg <- gg_varpro(vb, conditional = TRUE, local.std = FALSE)
  expect_identical(levels(gg$conditional$class), levels(vb$y.org))
  # The recode is positional: code 0 is the first level of y.org.
  z0 <- varPro::importance(vb, local.std = FALSE)$conditional.z[, "0"]
  first <- gg$conditional[gg$conditional$class == levels(vb$y.org)[1], ]
  expect_equal(first$z, unname(z0[as.character(first$variable)]))
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
  gg <- gg_varpro(v, conditional = TRUE, local.std = FALSE)
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
  set.seed(42L)
  vp <- make_vp_regr(ntree = 50L)
  suppressMessages(gg_raw <- gg_varpro(vp, local.std = FALSE, faithful = TRUE))
  mat   <- gg_raw$imp.tree          # ntree x p raw importance matrix
  vname <- colnames(mat)[[1L]]      # pick any variable by name, not position
  expect_equal(
    gg_raw$stats$median[as.character(gg_raw$stats$variable) == vname],
    stats::median(mat[, vname], na.rm = TRUE),
    tolerance = 1e-10
  )
})

## ── Same trees for boxes, z and coverage (#340) ──────────────────────────────

test_that("gg_varpro: default z is varPro::importance()'s default z", {
  skip_on_cran()
  vp <- make_vp_regr(ntree = 50L)
  set.seed(1L)
  gg <- gg_varpro(vp)
  set.seed(1L)
  imp <- varPro::importance(vp, plot.it = FALSE)
  z <- stats::setNames(gg$imp$z, as.character(gg$imp$variable))
  expect_equal(unname(z[rownames(imp)]), imp$z)
})

test_that("gg_varpro: local.std=FALSE z is varPro::importance(local.std=FALSE)'s z", {
  skip_on_cran()
  set.seed(42L)
  vp <- make_vp_regr(ntree = 50L)
  gg <- gg_varpro(vp, local.std = FALSE)
  imp <- varPro::importance(vp, local.std = FALSE, plot.it = FALSE)
  imp <- imp[is.finite(imp$z), , drop = FALSE]
  z <- stats::setNames(gg$imp$z, as.character(gg$imp$variable))
  expect_equal(unname(z[rownames(imp)]), imp$z)
})

test_that("gg_varpro: z is the winsorised mean of the boxed per-tree values", {
  skip_on_cran()
  set.seed(42L)
  vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50L, max.tree = 20L)
  for (args in list(list(), list(local.std = FALSE), list(local.std = FALSE, max.tree = 50L))) {
    set.seed(7L)
    gg <- do.call(gg_varpro, c(list(vp, faithful = TRUE), args))
    mat <- gg$imp.tree
    v <- as.character(gg$imp$variable)
    wm <- apply(mat[, v, drop = FALSE], 2L, varPro:::winsorize.mean, trim = 0.1)
    if (isFALSE(args$local.std)) {
      wm <- wm / apply(mat[, v, drop = FALSE], 2L, varPro:::winsorize.sd, trim = 0.1)
    }
    ok <- is.finite(gg$imp$z)
    expect_equal(gg$imp$z[ok], unname(wm[ok]))
  }
  ## max.tree reaches the rules the boxes are drawn from, not only z
  expect_identical(attr(gg, "provenance")$max.tree, 50L)
  expect_gt(nrow(mat), vp$max.tree)
})

test_that("gg_varpro: coverage and tested flag untested variables", {
  skip_on_cran()
  set.seed(20261014L)
  d <- mtcars
  for (j in 1:6) d[[paste0("noise", j)]] <- sample(d$wt)
  set.seed(20261014L)
  vp <- varPro::varpro(mpg ~ ., data = d, ntree = 50, split.weight = FALSE, mtry = 3)
  set.seed(3L)
  gg <- gg_varpro(vp)
  expect_true(all(c("coverage", "tested") %in% names(gg$imp)))
  expect_true(all(c("coverage", "tested") %in% names(gg$stats)))
  expect_true(all(gg$imp$coverage >= 0 & gg$imp$coverage <= 1))
  expect_identical(gg$imp$tested, gg$imp$coverage >= 0.1)
  ## local.std = FALSE: an untested variable's z is 0/0, reported as NA, not 0
  set.seed(3L)
  gg_f <- gg_varpro(vp, local.std = FALSE, trim = 0.3)
  expect_true(all(is.na(gg_f$imp$z) <= !gg_f$imp$tested))
  expect_false(any(gg_f$imp$selected & is.na(gg_f$imp$z)))
  p <- plot(gg_f)
  expect_s3_class(p, "ggplot")
  if (!all(gg_f$stats$tested)) expect_match(p$labels$caption, "untested")
})

test_that("gg_varpro: arguments varPro does not take are an error, not ignored", {
  skip_on_cran()
  set.seed(42L)
  vp <- make_vp_regr()
  expect_error(gg_varpro(vp, y.external = 1), "unused argument")
  expect_error(gg_varpro(vp, trim = 0.6), "trim")
})

test_that("plot.gg_varpro conditional: no type message unless type was passed", {
  skip_on_cran()
  set.seed(42L)
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE, local.std = FALSE)
  expect_silent(plot(gg))
  expect_message(plot(gg, type = "raw"), "ignores type")
})

test_that("gg_varpro: a fit without its forest needs local.std = FALSE", {
  skip_on_cran()
  set.seed(42L)
  vp <- make_vp_regr()
  vp$rf <- NULL
  expect_error(gg_varpro(vp), "no forest")
  expect_error(gg_varpro(vp, local.std = FALSE, max.tree = 10L), "no forest")
  expect_s3_class(gg_varpro(vp, local.std = FALSE), "gg_varpro")
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

test_that("plot.gg_varpro labels the variable axis", {
  skip_if_not(exists("make_mock_gg_varpro"), "no gg_varpro fixture available")
  obj <- make_mock_gg_varpro()
  p <- plot(obj, labels = c(bpd = "BP Diastole"))
  built <- ggplot2::ggplot_build(p)
  expect_true(any(grepl("BP Diastole",
                        unlist(lapply(built$layout$panel_params,
                                      function(pp) as.character(pp$y$get_labels()))))))
})

test_that("plot.gg_varpro conditional labels the variable axis", {
  skip_if_not(exists("make_mock_gg_varpro"), "no gg_varpro fixture available")
  # Regression test: conditional path must honour labels argument, not ignore it.
  # When x$conditional is non-NULL, lab_lookup is computed and passed to
  # .plot_varpro_conditional(), which applies scale_x_discrete labelling.
  obj <- make_mock_gg_varpro(conditional = TRUE)
  p <- plot(obj, labels = c(bpd = "BP Diastole"))
  built <- ggplot2::ggplot_build(p)
  # Conditional plots facet by class; each panel has a discrete x scale.
  # After coord_flip(), variable categories are on the y-scale.
  labels_all <- unlist(lapply(built$layout$panel_params,
                              function(pp) as.character(pp$y$get_labels())))
  expect_true(any(grepl("BP Diastole", labels_all)))
})
