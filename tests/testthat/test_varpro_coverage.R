# varpro_coverage(): per-term and per-variable coverage of a varpro fit.
# Two supervised mtcars fits with a factor and two permuted noise columns:
# guided (default split weights, which cut the noise before rule-building)
# and unguided (every term eligible, so some noise is tested).

.coverage_fits <- function() {
  if (is.null(.varpro_cache$cov)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20261014L)
    d <- mtcars
    d$cyl <- factor(d$cyl)
    d$noise_wt <- sample(d$wt)
    d$noise_cyl <- sample(d$cyl)
    set.seed(20261014L)
    guided <- varPro::varpro(mpg ~ ., data = d, ntree = 50)
    set.seed(20261014L)
    unguided <- varPro::varpro(mpg ~ ., data = d, ntree = 50, split.weight = FALSE, mtry = 3)
    .varpro_cache$cov <- list(guided = guided, unguided = unguided)
  }
  .varpro_cache$cov
}

test_that("term table has the documented shape and settings", {
  vp <- .coverage_fits()$guided
  set.seed(1)
  cv <- varpro_coverage(vp)
  expect_s3_class(cv, "varpro_coverage")
  expect_s3_class(cv, "data.frame")
  expect_named(cv, c("term", "variable", "eligible", "n_trees", "coverage", "n_rules",
                     "z", "tested"))
  expect_setequal(cv$term, colnames(vp$x))
  expect_identical(attr(cv, "by"), "term")
  expect_identical(attr(cv, "trees"), vp$rf$ntree)
  expect_identical(attr(cv, "trim"), 0.1)
  expect_equal(cv$coverage, cv$n_trees / vp$rf$ntree)
  expect_identical(cv$tested, cv$coverage >= 0.1)
  expect_false(is.unsorted(rev(cv$coverage)))
})

test_that("ineligible terms are listed with no rules and no z", {
  vp <- .coverage_fits()$guided
  set.seed(1)
  cv <- varpro_coverage(vp)
  expect_setequal(cv$term[cv$eligible], vp$xvar.names)
  out <- cv[!cv$eligible, ]
  expect_true(all(out$n_trees == 0L & out$n_rules == 0L & is.na(out$z)))
})

test_that("z agrees with importance() scored on the same trees", {
  vp <- .coverage_fits()$unguided
  set.seed(1)
  cv <- varpro_coverage(vp)
  set.seed(1)
  imp <- varPro::importance(vp, plot.it = FALSE, max.tree = vp$rf$ntree)
  expect_equal(cv$z[match(rownames(imp), cv$term)], imp$z)
  expect_true(all(is.na(cv$z[!cv$term %in% rownames(imp)])))
})

test_that("variable rows roll factor levels up", {
  vp <- .coverage_fits()$unguided
  set.seed(1)
  tm <- varpro_coverage(vp)
  set.seed(1)
  vr <- varpro_coverage(vp, by = "variable")
  expect_named(vr, c("variable", "n_terms", "eligible", "n_trees", "coverage", "n_rules",
                     "z", "tested"))
  expect_setequal(vr$variable, vp$xvar.org.names)
  cyl <- vr[vr$variable == "cyl", ]
  cyl_terms <- tm[tm$variable == "cyl", ]
  expect_identical(cyl$n_terms, nrow(cyl_terms))
  expect_identical(cyl$n_rules, sum(cyl_terms$n_rules))
  expect_gte(cyl$n_trees, max(cyl_terms$n_trees))
  expect_lte(cyl$n_trees, sum(cyl_terms$n_trees))
  expect_equal(cyl$z, max(cyl_terms$z, na.rm = TRUE))
})

test_that("a fixed seed reproduces the table", {
  vp <- .coverage_fits()$unguided
  set.seed(7)
  a <- varpro_coverage(vp, seed = -7L)
  set.seed(7)
  b <- varpro_coverage(vp, seed = -7L)
  expect_identical(a, b)
})

test_that("noise columns set a cutoff by kind", {
  vp <- .coverage_fits()$unguided
  set.seed(1)
  cv <- varpro_coverage(vp, by = "variable", noise = "^noise")
  expect_true(all(c("noise", "kind", "noise_cut", "selected") %in% names(cv)))
  expect_identical(cv$kind[cv$variable == "cyl"], "categorical")
  expect_identical(cv$kind[cv$variable == "disp"], "continuous")
  expect_false(any(cv$selected & cv$noise))
  expect_true(all(!cv$selected[!cv$tested]))
})

test_that("the noise cutoff ignores untested noise and falls back across kinds", {
  out <- data.frame(
    variable = c("a", "b", "noise_a", "noise_b"),
    z        = c(0.9, 0.2, 0.15, 0.00),
    tested   = c(TRUE, TRUE, TRUE, FALSE),
    noise    = c(FALSE, FALSE, TRUE, TRUE),
    kind     = c("continuous", "categorical", "continuous", "categorical"))
  ## categorical noise is untested, so categorical rows fall back to 0.15
  expect_identical(.varpro_coverage_cut(out), rep(0.15, 4))
  out$tested[4] <- TRUE
  out$z[4] <- 0.05
  expect_identical(.varpro_coverage_cut(out), c(0.15, 0.05, 0.15, 0.05))
})

test_that("noise warnings name the problem", {
  fits <- .coverage_fits()
  set.seed(1)
  expect_warning(varpro_coverage(fits$guided, noise = "^nothing_matches"),
                 "no variable name matches")
  set.seed(1)
  expect_warning(varpro_coverage(fits$guided, noise = "^noise"),
                 "no noise variable was tested")
})

test_that("bad inputs fail early", {
  vp <- .coverage_fits()$guided
  expect_error(varpro_coverage(list()), "must be a varpro fit")
  expect_error(varpro_coverage(vp, trim = 0.6), "'trim' must be")
  expect_error(varpro_coverage(vp, by = "tree"))
})
