test_that("gg_beta_varpro returns the expected tidy shape", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)

  expect_s3_class(out, "gg_beta_varpro")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out),
                  c("variable", "beta_mean", "n_rules", "selected"))
  released <- unique(b$results$variable[is.finite(b$results$imp)])
  expect_equal(nrow(out), length(released))
})

test_that("gg_beta_varpro aggregation equals mean(|imp|) per variable", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  for (i in seq_len(min(2L, nrow(out)))) {
    var_name <- as.character(out$variable[i])
    var_idx  <- match(var_name, b$xvar.names)
    expected <- mean(abs(res$imp[res$variable == var_idx]))
    expect_equal(out$beta_mean[i], expected, tolerance = 1e-10,
                 label = paste0("beta_mean for ", var_name))
  }
})

test_that("gg_beta_varpro now accepts classification family (regr + class supported)", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  expect_s3_class(out, "gg_beta_varpro")
  expect_equal(attr(out, "provenance")$family, "class")
})

test_that("gg_beta_varpro cutoff = 0 selects everything, Inf selects nothing", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  zero_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = 0)
  expect_true(all(zero_cut$selected))

  inf_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = Inf)
  expect_true(!any(inf_cut$selected))
})

test_that("gg_beta_varpro default cutoff is mean(beta_mean), provenance flagged", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)
  prov <- attr(out, "provenance")

  expect_equal(prov$cutoff[["regr"]], mean(out$beta_mean), tolerance = 1e-10)
  expect_true(prov$cutoff_default)

  explicit <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.123)
  expect_false(attr(explicit, "provenance")$cutoff_default)
  expect_equal(attr(explicit, "provenance")$cutoff[["regr"]], 0.123)
})

test_that("gg_beta_varpro rejects malformed beta_fit", {
  v <- .varpro_mtcars()
  expect_error(
    gg_beta_varpro(v, beta_fit = list()),
    "beta_fit does not look like a varPro::beta.varpro\\(\\) result"
  )

  b <- .beta_fit_mtcars()
  b_bad <- b
  b_bad$results <- b_bad$results[, setdiff(names(b_bad$results), "imp"), drop = FALSE]
  expect_error(
    gg_beta_varpro(v, beta_fit = b_bad),
    "imp"
  )
})

test_that("gg_beta_varpro warns when ... is supplied alongside beta_fit", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  expect_warning(
    out <- gg_beta_varpro(v, use.cv = TRUE, beta_fit = b),
    "ignored because beta_fit is supplied"
  )
  expect_s3_class(out, "gg_beta_varpro")
})

test_that("gg_beta_varpro returns empty frame when beta.varpro yields no rules", {
  v <- .varpro_mtcars()
  empty <- structure(
    list(results = data.frame(tree = integer(0), branch = integer(0),
                              variable = integer(0), n.oob = integer(0),
                              imp = numeric(0)),
         xvar.names = v$xvar.names),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = empty)
  expect_s3_class(out, "gg_beta_varpro")
  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), c("variable", "beta_mean", "n_rules", "selected"))
  expect_equal(attr(out, "provenance")$n_rules_total, 0L)
})

test_that("gg_beta_varpro counts lasso-shrunk-to-zero rules in n_rules", {
  v <- .varpro_mtcars()
  fake <- structure(
    list(
      results = data.frame(
        tree     = c(1L, 1L, 2L, 2L),
        branch   = c(1L, 2L, 1L, 2L),
        variable = c(1L, 1L, 1L, 1L),
        n.oob    = c(5L, 5L, 5L, 5L),
        imp      = c(0.0, 0.0, 1.5, -2.5)
      ),
      xvar.names = v$xvar.names
    ),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = fake)
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_rules, 4L)
  # mean(|0|, |0|, |1.5|, |-2.5|) = 1.0
  expect_equal(out$beta_mean, 1.0, tolerance = 1e-10)
})

test_that("gg_beta_varpro cached and uncached paths agree", {
  if (!identical(Sys.getenv("GG_BETA_VARPRO_SLOW_TESTS", "true"), "true")) {
    skip("Slow test — set GG_BETA_VARPRO_SLOW_TESTS=true to run")
  }
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()

  set.seed(20260526L)
  uncached <- gg_beta_varpro(v)
  cached   <- gg_beta_varpro(v, beta_fit = b)

  # Strip provenance before comparing the tidy-frame content — `precomputed`
  # differs (TRUE vs FALSE) between the two paths by design, and that flag is
  # checked separately below.
  strip <- function(x) {
    attr(x, "provenance") <- NULL
    as.data.frame(x)
  }
  expect_equal(
    strip(uncached),
    strip(cached),
    tolerance = 1e-10
  )
  expect_false(attr(uncached, "provenance")$precomputed)
  expect_true(attr(cached, "provenance")$precomputed)
})

test_that("plot.gg_beta_varpro returns a ggplot that builds", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  p <- plot(out)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_true(length(built$data) >= 1L)
})

test_that("gg_beta_varpro S3 companions return the expected shapes", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  # print invisibly returns x
  pr <- withVisible(print(out))
  expect_false(pr$visible)
  expect_identical(pr$value, out)

  # summary returns a named numeric, sorted descending, attribute n_rules present
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_varpro")
  expect_type(unclass(s), "double")
  # as.numeric() strips both names and the n_rules attr so the comparison is
  # value-only; sort() drops the n_rules attr and would otherwise cause a
  # spurious attribute-mismatch failure.
  vals <- as.numeric(unclass(s))
  expect_equal(vals, sort(vals, decreasing = TRUE))
  expect_equal(length(s), nrow(out))
  expect_true(!is.null(attr(s, "n_rules")))

  # autoplot delegates to plot
  p1 <- plot(out)
  p2 <- ggplot2::autoplot(out)
  expect_equal(
    ggplot2::ggplot_build(p1)$data,
    ggplot2::ggplot_build(p2)$data
  )
})

# ---- Classification: shape + factor ordering --------------------------------

test_that("gg_beta_varpro binary classification returns long-format with class", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  expect_s3_class(out, "gg_beta_varpro")
  expect_true("class" %in% names(out))
  # Binary default: which_class = last factor level (positive class)
  expect_equal(as.character(unique(out$class)),
               tail(levels(droplevels(iris$Species[iris$Species != "setosa"])), 1))
})

test_that("gg_beta_varpro multi-class returns all classes faceted by default", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  expect_true("class" %in% names(out))
  expect_setequal(as.character(unique(out$class)), levels(iris$Species))
})

test_that("gg_beta_varpro variable is a factor ordered by total-|imp| descending", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  expect_true(is.factor(out$variable))

  # Compute expected order independently
  res <- bm$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  var_name <- bm$xvar.names[res$variable]
  agg <- vapply(split(abs(res$imp), var_name), mean, numeric(1))
  # Factor levels are reversed-descending so the most-important variable is
  # the LAST level (top of the plot after coord_flip).
  expected_levels <- rev(names(sort(agg, decreasing = TRUE)))
  expect_equal(levels(out$variable), expected_levels)
})

# ---- which_class semantics --------------------------------------------------

test_that("gg_beta_varpro which_class = NULL on binary resolves to last factor level", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  prov <- attr(out, "provenance")
  expect_equal(prov$which_class, "virginica")
})

test_that("gg_beta_varpro which_class explicit returns single panel", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm, which_class = "setosa")
  expect_equal(as.character(unique(out$class)), "setosa")
  expect_equal(attr(out, "provenance")$which_class, "setosa")
})

test_that("gg_beta_varpro which_class not in levels errors with levels listed", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  expect_error(
    gg_beta_varpro(vm, beta_fit = bm, which_class = "nope"),
    "is not a level of the response"
  )
})

# ---- Aggregation correctness ------------------------------------------------

test_that("gg_beta_varpro classification beta_mean equals mean(|imp.k|) per (var, class)", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)

  # Pick the first (variable, class) pair
  first_var   <- as.character(out$variable[1])
  first_class <- as.character(out$class[1])
  k_idx       <- match(first_class, levels(vm$y))
  imp_col     <- paste0("imp.", k_idx)
  v_idx       <- match(first_var, bm$xvar.names)

  vals     <- bm$results[[imp_col]][bm$results$variable == v_idx]
  expected <- mean(abs(vals[is.finite(vals)]))
  expect_equal(out$beta_mean[1], expected, tolerance = 1e-10)
})

# ---- cutoff polymorphism ----------------------------------------------------

test_that("gg_beta_varpro cutoff = NULL gives per-class mean", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  prov <- attr(out, "provenance")
  expect_type(prov$cutoff, "double")
  expect_named(prov$cutoff, levels(iris$Species))
  for (cls in levels(iris$Species)) {
    expect_equal(
      prov$cutoff[[cls]],
      mean(out$beta_mean[out$class == cls]),
      tolerance = 1e-10
    )
  }
  expect_true(prov$cutoff_default)
})

test_that("gg_beta_varpro cutoff = scalar broadcasts across classes", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm, cutoff = 0.5)
  prov <- attr(out, "provenance")
  expect_equal(unname(prov$cutoff), rep(0.5, 3))
  expect_named(prov$cutoff, levels(iris$Species))
  expect_false(prov$cutoff_default)
})

test_that("gg_beta_varpro cutoff = named vector respects per-class, falls back to mean", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm,
                       cutoff = c("setosa" = 1.5))   # only one named; others default
  prov <- attr(out, "provenance")
  expect_equal(prov$cutoff[["setosa"]], 1.5)
  expect_equal(prov$cutoff[["versicolor"]],
               mean(out$beta_mean[out$class == "versicolor"]), tolerance = 1e-10)
})

test_that("gg_beta_varpro cutoff = named vector with unknown name errors", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  expect_error(
    gg_beta_varpro(vm, beta_fit = bm, cutoff = c("bogus" = 0.5)),
    "is not a level of the response"
  )
})

# ---- Phase 4c provenance shape update --------------------------------------

test_that("gg_beta_varpro regression cutoff is now a length-1 named numeric", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, "regr")
  expect_length(prov$cutoff, 1L)
  expect_equal(prov$cutoff[["regr"]], mean(out$beta_mean), tolerance = 1e-10)
})

# ---- which_class warning on regression --------------------------------------

test_that("gg_beta_varpro which_class on regression warns and is ignored", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  expect_warning(
    out <- gg_beta_varpro(v, beta_fit = b, which_class = "anything"),
    "ignored for regression family"
  )
  expect_s3_class(out, "gg_beta_varpro")
  expect_false("class" %in% names(out))
})

# ---- Plot: binary single panel + multi-class faceted -----------------------

test_that("plot.gg_beta_varpro binary builds (single panel)", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_beta_varpro multi-class builds (faceted) with per-class cutoff lines", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # facet_wrap engaged -> multiple panels in layout
  expect_true(nrow(built$layout$layout) >= 2L)
})

# ---- Print + summary: classification ---------------------------------------

test_that("print.gg_beta_varpro classification surfaces class info", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  txt <- utils::capture.output(print(out))
  expect_true(any(grepl("n_classes", txt) | grepl("class", txt)))
  expect_true(any(grepl("faceted", txt)))
})

test_that("summary.gg_beta_varpro classification returns per-class list", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_varpro")
  expect_true(is.list(unclass(s)))
  expect_setequal(names(unclass(s)), levels(iris$Species))
})

# ---- Copilot review fixes (PR #98) -----------------------------------------

test_that("summary on a which_class-filtered classification only lists that class", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm, which_class = "setosa")
  s <- summary(out)
  expect_equal(names(unclass(s)), "setosa")
})

test_that("beta_fit shape guard catches missing per-class imp.<k> columns", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  bm_bad <- bm
  bm_bad$results <- bm_bad$results[, setdiff(names(bm_bad$results), "imp.2"),
                                   drop = FALSE]
  expect_error(
    gg_beta_varpro(vm, beta_fit = bm_bad),
    "Missing per-class column"
  )
})

test_that("empty classification fast-path returns named cutoff vector", {
  vm <- .varpro_iris_multiclass()
  empty_bm <- structure(
    list(
      results = data.frame(
        tree     = integer(0), branch = integer(0),
        variable = integer(0), n.oob  = integer(0),
        imp      = numeric(0),
        imp.1    = numeric(0), imp.2 = numeric(0), imp.3 = numeric(0)
      ),
      xvar.names = vm$xvar.names
    ),
    class = "varpro"
  )
  out <- gg_beta_varpro(vm, beta_fit = empty_bm)
  expect_s3_class(out, "gg_beta_varpro")
  expect_equal(nrow(out), 0L)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, levels(iris$Species))
  expect_true(all(is.na(prov$cutoff)))
  expect_equal(prov$class_levels, levels(iris$Species))
})
