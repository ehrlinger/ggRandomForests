# Tests for gg_isopro() — varPro::isopro tidy-data wrapper.
# Phase 4 of the v2.8.0 varPro integration.

make_iso_fit <- function(seed = 1L, method = "rnd", ntree = 25, sampsize = 16) {
  # Only the *unsupervised* isopro grow (method = "unsupv") trips
  # randomForestSRC's gcc-UBSAN report at entry.c:184 (length-0 yvar.wt
  # decremented to an out-of-bounds pointer; upstream bug, ggRandomForests is
  # pure R). The default method = "rnd" builds a synthetic-supervised forest and
  # is UBSAN-clean — verified under -fsanitize=undefined. So skip_on_cran() only
  # the unsupervised path; the rnd tests run on CRAN. Both run in CI and locally.
  if (identical(method, "unsupv")) testthat::skip_on_cran()
  testthat::skip_if_not_installed("varPro")
  set.seed(seed)
  varPro::isopro(
    data     = iris[, 1:4],
    method   = method,
    sampsize = sampsize,
    ntree    = ntree
  )
}

test_that("gg_isopro: returns gg_isopro data.frame with correct columns", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_s3_class(gg, "gg_isopro")
  expect_s3_class(gg, "data.frame")
  expect_named(gg, c("obs", "case.depth", "howbad"), ignore.order = TRUE)
})

test_that("gg_isopro: row count equals n; howbad in [0,1]; obs is 1..n", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_equal(nrow(gg), nrow(iris))
  expect_true(all(gg$howbad >= 0 & gg$howbad <= 1))
  expect_equal(gg$obs, seq_len(nrow(iris)))
})

test_that("gg_isopro: provenance attribute attached", {
  fit  <- make_iso_fit(ntree = 25)
  gg   <- gg_isopro(fit)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$source, "varPro::isopro")
  expect_equal(prov$n, nrow(iris))
  expect_equal(prov$ntree, 25)
})

test_that("plot.gg_isopro: panel='both' returns patchwork with 2 sub-plots that build", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "both")
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1L) # left panel is `p` itself; right is in $patches$plots
  for (sub in c(list(p), p$patches$plots)) {
    expect_no_error(ggplot2::ggplot_build(sub))
  }
})

test_that("plot.gg_isopro: panel='elbow' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "elbow")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: panel='density' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "density")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: rejects bad panel values via match.arg", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_error(plot(gg, panel = "nope"))
})

# Helper: count geom_hline / geom_vline layers in a ggplot or patchwork.
# Note: a patchwork object is itself the "left" plot (still inherits "ggplot"),
# while siblings live in $patches$plots. Strip the patchwork class before
# counting layers on the top-level object to avoid infinite recursion.
.count_ref_lines <- function(p) {
  count_one <- function(plt) {
    sum(vapply(plt$layers, function(l) {
      inherits(l$geom, "GeomHline") || inherits(l$geom, "GeomVline")
    }, logical(1L)))
  }
  if (inherits(p, "patchwork")) {
    siblings <- p$patches$plots
    top <- p
    class(top) <- setdiff(class(top), "patchwork")
    return(count_one(top) + sum(vapply(siblings, count_one, integer(1L))))
  }
  count_one(p)
}

test_that("plot.gg_isopro: threshold adds a reference line on each panel", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p_none <- plot(gg, panel = "both")
  p_thr  <- plot(gg, panel = "both", threshold = 0.8)
  # panel = both has elbow + density => one hline + one vline = 2 ref lines.
  expect_equal(.count_ref_lines(p_none), 0L)
  expect_equal(.count_ref_lines(p_thr), 2L)
})

test_that("plot.gg_isopro: top_n_pct resolves to the matching quantile", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  q95 <- as.numeric(stats::quantile(gg$howbad, 0.95))
  p   <- plot(gg, panel = "elbow", top_n_pct = 5)
  # Find the hline; check its yintercept equals q95.
  yints <- vapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomHline")) l$data$yintercept else NA_real_
  }, numeric(1L))
  yints <- yints[!is.na(yints)]
  expect_length(yints, 1L)
  expect_equal(yints[[1]], q95, tolerance = 1e-9)
})

test_that("plot.gg_isopro: threshold + top_n_pct both set => message, threshold wins", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_message(
    p <- plot(gg, panel = "elbow", threshold = 0.7, top_n_pct = 5),
    "Both .* using `threshold`"
  )
  yints <- vapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomHline")) l$data$yintercept else NA_real_
  }, numeric(1L))
  yints <- yints[!is.na(yints)]
  expect_equal(yints[[1]], 0.7, tolerance = 1e-9)
})

test_that("plot.gg_isopro: a method column triggers colour grouping in the elbow", {
  fit_rnd <- make_iso_fit(seed = 1L, method = "rnd")
  fit_uns <- make_iso_fit(seed = 1L, method = "unsupv")
  gg <- rbind(
    cbind(gg_isopro(fit_rnd), method = "rnd"),
    cbind(gg_isopro(fit_uns), method = "unsupv")
  )
  class(gg) <- c("gg_isopro", "data.frame")

  p <- plot(gg, panel = "elbow")
  expect_s3_class(p, "ggplot")
  # Built scales should include a colour scale because `method` is mapped.
  built <- ggplot2::ggplot_build(p)
  scales <- built$plot$scales$scales
  has_colour <- any(vapply(scales, function(s) "colour" %in% s$aesthetics, logical(1L)))
  # Even when ggplot2 hasn't materialised a discrete colour scale into
  # $scales, the layer's aes mapping must include `colour = method`.
  layer_aes <- p$layers[[1]]$mapping
  expect_true("colour" %in% names(layer_aes) || has_colour)
})

test_that("print.gg_isopro: prints a one-line header, invisibly", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  txt <- utils::capture.output(out <- print(gg))
  expect_identical(out, gg)               # returns invisibly
  expect_length(txt, 1L)                  # exactly one line
  expect_match(txt, "gg_isopro")
})

test_that("summary.gg_isopro: returns summary.gg with quantile snapshot in body", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  s   <- summary(gg)
  expect_s3_class(s, "summary.gg")
  expect_true(any(grepl("rows", s$body)))
  expect_true(any(grepl("howbad", s$body)))
})

test_that("autoplot.gg_isopro: same return as plot.gg_isopro for the default args", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_s3_class(autoplot(gg), "patchwork")
})

test_that("plot.gg_isopro: threshold must be numeric scalar in [0,1]", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_error(plot(gg, threshold = "high"),  "single numeric value in \\[0, 1\\]")
  expect_error(plot(gg, threshold = c(0.5, 0.8)), "single numeric value in \\[0, 1\\]")
  expect_error(plot(gg, threshold = 1.5), "single numeric value in \\[0, 1\\]")
})

test_that("plot.gg_isopro: top_n_pct must be numeric scalar in (0,100)", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_error(plot(gg, top_n_pct = "5"),  "single numeric value in \\(0, 100\\)")
  expect_error(plot(gg, top_n_pct = 0),    "single numeric value in \\(0, 100\\)")
  expect_error(plot(gg, top_n_pct = 100),  "single numeric value in \\(0, 100\\)")
  expect_error(plot(gg, top_n_pct = 150),  "single numeric value in \\(0, 100\\)")
})

## ── predict.isopro path (PR for Phase 4b) ──────────────────────────────────

test_that("gg_isopro: newdata returns gg_isopro data.frame with correct columns", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit, newdata = iris[, 1:4])
  expect_s3_class(gg, "gg_isopro")
  expect_s3_class(gg, "data.frame")
  expect_named(gg, c("obs", "case.depth", "howbad"), ignore.order = TRUE)
  expect_equal(nrow(gg), nrow(iris))
  expect_equal(gg$obs, seq_len(nrow(iris)))
  expect_true(all(gg$howbad >= 0 & gg$howbad <= 1))
})

test_that("gg_isopro: newdata provenance carries prediction = TRUE and right n", {
  fit  <- make_iso_fit(ntree = 25)
  test_df <- iris[c(1:10, 51:60, 101:110), 1:4]  # 30 rows
  gg   <- gg_isopro(fit, newdata = test_df)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$source, "varPro::isopro")
  expect_equal(prov$n, nrow(test_df))
  expect_equal(prov$ntree, 25)
  expect_true(isTRUE(prov$prediction))
})

test_that("gg_isopro: training-path provenance has no prediction field set TRUE", {
  fit  <- make_iso_fit()
  gg   <- gg_isopro(fit)
  prov <- attr(gg, "provenance")
  expect_false(isTRUE(prov$prediction))
})

test_that("gg_isopro: newdata must be a data.frame", {
  fit <- make_iso_fit()
  expect_error(gg_isopro(fit, newdata = "not a df"),
               "newdata must be a data.frame")
  expect_error(gg_isopro(fit, newdata = 1:10),
               "newdata must be a data.frame")
  expect_error(gg_isopro(fit, newdata = list(a = 1)),
               "newdata must be a data.frame")
})

test_that("gg_isopro: scoring the training set as newdata matches training howbad in range and top-5 ordering", {
  fit       <- make_iso_fit()
  train_df  <- iris[, 1:4]
  gg_train  <- gg_isopro(fit)
  gg_pred   <- gg_isopro(fit, newdata = train_df)
  # The two code paths inside varPro are slightly different so byte equality
  # is too strong, but the score range and the most-anomalous rows should agree.
  expect_equal(nrow(gg_pred), nrow(gg_train))
  expect_true(all(gg_pred$howbad  >= 0 & gg_pred$howbad  <= 1))
  expect_true(all(gg_train$howbad >= 0 & gg_train$howbad <= 1))
  top_train <- head(order(-gg_train$howbad), 5)
  top_pred  <- head(order(-gg_pred$howbad),  5)
  # At least 3 of the top-5 anomalous rows should overlap between the two paths.
  expect_gte(length(intersect(top_train, top_pred)), 3L)
})

test_that("gg_isopro: howbad == 1 - predict(quantiles = TRUE)", {
  fit     <- make_iso_fit()
  test_df <- iris[1:30, 1:4]
  gg      <- gg_isopro(fit, newdata = test_df)
  q_raw   <- as.numeric(stats::predict(fit, newdata = test_df, quantiles = TRUE))
  expect_equal(gg$howbad, 1 - q_raw, tolerance = 1e-12)
})

test_that("gg_isopro: bind_rows(train, test) plots without error via the method-column path", {
  fit      <- make_iso_fit()
  test_df  <- iris[seq(1, nrow(iris), by = 3), 1:4]  # 50 rows
  gg_train <- gg_isopro(fit)
  gg_test  <- gg_isopro(fit, newdata = test_df)
  gg_both  <- rbind(
    cbind(as.data.frame(gg_train), method = "train"),
    cbind(as.data.frame(gg_test),  method = "test")
  )
  class(gg_both) <- c("gg_isopro", "data.frame")
  p <- plot(gg_both, panel = "both")
  expect_s3_class(p, "patchwork")
  # Each sub-plot must actually build (catches aes/data mismatches the way the
  # original plot.gg_variable regression test did).
  built_top <- ggplot2::ggplot_build(p)
  expect_true(inherits(built_top, "ggplot_built"))
  for (sub in p$patches$plots) {
    expect_no_error(ggplot2::ggplot_build(sub))
  }
})
