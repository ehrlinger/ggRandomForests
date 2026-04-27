# Regression tests for the empty-figure bug class.
#
# A `plot.gg_*()` method can return a ggplot whose data frame has zero rows,
# or whose layer data computes to zero rows after aesthetic mapping. That
# produces a figure with axes but no points or lines — which is what users
# saw in the survival vignette when the data transform's column names did
# not match what the plot method expected.
#
# These tests use `ggplot2::layer_data()` to inspect what each plot would
# actually draw. layer_data() runs the full ggplot build pipeline and returns
# the post-mapping data frame, so a zero-row result here means the figure
# would render empty.

Surv <- survival::Surv  # nolint: object_name_linter

# Helper: number of rows ggplot2 would actually render for a given layer.
expect_layer_nonempty <- function(p, layer = 1L, label = NULL) {
  testthat::expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p, layer)
  testthat::expect_true(
    nrow(ld) > 0,
    info = sprintf(
      "%slayer %d data should have rows (got %d). cols: %s",
      if (is.null(label)) "" else paste0(label, ": "),
      layer, nrow(ld), paste(colnames(ld), collapse = ",")
    )
  )
  invisible(ld)
}

# Helper: every aesthetic in `mapping_keys` should resolve to a column with
# more than one unique value somewhere in the layer data. The empty-figure
# bug presented as columns mapped to axis labels because the data frame had
# only one literal value per "variable"/"value"/etc. column.
expect_layer_has_variation <- function(p, layer = 1L, mapping_keys) {
  ld <- ggplot2::layer_data(p, layer)
  for (k in mapping_keys) {
    testthat::expect_true(
      k %in% colnames(ld),
      info = sprintf("layer %d missing aesthetic '%s'", layer, k)
    )
    testthat::expect_gt(
      length(unique(ld[[k]])), 1,
      label = sprintf("variation in layer %d aes '%s'", layer, k)
    )
  }
  invisible(ld)
}

# ----------------------------------------------------------------------------
# gg_rfsrc — survival
# ----------------------------------------------------------------------------
test_that("plot.gg_rfsrc survival (no CI) renders many step curves", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)

  gg <- gg_rfsrc(rf)
  expect_true(all(c("variable", "value", "obs_id", "event") %in% colnames(gg)))
  expect_type(gg$variable, "double")
  expect_type(gg$value, "double")

  p <- plot(gg)
  expect_layer_nonempty(p)
  expect_layer_has_variation(p, mapping_keys = c("x", "y"))
})

test_that("plot.gg_rfsrc survival (CI) renders ribbon + median curve", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  gg <- gg_rfsrc(rf, conf.int = .95)
  expect_true(all(c("value", "lower", "upper", "median", "mean") %in% colnames(gg)))
  p <- plot(gg)
  # Layer 1 is the ribbon, layer 2 is the median step line.
  expect_layer_nonempty(p, layer = 1L)
  expect_layer_nonempty(p, layer = 2L)
})

test_that("plot.gg_rfsrc survival (by group) renders per-group curves", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  gg <- gg_rfsrc(rf, by = "trt")
  expect_true("group" %in% colnames(gg))
  p <- plot(gg)
  ld <- expect_layer_nonempty(p)
  expect_gt(length(unique(ld$group)), 1)
})

# ----------------------------------------------------------------------------
# gg_rfsrc — regression
# ----------------------------------------------------------------------------
test_that("plot.gg_rfsrc regression renders jitter + boxplot with data", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                               na.action = "na.impute", ntree = 50)
  gg <- gg_rfsrc(rf)
  expect_true(all(c("yhat", "Ozone") %in% colnames(gg)))
  p <- plot(gg)
  ld <- expect_layer_nonempty(p)
  expect_equal(nrow(ld), nrow(gg))
  expect_gt(length(unique(ld$y)), 1)
})

# ----------------------------------------------------------------------------
# gg_rfsrc — classification
# ----------------------------------------------------------------------------
test_that("plot.gg_rfsrc multi-class renders one row per (obs, class)", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  gg <- gg_rfsrc(rf)
  # Multi-class: one column per class plus "y"
  expect_true(all(levels(iris$Species) %in% colnames(gg)))
  expect_true("y" %in% colnames(gg))
  p <- plot(gg)
  ld <- expect_layer_nonempty(p)
  # Three classes × 150 observations => 450 rows after pivot
  expect_equal(nrow(ld), nrow(iris) * nlevels(iris$Species))
})

# ----------------------------------------------------------------------------
# gg_partial_rfsrc — survival regression test for the partial.type fix
# ----------------------------------------------------------------------------
test_that("gg_partial_rfsrc survival passes partial.type and produces data", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  ti  <- rf$time.interest
  t90 <- ti[which.min(abs(ti - 90))]

  # Single time point.
  expect_no_error({
    pd <- gg_partial_rfsrc(rf, xvar.names = "age",
                           partial.time = t90, n_eval = 8)
  })
  expect_s3_class(pd, "gg_partial_rfsrc")
  expect_true(nrow(pd$continuous) > 0)
  expect_true(all(c("x", "yhat", "name", "time") %in% colnames(pd$continuous)))
  # Survival plot path expects a "time" column.
  p <- plot(pd)
  expect_layer_nonempty(p)
})

test_that("gg_partial_rfsrc survival multi-time expands to long form", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  ti  <- rf$time.interest
  t30 <- ti[which.min(abs(ti - 30))]
  t90 <- ti[which.min(abs(ti - 90))]

  pd <- gg_partial_rfsrc(rf, xvar.names = "age",
                         partial.time = c(t30, t90), n_eval = 8)
  # Two time points × evaluation grid: long form, each (x, time) one row.
  expect_equal(length(unique(pd$continuous$time)), 2L)
  per_time <- table(pd$continuous$time)
  expect_true(all(per_time == per_time[[1]]))

  p <- plot(pd)
  ld <- expect_layer_nonempty(p)
  # The fixed plot maps time → colour, so we should see two groups.
  expect_gte(length(unique(ld$colour)), 2)
})

test_that("gg_partial_rfsrc partial.type rejects bad values", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  expect_error(
    gg_partial_rfsrc(rf, xvar.names = "age", partial.type = "bogus"),
    regexp = "should be one of"
  )
})

test_that("plot.gg_partial_rfsrc y-axis adapts to partial.type", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  ti  <- rf$time.interest
  t90 <- ti[which.min(abs(ti - 90))]

  # Default ("surv") => "Predicted Survival"
  pd_s <- gg_partial_rfsrc(rf, xvar.names = "age",
                           partial.time = t90, n_eval = 6)
  expect_equal(attr(pd_s, "partial.type"), "surv")
  p_s <- plot(pd_s)
  expect_equal(p_s$labels$y, "Predicted Survival")

  # "chf" => "Predicted CHF"
  pd_c <- gg_partial_rfsrc(rf, xvar.names = "age",
                           partial.time = t90, partial.type = "chf",
                           n_eval = 6)
  expect_equal(attr(pd_c, "partial.type"), "chf")
  p_c <- plot(pd_c)
  expect_equal(p_c$labels$y, "Predicted CHF")

  # "mort" => "Predicted Mortality"; mort returns one value per x (no time dim),
  # so the survival "time" branch will not engage — we only assert the
  # attribute round-trips.
  pd_m <- gg_partial_rfsrc(rf, xvar.names = "age",
                           partial.type = "mort",
                           n_eval = 6)
  expect_equal(attr(pd_m, "partial.type"), "mort")
})

test_that("plot.gg_partial_rfsrc preserves full-precision time grouping", {
  # Distinct times that round to the same 2-dp value must not collapse into a
  # single line. Build a synthetic gg_partial_rfsrc object with two such times.
  cont <- data.frame(
    x    = rep(c(1, 2, 3), times = 2),
    yhat = c(0.9, 0.8, 0.7, 0.5, 0.4, 0.3),
    name = "x",
    time = rep(c(1.001, 1.002), each = 3)
  )
  obj <- structure(
    list(
      continuous  = cont,
      categorical = data.frame(x = character(0), yhat = numeric(0),
                               name = character(0), time = integer(0))
    ),
    class = "gg_partial_rfsrc",
    partial.type = "surv"
  )
  p  <- plot(obj)
  ld <- ggplot2::layer_data(p, 1L)
  # Two distinct full-precision time horizons => two distinct groups, even
  # though both round to "1" at 2-dp precision.
  expect_equal(length(unique(ld$group)), 2L)
  expect_equal(length(unique(ld$colour)), 2L)
})

# ----------------------------------------------------------------------------
# gg_partial_rfsrc — regression
# ----------------------------------------------------------------------------
test_that("plot.gg_partial_rfsrc regression renders a non-empty line plot", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  pd <- gg_partial_rfsrc(rf, xvar.names = "Wind", n_eval = 8)
  p <- plot(pd)
  expect_layer_nonempty(p)
})

# ----------------------------------------------------------------------------
# gg_error
# ----------------------------------------------------------------------------
test_that("plot.gg_error single-outcome renders points or a line", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                               na.action = "na.impute",
                               ntree = 50, tree.err = TRUE,
                               block.size = 5)
  gg <- gg_error(rf)
  expect_true(all(c("ntree", "error") %in% colnames(gg)))
  p <- plot(gg)
  expect_layer_nonempty(p)
})

test_that("plot.gg_error multi-class pivots and colours by class", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris,
                               ntree = 50, tree.err = TRUE,
                               block.size = 5)
  p <- plot(gg_error(rf))
  ld <- expect_layer_nonempty(p)
  expect_gte(length(unique(ld$colour)), 2)
})

# ----------------------------------------------------------------------------
# gg_vimp
# ----------------------------------------------------------------------------
test_that("plot.gg_vimp renders one bar per variable", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                               na.action = "na.impute", ntree = 50,
                               importance = TRUE)
  p <- plot(gg_vimp(rf))
  ld <- expect_layer_nonempty(p)
  expect_equal(nrow(ld), length(rf$xvar.names))
})

test_that("plot.gg_vimp produces a single merged 'VIMP > 0' legend", {
  # Regression: previously the bar geom mapped both `fill` and `color` to
  # `positive` but only the fill legend got a custom title via labs(). ggplot
  # rendered two legends -- one labelled "VIMP > 0" (fill) and one labelled
  # "positive" (column name). Setting matching titles on both aesthetics
  # merges them into a single legend.
  #
  # We build a synthetic gg_vimp object rather than fit a real forest so the
  # test is deterministic across platforms (CI runners regularly produced
  # all-positive VIMP for the PBC dataset, which would skip the fill branch
  # we want to exercise here).
  gg <- structure(
    data.frame(
      vars     = c("a", "b", "c", "d"),
      vimp     = c(0.20, 0.05, -0.01, -0.04),
      rel_vimp = c(1.00, 0.25, 0.05,  0.20),
      positive = c(TRUE, TRUE, FALSE, FALSE)
    ),
    class = c("gg_vimp", "data.frame")
  )

  # Sanity: both signs present.
  expect_true(length(unique(gg$positive)) > 1)

  p <- plot(gg)
  expect_equal(p$labels$fill,   "VIMP > 0")
  expect_equal(p$labels$colour, "VIMP > 0")
  # The two aesthetic legend titles must match for ggplot to merge them.
  expect_identical(p$labels$fill, p$labels$colour)
})

test_that("plot.gg_vimp produces filled bars even when all VIMP are positive", {
  # Regression: previously the all-positive branch mapped only `color`,
  # leaving the bars hollow / outline-only and emitting "Ignoring unknown
  # labels: fill : 'VIMP > 0'" because the function-level labs(fill = ...)
  # had nothing to bind to. The fix maps fill + color unconditionally so the
  # bars are always filled and the legend title applies cleanly.
  gg <- structure(
    data.frame(
      vars     = c("a", "b", "c"),
      vimp     = c(0.30, 0.10, 0.05),
      rel_vimp = c(1.00, 0.33, 0.17),
      positive = c(TRUE, TRUE, TRUE)
    ),
    class = c("gg_vimp", "data.frame")
  )
  expect_equal(length(unique(gg$positive)), 1L)

  warns <- character()
  p <- withCallingHandlers(
    plot(gg),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("Ignoring unknown labels", warns)),
               info = sprintf("got warnings: %s", paste(warns, collapse = " | ")))

  ld <- ggplot2::layer_data(p, 1L)
  # Bars must be filled, not hollow.
  expect_true("fill" %in% colnames(ld))
  expect_true(all(!is.na(ld$fill)))
})

# ----------------------------------------------------------------------------
# gg_variable
# ----------------------------------------------------------------------------
test_that("plot.gg_variable survival single xvar has a point per observation", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Surv(time, status) ~ ., data = veteran,
                               ntree = 50)
  gg <- gg_variable(rf, time = 90)
  expect_true(all(c("event", "yhat", "time") %in% colnames(gg)))
  p <- plot(gg, xvar = "age")
  ld <- expect_layer_nonempty(p)
  expect_equal(nrow(ld), nrow(veteran))
})

test_that("plot.gg_variable regression renders a scatter with smooth", {
  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                               na.action = "na.impute", ntree = 50)
  gg <- gg_variable(rf)
  p <- plot(gg, xvar = "Wind", smooth = FALSE)
  expect_layer_nonempty(p)
})
