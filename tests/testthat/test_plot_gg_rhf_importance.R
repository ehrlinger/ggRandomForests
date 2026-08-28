.rhf_priority_test_object <- function() {
  f <- .fake_rhf_importance()
  gg_rhf_importance(f$object, importance_fit = f$fit)
}

test_that("plot.gg_rhf_importance returns the published point matrix", {
  x <- .rhf_priority_test_object()
  original <- x$priority
  p <- plot(x, top_n_union = NULL)

  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(layer) {
    class(layer$geom)[1L]
  }, character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_equal(x$priority, original)
  expect_equal(p$labels$size, "RHF variable priority")
  expect_equal(p$labels$colour, "RHF variable priority")
  expect_equal(levels(p$data$time_window), c("(0, 1]", "(1, 2]"))
})

test_that("plot.gg_rhf_importance lets geom_point arguments override defaults", {
  p <- plot(.rhf_priority_test_object(), alpha = 0.4)

  expect_equal(p$layers[[1L]]$aes_params$alpha, 0.4)
})

test_that("plot.gg_rhf_importance keeps the highest q90 variable on top", {
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL)

  expect_equal(tail(levels(p$data$variable), 1L), "x1")
})

test_that("plot.gg_rhf_importance filters explicit variables", {
  p <- plot(.rhf_priority_test_object(), vars = c("x3", "x1"))

  expect_setequal(as.character(unique(p$data$variable)), c("x1", "x3"))
})

test_that("plot.gg_rhf_importance uses the per-window top-variable union", {
  x <- .rhf_priority_test_object()
  top <- plot(x, top_n_union = 1L)
  all <- plot(x, top_n_union = NULL)

  expect_setequal(as.character(unique(top$data$variable)), c("x1", "x2"))
  expect_setequal(as.character(unique(all$data$variable)),
                  c("x1", "x2", "x3"))
})

test_that("plot.gg_rhf_importance transforms display values only", {
  x <- .rhf_priority_test_object()
  original <- x$priority
  p <- plot(x, top_n_union = NULL, transform = "log10",
            size_cap = 1, color_cap = 1)

  expect_equal(p$data$display_priority, log10(original + 1))
  expect_equal(x$priority, original)
})

test_that("plot.gg_rhf_importance reports applied display caps", {
  x <- .rhf_priority_test_object()
  original <- x$priority
  p <- plot(x, top_n_union = NULL, size_cap = 0.5, color_cap = 0.5)
  cap <- unname(stats::quantile(original, 0.5, names = FALSE))

  expect_lte(max(p$data$size_display), cap)
  expect_lte(max(p$data$color_display), cap)
  expect_match(p$labels$caption, "Display only")
  expect_match(p$labels$caption, "size capped at q50")
  expect_match(p$labels$caption, "color capped at q50")
  expect_equal(x$priority, original)

  quiet <- plot(x, top_n_union = NULL, size_cap = 0.5, color_cap = 0.5,
                display_note = FALSE)
  expect_null(quiet$labels$caption)
})

test_that("plot.gg_rhf_importance retains zeros and omits missing scores", {
  x <- .rhf_priority_test_object()
  x$priority[1L] <- 0
  x$priority[2L] <- NA_real_
  p <- plot(x, top_n_union = NULL)

  expect_true(0 %in% p$data$priority)
  expect_false(anyNA(p$data$priority))
  expect_equal(nrow(p$data), nrow(x) - 1L)
})

test_that("plot.gg_rhf_importance validates variable filters", {
  x <- .rhf_priority_test_object()

  expect_error(plot(x, vars = "other"), "Unknown RHF priority variables")
  expect_error(plot(x, vars = character()), "nonempty character vector")
  expect_error(plot(x, vars = 1), "nonempty character vector")
  expect_error(plot(x, vars = NA_character_), "nonempty character vector")
})

test_that("plot.gg_rhf_importance validates top-union and display controls", {
  x <- .rhf_priority_test_object()

  expect_error(plot(x, top_n_union = 0), "positive integer")
  expect_error(plot(x, top_n_union = 1.5), "positive integer")
  expect_error(plot(x, top_n_union = c(1, 2)), "positive integer")
  expect_error(plot(x, top_n_union = "1"), "positive integer")
  expect_error(plot(x, transform = "sqrt"), "arg")
  expect_error(plot(x, size_cap = 0), "size_cap.*\\(0, 1\\]")
  expect_error(plot(x, size_cap = NA_real_), "size_cap.*\\(0, 1\\]")
  expect_error(plot(x, color_cap = 1.1), "color_cap.*\\(0, 1\\]")
  expect_error(plot(x, color_cap = "1"), "color_cap.*\\(0, 1\\]")
})

test_that("plot.gg_rhf_importance rejects wrong and empty inputs", {
  expect_error(
    plot.gg_rhf_importance(data.frame()),
    "requires a 'gg_rhf_importance' object"
  )

  x <- .rhf_priority_test_object()
  x$priority[] <- NA_real_
  expect_error(plot(x, top_n_union = NULL), "No finite RHF priority values")
})

## ---- labels= on the variable axis -----------------------------------------
## Unlike the other importance methods, this one draws variables on y directly
## (no coord_flip), so the labelled scale is scale_y_discrete.

.rhf_priority_axis_labels <- function(p) {
  built <- ggplot2::ggplot_build(p)
  unlist(lapply(built$layout$panel_params,
                function(pp) as.character(pp$y$get_labels())))
}

test_that("plot.gg_rhf_importance labels the variable axis", {
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL,
            labels = c(x1 = "Serum bilirubin"))

  expect_true("Serum bilirubin" %in% .rhf_priority_axis_labels(p))
})

test_that("plot.gg_rhf_importance falls back to the raw name per variable", {
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL,
            labels = c(x1 = "Serum bilirubin"))
  axis <- .rhf_priority_axis_labels(p)

  expect_true(all(c("x2", "x3") %in% axis))
  expect_false("x1" %in% axis)
})

test_that("plot.gg_rhf_importance accepts a labelled data frame", {
  d <- data.frame(x1 = 1:2, x2 = 3:4)
  attr(d$x1, "label") <- "Serum bilirubin"
  attr(d$x2, "label") <- "Prothrombin time"
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL, labels = d)

  expect_true(all(c("Serum bilirubin", "Prothrombin time") %in%
                    .rhf_priority_axis_labels(p)))
})

test_that("plot.gg_rhf_importance accepts a key/label data frame", {
  m <- data.frame(key = "x1", label = "Serum bilirubin",
                  stringsAsFactors = FALSE)
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL, labels = m)

  expect_true("Serum bilirubin" %in% .rhf_priority_axis_labels(p))
})

test_that("plot.gg_rhf_importance with labels = NULL keeps the raw names", {
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL)

  expect_setequal(.rhf_priority_axis_labels(p), c("x1", "x2", "x3"))
})

test_that("plot.gg_rhf_importance warns once when no label resolves", {
  expect_warning(
    plot(.rhf_priority_test_object(), top_n_union = NULL,
         labels = c(x1 = "")),
    "No variable labels"
  )
})

test_that("plot.gg_rhf_importance keeps raw names in the returned data", {
  # Labels are a display concern: the plot's data must still carry the raw
  # variable names, so downstream code reading p$data is unaffected.
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL,
            labels = c(x1 = "Serum bilirubin"))

  expect_setequal(as.character(unique(p$data$variable)), c("x1", "x2", "x3"))
})

test_that("plot.gg_rhf_importance preserves importance order under labels", {
  # The q90 ordering lives in the factor levels; relabelling the scale must
  # not disturb it. Highest-ranked variable stays the last level (top of axis).
  p <- plot(.rhf_priority_test_object(), top_n_union = NULL,
            labels = c(x1 = "Serum bilirubin"))

  expect_equal(tail(levels(p$data$variable), 1L), "x1")
  expect_equal(tail(.rhf_priority_axis_labels(p), 1L), "Serum bilirubin")
})

test_that("autoplot.gg_rhf_importance forwards labels to the plot method", {
  p <- ggplot2::autoplot(.rhf_priority_test_object(), top_n_union = NULL,
                         labels = c(x1 = "Serum bilirubin"))

  expect_true("Serum bilirubin" %in% .rhf_priority_axis_labels(p))
})
