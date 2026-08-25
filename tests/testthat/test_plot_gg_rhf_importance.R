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
