# Smoke tests for print.gg_* / summary.gg_* methods.
# We fit one forest per family and exercise every method's happy path.

setup_forests <- function() {
  data(iris,     package = "datasets")
  data(airquality, package = "datasets")
  data(pbc,      package = "randomForestSRC")
  set.seed(1)
  list(
    cls = randomForestSRC::rfsrc(Species ~ ., data = iris,
                                 ntree = 50, importance = TRUE,
                                 tree.err = TRUE),
    reg = randomForestSRC::rfsrc(Ozone ~ ., data = airquality,
                                 ntree = 50, na.action = "na.impute",
                                 importance = TRUE, tree.err = TRUE),
    srv = randomForestSRC::rfsrc(Surv(days, status) ~ ., data = pbc,
                                 ntree = 50, nsplit = 10,
                                 importance = TRUE, tree.err = TRUE)
  )
}

test_that("provenance attribute is attached by every constructor", {
  f <- setup_forests()
  expect_equal(attr(gg_error(f$cls),    "provenance")$source, "randomForestSRC")
  expect_equal(attr(gg_vimp(f$reg),     "provenance")$source, "randomForestSRC")
  expect_equal(attr(gg_rfsrc(f$srv),    "provenance")$source, "randomForestSRC")
  expect_equal(attr(gg_variable(f$reg), "provenance")$source, "randomForestSRC")
  expect_equal(attr(gg_roc(f$cls, which_outcome = 1),
                    "provenance")$source, "randomForestSRC")
  expect_equal(attr(gg_brier(f$srv),    "provenance")$source, "randomForestSRC")
  # gg_survival from a forest object should carry provenance.
  expect_equal(attr(gg_survival(f$srv), "provenance")$source, "randomForestSRC")
  # gg_partial objects come from plot.variable output, not a forest directly;
  # they do not carry provenance — confirm the attribute is absent (not NA).
  part_dta <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                              xvar.names = "Wind")
  gp <- gg_partial(part_dta)
  expect_null(attr(gp, "provenance"))
})

test_that("print methods return their input invisibly and emit a header", {
  f <- setup_forests()

  # Header-only contract: exactly one newline-terminated line.
  expect_print_header <- function(obj) {
    out <- capture.output(print(obj))
    expect_length(out, 1L)
    expect_true(nchar(out) > 0)
  }

  expect_print_header(gg_error(f$cls))
  expect_print_header(gg_vimp(f$cls))
  expect_print_header(gg_rfsrc(f$reg))
  expect_print_header(gg_variable(f$reg))
  expect_print_header(gg_roc(f$cls, which_outcome = 1))
  data(pbc, package = "randomForestSRC")
  expect_print_header(gg_survival(interval = "days", censor = "status",
                                  data = pbc))
  expect_print_header(gg_brier(f$srv))

  # Partial classes — header includes variable counts.
  part_dta <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                              xvar.names = "Wind")
  expect_print_header(gg_partial(part_dta))
  expect_print_header(gg_partial_rfsrc(f$reg, xvar.names = "Wind"))
})

test_that("print.gg_partial uses 'name' column (not 'variable')", {
  f <- setup_forests()
  part_dta <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                              xvar.names = c("Wind", "Temp"))
  gp <- gg_partial(part_dta)
  out <- capture.output(print(gp))
  # Should report 2 continuous predictors (Wind and Temp both numeric in airquality).
  expect_match(out[1], "continuous: [12]")
})

test_that("summary methods return summary.gg objects that print cleanly", {
  f <- setup_forests()

  part_dta <- randomForestSRC::plot.variable(f$reg, partial = TRUE,
                                              xvar.names = "Wind")

  for (obj in list(
    gg_error(f$cls),
    gg_vimp(f$cls),
    gg_rfsrc(f$reg),
    gg_variable(f$reg),
    gg_roc(f$cls, which_outcome = 1),
    {
      data(pbc, package = "randomForestSRC", envir = environment())
      gg_survival(interval = "days", censor = "status", data = pbc)
    },
    gg_brier(f$srv),
    gg_partial(part_dta),
    gg_partial_rfsrc(f$reg, xvar.names = "Wind")
  )) {
    s <- summary(obj)
    expect_s3_class(s, "summary.gg")
    out <- capture.output(print(s))
    expect_true(length(out) >= 1L)
  }
})

# A gg_shap object built by hand, so the method contracts can be exercised
# without running kernelshap. These tests carry no skip_on_cran(): they are the
# ones that keep print/summary.gg_shap covered in a default R CMD check, since
# the forest-driven tests below are skipped there.
make_mock_gg_shap <- function(provenance = TRUE) {
  vars <- c("Temp", "Wind")
  gg_dta <- data.frame(
    id          = rep(1:3, times = 2),
    vars        = factor(rep(vars, each = 3), levels = rev(vars)),
    shap        = c(2, -3, 4, 0.5, -0.25, 0.75),
    value       = c(80, 72, 90, 5, 12, 8),
    value_label = as.character(c(80, 72, 90, 5, 12, 8)),
    stringsAsFactors = FALSE
  )
  class(gg_dta) <- c("gg_shap", "data.frame")
  attr(gg_dta, "baseline")    <- 42
  attr(gg_dta, "bg_n")        <- 10L
  attr(gg_dta, "which.class") <- 1
  if (provenance) {
    attr(gg_dta, "provenance") <- list(source = "randomForestSRC",
                                       family = "regr", ntree = 10L, n = 3L)
  }
  gg_dta
}

test_that("print.gg_shap emits a single header line and returns invisibly", {
  gg_dta <- make_mock_gg_shap()

  out <- capture.output(res <- withVisible(print(gg_dta)))
  # Same header-only contract as every other print.gg_* method.
  expect_length(out, 1L)
  expect_match(out, "gg_shap")
  expect_false(res$visible)
  expect_s3_class(res$value, "gg_shap")
})

test_that("summary.gg_shap returns a summary.gg object that prints cleanly", {
  gg_dta <- make_mock_gg_shap()

  s <- summary(gg_dta)
  expect_s3_class(s, "summary.gg")
  out <- capture.output(print(s))
  expect_true(length(out) > 1L)
  # The summary should surface what gg_shap actually records.
  expect_true(any(grepl("baseline", out, ignore.case = TRUE)))
  expect_true(any(grepl("background", out, ignore.case = TRUE)))
  # Ranked by mean |SHAP|: Temp (3) outranks Wind (0.5).
  expect_match(paste(out, collapse = " "), "Temp.*Wind")
})

test_that("print/summary.gg_shap tolerate a missing provenance attribute", {
  # Provenance is best-effort (see R/print_helpers.R) -- objects saved before it
  # existed have none. Both methods must degrade to the bare header rather than
  # fail on a NULL lookup.
  gg_dta <- make_mock_gg_shap(provenance = FALSE)

  out <- capture.output(print(gg_dta))
  expect_length(out, 1L)
  expect_match(out, "^<gg_shap>")

  s <- summary(gg_dta)
  expect_s3_class(s, "summary.gg")
  body <- capture.output(print(s))
  # which.class is meaningless without a family to gate on, so it is omitted.
  expect_false(any(grepl("which.class", body)))
})

test_that("summary.gg_shap reports which.class only for classification fits", {
  reg <- make_mock_gg_shap()
  expect_false(any(grepl("which.class", capture.output(print(summary(reg))))))

  cls <- make_mock_gg_shap()
  attr(cls, "provenance")$family <- "class"
  attr(cls, "which.class") <- 2
  expect_true(any(grepl("which.class: 2", capture.output(print(summary(cls))))))
})

test_that("print/summary.gg_shap work on a real kernelshap-backed object", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  set.seed(42)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 10)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 10)

  out <- capture.output(print(gg_dta))
  expect_length(out, 1L)
  expect_match(out, "gg_shap")

  s <- summary(gg_dta)
  expect_s3_class(s, "summary.gg")
  expect_true(any(grepl("baseline", capture.output(print(s)))))
})
