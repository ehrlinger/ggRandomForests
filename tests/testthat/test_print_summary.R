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
})

test_that("summary methods return summary.gg objects that print cleanly", {
  f <- setup_forests()

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
    gg_brier(f$srv)
  )) {
    s <- summary(obj)
    expect_s3_class(s, "summary.gg")
    out <- capture.output(print(s))
    expect_true(length(out) >= 1L)
  }
})
