# testthat for gg_brier

test_that("gg_brier.rfsrc produces a tidy survival frame", {
  data(pbc, package = "randomForestSRC")
  set.seed(1)
  rf <- randomForestSRC::rfsrc(
    Surv(days, status) ~ .,
    data = pbc,
    nsplit = 10,
    ntree = 100
  )

  gg_dta <- gg_brier(rf)

  expect_s3_class(gg_dta, "gg_brier")
  expect_true(all(c("time", "brier", "bs.q25", "bs.q50", "bs.q75",
                    "bs.q100", "bs.lower", "bs.upper",
                    "crps", "crps.q25", "crps.q50", "crps.q75",
                    "crps.q100", "crps.lower", "crps.upper") %in%
                    names(gg_dta)))
  # Envelope sanity: lower <= upper at every time.
  expect_true(all(gg_dta$bs.lower <= gg_dta$bs.upper))
  expect_true(all(gg_dta$crps.lower <= gg_dta$crps.upper, na.rm = TRUE))
  expect_equal(nrow(gg_dta), length(rf$time.interest))
  expect_true(all(is.finite(gg_dta$brier)))
  expect_true(all(gg_dta$brier >= 0 & gg_dta$brier <= 1))

  # integrated CRPS attribute matches the upstream scalar.
  expect_true(!is.null(attr(gg_dta, "crps_integrated")))

  # plot method returns a ggplot for each supported display.
  expect_s3_class(plot(gg_dta), "ggplot")
  expect_s3_class(plot(gg_dta, type = "crps"), "ggplot")
  expect_s3_class(plot(gg_dta, by_quartile = TRUE), "ggplot")
  expect_s3_class(plot(gg_dta, type = "crps", by_quartile = TRUE), "ggplot")
})

test_that("gg_brier rejects non-survival forests", {
  data(iris, package = "datasets")
  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  expect_error(gg_brier(rf), "right-censored survival")
})

test_that("plot.gg_brier rejects non-gg_brier input", {
  expect_error(plot.gg_brier(iris), "gg_brier object")
})
