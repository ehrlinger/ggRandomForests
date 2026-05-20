# Tests for gg_partial_varpro (Phase 1: A-path extractor)
# C-path tests are in Task 6.

## ── Helper: mock partialpro data ────────────────────────────────────────────
make_mock_vpro_data <- function(n_obs = 30, n_pts = 15) {
  set.seed(42)
  list(
    age = list(
      xvirtual    = seq(30, 80, length.out = n_pts),   # continuous: 15 > 10
      xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
    ),
    sex = list(
      xvirtual    = c(0, 1),                           # categorical: 2 <= 10
      xorg        = sample(c(0, 1), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
    )
  )
}

## ── Input validation ─────────────────────────────────────────────────────────
test_that("gg_partial_varpro: neither part_dta nor object → stop", {
  expect_error(gg_partial_varpro(), regexp = "at least one")
})

test_that("gg_partial_varpro: scale='surv' without object → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "surv"),
    regexp = "requires 'object'"
  )
})

test_that("gg_partial_varpro: scale='chf' without object → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "chf"),
    regexp = "requires 'object'"
  )
})

test_that("gg_partial_varpro: scale='rmst' without time → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst"),
    regexp = "requires 'time'"
  )
})

## ── Class & structure ────────────────────────────────────────────────────────
test_that("gg_partial_varpro returns gg_partial_varpro class", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_s3_class(result, "gg_partial_varpro")
})

test_that("gg_partial_varpro has continuous and categorical elements", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("gg_partial_varpro continuous has required columns", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true(all(c("variable", "parametric", "nonparametric", "causal", "name")
                  %in% colnames(result$continuous)))
})

test_that("gg_partial_varpro categorical has required columns", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true(all(c("parametric", "nonparametric", "causal", "variable", "name")
                  %in% colnames(result$categorical)))
})

test_that("gg_partial_varpro continuous: one row per xvirtual point (age)", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  age_rows <- result$continuous[result$continuous$name == "age", ]
  expect_equal(nrow(age_rows), 15L)
})

test_that("gg_partial_varpro: age is continuous, sex is categorical", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true("age" %in% result$continuous$name)
  expect_false("age" %in% result$categorical$name)
  expect_true("sex" %in% result$categorical$name)
  expect_false("sex" %in% result$continuous$name)
})

## ── Provenance attribute ─────────────────────────────────────────────────────
test_that("gg_partial_varpro attaches provenance list attr", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  prov <- attr(result, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("family", "scale", "rmst_tau", "xvar.names", "n", "path",
                    "source", "ntree")
                  %in% names(prov)))
})

test_that("gg_partial_varpro: provenance path = 'A' for A-path", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_equal(attr(result, "provenance")$path, "A")
})

## ── Scale resolution ─────────────────────────────────────────────────────────
test_that("gg_partial_varpro: scale='auto' no object → prov scale='generic'", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_equal(attr(result, "provenance")$scale, "generic")
})

test_that("gg_partial_varpro: scale='mortality' recorded in provenance", {
  result <- gg_partial_varpro(make_mock_vpro_data(), scale = "mortality")
  expect_equal(attr(result, "provenance")$scale, "mortality")
})

test_that("gg_partial_varpro: scale='rmst' with time stored in provenance", {
  result <- gg_partial_varpro(make_mock_vpro_data(), scale = "rmst", time = 365)
  prov <- attr(result, "provenance")
  expect_equal(prov$scale, "rmst")
  expect_equal(prov$rmst_tau, 365)
})

## ── nvars + model ────────────────────────────────────────────────────────────
test_that("gg_partial_varpro: nvars=1 processes only first variable", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  expect_true("age" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0L)
})

test_that("gg_partial_varpro: model arg adds column", {
  result <- gg_partial_varpro(make_mock_vpro_data(), model = "forest1")
  expect_true("model" %in% colnames(result$continuous))
  expect_true("model" %in% colnames(result$categorical))
  expect_equal(unique(result$continuous$model), "forest1")
})

test_that("gg_partial_varpro: no model arg → no model column", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_false("model" %in% colnames(result$continuous))
})

## ── Numeric/structural tests ──────────────────────────────────────────────────
test_that("gg_partial_varpro: continuous parametric equals colMeans(yhat.par)", {
  d      <- make_mock_vpro_data()
  result <- gg_partial_varpro(d)
  expected <- colMeans(d$age$yhat.par, na.rm = TRUE)
  expect_equal(result$continuous$parametric[result$continuous$name == "age"],
               expected)
})

test_that("gg_partial_varpro: categorical sex has n_obs * 2 rows", {
  result  <- gg_partial_varpro(make_mock_vpro_data())
  sex_rows <- result$categorical[result$categorical$name == "sex", ]
  expect_equal(nrow(sex_rows), 30L * 2L)
})

## ── plot.gg_partial_varpro (A-path) ──────────────────────────────────────────
test_that("plot.gg_partial_varpro: continuous-only returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: both cont + cat returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: type arg selects effect columns", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  gg <- plot(result, type = "parametric")
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: scale='mortality' → honest y-label", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1,
                               scale = "mortality")
  gg <- plot(result)
  expect_true(grepl("mortality|Ensemble|expected", gg$labels$y,
                    ignore.case = TRUE))
})

test_that("plot.gg_partial_varpro: scale='rmst' with time → RMST y-label", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1,
                               scale = "rmst", time = 365)
  gg <- plot(result)
  expect_true(grepl("RMST|365", gg$labels$y))
})

## ── autoplot / print / summary ───────────────────────────────────────────────
test_that("autoplot.gg_partial_varpro: returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_s3_class(ggplot2::autoplot(result), "ggplot")
})

test_that("print.gg_partial_varpro: returns x invisibly", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
  expect_true(any(grepl("gg_partial_varpro", out)))
})

test_that("summary.gg_partial_varpro: returns summary.gg", {
  result  <- gg_partial_varpro(make_mock_vpro_data())
  s       <- summary(result)
  expect_s3_class(s, "summary.gg")
})
