# Tests for gg_partialpro

# Helper: create mock VarPro partialpro data
# - Continuous: length(xvirtual) > cat_limit (10)
# - Categorical: length(xvirtual) <= cat_limit (10)
make_mock_partialpro_data <- function(n_obs = 50, n_pts = 20) {
  set.seed(42)

  cont_var <- list(
    xvirtual = seq(20, 70, length.out = n_pts),  # 20 unique points > 10
    xorg = rnorm(n_obs, 45, 15),
    yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs, ncol = n_pts),
    yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs, ncol = n_pts),
    yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs, ncol = n_pts)
  )

  cat_var <- list(
    xvirtual = c(0, 1),  # 2 unique points <= 10
    xorg = sample(c(0, 1), n_obs, replace = TRUE),
    yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs, ncol = 2),
    yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs, ncol = 2),
    yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs, ncol = 2)
  )

  result <- list(age = cont_var, sex = cat_var)
  return(result)
}

# ---- basic structure -------------------------------------------------------

test_that("gg_partialpro returns list with continuous and categorical", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("gg_partialpro separates continuous (age) and categorical (sex)", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  # age has 20 unique points > cat_limit=10
  expect_true("age" %in% result$continuous$name)
  expect_false("age" %in% result$categorical$name)

  # sex has 2 unique points <= cat_limit=10
  expect_true("sex" %in% result$categorical$name)
  expect_false("sex" %in% result$continuous$name)
})

test_that("gg_partialpro continuous output has correct columns", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  expect_true(all(c("variable", "parametric", "nonparametric", "causal", "name")
                  %in% colnames(result$continuous)))
})

test_that("gg_partialpro categorical output has correct columns", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  expect_true(all(c("parametric", "nonparametric", "causal", "variable", "name")
                  %in% colnames(result$categorical)))
})

test_that("gg_partialpro continuous has one row per xvirtual point", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  # age has 20 xvirtual points
  age_rows <- result$continuous[result$continuous$name == "age", ]
  expect_equal(nrow(age_rows), 20)
})

# ---- nvars argument --------------------------------------------------------

test_that("gg_partialpro respects nvars = 1 (only age processed)", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta, nvars = 1)

  # Only age (continuous) should appear
  expect_true("age" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0)
})

# ---- model argument --------------------------------------------------------

test_that("gg_partialpro adds model column when model is provided", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta, model = "my_forest")

  expect_true("model" %in% colnames(result$continuous))
  expect_true("model" %in% colnames(result$categorical))
  expect_equal(unique(result$continuous$model), "my_forest")
  expect_equal(unique(result$categorical$model), "my_forest")
})

test_that("gg_partialpro without model has no model column", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta)

  expect_false("model" %in% colnames(result$continuous))
  expect_false("model" %in% colnames(result$categorical))
})

# ---- cat_limit edge cases --------------------------------------------------

# Note: cat_limit edge case for high cat_limit is tested using data specifically
# designed for categorical use: xvirtual <= cat_limit points, and yhat columns
# match the number of unique xorg values.

test_that("gg_partialpro with low cat_limit makes sex continuous", {
  mock_dta <- make_mock_partialpro_data()
  result <- gg_partialpro(mock_dta, cat_limit = 1)

  expect_true("age" %in% result$continuous$name)
  expect_true("sex" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0)
})

# ---- multi-category variable -----------------------------------------------

test_that("gg_partialpro handles variable with 3 categories", {
  set.seed(42)
  n_obs <- 30

  # Each category gets one yhat column; xorg has 3 unique values
  three_cat <- list(
    grp = list(
      xvirtual = c(1, 2, 3),  # 3 unique points <= cat_limit=10
      xorg = sample(c(1, 2, 3), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * 3), nrow = n_obs, ncol = 3),
      yhat.nonpar = matrix(rnorm(n_obs * 3), nrow = n_obs, ncol = 3),
      yhat.causal = matrix(rnorm(n_obs * 3), nrow = n_obs, ncol = 3)
    )
  )

  result <- gg_partialpro(three_cat)

  expect_equal(nrow(result$continuous), 0)
  expect_true("grp" %in% result$categorical$name)
  # Each category produces n_obs rows (one per observation), total = n_obs * 3
  grp_rows <- result$categorical[result$categorical$name == "grp", ]
  expect_equal(nrow(grp_rows), n_obs * 3)
  # All 3 category labels should appear
  expect_equal(length(unique(grp_rows$variable)), 3)
})
