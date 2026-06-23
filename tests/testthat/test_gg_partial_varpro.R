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

test_that("gg_partial_varpro: scale='rmst' part_dta-only no longer needs time", {
  # 3.3.0: tau defaults when recomputing from object; with part_dta only there
  # is nothing to recompute, so this is a label-only call and must not error.
  expect_no_error(suppressWarnings(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst")))
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
  # precomputed part_dta → label-only RMST (warns); provenance still records it
  result <- suppressWarnings(
    gg_partial_varpro(make_mock_vpro_data(), scale = "rmst", time = 365)
  )
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
  result <- suppressWarnings(
    gg_partial_varpro(make_mock_vpro_data(), nvars = 1,
                      scale = "rmst", time = 365)
  )
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

## ── RMST integration helper (.rmst_from_survival) ────────────────────────────
test_that(".rmst_from_survival integrates a step survival curve to tau", {
  times <- c(1, 2, 3)
  surv  <- matrix(c(0.8, 0.5, 0.2), nrow = 1)   # S(1), S(2), S(3)
  # [0,1) S=1, [1,2) S=0.8, [2,3) S=0.5  → 1 + 0.8 + 0.5 = 2.3
  expect_equal(ggRandomForests:::.rmst_from_survival(surv, times, tau = 3), 2.3)
  # tau = 2.5 caps the last interval at width 0.5 → 1 + 0.8 + 0.25 = 2.05
  expect_equal(ggRandomForests:::.rmst_from_survival(surv, times, tau = 2.5), 2.05)
  # tau = 0.5 lands inside the first (S=1) interval → 0.5
  expect_equal(ggRandomForests:::.rmst_from_survival(surv, times, tau = 0.5), 0.5)
})

test_that(".rmst_from_survival accepts a bare numeric vector (single curve)", {
  out <- ggRandomForests:::.rmst_from_survival(c(0.8, 0.5, 0.2), c(1, 2, 3),
                                               tau = 3)
  expect_equal(out, 2.3)            # same as the 1-row matrix case
})

test_that(".rmst_from_survival errors on a time-grid / column mismatch", {
  # surv has 3 columns but only 2 time points -> must fail loud, not misalign
  expect_error(
    ggRandomForests:::.rmst_from_survival(matrix(c(0.9, 0.6, 0.3), nrow = 1),
                                          c(1, 2), tau = 2),
    regexp = "grids must match|time point"
  )
})

test_that(".resolve_varpro_scale maps 'auto' by family, passes others through", {
  # 3.3.0: bounded/interpretable defaults
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "surv"),  "surv")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "class"), "prob")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", "regr"),
               "generic")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("auto", NA_character_),
               "generic")
  # explicit scales returned unchanged
  expect_equal(ggRandomForests:::.resolve_varpro_scale("rmst", "surv"), "rmst")
  expect_equal(ggRandomForests:::.resolve_varpro_scale("odds", "class"), "odds")
})

## ── v3.3.0 scale transform + bounded predicate ───────────────────────────────
test_that(".scale_transform applies prob/odds/identity", {
  z <- matrix(c(0, log(3)), nrow = 1)        # log-odds: 0->.5, log(3)->.75
  expect_equal(ggRandomForests:::.scale_transform(z, "prob"),
               matrix(c(0.5, 0.75), nrow = 1))
  expect_equal(ggRandomForests:::.scale_transform(z, "odds"),
               matrix(c(1, 3), nrow = 1))
  for (s in c("logodds", "generic", "surv", "rmst", "mortality"))
    expect_equal(ggRandomForests:::.scale_transform(z, s), z)
})

test_that(".is_bounded_scale flags prob/odds/surv only", {
  for (s in c("prob", "odds", "surv"))
    expect_true(ggRandomForests:::.is_bounded_scale(s))
  for (s in c("logodds", "generic", "mortality", "rmst", "chf"))
    expect_false(ggRandomForests:::.is_bounded_scale(s))
})

## ── v3.3.0 conversion in the extractor (mean of probabilities) ───────────────
test_that("gg_partial_varpro: scale='prob' is mean of plogis, causal NA", {
  d <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, scale = "prob")
  age <- res$continuous[res$continuous$name == "age", ]
  expected <- colMeans(stats::plogis(d$age$yhat.par), na.rm = TRUE)
  expect_equal(age$parametric, expected)
  expect_true(all(age$parametric >= 0 & age$parametric <= 1))
  expect_true(all(is.na(res$continuous$causal)))
})

test_that("gg_partial_varpro: scale='logodds' keeps raw values + causal", {
  d <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, scale = "logodds")
  age <- res$continuous[res$continuous$name == "age", ]
  expect_equal(age$parametric, colMeans(d$age$yhat.par, na.rm = TRUE))
  expect_false(all(is.na(res$continuous$causal)))
})

## ── v3.3.0 survival probability at tau (pure) + default-tau fallback ──────────
test_that(".surv_at_tau pulls S(tau) snapped to nearest event time", {
  surv  <- matrix(c(0.9, 0.6, 0.3), nrow = 1)
  times <- c(1, 2, 3)
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 2),   0.6)
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 2.1), 0.6)
  expect_equal(ggRandomForests:::.surv_at_tau(surv, times, 3),   0.3)
})

test_that(".default_surv_tau falls back to median(time.interest)", {
  fake <- list(rf = list(time.interest = c(2, 4, 6, 8)))
  expect_equal(ggRandomForests:::.default_surv_tau(fake), 5)
})

## ── v3.3.0 y-axis labels ─────────────────────────────────────────────────────
test_that(".partial_varpro_ylabel: prob/odds/logodds/surv labels", {
  lab <- ggRandomForests:::.partial_varpro_ylabel
  expect_match(lab(list(scale = "prob", target = "yes")),    "P\\(Y = yes\\)")
  expect_match(lab(list(scale = "odds", target = "yes")),    "Odds\\(Y = yes\\)")
  expect_match(lab(list(scale = "logodds", target = "yes")), "Log-odds")
  expect_equal(lab(list(scale = "prob", target = NA)),       "Probability")
  expect_match(lab(list(scale = "surv", rmst_tau = 1000)),
               "Survival probability at t = 1000")
})

## ── v3.3.0 plot: causal hidden on bounded scales ─────────────────────────────
test_that("plot.gg_partial_varpro: bounded scale drops causal, warns if asked", {
  d   <- make_mock_vpro_data()
  res <- gg_partial_varpro(d, nvars = 1, scale = "prob")
  expect_s3_class(plot(res), "ggplot")
  expect_warning(plot(res, type = "causal"), regexp = "causal")
})

test_that(".rmst_from_survival is vectorised over rows and bounded by tau", {
  times <- c(1, 2, 3, 4)
  surv  <- rbind(c(0.9, 0.7, 0.4, 0.1),
                 c(0.5, 0.3, 0.2, 0.1))
  out <- ggRandomForests:::.rmst_from_survival(surv, times, tau = 4)
  expect_length(out, 2L)
  expect_true(all(out <= 4))          # RMST(tau) <= tau
  expect_true(out[1] > out[2])        # higher survival → larger RMST
})

## ── RMST guardrails ──────────────────────────────────────────────────────────
test_that("gg_partial_varpro: scale='rmst' with precomputed part_dta warns", {
  expect_warning(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst",
                      time = 365),
    regexp = "cannot drive|precomputed"
  )
})

test_that("gg_partial_varpro: 'time' on a scale that ignores it warns", {
  expect_warning(
    gg_partial_varpro(make_mock_vpro_data(), scale = "mortality", time = 365),
    regexp = "ignored"
  )
})

test_that("gg_partial_varpro: scale='rmst' on a non-survival fit errors", {
  fake <- structure(list(family = "regr", rf = list()), class = "varpro")
  expect_error(
    gg_partial_varpro(object = fake, scale = "rmst", time = 1),
    regexp = "survival varpro fit"
  )
})

test_that("gg_partial_varpro: non-scalar / non-finite 'time' errors", {
  expect_error(
    gg_partial_varpro(make_mock_vpro_data(), scale = "rmst", time = c(1, 2)),
    regexp = "single finite numeric"
  )
  expect_error(
    gg_partial_varpro(make_mock_vpro_data(), scale = "rmst", time = NA_real_),
    regexp = "single finite numeric"
  )
})

test_that("gg_partial_varpro: tau below the first event time is not flagged", {
  # tau < min(time.interest) is valid (S(t)=1 on [0, t1)), so no truncation warning
  ti <- c(10, 20, 30)
  fake <- structure(list(family = "surv", rf = list(time.interest = ti)),
                    class = "varpro")
  expect_no_warning(
    ggRandomForests:::.warn_varpro_rmst(NULL, fake, "rmst", time = 5)
  )
  # tau beyond the largest event time IS flagged
  expect_warning(
    ggRandomForests:::.warn_varpro_rmst(NULL, fake, "rmst", time = 40),
    regexp = "exceeds the model's largest event time|truncated"
  )
})

## ── RMST learner end-to-end (real varpro fit) ────────────────────────────────
test_that("gg_partial_varpro: scale='rmst' is genuinely tau-dependent", {
  skip_on_cran()                      # varpro fit + 2 partialpro calls (~15 s)
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("varPro")
  set.seed(11)
  pbc <- get(utils::data("pbc", package = "randomForestSRC",
                         envir = environment()))
  pbc <- pbc[stats::complete.cases(pbc), ]
  vp  <- varPro::varpro(Surv(days, status) ~ ., pbc, ntree = 60, nvar = 3)

  r_short <- gg_partial_varpro(object = vp, scale = "rmst", time = 500,
                               nvars = 1)
  r_long  <- gg_partial_varpro(object = vp, scale = "rmst", time = 2000,
                               nvars = 1)

  expect_s3_class(r_short, "gg_partial_varpro")
  expect_equal(attr(r_short, "provenance")$scale, "rmst")
  expect_equal(attr(r_short, "provenance")$rmst_tau, 500)

  # RMST(500) is bounded by 500; the two horizons must differ (not MC noise).
  expect_true(all(r_short$continuous$parametric <= 500 + 1e-6))
  expect_gt(
    max(abs(r_long$continuous$parametric - r_short$continuous$parametric)),
    50
  )

  # Default (mortality) recompute exercises the plain partialpro(object) path.
  r_mort <- gg_partial_varpro(object = vp, scale = "mortality", nvars = 1)
  expect_s3_class(r_mort, "gg_partial_varpro")
  expect_equal(attr(r_mort, "provenance")$scale, "mortality")

  # '...' is forwarded to partialpro(): xvar.names restricts which variables
  # are computed (instead of falling back to get.topvars()).
  one  <- varPro::get.topvars(vp)[1]
  r1   <- gg_partial_varpro(object = vp, scale = "rmst", time = 500,
                            xvar.names = one)
  vars <- unique(c(
    if (nrow(r1$continuous)  > 0) r1$continuous$name,
    if (nrow(r1$categorical) > 0) r1$categorical$name
  ))
  expect_equal(vars, one)
})

test_that("gg_partial_varpro: '...' with a precomputed part_dta warns", {
  expect_warning(
    gg_partial_varpro(make_mock_vpro_data(), xvar.names = "age"),
    regexp = "ignored because 'part_dta'"
  )
})

## ── Helper: mock C-path varpro object ────────────────────────────────────────
make_mock_cpath <- function(xvar_subset = NULL) {
  skip_if_not_installed("randomForestSRC")
  set.seed(7)
  veteran_data <- survival::veteran
  rf <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran_data, ntree = 30, importance = FALSE
  )
  xnames <- if (is.null(xvar_subset)) rf$xvar.names else rf$xvar.names[xvar_subset]
  x      <- if (is.null(xvar_subset)) rf$xvar else rf$xvar[, xnames, drop = FALSE]
  vp <- list(rf = rf, family = "surv", xvar.names = xnames,
             x = x, max.tree = 30L)
  class(vp) <- "varpro"
  list(vp = vp, rf = rf)
}

## ── .rmst_learner closure (small rfsrc fit; no varpro grow → CRAN-safe) ───────
test_that(".rmst_learner returns RMST(tau) for OOB and newdata calls", {
  skip_if_not_installed("randomForestSRC")
  m       <- make_mock_cpath()
  tau     <- stats::median(m$rf$time.interest)
  learner <- ggRandomForests:::.rmst_learner(list(rf = m$rf), tau)

  oob <- learner()                              # missing(newx) → OOB branch
  nd  <- learner(survival::veteran[1:5, ])      # newdata branch
  expect_length(oob, nrow(m$rf$xvar))
  expect_length(nd, 5L)
  expect_true(all(oob >= 0 & oob <= tau + 1e-6))
})

## ── v3.3.0 survival S(t) learner + default tau (rfsrc fit; no varpro grow) ────
test_that(".surv_learner returns S(tau) in [0,1] for OOB and newdata", {
  skip_if_not_installed("randomForestSRC")
  m       <- make_mock_cpath()
  tau     <- stats::median(m$rf$time.interest)
  learner <- ggRandomForests:::.surv_learner(list(rf = m$rf), tau)
  oob <- learner()
  nd  <- learner(survival::veteran[1:5, ])
  expect_length(oob, nrow(m$rf$xvar))
  expect_length(nd, 5L)
  expect_true(all(oob >= 0 & oob <= 1))
  expect_true(all(nd  >= 0 & nd  <= 1))
})

test_that(".default_surv_tau is the median observed survival time", {
  skip_if_not_installed("randomForestSRC")
  m   <- make_mock_cpath()
  tau <- ggRandomForests:::.default_surv_tau(list(rf = m$rf))
  expect_equal(tau, stats::median(survival::veteran$time))
})

## ── v3.3.0 survival routing + classification provenance (real varpro fits) ───
test_that("gg_partial_varpro: scale='surv' via learner, in [0,1], default tau", {
  skip_on_cran()
  skip_if_not_installed("randomForestSRC"); skip_if_not_installed("varPro")
  set.seed(13)
  pbc <- get(utils::data("pbc", package = "randomForestSRC",
                         envir = environment()))
  pbc <- pbc[stats::complete.cases(pbc), ]
  vp  <- varPro::varpro(Surv(days, status) ~ ., pbc, ntree = 60, nvar = 2)

  r <- gg_partial_varpro(object = vp, scale = "surv", time = 1000, nvars = 1)
  expect_equal(attr(r, "provenance")$scale, "surv")
  expect_equal(attr(r, "provenance")$path,  "A")
  expect_true(all(r$continuous$parametric >= 0 &
                  r$continuous$parametric <= 1))
  expect_true(all(is.na(r$continuous$causal)))

  rd <- suppressMessages(gg_partial_varpro(object = vp, scale = "surv",
                                           nvars = 1))
  expect_equal(attr(rd, "provenance")$rmst_tau,
               ggRandomForests:::.default_surv_tau(vp))

  ra <- suppressMessages(gg_partial_varpro(object = vp, nvars = 1))
  expect_equal(attr(ra, "provenance")$scale, "surv")
})

test_that("gg_partial_varpro: classification provenance records target class", {
  skip_on_cran(); skip_if_not_installed("varPro")
  set.seed(5)
  dat <- data.frame(y = factor(rep(c("a", "b"), 60)),
                    x1 = rnorm(120), x2 = rnorm(120))
  vp  <- varPro::varpro(y ~ ., dat, ntree = 40, nvar = 2)
  r   <- suppressMessages(
    gg_partial_varpro(object = vp, scale = "prob", nvars = 1))
  expect_equal(attr(r, "provenance")$scale, "prob")
  expect_equal(attr(r, "provenance")$target, "b")
})

## ── C-path (scale = chf; surv now uses the path-A learner) ───────────────────
test_that("gg_partial_varpro: C-path returns gg_partial_varpro class", {
  skip_if_not_installed("randomForestSRC")
  m      <- make_mock_cpath()
  result <- suppressWarnings(
    gg_partial_varpro(object = m$vp, scale = "chf",
                      time = median(m$rf$time.interest))
  )
  expect_s3_class(result, "gg_partial_varpro")
})

test_that("gg_partial_varpro: C-path provenance path='C'", {
  skip_if_not_installed("randomForestSRC")
  m      <- make_mock_cpath()
  result <- suppressWarnings(
    gg_partial_varpro(object = m$vp, scale = "chf",
                      time = median(m$rf$time.interest))
  )
  expect_equal(attr(result, "provenance")$path, "C")
  expect_equal(attr(result, "provenance")$scale, "chf")
})

test_that("gg_partial_varpro: plot C-path returns ggplot", {
  skip_if_not_installed("randomForestSRC")
  m      <- make_mock_cpath(xvar_subset = 1L)
  result <- suppressWarnings(
    gg_partial_varpro(object = m$vp, scale = "chf",
                      time = median(m$rf$time.interest))
  )
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})

test_that("gg_partial_varpro: C-path 'model' label is attached to the frames", {
  skip_if_not_installed("randomForestSRC")
  # full var set → both continuous (age/karno) and categorical (celltype)
  # frames populate, so the model column is attached to each.
  m      <- make_mock_cpath()
  result <- suppressWarnings(
    gg_partial_varpro(object = m$vp, scale = "chf",
                      time = median(m$rf$time.interest), model = "forestC")
  )
  got_cont <- is.data.frame(result$continuous) &&
    nrow(result$continuous) > 0 && "model" %in% names(result$continuous)
  got_cat  <- is.data.frame(result$categorical) &&
    nrow(result$categorical) > 0 && "model" %in% names(result$categorical)
  expect_true(got_cont || got_cat)
  # at least the populated continuous frame should carry the label
  if (is.data.frame(result$continuous) && nrow(result$continuous) > 0)
    expect_equal(unique(result$continuous$model), "forestC")
})
