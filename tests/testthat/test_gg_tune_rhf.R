test_that("gg_tune_rhf tidies an OOB risk path in upstream order", {
  fit <- .fake_rhf_tune_risk()
  out <- gg_tune_rhf(fit)

  expect_identical(class(out), c("gg_tune_rhf", "data.frame"))
  expect_identical(names(out),
                   c("treesize", "metric", "value", "se", "selected"))
  expect_identical(out$treesize, fit$path$treesize)
  expect_identical(out$metric, rep("OOB risk", nrow(fit$path)))
  expect_equal(out$value, fit$path$risk)
  expect_true(all(is.na(out$se)))
  expect_identical(which(out$selected), 3L)
})

test_that("gg_tune_rhf uses iAUC and optional bootstrap standard errors", {
  fit <- .fake_rhf_tune_iauc()
  out <- gg_tune_rhf(fit)

  expect_identical(out$metric, rep("OOB iAUC", nrow(fit$path)))
  expect_equal(out$value, fit$path$iAUC)
  expect_equal(out$se, fit$path$iAUC.se)
  expect_identical(which(out$selected), 2L)

  no_se <- gg_tune_rhf(.fake_rhf_tune_iauc(with_se = FALSE))
  expect_true(all(is.na(no_se$se)))

  missing_one <- .fake_rhf_tune_iauc()
  missing_one$path$iAUC.se[2L] <- NA_real_
  expect_true(is.na(gg_tune_rhf(missing_one)$se[2L]))
})

test_that("gg_tune_rhf rejects the wrong upstream class", {
  expect_error(gg_tune_rhf(unclass(.fake_rhf_tune_risk())),
               "tune.treesize.rhf")
})

test_that("gg_tune_rhf validates required scalar metadata", {
  cases <- list(
    best_size = list(field = "best.size", value = c(5L, 8L)),
    best_err = list(field = "best.err", value = NA_real_),
    bounds = list(field = "bounds", value = c(2, NA_real_)),
    method = list(field = "method", value = character()),
    perf = list(field = "perf", value = "other")
  )
  for (case in cases) {
    fit <- .fake_rhf_tune_risk()
    fit[[case$field]] <- case$value
    expect_error(gg_tune_rhf(fit), case$field, fixed = TRUE)
  }
})

test_that("gg_tune_rhf validates path shape and numeric alignment", {
  fit <- .fake_rhf_tune_risk()
  fit$path$treesize[2L] <- fit$path$treesize[1L]
  expect_error(gg_tune_rhf(fit), "unique positive")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC.se <- "not numeric"
  expect_error(gg_tune_rhf(fit), "iAUC.se")
})

test_that("gg_tune_rhf requires one evaluated upstream optimum", {
  fit <- .fake_rhf_tune_risk()
  fit$best.size <- 7L
  expect_error(gg_tune_rhf(fit), "best.size")

  fit <- .fake_rhf_tune_risk()
  fit$path$risk[4L] <- fit$best.err
  expect_error(gg_tune_rhf(fit), "unique optimum")

  fit <- .fake_rhf_tune_iauc()
  fit$best.err <- 0.15
  expect_error(gg_tune_rhf(fit), "best.err")
})

test_that("gg_tune_rhf rejects empty paths and missing metrics", {
  fit <- .fake_rhf_tune_risk()
  fit$path <- fit$path[0, , drop = FALSE]
  expect_error(gg_tune_rhf(fit), "path")

  fit <- .fake_rhf_tune_risk()
  fit$path$risk <- NULL
  expect_error(gg_tune_rhf(fit), "risk")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC <- NULL
  expect_error(gg_tune_rhf(fit), "iAUC")
})

test_that("gg_tune_rhf rejects non-finite metrics and standard errors", {
  fit <- .fake_rhf_tune_risk()
  fit$path$risk[2L] <- Inf
  expect_error(gg_tune_rhf(fit), "risk")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC[2L] <- NA_real_
  expect_error(gg_tune_rhf(fit), "iAUC")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC.se[2L] <- -0.01
  expect_error(gg_tune_rhf(fit), "iAUC.se")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC.se[2L] <- Inf
  expect_error(gg_tune_rhf(fit), "iAUC.se")
})

test_that("gg_tune_rhf checks selected metric against best.err", {
  fit <- .fake_rhf_tune_risk()
  fit$path$risk[3L] <- 0.25
  expect_error(gg_tune_rhf(fit), "best.err")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC[2L] <- 0.80
  expect_error(gg_tune_rhf(fit), "best.err")
})

test_that("gg_tune_rhf preserves the supplied path order and values", {
  fit <- .fake_rhf_tune_risk()
  original <- fit$path
  gg_tune_rhf(fit)
  expect_identical(fit$path, original)
})

test_that("gg_tune_rhf records provenance without copying the forest", {
  risk <- gg_tune_rhf(.fake_rhf_tune_risk())
  prov <- attr(risk, "provenance")

  expect_identical(names(prov), c(
    "best_size", "best_err", "perf", "method", "bounds",
    "n_evaluations", "randomForestRHF_version"
  ))
  expect_identical(prov$best_size, 8L)
  expect_identical(prov$best_err, 0.24)
  expect_identical(prov$perf, "risk")
  expect_identical(prov$method, "golden")
  expect_identical(prov$bounds, c(lower = 2L, upper = 12L))
  expect_identical(prov$n_evaluations, 4L)
  expect_false("forest" %in% names(risk))
})
