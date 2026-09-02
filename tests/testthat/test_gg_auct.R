test_that("gg_auct.rhf returns a tidy AUC(t) frame (no bootstrap -> NA CI)", {
  gg <- gg_auct(.rhf_pbc(), marker = "chf", auct_fit = .auct_pbc_noboot())
  expect_s3_class(gg, "gg_auct")
  expect_true(all(c("time", "auc", "se", "lower", "upper", "marker") %in% names(gg)))
  a <- .auct_pbc_noboot()
  expect_equal(nrow(gg), nrow(a$AUC.by.time))
  expect_equal(gg$auc, a$AUC.by.time$AUC)
  expect_true(all(is.na(gg$lower)))
  expect_equal(attr(gg, "iauc")$uno, a$iAUC.uno)
})

test_that("gg_auct.rhf carries bootstrap CI when present", {
  gg  <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  fin <- is.finite(gg$lower) & is.finite(gg$upper)
  expect_true(any(fin))                          # bootstrap CIs present
  expect_true(all(gg$upper[fin] >= gg$lower[fin]))  # valid where defined
  expect_true(is.finite(attr(gg, "iauc")$uno.se))
})

test_that("gg_auct rejects non-rhf input and bad auct_fit", {
  expect_error(gg_auct(lm(mpg ~ wt, mtcars)))
  expect_error(gg_auct(.rhf_pbc(), auct_fit = list(1)), "auct.rhf")
})

test_that("gg_auct S3 companions work", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  expect_output(print(gg), "gg_auct")
  expect_invisible(print(gg))
  s <- summary(gg)
  expect_true(is.data.frame(s))
  expect_true(all(c("iAUC.uno", "iAUC.std") %in% names(s)))
  expect_s3_class(autoplot(gg), "ggplot")
})

test_that("gg_auct attaches provenance from the rhf object", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  prov <- attr(gg, "provenance")
  expect_equal(prov$source, "randomForestRHF")
  expect_equal(prov$ntree, .rhf_pbc()$ntree)
})

test_that("gg_auct forwards method to auct.rhf", {
  # auct.rhf()'s own method default is "cumulative", so without this argument
  # the incident/dynamic path was unreachable through gg_auct(): the only route
  # was to call auct.rhf() directly and hand the result back via auct_fit.
  o <- .rhf_pbc()
  set.seed(20260828L)
  inc <- gg_auct(o, marker = "haz", method = "incident")
  ref <- randomForestRHF::auct.rhf(o, marker = "haz", method = "incident")
  expect_s3_class(inc, "gg_auct")
  expect_equal(attr(inc, "iauc")$uno, ref$iAUC.uno)
  expect_equal(inc$auc, ref$AUC.by.time$AUC)
})

test_that("gg_auct method='cumulative' stays the default", {
  o <- .rhf_pbc()
  expect_equal(
    attr(gg_auct(o, marker = "chf"), "iauc")$uno,
    attr(gg_auct(o, marker = "chf", method = "cumulative"), "iauc")$uno
  )
})

test_that("gg_auct passes ... through to auct.rhf", {
  # bootstrap.rep drives the CI ribbon in plot.gg_auct(), and before ... was
  # forwarded there was no way to request it without precomputing the fit.
  o <- .rhf_pbc()
  set.seed(20260828L)
  gg <- gg_auct(o, marker = "chf", bootstrap.rep = 5L)
  expect_true(any(is.finite(gg$lower)))
  expect_true(any(is.finite(gg$upper)))
})

test_that("gg_auct ignores method and ... when auct_fit is supplied", {
  o <- .rhf_pbc()
  fit <- .auct_pbc_noboot()
  gg <- gg_auct(o, marker = "chf", method = "incident", auct_fit = fit)
  expect_equal(attr(gg, "iauc")$uno, fit$iAUC.uno)
})

test_that("the cumulative/dynamic version guard gates only what it should", {
  # randomForestRHF < 2.0.3 returns an inverted cumulative/dynamic AUC, and a
  # Suggests version is not enforced at run time, so gg_auct() checks it. The
  # guard takes the version as an argument precisely so this can be exercised
  # without a downgraded install.
  guard <- ggRandomForests:::.stop_if_auct_cumulative_unsupported

  # Gated: the affected definition on an affected version.
  for (v in c("1.0.1", "2.0.0", "2.0.2")) {
    expect_error(
      guard("cumulative", package_version(v)),
      "needs randomForestRHF >= 2.0.3"
    )
  }

  # Not gated: fixed versions, including a later one.
  for (v in c("2.0.3", "2.1.0", "3.0.0")) {
    expect_silent(guard("cumulative", package_version(v)))
    expect_null(guard("cumulative", package_version(v)))
  }

  # Not gated: the incident definition never inherited the problem, so gating
  # it would break working code.
  for (v in c("2.0.0", "2.0.3")) {
    expect_silent(guard("incident", package_version(v)))
  }
})

test_that("the version guard names the installed version and a way out", {
  guard <- ggRandomForests:::.stop_if_auct_cumulative_unsupported
  msg <- tryCatch(
    guard("cumulative", package_version("2.0.0")),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "2.0.0", fixed = TRUE)
  expect_match(msg, "method = \"incident\"", fixed = TRUE)
})

test_that("gg_auct.rhf still calls the version guard on the compute path", {
  # The guard's logic is tested directly above, which would keep passing if the
  # call site were dropped in a refactor. Pin that it is still wired in, and
  # still inside the branch that computes rather than the one that accepts a
  # supplied auct_fit.
  txt <- paste(
    deparse(body(ggRandomForests:::gg_auct.rhf)), collapse = "\n"
  )
  expect_match(txt, ".stop_if_auct_cumulative_unsupported", fixed = TRUE)
  compute_branch <- sub(
    ".*is\\.null\\(auct_fit\\)", "", sub("inherits\\(auct_fit.*", "", txt)
  )
  expect_match(
    compute_branch, ".stop_if_auct_cumulative_unsupported", fixed = TRUE
  )
})
