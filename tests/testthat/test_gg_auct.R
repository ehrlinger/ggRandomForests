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
