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
