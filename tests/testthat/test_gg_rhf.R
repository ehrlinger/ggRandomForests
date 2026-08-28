test_that(".gg_provenance recognises rhf objects", {
  o <- .rhf_pbc()
  prov <- ggRandomForests:::.gg_provenance(o)
  expect_equal(prov$source, "randomForestRHF")
  expect_equal(prov$ntree, o$ntree)
  expect_equal(prov$n, o$n)
})

test_that("gg_rhf.rhf returns a tidy long frame over time.interest", {
  o  <- .rhf_pbc()
  gg <- gg_rhf(o)
  expect_s3_class(gg, "gg_rhf")
  expect_true(all(c("id", "time", "hazard", "chf", "source") %in% names(gg)))
  n_case <- nrow(o$hazard.oob)
  n_time <- length(o$time.interest)
  expect_equal(nrow(gg), n_case * n_time)
  expect_setequal(unique(gg$id), o$ensemble.id)
  expect_setequal(unique(gg$time), o$time.interest)
  expect_equal(unique(gg$source), "oob")
  expect_true(all(gg$hazard >= 0, na.rm = TRUE))
  expect_true(all(is.finite(gg$chf) & gg$chf >= 0))
})

test_that("gg_rhf carries the rhf hazard NA mask through unchanged", {
  o  <- .rhf_pbc()
  gg <- gg_rhf(o)
  # randomForestRHF >= 2.0.0 defines the pointwise hazard only where the grid
  # point falls inside one of the case's supplied (start, stop] intervals, and
  # returns NA in gaps and after the final stop. chf is unaffected: it
  # accumulates the exact interval overlap, so it stays flat across those
  # regions rather than going NA. gg_rhf is a passthrough, so the mask has to
  # arrive in the frame unrepaired, undropped and in column-major order.
  expect_identical(is.na(gg$hazard), as.vector(is.na(o$hazard.oob)))
  expect_true(all(is.finite(gg$hazard[!is.na(gg$hazard)])))
  expect_false(anyNA(gg$chf))
})

test_that("gg_rhf source='inbag' selects the inbag matrices", {
  o  <- .rhf_pbc()
  gg <- gg_rhf(o, source = "inbag")
  expect_equal(unique(gg$source), "inbag")
  expect_equal(nrow(gg), nrow(o$hazard.inbag) * length(o$time.interest))
})

test_that("gg_rhf rejects non-rhf input", {
  expect_error(gg_rhf(lm(mpg ~ wt, mtcars)), "rhf")
})

test_that("gg_rhf falls back to inbag when oob is absent", {
  o <- .rhf_pbc()
  o$hazard.oob <- NULL
  o$chf.oob    <- NULL
  gg <- gg_rhf(o)
  expect_equal(unique(gg$source), "inbag")
})

test_that("gg_rhf S3 companions work", {
  gg <- gg_rhf(.rhf_pbc())
  expect_output(print(gg), "gg_rhf")
  expect_invisible(print(gg))
  s <- summary(gg)
  expect_true(is.list(s) || is.data.frame(s))
  expect_s3_class(autoplot(gg, idx = 1), "ggplot")
})
