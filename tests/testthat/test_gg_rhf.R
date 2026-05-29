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
  expect_true(all(is.finite(gg$hazard) & gg$hazard >= 0))
  expect_true(all(is.finite(gg$chf) & gg$chf >= 0))
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
