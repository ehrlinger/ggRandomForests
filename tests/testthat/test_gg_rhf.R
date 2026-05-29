test_that(".gg_provenance recognises rhf objects", {
  o <- .rhf_pbc()
  prov <- ggRandomForests:::.gg_provenance(o)
  expect_equal(prov$source, "randomForestRHF")
  expect_equal(prov$ntree, o$ntree)
  expect_equal(prov$n, o$n)
})
