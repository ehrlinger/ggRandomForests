# Tests for ggrandomforests.news

test_that("ggrandomforests.news NEWS file exists in package", {
  newsfile <- file.path(system.file(package = "ggRandomForests"), "NEWS")
  expect_true(file.exists(newsfile))
})

test_that("ggrandomforests.news is callable without error", {
  # file.show opens the file but should not throw an error
  expect_error(ggrandomforests.news(), NA)
})
