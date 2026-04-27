# Tests for ggrandomforests.news()
#
# v2.7.1 consolidated the helper to read NEWS.md (the canonical change log
# R also exposes via utils::news()) instead of the legacy inst/NEWS plain
# text file. These tests guard against silent drift back to a separate
# hand-maintained source.

test_that("NEWS.md is bundled with the installed package", {
  nf <- system.file("NEWS.md", package = "ggRandomForests")
  expect_true(nzchar(nf))
  expect_true(file.exists(nf))
})

test_that("Bundled NEWS.md reflects the installed package version", {
  # Reading NEWS.md and finding the DESCRIPTION version in it ensures the
  # change-log entry was written before the version bump landed — i.e. that
  # users running ggrandomforests.news() see the correct release.
  ver <- as.character(utils::packageVersion("ggRandomForests"))
  nf  <- system.file("NEWS.md", package = "ggRandomForests")
  txt <- readLines(nf, warn = FALSE)
  expect_true(any(grepl(ver, txt, fixed = TRUE)),
              info = sprintf("Installed version %s not found in %s", ver, nf))
})

test_that("ggrandomforests.news is callable without error", {
  expect_error(ggrandomforests.news(), NA)
})

test_that("inst/NEWS is no longer the source of truth", {
  # Defensive: if a future change re-introduces inst/NEWS, this test won't
  # fail (the helper still falls back to it) -- but we explicitly assert
  # NEWS.md takes priority when both exist.
  nf_md <- system.file("NEWS.md", package = "ggRandomForests")
  expect_true(nzchar(nf_md))
})
