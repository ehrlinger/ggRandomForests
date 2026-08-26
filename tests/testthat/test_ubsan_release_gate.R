test_that("the release probe rejects UBSAN diagnostics", {
  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = FALSE
  )
  if (!file.exists(file.path(source_root, ".git"))) {
    skip("repository-only release probe")
  }
  if (!file.exists(file.path(source_root, "DESCRIPTION"))) {
    skip("source-only release probe")
  }
  probe <- file.path(source_root, "tools", "check-upstream-ubsan.R")
  expect_true(file.exists(probe), info = "release probe must be tracked")

  old <- Sys.getenv("GGRF_UBSAN_SOURCE_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("GGRF_UBSAN_SOURCE_ONLY")
    } else {
      Sys.setenv(GGRF_UBSAN_SOURCE_ONLY = old)
    }
  })
  Sys.setenv(GGRF_UBSAN_SOURCE_ONLY = "true")
  source(probe, local = TRUE)

  clean_log <- tempfile()
  writeLines("Supported upstream workflows completed", clean_log)
  expect_invisible(assert_ubsan_clean(clean_log))

  bad_log <- tempfile()
  writeLines(
    "entry.c:184:55: runtime error: pointer index expression overflowed",
    bad_log
  )
  expect_error(
    assert_ubsan_clean(bad_log),
    "UndefinedBehaviorSanitizer diagnostic",
    fixed = TRUE
  )
  expect_invisible(assert_known_rfsrc_ubsan(bad_log))

  expect_error(
    assert_known_rfsrc_ubsan(clean_log),
    "known randomForestSRC UBSAN diagnostic",
    fixed = TRUE
  )
})

test_that("the release probe completes supported upstream workflows", {
  skip_on_cran()
  skip_if_not_installed("randomForestRHF")
  skip_if_not_installed("varPro")

  source_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = FALSE
  )
  if (!file.exists(file.path(source_root, ".git"))) {
    skip("repository-only release probe")
  }
  if (!file.exists(file.path(source_root, "DESCRIPTION"))) {
    skip("source-only release probe")
  }

  old <- Sys.getenv("GGRF_UBSAN_SOURCE_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("GGRF_UBSAN_SOURCE_ONLY")
    } else {
      Sys.setenv(GGRF_UBSAN_SOURCE_ONLY = old)
    }
  })
  Sys.setenv(GGRF_UBSAN_SOURCE_ONLY = "true")
  source(
    file.path(source_root, "tools", "check-upstream-ubsan.R"),
    local = TRUE
  )

  expect_invisible(run_supported_workflows())
})
