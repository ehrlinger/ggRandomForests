# cran-comments.md is pasted verbatim into the CRAN submission form by
# devtools::submit_cran(), so anything left in it reaches the reviewer.
#
# This guards the two ways it goes wrong. A stale version heading is the common
# copy-paste error, and it tells CRAN you are submitting something you are not.
# An unfinished-work marker is the one that bit here: the win-builder results
# are filled in last, after the tarball is final, so the file legitimately
# carries a placeholder for a while. Nothing but a person's memory stopped that
# placeholder travelling to CRAN.
#
# Sibling of test_ggrandomforests_news.R, which pins NEWS against DESCRIPTION
# the same way.

cran_comments_path <- function() {
  p <- testthat::test_path("..", "..", "cran-comments.md")
  if (file.exists(p)) return(p)
  # Under R CMD check the source tree is not present; cran-comments.md is
  # .Rbuildignore'd and never installed, so there is nothing to check.
  NA_character_
}

test_that("cran-comments.md carries no unfinished-work markers", {
  skip_on_cran()
  p <- cran_comments_path()
  skip_if(is.na(p), "cran-comments.md not available (installed package)")

  txt <- readLines(p, warn = FALSE)
  markers <- grep("RELEASE GATE|\\bTBD\\b|\\bTODO\\b|FIXME|XXX", txt, value = TRUE)

  expect_equal(
    markers, character(0),
    info = paste0(
      "cran-comments.md still contains unfinished-work markers. It is pasted ",
      "verbatim into the CRAN submission form, so these would be sent to the ",
      "reviewer. Finish or remove them before submitting:\n  ",
      paste(markers, collapse = "\n  ")
    )
  )
})

test_that("cran-comments.md leads with the version being submitted", {
  skip_on_cran()
  p <- cran_comments_path()
  skip_if(is.na(p), "cran-comments.md not available (installed package)")

  desc_path <- system.file("DESCRIPTION", package = "ggRandomForests")
  if (!nzchar(desc_path)) desc_path <- testthat::test_path("..", "..", "DESCRIPTION")
  version <- unname(read.dcf(desc_path, fields = "Version")[1, 1])

  # The first "## vX.Y.Z" heading is the release being submitted; older
  # releases are kept below it as history.
  headings <- grep("^##+\\s*v?[0-9]", readLines(p, warn = FALSE), value = TRUE)
  skip_if(length(headings) == 0L, "no version headings in cran-comments.md")

  expect_match(
    headings[1], version, fixed = TRUE,
    info = paste0(
      "The first version heading in cran-comments.md is\n  ", headings[1],
      "\nbut DESCRIPTION says Version: ", version,
      "\nA stale heading tells CRAN you are submitting a different release."
    )
  )
})
