# Cross-file convention: a test_that() block that consumes the RNG must seed it
# itself. Forest fits, varPro grows and sample()/rnorm() are all RNG consumers,
# so an unseeded block is reproducible only by accident: testthat does not
# promise an execution order, and every earlier block advances the stream.
#
# This is the same kind of guard as test_plot_conventions.R and
# test_namespace_hygiene.R. It exists because 46 blocks had drifted into that
# state before anyone counted, and a one-time cleanup with nothing pinning it
# would drift straight back.
#
# A file-level set.seed() outside the block deliberately does NOT count.
#
# The rule is unconditional on purpose. A seed on a pure error-path test buys
# nothing, but "every block that touches the RNG seeds it" is a rule a reviewer
# can check in one pass, and the alternative ("seed it when the assertions
# depend on fitted values") needs a judgement call on every future test.

rng_consumers <- paste0(
  "^(rfsrc|randomForest|varpro|uvarpro|isopro|partialpro|beta\\.varpro|",
  "sample|rnorm|runif|rbinom|rpois)$"
)

# TRUE when any call anywhere inside `expr` has a function name matching
# `pattern`, ignoring any pkg:: prefix.
calls_any <- function(expr, pattern) {
  found <- FALSE
  recurse <- function(x) {
    if (found) return(invisible(NULL))
    if (is.call(x)) {
      nm <- sub("^.*::", "", paste(deparse(x[[1]]), collapse = ""))
      if (grepl(pattern, nm)) {
        found <<- TRUE
        return(invisible(NULL))
      }
    }
    if (is.recursive(x)) {
      for (i in seq_along(x)) try(recurse(x[[i]]), silent = TRUE)
    }
    invisible(NULL)
  }
  recurse(expr)
  found
}

is_test_that_call <- function(expr) {
  is.call(expr) &&
    identical(paste(deparse(expr[[1]]), collapse = ""), "test_that") &&
    length(expr) >= 3L
}

# The offending combination: the block reaches the RNG but never seeds it.
block_is_unseeded <- function(expr) {
  calls_any(expr[[3]], rng_consumers) && !calls_any(expr[[3]], "^set\\.seed$")
}

block_description <- function(expr) {
  tryCatch(as.character(expr[[2]]), error = function(e) NA_character_)
}

# Descriptions of every test_that() block in `file` that uses the RNG without
# seeding. Recurses, so blocks nested in local() are covered too.
unseeded_blocks <- function(file) {
  parsed <- tryCatch(parse(file), error = function(e) NULL)
  if (is.null(parsed)) {
    return(character(0))
  }
  offenders <- character(0)
  visit <- function(expr) {
    if (is_test_that_call(expr)) {
      desc <- block_description(expr)
      if (!is.na(desc) && block_is_unseeded(expr)) {
        offenders <<- c(offenders, desc)
      }
      return(invisible(NULL))
    }
    if (is.recursive(expr)) {
      for (i in seq_along(expr)) try(visit(expr[[i]]), silent = TRUE)
    }
    invisible(NULL)
  }
  for (expr in parsed) visit(expr)
  offenders
}

test_that("every RNG-consuming test_that() block seeds itself", {
  # Needs the test sources on disk. They are present under R CMD check, but
  # skip_on_cran() keeps a parser change on an old R from reddening CRAN for a
  # convention the maintainer can only fix here.
  skip_on_cran()

  files <- list.files(test_path("."), pattern = "^test_", full.names = TRUE)
  files <- files[grepl("[.]R$", files)]
  skip_if(length(files) == 0L, "test sources not available")

  offenders <- unlist(lapply(files, function(f) {
    descs <- unseeded_blocks(f)
    if (length(descs)) paste0(basename(f), ": ", descs) else character(0)
  }))

  expect_equal(
    sort(offenders), character(0),
    info = paste0(
      "These test_that() blocks consume the RNG without calling set.seed() ",
      "inside the block. Add a set.seed() call anywhere inside each block, ",
      "before the first RNG-consuming call. Placing it after skip_*() guards ",
      "is fine and is what most blocks here do:\n  ",
      paste(offenders, collapse = "\n  ")
    )
  )
})
