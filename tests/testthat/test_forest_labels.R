test_that(".forest_labels returns NULL for NULL", {
  expect_null(.forest_labels(NULL))
})

test_that(".forest_labels accepts a named character vector", {
  out <- .forest_labels(c(bpd_last = "BP Diastole", vis_last = "VIS"))
  expect_equal(out[["bpd_last"]], "BP Diastole")
  expect_equal(out[["vis_last"]], "VIS")
})

test_that(".forest_labels reads attr(col, 'label') from a labelled data frame", {
  d <- data.frame(age = 1:3, bpd = 4:6)
  attr(d$age, "label") <- "Age at operation"
  attr(d$bpd, "label") <- "BP Diastole"
  out <- .forest_labels(d)
  expect_equal(out[["age"]], "Age at operation")
  expect_equal(out[["bpd"]], "BP Diastole")
})

test_that(".forest_labels accepts a key/label data frame", {
  m <- data.frame(key = c("age", "bpd"),
                  label = c("Age at operation", "BP Diastole"),
                  stringsAsFactors = FALSE)
  out <- .forest_labels(m)
  expect_equal(out[["bpd"]], "BP Diastole")
})

test_that(".forest_labels prefers the key/label shape over attribute reading", {
  m <- data.frame(key = "age", label = "From key/label", stringsAsFactors = FALSE)
  attr(m$key, "label") <- "From attribute"
  out <- .forest_labels(m)
  expect_equal(out[["age"]], "From key/label")
})

test_that(".forest_labels warns when nothing resolves", {
  d <- data.frame(age = 1:3, bpd = 4:6)   # no label attributes
  expect_warning(.forest_labels(d), "No variable labels")
})

test_that(".forest_labels rejects an unnamed character vector", {
  expect_error(.forest_labels(c("BP Diastole")), "must be a named character vector")
})

test_that(".apply_forest_labels falls back per variable", {
  lookup <- c(bpd_last = "BP Diastole")
  out <- .apply_forest_labels(c("bpd_last", "vis_last"), lookup)
  expect_equal(out, c("BP Diastole", "vis_last"))
})

test_that(".apply_forest_labels is identity when lookup is NULL", {
  expect_equal(.apply_forest_labels(c("a", "b"), NULL), c("a", "b"))
})

test_that(".forest_strip_labeller builds a labeller that renames and falls back", {
  lb <- .forest_strip_labeller(c(bpd = "BP Diastole"))
  expect_true(is.function(lb))
  out <- lb(list(name = c("bpd", "vis")))
  expect_equal(unname(unlist(out)), c("BP Diastole", "vis"))
})

test_that(".forest_strip_labeller with NULL labels leaves names unchanged", {
  lb <- .forest_strip_labeller(NULL)
  out <- lb(list(name = c("bpd", "vis")))
  expect_equal(unname(unlist(out)), c("bpd", "vis"))
})
