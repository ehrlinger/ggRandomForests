# Tests for varpro_feature_names

test_that("varpro_feature_names returns exact matches unchanged", {
  dataset <- data.frame(age = 1:5, sex = 1:5, weight = 1:5)
  varpro_names <- c("age", "sex", "weight")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_true("age" %in% result)
  expect_true("sex" %in% result)
  expect_true("weight" %in% result)
})

test_that("varpro_feature_names resolves single-suffix one-hot names", {
  dataset <- data.frame(
    age = 1:5,
    sex = c("M", "F", "M", "F", "M"),
    weight = 1:5
  )
  # sex was one-hot encoded to sex0 and sex1
  varpro_names <- c("age", "sex0", "sex1")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_true("age" %in% result)
  expect_true("sex" %in% result)
  # Encoded versions should NOT appear
  expect_false("sex0" %in% result)
  expect_false("sex1" %in% result)
})

test_that("varpro_feature_names resolves multi-level one-hot names", {
  # grade0, grade1, grade2 → grade
  dataset <- data.frame(
    age = 1:10,
    grade = letters[1:10]
  )
  varpro_names <- c("age", "grade0", "grade1", "grade2")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_true("age" %in% result)
  expect_true("grade" %in% result)
  expect_false("grade0" %in% result)
  expect_false("grade1" %in% result)
  expect_false("grade2" %in% result)
})

test_that("varpro_feature_names handles deeply nested suffixes", {
  # groupABC → group (three characters to strip)
  dataset <- data.frame(group = 1:5, value = 1:5)
  varpro_names <- c("value", "groupABC")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_true("value" %in% result)
  expect_true("group" %in% result)
  expect_false("groupABC" %in% result)
})

test_that("varpro_feature_names returns only unique names", {
  dataset <- data.frame(sex = 1:5, age = 1:5)
  # Both sex0 and sex1 resolve to sex — should appear once
  varpro_names <- c("sex0", "sex1", "age")

  result <- varpro_feature_names(varpro_names, dataset)

  # sex appears exactly once
  expect_equal(sum(result == "sex"), 1)
  expect_true("age" %in% result)
})

test_that("varpro_feature_names with all direct matches returns them", {
  dataset <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  varpro_names <- c("a", "b", "c")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_equal(sort(result), sort(varpro_names))
})

test_that("varpro_feature_names with empty input returns empty", {
  dataset <- data.frame(a = 1:3)
  result <- varpro_feature_names(character(0), dataset)

  expect_length(result, 0)
})

test_that("varpro_feature_names mix of direct and encoded names", {
  dataset <- data.frame(
    age = 1:20,
    smoking = 1:20,
    region = 1:20
  )
  # smoking is encoded, age and region are direct
  varpro_names <- c("age", "smoking0", "smoking1", "region")

  result <- varpro_feature_names(varpro_names, dataset)

  expect_true("age" %in% result)
  expect_true("smoking" %in% result)
  expect_true("region" %in% result)
  expect_false("smoking0" %in% result)
  expect_false("smoking1" %in% result)
})
