test_that(".varpro_rank_of matches exactly", {
  expect_equal(unname(.varpro_rank_of(c("bpd", "age"), c("age", "bpd"))), c(2, 1))
})

test_that(".varpro_rank_of returns Inf for unranked names", {
  expect_equal(unname(.varpro_rank_of("vis", c("age", "bpd"))), Inf)
})

test_that(".varpro_rank_of resolves one-hot names by digit suffix", {
  # get.topvars() gave sex0/sex1; part_dta carries the original 'sex'
  expect_equal(unname(.varpro_rank_of("sex", c("age", "sex0", "sex1"))), 2)
})

test_that(".varpro_rank_of prefers an exact match over a prefix match", {
  # 'age' must not be captured by 'age_group'; and a digit suffix is required,
  # so 'age_group' can never be a one-hot level of 'age'.
  expect_equal(unname(.varpro_rank_of("age", c("age_group", "age"))), 2)
})

test_that(".varpro_rank_of requires a digit suffix, not just a prefix", {
  # Pins the only thing stopping 'age' from ranking as a one-hot level of
  # 'age_group': the pattern requires trailing digits, so a name-only prefix
  # match is not accepted.
  expect_equal(unname(.varpro_rank_of("age", c("age_group"))), Inf)
})

test_that(".varpro_rank_of escapes regex metacharacters in the name", {
  # 're.gion' contains a literal '.', which is a regex metacharacter. Against
  # 'reXgion0' (where the '.' would match any character) there must be no
  # match; against 're.gion0' (the literal, escaped '.') there must be.
  expect_equal(unname(.varpro_rank_of("re.gion", c("reXgion0"))), Inf)
  expect_equal(unname(.varpro_rank_of("re.gion", c("re.gion0"))), 1)
})

test_that(".varpro_importance_order returns list order when object is NULL", {
  pd <- list(age = 1, bpd = 2, vis = 3)
  expect_equal(.varpro_importance_order(pd, NULL), c("age", "bpd", "vis"))
})

test_that(".varpro_importance_order ranks by get.topvars and appends the rest", {
  pd <- list(age = 1, bpd = 2, vis = 3)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("vis", "bpd"),
                        .package = "varPro")
  expect_equal(.varpro_importance_order(pd, fake), c("vis", "bpd", "age"))
})

test_that(".varpro_importance_order drops nothing", {
  pd <- list(a = 1, b = 2, c = 3, d = 4)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("c"), .package = "varPro")
  expect_setequal(.varpro_importance_order(pd, fake), names(pd))
  expect_equal(length(.varpro_importance_order(pd, fake)), 4L)
})

test_that(".varpro_importance_order keeps list order among unranked names", {
  pd <- list(z = 1, y = 2, x = 3)
  fake <- structure(list(), class = "varpro")
  local_mocked_bindings(get.topvars = function(...) c("y"), .package = "varPro")
  expect_equal(.varpro_importance_order(pd, fake), c("y", "z", "x"))
})
