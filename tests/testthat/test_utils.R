test_that(".check_time_units accepts NULL and a single string, rejects the rest", {
  expect_null(.check_time_units(NULL))
  expect_identical(.check_time_units("days"), "days")

  expect_error(.check_time_units(1), "single non-empty character")
  expect_error(.check_time_units(c("days", "years")), "single non-empty character")
  expect_error(.check_time_units(NA_character_), "single non-empty character")
  expect_error(.check_time_units(""), "single non-empty character")
})

test_that(".survival_at_label omits the unit unless one is given", {
  expect_identical(.survival_at_label(1191, NULL), "Survival at 1191")
  expect_identical(.survival_at_label(1191, "days"), "Survival at 1191 days")

  ## gg_dta$time is a factor; the level label must be used, not the integer code.
  tf <- factor("1191", levels = c("1191", "2000"))
  expect_identical(.survival_at_label(tf[1], NULL), "Survival at 1191")
})

test_that(".time_axis_label omits the unit unless one is given", {
  expect_identical(.time_axis_label(NULL), "time")
  expect_identical(.time_axis_label("days"), "time (days)")
})
