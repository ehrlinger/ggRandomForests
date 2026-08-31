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

## ---- time_units plausibility (issue #251) ----------------------------------

test_that(".check_time_units warns on a year-like unit against day-scale values", {
  expect_warning(.check_time_units("years", c(30, 90, 1191)), "implausible for years")
  expect_warning(.check_time_units("yr", c(1191)), "implausible for years")
})

test_that(".check_time_units matches year-like units case-insensitively", {
  expect_warning(.check_time_units("Yrs", 1191), "implausible for years")
  expect_warning(.check_time_units("YEAR", 1191), "implausible for years")
})

test_that(".check_time_units is silent in the direction we deliberately skip", {
  # Small values with "days" is ordinary; there is no signal, so no check.
  expect_silent(.check_time_units("days", c(1, 3, 5)))
  # Day-scale values labelled days is correct.
  expect_silent(.check_time_units("days", c(30, 90, 1191)))
  # Year-scale values labelled years is correct.
  expect_silent(.check_time_units("years", c(1, 3, 5)))
})

test_that(".check_time_units compares factor LABELS, not integer codes", {
  # gg_dta$time is a factor. A single 1191 level has integer code 1, so a naive
  # comparison would test 1 > 150 and never fire on the case that motivated this.
  f <- factor(1191)
  expect_equal(as.integer(f), 1L)                      # the trap
  expect_warning(.check_time_units("years", f), "implausible for years")
})

test_that(".check_time_units skips the check when values are absent or unusable", {
  expect_silent(.check_time_units("years"))            # no values supplied
  expect_silent(.check_time_units("years", NULL))
  expect_silent(.check_time_units("years", numeric(0)))
  expect_silent(.check_time_units("years", factor(c("a", "b"))))  # non-numeric
})

test_that(".check_time_units keeps its existing type validation", {
  expect_null(.check_time_units(NULL))
  expect_equal(.check_time_units("days"), "days")
  expect_error(.check_time_units(character(0)), "single non-empty")
  expect_error(.check_time_units(NA_character_), "single non-empty")
  expect_error(.check_time_units(""), "single non-empty")
})

## ---- retired time args (issue #251) ----------------------------------------

test_that(".check_retired_time_args warns and names the call that works", {
  expect_warning(.check_retired_time_args(time = 1191), "gg_variable\\(rf, time =")
  expect_warning(.check_retired_time_args(time_labels = "a"),
                 "gg_variable\\(rf, time_labels =")
})

test_that(".check_retired_time_args is silent on everything else", {
  # A guard that fires on everything is as useless as one that fires on nothing,
  # and ... is rarely empty in a plot method.
  expect_silent(.check_retired_time_args())
  expect_silent(.check_retired_time_args(alpha = 0.3))
  expect_silent(.check_retired_time_args(alpha = 0.3, size = 2))
})
