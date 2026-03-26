# ggRandomForests — Code Review & Testing Strategy
_Pre-release review against v2.6.1 codebase — March 2026_

---

## Part 1: Testing Strategy

### Current State

The suite has 15 test files covering all major exported functions. The high-level
picture is good — rfsrc + randomForest, regression + classification + survival
paths, error handling — but there are systematic gaps that mean genuine bugs
can and do pass undetected.

**Coverage summary**

| File | Status | Notable gaps |
|---|---|---|
| `test_gg_error.R` | Solid | No message-text checks on `expect_error` |
| `test_gg_rfsrc.R` | Excellent (17 cases) | Plots tested structurally only, not visually |
| `test_gg_vimp.R` | Good | `gg_dta[1:nvar, ]` path untested for nvar=0 |
| `test_gg_roc.R` | Decent | Typo in function name silently voids two tests (see bugs) |
| `test_gg_survival.R` | Thin (3 cases) | No column-structure checks, no error-message validation |
| `test_gg_variable.R` | Good | — |
| `test_gg_partial.R` | Good (mock-based) | — |
| `test_gg_partialpro.R` | Good (mock-based) | — |
| `test_surv_partial.R` | Reasonable | `npts` test doesn't verify actual point count |
| `test_randomForest_helpers.R` | Good | — |
| `test_varpro_feature_names.R` | Thorough | — |
| `test_quantile_pts.R` | Basic | No edge cases (n=1, all-identical values) |
| `test_shift.R` | **Broken API** | Uses `expect_that`/`is_identical_to` (testthat 1.x — removed) |
| `test_lint.R` | **Dead** | Test body is commented out |
| `test_ggrandomforests_news.R` | Trivial | — |

### Gap 1 — Deprecated testthat API throughout

Every test file uses one or more removed/deprecated calls:

- `context("...")` — deprecated in testthat 3.0; harmless now but will break in a future release
- `expect_is(x, "cls")` — deprecated; use `expect_s3_class(x, "cls")`
- `expect_equivalent(a, b)` — deprecated; use `expect_equal(a, b, ignore_attr = TRUE)`
- `test_shift.R`: `expect_that(x, is_identical_to(y))` — from testthat 1.x, removed entirely

**Action:** global find-and-replace across all test files.

### Gap 2 — Plots are tested structurally, not visually

Every `plot.*` test does `expect_s3_class(gg_plt, "ggplot")`. A ggplot object can
be created successfully even when all aesthetics map to bare string literals
(which produces a broken plot). This means the three broken `aes()` bugs
described in Part 2 pass the test suite silently.

**Action:** Adopt `vdiffr::expect_doppelganger()` for at least one snapshot per
plot function. Minimum targets: regression scatter, classification jitter,
survival step curve with and without CI ribbon.

```r
# Example
test_that("plot.gg_rfsrc survival CI ribbon snapshot", {
  vdiffr::expect_doppelganger(
    "rfsrc-surv-ci",
    plot(gg_rfsrc(rfsrc_veteran, conf.int = 0.95))
  )
})
```

### Gap 3 — `bootstrap_survival` has zero direct tests

The function is only tested via `gg_rfsrc(..., conf.int = ...)`. There are no
unit tests that verify the output columns (`value`, `lower`, `upper`, `median`,
`mean`), that `lower <= median <= upper` holds, or that a two-element
`level_set` produces the right column names.

**Action:** Add a `test_bootstrap_survival.R` that calls
`ggRandomForests:::bootstrap_survival(gg_t, bs_samples, level_set)` directly.

### Gap 4 — `gg_survival` column structure untested

`test_gg_survival.R` checks the class and then runs 7 `plot()` type
combinations, but never verifies that the returned data frame actually has the
expected columns (`time`, `surv`, `lower`, `upper`, `n.risk`, etc.).

**Action:**
```r
expect_true(all(c("time", "surv", "lower", "upper", "n.risk") %in%
                  colnames(gg_dta)))
```

### Gap 5 — Two `expect_error` calls test a non-existent function

In `test_gg_roc.R` lines 59–60 and 102–103:

```r
expect_error(gg_roc.rfrsrc(rf_iris))   # typo: "rfrsrc" not "rfsrc"
```

`gg_roc.rfrsrc` does not exist, so this always errors with
`"could not find function"` — it vacuously passes regardless of whether input
validation works correctly.

**Action:** Fix the typo to `gg_roc.rfsrc` and make the error message explicit.

### Gap 6 — Error message text is never asserted

Across all test files, `expect_error(fn(...))` is used without asserting the
message. A completely different error (e.g., NULL dereference) would pass.

**Action:** For each validated error path, pin the message:
```r
expect_error(gg_vimp(bad_obj), "This function only works for")
expect_error(gg_rfsrc(rfsrc_boston, by = c(1,2,3)), "correct dimension")
```

### Gap 7 — Missing `set.seed()` and unbounded `ntree`

Many tests build forests without `ntree` (defaulting to 500), and without
`set.seed()`. This makes the suite slow (CI times matter), non-reproducible
for debugging, and fragile against stochastic variation.

**Action:** Every test that builds a forest should open with `set.seed(42)` and
pass `ntree = 50` (or `ntree = 75` max).

### Gap 8 — `kaplan.R` and `nelson.R` have no tests

Both helpers are used by `gg_survival` but have zero direct test coverage.

### Gap 9 — `nvar = 0` path for `gg_vimp` untested

`gg_dta[1:nvar, ]` with `nvar = 0` returns a 0-row data frame (fine in R), but
`gg_dta[1:0, ]` also works because `1:0` is `c(1, 0)` — two rows — which is
actually a silent bug. The `seq_len(nvar)` fix applied elsewhere was not applied
here.

---

## Part 2: Code Review

Severity scale: 🔴 Bug (broken output) / 🟠 Logic error (usually silent) /
🟡 Code smell / ⚪ Style

---

### 🔴 BUG — `plot.gg_rfsrc.R`: every `aes()` uses bare string literals

This is the most widespread breakage in the package. A bare string in `aes()`
maps the aesthetic to a *constant* — all points end up at the same position.

**Affected lines** (current file, after recent edits):

```r
# Classification binary — line ~169
ggplot2::aes(
  x = 1,
  y = colnames(gg_dta)[1],          # ← string, not column ref
  color = colnames(gg_dta)[2],
  shape = colnames(gg_dta)[2]
)
# Fix:
ggplot2::aes(
  x = 1,
  y = .data[[colnames(gg_dta)[1]]],
  color = .data[[colnames(gg_dta)[2]]],
  shape = .data[[colnames(gg_dta)[2]]]
)
```

```r
# Classification multi-class — line ~195
ggplot2::aes(x = "variable", y = "value")
ggplot2::aes(color = "y", shape = "y")
# Fix:
ggplot2::aes(x = .data$variable, y = .data$value)
ggplot2::aes(color = .data$y, shape = .data$y)
```

```r
# Survival with CI, grouped — lines ~220–232
ggplot2::aes(x = "value", ymin = "lower", ymax = "upper", fill = "group")
ggplot2::aes(x = "value", y = "median", color = "group")
# Fix:
ggplot2::aes(x = .data$value, ymin = .data$lower, ymax = .data$upper,
             fill = .data$group)
ggplot2::aes(x = .data$value, y = .data$median, color = .data$group)
```

```r
# Survival with CI, ungrouped — lines ~237–244
ggplot2::aes(x = "value", ymin = "lower", ymax = "upper")
ggplot2::aes(x = "value", y = "median")
# Fix: same pattern
```

```r
# Survival no CI — lines ~250–255
ggplot2::aes(x = "variable", y = "value", col = "event", by = "obs_id")
# Fix:
ggplot2::aes(x = .data$variable, y = .data$value,
             col = .data$event, group = .data$obs_id)
# Note: "by" is not a ggplot2 aes; use "group" for geom_step
```

```r
# Regression grouped — line ~268
ggplot2::aes(x = "group", y = "yhat")
# Regression ungrouped — line ~271
ggplot2::aes(x = 1, y = "yhat")
# Fix:
ggplot2::aes(x = .data$group, y = .data$yhat)
ggplot2::aes(x = 1, y = .data$yhat)
```

Additionally, `geom_boxplot(ggplot2::aes(x = 1, y = colnames(gg_dta)[1]))` on
line ~176 has the same string-literal problem.

---

### 🔴 BUG — `plot.gg_roc.R`: multi-class aes() uses bare strings

```r
# line ~168–170
ggplot2::aes(
  x = .data$fpr,
  y = .data$sens,
  linetype = "outcome",   # ← constant string, not column
  col = "outcome"
)
# Fix:
ggplot2::aes(
  x = .data$fpr,
  y = .data$sens,
  linetype = .data$outcome,
  col = .data$outcome
)
```

---

### 🟠 LOGIC ERROR — `bootstrap_survival`: nonsensical negative indexing

```r
# Current (lines ~475–479):
dta <- data.frame(cbind(
  time_interest,
  t(rng)[-which(colnames(gg_dta) %in% c("obs_id", "event")), ],
  mn[-which(colnames(gg_dta) %in% c("obs_id", "event"))]
))
```

`t(rng)` has `n_time_points` rows. The negative index is derived from
`colnames(gg_dta)`, which has `n_time_points + 2` (or +3 with `group`)
columns. The positions of `"obs_id"` and `"event"` always exceed `n_time`,
so this negative index is a no-op in the common case — but the intent is
wrong, the code is misleading, and for a dataset with two or three time
points it would silently drop real rows.

**Fix:**
```r
dta <- data.frame(cbind(time_interest, t(rng), mn))
```
The `gg_t` that feeds `mn_bs` already has `obs_id`/`event`/`group` stripped.

---

### 🟠 LOGIC ERROR — `gg_rfsrc.rfsrc` and `.randomForest`: `is.null(df[, col])` does not detect missing columns

```r
# Line 216 / 520:
if (is.null(object$xvar[, grp])) { ... }
```

`df[, "nonexistent_col"]` in R throws `"undefined columns selected"`, it does
not return NULL. The intended check should be:

```r
if (!grp %in% colnames(object$xvar)) {
  stop(paste("No column named", grp, "in forest training set."))
}
grp <- object$xvar[, grp]
```

Additionally, `gg_rfsrc.randomForest` references `object$xvar` which **does not
exist** on `randomForest` objects. The `by` character-name lookup path for
randomForest would throw an obscure subscript error rather than an informative
message.

---

### 🟠 LOGIC ERROR — `plot.gg_error.R`: legend-suppression check uses wrong column name

```r
# Line ~247:
if (length(unique(gg_dta$variable)) == 1) {
  gg_plt <- gg_plt + ggplot2::theme(legend.position = "none")
}
```

For single-outcome forests (regression, survival), `gg_dta` is not gathered —
it has a column named `"error"`, not `"variable"`. `gg_dta$variable` is NULL,
so `length(unique(NULL)) == 0 != 1`, and the legend-suppression block never
fires. Single-outcome plots show a redundant legend.

**Fix:**
```r
# After both branches:
if (ncol(x) <= 2) {   # single outcome: no legend needed
  gg_plt <- gg_plt + ggplot2::theme(legend.position = "none")
}
```

Or check directly: `if (!"variable" %in% names(gg_dta) || length(unique(gg_dta$variable)) <= 1)`.

---

### 🟠 LOGIC ERROR — `gg_vimp.rfsrc` and `.randomForest`: `1:nvar` instead of `seq_len(nvar)`

```r
# gg_vimp.rfsrc line ~281:
gg_dta <- gg_dta[1:nvar, ]

# gg_vimp.randomForest line ~429:
gg_dta <- gg_dta[1:nvar, ]
```

If `nvar = 0`, `1:0` evaluates to `c(1, 0)`, returning two rows. This was
already fixed in the loop-iteration context elsewhere in the package but missed
here.

**Fix:** `gg_dta <- gg_dta[seq_len(nvar), ]`

---

### 🟠 LOGIC ERROR — `plot.gg_rfsrc.R` survival no-CI branch: `by = "obs_id"` is not a ggplot2 aesthetic

```r
ggplot2::aes(x = "variable", y = "value", col = "event", by = "obs_id")
```

`by` is not a recognised `ggplot2` aesthetic argument (it belongs to
`ggplot2::geom_line` in some versions but not `geom_step`). The correct
grouping aesthetic for step functions is `group`:

```r
ggplot2::aes(x = .data$variable, y = .data$value,
             col = .data$event, group = .data$obs_id)
```

---

### 🟡 CODE SMELL — Massive duplication: `by`-argument resolution block

The 30-line block that validates and resolves the `by` argument is copy-pasted
verbatim between `gg_rfsrc.rfsrc` and `gg_rfsrc.randomForest`. Same pattern in
`gg_vimp.rfsrc` and `gg_vimp.randomForest` for `which.outcome` handling.

**Action:** Extract to internal helpers:
```r
.resolve_by <- function(by, xvar) { ... }
.resolve_which_outcome <- function(which.outcome, gg_dta) { ... }
```

---

### 🟡 CODE SMELL — `tidyr::gather()` still used in four source files

`gather()` is superseded (not just deprecated) by `pivot_longer()` since tidyr
1.0 (2019). It still works but will eventually be removed and prints a lifecycle
warning on newer tidyr versions.

Files affected: `gg_rfsrc.R`, `gg_vimp.R`, `plot.gg_error.R`, `plot.gg_rfsrc.R`.

---

### 🟡 CODE SMELL — `geom_jitter(, ...)` has a stray comma

```r
# plot.gg_rfsrc.R line ~275:
gg_plt <- gg_plt +
  ggplot2::geom_jitter(, ...) +
```

The leading comma before `...` is syntactically valid in R but is clearly a
typo — the first positional argument to `geom_jitter` is `mapping`. This should
be `ggplot2::geom_jitter(...)`.

---

### 🟡 CODE SMELL — `geom_jitter` and `geom_boxplot` receive `...` in the same call

In the classification and regression branches, `...` is passed to both
`geom_jitter(...)` and `geom_boxplot(...)`. Arguments like `alpha` or `size`
may not be meaningful for both geoms and could produce warnings or silently
ignored args.

---

### 🟡 CODE SMELL — `plot.gg_roc` multi-class AUC annotation unreachable

```r
# Line ~183:
if (crv < 2) {
```

`crv` is the number of classes and is only defined in the branch that builds the
multi-class list (`crv > 2 && is.null(which_outcome)`). `crv < 2` in a
multi-class context is impossible. The AUC annotation for the multi-class path
is dead code.

---

### 🟡 CODE SMELL — `gg_rfsrc.rfsrc` builds `arg_list` but `by` is accessed via `missing(by)`, not via `arg_list`

`by` is a named parameter, not in `...`, so `arg_list <- list(...)` does not
capture it. This is fine — but `conf.int`, `surv_type`, and `bs.sample` **are**
in `...` and are accessed via `arg_list`. The asymmetry is confusing and should
be documented clearly.

---

### ⚪ STYLE — `point = FALSE` should be `point <- FALSE`

```r
# plot.gg_error.R line ~214:
point = FALSE
```

Using `=` for assignment at the top level is valid but violates the package's
own style guide (and lintr's default rules). Should be `<-`.

---

### ⚪ STYLE — `if(length(...) == 0)` spacing

```r
# gg_partial.R line ~94:
if(length(cat_list) == 0) {
```

Missing space after `if`. Minor but lintr will flag it.

---

## Summary: Prioritised fix list

| # | Severity | File | Issue |
|---|---|---|---|
| 1 | 🔴 | `plot.gg_rfsrc.R` | All `aes()` aesthetics use bare string literals — plots are visually broken |
| 2 | 🔴 | `plot.gg_roc.R` | Multi-class `aes()` uses bare string literals |
| 3 | 🟠 | `bootstrap_survival` in `gg_rfsrc.R` | Nonsensical negative indexing — silent no-op now, latent crash for small datasets |
| 4 | 🟠 | `gg_rfsrc.rfsrc` + `.randomForest` | `is.null(df[, col])` does not detect missing columns; `randomForest` has no `$xvar` |
| 5 | 🟠 | `plot.gg_error.R` | Legend suppression uses wrong column name — legend always shown for single-outcome |
| 6 | 🟠 | `gg_vimp.rfsrc` + `.randomForest` | `1:nvar` instead of `seq_len(nvar)` — returns 2 rows when `nvar=0` |
| 7 | 🟠 | `plot.gg_rfsrc.R` | `by = "obs_id"` is not a ggplot2 aesthetic — should be `group` |
| 8 | 🟡 | `gg_rfsrc.R` | Duplicate `by`-resolution block — extract to helper |
| 9 | 🟡 | `gg_vimp.R` | Duplicate `which.outcome` block — extract to helper |
| 10 | 🟡 | 4 files | `tidyr::gather()` → `pivot_longer()` |
| 11 | 🟡 | `plot.gg_rfsrc.R` | Stray comma in `geom_jitter(, ...)` |
| 12 | 🟡 | `plot.gg_roc.R` | Dead `if (crv < 2)` AUC annotation branch |
| 13 | ⚪ | Test suite | All deprecated API calls (`expect_is`, `expect_equivalent`, `context`, `expect_that`) |
| 14 | ⚪ | Test suite | Missing `set.seed()` + unbounded `ntree` in many tests |
| 15 | ⚪ | Test suite | Zero visual/snapshot tests for a visualization package |
| 16 | ⚪ | `test_gg_roc.R` | Typo `gg_roc.rfrsrc` voids two tests silently |
| 17 | ⚪ | `test_lint.R` | Lintr check is commented out entirely |
