# Release Checklist: ggRandomForests v2.7.0
**Date:** 2026-03-25 | **Maintainer:** John Ehrlinger <john.ehrlinger@gmail.com>

---

## What Changed Since 2.6.1

This release is a significant bug-fix and code-quality release. All changes should be summarised in NEWS.md before submission.

| Area | Fix |
|------|-----|
| `plot.gg_rfsrc.R` | **Breaking visual bug**: all `aes()` calls used bare string literals instead of `.data[[col]]` — plots mapped aesthetics to constant text, not data columns |
| `plot.gg_roc.R` | Multi-class `aes()` bare string literals fixed; dead `if (crv < 2)` branch removed |
| `gg_rfsrc.R` | `bootstrap_survival` negative-index bug fixed; `is.null(df[,col])` column-existence check fixed; `gg_rfsrc.randomForest` used non-existent `object$xvar` — now uses `.rf_recover_model_frame()` |
| `plot.gg_error.R` | Legend-suppression logic fixed for single-outcome forests; `=` → `<-` assignment style |
| `gg_vimp.R` | `1:nvar` → `seq_len(nvar)` in both `rfsrc` and `randomForest` methods (silent bug when `nvar == 0`) |
| `gg_partial.R` | `if(` → `if (` lintr spacing |
| Test suite | Full testthat 3.x migration: `expect_is` → `expect_s3_class/expect_type/expect_true(is.*())`, `expect_equivalent` → `expect_equal(ignore_attr=TRUE)`, all `context()` removed, `expect_that`/`is_identical_to` removed, `gg_roc.rfrsrc` typo fixed |
| GitHub Actions | `lint.yaml` upgraded (checkout@v4, fail-on-lint-errors, `.lintr` config created); `R-CMD-check.yaml` rtools 42→44, `error_on: warning` added; `test-coverage.yaml` duplicate codecov upload removed |

---

## Pre-Release Checklist

### 1. Version & Metadata
- [ ] Bump `Version:` in `DESCRIPTION` from `2.6.1` → `2.7.0`
- [ ] Update `Date:` in `DESCRIPTION` to today (`2026-03-25`)
- [ ] Add `covr` to `Suggests:` in `DESCRIPTION` (referenced in `test-coverage.yaml` but not declared)
- [ ] Confirm `RoxygenNote:` in `DESCRIPTION` matches your installed roxygen2 version (`devtools::document()` will warn if not)

### 2. NEWS.md
- [ ] Add `ggRandomForests v2.7.0` section at the top of `NEWS.md` summarising all the changes above
- [ ] Keep entries user-facing: "Fixed visual bug where all plot aesthetics were mapped to constant strings instead of data columns" is better than "fixed aes() calls"

### 3. Documentation
- [ ] Run `devtools::document()` — confirm zero warnings, all `.Rd` files regenerate cleanly
- [ ] Spot-check exported help pages: `?gg_rfsrc`, `?plot.gg_rfsrc`, `?gg_roc`, `?gg_vimp`
- [ ] Confirm `plot.gg_vimp` still has `1:nvar` in `R/plot.gg_vimp.R:77` — this was **not** fixed and should be added to Phase 2 or fixed now

### 4. Test Suite
- [ ] Run `devtools::test()` locally — zero failures, zero skips that aren't intentional
- [ ] Confirm no `expect_is()`, `expect_equivalent()`, or `context()` calls remain:
  ```r
  grep -r "expect_is\|expect_equivalent\|context(" tests/
  ```
- [ ] Check coverage with `covr::package_coverage()` — should be ≥ 83% (existing baseline)
- [ ] Verify `test_gg_roc.R` actually exercises `gg_roc.rfsrc()` error path (typo was `rfrsrc` → now fixed to `rfsrc`)

### 5. R CMD CHECK (local)
Run the full CRAN-equivalent check:
```r
devtools::check(args = c("--as-cran"))
```
- [ ] **0 errors** — hard gate, CRAN will reject
- [ ] **0 warnings** — hard gate (CI now enforces this with `error_on: "warning"`)
- [ ] **0 notes** — aim for this; if unavoidable, document in `cran-comments.md`
- [ ] Check output for `WARNING: no visible binding for global variable` — the `.data` pronoun fixes should have eliminated most, but verify
- [ ] Check for `NOTE: checking for unstated dependencies in examples` — confirm `MASS` and `datasets` are in `Suggests:`

### 6. Known Remaining Issues (Phase 2 — decide: fix now or document)
These were identified in the code review but not yet fixed. Decide before release:

- [ ] **`plot.gg_vimp.R:77`** — `gg_dta[1:nvar, ]` should be `seq_len(nvar)` (same class of bug fixed in `gg_vimp.R`) — **recommend fixing now**
- [ ] **`tidyr::gather()` → `pivot_longer()`** — `gather()` is superseded but still works; acceptable to defer if timeline is tight
- [ ] **No `vdiffr` snapshot tests** — visual regression testing gap; safe to defer to v2.8.0
- [ ] **`bootstrap_survival` unit tests** — no direct test for the CI band helper; safe to defer

### 7. CRAN Submission Prep
- [ ] Update `cran-comments.md` for v2.7.0:
  - List the R CMD CHECK result (0 errors | 0 warnings | 0 notes)
  - List test environments (local + GitHub Actions matrix)
  - Summarise the changes briefly for the CRAN reviewer
- [ ] Run `devtools::check_win_devel()` — submits to CRAN's own Windows R-devel server; results emailed within ~30 min
- [ ] Run `rhub::check_for_cran()` (or use the `rhub.yaml` workflow) to test on additional platforms
- [ ] Confirm no reverse-dependency breakage (no `revdep/` folder exists — if you have downstream users, consider `revdepcheck::revdep_check()`)

### 8. Git / GitHub
- [ ] All changes committed to `main`
- [ ] GitHub Actions are green on `main` (R-CMD-check, lint, test-coverage)
- [ ] Create and push a git tag: `git tag v2.7.0 && git push origin v2.7.0`
- [ ] Create a GitHub Release from the tag, copy the NEWS.md entry as the release notes

### 9. Submit to CRAN
```r
devtools::submit_cran()
```
- [ ] Confirm submission acknowledgement email received
- [ ] Watch for CRAN incoming queue status at https://cran.r-project.org/incoming/
- [ ] Respond to any CRAN reviewer queries within 14 days

---

## Rollback Triggers

If CRAN rejects or a critical issue is found post-release:
- [ ] Patch release `2.7.1` addressing the specific CRAN note/warning
- [ ] If a user-facing regression is reported: hotfix branch from `v2.7.0` tag, fix + test, release `2.7.1` within 24–48 h
- [ ] Re-submit with updated `cran-comments.md` explaining what was changed

---

## Quick Reference Commands

```r
# Full local CRAN check
devtools::check(args = "--as-cran")

# Regenerate documentation
devtools::document()

# Run tests with coverage
covr::package_coverage()

# CRAN Windows devel check
devtools::check_win_devel()

# Submit
devtools::submit_cran()
```
