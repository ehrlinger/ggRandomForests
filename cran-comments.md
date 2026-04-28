## v2.7.2 — Resubmission addressing CRAN reviewer feedback

This is a resubmission of v2.7.1 addressing every item raised by
Benjamin Altmann's review of 2026-04-27. All four requested changes have
been applied:

1. **References in DESCRIPTION.** Added Breiman (2001)
   `<doi:10.1023/A:1010933404324>` and Ishwaran et al. (2008)
   `<doi:10.1214/08-AOAS169>` to the Description field.
2. **`:::` in documentation.** `man/shift.Rd` had an example using
   `ggRandomForests:::shift(...)`. `shift()` is an internal utility, so
   the help page has been removed entirely (the function is now
   `@noRd`); the `:::` reference is gone.
3. **`cat()` in `R/surv_partial.rfsrc.R`.** Replaced the user-visible
   `cat("partial plot for: ", xvar, "\n")` with `message(...)` so the
   output can be suppressed via `suppressMessages()` and plays nicely
   inside notebooks / Shiny / quarto.
4. **`par()` reset in `man/surv_partial.rfsrc.Rd` example.** Wrapped
   the `par(mfrow = c(2, 2))` block with
   `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))` so the
   user's graphical parameters are restored on exit.

## Background — first resubmission since archiving

`ggRandomForests` was archived from CRAN on **2025-07-01** with the
notice *"issues were not corrected in time."* The root cause was an
upstream API change: `randomForestSRC` removed its
`var.select.rfsrc()` workflow that several `ggRandomForests` helpers
depended on. The fix in v2.7.x is to drop the `var.select`-based
variable-selection paths and direct users to minimum depth
(`randomForestSRC::max.subtree`) or VIMP-based ranking (`gg_vimp`)
until a `varPro`-based replacement is fleshed out in a future release.

## Test environments

* **Local:** R 4.5.3 on macOS Tahoe 26.4.1 (aarch64-apple-darwin20).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 2 informational
  NOTEs (CRAN incoming feasibility + local NTP timestamp; neither
  actionable on CRAN's own machines).
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release) —
  all green on the head commit.
* **Win-builder R-release / R-oldrel** (v2.7.1 tarball, prior to the
  reviewer feedback): both clean — Status 1 NOTE, the new-submission
  NOTE only.
* **Reverse-dependency check:** `tools::package_dependencies(reverse =
  TRUE)` returns 0; CRAN's auto-pretest also reported "No strong
  reverse dependencies to be checked".
* **URLs:** `urlchecker::url_check()` clean.

## NOTE disposition

### NOTE — "New submission / Package was archived on CRAN"

Expected for a resubmission after archiving. The root cause and
remediation in v2.7.x are documented above.
