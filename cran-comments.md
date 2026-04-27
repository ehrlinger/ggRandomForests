## Resubmission after archiving

This is the first submission of `ggRandomForests` since the package was
archived from CRAN on **2025-07-01**. The archiving notice was *"issues
were not corrected in time."* The underlying issue was an upstream API
change: `randomForestSRC` removed its `var.select.rfsrc()` workflow that
several `ggRandomForests` helpers depended on. v2.7.1 resolves this by:

* Removing the `var.select`-based variable-selection paths.  Users are
  now directed to use minimum depth (already supported via
  `randomForestSRC::max.subtree`) or VIMP-based ranking (`gg_vimp`) until
  a `varPro`-based replacement is fleshed out in a future release.
* Updating all vignettes and examples to use the supported workflow.
* Fixing a separate batch of latent bugs documented in `NEWS.md` for
  v2.7.0 / v2.7.1 (S3 design overhaul, empty-figure bugs in survival
  partial-dependence, duplicate VIMP legend, NEWS-source consolidation,
  PDF-manual Unicode-minus issue, etc.).

## Test environments

* **Local:** R 4.5.3 on macOS Tahoe 26.4.1 (aarch64-apple-darwin20).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 2 informational
  NOTEs (CRAN incoming feasibility + local NTP timestamp; neither
  actionable on CRAN's own machines).
* **win-builder R-release** (R 4.6.0, x86_64-w64-mingw32): 2 NOTEs
  (see disposition below).
* **win-builder R-devel:** submitted; results pending at the time of
  this submission.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release) —
  all green on the head commit.
* **Reverse-dependency check:** `tools::package_dependencies(reverse =
  TRUE)` returns 0; no downstream CRAN packages depend on
  `ggRandomForests`.
* **URLs:** `urlchecker::url_check()` clean.

## NOTE disposition

### NOTE 1 — "New submission / Package was archived on CRAN"

Expected for a resubmission. The archiving root cause and its
remediation in v2.7.1 are documented above.

### NOTE 2 — Examples > 10s on win-builder R-release

Win-builder R-release flagged two examples that exceeded 10 s
cumulative CPU + elapsed (`gg_variable` 18.4 s; `gg_rfsrc.rfsrc`
10.4 s). Both have been trimmed in this submission: the headline
runnable example is now a small `ntree = 50` classification fit on
`iris`, and the larger regression / survival demonstrations are
guarded with `\donttest{}` so they only run under `--run-donttest`.

## Other notes

* `cran-comments.md` reflects the disposition documented above; no
  silent NOTEs remain.
* `NEWS.md` carries entries for both v2.7.0 (the API-change response)
  and v2.7.1 (bug-fix follow-ups). The v2.7.0 entry is included
  because that release was tagged in the source repository but never
  reached CRAN before archiving.
