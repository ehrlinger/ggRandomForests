This is ggRandomForests package submission v2.7.1
-------------------------------------------------------------------------
This is a bug-fix release. Key changes:

* Fix `gg_partial_rfsrc()` for survival forests: `partial.rfsrc()` is now
  called with `partial.type = "surv"` (default; also accepts `"chf"` /
  `"mort"`). Without this, a zero-length comparison inside the underlying
  C code aborted the call and left the survival-vignette partial-dependence
  chunks empty.
* Fix `gg_partial_rfsrc()` for multiple `partial.time` values: yhat is
  reshaped from the matrix returned by `get.partial.plot.data()` into
  long form so each `(x, time)` pair is one row.
* Improve `plot.gg_partial_rfsrc()` survival layout: predictor on the
  x-axis with one curve per time point coloured by `Time`, faceted by
  variable name. Group/colour by full-precision time so distinct horizons
  that round to the same display value never collapse.
* `plot.gg_partial_rfsrc()` y-axis label now adapts to `partial.type`
  (`"Predicted Survival"` / `"Predicted CHF"` / `"Predicted Mortality"`).
* `plot.gg_vimp()` legend de-duplication: `fill` and `color` aesthetics
  share a "VIMP > 0" title so ggplot merges what was previously two
  legends.
* `ggrandomforests.news()` now reads `NEWS.md` (the canonical change log
  R also exposes via `utils::news()`); removes the stale hand-maintained
  `inst/NEWS`.
* Replace U+2212 Unicode minus with ASCII `-` in `calc_roc.R` and
  `plot.gg_roc.R` so the PDF manual builds under standard LaTeX.
* New regression test file `test_plot_layer_data.R` uses
  `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders
  non-empty layers across all forest families, catching empty-figure
  regressions without visual inspection.

## R CMD check results

`R CMD check --as-cran` on local R 4.5.3 (macOS aarch64): 0 errors |
0 warnings | 2 NOTEs.

The 2 NOTEs are both informational and not actionable for submission:

* `checking CRAN incoming feasibility ... NOTE` — the standard maintainer
  / tarball-size NOTE that always appears under `--as-cran`.
* `checking for future file timestamps ... NOTE` — "unable to verify
  current time", a local NTP-reachability NOTE that does not appear on
  CRAN's own check infrastructure.

`urlchecker::url_check()` is clean (0 broken URLs across DESCRIPTION, Rd
files, NEWS.md, README.md, and vignettes).

## Test environments

* local: R 4.5.3 on macOS Tahoe 26.4.1 (aarch64-apple-darwin20)
* GitHub Actions matrix on commit 570aad7:
  * ubuntu-latest (R-devel, R-release, R-oldrel-1)
  * windows-latest (R-release)
  * macos-latest (R-release)
* `devtools::check_win_devel()` and `check_win_release()` — pending,
  results to be appended below before submission.
* `revdepcheck::revdep_check()` — ggRandomForests has no reverse
  dependencies on CRAN; running anyway for completeness, results to be
  appended below.
