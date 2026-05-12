## v2.7.3 — New features and documentation

This is a new submission following v2.7.2 (accepted to CRAN 2026-04-29).

### Changes in v2.7.3

- New `gg_brier()` extractor and `plot.gg_brier()` for time-resolved
  Brier scores and CRPS on survival forests.
- Visual unification of ribbon overlays across all plot methods.
- `print()`, `summary()`, and `autoplot()` S3 methods for all 10
  `gg_*` data classes.
- `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and
  `plot.gg_partialpro()` now always return a single `ggplot`/`patchwork`
  object; `patchwork` moved from `Suggests` to `Imports`.

## Test environments

* **Local:** R 4.5.3 on macOS Tahoe 26.4.1 (aarch64-apple-darwin20).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 0 notes.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release) —
  all green on the head commit.
* **Reverse-dependency check:** `tools::package_dependencies(reverse =
  TRUE)` returns 0.
* **URLs:** `urlchecker::url_check()` clean.

## NOTE disposition

No notes in local `R CMD check --as-cran`.
