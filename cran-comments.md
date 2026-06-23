## v3.2.0 — minor release (RMST partial dependence; bug fixes)

This is a minor feature-and-fix release for the varPro survival workflow.

### What's new / fixed

* `gg_partial_varpro(scale = "rmst", time = tau)` now *drives* the survival
  partial-dependence computation through an RMST(tau) learner
  (RMST(tau) = integral_0^tau S(t) dt), rather than only relabeling the
  default ensemble-mortality curve. `varPro::partialpro()` exposes no time
  argument, so multi-horizon RMST plots built the old way differed only by
  Monte-Carlo noise, not by tau; the learner makes the curve genuinely
  tau-dependent. Guardrails warn when a precomputed `part_dta` cannot be
  driven by tau, when tau exceeds the model's largest event time (RMST is
  truncated there), and when `time` is supplied to a scale that ignores it.
* `gg_partial_varpro()` (and the deprecated `gg_partialpro()` alias) now
  forward `...` to `varPro::partialpro()` on the object-driven path, restoring
  control over which variables are computed (`xvar.names`, `nvar`) and the
  isolation-forest step (`cut`, `nsmp`, ...).
* `gg_varpro()` now fails with a clear message instead of a cryptic
  "differing number of rows" error when `varPro::importance()` returns a
  degenerate importance table (issue #118).
* `Imports` requires `varPro (>= 3.1.0)` (the version exposing the
  `partialpro()` `learner` argument the RMST path relies on).

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` (with the manual) returns 0 errors, 0 warnings,
  0 notes.
* **win-builder:** R-devel, R-release, and R-oldrelease (Windows Server 2022,
  x86_64) — all Status: OK (0 errors, 0 warnings, 0 notes).
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release).
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.

### NOTE disposition

`R CMD check --as-cran` is clean (0/0/0) locally. No notes expected beyond the
usual incoming-feasibility items (e.g. maintainer/timestamp), if any.

The gcc-UBSAN guard from v3.1.1/v3.1.2 is unchanged: the single unsupervised
`varPro::isopro(method = "unsupv")` test still calls `skip_on_cran()` to avoid
an upstream `randomForestSRC` sanitizer report (`rfsrcGrow`, `entry.c:184`);
all other varPro tests run. ggRandomForests remains a pure-R package
(`NeedsCompilation: no`).
