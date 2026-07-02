## v3.4.0 — minor release (interpretable varPro partial scales; unsupervised varPro wrappers; factor partial-dependence fix)

This is a minor feature-and-fix release. It consolidates the work developed
since the CRAN 3.2.0 release (the 3.3.0 and 3.4.0 development cycles) into a
single submission.

### What's new / fixed

* **Interpretable varPro partial-plot scales.** `gg_partial_varpro()` now
  defaults to bounded, interpretable y-axes: probability P(Y = target class)
  for classification, and survival probability S(tau) for survival (via a
  partialpro survival learner), with tau defaulting to the median follow-up
  time when omitted. The unbounded ensemble-mortality scale remains available
  via `scale = "mortality"`. The `causal` (virtual-twins) contrast is hidden on
  the bounded scales and documented.
* **Unsupervised varPro wrappers.** New `gg_beta_uvarpro()` (lasso-importance
  ranking from `varPro::get.beta.entropy()`) and `gg_sdependent()`
  (signal-variable detection from `varPro::sdependent()`), each with
  `plot`/`print`/`summary`/`autoplot` methods, complementing the existing
  `gg_udependent()` dependency graph. A new short "uvarpro" vignette walks the
  three unsupervised views on a single `uvarpro()` fit.
* **Bug fix: factor partial dependence in `gg_partial_rfsrc()`.** The wrapper
  passed factor *labels* to `randomForestSRC::partial.rfsrc()`, which imposes a
  level by its integer code; character labels became `NA` and numeric-looking
  labels went out of range, collapsing every level to a single value (a flat
  categorical partial plot). It now passes the integer codes and relabels the
  output, matching `plot.variable(partial = TRUE)`. The categorical `x` is
  returned as a factor in the model's level order.
* Documentation: fixed the main vignette's `\VignetteIndexEntry` (it carried a
  template placeholder); split the unsupervised varPro material out of the
  varPro vignette into the new companion vignette.

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` (with the manual) returns 0 errors, 0 warnings,
  0 notes.
* **win-builder:** R-devel (R 90199), R-release (4.6.1), and R-oldrelease
  (4.5.3), Windows Server 2022, x86_64 — all Status: OK.
* **macOS:** covered by the local aarch64-apple-darwin23 check above. The
  mac.r-project.org macOS builder was unavailable at submission time
  (HTTP 502), so no mac-builder result is included; the macOS platform is
  also exercised by the macos-latest GitHub Actions job below.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release).
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.

### NOTE disposition

`R CMD check --as-cran` is clean (0/0/0) locally. The only note expected at
incoming feasibility is the usual "days since last update" item (3.2.0 was
published 2026-06-23).

The gcc-UBSAN guard from v3.1.1/v3.1.2 is unchanged: the single unsupervised
`varPro::isopro(method = "unsupv")` test still calls `skip_on_cran()` to avoid
an upstream `randomForestSRC` sanitizer report (`rfsrcGrow`, `entry.c:184`);
all other varPro tests run. This is the only grow that trips the report (its
`yvar.wt` is length-0); `uvarpro()` and the other varPro grows are
synthetic-supervised and sanitizer-clean. ggRandomForests remains a pure-R
package (`NeedsCompilation: no`).
