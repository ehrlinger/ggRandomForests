## v3.1.1 — CRAN fix (gcc-UBSAN additional check)

This patch addresses the gcc-UBSAN "additional issue" flagged for 3.1.0
(email from Prof Ripley, 2026-06-12; correct before 2026-06-29).

### The issue

The sanitizer reports, once, during the package tests:

```
entry.c:184:55: runtime error: pointer index expression with base
0x000000000001 overflowed to 0xfffffffffffffff9
    #0 rfsrcGrow .../randomForestSRC/src/entry.c:184
```

This is a 0-length array access in the compiled code of the
`randomForestSRC` package (`rfsrcGrow`, `entry.c:184`), reached when
`varPro::varpro()` / `varPro::beta.varpro()` grow a forest in our tests.
ggRandomForests is a pure-R package (`NeedsCompilation: no`); it contains
no C/C++/Fortran of its own, so the overflow is not in our code. It is
surfaced because our test suite exercises that code path.

### The fix

Two changes, neither touching package R code (ggRandomForests remains
`NeedsCompilation: no`):

1. The tests that grow a varPro forest now call `testthat::skip_on_cran()`,
   so they do not run on CRAN's check machines (including the gcc-UBSAN
   check). They continue to run on our CI and for users who run
   `devtools::test()`.
2. The `varpro` vignette now loads every varPro fit from a precomputed
   file (`vignettes/varpro_precomputed.rds`) instead of growing forests
   live, so the vignette build performs no varPro grow under `R CMD check`
   and cannot surface the same upstream path. Each chunk falls back to a
   live fit if the file is absent, so the vignette is still reproducible
   from source.

The upstream issue has been reported to the randomForestSRC maintainers.

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` returns 0 errors, 0 warnings, 0 notes; the
  varPro tests skip under the CRAN check environment as intended.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release).
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.

### NOTE disposition

One NOTE on the incoming feasibility check, "Days since last update: 1".
This is expected: 3.1.0 was published 2026-06-11 and this patch is the
gcc-UBSAN fix the CRAN team requested (2026-06-12 email, correct before
2026-06-29), so the short interval is unavoidable. No other notes.
