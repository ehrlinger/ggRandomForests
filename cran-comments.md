## v3.1.2 — CRAN fix (gcc-UBSAN additional check, follow-up to 3.1.1)

This patch completes the gcc-UBSAN "additional issue" fix. v3.1.1 guarded
the varPro forest-growing tests with `skip_on_cran()`, but one fixture was
missed and the auto-check re-flagged the issue (win-builder incoming
pre-test, 2026-06-12; correct before 2026-06-29).

### The issue

The sanitizer reports, once, during the package tests:

```
entry.c:184:55: runtime error: pointer index expression with base
0x000000000001 overflowed to 0xfffffffffffffff9
    #0 rfsrcGrow .../randomForestSRC/src/entry.c:184
```

This is a 0-length weight-vector access in the compiled code of the
`randomForestSRC` package (`rfsrcGrow`, `entry.c:184`): when `yvar.wt` has
length 0 — which happens for the **unsupervised** family — the unconditional
`RF_yWeight--` forms an out-of-bounds pointer (undefined behaviour, though
never dereferenced). ggRandomForests is a pure-R package
(`NeedsCompilation: no`); it contains no C/C++/Fortran of its own, so the
overflow is not in our code. It is surfaced because our test suite grows an
unsupervised forest via `varPro::isopro()`.

### The fix

No package R code changes (ggRandomForests remains `NeedsCompilation: no`).
We built CRAN's `randomForestSRC` 3.6.2 with `-fsanitize=undefined` and ran
every varPro/rfsrc grow in our test suite under it. Only one grow fires
`entry.c:184`: the *unsupervised* isolation forest
(`varPro::isopro(method = "unsupv")`). The bug is a 0-length `yvar.wt`
(present only for the unsupervised family) that `rfsrcGrow` decrements to an
out-of-bounds pointer; supervised grows have a non-empty `yvar.wt` and are
unaffected.

`make_iso_fit()` in the `gg_isopro` tests therefore calls
`testthat::skip_on_cran()` only when `method = "unsupv"`, so the one
offending grow never runs on CRAN's check machines (including the gcc-UBSAN
additional check). All other varPro tests run on CRAN. We confirmed the full
test suite produces zero gcc-UBSAN errors under the sanitizer configuration
CRAN uses (GCC `-fsanitize=undefined`, which — unlike clang — does not
include `float-cast-overflow`).

The upstream issue has been reported to the randomForestSRC maintainers.

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` (with the manual) returns 0 errors, 0 warnings,
  1 NOTE (days since last update, see below); the single unsupervised isopro
  test (`method = "unsupv"`) skips under the CRAN check environment as
  intended, all other varPro tests run.
* **GitHub Actions matrix:** ubuntu-latest (R-devel / R-release /
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release).
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.

### NOTE disposition

One NOTE on the incoming feasibility check, "Days since last update: N".
This is expected: this patch is the follow-up to the gcc-UBSAN fix the CRAN
team requested (correct before 2026-06-29), so the short interval is
unavoidable. No other notes.
