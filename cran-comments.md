## v3.5.1 — patch release (fixes the gcc-UBSAN additional issue in 3.5.0)

Submitted in response to the CRAN check request of 2026-08-05 (correct before
2026-08-21). All other flavors were OK on 3.5.0. The `gcc-UBSAN` fix is the
reason for the release; it also carries two small defensive fixes in `R/`,
described under "What else is in this release" below.

### The report

`entry.c:184` in `randomForestSRC`:

```c
RF_yWeight = REAL(yWeight);  RF_yWeight--;
```

`randomForestSRC` decrements the `yvar.wt` pointer to index from 1. A forest
grown with no outcome makes `rfsrc` pass `yvar.wt = numeric(0)`, so the
decrement runs off a zero-length allocation — the "use of 0x1" the report
describes. `ggRandomForests` has no compiled code; the undefined behaviour is
upstream, and this release removes the call path that reaches it.

### The path, and the fix

One test grew that forest indirectly. `gg_partial_varpro()` calls
`varPro::partialpro()`, which grows its own isolation forest through
`isopro()` and lets `method` default to `"unsupv"`. The package's other
live-`partialpro()` tests were already `skip_on_cran()`'d for runtime, which
left this one as the only one running on CRAN — and so the only one reported.

That test now passes `method = "rnd"` itself. Verified by tracing every
`rfsrc` grow across the suite under CRAN skip semantics: 299 grows, 0 reached
without a formula (previously 2, both in that one test). The test asserts the
same warnings over the same number of rows, so CRAN coverage is unchanged.

The same audit found the `\donttest` example in `?gg_partial_varpro` on that
same path; it did not fire under gcc-UBSAN because that flavor did not run
`\donttest` code, but it is fixed here too rather than left to a future
check-flavor change.

### What else is in this release

The sanitizer fix is the reason for the submission, but it is not the whole
diff, so let me set out the rest. Two small defensive fixes in `R/` came out of
a code review of the same files:

* `calc_roc()` now routes `which_outcome = 0` through the same fallback as
  `which_outcome = "all"`, with a warning. `0` is the documented numeric
  spelling of "all", and left unnormalized it indexed as `predicted[, 0]` —
  legal R that yields a zero-column matrix, so the threshold sweep ran on empty
  input and returned a degenerate two-row object with no sensitivity or
  specificity columns instead of raising an error.
* `gg_partial_rfsrc()` now checks its first argument is an `rfsrc` object and
  stops with a message naming the class it got. Previously a non-forest reached
  an `ncol()` comparison on a `NULL` and failed with base R's "argument is of
  length zero", which does not point at the real problem.

Both replace a silent wrong answer with a clear one; neither changes results for
valid input. The rest of the diff is the test suite, the roxygen examples (every
`rfsrc` fit in an example now carries an explicit `ntree`, and the repeated
`pbc` setup is collapsed into one shared `inst/examples/pbc-setup.R`), the
regenerated `.Rd` files, and the version metadata.

### Test environments

* **Local:** macOS (aarch64-apple-darwin), R 4.6.0, `R CMD check --as-cran`
  with the manual, built from a clean `git archive` export: **0 ERRORs,
  0 WARNINGs, 1 NOTE**. Check time 3m53s across timed steps; the source tarball
  is 2.27 MB. The full test suite, run with `NOT_CRAN=true` so the
  `skip_on_cran()` tests execute too, is 1510 passing and 0 failing.
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.
* **URL check:** `urlchecker::url_check()` reports all URLs correct.

**win-builder:** R-devel, R-release and R-oldrelease, run against this exact
tarball.

On check time: the 3.5.0 win-builder totals ran to roughly 8 minutes
(R-release), 10 minutes (R-devel) and 12 minutes (R-oldrelease), with the
vignette rebuild the dominant term in each. R-oldrelease has come down sharply
on 3.5.1, because every `rfsrc` fit in an example now carries an explicit
`ntree`; R-release and R-devel are essentially unchanged, with R-devel the
closest to the line. If that is too long, the vignette rebuild is the lever and
I am glad to precompute further, as I did for 3.1.0.

### NOTE disposition

The one NOTE is `Number of updates in past 6 months: 7`. This submission is the
fix you requested on 2026-08-05 for the `gcc-UBSAN` additional issue in 3.5.0,
with a 2026-08-21 deadline, which is why it follows the previous release so
closely. I would not otherwise submit on this cadence.

The change is narrow, and I would rather describe it accurately than call it
smaller than it is. Beyond the sanitizer fix there are the two defensive fixes
in `calc_roc()` and `gg_partial_rfsrc()` set out above, both of which turn a
silent wrong answer into an error or a warning and neither of which changes
results for valid input. Everything else is tests, examples, regenerated `.Rd`
files and version metadata. Local check time is 3m53s, and on win-builder
R-oldrelease has roughly halved against 3.5.0, because every `rfsrc` fit in an
example now carries an explicit `ntree`.

---

## v3.5.0 — minor release (SHAP explanations; default S3 methods; survival partial-dependence labeling; randomForest VIMP fixes)

This is a minor feature-and-fix release. It consolidates the work developed
since the CRAN 3.4.0 release (the 3.4.1 and 3.5.0 development cycles) into a
single submission.

### What's new / fixed

* **SHAP explanations.** New `gg_shap()` with `plot`/`autoplot`/`print`/
  `summary` methods, plus `shap_importance()`, `shap_beeswarm()` and
  `shap_dependence()`, giving SHAP explanations of regression and
  classification forests by wrapping `kernelshap` (Suggests). `gg_shap()`
  enforces the documented integer contract on `bg_n` and `which.class` rather
  than silently coercing them: `bg_n = 1.9` was truncated to 1, `bg_n = Inf`
  became `NA`, and `which.class = 2.9` passed the range check and then indexed
  column 2 — returning SHAP values for a class the caller never asked for.
  Non-whole, non-finite, out-of-range and non-scalar values now raise a clear
  error; valid input is unaffected.
* **Default S3 methods for the classic wrappers.** The remaining
  `rfsrc`/`randomForest` wrappers — `gg_error()`, `gg_vimp()` and others —
  gained `default` methods, so an unsupported object now produces an
  informative error instead of dispatching somewhere unhelpful.
* **Bug fix: `gg_vimp()` reports the importance a `randomForest` fit actually
  computed.** A `randomForest` importance matrix stores permutation importance
  and node impurity side by side on incommensurable scales; `gg_vimp()` ranked
  them together, so node impurity (thousands) swept the top and the permutation
  values the caller asked for by passing `importance = TRUE` (tens) were
  truncated away — `randomForest(medv ~ ., Boston, importance = TRUE)` showed
  `lstat = 12576.7` where the permutation value is `62.4`. The permutation
  measure is now reported and node impurity left out of the ranking, for both
  regression and classification. Alongside: `which.outcome` resolves the class
  column by name (a `randomForest` matrix has no overall-first column, so `0`
  had returned the first class mislabeled as overall), `nvar` counts variables
  and ranks before trimming (it had been keeping the least-important variables),
  and the selected measure is named in the `set` column rather than the
  literal `"vimp"`.
* **Bug fix: survival partial dependence is no longer mistakable for a
  probability.** `randomForestSRC::plot.variable()` defaults to
  `surv.type = "mort"`, so `gg_partial()`'s `yhat` is *mortality* — an expected
  event count, not a value on [0, 1] — and it only superficially resembles a
  percentage. `yhat` is passed through unscaled (rescaling it would corrupt the
  quantity); instead the label describing what was plotted is carried on the
  object as `attr(x, "ylabel")` and used as the y-axis title by
  `plot.gg_partial()`. Note that `gg_partial_rfsrc()` defaults to
  `partial.type = "surv"` and so does report survival probabilities: the two
  entry points report different quantities by default. (#15)
* **Smaller tarball.** The vignettes now render figures with `ragg` and quantise
  them to a 256-color palette, taking the source tarball from 4.7 MB to 2.3 MB
  and `inst/doc` from 5.3 MB to 1.9 MB (the installed-size INFO is gone). The
  vignettes had never set a graphics device and so fell through to the default
  `png()`, which writes RGBA truecolor. Figures are visually unchanged (mean
  pixel difference 1.55 on a 0-255 scale). Both steps are guarded by
  `requireNamespace()` and degrade to no-ops, so a vignette rebuild on a machine
  without `ragg` or ImageMagick still succeeds.
* Documentation: the package help page (`?ggRandomForests`) now describes the
  whole current surface — the SHAP, Brier, varPro and unsupervised-varPro
  families were missing — and no longer claims that `plot()` methods may return
  a *list* of `ggplot2` objects; each returns a single plottable object (a
  `ggplot`, or a `patchwork` composite for the multi-panel methods).

### Test environments

* **Local:** R 4.6.0 on macOS (aarch64-apple-darwin23).
  `R CMD check --as-cran` (with the manual) returns 0 errors, 0 warnings,
  0 notes; overall check time under 4 minutes.
* **win-builder:** x86_64-w64-mingw32, Windows Server 2022. R-devel (r90347),
  R-release (R 4.6.1) and R-oldrelease (R 4.5.3) all return `R CMD check
  --as-cran` Status OK -- 0 errors, 0 warnings, 0 notes; CRAN incoming
  feasibility clean (no "Days since last update" note).
* **Reverse-dependency check:** 0 reverse dependencies on CRAN.
* **URL check:** `urlchecker::url_check()` reports all URLs correct.

### NOTE disposition

`R CMD check --as-cran` is clean (0/0/0) locally.

3.4.0 was published 2026-07-02, so this update follows 33 days later; any
"Days since last update" note simply reflects that cadence. The submission is
deliberate rather than a correction to 3.4.0: it lands the SHAP family, a
self-contained feature set
developed and reviewed as a unit, together with the `gg_partial()` fix in #15,
where the plotted quantity could be read as a probability when it is an
expected event count, and the `gg_vimp()` fixes, where a `randomForest` fit
reported node impurity in place of the permutation importance the caller
requested. I am happy to hold this release and resubmit later if the cadence is
unwelcome.

The gcc-UBSAN guard from v3.1.1/v3.1.2 is unchanged: the single unsupervised
`varPro::isopro(method = "unsupv")` test still calls `skip_on_cran()` to avoid
an upstream `randomForestSRC` sanitizer report (`rfsrcGrow`, `entry.c:184`);
all other varPro tests run. This is the only grow that trips the report (its
`yvar.wt` is length-0); `uvarpro()` and the other varPro grows are
synthetic-supervised and sanitizer-clean. ggRandomForests remains a pure-R
package (`NeedsCompilation: no`).
