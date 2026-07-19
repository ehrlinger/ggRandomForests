# Changelog

## ggRandomForests v3.5.0

- [`plot.gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_varpro.md)
  no longer draws a phantom “NA” category when `nvar` is smaller than
  the number of variables the fit reports. `$imp`/`$stats` are truncated
  to `nvar`, but the per-tree overlay (`$imp.tree`) and the
  class-conditional data (`$conditional`) still carry every variable;
  re-levelling those to the truncated `$imp` levels orphaned the extras
  to `NA`, which rendered as an empty box/bar. Those rows are now
  dropped, so only the displayed variables appear.
- The vignettes now render their figures with `ragg` and quantise them
  to a 256-color palette, cutting the source tarball from 4.7 MB to 2.3
  MB. The vignettes had never chosen a graphics device, so they fell
  through to the default
  [`png()`](https://rdrr.io/r/grDevices/png.html), which writes RGBA
  truecolor: an alpha channel these opaque plots never use, over tens of
  thousands of anti-aliased colors that PNG cannot compress. Figures are
  visually unchanged (mean pixel difference 1.55 on a 0-255 scale). Both
  steps are build-time only and degrade to no-ops when `ragg` or
  `magick` is absent, so a vignette rebuild without them still succeeds
  – at the old file size.
- The varPro vignette now documents which variables a `varpro` fit
  actually makes available. A fit narrows the predictors twice –
  `object$xvar.names` holds what
  [`varPro::partialpro()`](https://www.randomforestsrc.org/reference/partialpro.html)
  can reach,
  [`varPro::get.topvars()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  only the reported ranking – and `partialpro()` silently drops any
  requested name outside the first set. The new section covers naming
  `xvar.names` to get past the reported ranking, `split.weight = FALSE`
  to widen the candidate set itself, and the two arguments (`nvar`,
  `sparse`) that look like they should help and don’t.
- [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  now warns when a name passed in `xvar.names` is one the `varpro` fit
  cannot reach, instead of letting it disappear. `partialpro()` keeps
  only the names it finds in `object$xvar.names` and says nothing about
  the rest, so a request for twelve variables could come back with ten.
  The check runs before `partialpro()` does, so the warning arrives
  ahead of the computation rather than after it; it names every dropped
  variable and points at `split.weight = FALSE`. Supplying `part_dta`
  yourself is unchanged – the variables are already gone by then. The
  function’s examples now cover the object-driven path, which had none.
- `gg_partial_varpro(scale = "chf")` now computes the variables you name
  in `xvar.names` instead of every variable the fit can reach. The `chf`
  path routes through
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  rather than `partialpro()`, and it had never been given the variable
  list – so asking for one variable quietly did the work for all
  fourteen. This is the mirror image of the `partialpro()` bug above:
  that one returns fewer variables than you asked for, this one returned
  all of them. Naming a variable the forest does not carry has always
  been an error and still is. The `partialpro`-only arguments (`cut`,
  `nsmp`) mean nothing on this path and are now ignored with a warning
  rather than in silence.
- Fix:
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  on a `randomForest` fit grown with `importance = TRUE` now reports the
  permutation importance you asked for. It was reporting node purity
  instead, and silently: `randomForest` stores `%IncMSE` and
  `IncNodePurity` side by side, and
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  stacked both into one `vimp` column and ranked them together. The two
  are not commensurable – node purity runs in the thousands where
  `%IncMSE` runs in the tens – so every impurity row outranked every
  permutation row, and the truncation to `nvar` cut the permutation
  values away entirely. On
  `randomForest(medv ~ ., Boston, importance = TRUE)` the plot showed
  `lstat = 12576.7` (node purity) where the permutation value is
  `lstat = 62.4`. Node purity is now left out of the ranking; read
  `randomForest::importance(object)` if you want both. Fits grown
  without `importance = TRUE` are unaffected – they only ever stored
  node purity, and that is still what you get.
- Fix:
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  on a `randomForest` *classification* fit grown with
  `importance = TRUE` now reports permutation importance as well. That
  matrix mixes the same two scales – a permutation column per class plus
  `MeanDecreaseAccuracy`, alongside `MeanDecreaseGini` – but it is wider
  than the single-outcome branch that picks one measure, so it skipped
  that branch and every column was ranked together. `MeanDecreaseGini`
  came out the sole survivor: on
  `randomForest(Species ~ ., iris, importance = TRUE)`,
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  returned 4 rows of node purity where 16 rows of permutation importance
  were there to report. The per-class columns and `MeanDecreaseAccuracy`
  are all permutation measures on one scale, so they are now kept
  together and named in the `set` column, the way an `rfsrc` fit’s
  `all`/`<class>` columns already were; only `MeanDecreaseGini` is
  dropped.
- Fix: `which.outcome` now selects the column you asked for on a
  `randomForest` classification fit. `which.outcome = 0` documented
  itself as overall importance and took column 1 to get it, and
  `which.outcome = k` took column `k + 1` for class `k`. Both are right
  for an `rfsrc` fit, whose `$importance` leads with an `all` column,
  and neither is right here: a `randomForest` matrix opens on the
  classes and keeps the overall permutation measure in
  `MeanDecreaseAccuracy`, near the end. So `0` returned the first class
  labeled as overall – on
  `randomForest(Species ~ ., iris, importance = TRUE)` it handed back
  setosa’s values, ranking `Petal.Width` above `Petal.Length` where the
  overall measure has them the other way round – and every class index
  was shifted by one, `1` giving versicolor. The columns are now
  resolved by name: `0` reaches `MeanDecreaseAccuracy`, `k` reaches
  class `k`, and `which.outcome = 1` agrees with
  `which.outcome = "setosa"`. Fits grown with `importance = FALSE` keep
  no `MeanDecreaseAccuracy` column and their single measure answers to
  `0` as before.
- `which.outcome` now names the measure it selected in the `set` column,
  for both `rfsrc` and `randomForest` fits. Asking for one measure
  reported `set` as the literal `"vimp"` – the pivot takes `set` from
  the source column name, and the selected column was named after the
  `vimp` column it was about to be written into rather than after the
  measure it held. So the one path where you have to say which measure
  you want was the one path that would not tell you which measure you
  got. `gg_vimp(rfsrc_iris, which.outcome = 0)` now reports
  `set == "all"`, `gg_vimp(rf_iris, which.outcome = 0)` reports
  `set == "MeanDecreaseAccuracy"`, and both agree with the names the
  unfiltered pivot has always used. Values and ordering are unchanged,
  and plots are unaffected:
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  only facets on `set` when there is more than one of them, and
  selecting a measure leaves exactly one.
- `nvar` counts variables again for `randomForest` fits, not rows. It
  was applied after the multiclass pivot, where a frame holds one row
  per variable *per measure*, so it lopped whole measures off the end of
  the ranking instead of trimming the ranking itself.
- [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  now says in
  [`?gg_vimp`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  that a `randomForest` fit without `importance = TRUE` stores only
  `IncNodePurity`, so the ranking is node purity rather than permutation
  VIMP, and nothing in the plot marks the difference. The example now
  passes `importance = TRUE`.
- [`gg_error()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md)
  now explains that the error trajectory is `randomForestSRC`’s to
  record, not ours:
  [`rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html)’s
  `block.size` defaults to `NULL` unless you request importance, which
  stores the error at the final tree only, so a default fit gives
  [`gg_error()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md)
  a single point rather than a curve – `tree.err = TRUE` alone does not
  change that. Grow with `block.size = 1` for an error at every tree.
  The examples do this now; they had all been plotting one dot.
- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md):
  the `imp` column is documented as the *absolute* coefficient.
  [`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  wraps every coefficient it returns in
  [`abs()`](https://rdrr.io/r/base/MathFun.html), so the sign is
  discarded upstream and never reaches us – the docs had said “Sign is
  real (direction of local association)”, which cannot be read off this
  output. Use
  [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md)
  for a signed local estimator.
- [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md):
  the “What’s in the output” section now says the polarity flip is ours.
  [`varPro::isopro()`](https://www.randomforestsrc.org/reference/isopro.html)’s
  `howbad` is *lower* = more anomalous; we return `1 - howbad` so that
  higher = more anomalous. The section had credited that to the fit,
  contradicting this function’s own `@return`.
- Added
  [`gg_shap()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  and
  [`plot.gg_shap()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_shap.md)
  (with
  [`shap_importance()`](https://ehrlinger.github.io/ggRandomForests/reference/shap_importance.md),
  [`shap_beeswarm()`](https://ehrlinger.github.io/ggRandomForests/reference/shap_beeswarm.md),
  [`shap_dependence()`](https://ehrlinger.github.io/ggRandomForests/reference/shap_dependence.md))
  for SHAP explanations of regression and classification forests,
  wrapping `kernelshap` (Suggests).
- [`gg_shap()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_shap.md)
  now enforces the integer contract on `bg_n` and `which.class` instead
  of silently coercing them. Both are documented as integers, but were
  only loosely checked: `bg_n = 1.9` was truncated to 1 and `bg_n = Inf`
  (or any value above `.Machine$integer.max`) became `NA`, while
  `which.class = 2.9` passed the range check and then indexed column 2 –
  returning SHAP values for a class the caller never asked for.
  Non-whole, non-finite, out-of-range and non-scalar values now raise a
  clear error. Valid input is unaffected.
- Added
  [`print.gg_shap()`](https://ehrlinger.github.io/ggRandomForests/reference/print.gg.md)
  and
  [`summary.gg_shap()`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md).
  `gg_shap` was the only `gg_*` class without them, so it dumped every
  row at the REPL instead of showing a header.
  [`print()`](https://rdrr.io/r/base/print.html) now gives the standard
  one-line header (with the variable and observation counts) and
  [`summary()`](https://rdrr.io/r/base/summary.html) returns a
  `summary.gg` object reporting the baseline, background-sample size,
  the explained class for classification fits, and the top variables by
  mean \|SHAP\|.
- The package help page (`?ggRandomForests`) now describes the whole
  current surface – the SHAP, Brier, varPro and unsupervised-varPro
  families were missing – and no longer claims that
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods may
  return a *list* of `ggplot2` objects; each returns a single plottable
  object (a `ggplot`, or a `patchwork` composite for the multi-panel
  methods).
- [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md)
  no longer lets survival partial dependence be mistaken for a
  probability.
  [`randomForestSRC::plot.variable()`](https://www.randomforestsrc.org//reference/plot.variable.rfsrc.html)
  defaults to `surv.type = "mort"`, so `yhat` is *mortality* – an
  expected event count, not a value on \[0, 1\] – and it only
  superficially resembles a percentage. `yhat` is passed through
  unscaled (rescaling it would corrupt the quantity); instead the label
  describing what was plotted is carried on the object as
  `attr(x, "ylabel")` and used as the y-axis title by
  [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md).
  Note that
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  defaults to `partial.type = "surv"` and so does report survival
  probabilities: the two entry points report different quantities by
  default.
  ([\#15](https://github.com/ehrlinger/ggRandomForests/issues/15))

## ggRandomForests v3.4.1

- The remaining `rfsrc`/`randomForest` wrappers –
  [`gg_error()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_error.md),
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md),
  [`gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md),
  [`gg_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_rfsrc.rfsrc.md),
  and
  [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  – now have `default` S3 methods, so a wrong-class input gives a clear
  “expected an ‘rfsrc’ or ‘randomForest’ object” error (naming the class
  it got) instead of R’s generic “no applicable method”. This finishes
  the dispatch-consistency pass started for the varPro family in 3.4.0.
  ([`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  keeps its existing `gg_roc.rfsrc` default, which accepts rfsrc-shaped
  objects.)

## ggRandomForests v3.4.0

CRAN release: 2026-07-02

- [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md),
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md),
  and
  [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md)
  now have `default` S3 methods, so a wrong-class input gives a clear
  “expected a ‘’ object” error (naming the class it got) instead of R’s
  generic “no applicable method”. This makes the varPro-family wrappers
  consistent with
  [`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md)
  /
  [`gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md);
  the previously-unreachable inner class checks were removed.
- Fix:
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  now computes partial dependence correctly for `factor` predictors. It
  was passing factor *labels* as `partial.values` to
  [`randomForestSRC::partial.rfsrc()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
  which imposes a level by its integer code (internally
  `as.numeric(partial.values)`). Character labels (“No”/“Yes”) became
  `NA` and numeric-looking labels (“4”/“6”/“8”) became out-of-range
  codes, so every level collapsed to a single value (a flat categorical
  partial plot). The wrapper now passes the integer codes and relabels
  the output, matching `plot.variable(partial = TRUE)` and the
  ground-truth partial dependence. The categorical `x` is now returned
  as a `factor` in the model’s level order, so the plot keeps that order
  instead of re-sorting alphabetically. Continuous and numeric
  low-cardinality predictors are unaffected.
- [`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md)
  /
  [`plot.gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_beta_uvarpro.md):
  tidy wrapper and bar chart for
  [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  – the unsupervised analogue of
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md).
  From a `uvarpro()` fit it aggregates the per-region lasso coefficients
  into `beta_mean = colMeans(|beta|)` per variable (most-important
  first), flags variables above a selection cutoff, and accepts a
  precomputed `beta_fit` matrix. `print`/`summary`/`autoplot` companions
  follow the `gg_*` conventions.
- [`gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md)
  /
  [`plot.gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_sdependent.md):
  tidy wrapper and ranked lollipop for
  [`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  signal-variable detection. Returns one row per candidate variable
  (`imp_score`, graph `degree`, `signal` flag) ranked by `imp_score`.
  Complements
  [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
  (the dependency graph) with the “which variables are signal” ranking;
  shares the `beta_fit` entropy matrix. Follows the `get.beta.entropy` +
  `sdependent` workflow from the
  [`varPro::uvarpro()`](https://www.randomforestsrc.org/reference/uvarpro.html)
  help (iowa-housing example).
- New `uvarpro` vignette: a short, focused walk-through of the
  unsupervised varPro wrappers
  ([`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md),
  [`gg_beta_uvarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_uvarpro.md),
  [`gg_sdependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_sdependent.md))
  on a single `uvarpro()` fit, using the shared `beta_fit` matrix. The
  three unsupervised sections were lifted out of the `varpro` vignette,
  which now points to the new one and covers the five supervised
  wrappers.
- Fixed the main vignette’s `\VignetteIndexEntry`, which still carried
  the template placeholder “Vignette’s Title” – it now reads “Exploring
  Random Forests with ggRandomForests” (the index entry CRAN lists, not
  the document title, was the stale one).

## ggRandomForests v3.3.0

- [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md):
  **classification partial plots now default to probability.**
  `scale = "auto"` on a classification fit resolves to `"prob"` (P(Y =
  target class)) instead of raw log-odds; `"odds"` and `"logodds"` are
  options. The back-transform is applied before averaging (mean
  predicted probability). The `causal` contrast is shown only on
  `"logodds"`.
- [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md):
  **survival partial plots now default to survival probability.**
  `scale = "auto"` on a survival fit resolves to `"surv"` (S(tau \| x),
  bounded 0-1) via a new partialpro learner, instead of the unbounded
  ensemble-mortality score (still available via `scale = "mortality"`).
  `"surv"` and `"rmst"` default `tau` to the median follow-up time when
  `time` is omitted – a units-safe, data-driven horizon (v3.2.0’s `rmst`
  required `time`; this is a loosening). The resolved `tau` is reported
  in a message and the axis label.
- [`plot.gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md):
  documents what the `causal` (virtual-twins) estimator is and when to
  use it, and explains why it is hidden on the bounded probability
  scales.
- Documentation:
  [`plot.gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  gains a “Reading an RMST curve” section explaining how to interpret
  the `scale = "rmst"` y-axis – RMST(tau) is the expected event-free
  time within the first tau time-units (area under S(t) out to tau),
  read in the model’s own time units, bounded by tau, and
  higher-is-better (the opposite direction from ensemble mortality). It
  also notes that tau must be supplied in the fit’s time units, since a
  tau beyond the largest event time truncates to the full restricted
  mean. No code change.

## ggRandomForests v3.2.0

CRAN release: 2026-06-23

- Fix
  ([\#118](https://github.com/ehrlinger/ggRandomForests/issues/118)):
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
  no longer fails with the cryptic “arguments imply differing number of
  rows:

  , 0” when
  [`varPro::importance()`](https://www.randomforestsrc.org/reference/importance.html)
  returns a degenerate importance table (0 rows, or `p` variables with
  no usable `z` column) – observed intermittently on survival fits where
  the release-rule step selects no variables. It now stops with a clear,
  specific message explaining the empty importance and suggesting a
  larger `ntree`. The guard is scoped to the degenerate case only;
  well-formed fits (survival included) are unaffected – this is not a
  blanket survival-family block (cf. the reverted
  [\#116](https://github.com/ehrlinger/ggRandomForests/issues/116)).

- Fix: `gg_partial_varpro(scale = "rmst", time = tau)` now *drives* the
  survival partial computation instead of only relabeling the y-axis.
  [`varPro::partialpro()`](https://www.randomforestsrc.org/reference/partialpro.html)
  has no time argument, so its default survival learner returns ensemble
  mortality at every horizon – multi-horizon RMST plots built that way
  differed only by Monte-Carlo noise, not by `tau`. `scale = "rmst"` now
  passes `partialpro()` an RMST(`tau`) learner that integrates the
  survival curve (`integral_0^tau S(t) dt`) from `object$rf`, so the
  curve genuinely depends on `tau`. This path recomputes from `object`
  (a survival fit) with `part_dta = NULL`; a precomputed `part_dta` can
  only be relabeled, and the function now warns when you try. Also warns
  when `tau` exceeds the model’s event-time range (RMST is truncated
  there) and when `time` is passed to a scale that ignores it. `Imports`
  now requires `varPro (>= 3.1.0)` (the version exposing the
  `partialpro()` `learner` argument this path relies on).

- Fix: `gg_partial_varpro(scale = "surv"/"chf", model = ...)` no longer
  errors when a variable yields an empty continuous or categorical frame
  (the survival path-C `model`-label assignment now guards against a
  0-row data.frame).

- [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  (and the
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  alias) now forward `...` to
  [`varPro::partialpro()`](https://www.randomforestsrc.org/reference/partialpro.html)
  on the object-driven path. This restores control over which variables
  are computed (`xvar.names`, `nvar`) and the UVT step (`cut`, `nsmp`,
  …) for the RMST path, which must recompute from `object` and so cannot
  accept a precomputed `part_dta`. Without an explicit `xvar.names`,
  `partialpro()` falls back to `varPro::get.topvars(object)`, which can
  return few or no variables for some fits.

## ggRandomForests v3.1.2

CRAN release: 2026-06-13

- CRAN fix: skip only the single test grow that trips the upstream
  `randomForestSRC` gcc-UBSAN report at `entry.c:184` — the
  *unsupervised* isolation forest in `gg_isopro`
  (`varPro::isopro(method = "unsupv")`). Only an unsupervised grow has a
  0-length `yvar.wt`, the vector `rfsrcGrow` decrements to an
  out-of-bounds pointer; supervised grows are unaffected. We verified
  this under `-fsanitize=undefined`: of every varPro/rfsrc grow in the
  test suite, only `isopro(method = "unsupv")` fires `entry.c:184`.
  `make_iso_fit()` therefore calls `skip_on_cran()` only for
  `method = "unsupv"`. ggRandomForests is pure R and unchanged.
- The broader `skip_on_cran()` guards added in v3.1.1 (the `varpro`,
  `uvarpro`, `ivarpro`, `beta.varpro`, and `isopro(method = "rnd")` test
  fixtures) are removed: those grows are supervised (or
  synthetic-supervised) and gcc-UBSAN-clean, so they run on CRAN again,
  restoring that test coverage. The upstream issue is fixed in
  `randomForestSRC` and pending a CRAN release.

## ggRandomForests v3.1.1

- CRAN fix: the varPro tests now call `skip_on_cran()` so they do not
  run on CRAN’s check machines, including the gcc-UBSAN additional
  check. They were triggering an upstream `randomForestSRC` sanitizer
  issue (a 0-length array access in `rfsrcGrow`, `entry.c:184`) that
  surfaces when any `varPro` grow (`varpro()`, `beta.varpro()`,
  `uvarpro()`, `isopro()`, `ivarpro()`) builds a forest. ggRandomForests
  is pure R and its code is unchanged; the varPro tests still run in our
  CI (the workflows set `NOT_CRAN=true`) and locally; they are skipped
  only on CRAN’s check machines, including the gcc-UBSAN check. The
  upstream issue has been reported to the randomForestSRC maintainers.
- The `varpro` vignette now loads every varPro fit from a precomputed
  file (`vignettes/varpro_precomputed.rds`, built by
  `vignettes/precompute_varpro.R`), so the vignette performs no live
  varPro grow during `R CMD check`. This removes the same upstream
  sanitizer path from the vignette build and trims check time. Each
  chunk falls back to a live fit if the precomputed object is absent, so
  the vignette remains reproducible from source.

## ggRandomForests v3.1.0

CRAN release: 2026-06-11

- Fix:
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  for single-outcome rfsrc forests now correctly flags variables with
  non-positive VIMP in the `positive` column (affecting plot coloring).
  The column was named `VIMP` (uppercase) in single-outcome fits but the
  flag check accessed `$vimp` (lowercase), leaving `positive` stuck at
  `TRUE` for all variables. Surfaced by the Copilot review on PR
  [\#109](https://github.com/ehrlinger/ggRandomForests/issues/109).
- Documentation pass. Deepened the varPro-family and rfsrc
  importance/partial/survival help pages against the upstream
  randomForestSRC and varPro documentation, and made the line between
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md)
  (permutation, Breiman-Cutler importance) and
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
  (varPro release-rule importance) explicit and cross-linked. Vignette
  prose deepened with the same framing; one-line code-comment fixes;
  fixed a stale `@return` in
  [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
  (documented a `yvar` column the function does not return). No
  user-facing behavior change.
- Vignettes: the regression and survival partial-dependence surfaces are
  now rendered as static `ggplot2` heat maps instead of interactive
  `plotly` widgets, and figures render at 96 dpi. This cuts the
  installed size from ~17 MB to ~5 MB (the `plotly` library is no longer
  bundled into the vignette HTML). `plotly` is dropped from `Suggests`.
- Check time: reduced the `R CMD check` vignette-rebuild and test
  timings to bring the overall CRAN check comfortably under budget (CRAN
  flagged the overall check time on the 3.1.0 submission). The
  regression and survival vignettes use lighter forests (`ntree` 200 /
  150, imputation `ntree` 100) and coarser partial-dependence grids. The
  varpro vignette’s three
  [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  calls and the Boston `beta.varpro()` fit (~34 s combined) are
  precomputed offline by `vignettes/precompute_varpro.R` and loaded from
  `vignettes/varpro_precomputed.rds`, with an automatic live-computation
  fallback if the file is absent. The
  [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
  tests memoise the per-fit entropy matrix
  ([`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html),
  ~1.5 s and a pure function of the fit) instead of recomputing it once
  per test. No user-facing behavior change.

## ggRandomForests v3.0.0

- **Version jump to 3.0.0.** The varPro integration is a major scope
  expansion plus the
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
  soft-deprecation, which is major-version territory. Survival /
  multivariate varPro families, ROC confidence intervals, and hazard
  estimates are deferred to v3.1.0.
- CRAN-audit cleanup: the
  [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  /
  [`plot.gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md)
  examples move from `\dontrun` to `\donttest` (so they execute under
  `R CMD check --as-cran` and on CRAN;
  [`library(survival)`](https://github.com/therneau/survival) added so
  [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) resolves), the
  per-variable [`message()`](https://rdrr.io/r/base/message.html) in the
  deprecated
  [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
  is removed (its one behavior change: that function no longer prints a
  line per variable), and the README points to the new “varpro”
  vignette.
- Fix: importance plots now consistently put the most-important variable
  at the **top**.
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md),
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md),
  and
  [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md)
  previously built their `variable` factor with descending levels, so
  after
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  the most-important variable landed at the bottom — inverted relative
  to
  [`gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_vimp.md).
  All three now reverse the factor levels to match the `gg_vimp`
  convention (and the `varImpPlot` / `vip` standard). Row order and
  [`summary()`](https://rdrr.io/r/base/summary.html) output are
  unchanged (still most-important first). A new cross-function test pins
  the convention.
- New vignette: “Exploring variable importance with varPro.” Walks the
  full gg\_\* varPro layer (gg_partial_varpro, gg_varpro, gg_udependent,
  gg_isopro, gg_beta_varpro, gg_ivarpro) on three worked examples —
  regression (Boston), classification (iris binary + multi-class), and
  survival (PBC). Includes a family-support matrix documenting which
  wrapper works for which forest family. Headline document for v3.0.0.
- [`gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_ivarpro.md)
  and
  [`plot.gg_ivarpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_ivarpro.md):
  tidy wrapper and per-variable-distribution / per-observation-profile
  plots for
  [`varPro::ivarpro()`](https://www.randomforestsrc.org/reference/ivarpro.html)
  (individual / local variable importance) across regression and
  classification (binary + multi-class) families. The long-format tidy
  frame is `(obs, variable, local_imp, selected)` for regression;
  classification adds a `class` column. NA cells are filtered out and
  sparsity is surfaced in provenance. `which_obs` (integer index)
  collapses to a single-observation profile; the plot switches from a
  jittered distribution view to a horizontal bar chart. `which_class`
  (response level name) collapses to a single class panel; binary fits
  default to the last factor level (positive class). `cutoff` accepts
  `NULL` (per-class mean), a scalar, or a named numeric vector —
  matching the gg_beta_varpro classification contract. Optional
  `ivarpro_fit` argument lets callers cache the expensive `ivarpro()`
  call. Last of four Phase 4 sub-projects.
- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
  adds varPro classification support (binary + multi-class). Binary fits
  default to a single positive-class panel (last factor level);
  multi-class fits return a long-format frame with a `class` column and
  plot as `facet_wrap(~ class)`. Optional `which_class` selects a single
  class; `cutoff` accepts a scalar or per-class named vector. Variables
  are stored as a factor whose levels are set by
  `mean(|sum-of-class-beta|)` descending so every facet shows rows in
  the same order. Motivating use case: 30-day mortality.
- Provenance shape change for
  [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md):
  `attr(*, "provenance")$cutoff` is now always a named numeric vector —
  length 1 named `"regr"` for regression, length K named with the
  response factor levels for classification. Downstream tooling should
  read it as a vector and select by name; the prior scalar shape is
  gone.
- [`gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_beta_varpro.md)
  and
  [`plot.gg_beta_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_beta_varpro.md):
  tidy wrapper and default horizontal bar chart for
  [`varPro::beta.varpro()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
  — the per-rule lasso-β refinement of variable importance. Aggregates
  per-rule β̂ by variable into `beta_mean = mean(|β̂|)` and flags
  variables above a selection cutoff (default `mean(beta_mean)`).
  Optional `beta_fit` argument lets callers compute the expensive
  `beta.varpro()` step once and reuse the result across multiple wrapper
  calls (different cutoffs, snapshot rebuilds, vignette knits). `print`
  / `summary` / `autoplot` S3 companions follow the existing `gg_*`
  conventions. **Regression family only** — classification, regr+, and
  survival are tracked under Phase 4d (see the spec for the endpoint
  map). Third of three Phase 4 sub-projects.
- [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  gains a `newdata` argument so a fitted
  [`varPro::isopro`](https://www.randomforestsrc.org/reference/isopro.html)
  model can score new observations into the same tidy `gg_isopro` frame.
  Internally the wrapper calls `predict.isopro()` twice: with
  `quantiles = FALSE` to populate the `case.depth` column (varPro’s
  native polarity, lower = more anomalous) and with `quantiles = TRUE`
  to compute `howbad = 1 - quantile` (the wrapper convention, higher =
  more anomalous). Both polarities are visible in the returned data
  frame, and the relationship is named in the roxygen. The `plot` /
  `print` / `summary` / `autoplot` S3 companions work unchanged on the
  new tidy frame; to overlay training and test scores, bind the two
  extractor calls with a `method` label column and pass the result to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html). Second of
  three Phase 4 sub-projects.
- **Fix (gg_isopro training-path polarity).** Bug in the original
  `gg_isopro` (PR
  [\#94](https://github.com/ehrlinger/ggRandomForests/issues/94)):
  varPro’s `$howbad` on an `isopro` fit uses “lower = more anomalous”
  polarity (it is the quantile of `case.depth`), but the wrapper’s plot
  method and documentation both assume “higher = more anomalous”. Train
  scores and the new test-data scores were anti-correlated until this
  PR’s training-path flip (`howbad = 1 - object$howbad`) brought them
  into agreement. The fix surfaced because the test-data sanity check
  (training-as-newdata top-5 overlap) failed at 0/5 instead of 5/5
  before the flip. Note: the two vdiffr baselines recorded in PR
  [\#94](https://github.com/ehrlinger/ggRandomForests/issues/94)
  (`gg-isopro-default` and `gg-isopro-threshold`) were recorded under
  the inverted polarity; they are visually flipped relative to the new
  behavior but CI skips snapshots (`VDIFFR_RUN_TESTS = false`) so no
  failure surfaces. Re-record with `VDIFFR_RUN_TESTS = true` when
  convenient.
- Documentation: pedagogical pass over the varPro wrappers
  (`gg_partial_varpro`, `gg_varpro`, `gg_udependent` and their `plot.*`
  methods). Each help page now has explicit “What X is doing”, “What’s
  in the output”, and “What you use this for” sections so a reader new
  to varPro can learn the underlying method (release rules, beta-entropy
  dependency, parametric / nonparametric / causal partial estimators)
  from the help page alone, not just the wrapper mechanics. No API or
  behavioral change.
- Documentation: enable roxygen2 markdown package-wide via
  `Roxygen: list(markdown = TRUE)` in `DESCRIPTION`. New roxygen blocks
  can use backticks and `[fn()]` link syntax; existing `\code{}` /
  `\link{}` markup keeps working. Two source-roxygen edits to keep R CMD
  check clean: `randomForest[SRC]` in `R/help.R` (markdown read it as an
  unfinished link) becomes plain `randomForestSRC`; the `95\%` escape in
  `R/gg_rfsrc.R::bootstrap_survival` becomes a literal `95%`. No API or
  rendered-doc behavioral change beyond the conventions switch.
- New
  [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  and
  [`plot.gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_isopro.md):
  tidy wrapper and ranked-elbow + density visualization for
  [`varPro::isopro`](https://www.randomforestsrc.org/reference/isopro.html)
  isolation-forest anomaly scores.
  [`plot.gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_isopro.md)
  takes `panel = c("both", "elbow", "density")` and optional `threshold`
  (score-space) or `top_n_pct` (quantile-space) to draw a reference
  line; if both are set, `threshold` wins with a message. A `method`
  column auto-triggers color grouping for multi-method comparisons (use
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on three
  [`gg_isopro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_isopro.md)
  calls). `print` / `summary` / `autoplot` S3 companions follow the
  existing `gg_*` conventions. First of three Phase 4 sub-projects.
- [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md):
  fix render error on the default multi-class classification plot. The
  default-xvar selection was treating `yvar` (the observed-class column)
  and `outcome` (the multi-class pivot facet) as predictors; pivoting
  them into `var` then dropped the column the downstream
  `geom_jitter(aes(color = yvar))` referenced, and the patchwork errored
  when actually rendered. CI did not catch this because the existing
  test only asserted the patchwork class (lazy) and snapshots run with
  `VDIFFR_RUN_TESTS = false`. New test exercises a real build of every
  sub-plot.
- [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md):
  the same default-xvar selection used substring `grep("time", ...)` /
  `grep("event", ...)`, which silently dropped any predictor whose name
  contained those substrings – e.g. the documented veteran-data survival
  predictor `diagtime`. Switch to exact matching for `event` / `time` /
  `yvar` / `outcome` and an anchored prefix for `yhat` (`yhat` or
  `yhat.<class>`). New test exercises `diagtime` on the veteran survival
  forest.
- [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md):
  per-class one-vs-rest ROC curves
  ([\#88](https://github.com/ehrlinger/ggRandomForests/issues/88),
  closes
  [\#72](https://github.com/ehrlinger/ggRandomForests/issues/72)).
  - New `per_class` argument, default `FALSE`. With `per_class = TRUE`
    on a forest of more than two classes,
    [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
    returns a long-format `gg_roc` data frame with a `class` factor
    column, plus a named AUC vector attribute with one entry per class,
    ordered by descending AUC.
  - [`plot.gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_roc.md)
    gains `panel = c("overlay", "facet")`. When the object has a `class`
    column, `"overlay"` colors the curves by class and `"facet"` gives
    each class its own panel.
  - [`summary.gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/summary.gg.md)
    prints the named per-class AUC values when a `class` column is
    present.
  - On a binary forest, `per_class = TRUE` does nothing, the usual
    single-curve result comes back unchanged.
  - ROC confidence intervals are still to come, in v3.1.0 (issue
    [\#7](https://github.com/ehrlinger/ggRandomForests/issues/7) /
    [\#72](https://github.com/ehrlinger/ggRandomForests/issues/72)-CIs).
- New
  [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md):
  varPro cross-variable dependency (Phase 3).
  - [`gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_udependent.md)
    reads cross-variable dependency scores off a `uvarpro` fit, via
    [`varPro::get.beta.entropy()`](https://www.randomforestsrc.org/reference/utilities_internal.html)
    and
    [`varPro::sdependent()`](https://www.randomforestsrc.org/reference/utilities_internal.html).
    It returns a tidy list: `$edges` (variable_from, variable_to,
    weight), `$nodes` (variable, degree, selected), and `$graph`, an
    igraph object.
  - [`plot.gg_udependent()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_udependent.md)
    draws the dependency network with ggraph. Edge width and opacity
    scale with dependency strength; node color marks the signal
    variables. The layout is configurable (`"fr"`, `"kk"`, `"stress"`,
    and so on).
  - `ggraph` added to `Suggests:`.
- New
  [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md):
  varPro variable importance
  ([\#85](https://github.com/ehrlinger/ggRandomForests/issues/85)).
  - [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
    pulls per-tree importance scores from a fitted `varpro` object and
    draws a boxplot of the per-tree z-score distribution for each
    variable. The hinges sit at the 15th and 85th percentiles and the
    whiskers at the 5th and 95th, so the box is not the usual Tukey one
    — it reports the percentiles it actually shows. Variables with
    aggregate z above `cutoff` (default 0.79) are color-highlighted.
  - With `faithful = TRUE`, the individual per-tree z-scores are
    jittered over the box as semi-transparent points, with a
    white-outlined dot at the mean, the same view as varPro’s internal
    `bxp` output.
  - With `conditional = TRUE` (classification forests only),
    [`gg_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_varpro.md)
    reads `$conditional.z` and draws class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - Set `local.std = FALSE` to allow `plot(..., type = "raw")`, which
    shows raw per-tree importance instead of the z-normalized values.
- `gg_variable.randomForest`: classification fix
  ([\#87](https://github.com/ehrlinger/ggRandomForests/issues/87)).
  - For a classification forest,
    [`gg_variable.randomForest()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_variable.md)
    now stores per-class OOB vote fractions as `yhat.<classname>`
    columns, read from `object$votes`, the same layout the `rfsrc` path
    produces. It used to store a single `yhat` factor column of class
    labels (from `object$predicted`), and that column shape stopped the
    multi-class pivot in `plot.gg_variable` from ever running. The vote
    fractions are row-normalized to `[0, 1]`, even when the forest was
    fit with `norm.votes = FALSE`.
  - `plot.gg_variable`, binary classification: with `smooth = TRUE` the
    x and y aesthetics are now mapped onto the smooth layer correctly.
  - `plot.gg_variable`, multi-class numeric path: `smooth = TRUE` now
    adds the smooth layer instead of skipping it silently.
  - Closes stale issues
    [\#81](https://github.com/ehrlinger/ggRandomForests/issues/81)
    (fixed in PR
    [\#83](https://github.com/ehrlinger/ggRandomForests/issues/83)) and
    [\#82](https://github.com/ehrlinger/ggRandomForests/issues/82).
- New
  [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md):
  varPro partial dependence
  ([\#84](https://github.com/ehrlinger/ggRandomForests/issues/84)).
  - [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    takes over from
    [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    as the entry point for varPro partial dependence plots. It accepts
    an optional `object` argument (the originating `varpro` fit) which
    it uses for provenance-aware axis labels, and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - Ensemble mortality labeling (Ishwaran et al. 2008): with
    `scale = "mortality"`, or `scale = "auto"` on a survival forest, the
    y-axis reads “Ensemble mortality (expected events)”. That is an
    unbounded relative-risk score, not a survival probability, and the
    documentation says so plainly so it is not misread.
  - Survival path C: with `scale = "surv"` or `scale = "chf"`,
    [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    pulls the embedded rfsrc forest from `object$rf` and returns true
    S(t) or CHF partial curves through the existing `gg_partial_rfsrc`
    machinery.
  - `varPro` is now a hard dependency (`Imports:`).
  - [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md)
    is soft-deprecated: it warns, then hands off to
    [`gg_partial_varpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md).
    It will be removed in the release after v3.0.0.
- randomForest engine validation and repair
  ([\#82](https://github.com/ehrlinger/ggRandomForests/issues/82)).
  Fixes [\#80](https://github.com/ehrlinger/ggRandomForests/issues/80),
  [\#81](https://github.com/ehrlinger/ggRandomForests/issues/81), and a
  `plot.gg_error` label wart, and adds full randomForest regression test
  coverage. Details below.
  - [`plot.gg_variable()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_variable.md)
    now always returns a single `ggplot` (one variable) or a `patchwork`
    composite (several variables, or the default) — never a bare list.
    This matches the v2.7.3 `plot.gg_partial*` change. A list used to
    come back for multiple `xvar`, which broke `patchwork` /
    [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
    /
    [`layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
    composition
    ([\#80](https://github.com/ehrlinger/ggRandomForests/issues/80)).
  - [`gg_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_roc.rfsrc.md)
    and
    [`calc_roc()`](https://ehrlinger.github.io/ggRandomForests/reference/calc_roc.rfsrc.md)
    for `randomForest` now build the ROC from class probabilities (OOB
    votes by default, honoring `oob`) rather than the degenerate
    three-point curve they produced before. With `which_outcome = "all"`
    (the default for `gg_roc(rf)`) the result is a macro-averaged
    one-vs-rest ROC, and no warning. The shared
    `.validate_which_outcome` helper and `calc_roc.rfsrc` are
    byte-for-byte unchanged, so rfsrc behavior is untouched
    ([\#81](https://github.com/ehrlinger/ggRandomForests/issues/81)).
- Dependency modernization. This breaks scripts that relied on
  attachment. `randomForestSRC` and `randomForest` move from `Depends:`
  to `Imports:`; `igraph`, `callr`, and `varPro` are added to
  `Suggests:` (`varPro` later moves up to `Imports:`, with the first
  varPro-integration component).
  [`library(ggRandomForests)`](https://github.com/ehrlinger/ggRandomForests)
  no longer puts `randomForestSRC` or `randomForest` on the search path.
  A script that called
  [`rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html) or
  [`randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  unqualified after only
  [`library(ggRandomForests)`](https://github.com/ehrlinger/ggRandomForests)
  now needs its own
  [`library(randomForestSRC)`](https://www.randomforestsrc.org/) /
  [`library(randomForest)`](https://www.stat.berkeley.edu/~breiman/RandomForests/),
  or must qualify the calls. ggRandomForests itself is unaffected. It
  qualifies every call into its dependencies.

## ggRandomForests v2.7.3

CRAN release: 2026-05-12

- [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md),
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md),
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  now always return a single `ggplot`/`patchwork` object. Previously,
  when both continuous and categorical predictors were present, they
  returned a named list `list(continuous=, categorical=)`, which
  surprised users and made
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  dispatch ambiguous. The two panels are now combined vertically via
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  (patchwork moved from `Suggests` to `Imports`). Closes
  [\#77](https://github.com/ehrlinger/ggRandomForests/issues/77).
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  S3 methods for all 10 `gg_*` classes, delegating to the corresponding
  `plot.gg_*()` method so objects work in `|>` pipelines, `patchwork`,
  and `cowplot` compositions via
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) S3 methods for
  every `gg_*` data object (gg_error, gg_vimp, gg_rfsrc, gg_variable,
  gg_partial, gg_partial_rfsrc, gg_partialpro, gg_roc, gg_survival,
  gg_brier). [`print()`](https://rdrr.io/r/base/print.html) is
  header-only — use [`head()`](https://rdrr.io/r/utils/head.html) for
  rows. [`summary()`](https://rdrr.io/r/base/summary.html) returns a
  printable `summary.gg` object with per-class diagnostics. Each `gg_*`
  constructor now attaches a `"provenance"` attribute (source, family,
  ntree, n, xvar.names) consumed by the new methods.
- New
  [`gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  extractor and
  [`plot.gg_brier()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_brier.md)
  method for time-resolved Brier scores and CRPS on survival forests
  (issue [\#9](https://github.com/ehrlinger/ggRandomForests/issues/9)).
  Wraps
  [`randomForestSRC::get.brier.survival()`](https://www.randomforestsrc.org//reference/plot.survival.rfsrc.html)
  and adds the mortality-quartile decomposition, a 15-85 percent
  per-subject envelope, and running CRPS via trapezoidal integration.
  Supports `cens.model = c("km", "rfsrc")`, `type = c("brier", "crps")`,
  and `envelope` (overall line + 15-85% ribbon). Multi-model comparison
  is left to
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on multiple `gg_brier` outputs — see
  [`?gg_brier`](https://ehrlinger.github.io/ggRandomForests/reference/gg_brier.md)
  for an example.
- Visual unification of ribbon overlays across plot methods. All ribbons
  now use a shared alpha (`.gg_ribbon_alpha = 0.2`) and a shared fill
  (`.gg_ribbon_fill = "steelblue"`) for single-series cases (KM/NA CIs,
  bootstrap CIs, `gg_brier` envelope); group-stratified ribbons keep
  their group-colored fill. Statistical bounds unchanged — only styling.
  ggRandomForests v2.7.2 =====================
- Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and Ishwaran
    et al. 2008, with `<doi:...>` auto-links) per CRAN cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace [`cat()`](https://rdrr.io/r/base/cat.html) in
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    with [`message()`](https://rdrr.io/r/base/message.html) so progress
    output is suppressible
    ([`suppressMessages()`](https://rdrr.io/r/base/message.html)) and
    plays nicely inside notebooks / Shiny / quarto.
  - Restore the user’s [`par()`](https://rdrr.io/r/graphics/par.html)
    settings in the
    [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
    example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

## ggRandomForests v2.7.1

- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests:
  [`partial.rfsrc()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  was being called without `partial.type`, causing a zero-length
  comparison (`if (partial.type == "rel.freq") ...`) inside the C-level
  prediction routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new
  `partial.type` argument accepting `"surv"`, `"chf"`, or `"mort"`).
  This unblocks the `partial-dep` chunk in the survival vignette.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  for survival forests with multiple `partial.time` values:
  [`get.partial.plot.data()`](https://www.randomforestsrc.org//reference/partial.rfsrc.html)
  returns yhat as an `[length(partial.values) x length(partial.time)]`
  matrix, but the previous code assumed a vector and crashed on
  column-mismatch when assigning `time`. The result is now reshaped to
  long form so each `(x, time)` pair is a single row.
- Improve
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  survival layout: predictor value is now on the x-axis with one curve
  per (rounded) time point colored by `Time`, faceted by variable name.
  The previous default put time on the x-axis and one curve per
  predictor value, producing a saturated legend with dozens of
  nearly-identical lines.
- Add `tests/testthat/test_plot_layer_data.R`: regression suite that
  uses
  [`ggplot2::layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  to verify each `plot.gg_*()` method renders non-empty layers for every
  supported forest family. Catches the empty-figure class of bug
  (transform/plot column-name mismatch) without requiring visual
  inspection.
- [`ggrandomforests.news()`](https://ehrlinger.github.io/ggRandomForests/reference/ggrandomforests.news.md)
  now reads `NEWS.md` (the canonical change log R also surfaces via
  [`utils::news()`](https://rdrr.io/r/utils/news.html)). The legacy
  hand-maintained `inst/NEWS` has been removed — it had silently drifted
  to v2.4.0 (June 2025) across three releases, so users running the
  helper saw stale version info. One source of truth, no more drift
  window.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  legend duplication: the bar geom mapped both `fill` and `color` to the
  `positive` column, but only the fill legend was titled “VIMP \> 0”,
  leaving a redundant second legend titled “positive”. Both aesthetics
  now share the “VIMP \> 0” title so ggplot merges them into a single
  legend by default.
- Fix
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  for forests with all-positive VIMP: the bar geom previously mapped
  only `color` (no `fill`), producing hollow / outline- only bars and an
  “Ignoring unknown labels: fill” warning whenever `labs(fill = ...)`
  was applied. Both `fill` and `color` are now mapped unconditionally,
  so bars render filled in every case.
- Add `@examples` blocks to
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md).
  The latter uses a self-contained mock of the `varpro::partialpro()`
  output structure so the example runs without pulling in `varpro` as a
  dependency.

## ggRandomForests v2.7.0

- S3 design overhaul:
  [`gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial.md),
  [`gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_varpro.md),
  and
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  now stamp their return values with S3 classes (`gg_partial`,
  `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatch
  without any boilerplate.
- Add
  [`plot.gg_partial()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial.md),
  [`plot.gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_rfsrc.md),
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md)
  S3 methods; continuous predictors render as line plots, categorical as
  bar charts, faceted by variable name. Survival forests produce curves
  over time; two-variable surface plots group by `xvar2.name`.
- Convert
  [`gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  to an S3 generic dispatching on the class of its first argument. New
  [`gg_survival.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  method extracts the survival response directly from the fitted forest
  (no separate data argument needed);
  [`gg_survival.default()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_survival.md)
  preserves the existing interface.
- Fix
  [`plot.gg_survival()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_survival.md)
  auto-coercion: previously called `gg_survival(rfsrc_obj)` treating the
  forest as the `interval` string argument, causing a latent crash;
  replaced with [`inherits()`](https://rdrr.io/r/base/class.html) guard.
- Deprecate
  [`surv_partial.rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/surv_partial.rfsrc.md)
  via [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html) with a
  pointer to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md);
  all package tests updated to suppress the warning.
- Fix
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md)
  — `make_eval_grid()` used `unlist(dplyr::select())` which coerced
  factor columns to integer codes; now uses `newx[[xname]]` to preserve
  column class. Categorical detection extended to cover
  [`is.factor()`](https://rdrr.io/r/base/factor.html) and
  [`is.character()`](https://rdrr.io/r/base/character.html) in addition
  to the cardinality check.
- Add guards to
  [`gg_partial_rfsrc()`](https://ehrlinger.github.io/ggRandomForests/reference/gg_partial_rfsrc.md):
  all-NA `xval` after NA removal now emits a warning and skips the
  variable; all-NA grouping variable (`xvar2`) calls
  [`stop()`](https://rdrr.io/r/base/stop.html); `n_eval` and `cat_limit`
  are validated as single integers \>= 2 near function entry.
- Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
- Fix `@param partial.time` documentation: “see the section above”
  corrected to “see the section below”.
- Replace deprecated
  [`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html)
  with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  in
  [`plot.gg_vimp()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_vimp.md)
  and
  [`plot.gg_partialpro()`](https://ehrlinger.github.io/ggRandomForests/reference/plot.gg_partial_varpro.md).
- Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
- Update tests: add `expect_s3_class()` checks for all new classes; add
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) smoke tests
  for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`; add
  `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
- Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and
  `plot.gg_partialpro` to `_pkgdown.yml` reference index.

## ggRandomForests v2.7.0

- Fix critical visual bug in `plot.gg_rfsrc`: all
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) calls used
  bare string literals instead of `.data[[col]]`, causing every
  aesthetic to map to a constant string rather than the underlying data
  column. All plot types (regression, classification, survival) were
  affected.
- Fix [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  bare-string literals in `plot.gg_roc` multi-class branch; remove
  unreachable `if (crv < 2)` dead-code branch.
- Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative
  index computed via
  [`colnames()`](https://rdrr.io/r/base/colnames.html) was a no-op on
  large datasets and a latent crash for data with ≤ 2 unique event
  times.
- Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing
  columns; replaced with `!col %in% colnames()` guard.
- Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`;
  now recovers the training frame via `.rf_recover_model_frame()`.
- Fix legend suppression in `plot.gg_error` for single-outcome forests
  where the data frame has no `variable` column.
- Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with
  `seq_len(nvar)` in both S3 methods; `1:0` silently returned `c(1, 0)`
  instead of `integer(0)` when `nvar == 0`.
- Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all
  `context()` calls removed; testthat 1.x `expect_that` /
  `is_identical_to` removed.
- Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
- Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
- Add `covr` and `vdiffr` to `Suggests`.

## ggRandomForests v2.6.1

- Fix model-label assignment in `gg_partial` for categorical variable
  data
- Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalization and categorical data handling

## ggRandomForests v2.6.0

- Add and export new plotting functions; update existing plot
  documentation
- Improve unit and integration tests; overall coverage raised to 83%
- Remove `hvtiRutilities` internal dependency; clean up associated
  imports
- Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr`
  calls

## ggRandomForests v2.5.0

- Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via
  [`randomForestSRC::partial.rfsrc`](https://www.randomforestsrc.org//reference/partial.rfsrc.html),
  without requiring a separate `plot.variable` call
- Add support for a grouping variable (`xvar2.name`) in
  `gg_partial_rfsrc`
- Improved vignette formatting and namespace usage

## ggRandomForests v2.4.0

- Updating to latest ggplot2 functions
- Utilize some namespace referencing
- Added pkgdown documentation
- Minor testing improvements

## ggRandomForests v2.3.0

- Knocking the dust off this.
- Fix the ROC curves
- Fix the colors on VIMP plot

## ggRandomForests v2.2.1

CRAN release: 2022-09-01

- Fix docs for HTML5/Roxygen update

## ggRandomForests v2.2.0

CRAN release: 2022-05-09

- Bring back the regression vignette
- Improve package tests and code coverage
- Clean up code with lintr

## ggRandomForests v2.1.0

CRAN release: 2022-04-26

To pull this out of archive on randomForestSRC 3.1 build release. Fixed
a plot bug for gg_error to show the actual curve (issue 35)

## ggRandomForests v2.0.1

CRAN release: 2016-09-07

- Correct a bug in survival plots when predicting on future data without
  a known outcome.
- All Vignettes are now at <https://github.com/ehrlinger/ggRFVignette>
- All tests are being moved to
  <https://github.com/ehrlinger/ggRFVignette>
- Begin work on rewriting all checks to not use cached data. This will
  require more runtime, and hence we will run fewer of them on CRAN
  release.
- Minor bug and documentation fixes.

## ggRandomForests v2.0.0

CRAN release: 2016-06-11

- Added initial support for the randomForest package
- Updated cache files for randomForestSRC 2.2.0 release.
- Remove regression vignettes to meet CRAN size limits. These remain
  available at the package source
  <https://github.com/ehrlinger/ggRandomForests>
- Minor bug and documentation fixes.

## ggRandomForests v1.2.1

CRAN release: 2015-12-12

- Update cached datasets for randomForestSRC 2.0.0 release.
- Correct some vignette formatting errors (thanks Joe Smith)

## ggRandomForests v1.2.0

CRAN release: 2015-11-15

- Convert to semantic versioning <http://semver.org/>
- Updates for release of ggplot2 2.0.0
- Change from reshape2::melt dependence to tidyr::gather
- Optimize tests for CRAN to optimize R CMD CHECK times.

## ggRandomForests v1.1.4

CRAN release: 2015-03-29

- `combine.gg_partial` bug when giving a single variable plot.variable
  object.

- Remove `dplyr` depends to transitions from “Imports” to “Suggests”.

- Argument for single outcome `gg_vimp` plot for classification forests.

- Improvements to `gg_vimp` arguments for consistency.

- Add bootstrap confidence intervals to `gg_rfsrc` function.

- Initial `partial.rfsrc` function to replace the
  [`randomForestSRC::plot.variable`](https://www.randomforestsrc.org//reference/plot.variable.rfsrc.html)
  function.

- Move cache data to `randomForestSRC` v1.6.1 to take advantage of
  `rfsrc` version checking between function calls.

- Vignette updates for JSS submission of “ggRandomForests: Exploring
  Random Forest Survival”.

- Vignette updates for arXiv submission of ggRandomForests: Random
  Forests for Regression

- Some optimizations to reduce package size.

- Remove all tests from CRAN build to optimize R CMD CHECK times.

- Remove pdf vignette figure from CRAN build.

- Return S3method calls to NAMESPACE for “S3 methods exported but not
  registered” for R V3.2+.

- Misc Bug Fixes.

## ggRandomForests v1.1.3

CRAN release: 2015-01-08

- Update “ggRandomForests: Visually Exploring a Random Forest for
  Regression” vignette.
- Further development of draft package vignette “Survival with Random
  Forests”.
- Rename vignettes to align with randomForestSRC package usage.
- Add more tests and example functions.
- Refactor `gg_` functions into S3 methods to allow future
  implementation for other random forest packages.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.2

CRAN release: 2014-12-25

- Add package vignette “ggRandomForests: Visually Exploring a Random
  Forest for Regression”
- Add gg_partial_coplot, quantile_cuts and surface_matrix functions
- export the calc_roc and calc_auc functions.
- replace tidyr function dependency with reshape2 (melt instead of
  gather) due to lazy eval issues.
- reduce dplyr dependencies (remove select and %\>% usage for base
  equivalents, I still use tbl_df for printing)
- Further development of package vignette “Survival with Random Forests”
- Refactor cached example datasets for better documentation, estimates
  and examples.
- Improved help files.
- Updated DESCRIPTION file to remove redundant parts.
- Misc Bug Fixes.

## ggRandomForests v1.1.1

CRAN release: 2014-12-13

Maintenance release, mostly to fix gg_survival and gg_partial plots. \*
Fix the gg_survival functions to plot kaplan-meier estimates. \* Fix the
gg_partial functions for categorical variables. \* Add some more S3
print functions. \* Try to make gg_functions more consistent. \* Further
development of package vignette “Survival with Random Forests” \* Modify
the example cached datasets for better estimates and examples. \*
Improve help files. \* Misc Bug Fixes.

## ggRandomForests v1.1.0

CRAN release: 2014-12-05

- Add panel option for gg_variable and gg_partial
- Rework interactions plot
- add gg_coplot functions
- Imports instead of depends
- Add version dependencies for randomForestSRC
- Include package vignette “Random Forests for Survival”
- Misc Bug Fixes

## ggRandomForests v1.0.0

CRAN release: 2014-10-15

- First CRAN release.

## ggRandomForests v0.2

- Initial useR!2014 release.
