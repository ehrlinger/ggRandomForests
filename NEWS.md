Package: ggRandomForests
Version: 4.0.0

ggRandomForests v4.0.0 (development)
====================================
* `gg_partial_varpro()` gains `scale = "prob_typical"`. `partialpro()` returns
  per-subject log-odds, and collapsing them to a curve takes an average and a
  back-transform; the ORDER is a modelling choice. `"prob"` (unchanged, still
  the classification default) transforms per observation then averages, giving
  the mean predicted probability -- the expected proportion of the cohort.
  `"prob_typical"` averages on the log-odds scale then transforms once, giving
  the probability for a subject at the mean log-odds.

  They are different estimands and they disagree. The inverse logit is concave
  above zero and convex below it, so by Jensen `"prob"` is pulled toward 0.5 at
  both ends, by more the more heterogeneous the cohort. Where the per-subject
  log-odds carry an SD near 4.5, a point reading 0.96 under `"prob_typical"`
  reads 0.74 under `"prob"` -- large enough to change how a figure is read, so
  the choice should be deliberate. A figure captioned as a percentage of
  patients wants `"prob"`. `?gg_partial_varpro` sets out both.

  The distinction applies only to the `continuous` frame; the `categorical`
  frame keeps values unaveraged, so both scales return the same numbers there.
* `plot.gg_partial_varpro()` gains `ylim`, pinning the shared y range across
  panels. It could not be set from outside: on the `panels` route a
  `coord_cartesian()` added with `&` replaces the per-panel coordinate system
  and silently takes the per-panel x ranges with it (0-50 collapsed to the
  data's 0-46), while `scale_y_continuous(limits = )` is overridden by that
  same coordinate system. `ylim = c(0, 1)` now pins a probability axis so a
  flat curve reads as flat instead of filling the panel.
* `plot.gg_partial_varpro()` now defaults `palette` to `"black"`. These figures
  are made for manuscripts, and `linetype` is mapped to the effect type as
  well, so the three estimators stay legible as solid, dotted and dashed with
  no colour at all. Pass any ColorBrewer name (`palette = "Set1"`) for the
  colour scale, which separates two or three overlaid estimators faster on
  screen. `"mono"` is a synonym for black and `"grey"`/`"gray"` give a flat
  grey. **This changes rendered output**: five vdiffr baselines were
  regenerated.
* `plot.gg_partial_varpro()` gains `complement`, plotting 1 - p and prefixing
  the y label with `1 - `. It reads a fit that targets one class as the
  probability of the other -- a weaning-failure model shown as probability of
  successful weaning -- without recomputing `partialpro()` against the other
  target. Requires a probability scale (`prob` or `surv`); on the additive,
  multiplicative and unbounded scales 1 - x has no referent, so it errors
  rather than drawing something unreadable.
* `plot.gg_partial_varpro()` now warns, naming them, when arguments reach `...`
  that it does not use. Its own arguments sit after `...` and match by exact
  name, so a typo (or an argument from a newer version than the one installed)
  previously vanished without a word and left the default plot looking like a
  correct answer.
* `plot.gg_partial_varpro()` gains per-panel scale control. `facet_wrap(scales =
  "free_x")` gives each variable its own x *range* but a single shared x
  *scale*, so per-panel breaks, limits and axis titles were unreachable and a
  manuscript figure had to be hand-built one `ggplot()` per variable. A new
  `panels` data frame -- one row per panel, keyed by `name`, with optional
  `xlab`, `xmin`, `xmax`, `xby` and `span` columns -- switches rendering to
  patchwork, where the x scale can vary between panels. `panels = NULL`
  (the default) is unchanged.
* `plot.gg_partial_varpro()` gains `which`, to return the continuous or
  categorical frame alone as a bare `ggplot`. With both frames populated the
  method returns a patchwork, where `+` reaches only the last panel, so adding
  a scale or theme silently modified the categorical plot. `which` is the
  supported way to get one plot to modify.
* `plot.gg_partial_varpro()` gains `points`, `smooth`, `palette`, `ncol`,
  `point_size`, `point_alpha` and `linewidth`. `palette` takes a ColorBrewer
  name and goes through ggplot2's brewer scales, so `RColorBrewer` stays
  in `Suggests`. All default to the previous rendering.
* The survival vignette's variable-dependence figure passed `time.labels` where
  `gg_variable()` reads `time_labels`. The dotted name matched nothing and was
  dropped without a warning, so the facet strips rendered as bare "1" and "3"
  instead of the intended "1 Year" and "3 Years". Corrected; the figure now
  carries the labels its code always asked for.
* Development line opened after the v3.2.0 CRAN release (forward-merged the
  v3.2.0 RMST/varPro fixes onto the dev line).
* Begin the v4.0.0 development line: a Random Hazard Forests (RHF)
  visualization layer wrapping the 'randomForestRHF' package (added to
  Suggests). RHF support is gated — every gg_rhf* entry point checks
  `requireNamespace("randomForestRHF")`. No change for users who do not
  install it.
* The consistency sweep distinguishes current CRAN software versions from
  supported minimum versions and standardizes the three package-qualified fit
  calls and object classes: `randomForestSRC::rfsrc()` -> `rfsrc`,
  `randomForestRHF::rhf()` -> `rhf`, and `varPro::varpro()` -> `varpro`.
* Add a longitudinal RHF vignette covering `gg_rhf()`, `gg_auct()`,
  `gg_rhf_importance()`, and `gg_tune_rhf()` from one saved analysis.
* `gg_auct()` / `plot.gg_auct()`: tidy wrapper and plot for time-varying
  AUC from `randomForestRHF::auct.rhf()` (RHF Phase 2). Returns a long
  frame `time / auc / se / lower / upper / marker` with an `iauc`
  attribute (Uno + standardized integrated AUC); `plot.gg_auct()` draws
  AUC(t) with a bootstrap CI ribbon when available and a 0.5 reference
  line. `gg_auct.rhf(object, marker, auct_fit = NULL)` computes
  `auct.rhf()` internally or reuses a cached fit.
* `gg_rhf_importance()` / `plot.gg_rhf_importance()`: tidy wrapper and point
  matrix for time-localized variable priority from
  `randomForestRHF::importance.rhf()` (RHF Phase 3). It returns
  `variable / time_window / time / time_index / start / stop / midpoint /
  n_risk / n_rules / priority`, accepts a supplied `importance_fit` or
  calculates one when absent, and orders variables by their q90 priority over
  time windows. Priority is a ranking score, not a z-score; no selection
  cutoff is applied.
* `gg_tune_rhf()` / `plot.gg_tune_rhf()`: supplied-object-only inspection of a
  `tune.treesize.rhf` tree-size tuning path. The five returned columns are
  `treesize / metric / value / se / selected`; the plot marks the selected
  size and draws an iAUC standard-error ribbon only when finite supplied iAUC
  standard errors are available. `gg_tune_rhf()` never recalculates tuning.
* Require `randomForestRHF (>= 2.0.3)` in Suggests, and adopt its revised
  hazard semantics. From 2.0.0 the pointwise hazard is defined only where a
  grid point falls inside one of the case's supplied `(start, stop]`
  intervals, and is `NA` in gaps and after the final stop. 2.0.0 left the
  cumulative hazard unmasked; 2.0.3 masks it as well, on its own rule, setting
  it to `NA` after each case's final stop while still holding it flat through
  an internal gap. The two masks therefore coincide on a fit whose cases carry
  a single interval each, and come apart only with time-dependent covariates.
  `auct.rhf()` can likewise return an `NA` AUC at the final grid time, where
  the censoring-weight denominator is undefined once the control set is nearly
  exhausted. `gg_rhf()` passes both masks through unchanged, so `hazard` and
  `chf` may be `NA` where they previously were not; `plot.gg_rhf()` and
  `plot.gg_auct()` drop those cells before drawing, so a curve now ends with
  its case's follow-up instead of reporting removed missing values on every
  plot. 2.0.0 also changes the default hazard aggregation (`adaptive = TRUE`),
  which shifts fitted values, and 2.0.3 corrects cumulative/dynamic
  `auct.rhf()`, which had inverted that curve; the RHF vdiffr baselines and
  the precomputed vignette analysis were regenerated against 2.0.3. This
  resolves issue #229, where the earlier reading (a small negative hazard,
  specific to the macOS arm64 binary) was wrong on both counts, and
  kogalur/randomForestRHF#1, the inverted cumulative/dynamic AUC.
* `gg_auct()` gains a `method` argument and now forwards `...` to
  `randomForestRHF::auct.rhf()`. `auct.rhf()` defaults `method` to
  `"cumulative"`, and `gg_auct()` previously passed only `marker`, so the
  incident/dynamic definition could not be reached from `gg_auct()` at all:
  the only route was to call `auct.rhf()` directly and hand the result back
  through `auct_fit`. `?gg_auct` now carries a note on choosing between the
  two definitions, which estimate different targets rather than better and
  worse versions of one. Forwarding
  `...` also makes `bootstrap.rep` reachable, so the confidence ribbon no
  longer requires precomputing the fit. `method` sits after `auct_fit` in the
  signature, so positional calls are unchanged, and the default behavior is
  the same as before.
* `plot.gg_partial_varpro()`, `plot.gg_partial()`, `plot.gg_vimp()` and
  `plot.gg_varpro()` gain a `labels` argument for human-readable variable
  names. It accepts a named character vector, a labelled data frame (reading
  `attr(col, "label")`), or a two-column `key`/`label` data frame. Variables
  with no label keep their raw name. Labels apply at draw time only — the
  returned object still carries raw variable names, so downstream consumers
  are unaffected. The argument reaches every branch of these methods, not just
  their default one: `plot.gg_varpro()` honours it on both the main panel and
  the class-conditional panel, and `plot.gg_partial_varpro()` honours it on
  survival path-C objects (those extracted with `scale = "surv"` or `"chf"`),
  which are handed off to `plot.gg_partial_rfsrc()`.
* `plot.gg_rhf_importance()` also gains `labels`, so the RHF priority matrix can
  carry human-readable variable names. It takes the same three shapes and falls
  back to the raw name per variable. The variable axis here is `y`, not a
  flipped `x`, so the labelled scale is the y scale. The q90 variable ordering
  and the raw names in the returned data are untouched, and
  `autoplot.gg_rhf_importance()` forwards the argument. Previously `labels` fell
  through `...` into `ggplot2::geom_point()` and was dropped with only ggplot2's
  generic "Ignoring unknown parameters" warning, so the call looked accepted and
  did nothing.
* `plot.gg_beta_varpro()`, `plot.gg_ivarpro()` and `plot.gg_beta_uvarpro()`
  gain `labels` on the same terms, completing the varPro importance family.
  These three had been dropping the argument in complete silence: each declares
  `...` and does not use it, so `labels` was absorbed with no warning, no
  error, and an unlabelled plot as the only symptom. Their facets are per class
  rather than per variable, so the class strips are left alone and only the
  variable axis is relabelled; in a faceted plot every panel is relabelled.
* `plot.gg_shap()` and the three exported mode functions it dispatches to,
  `shap_importance()`, `shap_beeswarm()` and `shap_dependence()`, gain
  `labels`. Each of the three puts variable names somewhere different, so each
  honours the argument differently: `shap_importance()` labels a flipped
  discrete `x` scale, `shap_beeswarm()` labels `y` directly because it does not
  flip, and `shap_dependence()` has no variable scale at all and substitutes
  the label into both axis titles instead. In that last mode `xvar` still
  matches on raw variable names, so the label is display only and passing a
  label where a variable name belongs is still an error. As with the varPro
  methods above, `labels` was previously accepted and discarded in silence.
* `plot.gg_variable()` no longer declares `time` and `time_labels`, two formals
  its body never read. They are parameters of `gg_variable()`, the extractor,
  which bakes the horizon into the object before `plot()` runs, so the man page
  had been promising a horizon selection the method never performed. Supplying
  either now warns and names the call that works. ⚠️ `time_units` moved to
  **after** `...` in the signature as part of this: R partial-matches argument
  names only before `...`, so with `time_units` ahead of it a caller writing
  `time = 1191` bound silently to `time_units` and died on its type check.
  Past the dots, matching is exact. `time_units` always had to be named, so no
  working call changes.
* `plot.gg_variable()` sanity-checks `time_units` against the data it describes. A year-like
  unit supplied against values above 150 warns, because that is almost always a
  forest fit on a smaller unit and produces an axis title wrong by a factor of
  365. It warns rather than errors, and only in that one direction: small values
  labelled `"days"` is ordinary, so there is no signal to check. The package
  still cannot derive the unit and does not try to. Scoped to
  `plot.gg_variable()` for now: `plot.gg_rfsrc()` also takes `time_units`, but a
  `gg_rfsrc` object has no `time` column (its time points live in `variable`,
  which holds class names for a classification fit), so there is no unambiguous
  column to check and extending it is not a one-line change.
* `plot.gg_variable()`, `plot.gg_udependent()` and `plot.gg_sdependent()` gain
  `labels`, which completes the argument across every plot method in the
  package that renders variable names. `plot.gg_variable()` labels the facet strips in its panel
  plot, through all three faceting branches, and the x axis title in its
  individual plot; the `time` facet is untouched, because it facets by time
  rather than by variable, and the multi-time survival panel scopes its
  labeller to the variable dimension so a label key that collides with a time
  value cannot reach the time strips. `plot.gg_udependent()` labels the node text of its dependency
  network. There the display string is written to a separate vertex attribute
  and the igraph `name` is left alone, because `name` is the key the
  edge-weight backfill matches on and rewriting it would break edge weights on
  graphs saved before those weights were stored. `plot.gg_sdependent()` is the
  plain case, a flipped discrete axis like `plot.gg_vimp()`.
* `plot.gg_vimp()`: `lbls` is **deprecated** in favour of `labels` and will be
  removed in a future release. Its old `length(lbls) >= length(vars)` gate is
  also gone, so a partial label set is now honoured, falling back to the raw
  name per variable. Previously supplying fewer labels than variables silently
  applied none.
* `gg_partial_varpro()` orders variables by varPro importance
  (`varPro::get.topvars()`) when `object` is supplied, and `name` is now a
  **factor**, so facets follow importance order instead of being re-sorted
  alphabetically. Variables absent from the ranking keep their incoming order
  and are appended after the ranked block; none are dropped.
* `gg_partial_varpro()`: **`nvars` now selects the top n by importance.** It
  previously took the first n elements of the partial-dependence list before
  any ranking was applied, returning an arbitrary subset with no symptom in
  the output.
* `gg_partial_varpro()` warns when `scale = "auto"` cannot be resolved because
  no `object` was supplied, instead of silently falling back to the generic
  "Partial Effect" axis. The fallback label itself was never wrong — it was
  honest about an unknown scale — but the silence around it was, so the
  fallback now says so.
* The `labels` lookup now drops entries whose label or name is blank or `NA`,
  so a variable given an empty label falls back to its raw name rather than
  drawing blank axis or strip text. All three accepted input shapes now agree
  on the same information; previously the labelled-data-frame arm dropped
  blanks while a named vector or `key`/`label` frame kept them.
* `gg_partial_varpro()` now rejects an unnamed `part_dta` with a clear error
  instead of accepting it. The names of that list *are* the variable
  identities; without them the constructor cannot build a `name` column at all,
  and the omission used to surface two calls later as an opaque `facet_wrap()`
  failure about a missing faceting variable. An empty `part_dta` remains legal.
* The package's own vignettes (`ggRandomForests-regression.qmd`,
  `ggRandomForests-survival.qmd`) are moved off the now-deprecated `lbls`
  argument onto `labels`, so the shipped examples model the current API
  rather than the one being phased out.
* `plot.gg_variable()` and `plot.gg_rfsrc()` no longer hard-code a "year" time
  unit in survival axis titles. The unit was never derived from the data, so a
  fit measured in days -- `randomForestSRC::pbc`, this package's own canonical
  survival example, among them -- rendered "Survival at 1191 year" for a horizon
  of 1191 days. The titles now read "Survival at 1191" and "time" by default,
  and a new `time_units` argument on both methods restores an explicit unit:
  `time_units = "days"` gives "Survival at 1191 days" and "time (days)". Users
  whose data really is in years should pass `time_units = "years"` to keep the
  word.
* `plot.gg_variable()`'s survival branch has visual regression cover for the
  first time. The branch forks on `panel` and on whether the object carries one
  time or several, and the four resulting paths differ in both faceting and
  y-axis title; each now has a `vdiffr` baseline. The two single-time paths are
  the ones that render a time unit into the axis title, so a change to that
  title now surfaces as an SVG diff rather than resting on an `expect_equal()`
  of `p$labels$y`, which cannot see the rest of the panel. Tests only.

ggRandomForests v3.5.2
======================
* Three help pages no longer render a stray backslash where a percent sign
  belongs. roxygen2 escapes `%` for you, so the `\%` written in the roxygen
  prose of `calc_auc()`, `gg_isopro()` and `plot.gg_isopro()` reached the `.Rd`
  as `\\%` and rendered as `50\%` rather than `50%`. Documentation only.
* `R CMD check` is back inside CRAN's ten-minute budget. On the 3.5.1
  win-builder run the vignette rebuild was 287s and the tests 195s of a
  12-minute total, so both were cut at the source rather than moved around.
  The `rfsrc` vignettes grow smaller forests (Boston and iris at 100 trees,
  the `pbc` impute-and-fit pair at 50 and 100) and coarser partial-dependence
  surface grids (6 and 5 points, from 10 and 8); the SHAP sections explain 25
  rows against 30 background draws instead of 40 against 50. In the examples,
  `gg_error()` and `plot.gg_error()` grow 100 trees instead of 250, and
  `gg_vimp()` and `plot.gg_vimp()` 50 instead of 100. The four heaviest test
  files, `gg_udependent`, `gg_varpro`, `gg_variable` and `gg_vimp`, now
  `skip_on_cran()`; they still run in full under `devtools::test()`. No
  function, argument or returned object changed.
* `?gg_partial_varpro` now documents varPro's missing-data contract, which
  governs every fit this package plots. varPro has no imputation: each entry
  point grows a stump through `randomForestSRC::rfsrc` and inherits its
  `na.action = "na.omit"`, so any case missing a predictor or the outcome is
  deleted before the fit, silently. `na.action = "na.impute"` passed to
  `varpro()` lands in `...` and is discarded without remark. The loss
  compounds as `0.95^p`, and the fitted object keeps only the post-deletion
  count, so neither the user nor this package can recover the original from
  the object -- the check has to happen before the fit.
* The same section covers imputing beforehand without inventing outcomes.
  `roughfix()` and `randomForestSRC::impute()` both fill every column handed
  to them, the outcome included, so a frame with missing outcomes comes back
  with manufactured responses and the release rules are fit partly to them.
  Documents von Hippel's impute-then-delete, and the two cautions that follow:
  outcome-informed imputation crosses fold boundaries in `cv.varpro()`, and a
  completed frame carries no imputation uncertainty into the curves.

ggRandomForests v3.5.1
======================
* Test-only fix for the `gcc-UBSAN` additional issue reported against 3.5.0.
  One test grew an isolation forest with no outcome, which makes
  `randomForestSRC` hand `yvar.wt = numeric(0)` to its native code and
  decrement that zero-length pointer (`entry.c:184`). The route was indirect:
  `gg_partial_varpro()` calls `varPro::partialpro()`, which grows its own
  `isopro()` forest and lets `method` default to `"unsupv"`. The other
  live-`partialpro()` tests were already `skip_on_cran()`'d, which left this
  one as the only one running on CRAN. It now requests `method = "rnd"`
  itself; it asserts the same warnings over the same number of rows. No
  user-facing code changed -- `ggRandomForests` is pure R, and the undefined
  behaviour is upstream.
* The comments in the varPro test fixtures claimed the report fires only for
  `isopro(method = "unsupv")`. That was true of direct calls and wrong as a
  rule about the package, which is why the `partialpro()` route went unnoticed.
  They now state the actual condition -- any `rfsrc` grow reached without a
  formula -- and name the paths that satisfy it.
* An audit of the rest of the package found one more call on the same path:
  the `\donttest` example in `?gg_partial_varpro` used the object path without
  `method = "rnd"`. It did not fire on CRAN only because that check flavor did
  not run `\donttest` code, which is CRAN's setting to change rather than ours,
  so the example now passes `method = "rnd"` too. Every other varPro entry
  point is clean: `gg_varpro()`, `gg_ivarpro()`, `gg_udependent()`,
  `uvarpro()` (defaults to a formula-based `method = "auto"`) and every
  `isopro()` call in the package, all of which name `method = "rnd"`.
* `?gg_partial_varpro` now documents the underlying issue rather than leaving
  it to the tests. Any `gg_partial_varpro(object = )` call reaches it, because
  `partialpro()` grows its isolation forest with `isopro()`'s default
  `method = "unsupv"`. It is benign -- the pointer is formed, never
  dereferenced -- and the fix belongs upstream (`kogalur/randomForestSRC` PR
  #478); `method = "rnd"` avoids it in the meantime.
* `gg_roc()` on an `rfsrc` forest now honors `which_outcome = 0`. The help page
  has always documented `0` as the numeric spelling of `"all"`, but only the
  string was normalized, so `0` fell through to `predicted[, 0]`. That is a
  legal zero-column subset rather than an error, so the threshold sweep ran on
  empty input and returned a two-row frame with no `sens`/`spec` columns, which
  then broke `calc_auc()`. Both spellings now take the same route: a warning,
  and a fallback to class 1. The macro-average that will replace the fallback
  is still tracked under #72.
* The three ROC entry points still disagree about what "all classes" means --
  `gg_roc()` on a `randomForest` fit macro-averages, `gg_roc()` on an `rfsrc`
  fit falls back to class 1, and a direct `plot.gg_roc()` call on a raw
  multi-class forest overlays one curve per class. That divergence is
  unchanged here, but `?gg_roc` and `?plot.gg_roc` now say so instead of
  implying the paths agree. Both also correct a longer-standing claim: a raw
  forest passed to plain `plot()` never reaches `plot.gg_roc()` at all,
  because `randomForestSRC` and `randomForest` register their own `plot`
  methods and S3 dispatch prefers them. That branch is reachable only by
  naming the method outright. `?gg_roc` further stops advertising character
  class names on the `rfsrc` path, which only the `randomForest` method
  accepts.
* `gg_partial_rfsrc()` validates `rf_model` before using it. It read `$xvar`
  and `$xvar.names` first, so a non-forest failed with base R's "argument is of
  length zero" rather than naming the problem. It now matches the error style
  already used by `gg_error()`, `gg_vimp()`, `gg_variable()` and `gg_rfsrc()`.
* The `pbc` examples on `?gg_error`, `?plot.gg_error`, `?gg_vimp` and
  `?plot.gg_rfsrc` lost their editorial asides and a stray trailing comma in
  the `data()` call. The munging block that all four repeated is now a single
  shared `inst/examples/pbc-setup.R`, pulled in with `@example`, so the four
  pages cannot drift apart.
* Every `rfsrc` fit in an example now names an explicit `ntree`. The examples
  had been taking `rfsrc()`'s 500-tree default, which is far more forest than an
  illustration needs; bounding them took the local `R CMD check` total from
  4m44s to 3m16s.
* `tests/testthat/test_lint.R` runs again, wrapped in `skip_on_cran()`. It had
  been commented out entirely, so the suite enforced nothing about style
  locally even though CI kept its own lint job. The guard keeps it off the
  `R CMD check` clock.

ggRandomForests v3.5.0
======================
* `plot.gg_varpro()` no longer draws a phantom "NA" category when `nvar` is
  smaller than the number of variables the fit reports. `$imp`/`$stats` are
  truncated to `nvar`, but the per-tree overlay (`$imp.tree`) and the
  class-conditional data (`$conditional`) still carry every variable;
  re-levelling those to the truncated `$imp` levels orphaned the extras to
  `NA`, which rendered as an empty box/bar. Those rows are now dropped, so only
  the displayed variables appear.
* The vignettes now render their figures with `ragg` and quantise them to a
  256-color palette, cutting the source tarball from 4.7 MB to 2.3 MB. The
  vignettes had never chosen a graphics device, so they fell through to the
  default `png()`, which writes RGBA truecolor: an alpha channel these opaque
  plots never use, over tens of thousands of anti-aliased colors that PNG
  cannot compress. Figures are visually unchanged (mean pixel difference 1.55
  on a 0-255 scale). Both steps are build-time only and degrade to no-ops when
  `ragg` or `magick` is absent, so a vignette rebuild without them still
  succeeds -- at the old file size.
* The varPro vignette now documents which variables a `varpro` fit actually
  makes available. A fit narrows the predictors twice -- `object$xvar.names`
  holds what `varPro::partialpro()` can reach, `varPro::get.topvars()` only the
  reported ranking -- and `partialpro()` silently drops any requested name
  outside the first set. The new section covers naming `xvar.names` to get past
  the reported ranking, `split.weight = FALSE` to widen the candidate set
  itself, and the two arguments (`nvar`, `sparse`) that look like they should
  help and don't.
* `gg_partial_varpro()` now warns when a name passed in `xvar.names` is one the
  `varpro` fit cannot reach, instead of letting it disappear. `partialpro()`
  keeps only the names it finds in `object$xvar.names` and says nothing about
  the rest, so a request for twelve variables could come back with ten. The
  check runs before `partialpro()` does, so the warning arrives ahead of the
  computation rather than after it; it names every dropped variable and points
  at `split.weight = FALSE`. Supplying `part_dta` yourself is unchanged -- the
  variables are already gone by then. The function's examples now cover the
  object-driven path, which had none.
* `gg_partial_varpro(scale = "chf")` now computes the variables you name in
  `xvar.names` instead of every variable the fit can reach. The `chf` path
  routes through `gg_partial_rfsrc()` rather than `partialpro()`, and it had
  never been given the variable list -- so asking for one variable quietly did
  the work for all fourteen. This is the mirror image of the `partialpro()` bug
  above: that one returns fewer variables than you asked for, this one returned
  all of them. Naming a variable the forest does not carry has always been an
  error and still is. The `partialpro`-only arguments (`cut`, `nsmp`) mean
  nothing on this path and are now ignored with a warning rather than in
  silence.
* Fix: `gg_vimp()` on a `randomForest` fit grown with `importance = TRUE` now
  reports the permutation importance you asked for. It was reporting node
  purity instead, and silently: `randomForest` stores `%IncMSE` and
  `IncNodePurity` side by side, and `gg_vimp()` stacked both into one `vimp`
  column and ranked them together. The two are not commensurable -- node purity
  runs in the thousands where `%IncMSE` runs in the tens -- so every impurity
  row outranked every permutation row, and the truncation to `nvar` cut the
  permutation values away entirely. On `randomForest(medv ~ ., Boston,
  importance = TRUE)` the plot showed `lstat = 12576.7` (node purity) where the
  permutation value is `lstat = 62.4`. Node purity is now left out of the
  ranking; read `randomForest::importance(object)` if you want both. Fits grown
  without `importance = TRUE` are unaffected -- they only ever stored node
  purity, and that is still what you get.
* Fix: `gg_vimp()` on a `randomForest` *classification* fit grown with
  `importance = TRUE` now reports permutation importance as well. That matrix
  mixes the same two scales -- a permutation column per class plus
  `MeanDecreaseAccuracy`, alongside `MeanDecreaseGini` -- but it is wider than
  the single-outcome branch that picks one measure, so it skipped that branch
  and every column was ranked together. `MeanDecreaseGini` came out the sole
  survivor: on `randomForest(Species ~ ., iris, importance = TRUE)`,
  `gg_vimp()` returned 4 rows of node purity where 16 rows of permutation
  importance were there to report. The per-class columns and
  `MeanDecreaseAccuracy` are all permutation measures on one scale, so they are
  now kept together and named in the `set` column, the way an `rfsrc` fit's
  `all`/`<class>` columns already were; only `MeanDecreaseGini` is dropped.
* Fix: `which.outcome` now selects the column you asked for on a `randomForest`
  classification fit. `which.outcome = 0` documented itself as overall
  importance and took column 1 to get it, and `which.outcome = k` took column
  `k + 1` for class `k`. Both are right for an `rfsrc` fit, whose `$importance`
  leads with an `all` column, and neither is right here: a `randomForest`
  matrix opens on the classes and keeps the overall permutation measure in
  `MeanDecreaseAccuracy`, near the end. So `0` returned the first class
  labeled as overall -- on `randomForest(Species ~ ., iris, importance =
  TRUE)` it handed back setosa's values, ranking `Petal.Width` above
  `Petal.Length` where the overall measure has them the other way round -- and
  every class index was shifted by one, `1` giving versicolor. The columns are
  now resolved by name: `0` reaches `MeanDecreaseAccuracy`, `k` reaches class
  `k`, and `which.outcome = 1` agrees with `which.outcome = "setosa"`. Fits
  grown with `importance = FALSE` keep no `MeanDecreaseAccuracy` column and
  their single measure answers to `0` as before.
* `which.outcome` now names the measure it selected in the `set` column, for
  both `rfsrc` and `randomForest` fits. Asking for one measure reported `set`
  as the literal `"vimp"` -- the pivot takes `set` from the source column name,
  and the selected column was named after the `vimp` column it was about to be
  written into rather than after the measure it held. So the one path where you
  have to say which measure you want was the one path that would not tell you
  which measure you got. `gg_vimp(rfsrc_iris, which.outcome = 0)` now reports
  `set == "all"`, `gg_vimp(rf_iris, which.outcome = 0)` reports
  `set == "MeanDecreaseAccuracy"`, and both agree with the names the unfiltered
  pivot has always used. Values and ordering are unchanged, and plots are
  unaffected: `plot.gg_vimp()` only facets on `set` when there is more than one
  of them, and selecting a measure leaves exactly one.
* `nvar` counts variables again for `randomForest` fits, not rows. It was
  applied after the multiclass pivot, where a frame holds one row per
  variable *per measure*, so it lopped whole measures off the end of the
  ranking instead of trimming the ranking itself.
* `gg_vimp()` now says in `?gg_vimp` that a `randomForest` fit without
  `importance = TRUE` stores only `IncNodePurity`, so the ranking is node
  purity rather than permutation VIMP, and nothing in the plot marks the
  difference. The example now passes `importance = TRUE`.
* `gg_error()` now explains that the error trajectory is `randomForestSRC`'s to
  record, not ours: `rfsrc()`'s `block.size` defaults to `NULL` unless you
  request importance, which stores the error at the final tree only, so a
  default fit gives `gg_error()` a single point rather than a curve --
  `tree.err = TRUE` alone does not change that. Grow with `block.size = 1` for
  an error at every tree. The examples do this now; they had all been plotting
  one dot.
* `gg_beta_varpro()`: the `imp` column is documented as the *absolute*
  coefficient. `varPro::beta.varpro()` wraps every coefficient it returns in
  `abs()`, so the sign is discarded upstream and never reaches us -- the docs
  had said "Sign is real (direction of local association)", which cannot be
  read off this output. Use `gg_ivarpro()` for a signed local estimator.
* `gg_isopro()`: the "What's in the output" section now says the polarity flip
  is ours. `varPro::isopro()`'s `howbad` is *lower* = more anomalous; we return
  `1 - howbad` so that higher = more anomalous. The section had credited that
  to the fit, contradicting this function's own `@return`.
* Added `gg_shap()` and `plot.gg_shap()` (with `shap_importance()`,
  `shap_beeswarm()`, `shap_dependence()`) for SHAP explanations of
  regression and classification forests, wrapping `kernelshap` (Suggests).
* `gg_shap()` now enforces the integer contract on `bg_n` and `which.class`
  instead of silently coercing them. Both are documented as integers, but were
  only loosely checked: `bg_n = 1.9` was truncated to 1 and `bg_n = Inf` (or any
  value above `.Machine$integer.max`) became `NA`, while `which.class = 2.9`
  passed the range check and then indexed column 2 -- returning SHAP values for
  a class the caller never asked for. Non-whole, non-finite, out-of-range and
  non-scalar values now raise a clear error. Valid input is unaffected.
* Added `print.gg_shap()` and `summary.gg_shap()`. `gg_shap` was the only
  `gg_*` class without them, so it dumped every row at the REPL instead of
  showing a header. `print()` now gives the standard one-line header (with the
  variable and observation counts) and `summary()` returns a `summary.gg`
  object reporting the baseline, background-sample size, the explained class
  for classification fits, and the top variables by mean |SHAP|.
* The package help page (`?ggRandomForests`) now describes the whole current
  surface -- the SHAP, Brier, varPro and unsupervised-varPro families were
  missing -- and no longer claims that `plot()` methods may return a *list* of
  `ggplot2` objects; each returns a single plottable object (a `ggplot`, or a
  `patchwork` composite for the multi-panel methods).
* `gg_partial()` no longer lets survival partial dependence be mistaken for a
  probability. `randomForestSRC::plot.variable()` defaults to
  `surv.type = "mort"`, so `yhat` is *mortality* -- an expected event count,
  not a value on [0, 1] -- and it only superficially resembles a percentage.
  `yhat` is passed through unscaled (rescaling it would corrupt the quantity);
  instead the label describing what was plotted is carried on the object as
  `attr(x, "ylabel")` and used as the y-axis title by `plot.gg_partial()`.
  Note that `gg_partial_rfsrc()` defaults to `partial.type = "surv"` and so
  does report survival probabilities: the two entry points report different
  quantities by default. (#15)

ggRandomForests v3.4.1
======================
* The remaining `rfsrc`/`randomForest` wrappers -- `gg_error()`, `gg_vimp()`,
  `gg_variable()`, `gg_rfsrc()`, and `gg_brier()` -- now have `default` S3
  methods, so a wrong-class input gives a clear "expected an 'rfsrc' or
  'randomForest' object" error (naming the class it got) instead of R's generic
  "no applicable method". This finishes the dispatch-consistency pass started
  for the varPro family in 3.4.0. (`gg_roc()` keeps its existing
  `gg_roc.rfsrc` default, which accepts rfsrc-shaped objects.)

ggRandomForests v3.4.0
======================
* `gg_isopro()`, `gg_beta_varpro()`, and `gg_ivarpro()` now have `default` S3
  methods, so a wrong-class input gives a clear "expected a '<class>' object"
  error (naming the class it got) instead of R's generic "no applicable
  method". This makes the varPro-family wrappers consistent with
  `gg_beta_uvarpro()` / `gg_sdependent()`; the previously-unreachable inner
  class checks were removed.
* Fix: `gg_partial_rfsrc()` now computes partial dependence correctly for
  `factor` predictors. It was passing factor *labels* as
  `partial.values` to `randomForestSRC::partial.rfsrc()`, which imposes a
  level by its integer code (internally `as.numeric(partial.values)`).
  Character labels ("No"/"Yes") became `NA` and numeric-looking labels
  ("4"/"6"/"8") became out-of-range codes, so every level collapsed to a
  single value (a flat categorical partial plot). The wrapper now passes the
  integer codes and relabels the output, matching `plot.variable(partial =
  TRUE)` and the ground-truth partial dependence. The categorical `x` is now
  returned as a `factor` in the model's level order, so the plot keeps that
  order instead of re-sorting alphabetically. Continuous and numeric
  low-cardinality predictors are unaffected.
* `gg_beta_uvarpro()` / `plot.gg_beta_uvarpro()`: tidy wrapper and bar chart
  for `varPro::get.beta.entropy()` -- the unsupervised analogue of
  `gg_beta_varpro()`. From a `uvarpro()` fit it aggregates the per-region
  lasso coefficients into `beta_mean = colMeans(|beta|)` per variable
  (most-important first), flags variables above a selection cutoff, and
  accepts a precomputed `beta_fit` matrix. `print`/`summary`/`autoplot`
  companions follow the `gg_*` conventions.
* `gg_sdependent()` / `plot.gg_sdependent()`: tidy wrapper and ranked
  lollipop for `varPro::sdependent()` signal-variable detection. Returns one
  row per candidate variable (`imp_score`, graph `degree`, `signal` flag)
  ranked by `imp_score`. Complements `gg_udependent()` (the dependency
  graph) with the "which variables are signal" ranking; shares the
  `beta_fit` entropy matrix. Follows the `get.beta.entropy` + `sdependent`
  workflow from the `varPro::uvarpro()` help (iowa-housing example).
* New `uvarpro` vignette: a short, focused walk-through of the unsupervised
  varPro wrappers (`gg_udependent()`, `gg_beta_uvarpro()`, `gg_sdependent()`)
  on a single `uvarpro()` fit, using the shared `beta_fit` matrix. The three
  unsupervised sections were lifted out of the `varpro` vignette, which now
  points to the new one and covers the five supervised wrappers.
* Fixed the main vignette's `\VignetteIndexEntry`, which still carried the
  template placeholder "Vignette's Title" -- it now reads "Exploring Random
  Forests with ggRandomForests" (the index entry CRAN lists, not the document
  title, was the stale one).

ggRandomForests v3.3.0
======================
* `gg_partial_varpro()`: **classification partial plots now default to
  probability.** `scale = "auto"` on a classification fit resolves to `"prob"`
  (P(Y = target class)) instead of raw log-odds; `"odds"` and `"logodds"` are
  options. The back-transform is applied before averaging (mean predicted
  probability). The `causal` contrast is shown only on `"logodds"`.
* `gg_partial_varpro()`: **survival partial plots now default to survival
  probability.** `scale = "auto"` on a survival fit resolves to `"surv"`
  (S(tau | x), bounded 0-1) via a new partialpro learner, instead of the
  unbounded ensemble-mortality score (still available via
  `scale = "mortality"`). `"surv"` and `"rmst"` default `tau` to the median
  follow-up time when `time` is omitted -- a units-safe, data-driven horizon
  (v3.2.0's `rmst` required `time`; this is a loosening). The resolved `tau` is
  reported in a message and the axis label.
* `plot.gg_partial_varpro()`: documents what the `causal` (virtual-twins)
  estimator is and when to use it, and explains why it is hidden on the bounded
  probability scales.
* Documentation: `plot.gg_partial_varpro()` gains a "Reading an RMST curve"
  section explaining how to interpret the `scale = "rmst"` y-axis -- RMST(tau)
  is the expected event-free time within the first tau time-units (area under
  S(t) out to tau), read in the model's own time units, bounded by tau, and
  higher-is-better (the opposite direction from ensemble mortality). It also
  notes that tau must be supplied in the fit's time units, since a tau beyond
  the largest event time truncates to the full restricted mean. No code change.

ggRandomForests v3.2.0
======================
* Fix (#118): `gg_varpro()` no longer fails with the cryptic
  "arguments imply differing number of rows: <p>, 0" when
  `varPro::importance()` returns a degenerate importance table (0 rows, or
  `p` variables with no usable `z` column) -- observed intermittently on
  survival fits where the release-rule step selects no variables. It now
  stops with a clear, specific message explaining the empty importance and
  suggesting a larger `ntree`. The guard is scoped to the degenerate case
  only; well-formed fits (survival included) are unaffected -- this is not
  a blanket survival-family block (cf. the reverted #116).
* Fix: `gg_partial_varpro(scale = "rmst", time = tau)` now *drives* the
  survival partial computation instead of only relabeling the y-axis.
  `varPro::partialpro()` has no time argument, so its default survival
  learner returns ensemble mortality at every horizon -- multi-horizon RMST
  plots built that way differed only by Monte-Carlo noise, not by `tau`.
  `scale = "rmst"` now passes `partialpro()` an RMST(`tau`) learner that
  integrates the survival curve (`integral_0^tau S(t) dt`) from `object$rf`,
  so the curve genuinely depends on `tau`. This path recomputes from
  `object` (a survival fit) with `part_dta = NULL`; a precomputed
  `part_dta` can only be relabeled, and the function now warns when you try.
  Also warns when `tau` exceeds the model's event-time range (RMST is
  truncated there) and when `time` is passed to a scale that ignores it.
  `Imports` now requires `varPro (>= 3.1.0)` (the version exposing the
  `partialpro()` `learner` argument this path relies on).
* Fix: `gg_partial_varpro(scale = "surv"/"chf", model = ...)` no longer
  errors when a variable yields an empty continuous or categorical frame
  (the survival path-C `model`-label assignment now guards against a 0-row
  data.frame).
* `gg_partial_varpro()` (and the `gg_partialpro()` alias) now forward `...`
  to `varPro::partialpro()` on the object-driven path. This restores control
  over which variables are computed (`xvar.names`, `nvar`) and the UVT step
  (`cut`, `nsmp`, ...) for the RMST path, which must recompute from `object`
  and so cannot accept a precomputed `part_dta`. Without an explicit
  `xvar.names`, `partialpro()` falls back to `varPro::get.topvars(object)`,
  which can return few or no variables for some fits.

ggRandomForests v3.1.2
======================
* CRAN fix: skip only the single test grow that trips the upstream
  `randomForestSRC` gcc-UBSAN report at `entry.c:184` — the *unsupervised*
  isolation forest in `gg_isopro` (`varPro::isopro(method = "unsupv")`). Only
  an unsupervised grow has a 0-length `yvar.wt`, the vector `rfsrcGrow`
  decrements to an out-of-bounds pointer; supervised grows are unaffected.
  We verified this under `-fsanitize=undefined`: of every varPro/rfsrc grow
  in the test suite, only `isopro(method = "unsupv")` fires `entry.c:184`.
  `make_iso_fit()` therefore calls `skip_on_cran()` only for
  `method = "unsupv"`. ggRandomForests is pure R and unchanged.
* The broader `skip_on_cran()` guards added in v3.1.1 (the `varpro`,
  `uvarpro`, `ivarpro`, `beta.varpro`, and `isopro(method = "rnd")` test
  fixtures) are removed: those grows are supervised (or synthetic-supervised)
  and gcc-UBSAN-clean, so they run on CRAN again, restoring that test
  coverage. The upstream issue is fixed in `randomForestSRC` and pending a
  CRAN release.

ggRandomForests v3.1.1
======================
* CRAN fix: the varPro tests now call `skip_on_cran()` so they do not run
  on CRAN's check machines, including the gcc-UBSAN additional check. They
  were triggering an upstream `randomForestSRC` sanitizer issue (a 0-length
  array access in `rfsrcGrow`, `entry.c:184`) that surfaces when any
  `varPro` grow (`varpro()`, `beta.varpro()`, `uvarpro()`, `isopro()`,
  `ivarpro()`) builds a forest. ggRandomForests is pure R and its code is
  unchanged; the varPro tests still run in our CI (the workflows set
  `NOT_CRAN=true`) and locally; they are skipped only on CRAN's check
  machines, including the gcc-UBSAN check. The upstream issue has been
  reported to the randomForestSRC maintainers.
* The `varpro` vignette now loads every varPro fit from a precomputed
  file (`vignettes/varpro_precomputed.rds`, built by
  `vignettes/precompute_varpro.R`), so the vignette performs no live
  varPro grow during `R CMD check`. This removes the same upstream
  sanitizer path from the vignette build and trims check time. Each chunk
  falls back to a live fit if the precomputed object is absent, so the
  vignette remains reproducible from source.

ggRandomForests v3.1.0
======================
* Fix: `gg_vimp()` for single-outcome rfsrc forests now correctly flags
  variables with non-positive VIMP in the `positive` column (affecting
  plot coloring). The column was named `VIMP` (uppercase) in single-outcome
  fits but the flag check accessed `$vimp` (lowercase), leaving `positive`
  stuck at `TRUE` for all variables. Surfaced by the Copilot review on
  PR #109.
* Documentation pass. Deepened the varPro-family and rfsrc
  importance/partial/survival help pages against the upstream
  randomForestSRC and varPro documentation, and made the line between
  `gg_vimp()` (permutation, Breiman-Cutler importance) and `gg_varpro()`
  (varPro release-rule importance) explicit and cross-linked. Vignette
  prose deepened with the same framing; one-line code-comment fixes;
  fixed a stale `@return` in `gg_roc()` (documented a `yvar` column the
  function does not return). No user-facing behavior change.
* Vignettes: the regression and survival partial-dependence surfaces are
  now rendered as static `ggplot2` heat maps instead of interactive
  `plotly` widgets, and figures render at 96 dpi. This cuts the installed
  size from ~17 MB to ~5 MB (the `plotly` library is no longer bundled into
  the vignette HTML). `plotly` is dropped from `Suggests`.
* Check time: reduced the `R CMD check` vignette-rebuild and test timings to
  bring the overall CRAN check comfortably under budget (CRAN flagged the
  overall check time on the 3.1.0 submission). The regression and survival
  vignettes use lighter forests (`ntree` 200 / 150, imputation `ntree` 100)
  and coarser partial-dependence grids. The varpro vignette's three
  `gg_partial_varpro()` calls and the Boston `beta.varpro()` fit (~34 s
  combined) are precomputed offline by `vignettes/precompute_varpro.R` and
  loaded from `vignettes/varpro_precomputed.rds`, with an automatic
  live-computation fallback if the file is absent. The `gg_udependent()`
  tests memoise the per-fit entropy matrix (`varPro::get.beta.entropy()`,
  ~1.5 s and a pure function of the fit) instead of recomputing it once per
  test. No user-facing behavior change.

ggRandomForests v3.0.0
======================
* **Version jump to 3.0.0.** The varPro integration is a major scope
  expansion plus the `gg_partialpro()` soft-deprecation, which is
  major-version territory. Survival / multivariate varPro families,
  ROC confidence intervals, and hazard estimates are deferred to
  v3.1.0.
* CRAN-audit cleanup: the `gg_brier()` / `plot.gg_brier()` examples move
  from `\dontrun` to `\donttest` (so they execute under `R CMD check --as-cran` and on
  CRAN; `library(survival)` added so `Surv()` resolves), the
  per-variable `message()` in the deprecated `surv_partial.rfsrc()` is
  removed (its one behavior change: that function no longer prints a
  line per variable), and the README points to the new "varpro"
  vignette.
* Fix: importance plots now consistently put the most-important variable
  at the **top**. `gg_varpro()`, `gg_beta_varpro()`, and `gg_ivarpro()`
  previously built their `variable` factor with descending levels, so
  after `coord_flip()` the most-important variable landed at the bottom
  — inverted relative to `gg_vimp()`. All three now reverse the factor
  levels to match the `gg_vimp` convention (and the `varImpPlot` / `vip`
  standard). Row order and `summary()` output are unchanged (still
  most-important first). A new cross-function test pins the convention.
* New vignette: "Exploring variable importance with varPro." Walks the
  full gg_* varPro layer (gg_partial_varpro, gg_varpro, gg_udependent,
  gg_isopro, gg_beta_varpro, gg_ivarpro) on three worked examples —
  regression (Boston), classification (iris binary + multi-class), and
  survival (PBC). Includes a family-support matrix documenting which
  wrapper works for which forest family. Headline document for v3.0.0.
* `gg_ivarpro()` and `plot.gg_ivarpro()`: tidy wrapper and
  per-variable-distribution / per-observation-profile plots for
  `varPro::ivarpro()` (individual / local variable importance) across
  regression and classification (binary + multi-class) families. The
  long-format tidy frame is `(obs, variable, local_imp, selected)` for
  regression; classification adds a `class` column. NA cells are
  filtered out and sparsity is surfaced in provenance. `which_obs`
  (integer index) collapses to a single-observation profile; the plot
  switches from a jittered distribution view to a horizontal bar
  chart. `which_class` (response level name) collapses to a single
  class panel; binary fits default to the last factor level (positive
  class). `cutoff` accepts `NULL` (per-class mean), a scalar, or a
  named numeric vector — matching the gg_beta_varpro classification
  contract. Optional `ivarpro_fit` argument lets callers cache the
  expensive `ivarpro()` call. Last of four Phase 4 sub-projects.
* `gg_beta_varpro()` adds varPro classification support (binary +
  multi-class). Binary fits default to a single positive-class panel
  (last factor level); multi-class fits return a long-format frame
  with a `class` column and plot as `facet_wrap(~ class)`. Optional
  `which_class` selects a single class; `cutoff` accepts a scalar or
  per-class named vector. Variables are stored as a factor whose
  levels are set by `mean(|sum-of-class-beta|)` descending so every
  facet shows rows in the same order. Motivating use case: 30-day
  mortality.
* Provenance shape change for `gg_beta_varpro()`:
  `attr(*, "provenance")$cutoff` is now always a named numeric
  vector — length 1 named `"regr"` for regression, length K named
  with the response factor levels for classification. Downstream
  tooling should read it as a vector and select by name; the prior
  scalar shape is gone.
* `gg_beta_varpro()` and `plot.gg_beta_varpro()`: tidy wrapper and default
  horizontal bar chart for `varPro::beta.varpro()` — the per-rule lasso-β
  refinement of variable importance. Aggregates per-rule β̂ by variable
  into `beta_mean = mean(|β̂|)` and flags variables above a selection
  cutoff (default `mean(beta_mean)`). Optional `beta_fit` argument lets
  callers compute the expensive `beta.varpro()` step once and reuse the
  result across multiple wrapper calls (different cutoffs, snapshot
  rebuilds, vignette knits). `print` / `summary` / `autoplot` S3
  companions follow the existing `gg_*` conventions. **Regression family
  only** — classification, regr+, and survival are tracked under Phase 4d
  (see the spec for the endpoint map). Third of three Phase 4 sub-projects.
* `gg_isopro()` gains a `newdata` argument so a fitted `varPro::isopro`
  model can score new observations into the same tidy `gg_isopro` frame.
  Internally the wrapper calls `predict.isopro()` twice: with
  `quantiles = FALSE` to populate the `case.depth` column (varPro's native
  polarity, lower = more anomalous) and with `quantiles = TRUE` to compute
  `howbad = 1 - quantile` (the wrapper convention, higher = more anomalous).
  Both polarities are visible in the returned data frame, and the
  relationship is named in the roxygen. The `plot` / `print` / `summary` /
  `autoplot` S3 companions work unchanged on the new tidy frame; to overlay
  training and test scores, bind the two extractor calls with a `method`
  label column and pass the result to `plot()`. Second of three Phase 4
  sub-projects.
* **Fix (gg_isopro training-path polarity).** Bug in the original
  `gg_isopro` (PR #94): varPro's `$howbad` on an `isopro` fit uses
  "lower = more anomalous" polarity (it is the quantile of `case.depth`),
  but the wrapper's plot method and documentation both assume "higher =
  more anomalous". Train scores and the new test-data scores were
  anti-correlated until this PR's training-path flip
  (`howbad = 1 - object$howbad`) brought them into agreement. The fix
  surfaced because the test-data sanity check (training-as-newdata top-5
  overlap) failed at 0/5 instead of 5/5 before the flip. Note: the two
  vdiffr baselines recorded in PR #94 (`gg-isopro-default` and
  `gg-isopro-threshold`) were recorded under the inverted polarity; they
  are visually flipped relative to the new behavior but CI skips
  snapshots (`VDIFFR_RUN_TESTS = false`) so no failure surfaces. Re-record
  with `VDIFFR_RUN_TESTS = true` when convenient.
* Documentation: pedagogical pass over the varPro wrappers
  (`gg_partial_varpro`, `gg_varpro`, `gg_udependent` and their `plot.*`
  methods). Each help page now has explicit "What X is doing", "What's
  in the output", and "What you use this for" sections so a reader new
  to varPro can learn the underlying method (release rules, beta-entropy
  dependency, parametric / nonparametric / causal partial estimators)
  from the help page alone, not just the wrapper mechanics. No API or
  behavioral change.
* Documentation: enable roxygen2 markdown package-wide via
  `Roxygen: list(markdown = TRUE)` in `DESCRIPTION`. New roxygen blocks
  can use backticks and `[fn()]` link syntax; existing `\code{}` /
  `\link{}` markup keeps working. Two source-roxygen edits to keep
  R CMD check clean: `randomForest[SRC]` in `R/help.R` (markdown read
  it as an unfinished link) becomes plain `randomForestSRC`; the `95\%`
  escape in `R/gg_rfsrc.R::bootstrap_survival` becomes a literal `95%`.
  No API or rendered-doc behavioral change beyond the conventions
  switch.
* New `gg_isopro()` and `plot.gg_isopro()`: tidy wrapper and ranked-elbow +
  density visualization for `varPro::isopro` isolation-forest anomaly
  scores. `plot.gg_isopro()` takes `panel = c("both", "elbow", "density")`
  and optional `threshold` (score-space) or `top_n_pct` (quantile-space)
  to draw a reference line; if both are set, `threshold` wins with a
  message. A `method` column auto-triggers color grouping for multi-method
  comparisons (use `dplyr::bind_rows()` on three `gg_isopro()` calls).
  `print` / `summary` / `autoplot` S3 companions follow the existing `gg_*`
  conventions. First of three Phase 4 sub-projects.
* `plot.gg_variable()`: fix render error on the default multi-class
  classification plot. The default-xvar selection was treating `yvar` (the
  observed-class column) and `outcome` (the multi-class pivot facet) as
  predictors; pivoting them into `var` then dropped the column the
  downstream `geom_jitter(aes(color = yvar))` referenced, and the patchwork
  errored when actually rendered. CI did not catch this because the existing
  test only asserted the patchwork class (lazy) and snapshots run with
  `VDIFFR_RUN_TESTS = false`. New test exercises a real build of every
  sub-plot.
* `plot.gg_variable()`: the same default-xvar selection used substring
  `grep("time", ...)` / `grep("event", ...)`, which silently dropped any
  predictor whose name contained those substrings -- e.g. the documented
  veteran-data survival predictor `diagtime`. Switch to exact matching for
  `event` / `time` / `yvar` / `outcome` and an anchored prefix for `yhat`
  (`yhat` or `yhat.<class>`). New test exercises `diagtime` on the veteran
  survival forest.
* `gg_roc()`: per-class one-vs-rest ROC curves (#88, closes #72).
  - New `per_class` argument, default `FALSE`. With `per_class = TRUE` on a
    forest of more than two classes, `gg_roc()` returns a long-format
    `gg_roc` data frame with a `class` factor column, plus a named AUC
    vector attribute with one entry per class, ordered by descending AUC.
  - `plot.gg_roc()` gains `panel = c("overlay", "facet")`. When the object
    has a `class` column, `"overlay"` colors the curves by class and
    `"facet"` gives each class its own panel.
  - `summary.gg_roc()` prints the named per-class AUC values when a `class`
    column is present.
  - On a binary forest, `per_class = TRUE` does nothing, the usual
    single-curve result comes back unchanged.
  - ROC confidence intervals are still to come, in v3.1.0 (issue #7 / #72-CIs).
* New `gg_udependent()`: varPro cross-variable dependency (Phase 3).
  - `gg_udependent()` reads cross-variable dependency scores off a `uvarpro`
    fit, via `varPro::get.beta.entropy()` and `varPro::sdependent()`. It
    returns a tidy list: `$edges` (variable_from, variable_to, weight),
    `$nodes` (variable, degree, selected), and `$graph`, an igraph object.
  - `plot.gg_udependent()` draws the dependency network with ggraph. Edge
    width and opacity scale with dependency strength; node color marks the
    signal variables. The layout is configurable (`"fr"`, `"kk"`,
    `"stress"`, and so on).
  - `ggraph` added to `Suggests:`.
* New `gg_varpro()`: varPro variable importance (#85).
  - `gg_varpro()` pulls per-tree importance scores from a fitted `varpro`
    object and draws a boxplot of the per-tree z-score distribution for each
    variable. The hinges sit at the 15th and 85th percentiles and the
    whiskers at the 5th and 95th, so the box is not the usual Tukey one —
    it reports the percentiles it actually shows. Variables with aggregate
    z above `cutoff` (default 0.79) are color-highlighted.
  - With `faithful = TRUE`, the individual per-tree z-scores are jittered
    over the box as semi-transparent points, with a white-outlined dot at
    the mean, the same view as varPro's internal `bxp` output.
  - With `conditional = TRUE` (classification forests only), `gg_varpro()`
    reads `$conditional.z` and draws class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - Set `local.std = FALSE` to allow `plot(..., type = "raw")`, which shows
    raw per-tree importance instead of the z-normalized values.
* `gg_variable.randomForest`: classification fix (#87).
  - For a classification forest, `gg_variable.randomForest()` now stores
    per-class OOB vote fractions as `yhat.<classname>` columns, read from
    `object$votes`, the same layout the `rfsrc` path produces. It used to
    store a single `yhat` factor column of class labels (from
    `object$predicted`), and that column shape stopped the multi-class
    pivot in `plot.gg_variable` from ever running. The vote fractions are
    row-normalized to `[0, 1]`, even when the forest was fit with
    `norm.votes = FALSE`.
  - `plot.gg_variable`, binary classification: with `smooth = TRUE` the
    x and y aesthetics are now mapped onto the smooth layer correctly.
  - `plot.gg_variable`, multi-class numeric path: `smooth = TRUE` now adds
    the smooth layer instead of skipping it silently.
  - Closes stale issues #81 (fixed in PR #83) and #82.
* New `gg_partial_varpro()`: varPro partial dependence (#84).
  - `gg_partial_varpro()` takes over from `gg_partialpro()` as the entry
    point for varPro partial dependence plots. It accepts an optional
    `object` argument (the originating `varpro` fit) which it uses for
    provenance-aware axis labels, and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - Ensemble mortality labeling (Ishwaran et al. 2008): with
    `scale = "mortality"`, or `scale = "auto"` on a survival forest, the
    y-axis reads "Ensemble mortality (expected events)". That is an
    unbounded relative-risk score, not a survival probability, and the
    documentation says so plainly so it is not misread.
  - Survival path C: with `scale = "surv"` or `scale = "chf"`,
    `gg_partial_varpro()` pulls the embedded rfsrc forest from `object$rf`
    and returns true S(t) or CHF partial curves through the existing
    `gg_partial_rfsrc` machinery.
  - `varPro` is now a hard dependency (`Imports:`).
  - `gg_partialpro()` is soft-deprecated: it warns, then hands off to
    `gg_partial_varpro()`. It will be removed in the release after v3.0.0.
* randomForest engine validation and repair (#82). Fixes #80, #81, and a
  `plot.gg_error` label wart, and adds full randomForest regression test
  coverage. Details below.
  - `plot.gg_variable()` now always returns a single `ggplot` (one
    variable) or a `patchwork` composite (several variables, or the
    default) — never a bare list. This matches the v2.7.3
    `plot.gg_partial*` change. A list used to come back for multiple
    `xvar`, which broke `patchwork` / `autoplot()` / `layer_data()`
    composition (#80).
  - `gg_roc()` and `calc_roc()` for `randomForest` now build the ROC from
    class probabilities (OOB votes by default, honoring `oob`) rather
    than the degenerate three-point curve they produced before. With
    `which_outcome = "all"` (the default for `gg_roc(rf)`) the result is a
    macro-averaged one-vs-rest ROC, and no warning. The shared
    `.validate_which_outcome` helper and `calc_roc.rfsrc` are
    byte-for-byte unchanged, so rfsrc behavior is untouched (#81).
* Dependency modernization. This breaks scripts that relied on attachment.
  `randomForestSRC` and `randomForest` move from `Depends:` to `Imports:`;
  `igraph`, `callr`, and `varPro` are added to `Suggests:` (`varPro` later
  moves up to `Imports:`, with the first varPro-integration component).
  `library(ggRandomForests)` no longer puts `randomForestSRC` or
  `randomForest` on the search path. A script that called `rfsrc()` or
  `randomForest()` unqualified after only `library(ggRandomForests)` now
  needs its own `library(randomForestSRC)` / `library(randomForest)`, or
  must qualify the calls. ggRandomForests itself is unaffected. It
  qualifies every call into its dependencies.

ggRandomForests v2.7.3
======================
* `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and `plot.gg_partialpro()`
  now always return a single `ggplot`/`patchwork` object. Previously, when
  both continuous and categorical predictors were present, they returned a
  named list `list(continuous=, categorical=)`, which surprised users and
  made `autoplot()` dispatch ambiguous. The two panels are now combined
  vertically via `patchwork::wrap_plots()` (patchwork moved from `Suggests`
  to `Imports`). Closes #77.
* `autoplot()` S3 methods for all 10 `gg_*` classes, delegating to the
  corresponding `plot.gg_*()` method so objects work in `|>` pipelines,
  `patchwork`, and `cowplot` compositions via `ggplot2::autoplot()`.
* `print()` and `summary()` S3 methods for every `gg_*` data object
  (gg_error, gg_vimp, gg_rfsrc, gg_variable, gg_partial,
  gg_partial_rfsrc, gg_partialpro, gg_roc, gg_survival, gg_brier).
  `print()` is header-only — use `head()` for rows. `summary()`
  returns a printable `summary.gg` object with per-class diagnostics.
  Each `gg_*` constructor now attaches a `"provenance"` attribute
  (source, family, ntree, n, xvar.names) consumed by the new methods.
* New `gg_brier()` extractor and `plot.gg_brier()` method for time-resolved
  Brier scores and CRPS on survival forests (issue #9). Wraps
  `randomForestSRC::get.brier.survival()` and adds the mortality-quartile
  decomposition, a 15-85 percent per-subject envelope, and running CRPS
  via trapezoidal integration. Supports `cens.model = c("km", "rfsrc")`,
  `type = c("brier", "crps")`, and `envelope` (overall line + 15-85%
  ribbon). Multi-model comparison is left to `dplyr::bind_rows()` on
  multiple `gg_brier` outputs — see `?gg_brier` for an example.
* Visual unification of ribbon overlays across plot methods. All
  ribbons now use a shared alpha (`.gg_ribbon_alpha = 0.2`) and a
  shared fill (`.gg_ribbon_fill = "steelblue"`) for single-series
  cases (KM/NA CIs, bootstrap CIs, `gg_brier` envelope); group-stratified
  ribbons keep their group-colored fill. Statistical bounds unchanged —
  only styling.
ggRandomForests v2.7.2
=====================
* Address CRAN reviewer (Benjamin Altmann) feedback on the v2.7.1
  resubmission:
  - Add methods references to `DESCRIPTION` (Breiman 2001 and
    Ishwaran et al. 2008, with `<doi:...>` auto-links) per CRAN
    cookbook.
  - Drop the `man/shift.Rd` Rd file: `shift()` is an internal utility
    and the example used `ggRandomForests:::shift(...)`. Marked the
    function `@noRd` so it no longer generates a help page.
  - Replace `cat()` in `surv_partial.rfsrc()` with `message()` so
    progress output is suppressible (`suppressMessages()`) and plays
    nicely inside notebooks / Shiny / quarto.
  - Restore the user's `par()` settings in the
    `surv_partial.rfsrc()` example via
    `oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))`.

ggRandomForests v2.7.1
=====================
* Fix `gg_partial_rfsrc()` for survival forests: `partial.rfsrc()` was being
  called without `partial.type`, causing a zero-length comparison
  (`if (partial.type == "rel.freq") ...`) inside the C-level prediction
  routine and aborting the call. Survival forests now pass
  `partial.type = "surv"` (default; configurable via the new `partial.type`
  argument accepting `"surv"`, `"chf"`, or `"mort"`). This unblocks the
  `partial-dep` chunk in the survival vignette.
* Fix `gg_partial_rfsrc()` for survival forests with multiple
  `partial.time` values: `get.partial.plot.data()` returns yhat as an
  `[length(partial.values) x length(partial.time)]` matrix, but the previous
  code assumed a vector and crashed on column-mismatch when assigning
  `time`. The result is now reshaped to long form so each `(x, time)` pair
  is a single row.
* Improve `plot.gg_partial_rfsrc()` survival layout: predictor value is now
  on the x-axis with one curve per (rounded) time point colored by `Time`,
  faceted by variable name. The previous default put time on the x-axis
  and one curve per predictor value, producing a saturated legend with
  dozens of nearly-identical lines.
* Add `tests/testthat/test_plot_layer_data.R`: regression suite that uses
  `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders
  non-empty layers for every supported forest family. Catches the
  empty-figure class of bug (transform/plot column-name mismatch) without
  requiring visual inspection.
* `ggrandomforests.news()` now reads `NEWS.md` (the canonical change log
  R also surfaces via `utils::news()`). The legacy hand-maintained
  `inst/NEWS` has been removed — it had silently drifted to v2.4.0
  (June 2025) across three releases, so users running the helper saw
  stale version info. One source of truth, no more drift window.
* Fix `plot.gg_vimp()` legend duplication: the bar geom mapped both
  `fill` and `color` to the `positive` column, but only the fill legend
  was titled "VIMP > 0", leaving a redundant second legend titled
  "positive". Both aesthetics now share the "VIMP > 0" title so ggplot
  merges them into a single legend by default.
* Fix `plot.gg_vimp()` for forests with all-positive VIMP: the bar geom
  previously mapped only `color` (no `fill`), producing hollow / outline-
  only bars and an "Ignoring unknown labels: fill" warning whenever
  `labs(fill = ...)` was applied. Both `fill` and `color` are now mapped
  unconditionally, so bars render filled in every case.
* Add `@examples` blocks to `plot.gg_partial_rfsrc()` and
  `plot.gg_partialpro()`. The latter uses a self-contained mock of the
  `varpro::partialpro()` output structure so the example runs without
  pulling in `varpro` as a dependency.

ggRandomForests v2.7.0
=====================
* S3 design overhaul: `gg_partial()`, `gg_partialpro()`, and
  `gg_partial_rfsrc()` now stamp their return values with S3 classes
  (`gg_partial`, `gg_partialpro`, `gg_partial_rfsrc` respectively), enabling
  `plot()` dispatch without any boilerplate.
* Add `plot.gg_partial()`, `plot.gg_partial_rfsrc()`, and
  `plot.gg_partialpro()` S3 methods; continuous predictors render as line
  plots, categorical as bar charts, faceted by variable name.  Survival
  forests produce curves over time; two-variable surface plots group by
  `xvar2.name`.
* Convert `gg_survival()` to an S3 generic dispatching on the class of its
  first argument.  New `gg_survival.rfsrc()` method extracts the survival
  response directly from the fitted forest (no separate data argument
  needed); `gg_survival.default()` preserves the existing interface.
* Fix `plot.gg_survival()` auto-coercion: previously called
  `gg_survival(rfsrc_obj)` treating the forest as the `interval` string
  argument, causing a latent crash; replaced with `inherits()` guard.
* Deprecate `surv_partial.rfsrc()` via `.Deprecated()` with a pointer to
  `gg_partial_rfsrc()`; all package tests updated to suppress the warning.
* Fix `gg_partial_rfsrc()` — `make_eval_grid()` used `unlist(dplyr::select())`
  which coerced factor columns to integer codes; now uses `newx[[xname]]` to
  preserve column class.  Categorical detection extended to cover
  `is.factor()` and `is.character()` in addition to the cardinality check.
* Add guards to `gg_partial_rfsrc()`: all-NA `xval` after NA removal now
  emits a warning and skips the variable; all-NA grouping variable (`xvar2`)
  calls `stop()`; `n_eval` and `cat_limit` are validated as single integers
  >= 2 near function entry.
* Fix cyclomatic complexity across `gg_partial_rfsrc.R`: refactored into
  eight top-level unexported helpers (`validate_scalar_int`,
  `validate_partial_args`, `snap_partial_time`, `make_eval_grid`,
  `call_partial_rfsrc`, `partial_one_var`, `partial_no_group`,
  `partial_with_group`, `split_partial_result`); all functions now score
  below the `cyclocomp_linter` limit of 20.
* Fix `@param partial.time` documentation: "see the section above" corrected
  to "see the section below".
* Replace deprecated `tidyr::gather()` with `tidyr::pivot_longer()` in
  `plot.gg_vimp()` and `plot.gg_partialpro()`.
* Add `gg_survival.rfsrc`, `gg_survival.default`, `plot.gg_partial`,
  `plot.gg_partial_rfsrc`, and `plot.gg_partialpro` to `NAMESPACE`; add
  corresponding `@rdname` / `@export` roxygen tags.
* Update tests: add `expect_s3_class()` checks for all new classes; add
  `plot()` smoke tests for `gg_partial`, `gg_partial_rfsrc`, `gg_partialpro`;
  add `gg_survival.rfsrc` tests for KM extraction, `by` stratification, and
  error on non-survival forest.
* Add `plot.gg_partial`, `plot.gg_partial_rfsrc`, and `plot.gg_partialpro`
  to `_pkgdown.yml` reference index.

ggRandomForests v2.7.0
=====================
* Fix critical visual bug in `plot.gg_rfsrc`: all `aes()` calls used bare
  string literals instead of `.data[[col]]`, causing every aesthetic to map
  to a constant string rather than the underlying data column. All plot
  types (regression, classification, survival) were affected.
* Fix `aes()` bare-string literals in `plot.gg_roc` multi-class branch;
  remove unreachable `if (crv < 2)` dead-code branch.
* Fix `bootstrap_survival` CI-band indexing in `gg_rfsrc`: negative index
  computed via `colnames()` was a no-op on large datasets and a latent crash
  for data with ≤ 2 unique event times.
* Fix `gg_rfsrc.rfsrc`: `is.null(df[, col])` does not detect missing columns;
  replaced with `!col %in% colnames()` guard.
* Fix `gg_rfsrc.randomForest`: method used non-existent `object$xvar`; now
  recovers the training frame via `.rf_recover_model_frame()`.
* Fix legend suppression in `plot.gg_error` for single-outcome forests where
  the data frame has no `variable` column.
* Fix `gg_vimp` and `plot.gg_vimp`: `1:nvar` replaced with `seq_len(nvar)`
  in both S3 methods; `1:0` silently returned `c(1, 0)` instead of
  `integer(0)` when `nvar == 0`.
* Migrate full test suite to testthat 3.x API: `expect_is` →
  `expect_s3_class` / `expect_type` / `expect_true(is.*())`;
  `expect_equivalent` → `expect_equal(ignore_attr = TRUE)`; all `context()`
  calls removed; testthat 1.x `expect_that` / `is_identical_to` removed.
* Add `.lintr` package-level linter configuration; fix lintr spacing in
  `gg_partial`.
* Improve GitHub Actions: `lint.yaml` now fails CI on any lint issue;
  `R-CMD-check.yaml` treats warnings as errors and uses Rtools 44;
  `test-coverage.yaml` duplicate codecov upload removed.
* Add `covr` and `vdiffr` to `Suggests`.

ggRandomForests v2.6.1
=====================
* Fix model-label assignment in `gg_partial` for categorical variable data
* Refactor `gg_partial` and `gg_partial_rfsrc` to improve factor-level
  normalization and categorical data handling

ggRandomForests v2.6.0
=====================
* Add and export new plotting functions; update existing plot documentation
* Improve unit and integration tests; overall coverage raised to 83%
* Remove `hvtiRutilities` internal dependency; clean up associated imports
* Refactor `gg_partial_rfsrc` to use `.data` pronoun for all `dplyr` calls

ggRandomForests v2.5.0
=====================
* Initial `gg_partial_rfsrc` function: computes partial dependence data
  directly from an `rfsrc` model via `randomForestSRC::partial.rfsrc`, without
  requiring a separate `plot.variable` call
* Add support for a grouping variable (`xvar2.name`) in `gg_partial_rfsrc`
* Improved vignette formatting and namespace usage

ggRandomForests v2.4.0
=====================
* Updating to latest ggplot2 functions
* Utilize some namespace referencing
* Added pkgdown documentation
* Minor testing improvements

ggRandomForests v2.3.0
=====================
* Knocking the dust off this.
* Fix the ROC curves
* Fix the colors on VIMP plot

ggRandomForests v2.2.1
=====================
* Fix docs for HTML5/Roxygen update

ggRandomForests v2.2.0
=====================
* Bring back the regression vignette
* Improve package tests and code coverage
* Clean up code with lintr

ggRandomForests v2.1.0
=====================
To pull this out of archive on randomForestSRC 3.1 build release.
Fixed a plot bug for gg_error to show the actual curve (issue 35)

ggRandomForests v2.0.1
======================
* Correct a bug in survival plots when predicting on future data without a known outcome.
* All Vignettes are now at https://github.com/ehrlinger/ggRFVignette
* All tests are being moved to https://github.com/ehrlinger/ggRFVignette
* Begin work on rewriting all checks to not use cached data. 
  This will require more runtime, and hence we will run fewer of them on CRAN release. 
* Minor bug and documentation fixes.

ggRandomForests v2.0.0
======================
* Added initial support for the randomForest package
* Updated cache files for randomForestSRC 2.2.0 release.
* Remove regression vignettes to meet CRAN size limits. These remain available at the package source https://github.com/ehrlinger/ggRandomForests
* Minor bug and documentation fixes.

ggRandomForests v1.2.1
======================
* Update cached datasets for randomForestSRC 2.0.0 release. 
* Correct some vignette formatting errors (thanks Joe Smith)

ggRandomForests v1.2.0
======================
* Convert to semantic versioning http://semver.org/
* Updates for release of ggplot2 2.0.0
* Change from reshape2::melt dependence to tidyr::gather
* Optimize tests for CRAN to optimize R CMD CHECK times.


ggRandomForests v1.1.4
======================
* `combine.gg_partial` bug when giving a single variable plot.variable object.
* Remove `dplyr` depends to transitions from "Imports" to "Suggests".
* Argument for single outcome `gg_vimp` plot for classification forests.
* Improvements to `gg_vimp` arguments for consistency.
* Add bootstrap confidence intervals to `gg_rfsrc` function.
* Initial `partial.rfsrc` function to replace the `randomForestSRC::plot.variable` function.
* Move cache data to `randomForestSRC` v1.6.1 to take advantage of `rfsrc` version checking between function calls.

* Vignette updates for JSS submission of "ggRandomForests: Exploring Random Forest Survival".
* Vignette updates for arXiv submission of ggRandomForests: Random Forests for Regression

* Some optimizations to reduce package size.
* Remove all tests from CRAN build to optimize R CMD CHECK times.
* Remove pdf vignette figure from CRAN build.
* Return S3method calls to NAMESPACE for "S3 methods exported but not registered" for R V3.2+.
  
* Misc Bug Fixes.

ggRandomForests v1.1.3
======================
* Update "ggRandomForests: Visually Exploring a Random Forest for Regression" vignette.
* Further development of draft package vignette "Survival with Random Forests". 
* Rename vignettes to align with randomForestSRC package usage.
* Add more tests and example functions.
* Refactor `gg_` functions into S3 methods to allow future implementation for other random forest packages.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.

ggRandomForests v1.1.2
======================
* Add package vignette "ggRandomForests: Visually Exploring a Random Forest for Regression"
* Add gg_partial_coplot, quantile_cuts and surface_matrix functions
* export the calc_roc and calc_auc functions.
* replace tidyr function dependency with reshape2 (melt instead of gather) due to lazy eval issues.
* reduce dplyr dependencies (remove select and %>% usage for base equivalents, I still use tbl_df for printing)
* Further development of package vignette "Survival with Random Forests" 
* Refactor cached example datasets for better documentation, estimates and examples.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.


ggRandomForests v1.1.1
======================
Maintenance release, mostly to fix gg_survival and gg_partial plots.
* Fix the gg_survival functions to plot kaplan-meier estimates.
* Fix the gg_partial functions for categorical variables.
* Add some more S3 print functions.
* Try to make gg_functions more consistent.
* Further development of package vignette "Survival with Random Forests" 
* Modify the example cached datasets for better estimates and examples.
* Improve help files.
* Misc Bug Fixes.


ggRandomForests v1.1.0
======================
* Add panel option for gg_variable and gg_partial
* Rework interactions plot
* add gg_coplot functions
* Imports instead of depends
* Add version dependencies for randomForestSRC
* Include package vignette "Random Forests for Survival" 
* Misc Bug Fixes

ggRandomForests v1.0.0
======================
* First CRAN release.

ggRandomForests v0.2
======================
* Initial useR!2014 release. 
