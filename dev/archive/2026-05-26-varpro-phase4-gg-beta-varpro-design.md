# ggRandomForests v2.8.0 — varPro Phase 4c: `gg_beta_varpro` Design

**Date:** 2026-05-26
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** Third of the Phase 4 sub-projects. Follows PR #94 (`gg_isopro`, in-sample) and PR #96 (`gg_isopro` `newdata` + training-path polarity fix). Lands as one PR before the v2.8.0 release candidate. A classification sibling sub-project (see [Future work](#future-work-endpoint-tracker)) may follow inside this release.

---

## Goal

Give users a tidy wrapper and a default plot for `varPro::beta.varpro()` — the per-rule lasso-coefficient refinement of varPro split-strength importance — with the same ergonomics as the other Phase 4 wrappers.

## Scope

Single PR. Regression family only (`o$family == "regr"`). Other endpoints are tracked in a dedicated section below so the future path is mapped, not lost.

---

## What `beta.varpro` actually returns

`varPro::beta.varpro(o)` takes a `varpro` fit, runs a per-region lasso (`glmnet::cv.glmnet` or `glmnet::glmnet` depending on `use.cv`) over each rule in the forest, and returns a modified `varpro` object where `$results` is replaced with a `data.frame` of shape `[n_rules x 5]` for regression:

| Column | Meaning |
|---|---|
| `tree` | Tree index |
| `branch` | Branch (rule) index within tree |
| `variable` | Released variable index (into `o$xvar.names`) |
| `n.oob` | OOB count for the rule |
| `imp` | **Lasso β coefficient** for the released variable in this rule's local regression |

The returned object's class is still `"varpro"`, so naïve dispatch on `gg_varpro()` would collide with the Phase 2 method. We avoid the collision by exporting a separate function and never dispatching on the modified fit.

### What `imp` actually is (pedantic, because the column name is misleading)

The `imp` column **is not a variable-importance score in the conventional sense.** It is a regularised regression coefficient, and the documentation has to be honest about this or users will scale-confuse it with split-strength VIMP (`gg_varpro`) or permutation VIMP. Specifically:

- For each rule (a `(tree, branch)` pair), `varPro::beta.varpro()` identifies the OOB observations that fall into that rule's region. The "released" variable for the rule is the predictor that was *not* used to define the region — the variable whose effect is being tested locally.
- A penalised regression of `y` on the released variable's values (within that region's OOB observations) is fit via `glmnet`. `use.cv = TRUE` selects λ by cross-validation (`cv.glmnet`, default `nfolds = 10`); `use.cv = FALSE` uses the full `glmnet` λ path and a fixed-rule choice. `use.1se = TRUE` (default) takes the `lambda.1se` solution; `use.1se = FALSE` takes `lambda.min`.
- The `imp` value stored on that rule's row of `$results` is the **fitted regression coefficient β̂ for the released variable** at the chosen λ — a scalar slope in a one-predictor local regression. **Sign is real** (direction of local association between predictor and response inside the region). **Magnitude depends on the scale of the predictor** (the regression is on the raw `x` values, not standardised); a predictor measured in millimetres will have a numerically smaller |β| than the same predictor measured in metres.
- Because of lasso shrinkage, `β̂` can be **exactly zero** when the local penalty drives the coefficient out. Those zeros are kept in `$results` (they aren't `NA`) and represent rules where lasso said "no local effect at this λ." If `glmnet` fails to converge or errors, that rule's `imp` is `NA_real_`; the wrapper drops `NA` rows but keeps the zeros.
- The per-variable aggregate `beta_mean = mean(|β̂|)` in `gg_beta_varpro` is therefore the **average magnitude of the local lasso slope across the rules where this variable was released**, including the zeros. It is *not* a permutation importance, *not* a split-strength importance, and *not* directly comparable on the same numeric scale to `gg_varpro`'s split-strength z-scores. The two ranking conventions can disagree, and that disagreement is often the point — `beta.varpro` is sensitive to a variable that has a real local slope inside many rules, even if the split-strength VIMP doesn't see it.
- **Scale caveat for the user-facing plot:** because `|β̂|` inherits the predictor's units, the bar chart's numeric scale is not interpretable across data sets, and comparing two variables measured in very different units (say, "age in years" vs "creatinine in mg/dL") requires the user to remember the units context. The roxygen will name this explicitly so a clinician reading the chart doesn't over-interpret "variable A's bar is 3× longer than variable B's" as "A is 3× more important."

This pedantic block lives in the `@details` of `gg_beta_varpro` and is mirrored (shortened) in the `plot.gg_beta_varpro` "Reading the chart" section. Both will say in code form: `imp_r = β̂_glmnet(y | x_v restricted to rule r, λ chosen by use.cv / use.1se)`.

---

## Architecture

```
varPro::varpro fit (regr)  ──►  gg_beta_varpro(fit, ..., cutoff = NULL)
                                       │
                                       ├── varPro::beta.varpro(fit, ...)   ← internal
                                       │       └── per-rule lasso β in $results
                                       │
                                       └── tidy data.frame
                                           class : c("gg_beta_varpro", "data.frame")
                                           cols  : variable, beta_mean, n_rules, selected
                                           attr  : provenance (family, ntree, cutoff,
                                                   use.cv, lambda.sel, n_rules_total, ...)
                                                  │
                                           plot / print / summary / autoplot
```

One small unit per file. The extractor (`R/gg_beta_varpro.R`) calls `beta.varpro` and reshapes; the plot method (`R/plot.gg_beta_varpro.R`) renders the bar chart. S3 companions live alongside the extractor (consistent with the Phase 2 layout).

---

## Extractor signature

```r
gg_beta_varpro(object, ..., cutoff = NULL, beta_fit = NULL)
```

- **`object`** — a `varpro` fit (regression family). `inherits(object, "varpro")` plus an explicit `object$family == "regr"` check; failure raises the family-guard error.
- **`...`** — forwarded to `varPro::beta.varpro()` **only when `beta_fit = NULL`** (otherwise unused; passing `...` alongside `beta_fit` warns once and ignores the args). Documented forwardables:
  `use.cv`, `use.1se`, `nfolds`, `maxit`, `thresh`, `max.rules.tree`, `max.tree`.
- **`cutoff`** — selection threshold on aggregate importance.
  - `NULL` (default): cutoff is `mean(beta_mean)` over all variables in the released set.
  - Numeric scalar: explicit threshold on the same scale as `beta_mean`.
- **`beta_fit`** — optional pre-computed `varPro::beta.varpro()` result. Lets the caller compute the expensive lasso step once and reuse it. `NULL` (default) → the wrapper runs `beta.varpro(object, ...)` itself. A non-`NULL` value must be a `varpro`-classed object whose `$results` has the post-`beta.varpro` shape (validated — see Caching below); otherwise the wrapper errors with the precomputed-shape guard message.

`...` is placed before `cutoff` and `beta_fit` so both are matched by name only. This mirrors the back-compat pattern locked in for PR #96 (Copilot review): named-only trailing arguments never break positional callers.

### Internal flow

1. Validate input: `inherits(object, "varpro")`, and `identical(object$family, "regr")`. Anything else errors with the family-guard message (see [Errors](#errors)).
2. Resolve the `beta.varpro` result:
   - If `beta_fit = NULL`: run `b <- varPro::beta.varpro(object, ...)`. **This is the expensive path** — see [Caching](#caching).
   - If `beta_fit` is supplied: validate it has the post-`beta.varpro` shape (`inherits(beta_fit, "varpro")`, `is.data.frame(beta_fit$results)`, and the required columns `tree / branch / variable / n.oob / imp` are present). On failure, error with the precomputed-shape guard message. On success, set `b <- beta_fit` and skip the expensive call. If `...` was supplied alongside `beta_fit`, emit a single `warning()` that those args were ignored.
3. If `b` is `NULL` (no rules retained — `beta.varpro` returns `NULL` when no rules pass the `oobCT > 0 & compCT > 0` filter), return an empty `gg_beta_varpro` frame with the right columns and a provenance attribute flagging `n_rules_total = 0`. Downstream `plot()` errors with a clear ggplot-empty message; no special-case in the plot.
4. Pull `res <- b$results`. Keep only rows with finite `imp` (lasso-refit failures land as `NA_real_`; lasso-shrunk-to-zero rows are kept).
4. Map `variable` (integer index) to a variable name via `b$xvar.names[res$variable]`. The Phase 2 wrapper uses the same lookup.
5. Aggregate per variable:

   ```r
   beta_mean[v] = mean(abs(res$imp[res$variable == v]))
   n_rules[v]   = sum(res$variable == v)
   ```

   - `beta_mean` uses **mean of |β̂|** — sign of the lasso coefficient is informative in the per-rule fit but irrelevant for a global importance ranking, and averaging signed coefficients cancels out variables that consistently swap sign across rules.
   - `n_rules` is the count of finite-β rules for that variable (lasso-shrunk-to-zero rules are included; only `NA_real_` failures are excluded). Surfaced so a user can see when an aggregate is built on three rules vs three hundred.
7. Determine `cutoff`: explicit argument when supplied; otherwise `mean(beta_mean)` across variables in the released set. `selected <- beta_mean >= cutoff`.
8. Build the tidy frame, sort by `beta_mean` descending, set class `c("gg_beta_varpro", "data.frame")`, attach provenance:
   - `source = "varPro::beta.varpro"`
   - `family = "regr"`
   - `ntree` (carried from `object$ntree` if present)
   - `cutoff` (resolved value)
   - `cutoff_default` (`TRUE` when user passed `NULL`)
   - `use.cv` (echo of the forwarded arg, defaulting to `FALSE` per `beta.varpro`)
   - `n_rules_total` = `nrow(b$results)`
   - `n_rules_nonzero` = count of rules with `|imp| > 0` after filtering NAs (lets the user see how much the lasso shrunk)
   - `precomputed` (`TRUE` when `beta_fit` was supplied; `FALSE` when the wrapper ran `beta.varpro` itself)
   - `xvar.names` carried through for downstream tools

### Errors

- Non-`varpro` input → `"gg_beta_varpro: expected a 'varpro' object from varPro::varpro()."` (mirrors the Phase 2 wording.)
- Non-regression family → `"gg_beta_varpro currently supports varpro regression forests only; got family = '<x>'. Classification, regr+, and survival are tracked under Phase 4d (see vignette / NEWS)."` — the message is intentionally long because it doubles as the user-visible signpost to the deferred sub-projects.
- `beta_fit` supplied but missing required `$results` columns → `"gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. Missing column(s): <names>."`
- `beta_fit` supplied alongside non-empty `...` → single `warning()` `"gg_beta_varpro: arguments in '...' ignored because beta_fit is supplied."` (warn, not error — user intent is unambiguous).
- `beta.varpro` returns `NULL` → graceful empty-frame return; no error.

---

## Caching

`varPro::beta.varpro()` is **the expensive call** in this pipeline. It fits a `glmnet`/`cv.glmnet` lasso once per rule (potentially thousands of rules on a modestly-sized forest, each call doing 10-fold CV when `use.cv = TRUE`). On a real data set users have reported runtimes in the tens of seconds to minutes; iterating on the wrapper (different cutoffs, snapshot rebuilds, vignette knits) without caching would be a quality-of-life cliff.

The wrapper handles this in two layers:

### Layer 1: explicit `beta_fit` argument (in this PR)

The caller does the expensive call once, then reuses the result:

```r
v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 200)
b <- varPro::beta.varpro(v, use.cv = TRUE)        # one expensive call

gg_a <- gg_beta_varpro(v, beta_fit = b)            # cheap
gg_b <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.5)
gg_c <- gg_beta_varpro(v, beta_fit = b, cutoff = quantile(gg_a$beta_mean, 0.75))
```

Concrete use cases that this enables:
- **Iterating on `cutoff`** — recompute selection without re-fitting any lasso.
- **Tests / fixtures** — `tests/testthat/helper-beta_varpro.R` computes a small `beta.varpro` fixture once per test session via a memoised helper (see Test fixtures below); every `gg_beta_varpro` test then passes that same `beta_fit` instead of re-fitting.
- **Vignettes** — `knitr` chunks set `beta_fit = cached_b` so rebuilding the vignette doesn't re-fit.
- **Comparisons** — running the wrapper twice with two different `cutoff` semantics (the `mean(beta_mean)` default vs a quantile) on the same `beta.varpro` result, for side-by-side figures.

The `precomputed` provenance flag is set so downstream tooling (summary, the eventual classification sibling, snapshot inspection) can tell whether the wrapper did the expensive work or accepted a cached input.

### Layer 2: session-scoped fixture helper for tests (in this PR)

`tests/testthat/helper-beta_varpro.R` exposes `.beta_fit_mtcars()` — a zero-arg function that returns a `beta.varpro` result on a fixed-seed mtcars `varpro` fit. The helper memoises into a file-scoped environment so the lasso runs **once per `R` session** even though many tests touch it. This keeps the new file's wall-clock test cost on the order of a single `beta.varpro` call rather than (number of tests) × (one call each). The helper does *not* cache to disk — keeping the fixture in-memory only means it never goes stale across package edits, and CRAN's test machines re-fit cleanly each run.

### What is explicitly *not* in this PR

- **On-disk cache.** No `cachem` / `memoise` integration. The pattern doesn't generalise well across users' working directories and would pollute the package with cache-management surface area. If a user wants disk caching they can wrap `beta.varpro` themselves with `memoise::memoise(cache = cachem::cache_disk(...))` and pass the result via `beta_fit`.
- **Auto-detection of a cached result.** The wrapper does not try to inspect a `varpro` object and guess whether it has already been through `beta.varpro`. `beta_fit` is explicit; the contract is clear.
- **Parallelism inside the wrapper.** `beta.varpro` itself accepts `papply = mclapply` (for `get.beta.entropy`-style flows); the wrapper just forwards via `...`. The wrapper does not introduce its own parallel layer.

---

## Plot

`plot.gg_beta_varpro(x, ...)` returns a single `ggplot`. Horizontal bar chart of `beta_mean` per variable, sorted descending so the eye lands on the top variable first:

- `geom_col()` with `fill = selected` mapped to the existing palette (`TRUE = "#4e8fcd"`, `FALSE = "#888888"`) — same colours as `plot.gg_varpro`.
- `geom_vline(yintercept = cutoff, linetype = "dashed", color = "#e74c3c", linewidth = 0.7)` — same cutoff styling.
- `coord_flip()` for readability.
- y-axis label: `"Mean |β| (per-rule lasso)"`.
- Caption: `"Mean |β| over <n_rules_total> rules. Lasso: <cv | fixed>, cutoff: <cutoff>."`
- `theme_minimal()`.

No `faithful = TRUE` per-rule overlay in this PR. The plot story stays one chart, one idiom.

---

## S3 companions

- `print.gg_beta_varpro(x, ...)` — header-only, mirrors `print.gg_varpro`. Prints class, family, n variables, n_rules_total, cutoff, then directs the user to `head(x)` for rows.
- `summary.gg_beta_varpro(object, ...)` — returns a printable `summary.gg_beta_varpro` object with `beta_mean` as a named numeric vector (sorted descending) and `n_rules` shown alongside.
- `autoplot.gg_beta_varpro(object, ...)` — delegates to `plot.gg_beta_varpro`. Standard pattern.

---

## Future work (endpoint tracker)

This section is part of the spec on purpose: it commits the package to a concrete classification follow-up and maps the other endpoints so they don't slip.

| Family | `beta.varpro` output | Status | Notes |
|---|---|---|---|
| `regr` | scalar `imp` (β) per rule | **In this PR** | Mean \|β\| per variable, single bar chart |
| `class` (binary) | scalar `imp` (binomial β) | **Sibling sub-project — likely v2.8.0** | Direct extension of regression path: same tidy shape, same plot. Motivating use case: **30-day mortality** (binary clinical outcome). Spec stub to live at `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-classification-design.md` once this PR lands |
| `class` (multi-class) | per-class `imp.<class>` + sum-β column | **Sibling sub-project — v2.8.0 if classification proceeds** | Two plot stories to choose between: (a) aggregated bar of summed \|β\| across classes, or (b) faceted bar per class — echoes `plot.gg_varpro` `conditional = TRUE`. Choice deferred to the classification sibling brainstorm |
| `regr+` (multivariate) | per-response `imp.<resp>` | **Deferred — v2.9.0** | Plot story genuinely unclear (which response to rank by? colour all in one panel? facet?). Needs its own brainstorm pass |
| `surv` | not supported upstream — `beta.varpro` calls `stop()` for non-regr/class families | **Blocked on upstream** | `@note` in roxygen documents this; revisit if varPro adds survival support to `beta.varpro` |

The classification sub-project is the immediate next candidate inside v2.8.0. It is *not* committed here — only mapped — so the current PR doesn't expand mid-implementation, but the path is concrete.

---

## Tests

Mirroring the Phase 1–4b coverage. **Every test uses the session-cached `.beta_fit_mtcars()` fixture (see [Caching](#caching))** unless the test is specifically exercising the `beta_fit = NULL` path — keeps the new file's wall-clock cost on the order of one real `beta.varpro` call.

1. **Shape:** `gg_beta_varpro(v, beta_fit = b)` returns `c("gg_beta_varpro", "data.frame")` with columns `variable / beta_mean / n_rules / selected` and `nrow == length(unique released variables)`.
2. **Aggregation correctness:** for at least two variables, `beta_mean` equals `mean(abs(b$results$imp[b$results$variable == idx]))` recomputed independently from the cached fixture.
3. **Family guard:** passing a classification `varpro` fit errors with the expected message (regex match on `"family = 'class'"`).
4. **Cutoff argument:** explicit `cutoff = 0` makes every row `selected = TRUE`; `cutoff = Inf` makes every row `selected = FALSE`.
5. **Default cutoff:** with `cutoff = NULL`, the resolved cutoff equals `mean(beta_mean)` and provenance carries `cutoff_default = TRUE`.
6. **Empty result handling:** when `beta.varpro` returns `NULL` (synthesised by passing a hand-built `beta_fit` whose `$results` has zero rows), the wrapper returns an empty frame with the correct columns and `n_rules_total = 0`; `plot()` errors with a non-cryptic message.
7. **Plot smoke:** `plot(gg_beta_varpro(v, beta_fit = b))` is a `ggplot`, `ggplot_build()` succeeds.
8. **S3 companions:** `print` returns invisibly, `summary` returns a `summary.gg_beta_varpro` carrying the right names, `autoplot` matches `plot` (identical `ggplot_build()` data).
9. **`beta_fit` cache equivalence:** running `gg_beta_varpro(v)` (no cache, internal call) and `gg_beta_varpro(v, beta_fit = b)` on the same fixture, under a fixed seed, yields *identical* tidy frames (column-by-column `expect_equal`). Provenance differs: the cached call carries `precomputed = TRUE`, the non-cached call carries `precomputed = FALSE`. This is the single test that exercises the internal-call path; placed in its own slow-test guard so CRAN can run a subset.
10. **`beta_fit` shape guard:** `gg_beta_varpro(v, beta_fit = list())` errors with `"beta_fit does not look like a varPro::beta.varpro() result"`. `gg_beta_varpro(v, beta_fit = b_minus_one_col)` (a `beta.varpro` result with one required column removed) errors and names the missing column.
11. **`...` + `beta_fit` warning:** `gg_beta_varpro(v, use.cv = TRUE, beta_fit = b)` emits exactly one `warning()` and still returns the correct frame from the cached `b` (i.e. `use.cv = TRUE` is ignored, not honoured).
12. **Zero-β rows kept:** synthesise a `beta_fit` with two rules whose `imp = 0` and two whose `imp ≠ 0` for the same variable. `n_rules` for that variable equals 4 (zeros included). This locks in the "lasso-shrunk-to-zero is data, not missingness" decision.

## Snapshots

One `vdiffr::expect_doppelganger` inside the existing `VDIFFR_RUN_TESTS` guard: `gg-beta-varpro-default` — default `plot()` on a small regression fit (mtcars or a synthetic fixture with a fixed seed). Skip cleanly without the env var.

## Documentation

- Two new roxygen pages (`gg_beta_varpro`, `plot.gg_beta_varpro`) in the markdown register (PR #95 enabled this).
- Pedagogical pass mandated, **especially on the β-coefficient semantics.** Required sections in `gg_beta_varpro` `@details`:
  - **"What this is doing"** — one paragraph: per-rule lasso regression of `y` on the released variable inside that rule's region.
  - **"What `imp` actually is"** — the pedantic block from above, lifted into roxygen verbatim: lasso β̂, sign carries direction, magnitude carries predictor units (so it is *not* unit-free, *not* a permutation/VIMP score, *not* comparable to `gg_varpro` on the same axis). Code-form definition `imp_r = β̂_glmnet(y | x_v restricted to rule r)`.
  - **"What's in the output"** — column-by-column on the tidy frame, including the `n_rules` count and the zero-β-rows decision.
  - **"What you use this for"** — picking variables when local effects matter more than split-strength contribution; the kind of disagreement with `gg_varpro` that's diagnostic.
- `plot.gg_beta_varpro` gets a parallel "Reading the chart" section that **names the scale caveat** (different predictor units → non-comparable bar lengths across data sets; comparable within one data set).
- `@note` block documenting the family-guard and pointing to the Future-work tracker (vignette / NEWS reference).
- `@section Caching:` block on `gg_beta_varpro` covering the `beta_fit` argument with the worked example from the [Caching](#caching) section.
- Update the existing varPro family pkgdown reference group (`_pkgdown.yml`) to include `gg_beta_varpro` and `plot.gg_beta_varpro`.

## Files

- **New:** `R/gg_beta_varpro.R`, `R/plot.gg_beta_varpro.R`, `tests/testthat/test_gg_beta_varpro.R`, `tests/testthat/helper-beta_varpro.R` (session-memoised `.beta_fit_mtcars()` fixture)
- **Modify:** `tests/testthat/test_snapshots.R`, `NEWS.md`, `DESCRIPTION` (version bump to the next free `2.7.3.901x` slot — `.9011` if no other PR has landed first), `_pkgdown.yml`
- **New (after this PR lands):** `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-classification-design.md` — classification sibling spec stub

## Acceptance criteria

- `R CMD check --as-cran`: 0 errors / 0 warnings / 0 notes.
- `devtools::test()`: 0 failures. New tests pass; all prior Phase 4 coverage stays green.
- pkgdown reference group includes the new pages and they render without warnings.
- One PR before the v2.8.0 release candidate.

## Out of scope

- Classification, regr+, survival (tracked in [Future work](#future-work-endpoint-tracker)).
- `faithful = TRUE` per-rule β overlay on top of the bar chart.
- A side-by-side compare-vs-`gg_varpro` plot (interesting but a separate visual question — would need its own brainstorm).
- On-disk caching of `beta.varpro` results inside the package (rationale in [Caching](#caching)).
- Auto-detection of an already-`beta.varpro`-processed `varpro` object (caller must pass it via `beta_fit` explicitly).
- Any change to upstream `varPro::beta.varpro` or `gg_varpro` / `plot.gg_varpro`.
