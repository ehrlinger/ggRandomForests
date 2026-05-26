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
gg_beta_varpro(object, ..., cutoff = NULL)
```

- **`object`** — a `varpro` fit (regression family). `inherits(object, "varpro")` plus an explicit `object$family == "regr"` check; failure raises the family-guard error.
- **`...`** — forwarded to `varPro::beta.varpro()`. Documented forwardables:
  `use.cv`, `use.1se`, `nfolds`, `maxit`, `thresh`, `max.rules.tree`, `max.tree`.
- **`cutoff`** — selection threshold on aggregate importance.
  - `NULL` (default): cutoff is `mean(beta_mean)` over all variables in the released set.
  - Numeric scalar: explicit threshold on the same scale as `beta_mean`.

`...` is placed before `cutoff` so the cutoff is matched by name only. This mirrors the back-compat pattern locked in for PR #96 (Copilot review): a named-only trailing argument never breaks positional callers.

### Internal flow

1. Validate input: `inherits(object, "varpro")`, and `identical(object$family, "regr")`. Anything else errors with the family-guard message (see [Errors](#errors)).
2. Run `b <- varPro::beta.varpro(object, ...)`. If `b` is `NULL` (no rules retained — `beta.varpro` returns `NULL` when no rules pass the `oobCT > 0 & compCT > 0` filter), return an empty `gg_beta_varpro` frame with the right columns and a provenance attribute flagging `n_rules_total = 0`. Downstream `plot()` errors with a clear ggplot-empty message; no special-case in the plot.
3. Pull `res <- b$results`. Keep only rows with finite `imp` (lasso-refit failures land as `NA_real_`).
4. Map `variable` (integer index) to a variable name via `b$xvar.names[res$variable]`. The Phase 2 wrapper uses the same lookup.
5. Aggregate per variable:

   ```r
   beta_mean[v] = mean(abs(res$imp[res$variable == v]))
   n_rules[v]   = sum(res$variable == v)
   ```

   - `beta_mean` uses **mean of |β|** — sign of the lasso coefficient is informative in the per-rule fit but irrelevant for a global importance ranking, and averaging signed coefficients cancels out variables that consistently swap sign across rules.
   - `n_rules` is the count of finite-β rules for that variable. Surfaced so a user can see when an aggregate is built on three rules vs three hundred.
6. Determine `cutoff`: explicit argument when supplied; otherwise `mean(beta_mean)` across variables in the released set. `selected <- beta_mean >= cutoff`.
7. Build the tidy frame, sort by `beta_mean` descending, set class `c("gg_beta_varpro", "data.frame")`, attach provenance:
   - `source = "varPro::beta.varpro"`
   - `family = "regr"`
   - `ntree` (carried from `object$ntree` if present)
   - `cutoff` (resolved value)
   - `cutoff_default` (`TRUE` when user passed `NULL`)
   - `use.cv` (echo of the forwarded arg, defaulting to `FALSE` per `beta.varpro`)
   - `n_rules_total` = `nrow(b$results)`
   - `xvar.names` carried through for downstream tools

### Errors

- Non-`varpro` input → `"gg_beta_varpro: expected a 'varpro' object from varPro::varpro()."` (mirrors the Phase 2 wording.)
- Non-regression family → `"gg_beta_varpro currently supports varpro regression forests only; got family = '<x>'. Classification, regr+, and survival are tracked under Phase 4d (see vignette / NEWS)."` — the message is intentionally long because it doubles as the user-visible signpost to the deferred sub-projects.
- `beta.varpro` returns `NULL` → graceful empty-frame return; no error.

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

Mirroring the Phase 1–4b coverage:

1. **Shape:** `gg_beta_varpro(v)` returns `c("gg_beta_varpro", "data.frame")` with columns `variable / beta_mean / n_rules / selected` and `nrow == length(unique released variables)`.
2. **Aggregation correctness:** for at least two variables, `beta_mean` equals `mean(abs(b$results$imp[b$results$variable == idx]))` recomputed independently from the underlying `beta.varpro` output.
3. **Family guard:** passing a classification `varpro` fit errors with the expected message (regex match on `"family = 'class'"`).
4. **Cutoff argument:** explicit `cutoff = 0` makes every row `selected = TRUE`; `cutoff = Inf` makes every row `selected = FALSE`.
5. **Default cutoff:** with `cutoff = NULL`, the resolved cutoff equals `mean(beta_mean)` and provenance carries `cutoff_default = TRUE`.
6. **Empty result handling:** when `beta.varpro` returns `NULL` (synthesised by mocking or by a degenerate ntree=1 fit), the wrapper returns an empty frame with the correct columns and `n_rules_total = 0`; `plot()` errors with a non-cryptic message.
7. **Plot smoke:** `plot(gg_beta_varpro(v))` is a `ggplot`, `ggplot_build()` succeeds.
8. **S3 companions:** `print` returns invisibly, `summary` returns a `summary.gg_beta_varpro` carrying the right names, `autoplot` matches `plot` (identical `ggplot_build()` data).

## Snapshots

One `vdiffr::expect_doppelganger` inside the existing `VDIFFR_RUN_TESTS` guard: `gg-beta-varpro-default` — default `plot()` on a small regression fit (mtcars or a synthetic fixture with a fixed seed). Skip cleanly without the env var.

## Documentation

- Two new roxygen pages (`gg_beta_varpro`, `plot.gg_beta_varpro`) in the markdown register (PR #95 enabled this).
- A "What this is doing" / "What's in the output" / "What you use this for" structure, consistent with the Phase 4 pedagogical pass.
- `@note` block documenting the family-guard and pointing to the Future-work tracker (vignette / NEWS reference).
- Update the existing varPro family pkgdown reference group (`_pkgdown.yml`) to include `gg_beta_varpro` and `plot.gg_beta_varpro`.

## Files

- **New:** `R/gg_beta_varpro.R`, `R/plot.gg_beta_varpro.R`, `tests/testthat/test_gg_beta_varpro.R`
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
- Any change to upstream `varPro::beta.varpro` or `gg_varpro` / `plot.gg_varpro`.
