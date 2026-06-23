# Design: Interpretable y-axis scales for varPro partial plots (v3.3.0)

- **Date:** 2026-06-23
- **Status:** Approved design (pre-implementation)
- **Version:** 3.2.0 → 3.3.0 (CRAN minor, off `main`)
- **Branch:** `feat/varpro-figures-3.3.0`
- **Functions:** `gg_partial_varpro()`, `plot.gg_partial_varpro()` (+ internals)

---

## 1. Motivation

`varPro::partialpro()` returns classification partial effects on the **log-odds**
scale and survival partial effects as **ensemble mortality** (an unbounded
relative-risk score). Neither is the scale a clinical reader wants:

- Classification curves should be readable as **probability** P(Y = target).
- Survival curves should be readable as a **bounded** quantity — survival
  probability S(τ) or restricted mean survival time RMST(τ) — not the unbounded
  mortality score.

v3.2.0 already added the RMST(τ) learner and a "Reading an RMST curve" doc
section. v3.3.0 extends the same idea to **classification probability** and
**survival probability**, and makes the *default* scales interpretable.

### Goals

1. Classification partial plots return **probability by default**, with `odds`
   and `logodds` as options.
2. Survival partial plots can return **survival probability S(τ)** on the same
   partialpro/UVT engine as mortality and RMST.
3. Never silently hand back the misleading unbounded mortality score for a
   survival fit: `scale = "auto"` defaults to bounded **survival probability**
   S(τ) at a units-safe, data-driven default τ (mortality stays an explicit
   opt-in).
4. Document every scale's interpretation honestly.

### Non-goals

- No change to the survival `chf` (cumulative hazard) path.
- No change to regression partial plots (already on the response scale).
- No multi-τ faceting in one call (one τ per call; users loop, as in ST1267).
- No new exported functions — this is scale/default work on existing entry
  points.

---

## 2. Current behaviour (baseline)

`scale` argument: `c("auto", "rmst", "mortality", "surv", "chf")`.

- `.resolve_varpro_scale(scale, family)` (in `R/gg_partial_varpro.R`): `auto` →
  `"mortality"` for survival, `"generic"` for regr/class/unknown.
- Classification: `partialpro()` applies `mylogodds()` internally, so
  `yhat.par`/`yhat.nonpar`/`yhat.causal` are log-odds of the target class.
  `gg_partial_varpro()` averages them (`colMeans`) into the
  continuous/categorical frames; the resolved scale is `"generic"`, y-label
  `"Partial Effect"`. **The values are log-odds but unlabelled as such.**
- Survival: `auto` → `mortality` (path A, partialpro default learner). `rmst`
  (path A, RMST learner, requires `time`). `surv`/`chf` route through **path C**
  (`gg_partial_rfsrc` on `object$rf`) — a *different* engine, time-point S(t)/
  CHF curves, no UVT, no par/nonpar/causal structure.
- The three curves' scales (from partialpro internals): `yhat.par` and
  `yhat.nonpar` are **absolute** (global mean intercept added back);
  `yhat.causal` is a **centered contrast** (deviation from the reference grid
  point — a log odds-ratio for classification).

---

## 3. Design

### 3a. New scale vocabulary

`scale` gains three classification values: `"prob"`, `"odds"`, `"logodds"`.
Full set:

```
scale = c("auto", "prob", "odds", "logodds",      # classification (+ generic)
          "rmst", "surv", "mortality", "chf")      # survival
```

`match.arg` accepts all; validity is family-dependent (already true today).

### 3b. Scale resolution (`.resolve_varpro_scale`)

| `scale` | family | resolves to | notes |
|---|---|---|---|
| `auto` | class | `prob` | **changed** (was `generic`) |
| `auto` | regr | `generic` | unchanged |
| `auto` | surv | `surv` | **changed** (was `mortality`); τ defaults to median follow-up (3e) |
| `auto` | unknown (part_dta only) | `generic` | unchanged — backward compatible |
| explicit | any | itself | unchanged |

Backward compatibility: when only `part_dta` is supplied (no `object`), family is
unknown → `generic`, log-odds values as before. The probability default only
engages when an `object` identifies the fit as classification.

### 3c. Classification conversion (where & how)

partialpro returns log-odds. Conversion happens in the **extractor**, on the
per-observation matrices **before averaging** — so the partial value is the
*mean of probabilities*, not the probability of the mean log-odds
(`mean(plogis(z))`, not `plogis(mean(z))`).

In `.build_varpro_dfs()` (and `.process_cat_var()`), apply a per-scale transform
to `yhat.par`/`yhat.nonpar` matrices before `colMeans`:

- `prob`: `plogis(z)`
- `odds`: `exp(z)`
- `logodds` / `generic`: identity

The transform applies to the **absolute** curves only (`par`, `nonpar`).

### 3d. The `causal` curve

`yhat.causal` is a centered contrast (log odds-ratio for classification; a
centered contrast in whatever units the learner returns for survival). A
contrast cannot share a probability/odds axis with the absolute level curves.

Rule (consistent across families):

- **Additive/unbounded scales** — `logodds` (class), `mortality`, `rmst` (surv):
  all three curves shown (level + contrast commensurable). Current behaviour.
- **Bounded scales** — `prob`, `odds` (class), `surv` (surv): `par`/`nonpar`
  convert; **`causal` is not shown.** The extractor sets the `causal` column to
  `NA` for bounded scales; the plot drops it from `type` and **warns** if the
  user explicitly passed `type = "causal"`.

### 3e. Survival: S(t) learner + data-driven default τ

**New internal `.surv_learner(object, tau)`** — a near-clone of `.rmst_learner`,
replacing the integral with a single-column pull:

```r
pr    <- randomForestSRC::predict.rfsrc(rf, newx, perf.type = "none")
surv  <- pr$survival            # (or survival.oob for the OOB/no-newx branch)
times <- pr$time.interest
surv[, which.min(abs(times - tau))]    # S(tau | x), snapped to nearest event time
```

Returns a bounded 0–1 vector; partialpro fits par/nonpar on it directly (no
extractor-level transform needed — the learner already yields probability).

**Default τ (the key safety property).** When `time` is `NULL` for a survival
scale that needs one (`surv`, `rmst`), τ defaults to the **median follow-up
time** — `median()` of the model's observed event/censoring times. This is
*units-safe by construction*: it is computed from the fit, so it is always in
the model's own time units and cannot mismatch them — unlike a hand-typed τ,
which is exactly how the ST1267 days-vs-years bug arose. A derived default is
therefore *safer* than requiring the user to type a number. The resolved τ
appears in the y-label (`"... at t = X"`) and a one-time message; `time = τ`
overrides it.

Routing & defaults:

- `scale = "auto"` + survival → **`surv`** at the default τ — the bounded,
  interpretable default (retires the old unbounded `mortality` default).
- `scale = "surv"` → **path A** via `.surv_learner` (retires the old path-C
  `surv` route); τ defaults to median follow-up if not supplied.
- `scale = "rmst"` → path A via `.rmst_learner`; τ defaults to median follow-up
  if not supplied (**loosened** from v3.2.0, which *required* `time`).
- `scale = "mortality"` → path A, default learner, no τ (explicit opt-in; the
  unbounded score is never the silent default now).
- `scale = "chf"` → **path C** unchanged (no learner; cumulative hazard).

No survival scale errors on a missing `time` any more — the default τ fills in.
`.validate_*` drop the `surv`/`rmst` "requires time" stops; a *supplied* `time`
still gets the scalar-finite check.

### 3f. Y-axis labels (`.partial_varpro_ylabel`)

| scale | label |
|---|---|
| `prob` | `P(Y = <target>)` (or `Probability` if target unknown) |
| `odds` | `Odds(Y = <target>)` |
| `logodds` | `Log-odds(Y = <target>)` |
| `surv` | `Survival probability at t = <τ>` |
| `rmst` | `RMST (τ = <τ>)` (unchanged) |
| `mortality` | `Ensemble mortality (expected events)` (unchanged) |
| `chf` | `Cumulative hazard at t = <t>` (unchanged) |

Target class for the classification labels: resolved from `object` — the
`target` passed through `...` if present, else the **last factor level** of the
response (partialpro's default target). If only `part_dta` is supplied, omit the
class name (`Probability`/`Odds`/`Log-odds`).

### 3g. Provenance

`provenance$scale` records the resolved scale (already present). Add
`provenance$target` (the classification target class label, or `NA`) so the plot
can build the label without re-deriving it.

---

## 4. Documentation

- `plot.gg_partial_varpro` gains `@section`s mirroring the existing
  ensemble-mortality and "Reading an RMST curve" sections:
  - **Reading a probability curve (scale = "prob")** — P(Y = target); odds/
    log-odds variants; why `causal` is hidden here.
  - **Reading a survival-probability curve (scale = "surv")** — S(τ | x),
    bounded 0–1, τ in the model's time units, higher = more survival.
- `gg_partial_varpro` `@details`: the classification probability default; the
  survival `surv` default at median-follow-up τ; the `surv` learner; the
  units-safe data-driven τ (and how to override with `time`).
- NEWS v3.3.0 bullets: classification probability default (behaviour change);
  survival `auto` → `surv` default + `surv`/`rmst` τ defaulting to median
  follow-up (behaviour change); the `surv` learner; RMST interpretation docs
  (already present).

---

## 5. Testing

CRAN-safe unit tests (no varpro grow) where possible:

- `.surv_learner`: against a small `rfsrc` survival fit (like the existing
  `make_mock_cpath` helper) — returns length-n, bounded 0–1, equals the snapped
  S(τ) column; OOB and newdata branches.
- Scale resolution: `.resolve_varpro_scale("auto","class") == "prob"`, etc.
- Classification conversion: `prob`/`odds`/`logodds` produce expected
  transforms of a mock log-odds `part_dta`; mean-of-probabilities (not
  plogis-of-mean) verified on a hand-computable case.
- `causal` hidden on bounded scales; warning when `type = "causal"` requested
  with `prob`/`odds`/`surv`.
- Default τ: `scale = "surv"`/`"rmst"` with `time = NULL` resolve τ to the
  median follow-up (verify the resolved τ equals `median()` of the observed
  times and is surfaced in the label/message); `scale = "auto"` + a survival
  object resolves to `surv` (not mortality, no error).
- Labels: y-label strings for each scale (incl. target-class name).
- End-to-end (skip_on_cran): a real classification varpro fit →
  `scale = "prob"` populated, values in [0, 1]; a survival fit →
  `scale = "surv", time = τ` populated, values in [0, 1].

Backward-compat tests: `part_dta`-only classification call still yields
log-odds/`generic` (no error, no conversion).

---

## 6. Backward compatibility / breaking changes

Two deliberate behaviour changes, documented prominently in NEWS:

1. **Classification `scale = "auto"` default changes** from generic log-odds to
   probability. Existing object-driven classification calls now return
   probability values + a `P(Y=…)` label. (`part_dta`-only calls are unchanged.)
2. **Survival `scale = "auto"` now returns survival probability** S(τ) at the
   median-follow-up default τ (was ensemble mortality). The old mortality
   behaviour remains available via `scale = "mortality"`. Also: `scale = "rmst"`
   and `"surv"` now **default τ to median follow-up** when `time` is omitted
   (v3.2.0's `rmst` required `time` — this is a loosening, not a break); a
   supplied `time` behaves as before.

The old path-C `surv` route is retired (replaced by the learner); `chf` is
unchanged.

---

## 7. Open implementation detail

- **Target-class extraction.** Confirm where partialpro's effective `target`
  lands when passed via `...`, and the exact object slot for the response factor
  levels (`object$y` vs `object$yvar.org`), so the label names the correct
  class. Resolve during implementation; fall back to `Probability` (no class
  name) if it can't be determined.
- **Median-follow-up source.** Confirm the object slot for the observed survival
  times to compute the default τ = `median(follow-up)` — likely the time column
  of `object$rf$yvar` (the survival response), *not* `time.interest` (which is
  the distinct event times, unweighted by subjects). Fall back to
  `median(object$rf$time.interest)` if the raw times aren't cleanly reachable.

---

## 8. Touched files

- `R/gg_partial_varpro.R` — `scale` arg values, `.resolve_varpro_scale`,
  `.build_varpro_dfs`/`.process_cat_var` (conversion), `.surv_learner`,
  validation, provenance `target`, routing for `surv`.
- `R/plot.gg_partial_varpro.R` — `.partial_varpro_ylabel` (new labels),
  `causal` handling/warning, `@section`s.
- `R/gg_partialpro.R` — alias inherits new args automatically.
- `NEWS.md`, `man/` (regenerated), `tests/testthat/test_gg_partial_varpro.R`.
