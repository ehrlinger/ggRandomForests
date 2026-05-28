# Phase 5 — Consolidated varPro Vignette Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Author one new vignette (`vignettes/varpro.qmd`) walking the full `gg_*` varPro layer across regression, classification, and survival worked examples — the headline document for v2.8.0.

**Architecture:** Quarto `.qmd` vignette built by `quarto::html`. Live chunk execution with `knitr` chunk-level cache (each expensive `varpro` / `beta.varpro` / `ivarpro` call is its own chunk so a rebuild only re-runs what changed). Six sections per the spec: intro, regression, classification, survival, cross-cutting reference, further reading. Voice written from scratch to current `~/Documents/ObsidianVault/memory/writing-voice.md`; no post-hoc audit.

**Tech Stack:** R, Quarto, knitr, ggplot2, varPro, MASS::Boston, datasets::iris, randomForestSRC::pbc.

**Spec:** `dev/plans/2026-05-27-phase5-varpro-vignette-design.md`

**Branch state:** `feat/phase5-varpro-vignette` already exists from `origin/main` at `2.7.3.9013`. Spec committed (`4df5d15`). This PR bumps to `2.7.3.9014`.

---

## File map

| File | Purpose |
|---|---|
| `vignettes/varpro.qmd` *(new)* | The vignette. Six sections per the spec. |
| `vignettes/ggRandomForests.bib` *(modify, append)* | Add Lu & Ishwaran 2024 and Ishwaran 2007 bib entries if missing. |
| `vignettes/ggRandomForests.qmd` *(modify, one line)* | Add a pointer to the new varPro vignette in the existing "Next steps" section. |
| `_pkgdown.yml` *(modify)* | Add `varpro.qmd` to the Articles navbar. |
| `DESCRIPTION` *(modify)* | Version → `2.7.3.9014`. |
| `NEWS.md` *(modify)* | Phase 5 bullet. |
| `.gitignore` *(verify, possibly modify)* | Confirm `vignettes/varpro_cache/`, `vignettes/varpro_files/`, `vignettes/varpro.html` are ignored. |

---

## Authoring principles applied throughout

1. **Voice fingerprint at `~/Documents/ObsidianVault/memory/writing-voice.md`** — read once at the start of T2 and keep it open. Two registers: narrative for section intros, terse for code captions / matrices / error notes.
2. **Lift, don't rewrite.** Each `gg_*` wrapper already has pedagogical `@section What this is doing` / `@section What you use this for` roxygen blocks (Phase 4 pedagogical pass + Phase 4d's `local_imp` block). Where those sentences fit the vignette's flow, lift them with minimal editing. Where the vignette needs different framing, write new prose.
3. **One chunk per expensive call.** `varpro`, `beta.varpro`, `ivarpro` each get their own chunk with `cache: true`. Cheap calls (`gg_*` extractors, plot calls) can share chunks.
4. **Code shown, plots rendered.** Never `eval: false` on a `gg_*` call — the figures are the point.
5. **Render after every section.** A broken chunk early stops the build. Always run `quarto::quarto_render("vignettes/varpro.qmd")` before commit.

---

## Task 0: Open dev cycle

**Files:**
- Modify: `DESCRIPTION:4`
- Modify: `NEWS.md`
- Verify: `.gitignore`

- [ ] **Step 1: Confirm branch state**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/phase5-varpro-vignette
grep '^Version:' DESCRIPTION   # expect: 2.7.3.9013
```

- [ ] **Step 2: Bump version**

Edit `DESCRIPTION` line 4: `Version: 2.7.3.9014`.
Edit `NEWS.md` line 2: `Version: 2.7.3.9014`.

- [ ] **Step 3: Insert NEWS placeholder bullet**

Insert at the top of the `ggRandomForests v2.8.0 (development) — continued` section in `NEWS.md`:

```
* New vignette: "Exploring variable importance with varPro." Walks the
  full gg_* varPro layer (gg_partial_varpro, gg_varpro, gg_udependent,
  gg_isopro, gg_beta_varpro, gg_ivarpro) on three worked examples —
  regression (Boston), classification (iris binary + multi-class), and
  survival (PBC). Includes a family-support matrix documenting which
  wrapper works for which forest family. Headline document for v2.8.0.
```

- [ ] **Step 4: Verify `.gitignore` covers vignette build artefacts**

```bash
grep -E "vignettes/.*_(cache|files)|vignettes/.*\.html" .gitignore
```

If `vignettes/varpro_cache/` is not already matched by an existing pattern (the other vignettes' cache directories should already be covered), add:

```
vignettes/varpro_cache/
vignettes/varpro_files/
vignettes/varpro.html
```

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NEWS.md .gitignore
git commit -m "chore: open v2.7.3.9014 dev cycle (Phase 5 varPro vignette)"
```

---

## Task 1: Scaffold the vignette (front matter + setup chunk)

**Files:**
- Create: `vignettes/varpro.qmd`

- [ ] **Step 1: Read the voice fingerprint**

```bash
cat ~/Documents/ObsidianVault/memory/writing-voice.md
```

Keep it open in the editor for reference throughout sections 2–7. The two-register split (narrative vs terse) and the anti-patterns it lists are the contract.

- [ ] **Step 2: Read the existing vignettes' front matter** so the new vignette matches the in-repo convention.

```bash
head -25 vignettes/ggRandomForests-regression.qmd
```

- [ ] **Step 3: Write the scaffold**

Create `vignettes/varpro.qmd` with:

````markdown
---
title: "Exploring variable importance with varPro"
author: "John Ehrlinger"
date: today
format:
  html:
    toc: true
    toc-depth: 3
    html-math-method: mathjax
    code-fold: false
bibliography: ggRandomForests.bib
vignette: >
  %\VignetteIndexEntry{Exploring variable importance with varPro}
  %\VignetteEngine{quarto::html}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  message    = FALSE,
  warning    = FALSE,
  fig.width  = 7,
  fig.height = 4.5,
  cache      = TRUE,
  cache.path = "varpro_cache/"
)
```

```{r libs}
library(ggRandomForests)
library(varPro)
library(ggplot2)
```

# Placeholder

Scaffold only — sections to follow.
````

- [ ] **Step 4: Verify it renders empty**

```bash
R -q -e 'quarto::quarto_render("vignettes/varpro.qmd")' 2>&1 | tail -5
```

Expected: succeeds, produces `vignettes/varpro.html`. If quarto isn't installed run `quarto check` separately and surface the error.

- [ ] **Step 5: Commit the scaffold**

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: scaffold varpro.qmd (front matter + setup chunk)"
```

---

## Task 2: Section 1 — What varPro is

**Files:**
- Modify: `vignettes/varpro.qmd`

Replace the `# Placeholder` heading with section 1. Aim ~200–300 words narrative register. The content: release rules vs permutation, what the `gg_*` layer adds.

- [ ] **Step 1: Draft Section 1 prose**

Replace the `# Placeholder` section in `vignettes/varpro.qmd` with the body below. Keep it tight — this is the front door, not a tutorial.

````markdown
# What varPro is

Random-forest variable importance has, for two decades, mostly meant
*permutation importance*: shuffle a column, measure the drop in OOB
accuracy, repeat. It works, but the score depends on artificial data — the
permuted column — and the answer is unstable when variables are correlated.

varPro [@Lu2024varpro] replaces the shuffling with *release rules*. From a
guided-splitting forest, varPro samples a collection of decision-rule
branches; for each variable in each rule, it compares the response inside
the rule's region with the response after the constraint on that variable
is "released". The size of that change, aggregated over rules and trees,
is the variable's importance. No synthetic columns, no permutation — the
contrast is between two real subsets of the data.

That core machinery feeds several views. **`gg_varpro()`** summarises the
per-tree importance distribution. **`gg_beta_varpro()`** refines those
release-rule contrasts with a per-rule lasso. **`gg_partial_varpro()`**
turns the release machinery into partial-dependence curves.
**`gg_udependent()`** reads cross-variable dependency off a `uvarpro()`
fit. **`gg_isopro()`** scores observations for anomaly using an
isolation-forest variant. **`gg_ivarpro()`** computes per-observation
local importance.

This vignette walks all six wrappers on three worked examples — a
regression problem (Boston housing), a classification problem (iris,
binary and multi-class), and a survival problem (PBC). The closing
section is a one-page reference matrix mapping each wrapper to the
forest families it supports.
````

- [ ] **Step 2: Render to confirm**

```bash
R -q -e 'quarto::quarto_render("vignettes/varpro.qmd")' 2>&1 | tail -5
```

- [ ] **Step 3: Commit**

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 1 — what varPro is"
```

---

## Task 3: Section 3 — Regression worked example (Boston)

**Files:**
- Modify: `vignettes/varpro.qmd`

**Note:** Section 2 is "Setup" (the libs chunk already in place from Task 1 covers it). Section 3 is the regression worked example — the longest section in the vignette. Lift pedagogical text from the Phase 4 roxygen blocks where it fits the prose.

This task has six sub-steps, one per `gg_*` wrapper. Each sub-step authors:
1. A short prose introduction (~80–150 words narrative register)
2. One code chunk (the wrapper call + the plot)
3. A short caption / "what this tells you" paragraph

Render and commit after **each** sub-step. The cache means each render only re-runs the new chunks.

- [ ] **Step 1: Add the section header and the Boston fit**

Append to `vignettes/varpro.qmd`:

````markdown
# Regression: Boston housing

We start with the Boston housing data: 506 census tracts, 13 numeric
predictors, median home value as the response.

```{r boston-fit}
data("Boston", package = "MASS")
set.seed(20260527L)
v_boston <- varPro::varpro(medv ~ ., data = Boston, ntree = 50)
v_boston
```

`varpro()` builds the importance machinery — release rules, per-tree
scores, the splitweight vector — once. Every subsequent figure in this
section reads off that single fit.
````

Render. Commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 setup — Boston varpro fit"
```

- [ ] **Step 2: Sub-section `gg_varpro()`**

Append:

````markdown
## Per-tree importance with `gg_varpro()`

The first question varPro answers is the same one permutation importance
answers — which variables matter, ranked. `gg_varpro()` extracts the
per-tree importance scores and draws their distribution as a horizontal
boxplot, one row per variable, sorted by median z-score. Variables above
a configurable cutoff (default 0.79) are highlighted.

```{r boston-gg-varpro}
plot(gg_varpro(v_boston))
```

The narrow boxes near the top are the variables varPro is confident
about — every tree agrees they matter. Wide boxes that straddle the
cutoff line are the ones to look at twice; the forest disagrees with
itself.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_varpro on Boston"
```

- [ ] **Step 3: Sub-section `gg_partial_varpro()`**

Append:

````markdown
## Partial dependence with `gg_partial_varpro()`

The ranking view tells you *which* variables matter. Partial dependence
asks the next question: *how* does the response change with a variable,
holding the others fixed? `gg_partial_varpro()` wraps
`varPro::partialpro()` and returns a tidy frame of parametric,
non-parametric, and causal partial-dependence curves.

```{r boston-gg-partial-varpro}
gg_pd <- gg_partial_varpro(v_boston)
plot(gg_pd)
```

Each panel is a single predictor. The three curves correspond to the
three estimators varPro carries — read them as a sensitivity analysis.
Tight agreement is reassuring; divergence is information.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_partial_varpro on Boston"
```

- [ ] **Step 4: Sub-section `gg_beta_varpro()`**

Append:

````markdown
## Per-rule lasso refinement with `gg_beta_varpro()`

`gg_varpro()`'s ranking is built from the release-rule contrast.
`gg_beta_varpro()` re-asks the question variable by variable inside each
rule: it fits a one-predictor lasso of the response on the released
variable, restricted to the OOB observations in that rule's region. The
fitted β̂ is the variable's *local* effect inside that rule. Aggregating
`mean(|β̂|)` across rules gives one number per variable; that number is
a regression-coefficient-flavoured importance, not a VIMP score.

Because `beta.varpro()` is expensive (a `glmnet` per rule), the wrapper
accepts a pre-computed `beta_fit` so you can iterate on selection
without re-fitting.

```{r boston-beta-fit, cache=TRUE}
b_boston <- varPro::beta.varpro(v_boston)
```

```{r boston-gg-beta-varpro}
plot(gg_beta_varpro(v_boston, beta_fit = b_boston))
```

Compare against `gg_varpro()` above. Disagreement is the diagnostic
signal: a variable that ranks high here but low there is one whose
local linear effect inside many rules is real even when the release
contrast is modest.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_beta_varpro on Boston"
```

- [ ] **Step 5: Sub-section `gg_udependent()`**

Append:

````markdown
## Cross-variable dependency with `gg_udependent()`

The three views so far take one variable at a time. `gg_udependent()`
reads cross-variable structure off a `uvarpro()` fit and draws the
result as a network: nodes are variables, edges are dependencies above a
configurable threshold. The visual is built with `ggraph`, which is in
`Suggests` rather than `Imports` — install it if you want this view.

```{r boston-uvarpro, cache=TRUE}
u_boston <- varPro::uvarpro(Boston[, setdiff(names(Boston), "medv")],
                            ntree = 50)
```

```{r boston-gg-udependent, eval = requireNamespace("ggraph", quietly = TRUE)}
plot(gg_udependent(u_boston))
```

Clusters of mutually-connected variables are worth checking for
redundancy — they may be several views of the same underlying quantity.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_udependent on Boston"
```

- [ ] **Step 6: Sub-section `gg_isopro()`**

Append:

````markdown
## Anomaly scoring with `gg_isopro()`

Variable importance is one axis; *observation* outlierness is another.
`gg_isopro()` wraps `varPro::isopro()` — an isolation-forest variant
that scores how anomalous each training row looks — and renders the
result as a ranked elbow plus a density of the scores. The score is
on `[0, 1]`; the wrapper's convention is "higher = more anomalous"
(opposite of varPro's native polarity; the wrapper flips it for
consistency).

```{r boston-isopro, cache=TRUE}
iso_boston <- varPro::isopro(data = Boston[, setdiff(names(Boston),
                                                      "medv")],
                              method = "rnd", sampsize = 256,
                              ntree = 50)
```

```{r boston-gg-isopro}
plot(gg_isopro(iso_boston))
```

The elbow flags rows that diverge from the bulk. Pair with the
domain — anomalous in feature space is not the same as wrong, but it's
often where the most interesting cases live.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_isopro on Boston"
```

- [ ] **Step 7: Sub-section `gg_ivarpro()`**

Append:

````markdown
## Local importance with `gg_ivarpro()`

The wrappers so far aggregate across observations. `gg_ivarpro()` does
the opposite: it returns one value per (observation, variable) pair,
capturing how much variable *v* contributed to predicting observation
*i*. The aggregate view is a distribution of those local importances
per variable; the per-observation view is a horizontal bar of one row's
local importances across variables.

`ivarpro()` is the most expensive call in varPro, so the wrapper
accepts a pre-computed `ivarpro_fit` for reuse across views.

```{r boston-ivarpro-fit, cache=TRUE}
iv_boston <- varPro::ivarpro(v_boston)
```

```{r boston-gg-ivarpro-distribution}
plot(gg_ivarpro(v_boston, ivarpro_fit = iv_boston))
```

```{r boston-gg-ivarpro-which-obs}
plot(gg_ivarpro(v_boston, ivarpro_fit = iv_boston, which_obs = 1L))
```

The distribution view tells you which variables drive predictions
*across* observations. The per-observation view answers the same
question for a specific case — useful for explaining one prediction
back to whoever asked.
````

Render. Verify all of Section 3 renders cleanly. Commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 3 — gg_ivarpro on Boston (regression section done)"
```

---

## Task 4: Section 4 — Classification (iris)

**Files:**
- Modify: `vignettes/varpro.qmd`

Section 4 covers binary and multi-class on iris with the **core three-beat** only: `gg_varpro(conditional = TRUE)`, `gg_partial_varpro`, `gg_beta_varpro`. The advanced trio (`gg_udependent`, `gg_isopro`, `gg_ivarpro`) was demonstrated in §3 and is not repeated here — `gg_udependent` and `gg_isopro` are family-agnostic; `gg_ivarpro` is expensive and adds no new pedagogical content on iris.

- [ ] **Step 1: Append the section header and the two iris fits**

Append:

````markdown
# Classification: iris

Iris is a small data set — 150 rows, four predictors, three response
classes — and that's a feature here, not a flaw: every figure renders in
under a second, and the structure is well-understood enough that any
strange behaviour stands out. Two fits: a binary problem (drop *setosa*,
positive class = *virginica*) and the full three-class problem.

```{r iris-fit-binary}
iris_binary <- iris[iris$Species != "setosa", ]
iris_binary$Species <- droplevels(iris_binary$Species)
set.seed(20260527L)
v_iris_binary <- varPro::varpro(Species ~ ., data = iris_binary, ntree = 50)
```

```{r iris-fit-multi}
set.seed(20260527L)
v_iris_multi <- varPro::varpro(Species ~ ., data = iris, ntree = 50)
```
````

- [ ] **Step 2: Add `gg_varpro(conditional = TRUE)` on multi-class**

Append:

````markdown
## Class-conditional importance with `gg_varpro(conditional = TRUE)`

For a classification forest, `gg_varpro()` can split the importance
view into one facet per class. Variables keep the unconditional sort
order so rows line up across facets — read along a row to see which
class a variable is informative for.

```{r iris-multi-gg-varpro-conditional}
plot(gg_varpro(v_iris_multi, conditional = TRUE))
```
````

Render, commit each sub-step.

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 4 — iris fits + gg_varpro conditional"
```

- [ ] **Step 3: Add `gg_partial_varpro()` on the multi-class fit**

Append:

````markdown
## Partial dependence: `gg_partial_varpro()` on classification

`gg_partial_varpro()` works the same way on a classification fit, but
each predictor's curve becomes per-class probabilities. The plot
panels are organised by predictor with class encoded as colour or
facet (the wrapper defaults handle this).

```{r iris-multi-gg-partial-varpro}
plot(gg_partial_varpro(v_iris_multi))
```

Read each panel as: "as predictor X changes from low to high, how does
the probability of each class shift?" Patterns that match the
underlying biology (e.g. petal length separating setosa from the
others) act as a sanity check on the forest.
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 4 — gg_partial_varpro on iris multi-class"
```

- [ ] **Step 4: Add binary and multi-class `gg_beta_varpro()`**

Append:

````markdown
## Per-class lasso refinement with `gg_beta_varpro()`

On a classification fit, `gg_beta_varpro()` returns one row per
(variable, class) pair. For a binary fit, `which_class = NULL` defaults
to the last factor level — the positive class — so the headline view
is a single panel of that class.[^mortality]

[^mortality]: The same code pattern applies to clinical binary outcomes
such as 30-day mortality: drop the negative class, set the event class
as the last factor level, and read the figure for the event panel.

```{r iris-binary-beta-fit, cache=TRUE}
b_iris_binary <- varPro::beta.varpro(v_iris_binary)
```

```{r iris-binary-gg-beta-varpro}
plot(gg_beta_varpro(v_iris_binary, beta_fit = b_iris_binary))
```

For a multi-class fit, the default view is faceted by class with each
class sharing the row order set by the unified ranking — same trick as
`gg_varpro(conditional = TRUE)`.

```{r iris-multi-beta-fit, cache=TRUE}
b_iris_multi <- varPro::beta.varpro(v_iris_multi)
```

```{r iris-multi-gg-beta-varpro}
plot(gg_beta_varpro(v_iris_multi, beta_fit = b_iris_multi))
```

`which_class = "<level>"` collapses the faceted view to a single class
when you want it.
````

Render. Commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 4 — gg_beta_varpro on iris binary + multi"
```

---

## Task 5: Section 5 — Survival (PBC)

**Files:**
- Modify: `vignettes/varpro.qmd`

This section is explicitly partial: `gg_beta_varpro` and `gg_ivarpro` are not available for survival fits. Be honest about it.

- [ ] **Step 1: Append the section header and the PBC fit**

Append:

````markdown
# Survival: PBC

Survival is the family where the varPro toolchain shows its limits. The
forest-fitting and the family-agnostic wrappers all work; the
lasso-refined and individual-importance wrappers don't, because the
underlying `varPro::beta.varpro()` and `varPro::ivarpro()` calls don't
support survival fits in the current release.

```{r pbc-data}
data(pbc, package = "randomForestSRC")
pbc_small <- pbc[, c("days", "status", "age", "albumin", "bili",
                     "edema", "platelet")]
pbc_small <- na.omit(pbc_small)
set.seed(20260527L)
v_pbc <- varPro::varpro(
  survival::Surv(days, status) ~ .,
  data = pbc_small, ntree = 50
)
```
````

- [ ] **Step 2: Add survival `gg_varpro()`**

Append:

````markdown
## Variable importance: `gg_varpro()`

```{r pbc-gg-varpro}
plot(gg_varpro(v_pbc))
```
````

- [ ] **Step 3: Add survival `gg_partial_varpro()` C-path**

Append:

````markdown
## Partial dependence: `gg_partial_varpro()` (C-path)

For survival, `gg_partial_varpro()` dispatches into the
`randomForestSRC`-backed partial-dependence machinery via the underlying
`$rf` survival forest — what the package calls the *C-path*. The output
shape is the same as the regression case (parametric / non-parametric /
causal panels); the y-axis carries an `ensemble mortality` interpretation
(Ishwaran 2008), not a survival probability.

```{r pbc-gg-partial-varpro}
plot(gg_partial_varpro(v_pbc))
```
````

- [ ] **Step 4: Add `gg_isopro()` on the X-matrix**

Append:

````markdown
## Anomaly scoring: `gg_isopro()` on the X-matrix

Because `isopro()` only sees the predictor matrix, it doesn't care
about the family. The same call from section 3 works here.

```{r pbc-isopro, cache=TRUE}
iso_pbc <- varPro::isopro(data = pbc_small[, c("age", "albumin", "bili",
                                                "platelet")],
                          method = "rnd", sampsize = 256, ntree = 50)
plot(gg_isopro(iso_pbc))
```
````

- [ ] **Step 5: Add the non-coverage callout**

Append:

````markdown
## Not available for survival: `gg_beta_varpro`, `gg_ivarpro`

`varPro::beta.varpro()` errors on survival fits in the current release
(it only supports `regr` and `class`). `gg_ivarpro()` for survival is
similarly deferred pending design work on the per-rule risk-scaling
story. Both are tracked for v2.9.0.

If you call either on a survival fit you'll get a clear error message
pointing at the deferred work, not a silent miscalculation. The
family-support matrix in §6 records this — and the rest of the toolkit
that *does* work on survival (`gg_varpro`, `gg_partial_varpro`,
`gg_isopro` above; `gg_udependent` shown in §3 on the X-matrix).
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 5 — survival PBC with explicit non-coverage callout"
```

---

## Task 6: Section 6 — Cross-cutting reference

**Files:**
- Modify: `vignettes/varpro.qmd`

This section is terse register throughout: a matrix, three short subsections, no narrative.

- [ ] **Step 1: Append the family-support matrix**

Append:

````markdown
# Cross-cutting reference

## Family-support matrix

| Wrapper | regr | class | surv | regr+ |
|---|---|---|---|---|
| `gg_partial_varpro` | ✓ | ✓ | ✓ (C-path via `$rf`) | ✗ (not audited) |
| `gg_varpro` | ✓ | ✓ (`conditional = TRUE`) | ✓ | ✗ (errors) |
| `gg_udependent` | ✓ (uvarpro on X) | ✓ (X) | ✓ (X) | ✓ (X) |
| `gg_isopro` | ✓ (X) | ✓ (X) | ✓ (X) | ✓ (X) |
| `gg_beta_varpro` | ✓ | ✓ | ✗ (upstream stop) | ✗ (deferred) |
| `gg_ivarpro` | ✓ | ✓ | ✗ (deferred) | ✗ (deferred) |

The four wrappers in the lower-right are the v2.9.0 work surface.
````

- [ ] **Step 2: Append the factor-level ordering note**

Append:

````markdown
## Factor-level ordering

Across `gg_beta_varpro()` and `gg_ivarpro()`, the `variable` column is
stored as a factor whose levels are set by descending aggregate
importance (`mean(|imp|)` summed across classes for classification).
The default plot inherits that ordering, so faceted views show
variables in the same row order across panels. If you re-shape the
frame downstream and want the order preserved, keep `variable` as a
factor rather than coercing to character.

This convention will be propagated to `gg_vimp` and
`plot.gg_varpro(conditional = TRUE)` in a follow-up release.
````

- [ ] **Step 3: Append the caching note**

Append:

````markdown
## Caching the expensive calls

`varPro::beta.varpro()` and `varPro::ivarpro()` are the two heavy
calls. Both wrappers accept a pre-computed fit (`beta_fit`,
`ivarpro_fit`) so you can iterate on selection, observation index, or
cutoff without re-fitting the lasso or the local-importance machinery.
The vignette uses this throughout — every section computes the heavy
fit once in a `cache: true` chunk and re-uses it for every figure.

Provenance carries `precomputed = TRUE` when the cached path was used,
so downstream tooling can tell the two paths apart.
````

- [ ] **Step 4: Append the provenance-shape note**

Append:

````markdown
## Provenance shape

`attr(., "provenance")$cutoff` is always a *named numeric vector*
across the toolkit:

- regression: length 1, named `"regr"`
- classification: length K, named with the response factor levels

Downstream code that picks a value should read it as a vector
(`prov$cutoff[[class_name]]` or `prov$cutoff[[1]]`), not as a scalar.
This contract was established in v2.7.3.9012 (PR #98).
````

Render, commit:

```bash
git add vignettes/varpro.qmd
git commit -m "vignette: section 6 — cross-cutting reference"
```

---

## Task 7: Section 7 — Further reading + bibliography

**Files:**
- Modify: `vignettes/varpro.qmd`
- Modify: `vignettes/ggRandomForests.bib` (if Lu & Ishwaran 2024 not already present)

- [ ] **Step 1: Check bib entries**

```bash
grep -E "^@.*\{Lu2024|Ishwaran2007|Ishwaran2008" vignettes/ggRandomForests.bib
```

If `Lu2024varpro` is missing, append to `vignettes/ggRandomForests.bib`:

```bibtex
@article{Lu2024varpro,
  author  = {Lu, M. and Ishwaran, H.},
  title   = {Model-independent variable selection via the rule-based variable priority},
  journal = {arXiv preprint},
  year    = {2024},
  url     = {https://arxiv.org/abs/2409.09003}
}
```

(If the published-journal citation has appeared by the time this PR lands, prefer that over the arXiv preprint.)

- [ ] **Step 2: Append the further-reading section**

Append to `vignettes/varpro.qmd`:

````markdown
# Further reading

The release-rule framework is laid out in @Lu2024varpro. The ensemble
mortality scale used by survival partial dependence comes from
@Ishwaran2008rsf. Each wrapper's help page carries a "What this is
doing" section that goes one level deeper than this vignette.

# References
````

(The `# References` heading is consumed by quarto's bibliography
renderer — leave it as a single line.)

- [ ] **Step 3: Render. Verify the bibliography section appears at the end with the cited entries.**

```bash
R -q -e 'quarto::quarto_render("vignettes/varpro.qmd")' 2>&1 | tail -5
```

- [ ] **Step 4: Commit**

```bash
git add vignettes/varpro.qmd vignettes/ggRandomForests.bib
git commit -m "vignette: section 7 — further reading + bibliography entries"
```

---

## Task 8: pkgdown navigation + ggRandomForests.qmd pointer + final NEWS + check + PR

**Files:**
- Modify: `_pkgdown.yml`
- Modify: `vignettes/ggRandomForests.qmd`
- Modify: `NEWS.md` (refine the placeholder bullet from Task 0)

- [ ] **Step 1: Add the vignette to the pkgdown Articles navigation**

Read the current `_pkgdown.yml` Articles section:

```bash
grep -nB1 -A15 "^articles:" _pkgdown.yml
```

In the articles list, add an entry for the new vignette. The likely shape (verify against the existing convention):

```yaml
articles:
  - title: ~
    contents:
    - ggRandomForests
    - ggRandomForests-regression
    - ggRandomForests-survival
    - varpro
```

(If `_pkgdown.yml` uses a `desc:` per-article pattern, follow that convention.)

- [ ] **Step 2: Add a one-line pointer from `vignettes/ggRandomForests.qmd`**

Find the existing "Next steps" section in `vignettes/ggRandomForests.qmd` and append a single sentence:

```markdown
For the full varPro toolkit (release-rule importance, lasso-refined
importance, per-observation local importance, anomaly scores, and the
dependency graph) walked across regression, classification, and
survival examples, see `vignette("varpro")`.
```

- [ ] **Step 3: Refine the NEWS bullet (if needed)**

Inspect the placeholder bullet from Task 0. If polishing is warranted (the language was already final-quality, so usually no change), refine in place. Do NOT move the bullet from the top of the development section.

- [ ] **Step 4: Final R CMD check + pkgdown build**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -10
R -q -e 'pkgdown::build_site(lazy = TRUE)' 2>&1 | tail -10
```

Both must be clean. The pkgdown build is local-only; do NOT commit `docs/`.

- [ ] **Step 5: Push and open PR**

```bash
git add _pkgdown.yml vignettes/ggRandomForests.qmd NEWS.md
git commit -m "docs: pkgdown nav + ggRandomForests.qmd pointer + final NEWS"

git push -u origin feat/phase5-varpro-vignette

gh pr create --title "Phase 5: consolidated varPro vignette" --body "$(cat <<'EOF'
## Summary
- New `vignettes/varpro.qmd` — one consolidated vignette walking the full `gg_*` varPro layer (`gg_partial_varpro`, `gg_varpro`, `gg_udependent`, `gg_isopro`, `gg_beta_varpro`, `gg_ivarpro`) across three worked examples: regression (Boston), classification (iris binary + multi-class), and survival (PBC).
- Honest non-coverage callout in §5: `gg_beta_varpro` and `gg_ivarpro` don't support survival yet.
- Family-support matrix in §6 documents which wrapper works for which forest family — doubles as the v2.9.0 work surface.
- pkgdown Articles navigation updated; `ggRandomForests.qmd` overview points at the new vignette.
- Live knit with chunk-level cache, `ntree = 50` across all three fits. Cold-cache CRAN-runner knit estimated < 3 min.

Spec: `dev/plans/2026-05-27-phase5-varpro-vignette-design.md`
Plan: `dev/plans/2026-05-27-phase5-varpro-vignette-plan.md`

## Test plan
- [ ] `devtools::check(--as-cran)` 0/0/0 with the new vignette in the build
- [ ] `quarto::quarto_render("vignettes/varpro.qmd")` succeeds from a clean cache
- [ ] `pkgdown::build_site()` lands the vignette in Articles without warnings
- [ ] Family-support matrix matches empirical reality (verified during brainstorm against varPro 3.1.0)

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Return the PR URL.

---

## Self-review

**Spec coverage (revised structure):**

| Spec requirement | Task |
|---|---|
| Section 1 "What varPro is" | T2 |
| Section 2 "Setup" | T1 step 3 |
| Section 3 Regression Boston — core three-beat + advanced trio (only place those three appear) | T3 |
| Section 3 step order: `gg_varpro` → `gg_partial_varpro` → `gg_beta_varpro` → `gg_udependent` → `gg_isopro` → `gg_ivarpro` | T3 steps 2–7 |
| Section 4 Classification iris — core three-beat only | T4 |
| Section 4 step order: `gg_varpro(conditional)` → `gg_partial_varpro` → `gg_beta_varpro` | T4 steps 2–4 |
| Section 5 Survival PBC — `gg_varpro` + `gg_partial_varpro` (C-path) + `gg_isopro` + explicit non-coverage | T5 |
| Section 6 Cross-cutting reference + family-support matrix | T6 |
| Section 7 Further reading + bibliography | T7 |
| Bibliography entries (Lu & Ishwaran 2024, Ishwaran 2008) | T7 step 1 |
| pkgdown Articles nav | T8 step 1 |
| `ggRandomForests.qmd` pointer | T8 step 2 |
| NEWS bullet | T0 step 3 + T8 step 3 |
| Voice from scratch (no audit pass) | T1 step 1 (read fingerprint) + applied throughout |
| Live knit with knitr cache + ntree = 50 | T1 step 3 (setup chunk) + each fit chunk uses `cache: true` |
| Family-support matrix empirically verified | Lifted from spec; matches T6 step 1 verbatim |
| 30-day mortality footnote only | T4 step 4 (single footnote at the binary `gg_beta_varpro` block) |
| `gg_udependent` + `gg_ivarpro` appear once (only in §3) | T3 steps 5, 7 (no T4 / T5 step for these) |
| `gg_isopro` appears in §3 and §5 (family-agnostic demonstration) | T3 step 6 + T5 step 4 |

**Placeholder scan:** none. Every chunk has working R code; every prose block has finished text (not "TBD"). The bibliography entry has a real `arXiv` URL; if a published-journal citation supersedes it before the PR lands, the engineer should update.

**Type consistency:** `v_boston`, `v_iris_binary`, `v_iris_multi`, `v_pbc` are the fit names used consistently. `b_*` for `beta.varpro` results, `iv_*` for `ivarpro` results, `iso_*` for `isopro` results, `u_*` for `uvarpro` results. The same names are referenced across sub-tasks within a section.

**Render cadence:** every commit step ends with a `quarto::quarto_render(...)` — if the cache hits, this is sub-second; cold-cache it's bounded by the per-section heavy chunks. The engineer should not commit a section that hasn't rendered.

**Cache rationale:** the `varpro` fits are short (ntree = 50, small data); `beta.varpro` and `ivarpro` are the actual expensive calls. Caching at the chunk level means iteratively rewriting prose around a single figure costs nothing.

---

## Out-of-scope follow-ups flagged for the next sub-projects

- Voice audit on `-regression.qmd` and `-survival.qmd` (separate PR — they still carry the "Work in progress" callout).
- CRAN audit cleanup PR (`\dontrun` → `\donttest` in `gg_brier.R` / `plot.gg_brier.R`; chatty `message()` gating in `surv_partial.rfsrc.R`).
- Factor-level alignment for `gg_vimp` and `plot.gg_varpro(conditional = TRUE)` (consistency follow-up).
- v2.8.0 release candidate gate after the three follow-ups land.
