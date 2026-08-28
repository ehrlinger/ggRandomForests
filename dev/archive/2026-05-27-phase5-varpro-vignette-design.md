# ggRandomForests v2.8.0 — Phase 5: Consolidated varPro vignette

**Date:** 2026-05-27
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Awaiting approval — for inline review before commit
**Sequencing:** Phase 5, after Phase 4d (`gg_ivarpro`, PR #99) merged. Followed by CRAN audit cleanup (`\dontrun` → `\donttest`; chatty `message()` in `surv_partial.rfsrc.R`) and the factor-level alignment follow-up (`gg_vimp` / `plot.gg_varpro(conditional = TRUE)`), then v2.8.0 release candidate.

---

## Goal

Write one consolidated vignette that walks readers from "what varPro is" through the entire `gg_*` varPro layer (`gg_partial_varpro`, `gg_varpro`, `gg_udependent`, `gg_isopro`, `gg_beta_varpro`, `gg_ivarpro`) on three worked examples — regression, classification, and survival — with the v2.8.0 release narrative threaded through. The vignette becomes the headline document for v2.8.0; pkgdown lands users on it before the function reference.

## Scope

Single PR. One new vignette file `vignettes/varpro.qmd`. No code changes to the package functions. Voice written **from scratch** to the current `~/Documents/ObsidianVault/memory/writing-voice.md` fingerprint — no post-hoc voice audit pass needed.

---

## Empirical family-support matrix

Verified locally against `varPro 3.1.0`:

| Wrapper | regr | class | surv | regr+ |
|---|---|---|---|---|
| `gg_partial_varpro` | ✓ | ✓ | ✓ (C-path via `$rf`) | ✗ (not audited; deferred) |
| `gg_varpro` | ✓ | ✓ (`conditional = TRUE`) | ✓ | ✗ (errors empirically) |
| `gg_udependent` | ✓ (uvarpro on X) | ✓ (X) | ✓ (X) | ✓ (X) |
| `gg_isopro` | ✓ (X) | ✓ (X) | ✓ (X) | ✓ (X) |
| `gg_beta_varpro` | ✓ | ✓ | ✗ (`varPro::beta.varpro` stops upstream) | ✗ (deferred) |
| `gg_ivarpro` | ✓ | ✓ | ✗ (deferred) | ✗ (deferred) |

This matrix lives in section 5 (Cross-cutting reference) of the vignette. It doubles as the v2.9.0 roadmap hint.

---

## Vignette structure

The vignette teaches the same three-beat pedagogical core
(importance → effect → refined importance) on three families, then
visits the family-agnostic / specialty wrappers exactly once in the
regression section. Survival deliberately surfaces a missing capability
rather than papering over it.

```
vignettes/varpro.qmd  (~estimated 500-750 lines)

1. What varPro is                          (~200-300 words)
   - The problem with permutation importance
   - Release rules (Lu & Ishwaran, 2024)
   - What the gg_* wrappers add: tidy + ggplot

2. Setup                                   (~one chunk)
   library() calls, helper, knitr cache config

3. Regression: Boston housing              (~headline section)
   varpro(medv ~ ., Boston, ntree = 50) on a fixed seed.
   Core three-beat:
   ├─ gg_varpro()              per-tree importance distribution
   ├─ gg_partial_varpro()      partial dependence
   └─ gg_beta_varpro()         per-rule lasso β refinement
   Advanced trio (shown ONCE in the vignette, here):
   ├─ gg_udependent()          uvarpro dependency graph (ggraph-gated)
   ├─ gg_isopro()              anomaly scores
   └─ gg_ivarpro()             local importance, distribution + which_obs

4. Classification: iris                    (binary + multi-class)
   varpro(Species ~ ., iris[!=setosa]) → binary
   varpro(Species ~ ., iris)             → 3-class
   Same core three-beat, family-aware:
   ├─ gg_varpro(conditional = TRUE)
   ├─ gg_partial_varpro()       (classification path)
   └─ gg_beta_varpro()          binary which_class default + multi-class facets
   footnote on the binary gg_beta_varpro block: clinical binary
   outcomes like 30-day mortality follow this pattern

5. Survival: PBC                           (honest about partial toolchain)
   varpro(Surv(days, status) ~ ., pbc, ntree = 50)
   ├─ gg_varpro()              survival family — works
   ├─ gg_partial_varpro()      C-path via $rf (Phase 1 tested)
   ├─ gg_isopro()              isopro on X — family-agnostic; second
   │                           appearance demonstrates that it survives
   │                           the family change
   └─ explicit non-coverage:   gg_beta_varpro errors upstream on surv;
                               gg_udependent + gg_ivarpro deferred —
                               see §6 family-support matrix

6. Cross-cutting reference
   - Family-support matrix (the table above)
   - Factor-level ordering convention (descending importance)
   - Caching the expensive calls (beta_fit, ivarpro_fit)
   - Provenance shape contract (cutoff always a named numeric vector)

7. Further reading
   - Lu & Ishwaran (2024)
   - Phase 1-4 release notes
   - Function reference index
```

**Design rationale for the asymmetric coverage:**

- **The advanced trio (`gg_udependent`, `gg_isopro`, `gg_ivarpro`) appears once, in the regression section.** `gg_udependent` operates on the X-matrix via `uvarpro()` — family-agnostic; one demonstration is enough. `gg_ivarpro` is expensive enough that running it on three datasets in one vignette doubles the cold-cache knit time without teaching anything new. `gg_isopro` is the exception: it gets a second appearance in §5 specifically to demonstrate that it doesn't care about the response family. That repetition is pedagogy, not duplication.
- **`gg_beta_varpro` is in §3 and §4 but not §5** because the upstream `varPro::beta.varpro()` errors on survival fits. The §5 callout names this explicitly.
- **`gg_partial_varpro` is in all three** because it has a meaningful family-specific story each time (regression: smooth curves; classification: per-class panels; survival: C-path dispatch into rfsrc-backed partial dependence with the ensemble-mortality scale).

---

## Tooling

- **Format**: Quarto `.qmd`, vignette engine `quarto::html`, matches the three existing vignettes.
- **Front matter**: same template as `vignettes/ggRandomForests-regression.qmd` (TOC, MathJax, bibliography).
- **Bibliography**: `bibliography: ggRandomForests.bib` (existing) plus add entries for Lu & Ishwaran 2024 (varPro) and Ishwaran 2007 (release rules).
- **Knit strategy**: live execution with chunk-level `cache: true`. Each `varpro` / `beta.varpro` / `ivarpro` call is its own chunk so a single rebuild only re-runs what changed.
- **`ntree`**: 50 across all three fits. ivarpro fits reuse the cached varpro fit.
- **Chunk options**: `message: false`, `warning: false`, `fig.width = 7`, `fig.height = 4.5` defaults.

## Voice

Aligned to `~/Documents/ObsidianVault/memory/writing-voice.md`. Written from scratch (not draft-then-audit). Two registers:

- **Narrative** for section intros and the "what varPro is" framing. Conversational, sentences that build on each other, no list-formatting where prose would do.
- **Terse** for code captions, the family-support matrix, error-handling notes, and the cross-cutting reference. Bullets, tables, short labels.

Lifts the pedagogical `@section What this is doing` / `@section What you use this for` text from the Phase 4 roxygen blocks where it fits — those sections were written to be liftable into vignette prose with minimal editing.

**No iris-as-30-day-mortality body framing.** A single footnote in section 4 says: "The same code pattern applies to clinical binary outcomes such as 30-day mortality, with the event class as the rightmost factor level." That's the only clinical signpost in the body.

## Data and runtime

| Section | Data | Source | ntree | Heavy calls | Wall-clock (estimate) |
|---|---|---|---|---|---|
| Regression | `MASS::Boston` (506 × 13 numeric) | already in `Suggests:` (used by gg_ivarpro examples) | 50 | `varpro` + `beta.varpro` + `partialpro` + `uvarpro` + `isopro` + `ivarpro` | < 90 s |
| Classification | `iris` and `iris[!=setosa, drop=TRUE]` | base R `datasets` | 50 each | 2 × (`varpro` + `beta.varpro` + `partialpro`) | < 20 s combined |
| Survival | `randomForestSRC::pbc`, dropped to a small subset (5 continuous predictors, na.omit) | already in `Suggests:` | 50 | `varpro` + `partialpro` (C-path) + `isopro` | < 45 s |

Estimated total cold-cache knit: **~2.5 minutes** (trimmed from the earlier ~3 min estimate because classification no longer carries an `ivarpro` fit and survival drops the `uvarpro` + `ivarpro` calls). Subsequent renders (cache hit): seconds. Stays well under the 5-minute soft cap CRAN flags for vignette build time.

## Files

- **New**: `vignettes/varpro.qmd` (the vignette)
- **Modify**: `vignettes/ggRandomForests.qmd` (one-line "see varPro vignette" pointer in the existing Next-steps section); `vignettes/ggRandomForests.bib` (add Lu & Ishwaran 2024 + Ishwaran 2007 entries — verify if already present); `_pkgdown.yml` (add the new vignette to the Articles navigation); `NEWS.md` (Phase 5 bullet); `DESCRIPTION` (`2.7.3.9014`).
- **Generated (gitignored)**: `vignettes/varpro_cache/`, `vignettes/varpro_files/`, `vignettes/varpro.html`.

## NEWS bullet (draft)

```
* New vignette: "Exploring variable importance with varPro." Walks the
  full gg_* varPro layer (gg_partial_varpro, gg_varpro, gg_udependent,
  gg_isopro, gg_beta_varpro, gg_ivarpro) on three worked examples —
  regression (Boston), classification (iris binary + multi-class), and
  survival (PBC). Includes a family-support matrix documenting which
  wrapper works for which forest family. Headline document for v2.8.0.
```

## Acceptance criteria

- `R -q -e 'quarto::quarto_render("vignettes/varpro.qmd")'` succeeds (verified manually before final commit).
- `R CMD check --as-cran` stays 0 errors / 0 warnings / 0 notes with the new vignette in the build.
- `pkgdown::build_site()` renders the vignette into Articles.
- All six wrappers exercised at least once with live code + figure output.
- Family-support matrix matches empirical reality (the table above is the contract).
- No "Work in progress" callout (the other two domain vignettes still carry one — that's a separate cleanup, not this PR).

## Out of scope

- Voice audit on the existing `-regression` and `-survival` vignettes — separate sub-project; they keep their current "Work in progress" callout.
- Renaming or restructuring the three pre-existing vignettes.
- A vdiffr-style "vignette render test" in `tests/testthat/` — skipped (vignettes already render under `R CMD check`'s vignette build; an explicit test would duplicate that).
- A clinical worked example using a real public clinical data set — kept to a footnote; a separate vignette for clinical-data use cases is a v2.9.0 candidate, not v2.8.0.
- `regr+` audit for `gg_varpro` and `gg_partial_varpro` (deferred; tracked in the family matrix).
- Phase 6 `regr+` and `surv` family support for `gg_beta_varpro` / `gg_ivarpro`.

---

## Sequencing note

After this lands, the v2.8.0 runway is:
- CRAN audit cleanup (`\dontrun` → `\donttest`; chatty `message()` gating in `surv_partial.rfsrc.R`)
- Factor-level alignment follow-up (`gg_vimp` / `plot.gg_varpro(conditional = TRUE)`)
- v2.8.0 release candidate gate
