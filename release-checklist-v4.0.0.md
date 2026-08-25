# Release Checklist: ggRandomForests v4.0.0

**Audit date:** 2026-08-25
**Integration branch:** `dev_rhf`
**Release status:** HOLD

This checklist records the v3/v4 consistency sweep and later RHF release
gates. It does not authorize a release, CRAN submission, tag, version change,
or merge to `main`. Those actions require explicit maintainer approval. CRAN
acceptance remains the final release condition.

## Canonical metadata

The dispositions used throughout this checklist are `pending`, `retained`,
`update`, `blocked`, and `verified`.

| Package | Fit call | Object class | Current CRAN | Supported minimum | Software citation | Method citation |
|---|---|---|---:|---:|---|---|
| `randomForestSRC` | `randomForestSRC::rfsrc()` | `rfsrc` | 3.6.2 | 3.4.0 | Ishwaran and Kogalur (2026), *Fast Unified Random Forests for Survival, Regression, and Classification (RF-SRC)* | Ishwaran and Kogalur (2007); Ishwaran et al. (2008) |
| `randomForestRHF` | `randomForestRHF::rhf()` | `rhf` | 1.0.1 | 1.0.1 | Ishwaran and Kogalur (2026), *Random Hazard Forests* | Ishwaran et al. (2026), arXiv:2608.21597 |
| `varPro` | `varPro::varpro()` | `varpro` | 3.2.0 | 3.1.0 | Ishwaran and Kogalur (2026), *Model-Independent Variable Selection via the Rule-Based Variable Priority* | Lu and Ishwaran (2024), arXiv:2409.09003 |

Zhou, Lu, and Ishwaran (2026) remains the specific method citation for
unsupervised variable priority where that method is discussed.

## Documentation audit

| Done | Surface | Finding | Disposition | Evidence |
|---|---|---|---|---|
| [x] | DESCRIPTION | Added the three engine roles and current method citations; retained version, date, and dependency floors. | corrected | `DESCRIPTION`; version/citation inventory and documentation regeneration on 2026-08-25. |
| [x] | `inst/CITATION` | The package citation describes ggRandomForests itself; dependency citations belong on their documentation surfaces. | retained | `inst/CITATION`; Task 5 source audit on 2026-08-25. |
| [x] | README | Standardized the three fit-call/object-class mappings; kept supported minima separate from current software citations; added the three implemented RHF families and their references. | corrected | `README.md`; fit-call and version/citation inventories on 2026-08-25. |
| [x] | Package help | Added the three engine mappings, RHF families, dependency gates, and current software and method citations. | corrected | `R/help.R`, generated `man/ggRandomForests-package.Rd`; fit-call and version/citation inventories on 2026-08-25. |
| [x] | Roxygen citations | Replaced stale RF-SRC and varPro software citations; added RHF software citations to the extractors; removed obsolete v3.1.0 promises from the beta/local-importance notes. | corrected | Scoped `R/` sources; generated `man/gg_beta_varpro.Rd` and `man/gg_ivarpro.Rd`; documentation regeneration on 2026-08-25. |
| [x] | Generated help | Regenerated the Rd pages corresponding to the edited roxygen sources. | corrected | `man/`; `devtools::document()` on 2026-08-25. |
| [x] | Six vignettes | Corrected software citation callouts and current unsupported-family prose; retained unqualified `rfsrc()` calls only where randomForestSRC is explicitly attached. | corrected | All six sources audited; `varpro.qmd` and `ggRandomForests.qmd` rendered after Task 5 changes on 2026-08-25. |
| [x] | Shared bibliography | Corrected RF-SRC software citation and added the varPro software citation. | corrected | `Ishwaran:RFSRC:software:2026`, `Ishwaran:varPro:software:2026`, and `Ishwaran:RHF:software:2026`. |
| [x] | v4 NEWS | Added the current-software/minimum-version distinction and all three canonical fit-call/object-class mappings. | corrected | `NEWS.md`; fit-call and version/citation inventories on 2026-08-25. |
| [x] | Active v3 NEWS | Retained v3.0.0--v3.5.2 as historical release facts; no present-tense instruction conflicts with the canonical mapping or current method/software citations. | retained | `NEWS.md`; Task 4 audit on 2026-08-25. |
| [x] | Runnable examples | Package-qualified fits are used throughout R examples; unqualified README and vignette calls follow explicit package attachment; RHF examples retain Suggests guards. | retained | Direct fitting-call inventory; focused guarded tests on 2026-08-25. |
| [x] | pkgdown | Moved the six implemented RHF topics into their own reference group; no RHF article link was added because the vignette is not yet present. | corrected | `_pkgdown.yml`; Task 4 pkgdown build and Task 5 source audit on 2026-08-25. |
| [x] | RHF tuning family | No current surface advertises `gg_tune_rhf()` before it exists. | deferred | PR 2 owns the tuning family and its documentation. |
| [x] | RHF vignette | Removed the overview's present-tense claim about a vignette that does not yet exist; no article link is published. | deferred | PR 3 owns `vignettes/rhf.qmd`; `vignettes/ggRandomForests.qmd` and `_pkgdown.yml`. |

## Behavioral defect log

| Done | Finding | Disposition | Evidence |
|---|---|---|---|
| [x] | `gg_beta_varpro()` reported that unsupported `regr+` and survival families were "tracked for v3.1.0". It now names the wrapper, supported regression/classification fits, and received family without a release promise. | corrected | RED 1 failure/87 passes; GREEN 88 passes in `test_gg_beta_varpro.R` with `NOT_CRAN=true VDIFFR_RUN_TESTS=true` on 2026-08-25. |
| [x] | `gg_ivarpro()` reported that unsupported `regr+` and survival families were "tracked for v3.1.0". It now names the wrapper, supported regression/classification fits, and received family without a release promise. | corrected | RED 1 failure/51 passes/1 skip; GREEN 52 passes/1 skip in `test_gg_ivarpro.R` with `NOT_CRAN=true VDIFFR_RUN_TESTS=true` on 2026-08-25. |

## PR 1 verification

| Done | Check | Disposition | Evidence |
|---|---|---|---|
| [x] | Documentation changes reviewed against the canonical table | verified | Task 5 fit-call and version/citation inventories; final stale-pattern searches returned no hits. |
| [x] | Spelling and prose review | verified | House-style review across the Task 1--5 documentation diff. |
| [x] | Lint | verified | `lintr::lint_package()`: no lints on 2026-08-25. |
| [x] | Six vignette renders | verified | All six rendered in Task 2; the changed `varpro.qmd` and `ggRandomForests.qmd` rendered again in Task 5. |
| [x] | Guarded tests | verified | `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()`: 1,646 passes, 0 failures, 59 existing warnings, 6 documented skips on 2026-08-25. |
| [x] | Snapshot integrity | verified | Identical scoped `git status` before and after the guarded suite; no vdiffr baseline changed. |
| [x] | pkgdown | verified | Task 4 pkgdown build on 2026-08-25. |
| [ ] | Clean-archive check | pending | `git archive` build/check |

## Release gates

| Done | Gate | Disposition | Evidence |
|---|---|---|---|
| [ ] | RHF vignette | pending | Rendered and reviewed RHF article |
| [x] | Consistency sweep | verified | Task 5 documentation audit complete; all rows are corrected, retained, or deferred to PR 2/PR 3. |
| [ ] | Full release verification | pending | Definition-of-done commands and `R CMD check --as-cran` |
| [ ] | Explicit maintainer authorization | pending | Maintainer approval recorded |
| [ ] | Submission | pending | CRAN submission record |
| [ ] | CRAN acceptance | pending | CRAN acceptance notice |

## Planning-time retention decisions

- `inst/CITATION` is **retained** because it correctly describes how to cite
  ggRandomForests itself; dependency citations belong in the documentation and
  shared bibliography, not in the package's own citation entry.
- Dependency floors are **retained** because they state supported
  compatibility, while this checklist separately states current CRAN versions.
