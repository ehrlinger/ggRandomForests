# Release Checklist: ggRandomForests v4.0.0

**Audit date:** 2026-08-25
**Integration branch:** `dev_rhf`
**Release status:** HOLD

This checklist records the v3/v4 consistency sweep and later RHF release
gates. It does not authorize a release, CRAN submission, tag, version change,
or merge to `main`. Those actions require explicit maintainer approval. CRAN
acceptance remains the final release condition.

## Canonical metadata

Audit dispositions record what happened to a finding: `corrected` means the
surface was changed in this sweep; `retained` means the existing language is
correct and remains in place; and `deferred` means the work has a stated future
boundary.

Gate statuses record whether a required check can advance: `pending` means it
has not yet been completed, `verified` means current evidence satisfies it, and
`blocked` means a stated issue prevents it from advancing.

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
| [x] | README | Standardized the three fit-call/object-class mappings; corrected the varPro object class to `varpro`; kept supported minima separate from current software citations; added the three implemented RHF families and their references. | corrected | `README.md`; final-review mapping correction and fit-call/version/citation inventories on 2026-08-25. |
| [x] | Package help | Added the three engine mappings, RHF families, dependency gates, and current software and method citations. The first mention now pairs every qualified fit call with its object class and attaches each supported minimum to its package. | corrected | `R/help.R`, generated `man/ggRandomForests-package.Rd`; final-review mapping correction on 2026-08-25. |
| [x] | Roxygen citations | Replaced stale RF-SRC and varPro software citations; added RHF software citations to the extractors; removed obsolete v3.1.0 promises from the beta/local-importance notes. | corrected | Scoped `R/` sources; generated `man/gg_beta_varpro.Rd` and `man/gg_ivarpro.Rd`; documentation regeneration on 2026-08-25. |
| [x] | Generated help | Regenerated the Rd pages corresponding to the edited roxygen sources. | corrected | `man/`; `devtools::document()` on 2026-08-25. |
| [x] | Six vignettes | Corrected software citation callouts and current unsupported-family prose; retained unqualified `rfsrc()` calls only where randomForestSRC is explicitly attached. The main vignette now states the three mappings before naming an upstream fit. | corrected | All six sources audited; `varpro.qmd` and `ggRandomForests.qmd` rendered after Task 5 and final-review changes on 2026-08-25. |
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
| [x] | Documentation changes reviewed against the canonical table | verified | Final-review mapping inventory confirmed all three qualified fit calls, classes, and package-attached minima; stale-pattern searches returned no hits. |
| [x] | Spelling and prose review | verified | Fresh `spelling::spell_check_package(use_wordlist = TRUE)` exited 0 on 2026-08-25. Its reported project terminology, package/API names, citations, and regional spellings were reviewed; no genuine spelling errors or source changes. |
| [x] | Documentation regeneration and lint | verified | Fresh `devtools::document()` exited 0, then `lintr::lint_package()` reported no lints on 2026-08-25. |
| [x] | Six vignette renders | verified | Fresh `quarto::quarto_render()` rendered all six `vignettes/*.qmd` files with exit 0 on 2026-08-25, using an isolated temporary `HOME` and the existing R library. |
| [x] | Guarded tests | verified | After the second Copilot review, fresh `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` exited 0: 1,648 passes, 0 failures, 59 existing warnings, and 6 documented skips on 2026-08-25. |
| [x] | Snapshot integrity | verified | Scoped `git status --short tests/testthat/_snaps` before and after the fresh guarded suite, plus the scoped name-status diff after, were empty; no vdiffr baseline changed. |
| [x] | pkgdown | verified | Fresh `pkgdown::build_site()` used an isolated temporary `HOME`. Its first attempt was DNS-blocked at Google Fonts; the approved network-enabled retry exited 0. Rendered site artifacts were not staged. |
| [x] | Clean-archive check | verified | After Copilot review, a fresh clean `git archive HEAD` build exited 0 with the isolated temporary `HOME`; the first check was DNS-blocked at CRAN/Bioconductor indexes, and the approved retry of `R CMD check --as-cran` exited 0 with 0 errors, 0 warnings, and 1 NOTE. The earlier citation NOTE is resolved. The remaining incoming-feasibility NOTE reports 4 days since the last update and 8 updates in the preceding 6 months. Tarball checks found only `ggRandomForests/.Rinstignore`, `Version: 4.0.0`, `Date: 2026-08-05`, and 0 `cran-comments` entries. |

## Release gates

| Done | Gate | Disposition | Evidence |
|---|---|---|---|
| [ ] | RHF vignette | pending | Rendered and reviewed RHF article |
| [x] | Consistency sweep | verified | All audit rows have a disposition; fresh documentation, lint, guarded-suite, spelling, vignette, pkgdown, and archive evidence was recorded on 2026-08-25. |
| [ ] | Full release verification | pending | Requires a maintainer-authorized release verification after the remaining RHF vignette and release gates are complete. Fresh consistency-sweep command evidence is recorded above. |
| [ ] | Explicit maintainer authorization | pending | Maintainer approval recorded |
| [ ] | Submission | pending | CRAN submission record |
| [ ] | CRAN acceptance | pending | CRAN acceptance notice |

## Planning-time retention decisions

- `inst/CITATION` is **retained** because it correctly describes how to cite
  ggRandomForests itself; dependency citations belong in the documentation and
  shared bibliography, not in the package's own citation entry.
- Dependency floors are **retained** because they state supported
  compatibility, while this checklist separately states current CRAN versions.

## Verification evidence: 2026-08-25

- The working tree and `tests/testthat/_snaps` were clean before verification.
  The scoped snapshot status and diff remained empty after the guarded suite.
- `devtools::document()`, `lintr::lint_package()`, and
  `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` exited 0 in that
  order. The suite reported 1,646 passes, 0 failures, 59 existing warnings,
  and 6 documented skips.
- All six vignettes and pkgdown rendered successfully with an isolated
  temporary `HOME`. The first pkgdown attempt was blocked only by restricted
  DNS access to Google Fonts; the approved network-enabled retry succeeded.
- The first clean-archive build inherited the known unusable normal Quarto
  Sass cache and failed while rebuilding vignettes. Repeating the same clean
  archive build with the isolated temporary `HOME` succeeded, as did the
  subsequent manual-inclusive check.
- At this stage, `R CMD check --as-cran` reported 0 errors, 0 warnings, and
  1 NOTE asking that the raw varPro arXiv citation in `DESCRIPTION` use its
  DOI form. The later Copilot-review pass resolved that citation NOTE. The RHF
  vignette, explicit authorization, submission, and CRAN-acceptance holds
  remain in force.

## Final-review verification evidence: 2026-08-25

- Final review corrected the first user-facing mention of each upstream mapping
  in package help and the main vignette, corrected the README `varpro` object
  class, and added a separate audit-disposition/gate-status legend.
- With an isolated temporary `HOME` and the existing R library,
  `devtools::document()`, `lintr::lint_package()`, and
  `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` exited 0 in that
  order. The suite reported 1,646 passes, 0 failures, 59 existing warnings,
  and 6 documented skips; snapshot status and its scoped diff were empty
  before and after the suite.
- `spelling::spell_check_package(use_wordlist = TRUE)` and all six
  `quarto::quarto_render()` vignette renders exited 0. The pkgdown build and
  clean-archive check each required an approved retry after their initial DNS
  failures; both retries exited 0. At this stage, `R CMD check --as-cran`
  completed with the manual and reported 0 errors, 0 warnings, and the raw
  varPro arXiv NOTE later resolved during Copilot review.
- Only the consistency-sweep gate remains checked. The RHF-vignette, full
  release-verification, explicit-authorization, submission, and CRAN-
  acceptance gates remain pending.

## Copilot-review verification evidence: 2026-08-25

- `DESCRIPTION` now cites the varPro preprint as
  `<doi:10.48550/arXiv.2409.09003>`, and the README qualifies
  `varPro::uvarpro()` at its first mention and in the varPro overview.
- `devtools::document()`, `lintr::lint_package()`, and the guarded full suite
  exited 0 in that order. The suite reported 1,646 passes, 0 failures,
  59 existing warnings, and 6 documented skips; snapshot status remained
  empty.
- A fresh clean-archive build and manual-inclusive `R CMD check --as-cran`
  completed with 0 errors, 0 warnings, and 1 unrelated incoming-feasibility
  NOTE: 4 days since the last update and 8 updates in the preceding 6 months.
  The former citation NOTE did not recur.
- Only the consistency-sweep gate remains checked. All release,
  authorization, submission, and CRAN-acceptance holds remain pending.

## Second Copilot-review verification evidence: 2026-08-25

- Exact, anchored error expectations now cover both unsupported families,
  `surv` and `regr+`, for `gg_beta_varpro()` and `gg_ivarpro()`.
- Focused runs reported 89 passes for `test_gg_beta_varpro.R` and 53 passes
  with the documented slow-test skip for `test_gg_ivarpro.R`.
- `devtools::document()`, `lintr::lint_package()`, and the guarded full suite
  exited 0 in that order. The full suite reported 1,648 passes, 0 failures,
  59 existing warnings, and 6 documented skips; snapshot status remained
  empty.
- No production or generated documentation changed, so the clean-archive
  result from the preceding review pass remains the PR-level check evidence.
  Only the consistency-sweep gate remains checked; every release,
  authorization, submission, and CRAN-acceptance hold remains pending.
