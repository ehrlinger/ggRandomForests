# Release Checklist: ggRandomForests v4.0.0

**Audit date:** 2026-08-25
**Integration branch:** `main`
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

Verification sections are chronological records of separate runs. Their pass
totals are run-specific: later test additions can increase the total without
invalidating the earlier evidence.

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
| [x] | pkgdown | Moved the eight implemented RHF topics into their own reference group and added the RHF article to the tutorials index and navbar. | corrected | `_pkgdown.yml`; Task 4 source integration on 2026-08-25. Task 5 owns the fresh pkgdown build. |
| [x] | RHF tuning family | Published supplied-object-only tree-size tuning inspection by OOB risk or OOB iAUC, with the selected-size marker and conditional iAUC standard-error ribbon. | corrected | `R/gg_tune_rhf.R`, `R/plot.gg_tune_rhf.R`, generated `man/gg_tune_rhf.Rd` and `man/plot.gg_tune_rhf.Rd`, `tests/testthat/test_gg_tune_rhf.R`, `tests/testthat/test_plot_gg_tune_rhf.R`, and `tests/testthat/test_snapshots.R`; PR 2 verification on 2026-08-25. |
| [x] | RHF vignette | Added and published the longitudinal four-family RHF article. | corrected | `vignettes/rhf.qmd`; Tasks 2 and 3 rendered the article with `Rscript -e 'quarto::quarto_render("vignettes/rhf.qmd")'` and completed focused artifact review; commits `b31097c4` and `06d939ae`; `_pkgdown.yml`, `README.md`, and `R/help.R`. |

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
| [x] | Guarded tests | verified | Fresh `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` exited 0 after the second Copilot review. Exact run-specific totals are recorded in the chronological verification sections below. |
| [x] | Snapshot integrity | verified | Scoped `git status --short tests/testthat/_snaps` before and after the fresh guarded suite, plus the scoped name-status diff after, were empty; no vdiffr baseline changed. |
| [x] | pkgdown | verified | Fresh `pkgdown::build_site()` used an isolated temporary `HOME`. Its first attempt was DNS-blocked at Google Fonts; the approved network-enabled retry exited 0. Rendered site artifacts were not staged. |
| [x] | Clean-archive check | verified | After Copilot review, a fresh clean `git archive HEAD` build exited 0 with the isolated temporary `HOME`; the first check was DNS-blocked at CRAN/Bioconductor indexes, and the approved retry of `R CMD check --as-cran` exited 0 with 0 errors, 0 warnings, and 1 NOTE. The earlier citation NOTE is resolved. The remaining incoming-feasibility NOTE reports 4 days since the last update and 8 updates in the preceding 6 months. Tarball checks found only `ggRandomForests/.Rinstignore`, `Version: 4.0.0`, `Date: 2026-08-05`, and 0 `cran-comments` entries. |

## Release gates

| Done | Gate | Disposition | Evidence |
|---|---|---|---|
| [x] | RHF vignette | verified | `vignettes/rhf.qmd` rendered after Tasks 2 and 3 with `Rscript -e 'quarto::quarto_render("vignettes/rhf.qmd")'`; focused review of the complete article was clean at `06d939ae`; public links are in `_pkgdown.yml`, `README.md`, and `R/help.R`. |
| [x] | Consistency sweep | verified | All audit rows have a disposition; fresh documentation, lint, guarded-suite, spelling, vignette, pkgdown, and archive evidence was recorded on 2026-08-25. |
| [ ] | Upstream Linux GCC UBSAN execution | pending | Manually run `upstream-ubsan.yaml` on the release candidate. It builds `randomForestSRC`, `randomForestRHF`, and `varPro` from source with GCC 16 and `-fsanitize=undefined,bounds-strict`, confirms the contained unsupervised `randomForestSRC` finding, and requires the supported `rfsrc`, `varpro`, and RHF paths to remain clean. |
| [ ] | Full release verification | pending | Requires a maintainer-authorized release verification after the remaining release gates are complete. Fresh consistency-sweep command evidence is recorded above. |
| [ ] | Explicit maintainer authorization | pending | Maintainer approval recorded |
| [ ] | Submission | pending | CRAN submission record |
| [ ] | CRAN acceptance | pending | CRAN acceptance notice |

## Planning-time retention decisions

- `inst/CITATION` is **retained** because it correctly describes how to cite
  ggRandomForests itself; dependency citations belong in the documentation and
  shared bibliography, not in the package's own citation entry.
- Dependency floors are **retained** because they state supported
  compatibility, while this checklist separately states current CRAN versions.

## Internal release-candidate topology

- `maint/v3` preserves the v3 line from the last pre-v4 `main` commit. A v3
  correction is made and released there, then forward-ported to the v4 line.
- After v4 integration, `main` is the internal release-candidate channel.
  `hvtiR::install()` therefore installs the v4 candidate from GitHub even while
  CRAN remains on v3.
- `dev_rhf` is **retired** (2026-08-29). It was the pre-integration branch; once
  the v4 work reached `main` it held no commits `main` did not, so the local
  branch, the remote branch, and the `protect dev_rhf` ruleset were removed.
  `main` had already taken over as the candidate channel, per the point above.
  Anything that still points at `dev_rhf` is stale.
- **hvtiR follow-up:** make `hvtiR::status()` and `hvtiR::update()` commit-aware.
  Their current version comparison cannot distinguish two candidate commits
  that both declare `Version: 4.0.0`. Until that work lands, internal testers
  need an explicit reinstall to move between v4 release-candidate commits.
  This tooling follow-up does not replace or advance any ggRandomForests CRAN
  release gate.

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

## PR 2 verification

| Done | Check | Disposition | Evidence |
|---|---|---|---|
| [x] | Saved tuning extractor | verified | `ea7b11f1` added the supplied-object-only `gg_tune_rhf()` family, strict tuning-path validation, five-column result, and provenance. Focused extractor tests reported 42 expectations; the guarded suite reported 1,690 passes, 0 failures, 6 expected skips, and 59 existing warnings. |
| [x] | Tuning plot and S3 companions | verified | `7ac58872`, `84c13837`, and `0ca0efe0` added the selected-size plot marker, conditional finite-SE iAUC ribbon, shared fixtures, and print/summary/autoplot methods. Focused companion tests reported 87 expectations with 0 failures and 0 errors. |
| [x] | Installed upstream integration and snapshot | verified | `875d8252` added the memoised `randomForestRHF::tune.iAUC.rhf()` integration check and `gg-tune-rhf-iauc.svg`. The guarded suite reported 1,722 passes, 0 failures, 6 expected skips, and 59 existing warnings; the new SVG was the only snapshot delta. |
| [x] | CRAN-facing tuning documentation | verified | `R/gg_tune_rhf.R`, `R/plot.gg_tune_rhf.R`, `R/help.R`, `README.md`, `_pkgdown.yml`, `NEWS.md`, and generated help now document retaining the upstream tuning object, the five-column path, selected-size marker, and conditional iAUC ribbon. `devtools::document()` exited 0, `lintr::lint_package()` reported no lints, and the guarded suite reported 1,722 passes, 0 failures, 6 expected skips, and 59 existing warnings. |
| [x] | Fresh PR-level verification | verified | 2026-08-25: `devtools::document()` exited 0; `lintr::lint_package()` reported no lints; `git status --short tests/testthat/_snaps` before and after `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()`, plus its scoped name-status diff, were empty. The suite reported 1,722 passes, 0 failures, 6 expected skips, and 59 existing warnings. A fresh `git archive HEAD` build with an isolated temporary `HOME` and the existing user R library exited 0. Tarball inspections reported only `ggRandomForests/.Rinstignore`, `Version: 4.0.0`, `Date: 2026-08-05`, and 0 `cran-comments` entries. The first `R CMD check --as-cran` was DNS-blocked at CRAN/Bioconductor indexes; the approved network-enabled retry exited 0, including both manuals, with 0 errors, 0 warnings, and 1 NOTE only: incoming feasibility reports 4 days since the last update and 8 updates in the preceding 6 months. |
| [x] | PR 2 snapshot-evidence correction | verified | 2026-08-25: immediately before the fresh guarded full suite, both `git status --short tests/testthat/_snaps` and `git diff --name-status -- tests/testthat/_snaps` were empty. `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` exited 0 in 98.3 seconds with 1,722 passes, 0 failures, 6 expected skips, and 59 existing warnings. Immediately after the suite, both scoped commands were again empty; no vdiffr baseline changed or was deleted. |

## PR 3 verification

| Done | Check | Disposition | Evidence |
|---|---|---|---|
| [x] | RHF artifact and seven vignette renders | verified | 2026-08-25: the tracked `vignettes/rhf_precomputed.rds` was 1,381,824 bytes, below its 1.75 MiB contract. Fresh clean-export renders of all seven Quarto vignettes exited 0; `rhf.qmd` rendered in 4.46 seconds without a live upstream fit, AUC, importance, or tuning calculation. |
| [x] | RHF article and pkgdown integration | verified | 2026-08-25: a fresh clean-export `pkgdown::build_site()` completed in 129.22 seconds and published all seven articles. The RHF article, navigation, two citation targets, 41 local targets, seven figures, and five tables were complete. Page-level and original-resolution figure review found no clipping, overlap, unresolved citation, raw diagnostic output, or hidden setup text. |
| [x] | Package suite and snapshot integrity before archive verification | verified | 2026-08-25: `devtools::document()`, `lintr::lint_package()`, and the guarded full suite exited 0 in that order. The suite reported 1,758 passes, 0 failures, 0 errors, 59 existing warnings, and 6 documented skips. Scoped snapshot status and name-status diff were empty before and immediately after the suite. |
| [x] | Clean-archive artifact test correction | verified | 2026-08-25: the first manual-inclusive check exposed one test-only path assumption. `test_rhf_vignette_assets.R` looked two levels above the copied `<pkg>.Rcheck/tests/testthat` directory, while the verified bundle was present in `00_pkg_src/ggRandomForests/vignettes`. Commit `98539bb5` now checks the repository source path first and the `R CMD check` source-copy path second. The focused guarded artifact test reported 34 passes, 0 failures, 0 errors, 0 warnings, and 0 skips. |
| [x] | Fresh PR 3 archive and manual-inclusive check | verified | 2026-08-25: a new `git archive HEAD` export built all seven vignettes and produced `ggRandomForests_4.0.0.tar.gz` at 3,947,006 bytes, below 5 MiB. Tar inspection found only `ggRandomForests/.Rinstignore` among hidden entries, `Version: 4.0.0`, `Date: 2026-08-05`, 0 `cran-comments` entries, the 1,381,824-byte RHF bundle and source, and built `inst/doc/rhf.html` and `inst/doc/rhf.R`. `R CMD check --as-cran` completed in 171.60 seconds with the PDF and HTML manuals, 0 errors, 0 warnings, and 1 incoming-feasibility NOTE: 4 days since the last update and 8 updates in the preceding 6 months. |
| [x] | Required coverage installed-test correction | verified | 2026-08-26: the live coverage Rout reported the only failure at `test_rhf_vignette_assets.R:11`, with 1,670 passes, 36 existing warnings, and 7 skips. Repository and `R CMD check` source-copy roots each contained `DESCRIPTION`, `vignettes/`, and the 1,381,824-byte bundle. The installed-package test copy contained no source root. Commit `3b6ff7d3` now requires both `DESCRIPTION` and `vignettes/` before enforcing the complete artifact contract, and otherwise skips with an explicit source-only reason. A fresh local `covr::package_coverage()` run in its retained copy exited 0 at 89.98% coverage, with 1,670 passes, 36 warnings, 8 skips, and 0 failures. Checkout snapshot status and diff were empty before and after the run with `VDIFFR_RUN_TESTS=false`. |
| [x] | Portable RHF artifact replacement | verified | 2026-08-26: base R's local file-operations documentation records OS-specific restrictions for `file.rename()` and explicit existing-destination replacement for `file.copy(..., overwrite = TRUE)`. Commit `3a6c9bc0` copies the validated temporary RDS over the tracked target, stops on `FALSE`, verifies equal MD5 sums, and removes the temporary file. Running `Rscript vignettes/precompute_rhf.R` against the existing target exited 0 in 2.26 seconds, left no temporary RDS, and the regenerated bundle passed all 34 artifact assertions. The only object drift was the fit's two runtime measurements; the original tracked RDS was restored byte for byte, so the verification left no artifact delta. |
| [x] | Fresh PR 3 final-fix verification | verified | 2026-08-26: `devtools::document()`, `lintr::lint_package()`, and the guarded full suite exited 0 in order. The suite reported 1,758 passes, 59 existing warnings, 6 documented skips, 0 failures, and 0 errors; snapshot and full-tree status were empty before and after. A fresh clean export produced a 3,945,392-byte tarball with only `ggRandomForests/.Rinstignore` among hidden entries, the correct version and date, no `cran-comments` or nested archive, and the RHF source, artifact, and built article. Manual-inclusive `R CMD check --as-cran` completed in 185.21 seconds with tests, vignette rebuild, and both manuals OK, 0 errors, 0 warnings, and 1 incoming-feasibility NOTE: 5 days since the last update and 8 updates in the preceding 6 months. |

This evidence verifies the implementation-side RHF-vignette gate. The
consistency-sweep gate remains verified. Full release verification, explicit
maintainer authorization, submission, and CRAN acceptance remain pending, and
the overall release status remains **HOLD**. This check does not authorize a
release, CRAN submission, tag, version change, merge to `main`, or closure of
CRAN acceptance.

## RC topology verification evidence: 2026-08-26

- Before recording the internal release-candidate topology,
  `devtools::document()`, `lintr::lint_package()`, and the guarded full suite
  exited 0 in that order. The suite reported 1,766 passes, 0 failures,
  59 existing warnings, and 6 documented skips.
- Scoped snapshot status and name-status diff were empty before and after the
  suite. No vdiffr baseline changed or was deleted.
- This run verifies the documentation-only topology record. It does not
  advance the UBSAN, full release-verification, submission, or CRAN-acceptance
  gates.
