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
| [x] | DESCRIPTION | Added the three engine roles and current method citations; retained version, date, and dependency floors. | verified | `DESCRIPTION`; documentation regeneration on 2026-08-25. |
| [ ] | `inst/CITATION` | Confirm the package citation describes ggRandomForests itself. | retained | `inst/CITATION` |
| [ ] | README | Reconcile package, dependency, and method citations and version language. | pending | `README.md` |
| [x] | Package help | Added the three engine mappings, RHF families, dependency gates, and current software and method citations. | verified | `R/help.R`, generated `man/ggRandomForests-package.Rd`; documentation regeneration on 2026-08-25. |
| [x] | Roxygen citations | Replaced stale RF-SRC and varPro software citations; added RHF software citations to the extractors. | verified | Scoped `R/` sources; documentation regeneration on 2026-08-25. |
| [x] | Generated help | Regenerated only the Rd pages corresponding to the edited roxygen sources. | verified | `man/`; documentation regeneration on 2026-08-25. |
| [x] | Six vignettes | Corrected RF-SRC and varPro software citation callouts; retained unqualified `rfsrc()` calls where the vignette explicitly attaches randomForestSRC. | verified | Stable software keys; all six sources rendered successfully on 2026-08-25. |
| [x] | Shared bibliography | Corrected RF-SRC software citation and added the varPro software citation. | verified | `Ishwaran:RFSRC:software:2026`, `Ishwaran:varPro:software:2026`, and `Ishwaran:RHF:software:2026` |
| [ ] | v4 NEWS | Record the v4 consistency sweep and RHF support changes. | pending | `NEWS.md` |
| [ ] | Active v3 NEWS | Preserve and reconcile the active v3 release notes. | pending | `NEWS.md` |
| [ ] | Runnable examples | Verify examples run with supported package versions and imports. | pending | Examples and guarded tests |
| [ ] | pkgdown | Verify navigation, reference pages, articles, and rendered links. | pending | pkgdown build |

## Behavioral defect log

| Done | Finding | Disposition | Evidence |
|---|---|---|---|
| [ ] | None found during planning | verified | Planning review |

## PR 1 verification

| Done | Check | Disposition | Evidence |
|---|---|---|---|
| [ ] | Documentation changes reviewed against the canonical table | pending | Diff review |
| [ ] | Spelling and prose review | pending | House-style review |
| [ ] | Lint | pending | `lintr::lint_package()` |
| [ ] | Six vignette renders | pending | Quarto render output |
| [ ] | Guarded tests | pending | `NOT_CRAN=true VDIFFR_RUN_TESTS=true devtools::test()` |
| [ ] | Snapshot integrity | pending | vdiffr baselines and `git status` |
| [ ] | pkgdown | pending | pkgdown build output |
| [ ] | Clean-archive check | pending | `git archive` build/check |

## Release gates

| Done | Gate | Disposition | Evidence |
|---|---|---|---|
| [ ] | RHF vignette | pending | Rendered and reviewed RHF article |
| [ ] | Consistency sweep | pending | Documentation audit complete |
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
