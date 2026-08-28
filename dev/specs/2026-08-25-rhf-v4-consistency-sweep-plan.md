# RHF v4 Consistency Sweep Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Establish and enforce one current package/function/object/citation
contract for the randomForestSRC, varPro, and randomForestRHF documentation,
then record the release-gate evidence for PR 1 into `dev_rhf`.

**Architecture:** A source-first documentation audit backed by a small textual
regression test. Canonical metadata lives in `release-checklist-v4.0.0.md`;
user-facing wording is corrected in README, package help, current NEWS,
vignettes, roxygen, and pkgdown, while `man/` is regenerated rather than edited.
The sweep records behavioral defects but does not fix them.

**Tech Stack:** R 4.4+, testthat edition 2, roxygen2, Quarto, pkgdown, lintr,
spelling, Git, and official CRAN/arXiv records.

**Spec:**
`dev/plans/2026-08-25-rhf-v4-consistency-tuning-vignette-design.md`

## Global Constraints

- Work in place on a `codex/` branch cut from current `dev_rhf`; do not create
  an isolated worktree.
- Open this work as PR 1 of 3 against `dev_rhf`.
- Do not push to `main`, merge to `main`, release, submit to CRAN, tag, or change
  `Version: 4.0.0`.
- Leave `DESCRIPTION` dependency floors at `randomForestSRC (>= 3.4.0)`,
  `varPro (>= 3.1.0)`, and `randomForestRHF (>= 1.0.1)`. A current CRAN version
  is not, by itself, a reason to raise a compatibility floor.
- Record current CRAN software versions as `randomForestSRC` 3.6.2,
  `randomForestRHF` 1.0.1, and `varPro` 3.2.0, verified on 2026-08-25.
- Use the canonical mappings
  `randomForestSRC::rfsrc()` -> `rfsrc`,
  `randomForestRHF::rhf()` -> `rhf`, and
  `varPro::varpro()` -> `varpro`.
- On first mention in explanatory prose, use the package-qualified fitting
  function. Unqualified calls in code are valid only after that package is
  explicitly attached in the same document.
- Cite software using the current CRAN citation and methods using the primary
  paper. A supported minimum version and a software citation version are
  different facts and must not be written as though they were interchangeable.
- Preserve older NEWS as history. Audit v4 plus present-tense guidance in the
  v3.0.0--v3.5.2 sections; edit an older entry only if it is a broken citation
  or currently instructs a user to use the wrong package, fit call, or class.
- Follow `.claude/house-style.md` for all prose.
- Do not hand-edit `man/` or `NAMESPACE`; run `devtools::document()` first at
  every full verification gate.
- Do not add a dependency or update the locally installed varPro 3.1.0 during
  this documentation PR.
- If the audit exposes a behavioral defect, add it to the checklist with an
  exact reproduction and stop for maintainer direction before changing code.
- Always run the full suite with both `NOT_CRAN=true` and
  `VDIFFR_RUN_TESTS=true`, checking snapshot status before and after.

---

### Task 1: Create the canonical audit record

**Files:**

- Create: `release-checklist-v4.0.0.md`
- Read: `DESCRIPTION`
- Read: `inst/CITATION`
- Read: `vignettes/ggRandomForests.bib`

**Interfaces:**

- Consumes: the approved design and official CRAN citation pages.
- Produces: the canonical table and disposition vocabulary used by every later
  task in this plan.

- [ ] **Step 1: Reconfirm official metadata before editing.**

Run:

```bash
for pkg in randomForestSRC randomForestRHF varPro; do
  curl -fsSL "https://cran.r-project.org/web/packages/${pkg}/DESCRIPTION" |
    sed -n -e '/^Package:/p' -e '/^Version:/p' -e '/^Date\/Publication:/p'
done
```

Expected versions: randomForestSRC 3.6.2, randomForestRHF 1.0.1, varPro
3.2.0. If CRAN has changed, stop and revise the plan's explicit software
versions before editing documentation.

Run:

```bash
curl -fsSL https://cran.r-project.org/web/packages/randomForestSRC/citation.html
curl -fsSL https://cran.r-project.org/web/packages/randomForestRHF/citation.html
curl -fsSL https://cran.r-project.org/web/packages/varPro/citation.html
curl -fsSL https://export.arxiv.org/api/query?id_list=2608.21597
```

Expected: official software citations name Ishwaran and Kogalur and report
versions 3.6.2, 1.0.1, and 3.2.0; the arXiv record identifies *Random Hazard
Forests* by Ishwaran, Hsich, Kogalur, and Lee.

- [ ] **Step 2: Create the checklist header and release hold.**

Start `release-checklist-v4.0.0.md` with:

```markdown
# Release Checklist: ggRandomForests v4.0.0

**Audit date:** 2026-08-25
**Integration branch:** `dev_rhf`
**Release status:** HOLD

This checklist records the v3/v4 consistency sweep and later RHF release
gates. It does not authorize a release, CRAN submission, tag, version change,
or merge to `main`. Those actions require explicit maintainer approval. CRAN
acceptance remains the final release condition.
```

- [ ] **Step 3: Add the canonical metadata table.**

Use these exact rows:

```markdown
| Package | Fit call | Object class | Current CRAN | Supported minimum | Software citation | Method citation |
|---|---|---|---:|---:|---|---|
| `randomForestSRC` | `randomForestSRC::rfsrc()` | `rfsrc` | 3.6.2 | 3.4.0 | Ishwaran and Kogalur (2026), *Fast Unified Random Forests for Survival, Regression, and Classification (RF-SRC)* | Ishwaran and Kogalur (2007); Ishwaran et al. (2008) |
| `randomForestRHF` | `randomForestRHF::rhf()` | `rhf` | 1.0.1 | 1.0.1 | Ishwaran and Kogalur (2026), *Random Hazard Forests* | Ishwaran et al. (2026), arXiv:2608.21597 |
| `varPro` | `varPro::varpro()` | `varpro` | 3.2.0 | 3.1.0 | Ishwaran and Kogalur (2026), *Model-Independent Variable Selection via the Rule-Based Variable Priority* | Lu and Ishwaran (2024), arXiv:2409.09003 |
```

Below it, state that Zhou, Lu, and Ishwaran (2026) remains the specific method
citation for unsupervised variable priority where that method is discussed.

- [ ] **Step 4: Add audit and verification sections.**

Create four tables/sections with unchecked boxes:

1. `Documentation audit`, with columns `Surface`, `Finding`, `Disposition`,
   `Evidence` and rows for DESCRIPTION, inst/CITATION, README, package help,
   roxygen/help, six vignettes, shared bibliography, v4 NEWS, active v3 NEWS,
   runnable examples, and pkgdown.
2. `Behavioral defect log`, initially containing `None found during planning`.
3. `PR 1 verification`, listing document, spelling, lint, six vignette renders,
   guarded tests, snapshot integrity, pkgdown, and clean-archive check.
4. `Release gates`, listing the RHF vignette, consistency sweep, full release
   verification, explicit maintainer authorization, submission, and CRAN
   acceptance. Leave every release-gate box unchecked.

- [ ] **Step 5: Record the two planning-time retention decisions.**

Mark:

- `inst/CITATION` as **retained** because it correctly describes how to cite
  ggRandomForests itself; dependency citations belong in the documentation and
  shared bibliography, not in the package's own citation entry.
- dependency floors as **retained** because they state supported compatibility,
  while the checklist separately states current CRAN versions.

- [ ] **Step 6: Check and commit the audit record.**

Run:

```bash
git diff --check
rg -n "HOLD|randomForestSRC::rfsrc|randomForestRHF::rhf|varPro::varpro|3.6.2|1.0.1|3.2.0|CRAN acceptance" release-checklist-v4.0.0.md
```

Expected: no whitespace errors; every canonical value and hold is present.

Commit:

```bash
git add release-checklist-v4.0.0.md
git commit -m "docs: establish v4 consistency checklist"
```

### Task 2: Pin dependency and citation consistency with tests

**Files:**

- Create: `tests/testthat/test_documentation_consistency.R`
- Modify: `vignettes/ggRandomForests.bib`
- Modify: `vignettes/ggRandomForests-classification.qmd`
- Modify: `vignettes/ggRandomForests-regression.qmd`
- Modify: `vignettes/ggRandomForests-survival.qmd`
- Modify: `vignettes/ggRandomForests.qmd`
- Modify: `vignettes/varpro.qmd`
- Modify: `vignettes/uvarpro.qmd`

**Interfaces:**

- Consumes: the version and citation table from Task 1.
- Produces: stable BibTeX keys and a regression test later tasks extend.

- [ ] **Step 1: Write the dependency-floor and bibliography tests.**

Create `tests/testthat/test_documentation_consistency.R` with this source-path
helper and the first two tests:

```r
.consistency_path <- function(...) {
  testthat::test_path("..", "..", ...)
}

.read_consistency_text <- function(...) {
  paste(readLines(.consistency_path(...), warn = FALSE), collapse = "\n")
}

test_that("upstream dependency floors remain explicit", {
  desc <- read.dcf(.consistency_path("DESCRIPTION"),
                   fields = c("Imports", "Suggests"))

  expect_match(desc[1, "Imports"],
               "randomForestSRC \\(>= 3\\.4\\.0\\)")
  expect_match(desc[1, "Imports"], "varPro \\(>= 3\\.1\\.0\\)")
  expect_match(desc[1, "Suggests"],
               "randomForestRHF \\(>= 1\\.0\\.1\\)")
})

test_that("shared bibliography identifies current upstream software", {
  bib <- .read_consistency_text("vignettes", "ggRandomForests.bib")

  expect_match(bib, "@manual\\{Ishwaran:RFSRC:software:2026,")
  expect_match(bib, "R package version 3\\.6\\.2")
  expect_match(bib, "@manual\\{Ishwaran:RHF:software:2026,")
  expect_match(bib, "R package version 1\\.0\\.1")
  expect_match(bib, "@manual\\{Ishwaran:varPro:software:2026,")
  expect_match(bib, "R package version 3\\.2\\.0")
  expect_match(bib, "10\\.48550/arXiv\\.2608\\.21597")
  expect_false(grepl("Ishwaran:RFSRC:2014", bib, fixed = TRUE))
})
```

- [ ] **Step 2: Run the focused test and verify the expected red state.**

Run:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_consistency.R")'
```

Expected: the dependency-floor test passes; the bibliography test fails because
the RF-SRC key is stale and the varPro software entry is absent.

- [ ] **Step 3: Replace and add the software BibTeX entries.**

Replace `@manual{Ishwaran:RFSRC:2014` with:

```bibtex
@manual{Ishwaran:RFSRC:software:2026,
  author = {Hemant Ishwaran and Udaya B. Kogalur},
  title  = {Fast Unified Random Forests for Survival, Regression, and Classification ({RF-SRC})},
  year   = {2026},
  note   = {R package version 3.6.2},
  url    = {https://cran.r-project.org/package=randomForestSRC}
}
```

Add:

```bibtex
@manual{Ishwaran:varPro:software:2026,
  author = {Hemant Ishwaran and Udaya B. Kogalur},
  title  = {Model-Independent Variable Selection via the Rule-Based Variable Priority},
  year   = {2026},
  note   = {R package version 3.2.0},
  url    = {https://cran.r-project.org/package=varPro}
}
```

Retain `Lu2024varpro`, `Ishwaran:RHF:2026`, and
`Ishwaran:RHF:software:2026` as the separate method/software records.

- [ ] **Step 4: Update every vignette citation callout to the stable keys.**

- Replace `@Ishwaran:RFSRC:2014` with
  `@Ishwaran:RFSRC:software:2026` in classification, regression, and survival.
- Cite `@Ishwaran:varPro:software:2026` alongside `@Lu2024varpro` where varPro
  is first introduced in `ggRandomForests.qmd`, `varpro.qmd`, and `uvarpro.qmd`.
- Add `bibliography: ggRandomForests.bib` to the main vignette's YAML before
  adding its first citation; the other five vignettes already declare it.
- In the main vignette introduction, name all three mappings exactly:
  `randomForestSRC::rfsrc()` -> `rfsrc`, `varPro::varpro()` -> `varpro`, and
  `randomForestRHF::rhf()` -> `rhf`. Keep the RHF discussion to an overview;
  the worked longitudinal analysis belongs to PR 3.
- Retain unqualified `rfsrc()` calls in the three RF-SRC worked vignettes
  because each explicitly attaches randomForestSRC before the call. Record
  that disposition in the checklist.

- [ ] **Step 5: Run the focused test and render all six existing vignettes.**

Run:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_consistency.R")'
Rscript -e 'for (v in Sys.glob("vignettes/*.qmd")) quarto::quarto_render(v)'
```

Expected: two passing tests and six successful renders. Do not stage rendered
HTML, `_files`, `.quarto`, or cache artifacts.

- [ ] **Step 6: Update the checklist and commit.**

Record the shared bibliography and six vignettes as **corrected**, with the
stable keys and render result as evidence.

Commit:

```bash
git add tests/testthat/test_documentation_consistency.R \
  vignettes/ggRandomForests.bib \
  vignettes/ggRandomForests-classification.qmd \
  vignettes/ggRandomForests-regression.qmd \
  vignettes/ggRandomForests-survival.qmd \
  vignettes/ggRandomForests.qmd vignettes/varpro.qmd \
  vignettes/uvarpro.qmd release-checklist-v4.0.0.md
git commit -m "docs: align upstream software citations"
```

### Task 3: Align DESCRIPTION, package help, and roxygen citations

**Files:**

- Modify: `DESCRIPTION`
- Modify: `R/help.R`
- Modify: `R/gg_error.R`
- Modify: `R/plot.gg_error.R`
- Modify: `R/plot.gg_rfsrc.R`
- Modify: `R/plot.gg_roc.R`
- Modify: `R/plot.gg_variable.R`
- Modify: `R/plot.gg_vimp.R`
- Modify: `R/gg_isopro.R`
- Modify: `R/gg_rhf.R`
- Modify: `R/gg_auct.R`
- Modify: `R/gg_rhf_importance.R`
- Modify: `R/plot.gg_rhf_importance.R`
- Modify generated help under: `man/`
- Modify: `tests/testthat/test_documentation_consistency.R`

**Interfaces:**

- Consumes: canonical software titles/versions and the method/software split.
- Produces: consistent package-level and function-level help; generated Rd is
  consumed by the final package and pkgdown checks.

- [ ] **Step 1: Extend the test to reject stale roxygen citation language.**

Append:

```r
test_that("roxygen uses current software citations", {
  paths <- Sys.glob(.consistency_path("R", "*.R"))
  txt <- paste(vapply(paths, .read_consistency_text, character(1)),
               collapse = "\n")

  expect_false(grepl("R package version >= 3.4.0", txt, fixed = TRUE))
  expect_false(grepl("R package version 3.x", txt, fixed = TRUE))
  expect_match(txt, "R package version 3\\.6\\.2")
  expect_match(txt, "R package version 3\\.2\\.0")
  expect_match(txt, "R package version 1\\.0\\.1")
})
```

Because `.read_consistency_text()` currently accepts path components, adjust it
to accept a single existing path as well:

```r
.read_consistency_text <- function(...) {
  parts <- list(...)
  path <- if (length(parts) == 1L && file.exists(parts[[1]])) {
    parts[[1]]
  } else {
    do.call(.consistency_path, parts)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
```

- [ ] **Step 2: Run the focused test and verify it fails on stale citations.**

Run the guarded focused-test command from Task 2.

Expected: the new test fails on the seven `R package version >= 3.4.0`
references and `gg_isopro.R`'s `R package version 3.x` reference.

- [ ] **Step 3: Update DESCRIPTION without changing versions or floors.**

Expand `Description:` so it names all three current engines and their roles:

- randomForestSRC/randomForest for survival, regression, and classification;
- varPro for rule-based variable priority; and
- suggested randomForestRHF for hazards with time-dependent covariates.

Add Lu and Ishwaran (2024), `<arXiv:2409.09003>`, and Ishwaran et al. (2026),
`<doi:10.48550/arXiv.2608.21597>`, to the method sentence. Leave `Version:`,
`Date:`, Imports, and Suggests versions unchanged.

- [ ] **Step 4: Update package help.**

In `R/help.R`:

- name the three canonical mappings on first mention;
- add RHF and all three implemented RHF families (`gg_rhf()`, `gg_auct()`,
  `gg_rhf_importance()`) to the family overview;
- state that randomForestRHF is Suggests-gated while varPro is in Imports;
- replace the RF-SRC minimum-version pseudo-citation with the official 3.6.2
  software citation;
- add the varPro 3.2.0 software citation plus Lu and Ishwaran (2024); and
- add the randomForestRHF 1.0.1 software citation plus Ishwaran et al. (2026).

Do not advertise `gg_tune_rhf()` or the RHF vignette before their later PRs.

- [ ] **Step 5: Replace stale function-page software citations.**

In `R/gg_error.R`, `R/plot.gg_error.R`, `R/plot.gg_rfsrc.R`,
`R/plot.gg_roc.R`, `R/plot.gg_variable.R`, and `R/plot.gg_vimp.R`, replace:

```text
randomForestSRC: Random Forests for Survival, Regression and Classification.
R package version >= 3.4.0.
```

with the official software citation:

```text
Ishwaran H, Kogalur U (2026). Fast Unified Random Forests for Survival,
Regression, and Classification (RF-SRC). R package version 3.6.2.
```

Keep any separate `@param` or description statement saying support begins at
3.4.0.

In `R/gg_isopro.R`, replace the 2025 three-author `R package version 3.x`
entry with the official Ishwaran and Kogalur (2026) varPro 3.2.0 software
citation. Keep the Isolation Forest method citation.

Audit the four listed RHF roxygen files and retain the 1.0.1 software citation
and arXiv method citation where already correct. Add the software citation to
`gg_rhf()` and `gg_auct()` if they currently contain only the method citation;
avoid duplicating it on a plot page that already links to the extractor's help.

- [ ] **Step 6: Regenerate help and rerun the focused test.**

Run:

```bash
Rscript -e 'devtools::document()'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_consistency.R")'
```

Expected: documentation completes without warnings and all three consistency
tests pass.

- [ ] **Step 7: Inspect generated changes and commit.**

Run:

```bash
git diff --check
git status --short man NAMESPACE
```

Expected: only Rd files corresponding to edited roxygen change; NAMESPACE is
unchanged.

Update the checklist dispositions for DESCRIPTION, package help, roxygen, and
generated help, then commit:

```bash
git add DESCRIPTION R/help.R R/gg_error.R R/plot.gg_error.R \
  R/plot.gg_rfsrc.R R/plot.gg_roc.R R/plot.gg_variable.R \
  R/plot.gg_vimp.R R/gg_isopro.R R/gg_rhf.R R/gg_auct.R \
  R/gg_rhf_importance.R R/plot.gg_rhf_importance.R man \
  tests/testthat/test_documentation_consistency.R \
  release-checklist-v4.0.0.md
git commit -m "docs: align package and function citations"
```

### Task 4: Align README, current NEWS, and pkgdown navigation

**Files:**

- Modify: `README.md`
- Modify: `NEWS.md`
- Modify: `_pkgdown.yml`
- Modify: `tests/testthat/test_documentation_consistency.R`
- Modify: `release-checklist-v4.0.0.md`

**Interfaces:**

- Consumes: canonical mappings and the currently implemented RHF API.
- Produces: the public overview and navigation baseline used by the tuning and
  vignette PRs.

- [ ] **Step 1: Add the overview contract test.**

Append:

```r
test_that("current overviews name the three fitting contracts", {
  overview <- c(
    .read_consistency_text("README.md"),
    .read_consistency_text("R", "help.R"),
    .read_consistency_text("vignettes", "ggRandomForests.qmd")
  )
  mappings <- c(
    "randomForestSRC::rfsrc()",
    "randomForestRHF::rhf()",
    "varPro::varpro()"
  )

  for (txt in overview) {
    expect_true(all(vapply(mappings, grepl, logical(1), x = txt,
                           fixed = TRUE)))
  }

  readme <- overview[[1]]
  expect_false(grepl("Eight of the nineteen", readme, fixed = TRUE))
  expect_match(readme, "gg_rhf\\(\\)")
  expect_match(readme, "gg_auct\\(\\)")
  expect_match(readme, "gg_rhf_importance\\(\\)")
})
```

- [ ] **Step 2: Run the focused test and verify the expected failure.**

Run the guarded focused-test command from Task 2.

Expected: failure because README and the overview vignette do not yet name the
RHF fitting contract and README still says `Eight of the nineteen`.

- [ ] **Step 3: Update README.**

- Replace the opening engine paragraphs with one compact mapping paragraph for
  `randomForestSRC::rfsrc()`/`rfsrc`, `varPro::varpro()`/`varpro`, and
  `randomForestRHF::rhf()`/`rhf`.
- Preserve the three supported minimum versions separately from citations.
- Remove the fragile `Eight of the nineteen` count rather than replacing it
  with another count that Phase 4 immediately changes.
- Add a `Random Hazard Forests` function-reference subsection listing
  `gg_rhf()`, `gg_auct()`, and `gg_rhf_importance()` with `rhf` input.
- Add current software references for RF-SRC 3.6.2, varPro 3.2.0, and
  randomForestRHF 1.0.1, plus the Lu/Ishwaran and RHF method papers.
- Add a v4 development highlight for the three implemented RHF families. Do
  not link to the not-yet-created RHF vignette or mention `gg_tune_rhf()`.

- [ ] **Step 4: Update NEWS within the approved historical boundary.**

Add one v4 bullet stating that the consistency sweep now distinguishes current
CRAN software versions from supported minima and standardizes the three
package-qualified fitting calls and object classes. Do not change line 2 or the
v4 heading.

Read the v3.0.0--v3.5.2 sections. Retain historical statements about what a
specific release changed. Change only present-tense instructions that conflict
with the canonical mapping or current method/software citation, recording each
retention or correction in the checklist.

- [ ] **Step 5: Give RHF its own pkgdown reference group.**

Move `gg_rhf`, `plot.gg_rhf`, `gg_auct`, `plot.gg_auct`,
`gg_rhf_importance`, and `plot.gg_rhf_importance` from `Survival Analysis` to a
new `Random Hazard Forests` reference section. Do not add an articles-menu link
until `vignettes/rhf.qmd` exists in PR 3.

- [ ] **Step 6: Run focused checks.**

Run:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_consistency.R")'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_ggrandomforests_news.R")'
Rscript -e 'pkgdown::build_site()'
```

Expected: consistency and NEWS tests pass; pkgdown builds without missing-topic
or missing-article warnings.

- [ ] **Step 7: Update the checklist and commit.**

Record README, v4 NEWS, active v3 NEWS, and pkgdown as corrected or retained,
with reasons. Commit:

```bash
git add README.md NEWS.md _pkgdown.yml \
  tests/testthat/test_documentation_consistency.R \
  release-checklist-v4.0.0.md
git commit -m "docs: align v3 and v4 package overviews"
```

### Task 5: Complete the exhaustive source and example audit

**Files:**

- Audit: `R/*.R`
- Audit: `vignettes/*.qmd`
- Audit: `README.md`, `NEWS.md`, `DESCRIPTION`, `inst/CITATION`, `_pkgdown.yml`
- Modify only confirmed documentation mismatches in those files
- Modify generated help under: `man/`
- Modify: `release-checklist-v4.0.0.md`

**Interfaces:**

- Consumes: all corrected primary surfaces from Tasks 1--4.
- Produces: a complete disposition matrix and no unresolved documentation
  mismatch inside the approved boundary.

- [ ] **Step 1: Generate the fit-call inventory.**

Run:

```bash
rg -n --glob '*.R' --glob '*.qmd' --glob '*.md' --glob '*.yml' \
  'randomForestSRC|randomForestRHF|varPro|rfsrc\(|rhf\(|varpro\(' \
  DESCRIPTION README.md NEWS.md _pkgdown.yml R vignettes
```

Classify every relevant hit using these exact rules:

- package, fitting function, and class agree with the canonical table;
- a first prose mention is qualified;
- an unqualified vignette code call is retained only when the package is
  explicitly attached earlier;
- helper calls such as `partial.rfsrc()`, `importance.rhf()`, and
  `beta.varpro()` are not misidentified as fitting functions; and
- old NEWS event descriptions stay historical unless they are current user
  instructions.

- [ ] **Step 2: Generate the version and citation inventory.**

Run:

```bash
rg -n --glob '*.R' --glob '*.qmd' --glob '*.bib' --glob '*.md' \
  '3\.4\.0|3\.6\.2|3\.1\.0|3\.2\.0|1\.0\.1|2409\.09003|2608\.21597|R package version' \
  DESCRIPTION README.md NEWS.md R vignettes
```

For each hit, label it in the checklist as supported minimum, current software
version, method citation, historical release fact, or mismatch. Correct only
the mismatch category.

- [ ] **Step 3: Audit runnable examples against attachment rules.**

For every `@examples` block and executable vignette chunk that calls
`rfsrc()`, `rhf()`, or `varpro()`:

- retain a package-qualified call; or
- retain an unqualified call only when the package is attached in that example
  or document; and
- retain RHF `requireNamespace()` guards because randomForestRHF is Suggests.

Do not alter behavior, forest size, seeds, or snapshot inputs during this
documentation audit.

- [ ] **Step 4: Apply remaining documentation-only corrections.**

Edit only findings proven by Steps 1--3. If any finding requires behavioral R
code to change, do not edit it; put the file, call, observed behavior, expected
behavior, and reproduction in the checklist's behavioral defect log, then stop
and report it to the maintainer.

- [ ] **Step 5: Regenerate help and validate the audit boundary.**

Run:

```bash
Rscript -e 'devtools::document()'
git diff --check
rg -n 'Ishwaran:RFSRC:2014|R package version >= 3\.4\.0|R package version 3\.x|Eight of the nineteen' R README.md vignettes
```

Expected: documentation succeeds; the final search returns no hits.

- [ ] **Step 6: Finish the audit matrix and commit.**

Every row in `Documentation audit` must now say **corrected**, **retained**, or
**deferred**, with a concrete reason and file evidence. `Deferred` is valid only
for work outside this PR, such as the tuning family or RHF vignette, and must
name PR 2 or PR 3.

Commit only if this task changed files:

```bash
git add DESCRIPTION README.md NEWS.md _pkgdown.yml R man vignettes \
  release-checklist-v4.0.0.md
git commit -m "docs: complete v4 consistency audit"
```

### Task 6: Run the PR verification gates and record evidence

**Files:**

- Modify: `release-checklist-v4.0.0.md`
- Modify only if spelling finds genuine errors: affected prose files and
  `inst/WORDLIST`

**Interfaces:**

- Consumes: the complete documentation sweep.
- Produces: reproducible evidence that PR 1 is ready for review, without
  authorizing a release.

- [ ] **Step 1: Record snapshot state before verification.**

Run:

```bash
git status --short
git status --short tests/testthat/_snaps
```

Expected: no deleted snapshots and no unrelated working-tree changes.

- [ ] **Step 2: Run the definition of done in order.**

Run each command separately:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
```

Expected: document clean; zero lints; zero test failures and errors. Record the
test pass/warning/skip totals in the checklist. Existing known warnings are not
silently relabeled as success; compare them with the pre-PR baseline.

- [ ] **Step 3: Confirm snapshot integrity after the suite.**

Run:

```bash
git status --short tests/testthat/_snaps
git diff --name-status -- tests/testthat/_snaps
```

Expected: no snapshot deletions or modifications; this PR changes no plots.

- [ ] **Step 4: Run spelling, all vignette renders, and pkgdown.**

Run:

```bash
Rscript -e 'print(spelling::spell_check_package(use_wordlist = TRUE))'
Rscript -e 'for (v in Sys.glob("vignettes/*.qmd")) quarto::quarto_render(v)'
Rscript -e 'pkgdown::build_site()'
```

Expected: no genuine spelling errors, six successful vignette renders, and a
successful pkgdown build. Add true technical terms to `inst/WORDLIST`; correct
actual misspellings in their source files. Do not stage rendered artifacts. If
this step changes source or creates `inst/WORDLIST`, rerun document, lint, and
the guarded suite, then commit only those corrections with
`git commit -m "docs: correct consistency sweep spelling"` before continuing.

- [ ] **Step 5: Run the clean-archive check with the manual.**

After all source changes are committed, run:

```bash
consistency_check_dir=$(mktemp -d /tmp/ggrf-v4-consistency.XXXXXX)
git archive HEAD | tar -x -C "$consistency_check_dir"
(cd "$consistency_check_dir" && R CMD build .)
(cd "$consistency_check_dir" && R CMD check --as-cran ggRandomForests_4.0.0.tar.gz)
tar tzf "$consistency_check_dir/ggRandomForests_4.0.0.tar.gz" | grep -E '/\.[^/]+'
tar xzf "$consistency_check_dir/ggRandomForests_4.0.0.tar.gz" -O ggRandomForests/DESCRIPTION | sed -n '4,5p'
tar tzf "$consistency_check_dir/ggRandomForests_4.0.0.tar.gz" | grep -c cran-comments
```

Expected: check completes with zero errors and zero warnings; any NOTE is
quoted and dispositioned in the checklist; the hidden-file command reports
only `ggRandomForests/.Rinstignore`; DESCRIPTION reports version 4.0.0 and the
unchanged development date; the cran-comments count is 0.

- [ ] **Step 6: Record evidence without lifting release holds.**

Check the completed PR 1 verification items and add command dates/results.
Mark the consistency-sweep release gate complete only when every audit row has
a disposition and every verification command passed. Leave the RHF vignette,
explicit release authorization, submission, and CRAN acceptance gates
unchecked.

- [ ] **Step 7: Commit the verification record.**

Run:

```bash
git diff --check
git status --short
git add release-checklist-v4.0.0.md
git commit -m "docs: record v4 consistency verification"
```

All source corrections must already have been committed before the archive
check, so this commit contains only the checklist evidence. Never stage
pkgdown, Quarto, check, tarball, or temporary artifacts.

### Task 7: Review, push, and open PR 1 into `dev_rhf`

**Files:** none unless review finds an issue.

**Interfaces:**

- Consumes: a clean, verified consistency branch.
- Produces: PR 1 for maintainer and Copilot review; no merge.

- [ ] **Step 1: Perform a final scope and release-hold review.**

Run:

```bash
git diff --stat origin/dev_rhf...HEAD
git diff --check origin/dev_rhf...HEAD
git log --oneline origin/dev_rhf..HEAD
rg -n 'Release status: HOLD|CRAN acceptance|explicit maintainer' release-checklist-v4.0.0.md
```

Expected: only the design, plan, checklist, documentation, generated help, and
documentation-consistency test are present; release holds remain explicit.

- [ ] **Step 2: Request code review before publishing.**

Use `superpowers:requesting-code-review` to review the full diff against the
approved design and this plan. Resolve correctness findings, rerun the
proportional focused checks, and repeat Task 6 if any user-facing source
changes.

- [ ] **Step 3: Push the branch and open the PR.**

Use the branch name `codex/rhf-v4-consistency-sweep`. If the local planning
branch still has its earlier name, rename it before the first push:

```bash
git branch -m codex/rhf-v4-consistency-sweep
git push -u origin codex/rhf-v4-consistency-sweep
gh pr create --base dev_rhf \
  --title "docs: complete v4 consistency sweep" \
  --body "PR 1 of the approved RHF v4 sequence. Standardizes the randomForestSRC::rfsrc()/rfsrc, randomForestRHF::rhf()/rhf, and varPro::varpro()/varpro mappings; updates current software and method citations; records the release gates; and adds a documentation consistency guard. This PR does not authorize a release, CRAN submission, tag, or merge to main."
```

- [ ] **Step 4: Stop after PR creation.**

Report the PR URL, commit range, exact verification results, any retained
NOTEs/warnings, and the still-open release gates. Do not merge the PR. Address
subsequent review in a separate review pass using
`superpowers:receiving-code-review`.

## Self-Review

**Spec coverage:** canonical mapping and current/minimum versions -> Tasks 1--4;
software and method citations -> Tasks 1--3; DESCRIPTION/CITATION/README ->
Tasks 1, 3, 4; six vignettes and shared bibliography -> Task 2; roxygen,
generated help, and runnable examples -> Tasks 3 and 5; v4 and active-v3 NEWS
boundary -> Task 4; pkgdown -> Task 4; corrected/retained/deferred audit record ->
Tasks 1 and 5; behavioral-defect stop rule -> Global Constraints and Task 5;
document/spelling/lint/vignettes/tests/pkgdown/clean archive -> Task 6; PR into
`dev_rhf` with release hold -> Task 7.

**Placeholder scan:** No unfinished implementation markers or unspecified
error-handling steps remain. The audit's discovery steps have exact commands,
classification rules, allowed dispositions, and stop conditions. PR 2 and PR 3
are named only as explicit deferral boundaries from the approved design.

**Type/name consistency:** The plan uses the approved mappings throughout:
`randomForestSRC::rfsrc()` -> `rfsrc`, `randomForestRHF::rhf()` -> `rhf`, and
`varPro::varpro()` -> `varpro`. Current software versions and supported minima
remain distinct in every task.
