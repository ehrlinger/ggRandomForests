# Documentation Voice Audit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Audit every ggRandomForests documentation surface and rewrite the AI-written portions in John Ehrlinger's own voice, changing how things are said but never what they say.

**Architecture:** Build a written two-register voice fingerprint, classify every doc surface as original / AI / mixed, then rewrite only the AI and mixed surfaces against the fingerprint. Voice-only: no factual or content changes. Ships as one PR in the v2.8.0 cycle, before the release candidate.

**Tech Stack:** R, roxygen2, Quarto vignettes, R CMD check, git.

**Gating:** Execution starts only after PR #91 (`feat/rf-88-multiclass-roc`) merges to `main`. The plan is written now; do not branch until #91 is merged.

**Note on "tests":** This is a prose task, not a code task. There is no failing-test-first cycle. The verification for each task is: `R CMD check` stays clean, roxygen still generates valid `.Rd`, vignettes still render, and the rewritten text passes a re-read against the fingerprint. Each task ends with that verification and a commit.

---

## Reference: the voice fingerprint (full text)

This is the exact content Task 1 writes to both the Vault and the repo. It is the yardstick every rewrite task uses.

```markdown
# John Ehrlinger — Writing Voice Fingerprint

Reference for keeping documentation and prose in a consistent human voice.
Canonical copy lives in the Obsidian vault; package repos hold a synced copy.

## The voice in one line

Pedagogical and conversational: start from something the reader already knows,
build to the new idea with a concrete analogy, and don't be afraid of a little
personality or a slightly imperfect sentence.

## Two registers

**Narrative** (vignettes, README, roxygen @description/@details, methods prose)
- Open from the familiar: "Most readers are familiar with simple linear regression..."
- Carry one concrete analogy through a hard idea (a fruit basket, the blind men
  and the elephant, a "noise-reduction filter").
- Question-headed sections: "Why Cluster?", "Where Do We Stop?".
- First person plural: "in our practice", "we proceed". Address the reader as "you".
- Gloss terms inline in parentheses; scare-quote a piece of jargon the first
  time it appears ("rules", "elbow", "eyeballing").
- Start sentences with But / Yet / Thus / Now when it helps the flow.
- Vary sentence length. A short flat statement after a long one lands well.

**Terse** (roxygen @param/@return, NEWS bullets)
- Compressed, but still plain and concrete, not sterile.
- State the thing; skip the preamble. "Logical; if TRUE, ..." not "This
  argument controls whether...".
- No analogies, no question headers here; the voice shows in word choice.

## Rules

- Em-dashes: use sparingly. Native to the voice and honestly overused. Keep one
  where it earns the pause; otherwise a comma, parentheses, or a full stop.
- Ellipses: an informal-register habit (text, email). Keep them out of package docs.
- Don't overstate. No overselling. Cut "enhanced", "powerful", "seamlessly",
  "robust" (as a brag), "comprehensive". State what the thing does, at its size.
- Imperfection is allowed. Mild redundancy, an occasional long sentence: human
  texture, not an error to scrub. Don't polish to a glassy finish.
- Repetition that teaches is voice, not defect. Restating a concept, or a
  callback structure (state the problems up front, answer each later), is kept.
  Repeat when it clarifies; cut repetition that only fills space.

## AI tells to hunt and kill

- Mechanical parallelism: same-shaped sentences with no teaching purpose, lists
  padded to equal length. (Pedagogical repetition is NOT this; preserve it.)
- Flavorlessness: correct but with no analogy, no picture, nothing a person
  would actually say.
- Missing "we"/"you": abstract, authorless prose.
- Forced tricolons: "fast, simple, and reliable".
- Hedge-free sterile balance: "X. However, Y. It is worth noting Z."
- Overstatement (see Rules).
- "in order to", "it is important to note", "leverage", "utilize".

Punctuation density is NOT a tell. The tells are structural.

## Before / after

AI:   "Set per_class = TRUE to enable the computation of per-class one-vs-rest
       ROC curves, providing enhanced flexibility for multi-class analysis."
John: "Set per_class = TRUE and a multi-class forest gives you one ROC curve
       per class, each class scored against all the others."
```

---

## Task 1: Branch, build the fingerprint, sync both copies

**Files:**
- Create: `~/Documents/ObsidianVault/memory/writing-voice.md` (canonical)
- Create: `dev/voice-fingerprint.md` (repo copy)

- [ ] **Step 1: Confirm PR #91 is merged, then branch from main**

```bash
git fetch origin
git log origin/main --oneline -5   # confirm the #91 merge commit is present
git checkout -b docs/voice-audit origin/main
```

Expected: `Switched to a new branch 'docs/voice-audit'`. If the #91 merge is not in `origin/main`, stop — execution is gated on it.

- [ ] **Step 2: Write the canonical fingerprint to the Vault**

Write the full fingerprint text from the "Reference: the voice fingerprint" section above to `~/Documents/ObsidianVault/memory/writing-voice.md`, verbatim.

- [ ] **Step 3: Write the repo copy**

Write the same content to `dev/voice-fingerprint.md`, with this header line prepended:

```markdown
<!-- Synced copy. Canonical: ~/Documents/ObsidianVault/memory/writing-voice.md -->
```

- [ ] **Step 4: Commit the Vault copy**

```bash
cd ~/Documents/ObsidianVault && git add memory/writing-voice.md && \
  git commit -m "memory: add writing voice fingerprint"
```

- [ ] **Step 5: Commit the repo copy**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git add dev/voice-fingerprint.md
git commit -m "docs: add voice fingerprint (synced from vault)"
```

---

## Task 2: Classification audit

**Files:**
- Create: `dev/voice-audit-classification.md`

- [ ] **Step 1: Gather first-commit dates for every R file**

```bash
for f in R/*.R; do echo "$f: $(git log --reverse --format='%ad' --date=short -- "$f" | head -1)"; done
```

Files first committed in 2026-05 are AI-era; files dating to 2014-2016 are
original-voice. Known AI-era function files: `gg_brier.R`, `gg_partial_varpro.R`,
`gg_varpro.R`, `gg_udependent.R`, and their `plot.*` counterparts;
`print_methods.R`, `summary_methods.R`, `autoplot_methods.R` (PR #75, 2026-05).
Known AI-*modified* (mixed): `gg_roc.R`, `plot.gg_roc.R`, `calc_roc.R`,
`gg_variable.R`, `plot.gg_variable.R` (RF work, 2026-05).

- [ ] **Step 2: Classify the vignettes and top-level docs**

```bash
for f in vignettes/*.qmd README.md NEWS.md; do \
  echo "=== $f ==="; git log --format='%ad %s' --date=short -- "$f" | tail -3; done
```

Vignette narrative bodies are original-voice (JSS-era); only sections that
document AI-era functions are mixed. `NEWS.md` entries under the two
`v2.8.0 (development)` headers are AI-written; older entries are original.
`README.md` body is mostly original; the feature paragraph(s) touched in
2026-05 commits are mixed.

- [ ] **Step 3: Write the classification table**

Create `dev/voice-audit-classification.md` with a table: one row per doc
surface, columns `Surface | Register | Verdict (original/AI/mixed) | Rewrite?`.
"Rewrite? = yes" only for AI and mixed rows. Original rows are recorded as
out-of-scope so the decision is auditable.

- [ ] **Step 4: Commit**

```bash
git add dev/voice-audit-classification.md
git commit -m "docs: voice-audit classification table"
```

---

## Task 3: Rewrite roxygen — varPro function family

**Files:**
- Modify: `R/gg_partial_varpro.R`, `R/plot.gg_partial_varpro.R`,
  `R/gg_varpro.R`, `R/plot.gg_varpro.R`, `R/gg_udependent.R`,
  `R/plot.gg_udependent.R` (roxygen blocks only)

- [ ] **Step 1: Audit each file's roxygen against the fingerprint**

For each file, read the roxygen block (`#'` lines only). Mark every sentence
that hits an AI tell from `dev/voice-fingerprint.md`: mechanical parallelism,
flavorlessness, missing "we"/"you", forced tricolons, overstatement,
"in order to" / "leverage" / "utilize".

- [ ] **Step 2: Rewrite the flagged `@description` and `@details` text (narrative register)**

Rewrite flagged narrative-register blocks. Example transformation:

```
# BEFORE (AI):
#' This function provides a comprehensive interface for extracting variable
#' importance, leveraging the varpro framework to deliver robust results.

# AFTER (John):
#' gg_varpro() pulls the per-tree importance scores out of a varpro fit so you
#' can plot them. Think of varpro as a filter: it scores how much each variable
#' actually moves the prediction, and the noise variables fall out near zero.
```

Keep every factual claim, every cross-reference, every `\code{}` and
`\link{}` intact. Voice only.

- [ ] **Step 3: Rewrite flagged `@param`/`@return` text (terse register)**

Terse register: compressed, plain, no preamble. Example:

```
# BEFORE (AI):
#' @param conditional Logical flag that, when set to TRUE, enables the
#'   computation and rendering of class-conditional importance values.

# AFTER (John):
#' @param conditional Logical; if TRUE, show class-conditional importance
#'   (classification forests only).
```

- [ ] **Step 4: Regenerate Rd and check**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -5
```

Expected: `0 errors | 0 warnings | 0 notes`. Roxygen must still produce valid
`.Rd`. If `@examples` were touched, they must still run.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R R/plot.gg_partial_varpro.R R/gg_varpro.R \
  R/plot.gg_varpro.R R/gg_udependent.R R/plot.gg_udependent.R man/
git commit -m "docs: rewrite varPro family roxygen in John's voice"
```

---

## Task 4: Rewrite roxygen — gg_brier

**Files:**
- Modify: `R/gg_brier.R`, `R/plot.gg_brier.R` (roxygen blocks only)

- [ ] **Step 1: Audit the roxygen in both files against the fingerprint**

Same procedure as Task 3 Step 1. Read `#'` lines, flag AI tells.

- [ ] **Step 2: Rewrite flagged `@description`/`@details` (narrative register)**

Rewrite flagged narrative blocks against the fingerprint. Open from the
familiar where it helps (the Brier score is a forecasting-accuracy idea most
readers have met). Keep all references — Graf et al., the CRPS definition,
`cens.model` options — factually unchanged.

- [ ] **Step 3: Rewrite flagged `@param`/`@return` (terse register)**

Same terse-register procedure as Task 3 Step 3.

- [ ] **Step 4: Regenerate Rd and check**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -5
```

Expected: `0 errors | 0 warnings | 0 notes`.

- [ ] **Step 5: Commit**

```bash
git add R/gg_brier.R R/plot.gg_brier.R man/
git commit -m "docs: rewrite gg_brier roxygen in John's voice"
```

---

## Task 5: Rewrite roxygen — ROC and randomForest surface

**Files:**
- Modify: `R/gg_roc.R`, `R/plot.gg_roc.R`, `R/calc_roc.R`,
  `R/gg_variable.R`, `R/plot.gg_variable.R` (roxygen blocks only)

- [ ] **Step 1: Audit the roxygen in all five files against the fingerprint**

Same procedure as Task 3 Step 1. These files are *mixed* — they have older
original-voice prose plus 2026-05 AI additions (the `per_class`, `panel`,
`oob`, randomForest-classification text). Rewrite only the AI additions; leave
the original prose alone. Use `git blame` on a `#'` line if origin is unclear:

```bash
git blame -L '/^#'"'"'/,/^[^#]/' R/gg_roc.R | head -60
```

- [ ] **Step 2: Rewrite flagged `@description`/`@details` (narrative register)**

Rewrite flagged narrative blocks against the fingerprint.

- [ ] **Step 3: Rewrite flagged `@param`/`@return` (terse register)**

Rewrite flagged terse-register blocks. The `per_class`, `panel`, and `oob`
param text added in PR #88/#91 is the main AI surface here.

- [ ] **Step 4: Regenerate Rd and check**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -5
```

Expected: `0 errors | 0 warnings | 0 notes`.

- [ ] **Step 5: Commit**

```bash
git add R/gg_roc.R R/plot.gg_roc.R R/calc_roc.R R/gg_variable.R \
  R/plot.gg_variable.R man/
git commit -m "docs: rewrite ROC and randomForest roxygen in John's voice"
```

---

## Task 6: Rewrite roxygen — print/summary/autoplot S3 methods

**Files:**
- Modify: `R/print_methods.R`, `R/summary_methods.R`, `R/autoplot_methods.R`
  (roxygen blocks only)

- [ ] **Step 1: Audit the roxygen against the fingerprint**

Same procedure as Task 3 Step 1. These three files are AI-era (PR #75). The
roxygen is largely shared `@rdname` blocks, so the surface is small.

- [ ] **Step 2: Rewrite flagged blocks**

Rewrite flagged narrative and terse blocks against the fingerprint. These
methods are simple; resist padding the description. State what `print()`,
`summary()`, `autoplot()` do for a `gg_*` object and stop.

- [ ] **Step 3: Regenerate Rd and check**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -5
```

Expected: `0 errors | 0 warnings | 0 notes`.

- [ ] **Step 4: Commit**

```bash
git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R man/
git commit -m "docs: rewrite S3 method roxygen in John's voice"
```

---

## Task 6b: Rewrite roxygen — gg_partial family + varpro helper

**Files:**
- Modify: `R/gg_partial_rfsrc.R`, `R/gg_partialpro.R`, `R/surv_partial.rfsrc.R`,
  `R/varpro_feature_names.R` (roxygen blocks only)

This task was added after the Task 2 classification audit found these four
files carry AI-written roxygen (2026-03 onward) — they were missing from the
original Task 3-6 file lists. `gg_partial_rfsrc.R` and `surv_partial.rfsrc.R`
are *mixed* (substantive 2025 `@examples` prose plus 2026 AI edits);
`gg_partialpro.R` and `varpro_feature_names.R` are *AI*.

- [ ] **Step 1: Audit each file's roxygen against the fingerprint**

For each file read the `#'` lines and flag AI tells per `dev/voice-fingerprint.md`.
For the two mixed files, use `git blame` to separate 2025 original prose (leave
it) from 2026 AI prose (rewrite it):

```bash
git blame -- R/gg_partial_rfsrc.R | grep "#'" | head -100
```

- [ ] **Step 2: Rewrite flagged `@description`/`@details` (narrative register)**

Rewrite flagged narrative blocks against the fingerprint. `gg_partialpro()` is
the soft-deprecated shim — its roxygen should plainly say it is superseded by
`gg_partial_varpro()` and stop; do not oversell either function.

- [ ] **Step 3: Rewrite flagged `@param`/`@return` (terse register)**

Same terse-register procedure as Task 3 Step 3.

- [ ] **Step 4: Regenerate Rd and check**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -5
```

Expected: `0 errors | 0 warnings | 0 notes`.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_rfsrc.R R/gg_partialpro.R R/surv_partial.rfsrc.R \
  R/varpro_feature_names.R man/
git commit -m "docs: rewrite gg_partial family roxygen in John's voice"
```

---

## Task 7: Rewrite vignette AI-touched sections

**Files:**
- Modify: `vignettes/ggRandomForests.qmd`, `vignettes/ggRandomForests-regression.qmd`,
  `vignettes/ggRandomForests-survival.qmd` (AI-touched prose only)

- [ ] **Step 1: Identify AI-touched vignette prose**

For each vignette, find prose added in 2026-05 commits (the sections
documenting `gg_brier` and the refactored `gg_partial`/`gg_survival`):

```bash
git log --format='%H %ad %s' --date=short -- vignettes/ggRandomForests-survival.qmd
git show <2026-05-commit-hash> -- vignettes/ggRandomForests-survival.qmd
```

The original JSS-era narrative is out of scope and stays untouched.

- [ ] **Step 2: Rewrite the AI-touched sections (narrative register)**

Rewrite only the flagged sections against the narrative-register fingerprint.
Vignettes are the highest-visibility narrative surface: this is where the
pedagogical openers, analogies, and question-headed sections matter most.
Match the surrounding original prose so the seam is invisible. Do not touch
code chunks, only the prose between them.

- [ ] **Step 3: Render every vignette**

```bash
Rscript -e "quarto::quarto_render('vignettes/ggRandomForests.qmd')"
Rscript -e "quarto::quarto_render('vignettes/ggRandomForests-regression.qmd')"
Rscript -e "quarto::quarto_render('vignettes/ggRandomForests-survival.qmd')"
```

Expected: each renders with no error.

- [ ] **Step 4: Commit**

```bash
git add vignettes/*.qmd
git commit -m "docs: rewrite AI-touched vignette prose in John's voice"
```

---

## Task 8: Rewrite README AI-touched sections

**Files:**
- Modify: `README.md` (AI-touched prose only)

- [ ] **Step 1: Identify AI-touched README prose**

```bash
git log --format='%H %ad %s' --date=short -- README.md
git show <2026-05-commit-hash> -- README.md
```

Badges, install instructions, and the original overview stay untouched. The
feature paragraphs added/edited in 2026-05 are the rewrite surface.

- [ ] **Step 2: Rewrite the flagged paragraphs (narrative register)**

Rewrite against the narrative-register fingerprint. The README opener is the
first impression: it should sound like John explaining the package to a
colleague, not a feature list.

- [ ] **Step 3: Verify README still renders on pkgdown build**

```bash
Rscript -e "pkgdown::build_home_index(pkgdown::as_pkgdown('.'))" 2>&1 | tail -3
```

Expected: builds with no error.

- [ ] **Step 4: Commit**

```bash
git add README.md
git commit -m "docs: rewrite README feature prose in John's voice"
```

---

## Task 9: Rewrite NEWS v2.8.0 entries

**Files:**
- Modify: `NEWS.md` (the two `v2.8.0 (development)` sections only)

- [ ] **Step 1: Audit the v2.8.0 bullets against the terse-register fingerprint**

All bullets under `ggRandomForests v2.8.0 (development) — continued` and
`ggRandomForests v2.8.0 (development)` are AI-written. Entries for v2.7.3 and
earlier are out of scope and stay untouched.

- [ ] **Step 2: Rewrite the v2.8.0 bullets (terse register)**

Rewrite against the terse-register fingerprint: plain, concrete, no
overstatement, no mechanical parallelism. Keep every issue/PR number, every
function name, every factual claim. A NEWS bullet says what changed and why a
user would care, in as few words as that takes.

- [ ] **Step 3: Verify NEWS still parses**

```bash
Rscript -e "devtools::test(filter = 'ggrandomforests_news')" 2>&1 | tail -5
```

Expected: `0 failures` (the NEWS-version test still finds `2.7.3.9006`).

- [ ] **Step 4: Commit**

```bash
git add NEWS.md
git commit -m "docs: rewrite v2.8.0 NEWS entries in John's voice"
```

---

## Task 10: Verification gate and PR

**Files:** none (verification + PR)

- [ ] **Step 1: Full R CMD check**

```bash
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -8
```

Expected: `0 errors | 0 warnings | 0 notes`.

- [ ] **Step 2: Render all vignettes**

```bash
for v in vignettes/*.qmd; do Rscript -e "quarto::quarto_render('$v')" || echo "FAIL: $v"; done
```

Expected: no `FAIL` lines.

- [ ] **Step 3: Re-audit each rewritten doc against the fingerprint**

Re-read every file touched in Tasks 3-9. For each, confirm: no AI tell from
`dev/voice-fingerprint.md` remains, and no factual claim changed versus the
pre-rewrite text. Spot-check with `git diff origin/main -- <file>` that diffs
are voice-only.

- [ ] **Step 4: Em-dash density check**

```bash
git diff origin/main -- R/ vignettes/ README.md NEWS.md | grep '^+' | grep -c '—'
git diff origin/main -- R/ vignettes/ README.md NEWS.md | grep '^-' | grep -c '—'
```

The added-em-dash count should not exceed the removed count. If it does,
revisit the rewrites and apply the "use sparingly" rule.

- [ ] **Step 5: Push and open the PR**

```bash
git push -u origin docs/voice-audit
gh pr create --title "docs: voice audit — rewrite AI-written docs in John's voice" \
  --body "$(cat <<'EOF'
## Summary

- Rewrites all AI-written documentation (roxygen, vignettes, README, NEWS) in
  John's own voice, per the approved spec
  (dev/plans/2026-05-22-doc-voice-audit-design.md).
- Voice-only: no factual or content changes. Function signatures, param names,
  return-value facts, example code, version numbers, and issue references are
  unchanged.
- Adds dev/voice-fingerprint.md (synced from the vault) as the yardstick, and
  dev/voice-audit-classification.md recording what was and was not in scope.

## Test plan
- [x] devtools::check(args = "--as-cran") — 0 errors, 0 warnings, 0 notes
- [x] All 3 vignettes render
- [x] Re-audit pass: no AI tells remain; diffs are voice-only
- [x] Em-dash density check: added <= removed
EOF
)"
```

- [ ] **Step 6: Verify CI is green**

```bash
gh pr checks <PR-NUMBER>
```

Expected: all checks pass.

---

## Self-Review

**Spec coverage:**
- Scope (4 surfaces): roxygen → Tasks 3-6; vignettes → Task 7; README → Task 8;
  NEWS → Task 9. Covered.
- Fingerprint, two registers → Task 1 (full text embedded). Covered.
- Classification → Task 2. Covered.
- Voice-not-content guardrail → stated in every rewrite task; verified Task 10
  Step 3. Covered.
- Verification (R CMD check, vignette render, re-audit, em-dash check) →
  Task 10. Covered.
- Two fingerprint homes (Vault + repo) → Task 1 Steps 2-5. Covered.
- One PR, after #91, before RC → gating note + Task 10. Covered.

**Placeholder scan:** `<2026-05-commit-hash>` and `<PR-NUMBER>` are runtime
values the worker fills from the commands immediately above them, not
plan-authoring gaps. No "TBD"/"add error handling"/"similar to Task N".

**Consistency:** File paths consistent throughout. Fingerprint filename
`dev/voice-fingerprint.md` and Vault path `memory/writing-voice.md` consistent
between Task 1 and later references. Register names ("narrative", "terse")
consistent.
