# ggRandomForests Documentation Voice Audit — Design Spec

**Date:** 2026-05-22
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning

---

## Goal

Audit all ggRandomForests documentation and rewrite the AI-written portions in
John Ehrlinger's own voice, so the package reads as one consistent human author
rather than a mix of human prose and AI-generated text.

## Scope

All four documentation surfaces:

- **Roxygen / man pages** — 39 R files → 40 `.Rd` pages.
- **Vignettes** — 3 Quarto files (`ggRandomForests.qmd`, `-regression.qmd`,
  `-survival.qmd`).
- **README** — `README.md`.
- **NEWS** — `NEWS.md`.

## Delivery

A single PR folded into the v2.8.0 development cycle, opened **after PR #91
merges** and **before the v2.8.0 release candidate**. It is the last
substantive change in the v2.8.0 cycle.

---

## Approach

Approach A of three considered (A: fingerprint → classify → targeted rewrite;
B: wholesale rewrite; C: mechanical checklist scan). A was chosen because it
preserves prose that is already John's, concentrates effort on the genuine AI
surface, and produces an objective yardstick for review.

### Step 1 — Voice fingerprint

Build a written voice reference before touching any doc. Corpus:

- The oldest git versions of the three vignettes (JSS-era prose, predating any
  AI involvement).
- The oldest `NEWS.md` entries.
- Two external writing samples supplied by John (CORR boilerplates):
  `VarPro Modeling in Plain English`, `supp_SIDclustering_methods`.

Observed voice characteristics (from the samples):

- **Pedagogical openers** — start from the familiar, build to the new.
- **Extended analogies** — homely concrete pictures for abstract methods
  (fruit basket, blind men and the elephant, "noise-reduction filter").
- **Question-headed sections** — "Why Cluster?", "Where Do We Stop?".
- **First-person plural** — "in our practice", "we proceed", direct "you".
- **Parenthetical glosses** — terms defined inline in parentheses.
- **Scare-quotes on jargon at first use** — "rules", "regions", "elbow".
- **Sentence-initial conjunctions** — "But…", "Yet…", "Thus…".
- **Deliberate imperfection** — mild redundancy, an occasional dropped
  article, a sentence that runs long. First-draft human texture is kept.
- **Em-dashes** — native to the voice, acknowledged as overused. Rule for
  package docs: **use sparingly** — keep where the em-dash earns the pause,
  otherwise prefer a comma, parentheses, or a full stop. Do not strip them
  entirely (un-John); do not sprinkle them.

AI tells to hunt (these are NOT John): relentless parallelism, flavorlessness,
absence of analogy, absence of "we"/"you", sterile hedge-free balance, forced
tricolons, "it is worth noting". Punctuation density is not itself a tell.

### Step 2 — Two registers

The fingerprint defines two registers of the one voice:

- **Narrative register** — vignettes, README narrative, roxygen
  `@description` / `@details`. Full pedagogical voice: analogies, question
  framing, "we"/"you", builds from the familiar.
- **Terse register** — roxygen `@param` / `@return`, NEWS bullets. Compressed
  but still John: plain, concrete, no AI parallelism or hedging.
  ("Terse in conversation, complete in deliverables.")

### Step 3 — Classify

Tag every doc surface — *original-voice / AI-written / mixed* — using git
blame and commit dates, and assign each its register. Expected: vignette
narrative is mostly pre-AI (original); the ~5 newest functions' roxygen
(`gg_brier`, `gg_partial_varpro`, `gg_varpro`, `gg_udependent`, the ROC work)
and recent `NEWS.md` entries are the AI surface. The classification table is
committed as part of the spec/plan so rewrite scope is auditable.

### Step 4 — Targeted rewrite

Rewrite only AI-written and mixed docs, each against its register's
fingerprint. Genuinely original-voice prose is left untouched.

---

## Critical guardrail — voice, not content

This rewrite changes *how* things are said, never *what*. Technical claims,
`@param` names, function signatures, return-value facts, example code, version
numbers, and issue references stay identical in meaning. If a rewrite would
change a factual statement, that is out of scope: flag it separately, do not
silently edit. This keeps the large diff safe to review and keeps it from
colliding with the v2.8.0 RC.

## Verification

- `R CMD check --as-cran` → 0 errors / 0 warnings / 0 notes (roxygen rewrites
  must still produce valid `.Rd`; examples must still run).
- All 3 vignettes render.
- Re-audit pass: each rewritten doc re-scored against its register fingerprint.
- Em-dash density check — confirm "sparingly" held.

## Acceptance criteria

- Every doc surface classified.
- Every AI-written and mixed doc rewritten; every original-voice doc untouched.
- Rewritten docs pass the fingerprint re-audit.
- Zero functional or factual changes to documentation content.
- One PR, opened after PR #91 merges, before the v2.8.0 release candidate.

## Artifacts

The voice fingerprint has two homes:

- **Canonical:** `~/Documents/ObsidianVault/memory/writing-voice.md` — portable,
  reusable across all of John's writing (manuscripts, boilerplates, email,
  other R packages). Source of truth; leads when refined from other work.
- **Repo copy:** `dev/voice-fingerprint.md` in ggRandomForests — a synced copy
  committed so the PR is self-contained for CI and reviewers. Header line notes
  the Vault file is canonical.
