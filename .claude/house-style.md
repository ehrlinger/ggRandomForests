<!--
  GENERATED FILE - DO NOT EDIT.

  Composed by compose-house-style.R in the ehrlinger/house-style
  repository. Edit the sources in the Obsidian vault (memory/), then
  recompose. Editing this file directly will be reverted by the next
  compose and flagged by --check.

  repo:            ggRandomForests
  profile:         package-cran
  default persona: (d)
  sources:
    writing-voice.md               sha256:3018e1e0bf8e
    writing-reader-profile.md      sha256:179212de138c
    writing-context.md             sha256:87d5555936e1
    r-package-structure.md         sha256:0b90e3e645fd
-->

# House Style — ggRandomForests

Default reader persona for this repository: **(d)**. Write for one persona at a time.

---

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
- **Conversational, not chatty.** A colleague explaining the method at a
  whiteboard, not a blog post. Keep an analogy when it teaches; cut winking
  asides and cute phrasing. "no Tukey rule hiding in the middle" is too cute;
  "not the usual Tukey 1.5 IQR whiskers" is right. Plain and direct beats
  folksy. The reader is a peer, so don't perform for them.
- Vary sentence length. A short flat statement after a long one lands well.

**Terse** (roxygen @param/@return, NEWS bullets)
- Compressed, but still plain and concrete, not sterile.
- State the thing; skip the preamble. "Logical; if TRUE, ..." not "This
  argument controls whether...".
- No analogies, no question headers here; the voice shows in word choice.

## Rules

- Em-dashes: Claude does not write them. Native to the voice and honestly
  overused, so the drafting rule is a comma, parentheses, or a full stop. John
  adds them back where he wants the pause. (Changed 2026-08-17 from "use
  sparingly, keep one where it earns the pause", which conflicted with the
  absolute rule in [[preferences]] and [[identity]]. Placing them is a judgment
  about his own voice, so he makes it rather than delegating it.)
- Ellipses: an informal-register habit (text, email). Keep them out of package docs.
- Don't overstate. No overselling. Cut "enhanced", "powerful", "seamlessly",
  "robust" (as a brag), "comprehensive". State what the thing does, at its size.
- Imperfection is allowed. Mild redundancy, an occasional long sentence: human
  texture, not an error to scrub. Don't polish to a glassy finish.
- Repetition that teaches is voice, not defect. Restating a concept, or a
  callback structure (state the problems up front, answer each later), is kept.
  Repeat when it clarifies; cut repetition that only fills space.

## When NOT to apply this voice

This voice governs documentation *prose* — Narrative and Terse. It does not
govern:

- CRAN-facing `\value` / `@return` boilerplate, which follows R documentation
  convention, not register.
- R error, warning, and message strings, which state the condition plainly.
- Code comments, which explain the code to a maintainer, not the method to a
  reader.

When the task is one of these, ignore the registers and write to the local
convention.

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


## Reference exemplars

Canonical samples of the voice, by register. When in doubt, read the one whose
register matches the task.

- **Plain-English explanatory** (the gold standard for teaching a method):
  the CORR methods boilerplates — the plain-English VarPro write-up and the
  SID-clustering supplement (internal, CORR Analysis Team share). Opens from the familiar, carries one analogy throughout
  (fruit basket, noise-reduction filter), question-headed sections, "we"/"you",
  numbered-problems-answered-later callback.
- **Short-form announcement** (LinkedIn / release posts): the TemporalHazard
  1.0.3 LinkedIn post, below. Compresses the narrative register: familiar
  opener, one rhetorical-question hook, one carried picture, understatement,
  "we"/"you", no marketing tricolons.
- **Recipe book ("When to use it" sections)**: the Survival Plots chapter
  opener in `hvti_graphics/survival.qmd`. Narrative register aimed at a
  biostatistician reader: names the clinical question first ("how long until an
  event"), defines the one idea that makes the method its own (right-censoring),
  ties the function to the SAS macro the reader already trusts (`%kaplan`), and
  closes on how the object is used. No overstatement, "you" throughout.
- **Formal academic**: arXiv 1612.08974 and 1501.07196 (Ishwaran/Ehrlinger).
  Passive, citation-dense, no "you" — use only when the task is a methods paper.

### Short-form exemplar — TemporalHazard 1.0.3 (LinkedIn, 2026-05-29)

> When modeling survival after surgery, we know that risk is not constant. It's
> high in the days right after the operation, falls to a low steady rate once
> patients recover, then creeps back up years later as they age. So why do we
> so often force a single Weibull curve onto all three? It can't be early, flat,
> and late at once — the Weibull curve splits the difference and fits none of
> these segments well.
>
> Additive hazards (Blackstone, Naftel, and Turner, 1986) allow us to linearly
> combine hazard functions into a coherent single model. Instead of forcing one
> shape, you add up several: an early phase, a constant phase, a late phase,
> each with its own scale and its own covariate effects. It's been the workhorse
> of cardiac surgery outcomes ever since, and its code has been openly licensed
> for years — but running it still meant a SAS license.
>
> TemporalHazard is that model, rebuilt in pure R at Cleveland Clinic. We
> checked it against the original program fit for fit, so the numbers match what
> longtime SAS users already trust. Putting it in R opens these methods to many
> more users.
>
> Around that core it does what you'd want: five distributions, stepwise
> selection, confidence limits on predictions, and the usual diagnostics —
> Kaplan-Meier overlays, calibration, bootstrap. The vignettes walk a real
> clinical dataset start to finish, the way you'd actually run the analysis.

Why it works: familiar opener ("we know that risk is not constant"), a
rhetorical-question hook ("So why do we so often..."), one carried picture
(early/flat/late), understatement ("the usual diagnostics"), and "we"/"you"
throughout. No forced tricolon, no padded feature list, no overselling.

---

# Reader Profiles — documentation audiences

A menu of selectable audiences for the `ehrlinger-writing` harness. Write for
ONE persona at a time, not a blend. The active persona is chosen per task
(explicit choice → repo `CLAUDE.md` default → ask). The `hvti_graphics` recipes
book defaults to persona (a); the public CRAN packages (`ggRandomForests`,
`temporal_hazard`) default to persona (d).

*Retitled 2026-07-16: this was "HVTI graphics documentation", but the harness
also governs two public CRAN packages whose readers have no HVTI context. See
(d).*

## (d) Public CRAN R user — DEFAULT for ggRandomForests / temporal_hazard

Someone who found the package on CRAN or GitHub and is reading `?fn` or a
vignette. No HVTI, no CORR, no access to the internal datasets, and no idea who
we are. They are a peer — often a statistician or a data scientist — but every
piece of shared context personas (a)–(c) rely on is absent.

- **Already knows:** R and ggplot2; random forests in general terms. Often
  `randomForestSRC`. Rarely `varPro`, which is new and thinly documented.
- **Wants from the docs:** what the function returns, what an argument
  actually does, and an example that runs on data they already have.
- **Lands when:** the example runs as written on a stock dataset (`mtcars`,
  `pbc`, `Boston`); the surprising behaviour is named *before* they trip over
  it; and the doc says which function to reach for and on what scale to read
  the result.
- **Bounces when:** the docs assume internal context, cite a dataset they can't
  obtain, or document the happy path and leave the footgun to be discovered
  mid-analysis.
- **Watch for:** *inherited upstream behaviour presented as ours.* Much of what
  surprises this reader originates in `varPro` or `randomForestSRC`, and from
  the outside they cannot tell which package to blame or where to file. Say
  when a behaviour is upstream's, name the upstream function, and give the
  lever that works around it. Second, examples that depend on a fit too
  expensive to run — if it can't be shown cheaply, show the inspection step
  instead of the whole computation.

---

# Project Context — HVTI graphics ecosystem

Why we write the way we do. The harness reads this so prose carries the right
assumptions about purpose and constraints.

## The ecosystem

- **hvtiPlotR** — ggplot2 themes and plot constructors; the R replacement for
  the historical `plot.sas` macro.
- **ggRandomForests** — graphics for random forests and variable priority
  (varPro), built on randomForestSRC.
- **temporal_hazard** — additive (Blackstone, Naftel, and Turner, 1986) hazard
  models in pure R.
- **hvti_graphics** — this recipes book, which ties the three together into a
  house style for clinical figures.

## Purpose

A single source of ggplot2 recipes for publication-quality, house-style figures
for HVTI CORR (Cardiovascular Outcomes Registries and Research, Cleveland Clinic
Heart & Vascular Institute) publications and presentations. Each recipe pairs a
figure with the code that produces it, so the next person starts from a working
script instead of a blank one.

Two ideas run through the book: a figure is built in two steps (a constructor
prepares and validates the data, then `plot()` hands you a bare ggplot you finish
with `+`), and every example stands on its own with its own sample data.

## Constraints that shape the writing

- **CORR publication standards** — figures must meet journal expectations.
- **Reproducibility** — Git, renv, dataset manifests; every figure regenerable.
- **No PHI** — never in code, prose, or example data.
- **R-first** — R is the working language; SAS is the heritage we migrate from.
- **SAS-migration heritage** — many readers trust SAS output, so we say when the
  R version matches the original (the way we checked temporal_hazard fit for fit).

---

# R Package Structural Rules — house style

This document governs the structural side of the house style: README order,
the roxygen contract, vignette roles, pkgdown layout, `DESCRIPTION` fields,
and versioning. `writing-voice.md` and `writing-reader-profile.md` govern how
you write; this one governs what has to be there and in what order. It is
written for the person about to write or audit a package README — most often
the biostatistician who already knows R and is deciding whether this
package's front door matches the other seven.

Derived from `hvtiPlotR`, the de-facto template across the eight-package
portfolio, with a small number of deliberate improvements it does not yet
itself reflect. Recorded so the other seven — and hvtiPlotR, on those few
points — can be brought into line with it rather than the rules drifting to
match whichever package they came from.

## README canonical order

Twelve elements, in this order. Skip an element only when its "required"
condition doesn't hold — don't reorder around a skip.

| # | Element | Required |
|---|---|---|
| 1 | `# <pkg> — <plain-language subtitle>` | always |
| 2 | Badge block | always |
| 3 | Provenance callout | if fork, SAS port, or inherited |
| 4 | Status block | if version < 1.0.0 |
| 5 | Lede paragraph | always |
| 6 | Docs-site link | if pkgdown |
| 7 | Installation | always |
| 8 | Quick start, runnable | always |
| 9 | Function reference, grouped tables | always |
| 10 | Documentation and vignette index | if vignettes |
| 11 | Related packages | if ecosystem member |
| 12 | Citation | `package-cran` only |

**Lede openings.** Three are permitted. Which one is right depends on where
the reader already stands, and forcing everyone through the same opening
would flatten a real difference between them:

- *What it is and who it is for* — the reader has already been told to use
  the package. Current examples: hvtiPlotR, hvtiRpropensity.
- *The pain you already have* — the reader still needs convincing to adopt.
  Current example: hvtiRtables.
- *What works today* — the reader is judging whether the package is ready.
  Current example: hvtiRdatasets.

Whichever opening you use, the first paragraph says what the package does.
That part isn't optional across the three.

**Provenance.** Mandatory wherever it applies, and in this portfolio that's
nearly everywhere — most of these packages started life as something else.
Three kinds:

- *SAS-macro port* — hvtiPlotR (`plot.sas`), hvtiRpropensity,
  hvtiRutilities (`PROC CONTENTS`, `PROC MEANS`), hvtiRtables (SAS table
  macro).
- *Upstream fork* — hvtiBoostmtree, forked from `kogalur/boostmtree` at
  v1.5.1.
- *Institutional inheritance* — TemporalHazard, from the UAB SAS/C HAZARD
  code.

The callout states what the package descends from and how faithful it is to
that source. For a fork, it also states what was renamed and what wasn't —
a reader diffing against upstream needs to know which names still line up.

**Status block.** Required while the version's major digit is 0, forbidden
once it isn't. While a package is pre-1.0, the status block says which parts
are implemented and which aren't, so a reader can judge readiness without
going and reading the NEWS file. Once you cross 1.0.0 the block goes away —
the version number is now doing that job.

**Badge tiers.** The order below is fixed; the contents are not. Include a
badge when the thing it reports on exists, skip it when it doesn't, and don't
reorder around a skip. Each tier is its own blank-line-separated block, and a
tier that ends up empty just isn't there.

Six are required of every package, in this order:

1. **R-CMD-check**
2. **codecov**
3. **repostatus**
4. **pkgdown**
5. **GitHub r-package version**
6. **lint**

These are required because they're already true. Seven of the eight packages
run the lint, pkgdown, and test-coverage workflows today; what's missing is
mostly the badge, not the machinery. A workflow running green that the README
never mentions is coverage nobody can see, which is its own small version of
the staleness problem — the check works, and the reader has no way to know.
repostatus is a static shield with no infrastructure behind it at all, and the
version badge just reads `DESCRIPTION`, so neither has an excuse.

Where a badge is genuinely missing because the underlying thing is missing,
the fix is to add the workflow, not to drop the badge. Only hvtiRdatasets is
in that position, lacking lint, pkgdown, and test-coverage entirely.

**Required for the `package-cran` profile**, after the six:

- CRAN status, cranlogs, cranlogs grand-total.

On an internal package these are meaningless rather than merely optional, so
they're absent there and that absence is correct. A CRAN package keeps the
GitHub version badge as well — the two report different numbers, and the gap
between the released version and the development one is worth seeing.

**Optional**, in this order where they apply:

- lifecycle, License, DOI.

DOI appears only where a Zenodo deposit exists; lifecycle only where the
package makes a stability claim it means.

Because five of the six required badges report on a workflow, this rule and
the CI standard have to move together. Requiring the codecov, pkgdown, and
lint badges is the same as requiring those three workflows.

The hand-rolled dynamic-regex version badge currently living in
hvtiRutilities is replaced by the standard GitHub r-package badge — it's
doing the same job with more code to maintain.

**Function reference** is grouped markdown tables by domain, one table per
function family, each a two-column table naming the callable and describing
it — the name column is `Function` or `Constructor`, whichever fits the
package's API. Not nested bullet lists, and not prose. The README's job here
is navigational — it maps "what am I trying to do" onto "which function" —
and a table does that in a way a reader can scan that a paragraph can't.
Tutorial content belongs in the vignettes; behavioral detail belongs in
roxygen `@details`.

## Roxygen contract

Roxygen 8.0.0, with `Roxygen: list(markdown = TRUE)`.

Every exported object carries `@description`, one `@param` per argument,
`@return`, `@examples`, and either `@family` or `@seealso`. `@return` is
mandatory, no exceptions — it's a CRAN requirement, and it's already on the
release checklist, so an export missing one should never reach that gate in
the first place.

Internal helpers stay out of the public index — by `@keywords internal` or
`@noRd`, whichever fits the helper. `@keywords internal` keeps a documented
topic that's simply hidden from the reference index, right for a helper a
determined reader might still want to look up. `@noRd` generates no topic at
all, right for a helper that's purely an implementation detail. The rule is
the outcome, not the tag: nothing internal shows up in the public index.

**Package-level documentation** lives in `R/<pkg>-package.R`, the filename
`usethis::use_package_doc()` generates. Three packages currently keep this
in `help.R` instead; rename with `git mv` — content and `NAMESPACE` are
unaffected by the move. hvtiRtables has no package doc at all and gains one.
At minimum it states what the package is, who it's for, the workflow the
package expects, and links to the vignettes.

**Voice registers**, per `writing-voice.md`:

- `@description` and `@details` — Narrative register.
- `@param` and `@return` — Terse register.
- `\value` boilerplate follows R documentation convention rather than either
  register — see that document's "When NOT to apply this voice" section.

**Examples** run against `sample_*()` companions or stock datasets (`mtcars`,
`pbc`, `Boston`) — never against PHI, and never against an internal dataset a
reader outside HVTI can't obtain. An example that's slow but still runnable
uses `\donttest`, never `\dontrun` — the difference matters because
`\dontrun` examples don't get checked at all, and a stale one can sit broken
for a release cycle before anyone notices. An example touching a Suggests
dependency is guarded with `requireNamespace()`.

## Vignette roles

Vignettes fill named roles, not free-form topics. A reader looking for the
methods write-up shouldn't have to guess which file it's hiding in.

| Role | Filename | Required for |
|---|---|---|
| Overview | `<pkg>.qmd` | all packages |
| SAS migration | `sas-migration-guide.qmd` | SAS ports; persona (c) |
| Reference | one or more; consolidated or split by family | packages with more than one family |
| Methods and mathematics | e.g. `mathematical-foundations.qmd` | method packages |
| Contributing | `contributing.qmd` | optional |

The SAS-migration vignette ties each R function to the SAS macro it replaces
and states that the numbers match. Persona (c) doesn't need hand-holding
through R — they already know R. What they need is confirmation that this
gives the same answer as the SAS they already trust.

**Reference vignettes.** "One or more" isn't a headcount to hit — it means a
reference vignette can cover every function family in a single indexed
document, or be split family by family, whichever suits the package, so long
as no family goes undocumented and the overview vignette or the pkgdown
index tells a reader where to look. hvtiPlotR takes the consolidated form:
`plot-functions.qmd` documents the plotting functions and
`plot-decorators.qmd` documents the decorator family, both indexed from the
overview vignette. Under this rule `plot-decorators.qmd` is simply a second
reference vignette, not a free-form topic outside the table above.

**Naming exemption.** TemporalHazard keeps `sas-to-r-migration.qmd` rather
than renaming to the standard filename. Renaming a published vignette breaks
`vignette()` calls and indexed pkgdown URLs that are already out in the
world, and that's not a price worth paying just for filename consistency.
hvtiRdatasets, whose `coming-from-sas.qmd` isn't published yet, renames to
the standard name — there's nothing to break.

**Front matter** carries `title`, `author`, `date: today`, `format: html`
with `toc: true`, and the three `%\Vignette*` fields with
`%\VignetteEngine{quarto::html}`.

Vignette prose method — how to write the body once the role and front matter
are settled — is owned by `vignette-clarity-pass.md` and isn't restated
here.

## pkgdown

Follows the hvtiPlotR model:

- `reference:` split into titled sections, each with a prose `desc:` that
  says when to reach for that family, not merely what it contains.
- `articles:` grouped by vignette role.
- `navbar:` cross-linking to related packages in the ecosystem.
- `template:` bootstrap 5 with the light-switch enabled.

Every exported object appears in exactly one `reference:` section. pkgdown
fails the build on an unreferenced topic, and that failure is the check that
keeps this rule honest rather than aspirational.

## Continuous integration

Five workflows required of every package, two more on the CRAN profile. A
package running more than that is almost certainly checking the same thing
twice — which has already happened three times here, and the cost isn't the
runner minutes, it's that a wall of green checks stops being read.

The rule for adding another: name the question it answers that no existing
workflow answers. If the answer is "the same one, on a different day," it
doesn't get added.

**Required of every package.** The first four run on `pull_request` and on
`push` to the default branch. Since all work goes through a branch and PR,
that push trigger is really the merge trigger — don't add feature-branch
globs, they double every run.

| File | Question it answers | Trigger | Matrix |
|---|---|---|---|
| `R-CMD-check.yaml` | Does it build and pass its tests where people use it? | `push[main]`, `pull_request` | macos·release, windows·release, ubuntu·devel, ubuntu·release, ubuntu·oldrel-1 |
| `test-coverage.yaml` | How much of the code do the tests reach, and which way is it moving? | `push[main]`, `pull_request` | ubuntu·release |
| `lint.yaml` | Does it match the style the rest of the portfolio is written in? | `push[main]`, `pull_request` | ubuntu·release |
| `lint.yaml` → `docs-current` job | Do the generated `man/` files still match their roxygen sources? | `pull_request` | ubuntu·release |
| `pkgdown.yaml` | Does the docs site still build, and does every exported topic still have a home? | `push[main]`, `pull_request`, `release`, `dispatch` | ubuntu·release |
| `check-manual.yaml` | Does the PDF manual build, and is every `.Rd` free of raw Unicode? | `push[main]`, `release: published`, `workflow_dispatch` | ubuntu·release |

`R-CMD-check.yaml` runs `r-lib/actions/check-r-package@v2` and leaves `args`
at its default, which is `c("--no-manual", "--as-cran")` — so the CRAN gate is
already on, and a second workflow to "add `--as-cran`" is adding nothing. Set
`build_args: 'c("--no-manual","--compact-vignettes=gs+qpdf")'` and
`upload-snapshots: true`. Don't restate `error-on: 'warning'`; it's the
default, and writing a default out invites the belief that it was chosen. On a
Quarto-vignette package, install the package into the user library before the
check step — Quarto's subprocess can't resolve `library(<pkg>)` on Windows
otherwise, and the failure looks like a package defect when it isn't.

`test-coverage.yaml` writes cobertura from `covr::package_coverage()` and
hands it to `codecov/codecov-action` **once**. Set `files: ./cobertura.xml`
and `disable_search: true`, so the action uploads what you produced rather
than what it went looking for.

Be careful with `fail_ci_if_error`. An expression of the shape
`${{ github.event_name != 'pull_request' || secrets.CODECOV_TOKEN != '' }}`
does not reliably yield a boolean -- GitHub's `||` returns the first truthy
*operand*, so this can evaluate to the token string rather than `true`, and it
is `true` for every non-PR event regardless of whether a token exists. Write a
condition that can only be `true` or `false`, and decide deliberately whether a
missing token should fail the build or not.

`lint.yaml` sets `LINTR_ERROR_ON_LINT: true`. A lint job that reports and then
passes is a green badge asserting nothing, which is worse than no badge — the
README rule reads a missing badge as an honest absence and a present one as a
claim. Commit a `.lintr` so the rule *selection* lives in the repo rather than
being whatever the defaults happen to be.

**Pin the lintr version, and install the package itself:**

```yaml
- uses: r-lib/actions/setup-r-dependencies@v2
  with:
    extra-packages: any::lintr@3.4.0, local::.
```

Two separate requirements in one line.

**`@3.4.0` — pin it.** A `.lintr` pins *which linters run*; it does not pin
*what each one checks*. A lintr release can add a check to an existing linter
and redden a green build with nobody touching the code. That is not
hypothetical: 3.4.0 added a `<<-` check to `assignment_linter`, so
temporal_hazard linted clean on a developer's 3.3.0.1 and failed CI on 3.4.0 —
same commit, same config. The same release also changed that linter's
arguments, dropping `allow_cascading_assign`, so a config written against one
version can be invalid on the other.

Pinning makes lint failures mean "the code changed", which is the only way the
signal stays readable. Upgrade deliberately, as its own PR, so a new release's
findings arrive as a reviewable diff rather than as a mystery red on unrelated
work. And when you do upgrade, run it locally against the pinned version first
— a developer machine carrying an older lintr will not reproduce CI, and will
report the code clean while the runner disagrees.

`object_usage_linter` needs the package's own namespace to resolve internal
calls. Without `local::.` it cannot see them and reports every call to one as an
undefined global. On hvtiRpropensity that was 58 phantom lints against 12
real ones — the noise outnumbered the signal five to one.

Worse than the count: the phantoms were *hiding a real finding of the same
type*. Once they cleared, one genuine `object_usage_linter` hit remained, an
assigned-but-unused local. In a list of 58 identical-looking messages it would
never have been read. A check calibrated wrongly is worse than a check switched
off, because the true findings arrive dressed as the false ones and get
dismissed together.

So when a lint backlog looks implausibly large, check this before treating any
of it as debt.

`lint.yaml` also carries a **`docs-current`** job, on `pull_request` only:

```yaml
- name: Documentation is current
  run: |
    Rscript -e 'roxygen2::roxygenise()'
    git diff --exit-code man/ NAMESPACE DESCRIPTION
```

Call `roxygen2::roxygenise()` rather than `devtools::document()`. They do the
same work, but devtools pulls roxygen2 in *transitively*, so the version that
runs is whatever the resolver picked, not the one you pinned — which is
precisely how a pin gets defeated without anyone editing it. And diff
`DESCRIPTION` too: roxygenise writes `Config/roxygen2/version` and the
`Collate:` field, so leaving it out lets real regeneration drift pass unseen.

It answers a question no other workflow answers, which is the bar for adding
anything: *are the generated files in sync with the sources they come from?*
`check-manual.yaml` would catch the same drift, but it deliberately runs on
pushes to the default branch rather than on pull requests, so it only speaks
after the merge. That gap is real and was hit within a day of this standard
existing — hvtiRdatasets PR #3 changed the `URL:` field in `DESCRIPTION` without
regenerating `man/`, passed every check it ran, and the drift surfaced only once
it reached `main`, needing a second PR to fix.

Put it in `lint.yaml` rather than a sixth workflow. It needs the same R setup
lint already does, it takes seconds, and `lint.yaml` has become this portfolio's
fast-pull-request-checks file — hvtiPlotR already runs the house-style drift
check there. Adding a whole workflow for a two-line job would inflate the count
the "name the question" rule exists to hold down.

Two things make this reliable rather than flaky. `DESCRIPTION` pins
`Config/roxygen2/version`, so the runner regenerates with the same roxygen the
author used and a version bump can't masquerade as drift. And it does **not**
build the manual, so the check-time budget decision stands untouched — this is
a `git diff`, not a LaTeX run.

**Set `dependencies: '"hard"'` on this job.** The action defaults to
`dependencies: "all"`, which installs the package's entire `Suggests` tree and
every system requirement it maps to. `roxygenise()` needs only the package's
`Imports` loadable, so on hvtiRdatasets the default was fetching arrow, dplyr
and quarto to support one second of real work. The doubled quoting is not a
typo — the input is an R expression, so `'"hard"'` is what yields `"hard"`.

The cost is not just minutes. On 2026-08-07 an Ubuntu mirror degraded to
roughly 92 kB in 21 seconds and that job sat in `Installing system
requirements` for 32 minutes before being cancelled, while hvtiRutilities'
equivalent — same action, same pin, shorter apt queue — came through the same
window in 49 seconds. Every package you install is exposure to someone else's
outage, so install the fewest that answer the question.

Check three things before narrowing it: no `@eval`/`@evalRd` tags, no
top-level `library()`/`require()` in `R/`, and no `Suggests` package
referenced in `R/`. A reference inside a function body guarded by
`requireNamespace()` is fine — roxygen loads the code, it does not run it.
`pkgload`, which `roxygenise()` loads code with, arrives via roxygen2's own
`Imports` and is unaffected.

**Install a tool only in the job that runs it.** A pinned version should
appear exactly once per repository. `extra-packages` entries outlive the step
that needed them: when `devtools::document()` moved out of `R-CMD-check.yaml`
into `docs-current`, both `any::devtools` and `any::roxygen2@8.1.0` stayed
behind in two repos — resolved on all five matrix platforms, used by nothing,
and leaving the roxygen2 version written down in two files that a bump has to
keep in agreement.

A pin in two places is not a formatting problem, it is a second place for it
to be wrong, and workflow-level `env:` does not fix it: `env:` is scoped per
*file*, so pins living in `R-CMD-check.yaml` and `lint.yaml` would need the
variable declared twice — hiding the duplication rather than removing it. If a
version genuinely must be read in two live jobs, derive it from
`Config/roxygen2/version` in `DESCRIPTION`, which is the value the check is
asserting against anyway. Otherwise delete the copy that no step uses.

Both copies being *live* is the harder case, and the paragraph above does not
reach it. temporal_hazard carried `any::roxygen2@8.1.0` in `R-CMD-check.yaml`
and again in `pkgdown.yaml`, and neither was a leftover: each fed a real
`roxygenise()` call, one gating the check matrix and one running before the
site build. With no dead copy to delete, "delete the copy that no step uses"
had nothing to act on.

Ask why the generator runs twice, not which copy to delete. Two live calls to
the same generator usually mean one of them regenerates something already
committed and already verified, which is drift-masking arriving by a different
route. pkgdown's call was that.
`install.packages(".", repos = NULL, type = "source")` already builds the help
database from the committed `man/*.Rd`, and pkgdown's reference index reads
those same files, so regenerating first supplied nothing. It overwrote the
committed docs seconds before the build, and a stale `man/` would have
published a correct-looking site over a wrong repo. Deleting that call is what
took the pin back down to one. The duplicate count was the symptom; the
redundant step was the defect.

So read a duplicate pin as a question about the steps rather than about the
`extra-packages` lines. A second use that survives that question is legitimate
and keeps its pin, and then you derive both from `Config/roxygen2/version`
rather than writing the number into two files.
(temporal_hazard PR #106, 2026-08-07.)

So when moving a step between workflows, delete its dependencies in the same
commit. Grep the workflow for every package it installs and confirm something
still calls it.

One more trap, from the commit that fixed the above: `extra-packages: |` is a
YAML *literal block scalar*, so `#` lines inside it are string content, not
comments. pak reads them as package refs and the run dies with
`Cannot parse packages: #, pinned:, ...`. Explanatory comments go above the
step, never inside the block.

`check-manual.yaml` is the one that runs `--as-cran` **without**
`--no-manual`, so the PDF manual is actually built. That step is what catches
raw Unicode in `.Rd` — Greek letters, β̂, combining marks — which
`--no-manual` skips in silence. It needs `r-lib/actions/setup-tinytex@v2`, and
only here: TinyTeX installed next to a default `args` is a LaTeX distribution
downloaded and never invoked, which is the state five workflows in this
portfolio were in.

It runs on pushes to the default branch rather than on pull requests, because
building the manual is slow and a check that makes every PR wait is one people
learn to route around. Since all work goes through a branch and PR, in practice
that means it runs on the merge commit -- but say "on pushes to the default
branch", not "on merge": a direct push would trigger it too, and describing a
trigger as something narrower than it is misleads the next reader.

Two details worth getting right in the workflow itself. Give it a `name:` a
human can scan in the Actions list (`Check manual`), not the filename with its
extension. And scope `permissions:` to `contents: read` rather than
`read-all` -- least privilege costs nothing here and stops the job quietly
gaining reach if a step is added later.

**Additional on the `package-cran` profile.**

| File | Trigger | Matrix |
|---|---|---|
| `spelling.yaml` | `push[main]`, `pull_request` | ubuntu·release |
| `check-release.yaml` | `release: published`, `workflow_dispatch` | windows·release, **windows·devel**, macos·release, ubuntu·release, ubuntu·devel |

`spelling.yaml` runs `spelling::spell_check_package(use_wordlist = TRUE)`
against `inst/WORDLIST`. CRAN rejects on misspelled documentation and the check
takes under a minute, so on a CRAN-bound package it isn't optional. It's cheap
enough to be worth having anywhere an `inst/WORDLIST` already exists.

`check-release.yaml` is the submission gate, and it is **not** a second
everyday check — that is exactly what the triplication here was. It differs
from `check-manual.yaml` in three ways that only matter when a tarball is
about to go to CRAN: `_R_CHECK_CRAN_INCOMING_` and
`_R_CHECK_CRAN_INCOMING_REMOTE_` both on; **windows·devel** in the matrix,
which is the win-builder branch that surprises most often and belongs in no
other matrix here; and the `check` directory uploaded as an artifact on
success as well as failure, because `00check.log` carries the per-step
`[Ns/Ns]` timings and the passing runs are the ones worth trending.

Read those timings against the check-time budget in
`r-package-release-checklist.md` before submitting. A local total is not the
number to trust — for ggRandomForests 3.5.1 the local check was 4m44s while
win-builder returned 8, 10 and 12 minutes, a 2.5x machine factor the margin
has to absorb. In both CRAN packages the dominant term is the vignette
rebuild, so that's where the lever is.

**Deliberately not required.**

`rhub.yaml` is `workflow_dispatch`-only so it costs nothing per push, but it
belongs on `package-cran` alone, and only where `secrets.RHUB_TOKEN` is set.
Its value is the flavors GitHub runners can't give you — gcc-UBSAN,
clang-ASAN, valgrind, noLD. On an internal package there's no submission for
those to protect, and an unmodified copy of the r-hub template is provenance
without coverage. Where you keep it, dispatch it before a submission and
record the result in `cran-comments.md`, so the file is evidence rather than
furniture.

`check-standard.yaml` should not exist. Everywhere it appears here it runs the
same five-cell matrix as `R-CMD-check.yaml`, on the same triggers, through the
same action, with strictly fewer safeguards — no `upload-snapshots`, no
Quarto pre-install. It is the weaker twin, not the complement it looks like.

### The house style drift check

Every repo carrying a composed `.claude/house-style.md` also carries a CI job
that recomposes from source and fails when the committed artifact disagrees.
That job checks out the composer from `ehrlinger/house-style`, and it
**pins `ref: house-style-v1`** rather than taking the default branch.

The pin is a tag, not a commit SHA, and the distinction matters. That repo
holds two things: the composer script, and a mirrored copy of the four source
documents CI compares against, because a runner has no vault. Pinning to a SHA
would freeze the *reference sources* along with the tool, so the check would
answer "no drift" forever and become decoration. Taking the default branch has
the opposite problem: a half-finished commit on the composer reddens every
consumer repo at once.

A moving tag keeps both properties. Advance it deliberately when the standard
changes:

```
git tag -f -a house-style-v1 <commit> -m 'what changed'
git push -f origin house-style-v1
```

Advancing it is what makes every repo start reporting drift until it
recomposes — the intended signal, not a failure. Between advances, consumer CI
is stable against whatever is happening on the composer's branches.

**Advance it only for changes that alter what consumers compose** — the four
documents under `sources/`, or the composer itself. Not for repo-internal
changes like CI workflows or README edits: those produce byte-identical
artifacts, so advancing would send every repo a drift signal that resolves to
no change. A signal that fires when nothing happened is how a signal stops
being read.

The test is cheap and worth running when unsure — compose from the tag and from
`main`, and compare. Run this from a clone of the composer repo:

```bash
repo=hvtiPlotR    # the consumer to test against

old=$(mktemp -d); new=$(mktemp -d)
git archive house-style-v1 | tar -x -f - -C "$old"
git archive main          | tar -x -f - -C "$new"

Rscript "$old/compose-house-style.R" --check --repo "$repo" --vault "$old/sources"
Rscript "$new/compose-house-style.R" --check --repo "$repo" --vault "$new/sources"

rm -rf "$old" "$new"
```

Same verdict from both means the tag does not need to move.

Be straight about what this does and doesn't buy: a given CI run is
reproducible only until the tag next moves. That is weaker than a SHA and
stronger than a branch, and the reason it is the right trade here is that
moves are deliberate and rare rather than incidental to every push. Use
`house-style-v2` if the composer's CLI ever changes incompatibly, so repos can
migrate on their own schedule.

### Branch protection

Every repo carries one active branch ruleset on `~DEFAULT_BRANCH` with four
rules: `pull_request`, `copilot_code_review`, `deletion`, `non_fast_forward`.
`required_approving_review_count` is 0 and `require_code_owner_review` is true,
which protects `main` without stopping a single maintainer merging their own
work.

`copilot_code_review` is what makes review automatic. It requests a Copilot
review on every PR into the default branch; it does not gate the merge on what
Copilot says. Requesting it by hand at PR-creation time is the thing that
doesn't survive contact with a busy week.

This is worth stating because it had already drifted once. In August 2026 only
four of eight repos enforced anything: two rulesets sat correctly configured
but disabled, and two repos — including CRAN-published `ggRandomForests` — had
none at all, so a direct push to `main` would simply have succeeded. The global
rule against pushing to `main` was written down and unenforced on half the
portfolio.

A ruleset can be switched off in the web UI without anything noticing, which
puts it in the same family as the stale synced file this whole house style
exists to catch. Audit it with:

```
gh api repos/ehrlinger/<repo>/rulesets \
  -q '.[] | select(.target=="branch") | "\(.enforcement) \(.name)"'
```

## DESCRIPTION

Title Case in `Title`. Software names quoted in `Description`. DOIs written
space-free as `<doi:10.xxxx/yyyy>`. `URL` lists both the GitHub repo and the
pkgdown site. `BugReports` set. `VignetteBuilder: quarto`.
`Config/roxygen2/version: 8.0.0`.

## Versioning

Defers entirely to the global versioning rule: a straight three-digit
semantic version, no `.9000` suffix and no fourth digit, the patch digit for
incremental work, and the minor and major digits reserved for the
maintainer's own consolidation decisions — never rolled by an agent on its
own judgment.

A documentation-only retrofit against this house style is a patch bump, with
the matching `NEWS.md` entry so the version-grep test passes.

What has to happen before a version actually ships — the CRAN Cookbook audit,
`R CMD check --as-cran` with the manual built, the check-time budget, the
reverse-dependency pass — is owned by `r-package-release-checklist.md` and
isn't restated here. A patch bump on a published package still runs that gate
in full, because a documentation change rebuilds the vignettes.
