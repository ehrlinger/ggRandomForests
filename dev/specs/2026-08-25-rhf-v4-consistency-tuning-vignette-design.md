# RHF v4 Consistency, Tuning, and Vignette Design

**Date:** 2026-08-25

**Status:** Written proposal for maintainer review

**Integration target:** `dev_rhf`

**Delivery:** Three sequential pull requests

## Purpose

Complete the remaining Random Hazard Forests (RHF) work needed before a
ggRandomForests v4 release candidate:

1. audit the current documentation for consistent package, fitting-function,
   object-class, version, and citation language;
2. add the Phase 4 `gg_tune_rhf()` extractor and its S3 methods; and
3. add a consolidated RHF vignette built around genuinely longitudinal data.

These changes do not authorize a release, CRAN submission, tag, major-version
bump, or merge to `main`. The release remains gated on the completed vignette,
the completed consistency sweep, the full release verification, explicit
maintainer approval, and eventual CRAN acceptance.

## Delivery sequence

The work lands as three reviewable pull requests into `dev_rhf`:

1. **Consistency sweep.** Establish and apply the canonical vocabulary and
   citation record.
2. **Phase 4 tuning.** Add `gg_tune_rhf()` against the audited documentation
   baseline.
3. **RHF vignette and gate closure.** Demonstrate all four RHF families and
   record evidence for the remaining release gates.

The pull requests are sequential because the tuning documentation depends on
the consistency decisions, and the vignette depends on the final tuning API.
The boundary avoids mixing behavioral code with a broad prose audit while
leaving little later alignment cost: subsequent work uses the canonical record
created by the first pull request.

## Pull request 1: consistency sweep

### Canonical vocabulary

Current documentation uses the following package-to-fit-to-object mapping:

| Package | Fitting function | Fitted object class |
|---|---|---|
| `randomForestSRC` | `randomForestSRC::rfsrc()` | `rfsrc` |
| `randomForestRHF` | `randomForestRHF::rhf()` | `rhf` |
| `varPro` | `varPro::varpro()` | `varpro` |

The audit record will extend this table with, for each package:

- the current CRAN version at audit time;
- the minimum version supported by ggRandomForests;
- the software citation; and
- the method citation or citations used by current package documentation.

The audit verifies current versions and citation metadata against official
CRAN records and primary publications. In particular, RHF documentation cites
both the `randomForestRHF` software and Ishwaran, Hsich, Kogalur, and Lee
(2026), *Random Hazard Forests*, arXiv:2608.21597.

### Audit record

Create `release-checklist-v4.0.0.md` as the durable audit and release-gate
record. For each finding, record one disposition:

- **corrected**, with the affected location;
- **retained**, with the reason the existing language is already correct; or
- **deferred**, with a reason and a named future boundary.

The record prevents later phases from having to repeat terminology and
citation decisions.

### Audit boundary

Inspect and, where necessary, correct:

- `DESCRIPTION` and `inst/CITATION`;
- `README.md`;
- all existing vignettes and `vignettes/ggRandomForests.bib`;
- roxygen source and generated help;
- the current v4 section and active current-v3 guidance in `NEWS.md`;
- runnable examples;
- `_pkgdown.yml` and package-level documentation.

Older NEWS entries remain historical. They change only when a citation is
broken or an old entry actively directs a current user to the wrong package,
fitting function, or object class. This boundary preserves the historical
record without making future alignment materially more expensive.

The audit corrects documentation inconsistencies in this pull request. If it
reveals a behavioral code defect, the checklist records it and the defect is
handled separately rather than being silently folded into a prose sweep.

### Verification

The pull request verifies:

- official CRAN versions and citation metadata;
- generated documentation;
- spelling and lint;
- every vignette;
- the guarded full test suite; and
- a clean-archive `R CMD check --as-cran`, including the manual and archive
  content checks required by `AGENTS.md`.

## Pull request 2: Phase 4 `gg_tune_rhf()`

### Boundary

`gg_tune_rhf()` is an extractor and visualization wrapper. It accepts an
already calculated `randomForestRHF` tuning object and never starts a tuning
run itself:

```r
gg_tune_rhf(tune_fit)
```

This keeps expensive model selection explicit and preserves
ggRandomForests' role as a visualization layer. Documentation leads with the
recommended workflow: calculate and retain the upstream object, then supply
it to `gg_tune_rhf()`.

The accepted upstream class is `tune.treesize.rhf`, returned by
`randomForestRHF::tune.treesize.rhf()`, its `tune.rhf` alias, and
`randomForestRHF::tune.iAUC.rhf()`.

### Returned object

The extractor returns a data frame with class:

```r
c("gg_tune_rhf", "data.frame")
```

Its columns are:

| Column | Meaning |
|---|---|
| `treesize` | Evaluated forest size. |
| `metric` | `"OOB risk"` or `"OOB iAUC"`. |
| `value` | Value of the selected metric at that forest size. |
| `se` | Bootstrap iAUC standard error when supplied upstream; otherwise `NA_real_`. |
| `selected` | Whether `treesize` equals the upstream `best.size`. |

The extractor reads the upstream `path` table. Risk tuning uses its `risk`
column. iAUC tuning uses `iAUC` and, when present, `iAUC.se`. It validates the
class, required fields, numeric shapes, path alignment, and the presence of a
unique evaluated optimum before returning a tidy result.

Provenance retains the upstream `best.size`, `best.err`, performance
criterion, method, bounds, number of evaluations, and installed
`randomForestRHF` version. It does not copy the optional fitted forest into
the tidy object.

### S3 behavior

`plot.gg_tune_rhf()` connects the evaluated tree sizes in their upstream
order and marks the selected size. An iAUC standard-error ribbon is available
when finite standard errors exist. Risk results do not fabricate uncertainty.

`print()`, `summary()`, and `autoplot()` follow the established `gg_*` family
conventions. Plotting methods return a ggplot object and do not print it.

### Tests and documentation

Tests use small synthetic upstream-shaped objects to cover both risk and iAUC
paths, validation, provenance, printing, summaries, and plotting. One small
real `randomForestRHF` integration fixture confirms compatibility with the
installed CRAN API. One vdiffr baseline covers the default plot, with the
repository's snapshot guard enabled.

The help page shows upstream tuning first and then supplies the saved result:

```r
tune_fit <- randomForestRHF::tune.iAUC.rhf(...)
gg_tune <- gg_tune_rhf(tune_fit)
plot(gg_tune)
```

Slow upstream work is guarded and is not repeated merely to redraw or
re-extract a tuning result.

## Pull request 3: RHF vignette and release-gate closure

### Vignette purpose and data

Add `vignettes/rhf.qmd`, titled *Random Hazard Forests with
ggRandomForests*. Its sole worked data source is
`randomForestRHF::hazard.simulation(1)`.

This simulation provides repeated counting-process intervals and the genuine
time-dependent covariate `xtd = (x4 + x5) * t`. It therefore demonstrates the
RHF feature that the existing PBC-based `randomForestSRC` material cannot:
covariate values that evolve over follow-up. PBC remains in the existing
randomForestSRC documentation and is not repeated as a second RHF workflow.
The RHF vignette may briefly note that RHF also supports static predictors,
but every fitted example, AUC result, priority result, and tuning result in the
vignette comes from the longitudinal simulation.

Before interpreting output, the vignette explains counting-process input,
predictable covariate paths, and no-lookahead routing. It explicitly
distinguishes cumulative from incident AUC. It describes RHF variable
priority as a time-local rule-release contrast, not a z-score, p-value, or
thresholded selection statistic.

### Narrative sequence

The vignette follows one model through the four RHF families:

1. simulate longitudinal data and fit `randomForestRHF::rhf()`;
2. extract and plot hazard and cumulative hazard with `gg_rhf()`;
3. compare cumulative and incident performance with `gg_auct()`;
4. examine time-local variable priority with `gg_rhf_importance()`;
5. inspect risk and iAUC tuning paths with `gg_tune_rhf()`;
6. summarize cross-family inputs, outputs, and interpretation in a support
   table; and
7. close with reproducibility, caching, and further reading.

The importance and tuning sections lead with supplied, precomputed upstream
objects. This makes the reusable-object workflow the documented default while
preserving APIs that calculate importance when it is not supplied.

### Runtime and saved results

A reproducible preparation script creates compressed RDS objects for the RHF
fit and expensive derived results. The vignette displays the preparation code
but loads the saved objects during routine builds. All ggRandomForests
extractors and plots run live against those objects.

The saved artifacts must remain small enough for the package tarball and must
not create a general forest-fixture directory for the test suite. Their
purpose is CRAN-safe vignette execution, not test acceleration. Preparation
records the random seed and relevant package versions.

### Documentation integration

Add the vignette to `_pkgdown.yml` and connect it from the README, package
overview, current NEWS, shared bibliography, and release checklist. Include
the RHF method paper and software citation wherever the claims they support
are made.

## Release gates

Completion of the third pull request closes only the implementation-side
gates. A v4 release remains on hold until all of the following are true:

- the RHF vignette is complete and reviewed;
- the V3/V4 consistency sweep is complete and recorded;
- `rfsrc()`, `rhf()`, and `varpro()` naming, package versions, and citations
  are consistent across current documentation;
- generated documentation, spelling, lint, all vignettes, pkgdown, and the
  guarded full test suite pass;
- a clean-archive `R CMD check --as-cran` passes with the manual and archive
  checks;
- the maintainer explicitly lifts the release hold; and
- the resulting release is accepted by CRAN.

The final item is a post-submission acceptance condition, not permission in
this design to submit. Until the maintainer gives that permission, no release,
submission, tag, major-version bump, or merge to `main` is performed.

## Out of scope

- fitting RHF models inside any `gg_*` extractor;
- adding a new dependency;
- adding RHF partial dependence;
- changing the returned shape of existing `gg_*` objects;
- revising historical NEWS solely for modern wording;
- duplicating the PBC example as an RHF worked analysis; and
- releasing or submitting v4.

## References

- Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). “Random Hazard
  Forests.” arXiv:2608.21597. <doi:10.48550/arXiv.2608.21597>.
- Ishwaran H, Kogalur UB (2026). *randomForestRHF: Random Hazard Forests*.
  R package version 1.0.1.
  <https://CRAN.R-project.org/package=randomForestRHF>.
