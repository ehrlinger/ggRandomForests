# Guard varPro's silently-dropped `xvar.names` — design (v1)

**Date:** 2026-07-16
**Branch:** `fix/varpro-xvar-drop-guard`
**Status:** design approved in brainstorming; pending spec review

## Goal

`gg_partial_varpro(object = fit, xvar.names = my_names)` is a documented,
supported call that can silently return fewer variables than asked for. Make
that loss loud, in the one place ggRandomForests can still see it.

## The upstream behaviour

`varPro::partialpro()` (varPro 3.1.0) filters requested variables with:

```r
if (missing(xvar.names)) {
    xvar.names <- topvars
    xvar.names <- xvar.names[1:min(length(xvar.names), nvar)]
}
variables <- object$xvar.names[as.numeric(na.omit(match(xvar.names,
    object$xvar.names)))]
```

The `na.omit()` on `match()` discards any requested name that is not in
`object$xvar.names` — no error, no warning, nothing recorded on the returned
object. Asking for 12 variables can return 10, and nothing downstream can tell
that apart from having asked for 10.

This is easy to hit because `varpro()` screens in two stages. Measured on a
45-predictor synthetic dataset (5 signal, 40 noise), 2026-07-16:

| Set | Size | What it is |
|---|---|---|
| `object$x` | 45 | the full design matrix |
| `object$xvar.names` | 26 | what `partialpro()` can reach |
| `get.topvars(object)` | 15 | the default when `xvar.names` is absent |

Users who pass an externally-derived variable set (e.g. names from an `rfsrc`
VIMP ranking) hit the 26-variable ceiling. It presents as intermittent — a
top-10 VIMP set came back complete, a top-12 set silently lost 2 — which makes
it harder to notice, not easier.

`varpro(..., split.weight = FALSE)` opens `object$xvar.names` to all 45.
`nvar=` and `sparse = FALSE` do **not**: `nvar` is a reporting cap, and
`sparse` only deepens topvars. `split.weight.method = "vimp"` barely helps
(25 vs 26).

## Scope

**In:** the object-driven path only — `part_dta = NULL`, `object` supplied.

**Out:**
- The `part_dta` path. When the caller runs `partialpro()` themselves, the
  dropped names are gone before ggRandomForests sees anything. The existing
  warning that `...` is ignored is already the honest answer there.
- Provenance changes. Considered and declined: `provenance$xvar.names` keeps
  recording `object$xvar.names` as it does today. Recording
  requested/returned/dropped is a plausible follow-up, but the attribute is a
  contract others may rely on and a console warning covers the failure.
- Filing upstream. The `na.omit` is varPro's and a warning belongs in
  `partialpro()`, but that is explicitly out of scope for this change. See
  **Upstream** below.

## Why ggRandomForests can guard this at all

A guard here might look like it requires ggRandomForests to take ownership of
the `partialpro()` call. It does not: `gg_partial_varpro()` already calls
`partialpro()` itself (`R/gg_partial_varpro.R:259-269`):

```r
if (is.null(part_dta)) {
  learner <- switch(scale, rmst = .rmst_learner(object, time),
                           surv = .surv_learner(object, time), NULL)
  part_dta <- if (is.null(learner)) varPro::partialpro(object, ...)
              else varPro::partialpro(object, learner = learner, ...)
}
```

and the `@param ...` roxygen (`R/gg_partial_varpro.R:100-107`) explicitly
advertises `xvar.names` as the thing to pass through. So the lossy call is one
this package documents and makes. The guard changes no division of labour; it
sits inside a path already owned.

## Design

### Placement

A new internal, `.warn_varpro_dropped_xvars()`, called inside the
`is.null(part_dta)` block **before** the `partialpro()` call. Warning early
means the caller learns before paying for the isolation-forest UVT
computation, not after.

### What it compares

Requested names against `object$xvar.names` — the exact set upstream filters
against:

```r
dropped <- setdiff(as.character(requested), object$xvar.names)
```

Not requested-vs-returned. Comparing against `object$xvar.names` mirrors
upstream's own `match()` semantics, is deterministic, and needs no
introspection of `partialpro()`'s return-value naming.

### When it fires

Only when `xvar.names` was explicitly supplied through `...`.

The upstream source settles this: the `nvar` cap applies *only* in the
`missing(xvar.names)` branch, so when the caller supplies names explicitly,
`nvar` cannot confound the comparison and the `setdiff` is exactly the
silently-dropped set.

When `xvar.names` is absent, the `get.topvars()` fallback is already
documented (`R/gg_partial_varpro.R:104-107`) and is expected behaviour, not a
silent drop. No warning there.

Silent (no warning) in all of: `xvar.names` absent; `object` NULL; every
requested name reachable.

### The message

Names the dropped variables, reports the ladder concretely via
`length(object$xvar.names)` and `ncol(object$x)` (e.g. "26 of 45 predictors"),
and points at `varpro(..., split.weight = FALSE)`.

### Shape

Sits beside `.warn_varpro_rmst()` — same file, same section, same
`call. = FALSE` convention, same `#' @keywords internal` tag.

## Precedent

This is the third `.warn_varpro_*` guard in this file, and the pattern is
established rather than invented here. `.warn_varpro_rmst()`
(`R/gg_partial_varpro.R:340`) exists for a structurally identical reason:
`partialpro()` has no `time` argument, so a horizon passed through `...` was
silently dropped and multi-horizon plots differed only by Monte-Carlo noise.
The v3.2.0 answer was not "that is upstream's bug" — it was a targeted
warning plus a `learner` workaround, tested at
`tests/testthat/test_gg_partial_varpro.R:393`.

The `na.omit` drop is the same species of trap, on the same function, reachable
through the same `...`. Treating it differently would be inconsistent with the
file's own stance.

## Documentation

Folded into the same change:

- `@param ...` gains the reachability ceiling.
- A `@details` paragraph gains the `x` (45) ⊃ `xvar.names` (26) ⊃
  `get.topvars` (15) ladder, that `nvar`/`sparse` do not lift it, and that
  `split.weight = FALSE` does.
- `@examples` gains a worked object-driven example — see below.
- The varpro vignette gains the same note in prose.

Prose follows the `ehrlinger-writing` harness per `CLAUDE.md`.

## Examples

### The gap

**No example in the package calls `partialpro()`.** `gg_partial_varpro()`'s
own `@examples` build a `mock_data` list by hand and call
`gg_partial_varpro(mock_data)` — the `part_dta` path. So the object-driven
path is entirely unexercised by examples, and it is the path that (a) this
package owns, (b) carries the ceiling, and (c) is the one in routine use.

The examples currently demonstrate only the path that structurally cannot hit
this bug. This change gives the object-driven path its first worked example.

### mtcars exhibits the ladder

Measured 2026-07-16, `set.seed(42); varpro(mpg ~ ., data = mtcars, ntree = 50)`:

| Set | Size | Members |
|---|---|---|
| `ncol(vp$x)` | 10 | cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb |
| `length(vp$xvar.names)` | 6 | cyl, disp, hp, drat, wt, carb |
| `get.topvars(vp)` | 4 | — |

Unreachable: `qsec`, `vs`, `am`, `gear`.

The silent drop reproduces end-to-end on stock mtcars — requesting
`c("wt", "hp", "qsec", "vs")` returns **only `wt` and `hp`**, with `qsec` and
`vs` gone and no signal of any kind. That is the same 45 ⊃ 26 ⊃ 15 shape from
the synthetic dataset, in miniature, on a dataset already used throughout the
package.

### Cost

`varpro()` 0.10s + `partialpro()` (2 vars) 0.23s = **~0.33s**. Negligible
against the <10 min overall check budget, so the example can be genuinely
runnable rather than `\dontrun`.

This matters because `\donttest` examples **are** run by CRAN. The measurement
is what makes a real example affordable — and it means the example can use
real screened-out variables rather than a strawman `"nonesuch"` typo, which
would teach the wrong lesson (that this catches misspellings) rather than the
right one (that the reachable set is a subset of the design matrix).

### Convention

Follows `R/gg_varpro.R:119` — `\donttest{}` with a live
`varPro::varpro(mpg ~ ., mtcars, ntree = 50)` grow, `set.seed()` for
reproducibility, and no `requireNamespace()` guard (varPro is in **Imports**,
`DESCRIPTION:25`, so it is always available; the `requireNamespace()` in
`R/gg_beta_uvarpro.R:56` is unnecessary and is left alone as out of scope).

### Shape

Added to the existing `@examples` block, after the `mock_data` block, so both
paths are shown. Demonstrates, in order:

1. **Inspect the ceiling** — `ncol(vp$x)` vs `length(vp$xvar.names)`.
2. **Check before asking** — `setdiff(my_names, vp$xvar.names)` as the
   defensive habit, which is the transferable lesson.
3. **The guard firing** — `gg_partial_varpro(object = vp, xvar.names = ...)`
   with real unreachable names, warning and naming them.
4. **The lever** — refit with `split.weight = FALSE`, show
   `length(vp$xvar.names)` opens to all 10.

Step 2 is the point. The warning catches the loss; the `setdiff` habit
prevents it.

## Version

Stays at **3.5.0**. Verified 2026-07-16: no `v3.5.0` tag exists (latest is
`v3.4.0`) and `CRAN-SUBMISSION` still records `Version: 3.4.0` submitted
2026-07-02 — so 3.5.0 is prepared but unsubmitted and still open. The guard
rides along inside it.

`NEWS.md` gains a bullet under the existing `v3.5.0` heading. Neither
`DESCRIPTION` line 4 nor `NEWS.md` line 2 moves, so the DESCRIPTION-vs-NEWS
version-grep test stays green.

## Done

- `devtools::test()` passes, with new tests mirroring the
  `.warn_varpro_rmst()` pattern at
  `tests/testthat/test_gg_partial_varpro.R:393` — direct calls to the internal
  with a fake object, covering:
  - some requested names unreachable → warns, and names them;
  - all requested names reachable → silent;
  - `xvar.names` absent → silent;
  - `object` NULL → silent.
- `R CMD check --as-cran` clean (0/0/0), with the manual build.
- Examples run clean.

## Upstream

Out of scope for this change, recorded so it is not rediscovered.

The `na.omit` silent drop has **never been filed**. `kogalur/varPro` has
exactly one issue ever — [#3, "Changes to importance.varpro return
values"](https://github.com/kogalur/varPro/issues/3), open since 2025-04-25,
unrelated.

Two adjacent intentions exist, both unactioned, both about varPro silently
returning less than asked for:

1. `Claude/Sessions/2026-06-22 ggRandomForests v3.2.0 RMST Fix.md:32` — an
   unchecked box: *"Upstream varPro fork: optionally add the guardrail to
   warn/error on unknown `...` args (e.g. a dropped `RMST=`) instead of
   silently dropping."* A different mechanism (unknown `...` fall-through, not
   `na.omit(match())`), so filing one would not cover the other.
2. [ggRF #118](https://github.com/ehrlinger/ggRandomForests/issues/118)
   (closed 2026-06-23) — `varPro::importance()` returns 0 rows,
   intermittently. Its task list says *"file upstream with varPro
   (Lu/Ishwaran) if it's a varPro bug"* + *"Protect against it in
   ggRandomForests."* The upstream half was never done.

Today's `na.omit` is a third instance of the same signature. When upstream does
get addressed, one consolidated issue covering all three is likely better than
three.

The guard specified here warns; it does not fix `partialpro()`. That
distinction is deliberate and should not be read as closing the upstream item.
