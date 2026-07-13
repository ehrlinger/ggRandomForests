# AGENTS.md

## Before you touch code
Load this repo's codemap first. It lives in the Obsidian vault under `Codex/repomaps/`
and is read via the `read-codemap` skill (`/codemap <repo>`). Orient on the public API
surface and where-things-live *before* editing — do not infer structure from a partial
file read. If the codemap looks stale, say so and offer to refresh it (`/regenerate-codemap`)
rather than working from a guess.

## Four rules
1. **Think before coding.** Don't assume — ask. If the request is ambiguous or a name/path/
   signature is uncertain, surface the confusion instead of running with a guess. One good
   clarifying question beats a confident wrong edit.

2. **Simplicity first.** Write the minimum code that solves the stated problem. No speculative
   abstractions, no "while I'm here" generalizing. For this scientific code, prefer the plain,
   readable form a future reader can follow over the clever one.

3. **Surgical changes.** Touch only what the task requires. Do not refactor, reformat, or
   re-style adjacent code, and do not reorganize imports or rename things that weren't asked
   for. If you spot something worth changing nearby, note it separately — don't fold it in.

4. **Goal-driven execution — define "done" as a passing test.** State what "done" looks like
   before you start, and use tests as the success criterion, not vibes. By language:
   - **R** — `devtools::test()` and `R CMD check` pass, examples run clean.
   - **Python** — the relevant `pytest` / doctests pass.
   - **C** — it compiles clean with warnings on (`-Wall -Wextra`) and its checks pass:
     `make check` / `ctest` for standalone code, or, for C compiled inside an R package,
     `R CMD check` builds the native routines and the R tests that exercise them pass.
     Run under a sanitizer (`-fsanitize=address,undefined`) when touching memory or pointers.
   - **SAS** — the log is clean (no ERROR, no unexpected WARNING or uninitialized-variable
     notes), and output validates against a known reference — `PROC COMPARE` against a
     baseline dataset, or check figures against previously verified results. A run that
     "finished" is not the same as a run that's correct; read the log.

   If there's no test covering the change, add or propose one rather than declaring success
   from inspection.

## Voice
Prose in vignettes, README, roxygen `@description`/`@details`, and release/post copy follows
the `ehrlinger-writing` harness — my voice, reader persona, project context. Apply it for any
documentation text in this repo.
