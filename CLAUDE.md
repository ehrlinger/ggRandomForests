@AGENTS.md

# Claude Code specifics

[`AGENTS.md`](AGENTS.md), imported above, is the operational contract and applies in full. It is written to
be tool neutral so that Codex and other agents read the same rules. Only the Claude Code
affordances live here.

## Before you touch code

`AGENTS.md` says to orient on the public API surface before editing. In Claude Code the way to
do that is the codemap: it lives in the Obsidian vault under `Claude/repomaps/` and is read via
the `read-codemap` skill (`/codemap ggRandomForests`). If the codemap looks stale, say so and
offer to refresh it (`/regenerate-codemap`) rather than working from a guess.

## Prose

`AGENTS.md` points at `.claude/house-style.md` for the house voice. In Claude Code, apply the
`ehrlinger-writing` skill instead: it carries the same voice, reader persona and project
context, kept in sync from the vault sources that `house-style.md` is composed from.
