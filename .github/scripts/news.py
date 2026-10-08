"""NEWS fragments: the pull-request guard and the bump-time collector.

Every pull request used to add its NEWS.md entry at the same line, the top of
the standing `# <package> (unreleased)` section, so any two pull requests open
at once conflicted there. Measured on hvtiRtemplates 2026-10-07: every
conflict among its open pull requests was in NEWS.md and nowhere else. Each
pull request now writes its entry to a file of its own, `news/<branch>.md`
with `/` in the branch name replaced by `-`, holding the bullet or bullets
exactly as they will read in NEWS.md and no heading. No two pull requests
touch the same file, so the conflict is gone.

    python3 .github/scripts/news.py check --base origin/main
        CI. Fails a pull request that changes a file the package ships and
        adds no fragment. A change ships nothing when R's built-in build
        exclusions or the base branch's `.Rbuildignore` cover every file it
        touches; the base's copy, so a pull request cannot exempt itself. The
        bump consumes fragments rather than adding one, so a pull request that
        moves `Version:` in DESCRIPTION passes too. So does one that deletes
        fragments and edits NEWS.md and otherwise ships nothing: some packages
        collect into a version heading that already exists, without moving
        `Version:`.

    python3 .github/scripts/news.py collect
        The bump commit, after `Version:` in DESCRIPTION has moved. Writes the
        fragments into NEWS.md under that version's heading, in merge order,
        and deletes them. A legacy `(unreleased)` section is folded in ahead
        of them, and the DCF `Version:` line some NEWS.md files carry above
        the first heading is updated to match.

The same file is copied into every package in the family; change them
together. Standard library only, so no pip install step on the runner.
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

FRAGMENT_RE = re.compile(r"^news/[^/]+\.md$")

# `tools:::inRbuildignore()` tests these before a package's own `.Rbuildignore`,
# which is why `.Rbuildignore` itself never ships. Copied from
# `tools:::get_exclude_patterns()` in R 4.6.1, as hvtiR's check_version.py does.
R_BUILD_EXCLUDES = [
    r"^\.Rbuildignore$", r"(^|/)\.DS_Store$", r"^\.(RData|Rhistory)$",
    r"~$", r"\.bak$", r"\.sw.$", r"(^|/)\.#[^/]*$", r"(^|/)#[^/]*#$",
    r"^TITLE$", r"^data/00Index$", r"^inst/doc/00Index\.dcf$",
    r"^config\.(cache|log|status)$", r"(^|/)autom4te\.cache$",
    r"^src/.*\.d$", r"^src/Makedeps$", r"^src/so_locations$",
    r"^inst/doc/Rplots\.(ps|pdf)$", r"^(GPATH|GRTAGS|GTAGS)$",
]


def read_field(text: str, field: str) -> str | None:
    match = re.search(rf"^{field}:\s*(\S+)", text, re.M)
    return match.group(1) if match else None


def ships_nothing(paths: list, patterns: list) -> bool:
    """Whether `R CMD build` would leave out every changed file.

    Each pattern is a case-insensitive Perl regex tested against paths
    relative to the package root, directories included, so a file is out when
    it or any directory above it matches. An empty list of paths or of
    patterns fails closed: it is never "ships nothing".
    """
    if not paths or not patterns:
        return False
    try:
        regexes = [re.compile(p, re.IGNORECASE)
                   for p in R_BUILD_EXCLUDES + list(patterns)]
    except re.error as exc:
        raise ValueError(f".Rbuildignore pattern does not compile: {exc}") from None

    def excluded(path: str) -> bool:
        parts = path.split("/")
        prefixes = ["/".join(parts[:i]) for i in range(1, len(parts) + 1)]
        return any(rx.search(pre) for pre in prefixes for rx in regexes)

    return all(excluded(p) for p in paths)


def check(changed: list, added: dict, patterns: list,
          base_version: str | None, head_version: str | None,
          deleted: list = ()) -> list:
    """Problems with a pull request's NEWS fragment. Empty means it passes.

    `added` maps each file the pull request adds to its content; `deleted`
    lists the files it removes.
    """
    if base_version and head_version and base_version != head_version:
        return []
    consumed = [p for p in deleted if FRAGMENT_RE.match(p)]
    if "NEWS.md" in changed and consumed:
        rest = [p for p in changed if p != "NEWS.md" and p not in consumed]
        if not rest or ships_nothing(rest, patterns):
            return []
    if ships_nothing(changed, patterns):
        return []
    fragments = {p: t for p, t in added.items() if FRAGMENT_RE.match(p)}
    if not fragments:
        return [
            "This pull request changes files the package ships and adds no NEWS "
            "fragment. Write its entry to news/<branch>.md (with '/' in the "
            "branch name replaced by '-'): the bullet or bullets exactly as they "
            "will read in NEWS.md, no heading. A change ships nothing, and needs "
            "no fragment, only when R's build exclusions or the base branch's "
            ".Rbuildignore cover every file it touches."
        ]
    return [f"{p} is empty." for p, t in sorted(fragments.items()) if not t.strip()]


def merge_order(log_names: list, present: list) -> list:
    """Fragments in the order the commits that added them landed.

    `log_names` is `git log --diff-filter=A --format= --name-only -- news/`,
    newest first. A name can recur across release cycles, so each fragment
    takes the position of its newest add. Fragments git has no record of yet
    come last, sorted.
    """
    present_set, seen = set(present), []
    for name in log_names:
        if name in present_set and name not in seen:
            seen.append(name)
    seen.reverse()
    return seen + sorted(set(present) - set(seen))


def _is_setext_rule(line: str) -> bool:
    return bool(re.fullmatch(r"=+\s*", line))


def split_sections(lines: list) -> tuple:
    """Split NEWS.md into its preamble and its level-one sections.

    A level-one heading is ATX (`# title`) or setext (a title line with a rule
    of `=` underneath). Lines inside a fenced code block are never headings:
    an R comment there starts with `# ` too. Returns
    (preamble, [(heading_lines, body_lines)]).
    """
    starts, fenced = [], False
    for i, line in enumerate(lines):
        if re.match(r"\s*(```|~~~)", line):
            fenced = not fenced
        elif fenced:
            continue
        elif re.match(r"#\s", line):
            starts.append((i, 1))
        elif (i + 1 < len(lines) and line.strip()
              and _is_setext_rule(lines[i + 1])):
            starts.append((i, 2))
    preamble = lines[:starts[0][0]] if starts else lines
    sections = []
    for k, (i, n) in enumerate(starts):
        end = starts[k + 1][0] if k + 1 < len(starts) else len(lines)
        sections.append((lines[i:i + n], lines[i + n:end]))
    return preamble, sections


def _trim(lines: list) -> list:
    while lines and not lines[0].strip():
        lines = lines[1:]
    while lines and not lines[-1].strip():
        lines = lines[:-1]
    return lines


def collect(news: str, package: str, version: str, fragments: list) -> str:
    """NEWS.md with the fragments filed under `version`.

    `fragments` is the fragment texts in merge order. A legacy
    `# <package> (unreleased)` section is removed and its entries go first,
    since they merged before any fragment existed. When a heading for
    `version` is already present, as ggRandomForests' `(development)` section
    is, the entries are appended to it; otherwise a new heading goes above
    the newest release, in the file's own heading style.
    """
    preamble, sections = split_sections(news.splitlines())
    entries, kept = [], []
    for heading, body in sections:
        if re.search(r"\(unreleased\)\s*$", heading[0], re.I):
            if _trim(body):
                entries.append(_trim(body))
        else:
            kept.append((heading, body))
    entries += [_trim(f.splitlines()) for f in fragments if f.strip()]
    if not entries:
        raise ValueError("nothing to collect: no fragments in news/ and no "
                         "(unreleased) section in NEWS.md")

    block = []
    for e in entries:
        block += e + [""]

    version_re = re.compile(rf"(^|[\s(v]){re.escape(version)}($|[\s)])")
    target = next((k for k, (h, _) in enumerate(kept)
                   if version_re.search(h[0])), None)
    if target is not None:
        heading, body = kept[target]
        kept[target] = (heading, [""] + _trim(body) + [""] + block)
    else:
        title = f"{package} {version}"
        setext = bool(kept) and len(kept[0][0]) == 2
        heading = [title, "=" * len(title)] if setext else [f"# {title}"]
        kept.insert(0, (heading, [""] + block))

    preamble = [re.sub(r"^Version:\s*\S+", f"Version: {version}", ln)
                for ln in preamble]
    out = list(preamble)
    if out and out[-1].strip():
        out.append("")
    for heading, body in kept:
        out += heading + body
    return "\n".join(_trim(out)) + "\n"


def _git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True,
                          text=True).stdout


def _show(ref: str, path: str) -> str:
    try:
        return _git("show", f"{ref}:{path}")
    except subprocess.CalledProcessError:
        return ""


def run_check(base: str) -> int:
    changed = _git("diff", "--name-only", f"{base}...HEAD").split()
    added = {p: Path(p).read_text() for p in
             _git("diff", "--name-only", "--diff-filter=A", f"{base}...HEAD").split()
             if FRAGMENT_RE.match(p)}
    patterns = [ln for ln in _show(base, ".Rbuildignore").splitlines() if ln.strip()]
    deleted = _git("diff", "--name-only", "--diff-filter=D", f"{base}...HEAD").split()
    problems = check(changed, added, patterns,
                     read_field(_show(base, "DESCRIPTION"), "Version"),
                     read_field(Path("DESCRIPTION").read_text(), "Version"),
                     deleted)
    if problems:
        print("news check failed:", file=sys.stderr)
        for p in problems:
            print(f"  - {p}", file=sys.stderr)
        return 1
    print("news ok")
    return 0


def run_collect() -> int:
    desc = Path("DESCRIPTION").read_text()
    package, version = read_field(desc, "Package"), read_field(desc, "Version")
    paths = sorted(str(p) for p in Path("news").glob("*.md"))
    log = _git("log", "--diff-filter=A", "--format=", "--name-only",
               "--", "news/").split()
    ordered = merge_order(log, paths)
    news = Path("NEWS.md")
    try:
        news.write_text(collect(news.read_text(), package, version,
                                [Path(p).read_text() for p in ordered]))
    except ValueError as exc:
        print(f"news collect: {exc}", file=sys.stderr)
        return 1
    for p in ordered:
        Path(p).unlink()
    print(f"Filed {len(ordered)} fragment(s) under {package} {version}. "
          "Review NEWS.md, then commit it with DESCRIPTION and the deletions.")
    return 0


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    p_check = sub.add_parser("check", help="CI guard for a pull request")
    p_check.add_argument("--base", required=True, help="base ref, e.g. origin/main")
    sub.add_parser("collect", help="file the fragments into NEWS.md")
    args = parser.parse_args(argv)
    return run_check(args.base) if args.command == "check" else run_collect()


if __name__ == "__main__":
    sys.exit(main())
