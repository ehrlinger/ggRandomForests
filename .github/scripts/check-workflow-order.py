#!/usr/bin/env python3
"""Fail when a workflow job runs an R-dependent setup action before setup-r.

r-lib/actions/setup-tinytex and setup-r-dependencies both shell out to R, and
GitHub runs the steps of a job strictly top-to-bottom with no dependency graph
(`needs:` sequences jobs, not steps). So the only thing expressing "R must
exist first" is physical order in the file, and nothing in YAML will warn you
when it is wrong.

Getting it wrong does not reliably fail at the offending step: the runner image
sometimes ships an R, so the action may install against an R the rest of the
job never uses and still exit 0. The symptom then surfaces much later as a
missing-LaTeX failure during the manual build. That indirection is why the
ordering bug reached eight repositories before anyone noticed.

Usage:
    check-workflow-order.py [PATH ...]

PATH may be a workflow file or a directory searched for *.yml / *.yaml.
Defaults to .github/workflows. Exits 1 if any violation is found.
"""

from __future__ import annotations

import glob
import os
import sys

try:
    import yaml
except ImportError:  # pragma: no cover - surfaced as a clear message, not a traceback
    sys.exit("error: PyYAML is required (pip install pyyaml)")

SETUP_R = "r-lib/actions/setup-r"

# Action -> why it needs R, used verbatim in the failure message.
NEEDS_R = {
    "r-lib/actions/setup-tinytex": "installs the TeX distribution via R",
    "r-lib/actions/setup-r-dependencies": "resolves and installs packages via R",
}


class _LineLoader(yaml.SafeLoader):
    """SafeLoader that records the source line of every mapping."""


def _construct_mapping(loader, node):
    mapping = yaml.SafeLoader.construct_mapping(loader, node, deep=True)
    mapping["__line__"] = node.start_mark.line + 1
    return mapping


_LineLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _construct_mapping
)


def action_path(uses: str) -> str:
    """'r-lib/actions/setup-r@v2' -> 'r-lib/actions/setup-r'.

    Splitting on '@' matters: a naive startswith() would treat
    setup-r-dependencies as setup-r, since one is a prefix of the other.
    """
    return uses.split("@", 1)[0].strip()


def iter_workflow_files(paths):
    for path in paths:
        if os.path.isdir(path):
            found = []
            for ext in ("yml", "yaml"):
                found.extend(glob.glob(os.path.join(path, "*." + ext)))
            yield from sorted(found)
        else:
            yield path


def check_file(path):
    """Return a list of (line, message) violations for one workflow file."""
    with open(path, encoding="utf-8") as handle:
        try:
            doc = yaml.load(handle, _LineLoader)
        except yaml.YAMLError as exc:
            return [(1, "could not be parsed as YAML: %s" % exc)]

    if not isinstance(doc, dict):
        return []

    violations = []
    for job_name, job in (doc.get("jobs") or {}).items():
        if job_name == "__line__" or not isinstance(job, dict):
            continue
        steps = job.get("steps")
        if not isinstance(steps, list):
            continue  # reusable-workflow call (job-level `uses:`), nothing to order

        setup_r_index = None
        uses_steps = []
        for index, step in enumerate(steps):
            if not isinstance(step, dict):
                continue
            uses = step.get("uses")
            if not isinstance(uses, str):
                continue
            path_only = action_path(uses)
            uses_steps.append((index, path_only, step.get("__line__", 1)))
            if path_only == SETUP_R and setup_r_index is None:
                setup_r_index = index

        for index, path_only, line in uses_steps:
            reason = NEEDS_R.get(path_only)
            if reason is None:
                continue
            if setup_r_index is None:
                violations.append(
                    (
                        line,
                        "job '%s': %s %s, but this job never runs %s"
                        % (job_name, path_only, reason, SETUP_R),
                    )
                )
            elif index < setup_r_index:
                violations.append(
                    (
                        line,
                        "job '%s': %s %s, so it must run after %s "
                        "(currently step %d, setup-r is step %d)"
                        % (
                            job_name,
                            path_only,
                            reason,
                            SETUP_R,
                            index + 1,
                            setup_r_index + 1,
                        ),
                    )
                )
    return violations


def main(argv):
    paths = argv[1:] or [os.path.join(".github", "workflows")]
    files = list(iter_workflow_files(paths))
    if not files:
        print("no workflow files found in: %s" % ", ".join(paths))
        return 0

    on_actions = os.environ.get("GITHUB_ACTIONS") == "true"
    total = 0
    for path in files:
        violations = check_file(path)
        total += len(violations)
        for line, message in violations:
            print("%s:%d: error: %s" % (path, line, message))
            if on_actions:
                # Renders the failure inline on the pull request diff.
                print(
                    "::error file=%s,line=%d::%s" % (path, line, message.replace("\n", " "))
                )

    if total:
        print(
            "\n%d ordering violation(s) in %d workflow file(s)." % (total, len(files))
        )
        return 1

    print("checked %d workflow file(s): setup-r ordering OK" % len(files))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
