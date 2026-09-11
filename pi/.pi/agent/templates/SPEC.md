# Spec: {Short Feature or Fix Title}

**Date**: {YYYY-MM-DD}  
**Status**: draft | in-progress | blocked | complete  
**Complexity**: small (1-2 files) | medium (3-5 files) | large (>5 files - consider splitting)  
**Try Counter**: 1 / 3  

## What & Why
{Concise description of the change, user impact, and immediate motivation.}

## Context & Scope
* **Governing ADRs**: {`docs/adr/000.md` or None}
* **Touched & Reference Files**:

| File | Role / Impact |
|------|---------------|
| `path/to/target_file.ext` | Modify — {specific change} |
| `path/to/caller.ext` | Context — {read-only reference} |
| `path/to/test_file.ext` | Test — {verify behavior} |

## Requirements
1. {Testable requirement 1 — clear input/output or behavior}
2. {Testable requirement 2}
3. {Negative constraint: what this change MUST NOT alter or break}

## Interface Contracts & Schemas
> **Scope:** Optional. Omit or leave empty if no contracts, types, or schemas change.

```text
// Define exact types, schemas, or function signatures here
```

## Emergent Decisions & Notes
> **Scope:** Document non-obvious design choices or structural additions made during implementation.

* {If none, state: "Standard implementation following existing conventions; no architectural trade-offs."}

## Implementation Tasks
> **Scope:** Break work into atomic steps. Each step MUST follow the TDD loop (RED -> GREEN -> REFACTOR) using `make test TARGET=path/to/test`.

- [ ] 1. {Atomic Task 1: e.g., Implement data structure / schema changes + test}
- [ ] 2. {Atomic Task 2: e.g., Implement business logic / context functions + test}
- [ ] 3. {Atomic Task 3: e.g., Implement UI / endpoint integration + test}
- [ ] 4. Run full verification gate: `make verify`
- [ ] 5. Inspect `git diff` and sync `CHANGELOG.md` (and `README.md` if interfaces, setup, architecture, or roles changed; otherwise mark N/A)
- [ ] 6. Update this spec's `Status` to `complete`
- [ ] 7. Stage all changes including docs: `git add docs/specs/ CHANGELOG.md <touched paths>` and commit: `git commit -m "type(scope): message"`
- [ ] 8. Confirm clean tree: verify `git status` outputs "nothing to commit, working tree clean"

## Retry & Failure Log
> **Scope:** Populate ONLY when an attempt fails. Append `### Try N` blocks dynamically.

### Try 1
* **Failure Point**: {Failing target or test name}
* **Terminal / Error Output**:
```text
{Paste exact error message or stack trace here}
```
* **Root Cause**: {Technical explanation of why it failed}
* **Correction Strategy (Try 2)**: {Concrete code or architectural fix for the next attempt}
* **Spec Deviation / Micro-ADR**: {Document only if design changed from original requirements}

## Done When
> **Scope:** Completion criteria. All conditions below MUST be true before closing this spec.

- [ ] All implementation tasks are checked off.
- [ ] Spec `Status` is marked `complete`.
- [ ] `make verify` passes with zero regressions, lint warnings, or security alerts.
- [ ] `git status` reports "nothing to commit, working tree clean" (commit landed; push is intentionally not in scope).
- [ ] Try Counter did not exceed maximum limit.
