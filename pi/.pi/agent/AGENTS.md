# GLOBAL AGENTS.md

## Communication
* Outputs MUST be direct and lean: no filler, pleasantries, apologies, or status openers/closers.
* MUST report facts, decisions, touched files, diffs, and verification results immediately.
* If instructions are unclear or conflicting, MUST halt and ask before editing files.

## Tool Discipline
* Use pi native tools (`ls`, `find`, `grep`, `read`, `write`, `edit`) — NEVER bash `ls/find/grep/cat/sed/awk` for inspection, exploration, reads, or edits.
* `bash` MUST be reserved for builds, compilation, tests, package management, and git only.

## Scope & Edits
* MUST inspect existing conventions and shared utilities before creating files or functions.
* MUST NOT modify files outside the prompt's scope; MUST NOT refactor or reformat untouched files.
* Edits MUST be surgical: minimal, targeted lines only. NEVER full-file rewrites when a block edit suffices. NEVER churn preserved logic, comments, or imports.
* MUST NOT install dependencies without explicit permission; use existing workspace or standard-library deps first.

## Verification
* After code changes, MUST run tests, linter, and type checker. NEVER report completion with failing checks.
* NEVER run destructive commands: `git push --force`, `git reset --hard`, `rm -rf`, DB drops.
* NEVER write, stage, or commit secrets or `.env` files.

## Engineering Standards
* Everything MUST be authored in English (code, comments, docs, commits, configs).
* DRY: MUST NOT duplicate logic. YAGNI: MUST NOT create speculative abstractions. KISS: MUST choose the simplest working design.
* MUST prefer modular composition and pure functions over inheritance.
* TDD MUST follow: Red (failing test) → Green (minimal implementation) → Refactor, then full verification gate, then commit.
* Commits MUST follow Conventional Commits: `feat|fix|refactor|test|docs|chore(<scope>): <description>`.

## Security
* MUST treat all external input as untrusted. MUST use parameterized queries and safe abstractions.
* NEVER interpolate user input into SQL, shell commands, or regexes.
* NEVER expose stack traces, DB internals, paths, or env vars to clients.
* Use standard audited crypto only; NEVER write custom cryptographic routines.
