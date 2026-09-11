# GLOBAL AGENTS.md

## Communication & Tools
* Outputs MUST be direct and lean: no filler, pleasantries, emojis, apologies, or conversational openers/closers.
* MUST report facts, decisions, touched files, and verification outcomes immediately; NEVER duplicate full diffs in markdown text when tool outputs already show them.
* BEFORE executing any `write` or `edit`, MUST use `ask_user_question` IF requirements are ambiguous, conflicting, or require unstated architectural decisions; NEVER make speculative assumptions or silent design trade-offs.
* MUST use Pi native tools (`ls`, `find`, `grep`, `read`, `write`, `edit`) — NEVER use bash (`ls`, `find`, `grep`, `cat`, `head`, `tail`, `sed`, `awk`, `echo`, `rg`, `fd`, `bat`) or scripts for workspace exploration, reads, or edits.
* `bash` MUST be reserved strictly for project `make` targets (`make test`, `make verify`, etc.), compilation, and local git commands (`git status`, `git diff`, `git add`, `git commit`).
* WHEN tasks involve Web UI AND browser tools are available, MUST use `agent-browser` tools to inspect and verify behavior.

## Context Discovery & Skills
* MUST check for and read project-specific `AGENTS.md` and root `Makefile` before taking action.
* BEFORE generating any plan, task decomposition, spec, proposal, or code review, MUST inspect relevant project documentation, `llms.txt`, or installed package source code to verify real interfaces and signatures; NEVER base plans or reviews on unverified assumptions or model memory.
* MUST prioritize local project documentation, vendor guidelines, and `llms.txt` endpoints over internal model training; NEVER guess framework APIs when documentation is accessible.
* MUST use Pi native `read` to inspect third-party library source code within workspace dependency directories whenever types, signatures, or runtime behaviors require verification.
* WHEN a specialized domain or framework skill exists in the environment, MUST invoke that skill before planning or modifying code.
* WHEN a task introduces a new feature, fixes a non-trivial bug, touches multiple files, or requires architectural planning, MUST copy `~/.pi/agent/templates/SPEC.md` to `docs/specs/<kebab-name>.md` and fill out all sections before writing code; NEVER execute implementation tasks directly in chat or ad-hoc markdown files.

## Scope & Edits
* MUST inspect existing project patterns, shared utilities, and conventions before creating new files or functions.
* MUST NOT modify files outside the prompt's scope; MUST NOT reformat or refactor untouched files.
* Edits MUST be surgical with minimal, targeted changes only; NEVER rewrite whole files when block edits suffice; NEVER churn preserved logic, comments, or imports.
* MUST NOT install new dependencies without explicit permission; ALWAYS rely on existing workspace dependencies or standard libraries instead.

## Verification & Testing
* MUST run relevant workspace checks (`make format_check`, `make lint`, `make static_analysis`) before reporting completion; NEVER introduce new test failures, lint errors, or regressions.
* WHEN an existing test suite is present, MUST execute the strict TDD cycle for all logic changes:
  1. RED: MUST write a targeted test first and MUST run it via bash (`make test TARGET=<path>`) to confirm it fails on an assertion or an expected missing-interface compilation error.
  2. GREEN: MUST write the minimal code required to pass and MUST run the targeted test via bash (`make test TARGET=<path>`) to confirm success.
  3. REFACTOR: MUST clean up newly written code (applying DRY/KISS) without modifying public interfaces or untouched files, and MUST re-run the targeted test to verify it remains green.
* MUST run `make verify` prior to final task completion.
* WHEN a test runner, compiler, linter, or verification gate fails unexpectedly:
  1. MUST NOT edit source code or re-run bash commands until `docs/specs/<kebab-name>.md` is updated.
  2. MUST use native `edit` to increment `Try Counter` (e.g., from `1 / 3` to `2 / 3`) and populate the matching `### Try N` log with failure point, raw terminal error, root cause, and correction strategy.
  3. WHEN `Try Counter` reaches its maximum limit (3 / 3) without passing all checks, MUST update `Status` to `blocked`, halt all edits, and invoke `ask_user_question`.

## Engineering Standards & Localization
* All source code, internal identifiers, schema definitions, comments, documentation, specs, and commits MUST be authored in English following `en-US` locale conventions.
* All dates in technical documentation, specifications, architectural decision records (ADRs), and commit messages MUST use strict ISO 8601 (`YYYY-MM-DD`); NEVER use localized date formats (`DD-MM-YYYY` or `MM/DD/YYYY`) in documentation, code, or specs.
* All user-facing UI copy and display formatting MUST follow `es-MX` conventions and the `America/Mexico_City` timezone; UI display dates MUST use `DD-MM-YYYY` and UI display times MUST use 24-hour `HH:mm:ss`.
* Internal and non-user-facing operations (databases, API contracts, serialization, telemetry, logs) MUST ALWAYS use UTC and strict ISO 8601 formatting (`YYYY-MM-DDTHH:mm:ss.sssZ`); NEVER persist or transmit non-ISO localized date/time strings across boundaries.
* MUST apply DRY, YAGNI, and KISS within the task scope; MUST prefer modular composition and pure functions over inheritance.
* BEFORE creating any commit:
  1. MUST inspect `git diff` and the active `docs/specs/<spec_id>.md` or governing ADR to verify all touched behavior.
  2. MUST update `CHANGELOG.md` under the `[Unreleased]` section following Keep a Changelog conventions (`Added`, `Changed`, `Fixed`, `Removed`).
  3. MUST synchronize `README.md` IF interfaces, setup instructions, architecture, workflows, modules, or user roles were added or modified.
* Commits MUST follow Conventional Commits: `feat|fix|refactor|test|docs|chore(<scope>): <description>`.
* ALWAYS apply all directives in this document across all work stages: planning, task splitting, proposals, specs, implementation, and code review.

## UI/UX & Design Systems
* MUST ALWAYS use the project's centralized design system across all views, layouts, and components to enforce visual consistency, layout coherence, and streamlined UX.
* Components MUST be self-contained, reusable, and governed by explicit prop/data contracts; NEVER build one-off custom components when design-system primitives exist.
* Layouts MUST be mobile-first, fluid, and responsive; MUST NOT use fixed container widths that cause horizontal scrolling on viewports.
* MUST reference centralized design system semantic tokens for colors, spacing, typography, and border radii; NEVER use hardcoded pixel dimensions or raw color codes.
* Visual styling MUST map directly to semantic theme tokens (e.g., surface, background, text-primary, accent) supporting light and dark modes.
* Dashboards MUST prioritize high information density, workspace ergonomics, and explicit state visibility.

## Security & Architecture
* Every endpoint, route, procedure, resolver, and view MUST require authentication (AuthN) and authorization (AuthZ) by default; public access MUST NOT be granted without an explicit decoration or registration in a public allowlist.
* MUST enforce two-stage access verification before executing business logic: verify identity and token validity first, then verify roles, tenant ownership, and permissions.
* Object ownership MUST be validated at the data-access layer on every request to prevent BOLA/IDOR vulnerabilities.
* Backend input validation MUST be authoritative and enforced using strict schema allowlists (types, lengths, ranges, formats); client-side validation MUST NOT be trusted for security enforcement.
* MUST treat all external input as untrusted; MUST use parameterized queries and safe interfaces; NEVER interpolate input into shell commands, SQL, or regexes.
* The backend MUST NOT fetch arbitrary user-supplied URLs; outbound network calls MUST match an explicit destination allowlist and MUST NOT resolve to private, loopback, or cloud metadata IP addresses (SSRF mitigation).
* Session cookies MUST ALWAYS include `HttpOnly`, `Secure`, and `SameSite` flags.
* NEVER run destructive commands: `git push` (any variant), `git reset --hard`, `git clean -fd`, `git restore .`, production database drops, or file deletions outside temporary build directories (`dist/`, `build/`, `_build/`, `target/`, `.cache/`, `.elixir_ls/`).
* NEVER read, print, stage, or commit `.env` files, credentials, or private keys; reading `.env.example` or documentation templates is permitted.
* NEVER expose stack traces, internal absolute paths, or database internals to client outputs.
* MUST use audited standard cryptographic libraries; NEVER write custom crypto routines.
