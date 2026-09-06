# GLOBAL AGENTS.md (~/.pi/agent/AGENTS.md)

Universal directives, operational guardrails, and engineering standards for all repositories.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119 / RFC 8174.

## 1. Agent Execution and Communication Protocol

* The agent **MUST** keep outputs direct, lean, and plain (maximum signal, zero noise).
* The agent **MUST NOT** generate conversational filler, pleasantries, apologies, status openers, or closing remarks.
* The agent **MUST** report facts, decisions, touched file paths, diff summaries, and verification outputs immediately.
* If instructions, parameters, or specifications lack clarity or conflict, the agent **MUST** halt immediately and request clarification before editing files.

## 2. Operational Guardrails and Safety

* The agent **MUST** inspect existing codebase conventions, shared modules, and utilities before creating files or functions.
* The agent **MUST NOT** modify files outside the direct scope of the user prompt.
* The agent **MUST NOT** perform unsolicited refactoring or reformat untouched files.
* The agent **MUST NOT** install external libraries, packages, or tools without explicit permission.
* The agent **MUST** use existing workspace dependencies or standard language libraries.
* The agent **MUST** execute the project test runner, static linter, and type checker after modifying code.
* The agent **MUST NOT** report task completion if verification commands fail or report regressions.
* The agent **MUST NOT** execute destructive commands, including `git push --force`, `git reset --hard`, `rm -rf`, or database drops.
* The agent **MUST NOT** write, log, stage, or commit secrets, credentials, private keys, or environment files (`.env`).

## 3. Engineering Language Standard

* All engineering artifacts **MUST** be authored exclusively in English:
  * Source code identifiers (variables, functions, classes, types, interfaces, schemas).
  * Code comments, docstrings, and technical documentation.
  * Git commit messages, branch names, and pull request descriptions.
  * Configuration files, infrastructure code, and schema definitions.

## 4. Software Engineering Standards

* Logic **MUST** adhere to DRY (Don't Repeat Yourself); duplicate business logic **MUST NOT** be introduced.
* The agent **MUST** follow YAGNI (You Aren't Gonna Need It); speculative abstractions and unused wrapper utilities **MUST NOT** be created.
* The agent **MUST** follow KISS (Keep It Simple, Stupid); straightforward implementations **MUST** be chosen over complex patterns.
* Modular composition and pure functions **MUST** be preferred over inheritance hierarchies.
* Code additions **MUST** follow TDD (Test-Driven Development):
  1. The agent **MUST** write or locate the automated test covering the requirement.
  2. The agent **MUST** run the test and confirm initial failure.
  3. The agent **MUST** implement the minimal code required to pass the test.
  4. The agent **MUST** run the test and verify that it passes.
  5. The agent **SHOULD** refactor while keeping tests green.
* Commit messages **MUST** follow the Conventional Commits specification strictly:
  * `feat(<scope>): <description>`
  * `fix(<scope>): <description>`
  * `refactor(<scope>): <description>`
  * `test(<scope>): <description>`
  * `docs(<scope>): <description>`
  * `chore(<scope>): <description>`

## 5. Baseline Defensive Engineering

* The agent **MUST** treat all external inputs, file payloads, and network data as untrusted.
* Database and execution layers **MUST** use parameterized queries and safe data abstraction interfaces.
* The agent **MUST NOT** concatenate or interpolate user input directly into SQL queries, system shell commands, or regular expressions.
* Client-facing responses **MUST NOT** expose stack traces, database internals, system paths, or environment variables.
* Cryptographic implementations **MUST** use standard, audited libraries. Custom cryptographic routines **MUST NOT** be written.

