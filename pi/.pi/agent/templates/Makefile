.DEFAULT_GOAL := help
SHELL := /usr/bin/env bash

# ==============================================================================
# Variables & Arguments
# ==============================================================================
TARGET ?=
ARGS ?=
STEPS ?= 1

# ==============================================================================
# Execution & Server
# ==============================================================================

## run: Start the local application development server
.PHONY: run
run:
	@echo "==> Starting application..."
	# Phoenix: mix phx.server
	# Node/TS: npm run dev
	# Python:  poetry run uvicorn app.main:app --reload
	# Rust:    cargo run

# ==============================================================================
# Quality, Verification & Static Analysis (Strict Mode)
# ==============================================================================

## format: Automatically format all workspace source files
.PHONY: format
format:
	@echo "==> Formatting code..."
	# Phoenix: mix format
	# Node/TS: npx prettier --write .
	# Python:  poetry run black . && poetry run isort .
	# Rust:    cargo fmt

## format_check: Check formatting without altering files (fails if unformatted)
.PHONY: format_check
format_check:
	@echo "==> Checking code format..."
	# Phoenix: mix format --check-formatted
	# Node/TS: npx prettier --check .
	# Python:  poetry run black --check . && poetry run isort --check-only .
	# Rust:    cargo fmt -- --check

## lint: Run linters in strict mode (fails on any stylistic or design warning)
.PHONY: lint
lint:
	@echo "==> Running linters..."
	# Phoenix: mix credo --strict
	# Node/TS: npx eslint . --max-warnings 0
	# Python:  poetry run ruff check .
	# Rust:    cargo clippy -- -D warnings

## security: Run security vulnerability scanners (fails on detected findings)
.PHONY: security
security:
	@echo "==> Running security audit..."
	# Phoenix: mix sobelow --config --exit low
	# Node/TS: npm audit --audit-level=moderate
	# Python:  poetry run bandit -r src/ -ll
	# Rust:    cargo audit

## static_analysis: Fast, zero-tolerance static analysis (compiler, lint, security)
.PHONY: static_analysis
static_analysis:
	@echo "==> Running static analysis..."
	# Phoenix: mix compile --warnings-as-errors
	# Node/TS: npx tsc --noEmit
	# Rust:    cargo check
	@$(MAKE) lint
	@$(MAKE) security

## test: Execute tests (supports TARGET=path/to/test or ARGS="...")
.PHONY: test
test:
	@echo "==> Running tests..."
	@if [ -n "$(TARGET)" ]; then \
		echo "Targeted execution: $(TARGET)"; \
		# Phoenix: mix test $(TARGET) \
		# Node/TS: npm test -- $(TARGET) \
		# Python:  poetry run pytest $(TARGET) \
		# Rust:    cargo test --test $(TARGET); \
	else \
		# Phoenix: mix test $(ARGS) \
		# Node/TS: npm test -- $(ARGS) \
		# Python:  poetry run pytest $(ARGS) \
		# Rust:    cargo test -- $(ARGS); \
	fi

## precommit: Fast pre-commit gate (format check, lint, security)
.PHONY: precommit
precommit: format_check lint security
	@echo "==> Fast pre-commit checks passed cleanly."

## verify: Comprehensive gate (format check, static analysis, full test suite)
.PHONY: verify
verify: format_check static_analysis
	@echo "==> Running full test suite for verification..."
	@$(MAKE) test
	@echo "==> Full verification gate passed cleanly."

# ==============================================================================
# Database Management (Local Development Only)
# ==============================================================================

## db_setup: Initialize database, run migrations, and apply seeds
.PHONY: db_setup
db_setup:
	@echo "==> Initializing and setting up database..."
	# Phoenix: mix ecto.setup
	# Node/TS: npx prisma migrate dev
	# Python:  poetry run python manage.py migrate
	# Rust:    sqlx database setup

## db_migrate: Apply pending database schema migrations
.PHONY: db_migrate
db_migrate:
	@echo "==> Applying database migrations..."
	# Phoenix: mix ecto.migrate
	# Node/TS: npx prisma migrate deploy
	# Python:  poetry run python manage.py migrate
	# Rust:    sqlx migrate run

## db_rollback: Roll back migration step(s) (supports STEPS=N)
.PHONY: db_rollback
db_rollback:
	@echo "==> Rolling back $(STEPS) migration step(s)..."
	# Phoenix: mix ecto.rollback --step $(STEPS)
	# Node/TS: npx knex migrate:rollback
	# Python:  poetry run alembic downgrade -$(STEPS)
	# Rust:    sqlx migrate revert

## db_reset: Drop, recreate, migrate, and seed local development database
.PHONY: db_reset
db_reset:
	@echo "==> Resetting local development database..."
	# Phoenix: mix ecto.reset
	# Node/TS: npx prisma migrate reset --force
	# Python:  poetry run python manage.py reset_db --noinput && $(MAKE) db_setup
	# Rust:    sqlx database drop -y && $(MAKE) db_setup

## db_seed: Populate database with baseline seed fixtures
.PHONY: db_seed
db_seed:
	@echo "==> Seeding database..."
	# Phoenix: mix run priv/repo/seeds.exs
	# Node/TS: npx prisma db seed
	# Python:  poetry run python manage.py loaddata initial_data
	# Rust:    cargo run --bin seed

# ==============================================================================
# Help & Discovery
# ==============================================================================

## help: Display all documented make targets
.PHONY: help
help:
	@echo "Available targets:"
	@grep -E '^## [a-zA-Z_-]+:.*?$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = "## |:"}; {printf "  \033[36m%-18s\033[0m %s\n", $$2, $$3}'
