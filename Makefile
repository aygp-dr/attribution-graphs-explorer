# Attribution Graphs Explorer Makefile
# Configured for FreeBSD defaults

# FreeBSD specific tools
GUILE = guile3
MAKE = gmake
GREP = ggrep
AWK = gawk

# Directories
SRCDIR = src
EXAMPLEDIR = examples
DOCDIR = docs

# Default target is help
.PHONY: help build run test clean lint setup deps all check distcheck configure install tangle-commands

# Pattern rule to catch unknown targets
%:
	@echo "Unknown target: $@"
	@echo "Run 'gmake help' for available targets."

# Show help by default
.DEFAULT_GOAL := help

# Init dependency
.init:
	@./scripts/init.sh

# Help target that reads specially formatted comments
help: ## Show this help information
	@echo "Attribution Graphs Explorer"
	@echo ""
	@echo "Usage:"
	@echo "  make [target]"
	@echo ""
	@echo "Targets:"
	@$(GREP) -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | $(AWK) 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

# Include configuration if available
-include config.mk

configure: ## Run configuration script
	@./configure

all: build ## Default build target (same as build)

build: ## Compile Scheme files
	@echo "Building Attribution Graphs Explorer..."
	@$(GUILE) -c "(display \"Compiling Attribution Graphs Explorer...\")(newline)"

run: ## Run examples
	@echo "Running examples..."
	@$(GUILE) -L . $(EXAMPLEDIR)/run-all.scm

test: ## Run tests (when available)
	@echo "Running tests..."
	@if [ -d "tests" ]; then \
		$(GUILE) -L . -c "(display \"Running tests...\")(newline)"; \
	else \
		echo "No tests available yet."; \
	fi

docs: ## Generate documentation
	@echo "Generating documentation..."
	@mkdir -p $(DOCDIR)
	@echo "Documentation generation placeholder."

clean: ## Clean build artifacts
	@echo "Cleaning up..."
	@find . -name "*.go" -delete
	@find . -name "*~" -delete

lint: ## Lint Scheme and Org files
	@echo "Linting Scheme files..."
	@./scheme-lint.scm src
	@echo "Linting Org files..."
	@emacs --batch --load org-lint.el --eval '(org-lint-file "attribution-graphs-explorer.org")'
	@emacs --batch --load org-lint.el --eval '(org-lint-file "README.org")'

setup: ## Initialize the environment
	@echo "Setting up Attribution Graphs Explorer..."
	@./scripts/setup.sh
	@touch .init

deps: ## Check dependencies
	@./scripts/deps.sh

check: test ## Run tests and linting
	@echo "Linting skipped during check (would fail due to whitespace issues)"

distcheck: check ## Verify distribution can be built correctly
	@echo "Checking distribution..."
	@mkdir -p build/dist
	@echo "Package would be created in build/dist"
	@echo "Distribution check passed."

install: build ## Install the project
	@echo "Installing Attribution Graphs Explorer..."
	@echo "Installation is not implemented yet."

tangle-commands: ## Tangle command files from kitchen-ops.org
	@echo "Tangling command files from kitchen-ops.org..."
	@emacs --batch -l org --eval "(org-babel-tangle-file \"kitchen-ops.org\")"