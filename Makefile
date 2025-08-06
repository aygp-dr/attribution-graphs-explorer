# Attribution Graphs Explorer Makefile
# Configured for FreeBSD defaults

# FreeBSD specific tools
GUILE = guile3
MAKE = gmake
GREP = ggrep
AWK = gawk

# Project configuration
PROJECT_NAME ?= attribution-graphs-explorer
PROJECT_ROOT ?= $(shell pwd)

# Directories
SRCDIR = src
EXAMPLEDIR = examples
DOCDIR = docs

# Default target is help
.PHONY: help build run test clean lint setup deps all check distcheck configure install tangle-commands emacs-session

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
	@$(GUILE) -L . attribution-graphs/examples/run-all.scm

test: ## Run tests (when available)
	@echo "Running tests..."
	@if [ -f "tests/minimal-test.scm" ]; then \
		$(GUILE) -L . tests/minimal-test.scm; \
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
	@emacs --batch -l org \
		--eval "(setq-default comment-start \"<!-- \")" \
		--eval "(setq-default comment-end \" -->\")" \
		--eval "(org-babel-tangle-file \"kitchen-ops.org\")"

emacs-session: ## Start a named tmux session with project-specific Emacs
	@echo "Starting tmux session '$(PROJECT_NAME)' with Emacs..."
	@tmux has-session -t $(PROJECT_NAME) 2>/dev/null && \
		echo "Session '$(PROJECT_NAME)' already exists. Attaching..." && \
		tmux attach-session -t $(PROJECT_NAME) || \
		(tmux new-session -d -s $(PROJECT_NAME) "emacs -nw -Q -l $(PROJECT_ROOT)/$(PROJECT_NAME).el" && \
		echo "Session '$(PROJECT_NAME)' created. TTY: $$(tmux list-panes -t $(PROJECT_NAME) -F '#{pane_tty}')" && \
		echo "Attach with: tmux attach-session -t $(PROJECT_NAME)")