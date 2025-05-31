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
.PHONY: help build run test clean lint

# Show help by default
.DEFAULT_GOAL := help

# Help target that reads specially formatted comments
help:
	@echo "Attribution Graphs Explorer"
	@echo ""
	@echo "Usage:"
	@echo "  make [target]"
	@echo ""
	@echo "Targets:"
	@$(GREP) -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | $(AWK) 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

build: ## Compile Scheme files
	@echo "Building Attribution Graphs Explorer..."
	@$(GUILE) -c "(display \"Compiling Attribution Graphs Explorer...\")(newline)"

run: ## Run examples
	@echo "Running examples..."
	@$(GUILE) -L . $(EXAMPLEDIR)/run-all.scm

test: ## Run tests (when available)
	@echo "Running tests..."
	@echo "No tests available yet."

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