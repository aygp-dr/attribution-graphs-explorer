#!/usr/bin/env bash
# Dependency checker for Attribution Graphs Explorer
# Verifies that all required tools are available

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo "= Checking dependencies for Attribution Graphs Explorer..."
echo

# Track if any dependencies are missing
MISSING_DEPS=0

# Function to check if a command exists
check_command() {
  local cmd=$1
  local name=${2:-$1}
  local alt_msg=$3
  
  echo -n "Checking for $name... "
  if command -v "$cmd" >/dev/null 2>&1; then
    echo -e "${GREEN} Found$(command -v "$cmd")${NC}"
    return 0
  else
    echo -e "${RED}L Missing${NC}"
    if [ -n "$alt_msg" ]; then
      echo -e "${YELLOW}$alt_msg${NC}"
    fi
    MISSING_DEPS=$((MISSING_DEPS + 1))
    return 1
  fi
}

# Required dependencies
check_command "guile3" "Guile 3" "Install with: pkg install -y guile3"
check_command "gmake" "GNU Make" "Install with: pkg install -y gmake"
check_command "ggrep" "GNU Grep" "Install with: pkg install -y grep"
check_command "gawk" "GNU Awk" "Install with: pkg install -y gawk"

# Development tools (not strictly required but recommended)
echo
echo "Development tools (recommended):"
check_command "shellcheck" "ShellCheck" "Install with: pkg install -y shellcheck" || true
check_command "remake" "Remake" "Install with: pkg install -y remake" || true
check_command "emacs" "Emacs" "Install with: pkg install -y emacs" || true

# Check if Guile has required modules
echo
echo "Checking Guile modules..."

GUILE_MISSING=0
check_guile_module() {
  local module=$1
  
  echo -n "Checking for Guile module $module... "
  if guile3 -c "(use-modules ($module))" >/dev/null 2>&1; then
    echo -e "${GREEN} Found${NC}"
    return 0
  else
    echo -e "${RED}L Missing${NC}"
    echo -e "${YELLOW}This module may need to be installed separately.${NC}"
    GUILE_MISSING=$((GUILE_MISSING + 1))
    return 1
  fi
}

check_guile_module "srfi srfi-1" "SRFI-1"
check_guile_module "srfi srfi-9" "SRFI-9"
check_guile_module "srfi srfi-43" "SRFI-43"
check_guile_module "ice-9 regex" "Regex"

# Summary
echo
if [ $MISSING_DEPS -eq 0 ]; then
  if [ $GUILE_MISSING -eq 0 ]; then
    echo -e "${GREEN} All dependencies are satisfied!${NC}"
    exit 0
  else
    echo -e "${YELLOW}� Core dependencies found, but some Guile modules may be missing.${NC}"
    echo "The project may still work with limited functionality."
    exit 1
  fi
else
  echo -e "${RED}L Missing $MISSING_DEPS required dependencies.${NC}"
  echo "Please install the missing dependencies and run this script again."
  exit 1
fi