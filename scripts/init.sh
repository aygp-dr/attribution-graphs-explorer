#!/usr/bin/env bash
# Initialization script for Attribution Graphs Explorer
# Used for checking if .init exists and running setup if needed

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
INIT_FILE="$PROJECT_ROOT/.init"

if [ ! -f "$INIT_FILE" ]; then
  echo "🔍 Missing .init file, running setup..."
  "$SCRIPT_DIR/setup.sh"
  exit $?
else
  echo "✅ Environment already initialized."
  exit 0
fi