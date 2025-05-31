#!/bin/bash
# Setup script for Attribution Graphs Explorer
# Initializes the environment and creates .init marker

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
INIT_FILE="$PROJECT_ROOT/.init"

echo -e "${BLUE}=€ Setting up Attribution Graphs Explorer...${NC}"

# Check if already initialized
if [ -f "$INIT_FILE" ]; then
  echo -e "${YELLOW}  Environment already initialized.${NC}"
  echo "If you want to reinitialize, remove $INIT_FILE and run this script again."
  exit 0
fi

# Run dependency check
echo -e "${BLUE}=Ë Checking dependencies...${NC}"
"$SCRIPT_DIR/deps.sh"
DEPS_STATUS=$?

if [ $DEPS_STATUS -ne 0 ]; then
  echo -e "${YELLOW}  Some dependencies might be missing.${NC}"
  read -p "Continue anyway? (y/n) " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${RED}L Setup canceled.${NC}"
    exit 1
  fi
fi

# Create necessary directories
echo -e "${BLUE}=Á Creating required directories...${NC}"
mkdir -p "$PROJECT_ROOT/build"
mkdir -p "$PROJECT_ROOT/docs/generated"

# Setup example environment
echo -e "${BLUE}>ê Setting up example environment...${NC}"
if [ ! -f "$PROJECT_ROOT/.env.example" ]; then
  cat > "$PROJECT_ROOT/.env.example" << EOF
# Example environment configuration
# Copy this to .env and modify as needed

# Guile settings
GUILE_LOAD_PATH=$PROJECT_ROOT

# Add your configuration below
EXAMPLE_API_KEY=your-api-key-here
EOF
  echo -e "${GREEN} Created .env.example${NC}"
  
  if [ ! -f "$PROJECT_ROOT/.env" ]; then
    cp "$PROJECT_ROOT/.env.example" "$PROJECT_ROOT/.env"
    echo -e "${GREEN} Created .env from template${NC}"
    echo -e "${YELLOW}  Remember to update your environment variables in .env${NC}"
  fi
fi

# Create .init marker file
echo "Initialized on $(date)" > "$INIT_FILE"
echo "OS: $(uname -s)" >> "$INIT_FILE"
echo "Guile version: $(guile3 --version | head -n 1)" >> "$INIT_FILE"

echo -e "${GREEN} Environment initialized successfully!${NC}"
echo "To get started, try running: gmake help"