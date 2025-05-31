# Mise en Place

This command helps ensure your repository is in a clean state, with all branches and work properly organized. Think of it as preparing your kitchen workspace before cooking - everything in its place!

## Command Description

The `mise-en-place` command performs the following checks and operations:

1. Ensures you're on the main branch
2. Pulls the latest changes
3. Checks for and cleans up stale branches
4. Removes temporary files
5. Runs linting to ensure code quality
6. Verifies all changes are committed

## Usage

```
/mise-en-place
```

## Implementation

```bash
#!/bin/bash
set -e

echo "🧹 Running mise-en-place..."
echo "🔍 Checking repository state..."

# Check if we're on main branch
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "main" ]; then
  echo "⚠️ You are currently on branch '$CURRENT_BRANCH', not 'main'"
  read -p "Would you like to switch to main? (y/n) " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "📌 Switching to main branch..."
    git checkout main
  else
    echo "⚠️ Remaining on branch '$CURRENT_BRANCH'"
  fi
fi

# Pull latest changes
echo "⬇️ Pulling latest changes from origin..."
git pull

# Check for uncommitted changes
if [ -n "$(git status --porcelain)" ]; then
  echo "⚠️ You have uncommitted changes:"
  git status --short
  echo
  echo "Please commit or stash these changes before proceeding."
fi

# List and offer to clean up stale branches
echo "🔍 Checking for stale branches..."
git fetch --prune
MERGED_BRANCHES=$(git branch --merged main | grep -v '^\*' | grep -v 'main' | tr -d ' ')

if [ -n "$MERGED_BRANCHES" ]; then
  echo "The following branches have been merged into main and can be deleted:"
  echo "$MERGED_BRANCHES"
  read -p "Would you like to delete these branches? (y/n) " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "$MERGED_BRANCHES" | xargs -r git branch -d
    echo "🗑️ Deleted merged branches"
  fi
fi

# Clean up temporary files
echo "🧹 Cleaning up temporary files..."
find . -name "*~" -delete
find . -name "*.tmp" -delete
find . -name ".#*" -delete
find . -name "#*#" -delete

# Run linting if available
echo "✅ Running linters..."
if [ -f "Makefile" ] && grep -q "lint:" Makefile; then
  echo "Running 'make lint'..."
  make lint
else
  echo "No lint target found in Makefile, skipping."
fi

echo "✨ mise-en-place complete!"
echo "Your repository is now clean and organized."
```

## Example Output

```
🧹 Running mise-en-place...
🔍 Checking repository state...
📌 You are currently on branch 'main'
⬇️ Pulling latest changes from origin...
Already up to date.
🔍 Checking for stale branches...
No stale branches found.
🧹 Cleaning up temporary files...
✅ Running linters...
Running 'make lint'...
Linting Scheme files...
Linting Org files...
✨ mise-en-place complete!
Your repository is now clean and organized.
```

## Customization

Feel free to modify this script for your specific workflow. You can add additional checks such as:

- Running tests
- Checking for disk space
- Verifying dependencies
- Checking for large files that shouldn't be committed