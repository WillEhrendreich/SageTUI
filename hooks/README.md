# Git Hooks

This directory contains version-controlled git hooks.

## Available Hooks

### pre-push
Automatically increments the patch version in project files and creates a git tag before pushing.

**Features:**
- Increments version in project files
- Creates a git tag with the new version
- Amends the current commit with version changes
- Prevents double-bumping with safeguards

## Installation

Run the installation script to copy hooks to your `.git/hooks/` directory:

```powershell
pwsh hooks/install-hooks.ps1
```

Or on Unix-like systems:
```bash
./hooks/install-hooks.ps1
```

## Updating Hooks

When hooks are updated in this folder, run the install script again to update your local `.git/hooks/` directory.
