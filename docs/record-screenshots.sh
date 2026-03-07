#!/usr/bin/env bash
# record-screenshots.sh — run this on Linux/macOS to regenerate screenshots and demo GIFs
#
# Requirements: vhs (https://github.com/charmbracelet/vhs)
#   brew install vhs       # macOS
#   sudo apt install vhs   # Ubuntu (or use the GitHub Actions workflow)

set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "==> Building samples..."
dotnet restore SageTUI.slnx --verbosity quiet
dotnet build samples/01-HelloWorld/HelloWorld.fsproj             -c Release --no-restore --verbosity quiet
dotnet build samples/04-InteractiveForm/InteractiveForm.fsproj   -c Release --no-restore --verbosity quiet
dotnet build samples/06-Kanban/Kanban.fsproj                     -c Release --no-restore --verbosity quiet
dotnet build samples/08-Sparklines/Sparklines.fsproj             -c Release --no-restore --verbosity quiet
dotnet build samples/09-SystemMonitor/SystemMonitor.fsproj       -c Release --no-restore --verbosity quiet

echo "==> Recording screenshots..."
mkdir -p docs
for tape in docs/tapes/screenshot-*.tape; do
  echo "  vhs $tape"
  vhs "$tape"
done

echo "==> Recording demo GIFs..."
for tape in docs/tapes/demo-*.tape; do
  echo "  vhs $tape"
  vhs "$tape"
done

echo ""
echo "Done. Assets written to docs/:"
ls -lh docs/screenshot-*.png 2>/dev/null || true
ls -lh docs/demo-*.gif 2>/dev/null || echo "  (no demo GIFs found — check VHS output above)"
