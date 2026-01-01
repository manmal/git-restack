#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-restack-test-fix-apply"
FORCE=0

while [ $# -gt 0 ]; do
  case "$1" in
    -f|--force)
      FORCE=1
      shift
      ;;
    *)
      TARGET_DIR="$1"
      shift
      ;;
  esac
done

if [ -e "$TARGET_DIR" ]; then
  if [ "$FORCE" -ne 1 ]; then
    echo "Error: $TARGET_DIR exists. Re-run with --force to replace it." >&2
    exit 1
  fi
  rm -rf "$TARGET_DIR"
fi

mkdir -p "$TARGET_DIR"
cd "$TARGET_DIR"
git init -q
git config user.email "test@test.com"
git config user.name "Test User"

echo "# Fix-apply conflict repo" > README.md
echo "base line" > conflict.txt
git add README.md conflict.txt
git commit -q -m "Base conflict.txt"
git branch -M main

git checkout -q -b feature/TEST-1-base
echo "feature line" > conflict.txt
git add conflict.txt
git commit -q -m "Feature edits conflict.txt"

git checkout -q -b feature/TEST-2-top
echo "top" > top.txt
git add top.txt
git commit -q -m "Add top.txt"

git checkout -q main
echo "main line" > conflict.txt
git add conflict.txt
git commit -q -m "Main edits conflict.txt"

git checkout -q feature/TEST-2-top
echo "feature line with working change" > conflict.txt

TOOL="restack-theirs"
SCRIPT_PATH="$PWD/.git/${TOOL}.sh"
cat > "$SCRIPT_PATH" <<'EOF'
#!/usr/bin/env bash
LOCAL="$1"
REMOTE="$2"
BASE="$3"
MERGED="$4"

cp "$REMOTE" "$MERGED"
exit 0
EOF
chmod +x "$SCRIPT_PATH"

git config mergetool.$TOOL.cmd "$SCRIPT_PATH \"\\\$LOCAL\" \"\\\$REMOTE\" \"\\\$BASE\" \"\\\$MERGED\""
git config mergetool.$TOOL.trustExitCode true

echo "Ready: $TARGET_DIR"
echo "Next: git-restack plan --mergetool $TOOL --force"
echo "Then: git-restack exec --force"
