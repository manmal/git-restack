#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-jenga-test-multi-conflict"
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

echo "# Multi-conflict repo" > README.md
echo "base a" > file_a.txt
echo "base b" > file_b.txt
git add README.md file_a.txt file_b.txt
git commit -q -m "Base files"
git branch -M main

git checkout -q -b feature/TEST-1-multi
echo "feature a" >> file_a.txt
echo "feature b" >> file_b.txt
git add file_a.txt file_b.txt
git commit -q -m "Feature edits both files"

git checkout -q -b feature/TEST-2-top
echo "top" > top.txt
git add top.txt
git commit -q -m "Add top.txt"

git checkout -q main
echo "main a" >> file_a.txt
echo "main b" >> file_b.txt
git add file_a.txt file_b.txt
git commit -q -m "Main edits both files"

git checkout -q feature/TEST-2-top

TOOL="jenga-ours"
SCRIPT_PATH="$PWD/.git/${TOOL}.sh"
cat > "$SCRIPT_PATH" <<'EOF'
#!/usr/bin/env bash
LOCAL="$1"
REMOTE="$2"
BASE="$3"
MERGED="$4"

cp "$LOCAL" "$MERGED"
exit 0
EOF
chmod +x "$SCRIPT_PATH"

git config mergetool.$TOOL.cmd "$SCRIPT_PATH \"\\\$LOCAL\" \"\\\$REMOTE\" \"\\\$BASE\" \"\\\$MERGED\""
git config mergetool.$TOOL.trustExitCode true

echo "Ready: $TARGET_DIR"
echo "Next: git-jenga plan --mergetool $TOOL --force"
echo "Then: git-jenga exec --force"
