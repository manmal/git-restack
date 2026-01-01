#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-jenga-test-crlf-conflict"
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
git config core.autocrlf false
git config core.safecrlf false

printf 'line1\r\nline2\r\n' > notes.txt
git add notes.txt
git commit -q -m "Base notes.txt CRLF"
git branch -M main

git checkout -q -b feature/TEST-1-crlf
printf 'line1\r\nline2 feature\r\n' > notes.txt
git add notes.txt
git commit -q -m "Feature edits line2"

git checkout -q -b feature/TEST-2-top
echo "top" > top.txt
git add top.txt
git commit -q -m "Add top.txt"

git checkout -q main
printf 'line1\r\nline2 main\r\n' > notes.txt
git add notes.txt
git commit -q -m "Main edits line2"

git checkout -q feature/TEST-2-top

TOOL="jenga-ours"
SCRIPT_PATH="$PWD/.git/${TOOL}.sh"
cat > "$SCRIPT_PATH" <<'EOF'
#!/usr/bin/env bash
LOCAL="$1"
REMOTE="$2"
BASE="$3"
MERGED="$4"

exit 1
EOF
chmod +x "$SCRIPT_PATH"

git config mergetool.$TOOL.cmd "$SCRIPT_PATH \"\\\$LOCAL\" \"\\\$REMOTE\" \"\\\$BASE\" \"\\\$MERGED\""
git config mergetool.$TOOL.trustExitCode true

echo "Ready: $TARGET_DIR"
echo "Next: git-jenga plan --mergetool $TOOL --force"
echo "Then: git-jenga exec --force"
