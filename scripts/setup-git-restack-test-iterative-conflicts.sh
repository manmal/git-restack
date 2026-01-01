#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-restack-test-iterative-conflicts"
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

cat > shared.txt <<'EOF'
line one
line two
EOF
git add shared.txt
git commit -q -m "Base shared.txt"
git branch -M main

git checkout -q -b feature/TEST-1-iter
cat > shared.txt <<'EOF'
line one feature1
line two
EOF
git add shared.txt
git commit -q -m "Feature1 edits line one"

git checkout -q -b feature/TEST-2-iter
cat > shared.txt <<'EOF'
line one feature2
line two
EOF
git add shared.txt
git commit -q -m "Feature2 edits line one"

git checkout -q -b feature/TEST-3-top
echo "top" > top.txt
git add top.txt
git commit -q -m "Add top.txt"

git checkout -q main
cat > shared.txt <<'EOF'
line one main
line two
EOF
git add shared.txt
git commit -q -m "Main edits line one"

git checkout -q feature/TEST-3-top

TOOL="restack-ours"
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
echo "Next: git-restack plan --mergetool $TOOL --force"
echo "Then: git-restack exec --force"
