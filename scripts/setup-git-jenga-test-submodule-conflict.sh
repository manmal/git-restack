#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-jenga-test-submodule-conflict"
SUBMODULE_DIR="../git-jenga-test-submodule-conflict-submodule"
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

if [ -e "$TARGET_DIR" ] || [ -e "$SUBMODULE_DIR" ]; then
  if [ "$FORCE" -ne 1 ]; then
    echo "Error: $TARGET_DIR or $SUBMODULE_DIR exists. Re-run with --force to replace them." >&2
    exit 1
  fi
  rm -rf "$TARGET_DIR" "$SUBMODULE_DIR"
fi

mkdir -p "$SUBMODULE_DIR"
cd "$SUBMODULE_DIR"
git init -q
git config user.email "test@test.com"
git config user.name "Test User"

echo "base" > lib.txt
git add lib.txt
git commit -q -m "Base lib.txt"
BASE_COMMIT=$(git rev-parse HEAD)

echo "feature change" >> lib.txt
git add lib.txt
git commit -q -m "Feature lib change"
FEATURE_COMMIT=$(git rev-parse HEAD)

git checkout -q "$BASE_COMMIT"
echo "main change" >> lib.txt
git add lib.txt
git commit -q -m "Main lib change"
MAIN_COMMIT=$(git rev-parse HEAD)

git checkout -q "$BASE_COMMIT"

mkdir -p "$TARGET_DIR"
cd "$TARGET_DIR"
git init -q
git config user.email "test@test.com"
git config user.name "Test User"

git -c protocol.file.allow=always submodule add "$SUBMODULE_DIR" lib/sub
git commit -q -m "Add submodule at base"
git branch -M main

git checkout -q -b feature/TEST-1-submodule
git -C lib/sub checkout -q "$FEATURE_COMMIT"
git add lib/sub
git commit -q -m "Update submodule to feature commit"

git checkout -q -b feature/TEST-2-top
echo "top" > top.txt
git add top.txt
git commit -q -m "Add top.txt"

git checkout -q main
git -C lib/sub checkout -q "$MAIN_COMMIT"
git add lib/sub
git commit -q -m "Main updates submodule"

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
