#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="../git-jenga-test-advanced"
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

echo "# Advanced git-jenga test repo" > README.md
echo "alpha base" > alpha.txt
printf '%s\n' "#!/bin/sh" "echo base" > run.sh
chmod -x run.sh
git add README.md alpha.txt run.sh
git commit -q -m "Base files"
git branch -M main

git checkout -q -b feature/TEST-1-rename
git mv alpha.txt alpha_feature.txt
echo "feature rename" >> alpha_feature.txt
git add alpha_feature.txt
git commit -q -m "Rename alpha.txt on feature"

git checkout -q -b feature/TEST-2-symlink
ln -s alpha_feature.txt linkme
git add linkme
git commit -q -m "Add symlink linkme"

git checkout -q -b feature/TEST-3-mode
echo "feature script" >> run.sh
chmod +x run.sh
git add run.sh
git commit -q -m "Make run.sh executable with feature change"

git checkout -q main
git mv alpha.txt alpha_main.txt
echo "main rename" >> alpha_main.txt
git add alpha_main.txt
git commit -q -m "Rename alpha.txt on main"

echo "main link content" > linkme
git add linkme
git commit -q -m "Add linkme as regular file on main"

echo "main script change" >> run.sh
git add run.sh
git commit -q -m "Modify run.sh on main"

git checkout -q feature/TEST-3-mode

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
