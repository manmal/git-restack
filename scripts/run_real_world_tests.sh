#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
BIN="$ROOT_DIR/zig-out/bin/git-jenga"

if [ ! -x "$BIN" ]; then
  (cd "$ROOT_DIR" && zig build)
fi

git config --global protocol.file.allow always >/dev/null 2>&1 || true
export GIT_ALLOW_PROTOCOL=file

apply_pushdown() {
  repo_dir="$1"
  mode="$2"

  case "$mode" in
    top)
      echo "ci update $(date +%s)" >> "$repo_dir/top.txt"
      ;;
    runsh)
      echo "ci update $(date +%s)" >> "$repo_dir/run.sh"
      ;;
    *)
      echo "Unknown pushdown mode: $mode" >&2
      exit 1
      ;;
  esac
}

run_case() {
  name="$1"
  setup_script="$2"
  repo_dir="$3"
  mergetool="$4"
  pushdown="$5"

  echo "==> $name"
  "$ROOT_DIR/scripts/$setup_script" --force

  plan_worktree="${repo_dir}-jenga-plan-ci"
  exec_worktree="${repo_dir}-jenga-ci"
  rm -rf "$plan_worktree" "$exec_worktree"

  if [ -f "$repo_dir/.gitmodules" ]; then
    (cd "$repo_dir" && git submodule update --init --recursive)
  fi

  apply_pushdown "$repo_dir" "$pushdown"

  (cd "$repo_dir" && "$BIN" plan --mergetool "$mergetool" --force --worktree-path "$plan_worktree")
  (cd "$repo_dir" && "$BIN" exec --force --worktree-path "$exec_worktree")
}

run_case "Base conflict" \
  "setup-git-jenga-test.sh" \
  "$ROOT_DIR/../git-jenga-test" \
  "jenga-ours" \
  "top"

run_case "Advanced rename/symlink/mode" \
  "setup-git-jenga-test-advanced.sh" \
  "$ROOT_DIR/../git-jenga-test-advanced" \
  "jenga-ours" \
  "runsh"

run_case "Fix-apply with theirs" \
  "setup-git-jenga-test-fix-apply.sh" \
  "$ROOT_DIR/../git-jenga-test-fix-apply" \
  "jenga-theirs" \
  "top"

run_case "Rename/rename" \
  "setup-git-jenga-test-rename-rename.sh" \
  "$ROOT_DIR/../git-jenga-test-rename-rename" \
  "jenga-ours" \
  "top"

run_case "Multi-conflict" \
  "setup-git-jenga-test-multi-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-multi-conflict" \
  "jenga-ours" \
  "top"

run_case "Rename/delete" \
  "setup-git-jenga-test-rename-delete.sh" \
  "$ROOT_DIR/../git-jenga-test-rename-delete" \
  "jenga-ours" \
  "top"

run_case "Space file/dir" \
  "setup-git-jenga-test-space-file-dir.sh" \
  "$ROOT_DIR/../git-jenga-test-space-file-dir" \
  "jenga-ours" \
  "top"

run_case "Space text conflict" \
  "setup-git-jenga-test-space-text-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-space-text-conflict" \
  "jenga-ours" \
  "top"

run_case "Add/add binary" \
  "setup-git-jenga-test-add-add-binary.sh" \
  "$ROOT_DIR/../git-jenga-test-add-add-binary" \
  "jenga-ours" \
  "top"

run_case "Delete/modify" \
  "setup-git-jenga-test-delete-modify.sh" \
  "$ROOT_DIR/../git-jenga-test-delete-modify" \
  "jenga-ours" \
  "top"

run_case "Rename/modify" \
  "setup-git-jenga-test-rename-modify.sh" \
  "$ROOT_DIR/../git-jenga-test-rename-modify" \
  "jenga-ours" \
  "top"

run_case "Iterative conflicts" \
  "setup-git-jenga-test-iterative-conflicts.sh" \
  "$ROOT_DIR/../git-jenga-test-iterative-conflicts" \
  "jenga-ours" \
  "top"

run_case "Dir rename" \
  "setup-git-jenga-test-dir-rename.sh" \
  "$ROOT_DIR/../git-jenga-test-dir-rename" \
  "jenga-ours" \
  "top"

run_case "Text + binary" \
  "setup-git-jenga-test-text-binary-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-text-binary-conflict" \
  "jenga-ours" \
  "top"

run_case "Empty cherry-pick" \
  "setup-git-jenga-test-empty-cherry-pick.sh" \
  "$ROOT_DIR/../git-jenga-test-empty-cherry-pick" \
  "jenga-ours" \
  "top"

run_case "Symlink conflict" \
  "setup-git-jenga-test-symlink-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-symlink-conflict" \
  "jenga-ours" \
  "top"

run_case "Submodule conflict" \
  "setup-git-jenga-test-submodule-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-submodule-conflict" \
  "jenga-ours" \
  "top"

run_case "CRLF conflict" \
  "setup-git-jenga-test-crlf-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-crlf-conflict" \
  "jenga-ours" \
  "top"

run_case "Submodule delete" \
  "setup-git-jenga-test-submodule-delete.sh" \
  "$ROOT_DIR/../git-jenga-test-submodule-delete" \
  "jenga-ours" \
  "top"

run_case "Submodule + text" \
  "setup-git-jenga-test-submodule-text-mix.sh" \
  "$ROOT_DIR/../git-jenga-test-submodule-text-mix" \
  "jenga-ours" \
  "top"

run_case "Rename/add" \
  "setup-git-jenga-test-rename-add.sh" \
  "$ROOT_DIR/../git-jenga-test-rename-add" \
  "jenga-ours" \
  "top"

run_case "Filemode conflict" \
  "setup-git-jenga-test-filemode-conflict.sh" \
  "$ROOT_DIR/../git-jenga-test-filemode-conflict" \
  "jenga-ours" \
  "top"

echo "All real-world tests passed."
