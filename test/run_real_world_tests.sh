#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
BIN="$ROOT_DIR/zig-out/bin/git-restack"

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

  plan_worktree="${repo_dir}-restack-plan-ci"
  exec_worktree="${repo_dir}-restack-ci"
  rm -rf "$plan_worktree" "$exec_worktree"

  if [ -f "$repo_dir/.gitmodules" ]; then
    (cd "$repo_dir" && git submodule update --init --recursive)
  fi

  apply_pushdown "$repo_dir" "$pushdown"

  (cd "$repo_dir" && "$BIN" plan --mergetool "$mergetool" --force --worktree-path "$plan_worktree")
  (cd "$repo_dir" && "$BIN" exec --force --worktree-path "$exec_worktree")
}

run_case "Base conflict" \
  "setup-git-restack-test.sh" \
  "$ROOT_DIR/../git-restack-test" \
  "restack-ours" \
  "top"

run_case "Advanced rename/symlink/mode" \
  "setup-git-restack-test-advanced.sh" \
  "$ROOT_DIR/../git-restack-test-advanced" \
  "restack-ours" \
  "runsh"

run_case "Fix-apply with theirs" \
  "setup-git-restack-test-fix-apply.sh" \
  "$ROOT_DIR/../git-restack-test-fix-apply" \
  "restack-theirs" \
  "top"

run_case "Rename/rename" \
  "setup-git-restack-test-rename-rename.sh" \
  "$ROOT_DIR/../git-restack-test-rename-rename" \
  "restack-ours" \
  "top"

run_case "Multi-conflict" \
  "setup-git-restack-test-multi-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-multi-conflict" \
  "restack-ours" \
  "top"

run_case "Rename/delete" \
  "setup-git-restack-test-rename-delete.sh" \
  "$ROOT_DIR/../git-restack-test-rename-delete" \
  "restack-ours" \
  "top"

run_case "Space file/dir" \
  "setup-git-restack-test-space-file-dir.sh" \
  "$ROOT_DIR/../git-restack-test-space-file-dir" \
  "restack-ours" \
  "top"

run_case "Space text conflict" \
  "setup-git-restack-test-space-text-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-space-text-conflict" \
  "restack-ours" \
  "top"

run_case "Add/add binary" \
  "setup-git-restack-test-add-add-binary.sh" \
  "$ROOT_DIR/../git-restack-test-add-add-binary" \
  "restack-ours" \
  "top"

run_case "Delete/modify" \
  "setup-git-restack-test-delete-modify.sh" \
  "$ROOT_DIR/../git-restack-test-delete-modify" \
  "restack-ours" \
  "top"

run_case "Rename/modify" \
  "setup-git-restack-test-rename-modify.sh" \
  "$ROOT_DIR/../git-restack-test-rename-modify" \
  "restack-ours" \
  "top"

run_case "Iterative conflicts" \
  "setup-git-restack-test-iterative-conflicts.sh" \
  "$ROOT_DIR/../git-restack-test-iterative-conflicts" \
  "restack-ours" \
  "top"

run_case "Dir rename" \
  "setup-git-restack-test-dir-rename.sh" \
  "$ROOT_DIR/../git-restack-test-dir-rename" \
  "restack-ours" \
  "top"

run_case "Text + binary" \
  "setup-git-restack-test-text-binary-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-text-binary-conflict" \
  "restack-ours" \
  "top"

run_case "Empty cherry-pick" \
  "setup-git-restack-test-empty-cherry-pick.sh" \
  "$ROOT_DIR/../git-restack-test-empty-cherry-pick" \
  "restack-ours" \
  "top"

run_case "Symlink conflict" \
  "setup-git-restack-test-symlink-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-symlink-conflict" \
  "restack-ours" \
  "top"

run_case "Submodule conflict" \
  "setup-git-restack-test-submodule-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-submodule-conflict" \
  "restack-ours" \
  "top"

run_case "CRLF conflict" \
  "setup-git-restack-test-crlf-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-crlf-conflict" \
  "restack-ours" \
  "top"

run_case "Submodule delete" \
  "setup-git-restack-test-submodule-delete.sh" \
  "$ROOT_DIR/../git-restack-test-submodule-delete" \
  "restack-ours" \
  "top"

run_case "Submodule + text" \
  "setup-git-restack-test-submodule-text-mix.sh" \
  "$ROOT_DIR/../git-restack-test-submodule-text-mix" \
  "restack-ours" \
  "top"

run_case "Rename/add" \
  "setup-git-restack-test-rename-add.sh" \
  "$ROOT_DIR/../git-restack-test-rename-add" \
  "restack-ours" \
  "top"

run_case "Filemode conflict" \
  "setup-git-restack-test-filemode-conflict.sh" \
  "$ROOT_DIR/../git-restack-test-filemode-conflict" \
  "restack-ours" \
  "top"

echo "All real-world tests passed."
