#!/bin/bash
# Integration tests for git-restack
# These tests create real git repos and test the full round-trip

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
RESTACK="$PROJECT_ROOT/zig-out/bin/git-restack"

# Test counter
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Ensure binary is built
if [ ! -f "$RESTACK" ]; then
    echo "Building git-restack..."
    cd "$PROJECT_ROOT" && zig build
fi

# Create temp directory for tests
TEST_BASE=$(mktemp -d)
trap "rm -rf $TEST_BASE" EXIT

echo "Running integration tests in $TEST_BASE"
echo "========================================"

# Helper function to run a test in a subshell with its own repo
run_test() {
    local name=$1
    local test_func=$2
    TESTS_RUN=$((TESTS_RUN + 1))
    
    # Create unique test directory
    local test_dir="$TEST_BASE/test_$TESTS_RUN"
    mkdir -p "$test_dir"
    
    # Run test in subshell
    (
        cd "$test_dir"
        git init -q
        git config user.email "test@test.com"
        git config user.name "Test User"
        
        # Run the test function
        $test_func
    )
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} $name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Configure a mergetool that auto-resolves using ours/theirs.
setup_auto_mergetool() {
    local mode=${1:-ours}
    local tool="restack-${mode}"
    local script_path="$PWD/.git/${tool}.sh"

    cat > "$script_path" <<'EOF'
#!/bin/sh
LOCAL="$1"
REMOTE="$2"
BASE="$3"
MERGED="$4"

if [ "$MODE" = "theirs" ]; then
    cp "$REMOTE" "$MERGED"
else
    cp "$LOCAL" "$MERGED"
fi

exit 0
EOF

    chmod +x "$script_path"

    if [ "$mode" = "theirs" ]; then
        MODE=theirs
    else
        MODE=ours
    fi

    git config mergetool.$tool.cmd "MODE=$MODE $script_path \"\\$LOCAL\" \"\\$REMOTE\" \"\\$BASE\" \"\\$MERGED\""
    git config mergetool.$tool.trustExitCode true

    echo "$tool"
}

# ============================================================================
# TEST: Stack command on simple hierarchy
# ============================================================================
test_stack_simple() {
    # Create main with initial commit
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create stacked branches
    git checkout -q -b feature/TEST-1-first
    echo "Feature 1" > feature1.txt
    git add . && git commit -q -m "Add feature 1"
    
    git checkout -q -b feature/TEST-2-second
    echo "Feature 2" > feature2.txt
    git add . && git commit -q -m "Add feature 2"
    
    # Run stack command
    local output=$("$RESTACK" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || { echo "Bad exit code: $code"; return 1; }
    echo "$output" | grep -q "main" || { echo "Missing main"; return 1; }
    echo "$output" | grep -q "feature/TEST-1-first" || { echo "Missing TEST-1"; return 1; }
    echo "$output" | grep -q "feature/TEST-2-second" || { echo "Missing TEST-2"; return 1; }
    return 0
}

# ============================================================================
# TEST: Stack command with --json flag
# ============================================================================
test_stack_json() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-json
    echo "Content" > file.txt
    git add . && git commit -q -m "Add file"
    
    local output=$("$RESTACK" stack --json 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q '"base_branch": "main"' || return 1
    echo "$output" | grep -q '"name": "feature/TEST-1-json"' || return 1
    return 0
}

# ============================================================================
# TEST: Plan command generates valid YAML
# ============================================================================
test_plan_generates_yaml() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-plan
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # Make an unstaged change
    echo "Modified" >> feature.txt
    
    # Run plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    
    # Check plan file exists and has expected content
    [ -f test-plan.yml ] || { echo "Plan file not created"; return 1; }
    grep -q "version: 2" test-plan.yml || { echo "Missing version"; return 1; }
    grep -q "feature/TEST-1-plan" test-plan.yml || { echo "Missing branch"; return 1; }
    grep -q "needs_fix: true" test-plan.yml || { echo "Missing needs_fix"; return 1; }
    return 0
}

# ============================================================================
# TEST: Plan command detects unmapped files
# ============================================================================
test_plan_unmapped_files() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-unmapped
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # Create a brand new file (not in any commit)
    echo "Brand new" > brand_new.txt
    git add brand_new.txt
    
    # Run plan - should exit with code 1 due to unmapped file
    "$RESTACK" plan -o test-plan.yml > /tmp/plan_out.txt 2>&1
    local code=$?
    
    [ $code -eq 1 ] || { echo "Expected exit 1, got $code"; return 1; }
    grep -q "unresolved errors" /tmp/plan_out.txt || return 1
    grep -q "unmapped_file" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Exec command refuses plan with errors
# ============================================================================
test_exec_refuses_errors() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-errors
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # Create unmapped file
    echo "New" > new.txt
    git add new.txt
    
    # Generate plan with errors
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1 || true
    
    # Try to execute - should fail with exit code 3
    "$RESTACK" exec test-plan.yml > /tmp/exec_out.txt 2>&1
    local code=$?
    
    [ $code -eq 3 ] || { echo "Expected exit 3, got $code"; return 1; }
    grep -q "unresolved errors" /tmp/exec_out.txt || return 1
    return 0
}

# ============================================================================
# TEST: Full execution without conflicts
# ============================================================================
test_exec_no_conflicts() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-base
    echo "Base feature" > base.txt
    git add . && git commit -q -m "Add base"
    
    git checkout -q -b feature/TEST-2-top
    echo "Top feature" > top.txt
    git add . && git commit -q -m "Add top"
    
    # Modify base.txt (maps to TEST-1)
    echo "Modified" >> base.txt
    
    # Generate plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    
    # Execute plan
    local output=$("$RESTACK" exec test-plan.yml --force 2>&1)
    local code=$?
    
    # Check success
    [ $code -eq 0 ] || { echo "Exit code was $code"; echo "$output"; return 1; }
    echo "$output" | grep -q "Execution complete" || { echo "Missing complete msg"; return 1; }
    echo "$output" | grep -q "feature/TEST-1-base-fix" || { echo "Missing TEST-1-fix"; return 1; }
    echo "$output" | grep -q "feature/TEST-2-top-fix" || { echo "Missing TEST-2-fix"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Abort when no execution in progress returns error
# ============================================================================
test_exec_abort() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Abort without any execution should fail
    "$RESTACK" exec --abort > /tmp/abort_out.txt 2>&1
    local code=$?
    
    [ $code -eq 1 ] || { echo "Exit code was $code, expected 1"; return 1; }
    grep -q "No execution in progress" /tmp/abort_out.txt || return 1
    return 0
}

# ============================================================================
# TEST: Status command shows plan info
# ============================================================================
test_status_plan() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-status
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    echo "Modified" >> feature.txt
    
    # Generate plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    
    # Check status
    local output=$("$RESTACK" status test-plan.yml 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "Plan Status" || return 1
    echo "$output" | grep -q "feature/TEST-1-status" || return 1
    return 0
}

# ============================================================================
# TEST: Develop branch as base
# ============================================================================
test_develop_base() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M develop  # Use develop instead of main
    
    git checkout -q -b feature/TEST-1-develop
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    local output=$("$RESTACK" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "develop" || return 1
    return 0
}

# ============================================================================
# TEST: Deep stack (5 branches)
# ============================================================================
test_deep_stack() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    for i in 1 2 3 4 5; do
        git checkout -q -b "feature/TEST-$i-layer$i"
        echo "Layer $i" > "layer$i.txt"
        git add . && git commit -q -m "Add layer $i"
    done
    
    local output=$("$RESTACK" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "feature/TEST-1-layer1" || return 1
    echo "$output" | grep -q "feature/TEST-5-layer5" || return 1
    return 0
}

# ============================================================================
# TEST: Continue without state fails gracefully
# ============================================================================
test_continue_no_state() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Try continue without any execution in progress
    "$RESTACK" exec --continue > /tmp/continue_out.txt 2>&1
    local code=$?
    
    # Should fail because no execution in progress
    [ $code -eq 1 ] || return 1
    grep -q "No execution in progress" /tmp/continue_out.txt || return 1
    return 0
}

# ============================================================================
# TEST: Multiple files mapping to different branches
# ============================================================================
test_multi_file_mapping() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Branch 1: create api.txt
    git checkout -q -b feature/TEST-1-api
    echo "API" > api.txt
    git add . && git commit -q -m "Add API"
    
    # Branch 2: create ui.txt
    git checkout -q -b feature/TEST-2-ui
    echo "UI" > ui.txt
    git add . && git commit -q -m "Add UI"
    
    # Modify both files
    echo "Modified API" >> api.txt
    echo "Modified UI" >> ui.txt
    
    # Generate plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    
    # Both files should be in the plan
    grep -q "api.txt" test-plan.yml || return 1
    grep -q "ui.txt" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Staged and unstaged changes together
# ============================================================================
test_staged_and_unstaged() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-mixed
    echo "File A" > a.txt
    echo "File B" > b.txt
    git add . && git commit -q -m "Add files"
    
    # Stage change to a.txt
    echo "Modified A" >> a.txt
    git add a.txt
    
    # Unstaged change to b.txt
    echo "Modified B" >> b.txt
    
    # Generate plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    
    # Both should be detected
    grep -q "a.txt" test-plan.yml || return 1
    grep -q "b.txt" test-plan.yml || return 1
    grep -q "staged: true" test-plan.yml || return 1
    grep -q "staged: false" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Help commands work
# ============================================================================
test_help_commands() {
    "$RESTACK" --help >/dev/null 2>&1 || return 1
    "$RESTACK" stack --help >/dev/null 2>&1 || return 1
    "$RESTACK" plan --help >/dev/null 2>&1 || return 1
    "$RESTACK" exec --help >/dev/null 2>&1 || return 1
    "$RESTACK" status --help >/dev/null 2>&1 || return 1
    return 0
}

# ============================================================================
# TEST: Version command
# ============================================================================
test_version() {
    local output=$("$RESTACK" --version 2>&1)
    echo "$output" | grep -q "git-restack" || return 1
    return 0
}

# ============================================================================
# TEST: No changes means nothing to fix
# ============================================================================
test_no_changes() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-clean
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # No changes made - generate plan
    "$RESTACK" plan -o test-plan.yml > /tmp/plan_out.txt 2>&1
    local code=$?
    
    [ $code -eq 0 ] || { echo "Exit code was $code"; return 1; }
    grep -q "needs_fix: false" test-plan.yml || { echo "Expected needs_fix: false"; return 1; }
    return 0
}

# ============================================================================
# TEST: Deleted file detection
# ============================================================================
test_deleted_file() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-delete
    echo "To be deleted" > delete_me.txt
    git add . && git commit -q -m "Add file to delete"
    
    # Delete the file
    rm delete_me.txt
    
    # Generate plan
    "$RESTACK" plan -o test-plan.yml > /tmp/plan_out.txt 2>&1
    
    grep -q "delete_me.txt" test-plan.yml || { echo "Deleted file not in plan"; return 1; }
    grep -q "change_type: deleted" test-plan.yml || { echo "Expected change_type: deleted"; return 1; }
    return 0
}

# ============================================================================
# TEST: Non-feature branch is excluded from stack
# ============================================================================
test_non_jira_branch() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create a branch without feature/ prefix - should be excluded
    git checkout -q -b cleanup/remove-dead-code
    echo "Cleanup" > cleanup.txt
    git add . && git commit -q -m "Cleanup"
    
    # Create a feature branch on top
    git checkout -q -b feature/TEST-1-actual-feature
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    "$RESTACK" stack > /tmp/stack_out.txt 2>&1
    local code=$?
    
    [ $code -eq 0 ] || return 1
    # cleanup branch should NOT be in the stack
    grep -q "cleanup/remove-dead-code" /tmp/stack_out.txt && return 1
    # feature branch SHOULD be in the stack
    grep -q "feature/TEST-1-actual-feature" /tmp/stack_out.txt || return 1
    return 0
}

# ============================================================================
# TEST: Stack from middle branch
# ============================================================================
test_stack_from_middle() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-bottom
    echo "Bottom" > bottom.txt
    git add . && git commit -q -m "Add bottom"
    
    git checkout -q -b feature/TEST-2-middle
    echo "Middle" > middle.txt
    git add . && git commit -q -m "Add middle"
    
    git checkout -q -b feature/TEST-3-top
    echo "Top" > top.txt
    git add . && git commit -q -m "Add top"
    
    # Go back to middle branch and check stack
    git checkout -q feature/TEST-2-middle
    
    "$RESTACK" stack > /tmp/stack_out.txt 2>&1
    local code=$?
    
    [ $code -eq 0 ] || return 1
    # Should show branches up to middle, not top
    grep -q "feature/TEST-1-bottom" /tmp/stack_out.txt || return 1
    grep -q "feature/TEST-2-middle" /tmp/stack_out.txt || return 1
    # Top should NOT be in the output (we're on middle)
    ! grep -q "feature/TEST-3-top" /tmp/stack_out.txt || { echo "Unexpected TEST-3 in stack"; return 1; }
    return 0
}

# ============================================================================
# TEST: Exec creates worktree directory
# ============================================================================
test_worktree_created() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-worktree
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    echo "Modified" >> feature.txt
    
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    "$RESTACK" exec test-plan.yml --force >/dev/null 2>&1
    
    # Check that worktree dir exists and has expected structure
    local worktree_dir=$(pwd)-restack
    [ -d "$worktree_dir" ] || { echo "Worktree dir not created"; return 1; }
    [ -f "$worktree_dir/feature.txt" ] || { echo "feature.txt not in worktree"; return 1; }
    return 0
}

# ============================================================================
# TEST: Plan output file path works
# ============================================================================
test_plan_output_path() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-path
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # Test nested output path
    mkdir -p plans/subdir
    "$RESTACK" plan -o plans/subdir/my-plan.yml >/dev/null 2>&1
    
    [ -f plans/subdir/my-plan.yml ] || { echo "Plan not created at nested path"; return 1; }
    grep -q "version: 2" plans/subdir/my-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Verify that diffs in plan are actually applied to files
# ============================================================================
test_diffs_applied() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with a file
    git checkout -q -b feature/TEST-1-first
    echo "line1" > myfile.txt
    echo "line2" >> myfile.txt
    echo "line3" >> myfile.txt
    git add . && git commit -q -m "Add myfile"
    
    # Create second branch 
    git checkout -q -b feature/TEST-2-second
    echo "second branch" > second.txt
    git add . && git commit -q -m "Add second"
    
    # Now modify myfile.txt (should map to TEST-1)
    echo "line1" > myfile.txt
    echo "MODIFIED LINE" >> myfile.txt
    echo "line3" >> myfile.txt
    
    # Generate and execute plan
    "$RESTACK" plan -o test-plan.yml >/dev/null 2>&1
    "$RESTACK" exec test-plan.yml --force >/dev/null 2>&1
    local code=$?
    
    [ $code -eq 0 ] || { echo "Exec failed with code $code"; return 1; }
    
    # Find the worktree
    local wt_path="../$(basename $(pwd))-restack"
    [ -d "$wt_path" ] || { echo "Worktree not found at $wt_path"; return 1; }
    
    # Check out the -fix branch and verify the file content
    cd "$wt_path"
    git checkout feature/TEST-1-first-fix >/dev/null 2>&1 || { echo "Could not checkout fix branch"; return 1; }
    
    # Verify the modification was applied
    grep -q "MODIFIED LINE" myfile.txt || { echo "Diff was NOT applied - MODIFIED LINE not found"; cat myfile.txt; return 1; }
    
    # Verify other lines are intact
    grep -q "line1" myfile.txt || { echo "line1 missing"; return 1; }
    grep -q "line3" myfile.txt || { echo "line3 missing"; return 1; }
    
    cd - >/dev/null
    return 0
}

# ============================================================================
# TEST: Verify command runs once per branch at completion
# ============================================================================
test_verify_runs_per_branch() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with 2 commits
    git checkout -q -b feature/TEST-1-first
    echo "commit1" > file1.txt
    git add . && git commit -q -m "First commit on TEST-1"
    echo "commit2" > file2.txt
    git add . && git commit -q -m "Second commit on TEST-1"
    
    # Create second branch with 1 commit
    git checkout -q -b feature/TEST-2-second
    echo "commit3" > file3.txt
    git add . && git commit -q -m "Commit on TEST-2"
    
    # Modify file1.txt (maps to TEST-1, will create a fix commit)
    echo "modified" >> file1.txt
    
    # Create a verification script that logs each call
    local verify_log="$PWD/verify_calls.log"
    rm -f "$verify_log"
    
    # Generate plan with verify command that logs to file
    "$RESTACK" plan -o test-plan.yml --verify "echo \$(git rev-parse --short HEAD) >> $verify_log" >/dev/null 2>&1
    
    # Verify the plan has verify_cmd
    grep -q 'verify_cmd:' test-plan.yml || { echo "verify_cmd not in plan"; return 1; }
    
    # Execute plan
    "$RESTACK" exec test-plan.yml --force >/dev/null 2>&1
    local code=$?
    
    [ $code -eq 0 ] || { echo "Exec failed with code $code"; return 1; }
    
    # Check the verify log exists
    [ -f "$verify_log" ] || { echo "Verify log not created - verify command never ran"; return 1; }
    
    # Count how many times verify was called
    local call_count=$(wc -l < "$verify_log" | tr -d ' ')
    
    # Expected: verification runs once per branch at completion
    # - TEST-1: 1 call (after all cherry-picks and fix commit)
    # - TEST-2: 1 call (after cherry-pick)
    # Total: 2 calls
    [ "$call_count" -eq 2 ] || { echo "Expected 2 verify calls (one per branch), got $call_count"; cat "$verify_log"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Apply command resets original branches to -fix branches
# ============================================================================
test_apply_resets_branches() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with a commit
    git checkout -q -b feature/TEST-1-first
    echo "original content" > myfile.txt
    git add . && git commit -q -m "Add myfile"
    
    # Create second branch
    git checkout -q -b feature/TEST-2-second
    echo "second branch" > second.txt
    git add . && git commit -q -m "Add second"
    
    # Record original commit SHAs
    local original_test1=$(git rev-parse feature/TEST-1-first)
    local original_test2=$(git rev-parse feature/TEST-2-second)
    
    # Modify myfile.txt (maps to TEST-1)
    echo "MODIFIED" >> myfile.txt
    
    # Generate and execute plan
    "$RESTACK" plan -o plan.yml >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    "$RESTACK" exec plan.yml --force >/dev/null 2>&1 || { echo "Exec failed"; return 1; }
    
    # Discard uncommitted changes and switch to main so we're not on a branch we're trying to update
    git checkout -- . 
    git checkout -q main
    
    # Verify -fix branches were created and have different commits
    local fix_test1=$(git rev-parse feature/TEST-1-first-fix)
    [ "$original_test1" != "$fix_test1" ] || { echo "-fix branch should have different commit"; return 1; }
    
    # Now apply - this should reset original branches to -fix branches
    "$RESTACK" apply plan.yml >/dev/null 2>&1
    local code=$?
    [ $code -eq 0 ] || { echo "Apply failed with code $code"; return 1; }
    
    # Verify original branches now point to same commits as -fix branches
    local new_test1=$(git rev-parse feature/TEST-1-first)
    local new_test2=$(git rev-parse feature/TEST-2-second)
    
    [ "$new_test1" = "$fix_test1" ] || { echo "TEST-1 should now match -fix branch"; return 1; }
    
    # TEST-2 should also be updated (rebased on new TEST-1)
    [ "$new_test2" != "$original_test2" ] || { echo "TEST-2 should have been updated"; return 1; }
    
    # Verify the actual file content on the original branch
    git checkout -q feature/TEST-1-first
    grep -q "MODIFIED" myfile.txt || { echo "myfile.txt should contain MODIFIED"; cat myfile.txt; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Verify-only mode runs verification without changes
# ============================================================================
test_verify_only_mode() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with 2 commits
    git checkout -q -b feature/TEST-1-first
    echo "file1" > file1.txt
    git add . && git commit -q -m "First commit"
    echo "file1-updated" > file1.txt
    git add . && git commit -q -m "Second commit"
    
    # Create second branch with 1 commit
    git checkout -q -b feature/TEST-2-second
    echo "file2" > file2.txt
    git add . && git commit -q -m "Third commit"
    
    # NO uncommitted changes - this is verify-only mode
    
    # Create a verification script that logs each call
    local verify_log="$PWD/verify_calls.log"
    rm -f "$verify_log"
    
    # Generate plan with --verify-only
    "$RESTACK" plan --verify-only "echo \$(git rev-parse --short HEAD) >> $verify_log" >/dev/null 2>&1
    local code=$?
    [ $code -eq 0 ] || { echo "Plan failed with code $code"; return 1; }
    
    # Verify the plan has verify_cmd but no fixes
    grep -q 'verify_cmd:' .git/git-restack/plan.yml || { echo "verify_cmd not in plan"; return 1; }
    grep -q 'needs_fix: true' .git/git-restack/plan.yml && { echo "Should not have needs_fix: true"; return 1; }
    
    # Execute plan
    "$RESTACK" exec --force >/dev/null 2>&1
    code=$?
    [ $code -eq 0 ] || { echo "Exec failed with code $code"; return 1; }
    
    # Check the verify log exists
    [ -f "$verify_log" ] || { echo "Verify log not created - verify command never ran"; return 1; }
    
    # Count how many times verify was called
    # Expected: verification runs once per branch at completion
    # - TEST-1: 1 call
    # - TEST-2: 1 call
    # Total: 2 calls
    local call_count=$(wc -l < "$verify_log" | tr -d ' ')
    [ "$call_count" -eq 2 ] || { echo "Expected 2 verify calls (one per branch), got $call_count"; cat "$verify_log"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Conflict shows future diffs for conflicted files
# ============================================================================
test_plan_auto_resolves_conflicts() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with a file
    git checkout -q -b feature/TEST-1-first
    echo "line1" > shared.txt
    git add . && git commit -q -m "Add shared.txt"
    
    # Create second branch that modifies the same line
    git checkout -q -b feature/TEST-2-second
    echo "line1-from-second" > shared.txt
    git add . && git commit -q -m "Modify shared.txt in second"
    
    # Move base branch forward with a conflicting change
    git checkout -q main
    echo "line1-from-main" > shared.txt
    git add . && git commit -q -m "Modify shared.txt on main"
    
    # Back to top of stack
    git checkout -q feature/TEST-2-second
    
    local tool
    tool=$(setup_auto_mergetool "ours")
    
    "$RESTACK" plan --mergetool "$tool" >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    grep -q "conflicts:" .git/git-restack/plan.yml || { echo "Plan missing conflicts block"; return 1; }
    grep -q "conflict_diff:" .git/git-restack/plan.yml || { echo "Plan missing conflict diff"; return 1; }
    grep -q "resolution:" .git/git-restack/plan.yml || { echo "Plan missing resolution block"; return 1; }
    
    "$RESTACK" exec --force >/dev/null 2>&1
    local code=$?
    [ $code -eq 0 ] || { echo "Exec failed with code $code"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Continue runs verification after conflict resolution
# ============================================================================
test_plan_conflict_runs_verification() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch with a file
    git checkout -q -b feature/TEST-1-first
    echo "line1" > conflict.txt
    git add . && git commit -q -m "Add conflict.txt"
    
    # Create second branch that modifies the same file (will cause conflict when rebased)
    git checkout -q -b feature/TEST-2-second
    echo "line1-modified-by-second" > conflict.txt
    git add . && git commit -q -m "Modify conflict.txt in second"
    
    # Move base branch forward with a conflicting change
    git checkout -q main
    echo "line1-modified-by-main" > conflict.txt
    git add . && git commit -q -m "Modify conflict.txt in main"
    
    git checkout -q feature/TEST-2-second
    
    local tool
    tool=$(setup_auto_mergetool "ours")
    
    # Create verification log
    local verify_log="$PWD/verify_after_plan.log"
    rm -f "$verify_log"
    
    # Generate plan with verification and conflict resolution
    "$RESTACK" plan --mergetool "$tool" --verify "echo \$(git rev-parse --short HEAD) >> $verify_log" >/dev/null 2>&1 || {
        echo "Plan failed"; return 1;
    }
    
    "$RESTACK" exec --force >/dev/null 2>&1
    local code=$?
    [ $code -eq 0 ] || { echo "Exec failed with code $code"; return 1; }
    
    [ -f "$verify_log" ] || { echo "Verify log not created"; return 1; }
    local call_count=$(wc -l < "$verify_log" | tr -d ' ')
    [ "$call_count" -ge 1 ] || { echo "Expected verification calls, got $call_count"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Modify/Delete conflict auto-resolves
# ============================================================================
test_plan_conflict_modify_delete() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    echo "line1" > doomed.txt
    git add doomed.txt && git commit -q -m "Add doomed.txt"

    git checkout -q -b feature/TEST-1-delete
    git rm -q doomed.txt
    git commit -q -m "Delete doomed.txt"

    git checkout -q main
    echo "line1-from-main" > doomed.txt
    git add doomed.txt && git commit -q -m "Modify doomed.txt on main"

    git checkout -q feature/TEST-1-delete

    local tool
    tool=$(setup_auto_mergetool "ours")

    "$RESTACK" plan --mergetool "$tool" >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    "$RESTACK" exec --force >/dev/null 2>&1 || { echo "Exec failed"; return 1; }

    return 0
}

# ============================================================================
# TEST: Add/Add conflict auto-resolves
# ============================================================================
test_plan_conflict_add_add() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    git checkout -q -b feature/TEST-1-add
    echo "from-feature" > added.txt
    git add added.txt && git commit -q -m "Add added.txt in feature"

    git checkout -q main
    echo "from-main" > added.txt
    git add added.txt && git commit -q -m "Add added.txt in main"

    git checkout -q feature/TEST-1-add

    local tool
    tool=$(setup_auto_mergetool "ours")

    "$RESTACK" plan --mergetool "$tool" >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    "$RESTACK" exec --force >/dev/null 2>&1 || { echo "Exec failed"; return 1; }

    return 0
}

# ============================================================================
# TEST: File/Directory conflict auto-resolves
# ============================================================================
test_plan_conflict_file_dir() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    git checkout -q -b feature/TEST-1-file
    echo "file-version" > node
    git add node && git commit -q -m "Add node file"

    git checkout -q main
    rm -f node
    mkdir -p node
    echo "dir-version" > node/inside.txt
    git add node/inside.txt && git commit -q -m "Add node directory"

    git checkout -q feature/TEST-1-file

    local tool
    tool=$(setup_auto_mergetool "ours")

    "$RESTACK" plan --mergetool "$tool" >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    "$RESTACK" exec --force >/dev/null 2>&1 || { echo "Exec failed"; return 1; }

    return 0
}

# ============================================================================
# TEST: Binary conflict stores base64 resolution
# ============================================================================
test_plan_conflict_binary() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    printf '\x00\x01\x02' > bin.dat
    git add bin.dat && git commit -q -m "Add bin.dat"

    git checkout -q -b feature/TEST-1-binary
    printf '\x00\x02\x03' > bin.dat
    git add bin.dat && git commit -q -m "Modify bin.dat in feature"

    git checkout -q main
    printf '\x00\x03\x04' > bin.dat
    git add bin.dat && git commit -q -m "Modify bin.dat in main"

    git checkout -q feature/TEST-1-binary

    local tool
    tool=$(setup_auto_mergetool "ours")

    "$RESTACK" plan --mergetool "$tool" >/dev/null 2>&1 || { echo "Plan failed"; return 1; }
    grep -q "encoding: base64" .git/git-restack/plan.yml || { echo "Expected base64 encoding in plan"; return 1; }
    "$RESTACK" exec --force >/dev/null 2>&1 || { echo "Exec failed"; return 1; }

    return 0
}

# ============================================================================
# TEST: Plan invalidated by new feature branch in stack
# ============================================================================
test_plan_invalid_new_branch() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    git checkout -q -b feature/TEST-1-first
    echo "one" > file.txt
    git add file.txt && git commit -q -m "Commit 1"
    echo "two" >> file.txt
    git add file.txt && git commit -q -m "Commit 2"

    local first_commit
    first_commit=$(git rev-list --reverse HEAD | head -n1)

    "$RESTACK" plan >/dev/null 2>&1 || { echo "Plan failed"; return 1; }

    git branch feature/TEST-NEW "$first_commit"

    "$RESTACK" exec --force >/dev/null 2>&1
    local code=$?
    [ $code -ne 0 ] || { echo "Expected exec to fail for new branch"; return 1; }

    return 0
}

# ============================================================================
# TEST: Plan invalidated by base branch moving
# ============================================================================
test_plan_invalid_base_tip() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main

    git checkout -q -b feature/TEST-1-first
    echo "line1" > file.txt
    git add file.txt && git commit -q -m "Add file"

    "$RESTACK" plan >/dev/null 2>&1 || { echo "Plan failed"; return 1; }

    git checkout -q main
    echo "line1-main" > file.txt
    git add file.txt && git commit -q -m "Advance main"

    git checkout -q feature/TEST-1-first

    "$RESTACK" exec --force >/dev/null 2>&1
    local code=$?
    [ $code -ne 0 ] || { echo "Expected exec to fail after base moved"; return 1; }

    return 0
}

# ============================================================================
# TEST: Step command processes one branch at a time
# ============================================================================
test_step_single_branch() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create first branch
    git checkout -q -b feature/TEST-1-first
    echo "First" > first.txt
    git add . && git commit -q -m "Add first"
    
    # Create second branch
    git checkout -q -b feature/TEST-2-second
    echo "Second" > second.txt
    git add . && git commit -q -m "Add second"
    
    # Create third branch
    git checkout -q -b feature/TEST-3-third
    echo "Third" > third.txt
    git add . && git commit -q -m "Add third"
    
    # Modify first.txt (maps to TEST-1)
    echo "Modified" >> first.txt
    
    # Generate plan
    "$RESTACK" plan >/dev/null 2>&1
    
    # First step - should only process TEST-1
    local output=$("$RESTACK" step 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || { echo "Step 1 failed with code $code"; return 1; }
    echo "$output" | grep -q "Step 1/3 done" || { echo "Should show Step 1/3 done"; echo "$output"; return 1; }
    echo "$output" | grep -q "TEST-1-first" || { echo "Should mention TEST-1"; return 1; }
    
    # Verify only TEST-1-fix branch exists
    git branch | grep -q "feature/TEST-1-first-fix" || { echo "TEST-1-fix should exist"; return 1; }
    git branch | grep -q "feature/TEST-2-second-fix" && { echo "TEST-2-fix should NOT exist yet"; return 1; }
    
    # Second step - should process TEST-2
    output=$("$RESTACK" step 2>&1)
    code=$?
    
    [ $code -eq 0 ] || { echo "Step 2 failed with code $code"; return 1; }
    echo "$output" | grep -q "Step 2/3 done" || { echo "Should show Step 2/3 done"; echo "$output"; return 1; }
    
    # Verify TEST-2-fix now exists
    git branch | grep -q "feature/TEST-2-second-fix" || { echo "TEST-2-fix should exist now"; return 1; }
    
    # Third step - should process TEST-3 and complete
    output=$("$RESTACK" step 2>&1)
    code=$?
    
    [ $code -eq 0 ] || { echo "Step 3 failed with code $code"; return 1; }
    echo "$output" | grep -q "All 3 branches complete" || { echo "Should show all complete"; echo "$output"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Step auto-detects fresh start vs continue
# ============================================================================
test_step_auto_detect() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Need at least 2 branches to test auto-detect (state persists between steps)
    git checkout -q -b feature/TEST-1-auto
    echo "Auto" > auto.txt
    git add . && git commit -q -m "Add auto"
    
    git checkout -q -b feature/TEST-2-auto2
    echo "Auto2" > auto2.txt
    git add . && git commit -q -m "Add auto2"
    
    echo "Modified" >> auto.txt
    
    # Generate plan
    "$RESTACK" plan >/dev/null 2>&1
    
    # First step - starts fresh (creates worktree)
    local output=$("$RESTACK" step 2>&1)
    echo "$output" | grep -q "Creating worktree" || { echo "First step should create worktree"; return 1; }
    
    # State should exist after first step (since there are more branches)
    [ -f .git/git-restack/state.json ] || { echo "State file should exist after first step"; return 1; }
    
    # Second step - should continue (not recreate worktree)
    output=$("$RESTACK" step 2>&1)
    echo "$output" | grep -q "Creating worktree" && { echo "Second step should NOT create worktree"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Step command applies fixes correctly
# ============================================================================
test_step_applies_fixes() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-fix
    echo "original" > fixme.txt
    git add . && git commit -q -m "Add fixme"
    
    git checkout -q -b feature/TEST-2-child
    echo "child" > child.txt
    git add . && git commit -q -m "Add child"
    
    # Modify fixme.txt
    echo "FIXED" > fixme.txt
    
    # Generate plan
    "$RESTACK" plan >/dev/null 2>&1
    
    # Run first step
    "$RESTACK" step >/dev/null 2>&1
    
    # Check fix was applied in -fix branch
    local wt_path="../$(basename $(pwd))-restack"
    cd "$wt_path"
    git checkout feature/TEST-1-fix-fix >/dev/null 2>&1
    grep -q "FIXED" fixme.txt || { echo "Fix was not applied"; cat fixme.txt; return 1; }
    cd - >/dev/null
    
    return 0
}

# ============================================================================
# TEST: Step preserves execution mode in state
# ============================================================================
test_step_preserves_mode() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Need 2 branches so state persists (doesn't complete immediately)
    git checkout -q -b feature/TEST-1-mode
    echo "Mode" > mode.txt
    git add . && git commit -q -m "Add mode"
    
    git checkout -q -b feature/TEST-2-mode2
    echo "Mode2" > mode2.txt
    git add . && git commit -q -m "Add mode2"
    
    echo "Modified" >> mode.txt
    
    # Generate plan
    "$RESTACK" plan >/dev/null 2>&1
    
    # Run step (only processes first branch, state persists)
    "$RESTACK" step >/dev/null 2>&1
    
    # Check state has mode: step (JSON format)
    grep -q '"mode": "step"' .git/git-restack/state.json || { echo "State should have mode: step"; cat .git/git-restack/state.json; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Step correctly chains branches (parent checkout)
# ============================================================================
test_step_chains_branches() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Create a 3-branch stack
    git checkout -q -b feature/TEST-1-base
    echo "base content" > base.txt
    git add . && git commit -q -m "Add base"
    
    git checkout -q -b feature/TEST-2-middle
    echo "middle content" > middle.txt
    git add . && git commit -q -m "Add middle"
    
    git checkout -q -b feature/TEST-3-top
    echo "top content" > top.txt
    git add . && git commit -q -m "Add top"
    
    # Modify base.txt (maps to TEST-1)
    echo "MODIFIED base" >> base.txt
    
    # Generate plan and step through all
    "$RESTACK" plan >/dev/null 2>&1
    "$RESTACK" step >/dev/null 2>&1  # TEST-1
    "$RESTACK" step >/dev/null 2>&1  # TEST-2
    "$RESTACK" step >/dev/null 2>&1  # TEST-3
    
    # Verify each -fix branch has correct parent
    local test1_fix=$(git rev-parse feature/TEST-1-base-fix)
    local test2_fix=$(git rev-parse feature/TEST-2-middle-fix)
    local test3_fix=$(git rev-parse feature/TEST-3-top-fix)
    
    # TEST-2-fix should be descendant of TEST-1-fix
    git merge-base --is-ancestor "$test1_fix" "$test2_fix" || { echo "TEST-2-fix should descend from TEST-1-fix"; return 1; }
    
    # TEST-3-fix should be descendant of TEST-2-fix
    git merge-base --is-ancestor "$test2_fix" "$test3_fix" || { echo "TEST-3-fix should descend from TEST-2-fix"; return 1; }
    
    # All three should have different commits
    [ "$test1_fix" != "$test2_fix" ] || { echo "TEST-1 and TEST-2 should have different commits"; return 1; }
    [ "$test2_fix" != "$test3_fix" ] || { echo "TEST-2 and TEST-3 should have different commits"; return 1; }
    
    return 0
}

# ============================================================================
# TEST: Continue respects execution mode (step vs exec)
# ============================================================================
test_continue_respects_mode() {
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Need 3 branches to test that continue only does one step
    git checkout -q -b feature/TEST-1-continue
    echo "continue" > cont.txt
    git add . && git commit -q -m "Add cont"
    
    git checkout -q -b feature/TEST-2-continue2
    echo "continue2" > cont2.txt
    git add . && git commit -q -m "Add cont2"
    
    git checkout -q -b feature/TEST-3-continue3
    echo "continue3" > cont3.txt
    git add . && git commit -q -m "Add cont3"
    
    echo "Modified" >> cont.txt
    
    # Generate plan
    "$RESTACK" plan >/dev/null 2>&1
    
    # Start with step (should process only TEST-1)
    "$RESTACK" step >/dev/null 2>&1
    
    # Verify mode is step (JSON format)
    grep -q '"mode": "step"' .git/git-restack/state.json || { echo "Mode should be step"; return 1; }
    
    # Use --continue - should respect step mode and only do one more (TEST-2)
    local output=$("$RESTACK" exec --continue 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || { echo "Continue failed with code $code"; return 1; }
    
    # Should show step 2/3 done (not all complete, since step mode only does one)
    echo "$output" | grep -q "Step 2/3 done" || { echo "Should show Step 2/3 done"; echo "$output"; return 1; }
    
    # State should still exist
    [ -f .git/git-restack/state.json ] || { echo "State should still exist after step 2"; return 1; }
    
    return 0
}

# ============================================================================
# Run all tests
# ============================================================================

run_test "Stack command on simple hierarchy" test_stack_simple
run_test "Stack command with --json flag" test_stack_json
run_test "Plan command generates valid YAML" test_plan_generates_yaml
run_test "Plan command detects unmapped files" test_plan_unmapped_files
run_test "Exec command refuses plan with errors" test_exec_refuses_errors
run_test "Full execution without conflicts" test_exec_no_conflicts
run_test "Abort cleans up properly" test_exec_abort
run_test "Status command shows plan info" test_status_plan
run_test "Develop branch as base" test_develop_base
run_test "Deep stack (5 branches)" test_deep_stack
run_test "Continue without state fails gracefully" test_continue_no_state
run_test "Multiple files mapping to different branches" test_multi_file_mapping
run_test "Staged and unstaged changes together" test_staged_and_unstaged
run_test "Help commands work" test_help_commands
run_test "Version command" test_version
run_test "No changes means nothing to fix" test_no_changes
run_test "Deleted file detection" test_deleted_file
run_test "Non-feature branch on stack" test_non_jira_branch
run_test "Stack from middle branch" test_stack_from_middle
run_test "Exec creates worktree directory" test_worktree_created
run_test "Plan output file path works" test_plan_output_path
run_test "Diffs are actually applied to files" test_diffs_applied
run_test "Verify command runs once per branch" test_verify_runs_per_branch
run_test "Apply resets original branches to -fix branches" test_apply_resets_branches
run_test "Verify-only mode runs verification without changes" test_verify_only_mode
run_test "Plan conflict resolution runs verification" test_plan_conflict_runs_verification
run_test "Plan auto-resolves conflicts" test_plan_auto_resolves_conflicts
run_test "Plan conflict modify/delete" test_plan_conflict_modify_delete
run_test "Plan conflict add/add" test_plan_conflict_add_add
run_test "Plan conflict file/dir" test_plan_conflict_file_dir
run_test "Plan conflict binary" test_plan_conflict_binary
run_test "Plan invalidated by new branch" test_plan_invalid_new_branch
run_test "Plan invalidated by base move" test_plan_invalid_base_tip
run_test "Step command processes one branch at a time" test_step_single_branch
run_test "Step auto-detects fresh start vs continue" test_step_auto_detect
run_test "Step command applies fixes correctly" test_step_applies_fixes
run_test "Step preserves execution mode in state" test_step_preserves_mode
run_test "Step correctly chains branches (parent checkout)" test_step_chains_branches
run_test "Continue respects execution mode (step vs exec)" test_continue_respects_mode

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "========================================"
echo "Test Results: $TESTS_PASSED/$TESTS_RUN passed"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "${RED}$TESTS_FAILED tests failed${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
