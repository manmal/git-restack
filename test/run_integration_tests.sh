#!/bin/bash
# Integration tests for git-jenga
# These tests create real git repos and test the full round-trip

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
JENGA="$PROJECT_ROOT/zig-out/bin/git-jenga"

# Test counter
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Ensure binary is built
if [ ! -f "$JENGA" ]; then
    echo "Building git-jenga..."
    cd "$PROJECT_ROOT" && zig build
fi

# Create temp directory for tests
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

echo "Running integration tests in $TEST_DIR"
echo "========================================"

# Helper function to create a test repo
setup_repo() {
    local name=$1
    local repo_dir="$TEST_DIR/$name"
    mkdir -p "$repo_dir"
    cd "$repo_dir"
    git init -q
    git config user.email "test@test.com"
    git config user.name "Test User"
    echo "$repo_dir"
}

# Helper function to run a test
run_test() {
    local name=$1
    shift
    TESTS_RUN=$((TESTS_RUN + 1))
    
    # Run the test function
    if "$@"; then
        echo -e "${GREEN}✓${NC} $name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}✗${NC} $name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# ============================================================================
# TEST: Stack command on simple hierarchy
# ============================================================================
test_stack_simple() {
    local repo=$(setup_repo "stack-simple")
    
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
    local output=$("$JENGA" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "main" || return 1
    echo "$output" | grep -q "feature/TEST-1-first" || return 1
    echo "$output" | grep -q "feature/TEST-2-second" || return 1
    return 0
}

# ============================================================================
# TEST: Stack command with --json flag
# ============================================================================
test_stack_json() {
    local repo=$(setup_repo "stack-json")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-json
    echo "Content" > file.txt
    git add . && git commit -q -m "Add file"
    
    local output=$("$JENGA" stack --json 2>&1)
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
    local repo=$(setup_repo "plan-yaml")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-plan
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    # Make an unstaged change
    echo "Modified" >> feature.txt
    
    # Run plan
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    
    # Check plan file exists and has expected content
    [ -f test-plan.yml ] || return 1
    grep -q "version: 1" test-plan.yml || return 1
    grep -q "feature/TEST-1-plan" test-plan.yml || return 1
    grep -q "needs_fix: true" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Plan command detects unmapped files
# ============================================================================
test_plan_unmapped_files() {
    local repo=$(setup_repo "plan-unmapped")
    
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
    local output=$("$JENGA" plan -o test-plan.yml 2>&1)
    local code=$?
    
    [ $code -eq 1 ] || return 1
    echo "$output" | grep -q "unresolved errors" || return 1
    grep -q "unmapped_file" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Exec command refuses plan with errors
# ============================================================================
test_exec_refuses_errors() {
    local repo=$(setup_repo "exec-errors")
    
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
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1 || true
    
    # Try to execute - should fail with exit code 3
    local output=$("$JENGA" exec test-plan.yml 2>&1)
    local code=$?
    
    [ $code -eq 3 ] || return 1
    echo "$output" | grep -q "unresolved errors" || return 1
    return 0
}

# ============================================================================
# TEST: Full execution without conflicts
# ============================================================================
test_exec_no_conflicts() {
    local repo=$(setup_repo "exec-no-conflicts")
    
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
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    
    # Execute plan
    local output=$("$JENGA" exec test-plan.yml --force 2>&1)
    local code=$?
    
    # Check success
    [ $code -eq 0 ] || { echo "Exit code was $code"; return 1; }
    echo "$output" | grep -q "Execution complete" || return 1
    echo "$output" | grep -q "feature/TEST-1-base-fix" || return 1
    echo "$output" | grep -q "feature/TEST-2-top-fix" || return 1
    
    # Verify worktree was created
    [ -d "../exec-no-conflicts-jenga" ] || return 1
    
    # Verify -fix branches exist in worktree
    cd "../exec-no-conflicts-jenga"
    git branch | grep -q "feature/TEST-1-base-fix" || return 1
    git branch | grep -q "feature/TEST-2-top-fix" || return 1
    return 0
}

# ============================================================================
# TEST: Abort cleans up properly
# ============================================================================
test_exec_abort() {
    local repo=$(setup_repo "exec-abort")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-abort
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    echo "Modified" >> feature.txt
    
    # Generate and start execution
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    "$JENGA" exec test-plan.yml --force >/dev/null 2>&1 || true
    
    # Abort
    local output=$("$JENGA" exec --abort 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "Aborted" || return 1
    # Worktree should be removed
    [ ! -d "../exec-abort-jenga" ] || return 1
    return 0
}

# ============================================================================
# TEST: Status command shows plan info
# ============================================================================
test_status_plan() {
    local repo=$(setup_repo "status-plan")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    git checkout -q -b feature/TEST-1-status
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    echo "Modified" >> feature.txt
    
    # Generate plan
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    
    # Check status
    local output=$("$JENGA" status test-plan.yml 2>&1)
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
    local repo=$(setup_repo "develop-base")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M develop  # Use develop instead of main
    
    git checkout -q -b feature/TEST-1-develop
    echo "Feature" > feature.txt
    git add . && git commit -q -m "Add feature"
    
    local output=$("$JENGA" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "develop" || return 1
    return 0
}

# ============================================================================
# TEST: Deep stack (5 branches)
# ============================================================================
test_deep_stack() {
    local repo=$(setup_repo "deep-stack")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    for i in 1 2 3 4 5; do
        git checkout -q -b "feature/TEST-$i-layer$i"
        echo "Layer $i" > "layer$i.txt"
        git add . && git commit -q -m "Add layer $i"
    done
    
    local output=$("$JENGA" stack 2>&1)
    local code=$?
    
    [ $code -eq 0 ] || return 1
    echo "$output" | grep -q "feature/TEST-1-layer1" || return 1
    echo "$output" | grep -q "feature/TEST-2-layer2" || return 1
    echo "$output" | grep -q "feature/TEST-3-layer3" || return 1
    echo "$output" | grep -q "feature/TEST-4-layer4" || return 1
    echo "$output" | grep -q "feature/TEST-5-layer5" || return 1
    return 0
}

# ============================================================================
# TEST: Continue without state fails gracefully
# ============================================================================
test_continue_no_state() {
    local repo=$(setup_repo "continue-no-state")
    
    echo "# Test" > README.md
    git add . && git commit -q -m "Initial commit"
    git branch -M main
    
    # Try continue without any execution in progress
    local output=$("$JENGA" exec --continue 2>&1)
    local code=$?
    
    # Should fail because no execution in progress
    [ $code -eq 1 ] || return 1
    echo "$output" | grep -q "No execution in progress" || return 1
    return 0
}

# ============================================================================
# TEST: Multiple files mapping to different branches
# ============================================================================
test_multi_file_mapping() {
    local repo=$(setup_repo "multi-file")
    
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
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    
    # Both files should be in the plan
    grep -q "api.txt" test-plan.yml || return 1
    grep -q "ui.txt" test-plan.yml || return 1
    return 0
}

# ============================================================================
# TEST: Staged and unstaged changes together
# ============================================================================
test_staged_and_unstaged() {
    local repo=$(setup_repo "staged-unstaged")
    
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
    "$JENGA" plan -o test-plan.yml >/dev/null 2>&1
    
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
    "$JENGA" --help >/dev/null 2>&1 || return 1
    "$JENGA" stack --help >/dev/null 2>&1 || return 1
    "$JENGA" plan --help >/dev/null 2>&1 || return 1
    "$JENGA" exec --help >/dev/null 2>&1 || return 1
    "$JENGA" status --help >/dev/null 2>&1 || return 1
    return 0
}

# ============================================================================
# TEST: Version command
# ============================================================================
test_version() {
    local output=$("$JENGA" --version 2>&1)
    echo "$output" | grep -q "git-jenga" || return 1
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
