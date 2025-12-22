const std = @import("std");
const testing = std.testing;

// Import our modules via build.zig imports
const process = @import("process");
const strings = @import("strings");
const stack_mod = @import("stack");
const diff_mod = @import("diff");
const parser = @import("parser");
const emitter = @import("emitter");
const types = @import("types");

const TestRepo = struct {
    allocator: std.mem.Allocator,
    path: []const u8,
    original_cwd: []const u8,

    /// Create a new test repo in /tmp
    fn init(allocator: std.mem.Allocator, name: []const u8) !TestRepo {
        // Generate unique path
        const timestamp = std.time.milliTimestamp();
        const path = try std.fmt.allocPrint(allocator, "/tmp/git-jenga-test-{s}-{d}", .{ name, timestamp });

        // Save original cwd
        var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
        const original_cwd = try allocator.dupe(u8, try std.fs.cwd().realpath(".", &cwd_buf));

        // Create directory
        try std.fs.cwd().makePath(path);

        // Change to test directory
        try std.posix.chdir(path);

        // Initialize git repo
        _ = try runCmd(allocator, &.{ "git", "init" });
        _ = try runCmd(allocator, &.{ "git", "config", "user.email", "test@test.com" });
        _ = try runCmd(allocator, &.{ "git", "config", "user.name", "Test User" });

        return TestRepo{
            .allocator = allocator,
            .path = path,
            .original_cwd = original_cwd,
        };
    }

    /// Clean up test repo
    fn deinit(self: *TestRepo) void {
        // Return to original directory
        std.posix.chdir(self.original_cwd) catch {};

        // Remove test directory
        std.fs.cwd().deleteTree(self.path) catch {};

        self.allocator.free(self.path);
        self.allocator.free(self.original_cwd);
    }

    /// Create a file with content
    fn createFile(self: *TestRepo, filename: []const u8, content: []const u8) !void {
        _ = self;
        const file = try std.fs.cwd().createFile(filename, .{});
        defer file.close();
        try file.writeAll(content);
    }

    /// Append to a file
    fn appendFile(self: *TestRepo, filename: []const u8, content: []const u8) !void {
        _ = self;
        const file = try std.fs.cwd().openFile(filename, .{ .mode = .write_only });
        defer file.close();
        try file.seekFromEnd(0);
        try file.writeAll(content);
    }

    /// Run git command
    fn git(self: *TestRepo, args: []const []const u8) ![]const u8 {
        return process.runGit(self.allocator, args);
    }

    /// Commit all changes
    fn commitAll(self: *TestRepo, message: []const u8) !void {
        _ = try self.git(&.{ "add", "-A" });
        _ = try self.git(&.{ "commit", "-m", message });
    }

    /// Create a branch and switch to it
    fn createBranch(self: *TestRepo, name: []const u8) !void {
        _ = try self.git(&.{ "checkout", "-b", name });
    }

    /// Switch to existing branch
    fn checkout(self: *TestRepo, name: []const u8) !void {
        _ = try self.git(&.{ "checkout", name });
    }

    /// Get current branch name
    fn currentBranch(self: *TestRepo) ![]const u8 {
        const result = try self.git(&.{ "symbolic-ref", "--short", "HEAD" });
        return strings.trim(result);
    }
};

/// Helper to run a command and return stdout
fn runCmd(allocator: std.mem.Allocator, args: []const []const u8) ![]const u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = args,
        .max_output_bytes = 10 * 1024 * 1024,
    }) catch |err| {
        std.debug.print("Command failed: {any}\n", .{err});
        return err;
    };

    defer allocator.free(result.stderr);

    const exit_code: u8 = switch (result.term) {
        .Exited => |code| code,
        else => 255,
    };

    if (exit_code != 0) {
        std.debug.print("Command failed with code {d}: {s}\n", .{ exit_code, result.stderr });
        allocator.free(result.stdout);
        return error.CommandFailed;
    }

    return result.stdout;
}

// ============================================================================
// Test: Basic Stack Detection
// ============================================================================
test "stack detection with simple branch hierarchy" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "stack-simple");
    defer repo.deinit();

    // Create initial commit on main
    try repo.createFile("README.md", "# Test Project\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Create feature branch 1
    try repo.createBranch("feature/TEST-1-first");
    try repo.createFile("feature1.txt", "Feature 1 content\n");
    try repo.commitAll("Add feature 1");

    // Create feature branch 2 (stacked on 1)
    try repo.createBranch("feature/TEST-2-second");
    try repo.createFile("feature2.txt", "Feature 2 content\n");
    try repo.commitAll("Add feature 2");

    // Analyze stack
    const stack = try stack_mod.analyzeStack(allocator);

    // Verify
    try testing.expectEqual(@as(usize, 2), stack.branches.len);
    try testing.expectEqualStrings("main", stack.base_branch);
    try testing.expectEqualStrings("feature/TEST-2-second", stack.head_branch);

    try testing.expectEqualStrings("feature/TEST-1-first", stack.branches[0].name);
    try testing.expectEqualStrings("main", stack.branches[0].parent_branch.?);
    try testing.expectEqual(@as(u32, 1), stack.branches[0].commits_from_parent);

    try testing.expectEqualStrings("feature/TEST-2-second", stack.branches[1].name);
    try testing.expectEqualStrings("feature/TEST-1-first", stack.branches[1].parent_branch.?);
    try testing.expectEqual(@as(u32, 1), stack.branches[1].commits_from_parent);
}

// ============================================================================
// Test: Stack with Multiple Commits per Branch
// ============================================================================
test "stack detection with multiple commits per branch" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "stack-multi-commit");
    defer repo.deinit();

    // Create initial commit on main
    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Create feature branch with 3 commits
    try repo.createBranch("feature/TEST-1-multi");
    try repo.createFile("file1.txt", "File 1\n");
    try repo.commitAll("Commit 1");
    try repo.createFile("file2.txt", "File 2\n");
    try repo.commitAll("Commit 2");
    try repo.createFile("file3.txt", "File 3\n");
    try repo.commitAll("Commit 3");

    // Analyze stack
    const stack = try stack_mod.analyzeStack(allocator);

    try testing.expectEqual(@as(usize, 1), stack.branches.len);
    try testing.expectEqual(@as(u32, 3), stack.branches[0].commits_from_parent);
}

// ============================================================================
// Test: Diff Detection for Staged Files
// ============================================================================
test "diff detection for staged changes" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "diff-staged");
    defer repo.deinit();

    // Setup
    try repo.createFile("README.md", "Initial content\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    try repo.createBranch("feature/TEST-1-diff");
    try repo.createFile("feature.txt", "Feature content\n");
    try repo.commitAll("Add feature");

    // Make a staged change
    try repo.appendFile("feature.txt", "Modified content\n");
    _ = try repo.git(&.{ "add", "feature.txt" });

    // Detect staged files
    const staged = try diff_mod.getStagedFiles(allocator);

    try testing.expectEqual(@as(usize, 1), staged.len);
    try testing.expectEqualStrings("feature.txt", staged[0].path);
    try testing.expectEqual(types.ChangeType.modified, staged[0].change_type);
    try testing.expect(staged[0].staged);
}

// ============================================================================
// Test: Diff Detection for Unstaged Files
// ============================================================================
test "diff detection for unstaged changes" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "diff-unstaged");
    defer repo.deinit();

    // Setup
    try repo.createFile("README.md", "Initial content\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    try repo.createBranch("feature/TEST-1-diff");
    try repo.createFile("feature.txt", "Feature content\n");
    try repo.commitAll("Add feature");

    // Make an unstaged change
    try repo.appendFile("feature.txt", "Unstaged modification\n");

    // Detect unstaged files
    const unstaged = try diff_mod.getUnstagedFiles(allocator);

    try testing.expectEqual(@as(usize, 1), unstaged.len);
    try testing.expectEqualStrings("feature.txt", unstaged[0].path);
    try testing.expect(!unstaged[0].staged);
}

// ============================================================================
// Test: YAML Plan Emit and Parse Round-trip
// ============================================================================
test "yaml plan round-trip" {
    const allocator = testing.allocator;

    // Create a plan
    const plan = types.Plan{
        .version = 1,
        .generated = "2025-01-01T00:00:00Z",
        .repository = "/test/repo",
        .errors = &[_]types.PlanError{},
        .stack = .{
            .branches = &[_]types.StackBranch{
                .{
                    .name = "feature/TEST-1-branch",
                    .commit_sha = "abc123def456",
                    .parent_branch = "main",
                    .commits_from_parent = 2,
                    .needs_fix = true,
                    .fix = types.Fix{
                        .commit_message = "fix: Test fix",
                        .files = &[_]types.FileChange{
                            .{
                                .path = "test.txt",
                                .change_type = .modified,
                                .diff = "--- a/test.txt\n+++ b/test.txt\n@@ -1 +1 @@\n-old\n+new\n",
                                .staged = true,
                            },
                        },
                    },
                    .step_status = .pending,
                },
            },
            .base_branch = "main",
            .base_commit = "base123",
            .head_branch = "feature/TEST-1-branch",
            .head_commit = "abc123def456",
        },
    };

    // Emit to YAML
    const yaml = try emitter.emitPlan(allocator, plan);
    defer allocator.free(yaml);

    // Parse back
    const parsed = try parser.parsePlan(allocator, yaml);

    // Verify round-trip
    try testing.expectEqual(@as(u32, 1), parsed.version);
    try testing.expectEqualStrings("2025-01-01T00:00:00Z", parsed.generated);
    try testing.expectEqualStrings("/test/repo", parsed.repository);
    try testing.expectEqual(@as(usize, 0), parsed.errors.len);
    try testing.expectEqual(@as(usize, 1), parsed.stack.branches.len);
    try testing.expectEqualStrings("feature/TEST-1-branch", parsed.stack.branches[0].name);
    try testing.expect(parsed.stack.branches[0].needs_fix);
    try testing.expect(parsed.stack.branches[0].fix != null);
}

// ============================================================================
// Test: Plan with Errors
// ============================================================================
test "yaml plan with errors" {
    const allocator = testing.allocator;

    const plan = types.Plan{
        .version = 1,
        .generated = "2025-01-01T00:00:00Z",
        .repository = "/test/repo",
        .errors = &[_]types.PlanError{
            .{
                .error_type = .unmapped_file,
                .path = "new_file.txt",
                .message = "New file detected",
            },
            .{
                .error_type = .outside_ancestry,
                .path = "other.txt",
                .message = "Outside ancestry",
            },
        },
        .stack = .{
            .branches = &[_]types.StackBranch{},
            .base_branch = "main",
            .base_commit = "abc",
            .head_branch = "feature/x",
            .head_commit = "def",
        },
    };

    // Emit
    const yaml = try emitter.emitPlan(allocator, plan);
    defer allocator.free(yaml);

    // Parse
    const parsed = try parser.parsePlan(allocator, yaml);

    try testing.expectEqual(@as(usize, 2), parsed.errors.len);
    try testing.expectEqual(types.ErrorType.unmapped_file, parsed.errors[0].error_type);
    try testing.expectEqualStrings("new_file.txt", parsed.errors[0].path);
}

// ============================================================================
// Test: File to Branch Mapping
// ============================================================================
test "file to branch mapping" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "file-mapping");
    defer repo.deinit();

    // Setup main
    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Branch 1 with specific file
    try repo.createBranch("feature/TEST-1-api");
    try repo.createFile("api.txt", "API content\n");
    try repo.commitAll("Add API");

    // Branch 2 with different file
    try repo.createBranch("feature/TEST-2-ui");
    try repo.createFile("ui.txt", "UI content\n");
    try repo.commitAll("Add UI");

    // Get stack
    const stack = try stack_mod.analyzeStack(allocator);

    // Modify file from branch 1
    try repo.appendFile("api.txt", "Modified API\n");

    // Find last commit for this file
    const last_commit = try diff_mod.getLastCommitForFile(allocator, "api.txt");
    try testing.expect(last_commit != null);

    // Find which branch it belongs to
    const target_branch = try diff_mod.findBranchForCommit(allocator, last_commit.?, stack);
    try testing.expect(target_branch != null);
    try testing.expectEqualStrings("feature/TEST-1-api", target_branch.?);
}

// ============================================================================
// Test: JIRA Prefix Extraction
// ============================================================================
test "jira prefix extraction" {
    try testing.expectEqualStrings("TEST", strings.extractJiraPrefix("feature/TEST-123-description").?);
    try testing.expectEqualStrings("PDI", strings.extractJiraPrefix("feature/PDI-1-short").?);
    try testing.expectEqualStrings("ABC", strings.extractJiraPrefix("bugfix/ABC-99-fix").?);
    try testing.expect(strings.extractJiraPrefix("main") == null);
    try testing.expect(strings.extractJiraPrefix("develop") == null);
    try testing.expect(strings.extractJiraPrefix("feature/no-ticket") == null);
}

// ============================================================================
// Test: Execution State Round-trip
// ============================================================================
test "execution state json round-trip" {
    const allocator = testing.allocator;

    const state = types.ExecutionState{
        .plan_file = "jenga-plan.yml",
        .plan_hash = "sha256:abc123",
        .worktree_path = "../repo-jenga",
        .current_step_index = 2,
        .started_at = "2025-01-01T10:00:00Z",
        .last_updated = "2025-01-01T10:05:00Z",
        .status = .conflict,
        .completed_branches = &[_][]const u8{ "branch-1-fix", "branch-2-fix" },
    };

    // Emit
    const json = try emitter.emitState(allocator, state);
    defer allocator.free(json);

    // Parse
    const parsed = try parser.parseState(allocator, json);

    try testing.expectEqualStrings("jenga-plan.yml", parsed.plan_file);
    try testing.expectEqual(@as(u32, 2), parsed.current_step_index);
    try testing.expectEqual(types.ExecutionStatus.conflict, parsed.status);
    try testing.expectEqual(@as(usize, 2), parsed.completed_branches.len);
}

// ============================================================================
// Test: Full End-to-End Without Conflicts
// ============================================================================
test "end-to-end execution without conflicts" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "e2e-simple");
    defer repo.deinit();

    // Setup: main with initial commit
    try repo.createFile("README.md", "# Project\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Create stacked branches
    try repo.createBranch("feature/TEST-1-base");
    try repo.createFile("base.txt", "Base feature\n");
    try repo.commitAll("Add base feature");

    try repo.createBranch("feature/TEST-2-top");
    try repo.createFile("top.txt", "Top feature\n");
    try repo.commitAll("Add top feature");

    // Make a modification to base.txt (should map to TEST-1)
    try repo.appendFile("base.txt", "Modified in TEST-2\n");

    // Verify stack detection works
    const stack = try stack_mod.analyzeStack(allocator);
    try testing.expectEqual(@as(usize, 2), stack.branches.len);

    // Verify change detection works
    const unstaged = try diff_mod.getUnstagedFiles(allocator);
    try testing.expectEqual(@as(usize, 1), unstaged.len);
    try testing.expectEqualStrings("base.txt", unstaged[0].path);

    // Verify file mapping works
    const last_commit = try diff_mod.getLastCommitForFile(allocator, "base.txt");
    try testing.expect(last_commit != null);

    const target = try diff_mod.findBranchForCommit(allocator, last_commit.?, stack);
    try testing.expect(target != null);
    try testing.expectEqualStrings("feature/TEST-1-base", target.?);
}

// ============================================================================
// Test: Develop as Base Branch
// ============================================================================
test "stack detection with develop as base" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "develop-base");
    defer repo.deinit();

    // Create initial commit on develop (not main)
    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "develop" });

    // Create feature branch
    try repo.createBranch("feature/TEST-1-feature");
    try repo.createFile("feature.txt", "Content\n");
    try repo.commitAll("Add feature");

    // Analyze stack
    const stack = try stack_mod.analyzeStack(allocator);

    try testing.expectEqualStrings("develop", stack.base_branch);
    try testing.expectEqual(@as(usize, 1), stack.branches.len);
}

// ============================================================================
// Test: Empty Stack (HEAD at base)
// ============================================================================
test "empty stack when head is at base" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "empty-stack");
    defer repo.deinit();

    // Just main with one commit
    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Create a feature branch but no commits
    try repo.createBranch("feature/TEST-1-empty");

    // Stack should be empty (no commits between main and HEAD)
    const stack = try stack_mod.analyzeStack(allocator);

    // The branch exists but points to same commit as main
    try testing.expectEqual(@as(usize, 0), stack.branches.len);
}

// ============================================================================
// Test: Deep Stack (5+ branches)
// ============================================================================
test "deep stack with many branches" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "deep-stack");
    defer repo.deinit();

    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    // Create 5 stacked branches
    const branch_names = [_][]const u8{
        "feature/TEST-1-layer1",
        "feature/TEST-2-layer2",
        "feature/TEST-3-layer3",
        "feature/TEST-4-layer4",
        "feature/TEST-5-layer5",
    };

    for (branch_names, 1..) |name, i| {
        try repo.createBranch(name);
        const filename = try std.fmt.allocPrint(allocator, "layer{d}.txt", .{i});
        defer allocator.free(filename);
        try repo.createFile(filename, "Content\n");
        try repo.commitAll("Add layer");
    }

    const stack = try stack_mod.analyzeStack(allocator);

    try testing.expectEqual(@as(usize, 5), stack.branches.len);

    // Verify chain
    try testing.expectEqualStrings("main", stack.branches[0].parent_branch.?);
    for (1..5) |i| {
        try testing.expectEqualStrings(branch_names[i - 1], stack.branches[i].parent_branch.?);
    }
}

// ============================================================================
// Test: New File Detection (Unmapped)
// ============================================================================
test "new file should be unmapped" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "new-file");
    defer repo.deinit();

    // Setup
    try repo.createFile("README.md", "# Test\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    try repo.createBranch("feature/TEST-1-feature");
    try repo.createFile("existing.txt", "Existing\n");
    try repo.commitAll("Add existing");

    // Create a brand new file (not committed anywhere)
    try repo.createFile("brand_new.txt", "Brand new content\n");
    _ = try repo.git(&.{ "add", "brand_new.txt" });

    // The new file should have no last commit
    const last_commit = try diff_mod.getLastCommitForFile(allocator, "brand_new.txt");
    try testing.expect(last_commit == null);
}

// ============================================================================
// Test: Get File Diff Content
// ============================================================================
test "get file diff content" {
    const allocator = testing.allocator;

    var repo = try TestRepo.init(allocator, "diff-content");
    defer repo.deinit();

    try repo.createFile("README.md", "Line 1\n");
    try repo.commitAll("Initial commit");
    _ = try repo.git(&.{ "branch", "-M", "main" });

    try repo.createBranch("feature/TEST-1-diff");
    try repo.createFile("test.txt", "Original content\n");
    try repo.commitAll("Add test file");

    // Modify and stage
    try repo.appendFile("test.txt", "Added line\n");
    _ = try repo.git(&.{ "add", "test.txt" });

    // Get diff
    const diff = try diff_mod.getFileDiff(allocator, "test.txt", true);
    defer allocator.free(diff);

    // Verify diff contains expected markers
    try testing.expect(std.mem.indexOf(u8, diff, "diff --git") != null);
    try testing.expect(std.mem.indexOf(u8, diff, "+Added line") != null);
}

pub fn main() !void {
    std.debug.print("Running integration tests...\n", .{});
}
