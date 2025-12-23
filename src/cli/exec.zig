const std = @import("std");
const types = @import("../types.zig");
const parser = @import("../yaml/parser.zig");
const emitter = @import("../yaml/emitter.zig");
const strings = @import("../utils/strings.zig");
const process = @import("../utils/process.zig");

const STATE_DIR = ".git/git-jenga";
const STATE_FILE = ".git/git-jenga/state.json";
const DEFAULT_PLAN_FILE = ".git/git-jenga/plan.yml";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var plan_file: ?[]const u8 = null;
    var worktree_path: ?[]const u8 = null;
    var continue_mode = false;
    var abort_mode = false;
    var force = false;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--worktree-path")) {
            if (i + 1 < args.len) {
                i += 1;
                worktree_path = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--continue")) {
            continue_mode = true;
        } else if (std.mem.eql(u8, arg, "--abort")) {
            abort_mode = true;
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            plan_file = arg;
        }
    }

    // Handle abort
    if (abort_mode) {
        try handleAbort(allocator);
        return;
    }

    // Handle continue
    if (continue_mode) {
        try handleContinue(allocator);
        return;
    }

    // Use default plan file if not specified
    const actual_plan_file = plan_file orelse DEFAULT_PLAN_FILE;

    // Read and parse plan
    const plan_content = std.fs.cwd().readFileAlloc(allocator, actual_plan_file, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error: Could not read plan file '{s}': {any}\n", .{ actual_plan_file, err });
        std.process.exit(1);
    };
    defer allocator.free(plan_content);

    const plan = parser.parsePlan(allocator, plan_content) catch |err| {
        std.debug.print("Error: Could not parse plan file: {any}\n", .{err});
        std.process.exit(1);
    };

    // Validate: errors must be empty
    if (plan.errors.len > 0) {
        std.debug.print("\x1b[31mError:\x1b[0m Plan has {d} unresolved errors.\n\n", .{plan.errors.len});

        for (plan.errors, 1..) |err, num| {
            std.debug.print("  {d}. [{s}] {s}\n", .{ num, err.error_type.toString(), err.path });
            std.debug.print("     {s}\n\n", .{err.message});
        }

        std.debug.print("Fix these in {s} first, then run: git-jenga exec {s}\n", .{ actual_plan_file, actual_plan_file });
        std.process.exit(3);
    }

    // Check if state file exists (interrupted execution)
    if (stateExists()) {
        std.debug.print("\x1b[33mWarning:\x1b[0m Found interrupted execution.\n", .{});
        std.debug.print("Run with --continue to resume or --abort to clean up.\n", .{});
        std.process.exit(1);
    }

    // Determine worktree path
    const cwd = try process.getCwd(allocator);
    const repo_name = std.fs.path.basename(cwd);
    const default_worktree = try std.fmt.allocPrint(allocator, "../{s}-jenga", .{repo_name});
    const wt_path = worktree_path orelse default_worktree;

    // Check if worktree path exists
    const wt_exists = blk: {
        std.fs.cwd().access(wt_path, .{}) catch break :blk false;
        break :blk true;
    };

    if (wt_exists and !force) {
        std.debug.print("Error: Worktree path '{s}' already exists.\n", .{wt_path});
        std.debug.print("Use --force to remove it first, or --worktree-path to specify a different path.\n", .{});
        std.process.exit(1);
    }

    if (wt_exists and force) {
        std.debug.print("Removing existing worktree: {s}\n", .{wt_path});
        _ = process.runGit(allocator, &.{ "worktree", "remove", wt_path, "--force" }) catch {};
    }

    // Count branches that need fixing
    var branches_to_fix: u32 = 0;
    for (plan.stack.branches) |branch| {
        if (branch.needs_fix) branches_to_fix += 1;
    }

    // If no fixes and no verify command, nothing to do
    if (branches_to_fix == 0 and plan.verify_cmd == null) {
        std.debug.print("No branches need fixing. Nothing to do.\n", .{});
        return;
    }

    if (branches_to_fix == 0 and plan.verify_cmd != null) {
        std.debug.print("\nVerify-only mode: {d} branches to restack with verification\n\n", .{
            plan.stack.branches.len,
        });
    } else {
        std.debug.print("\nExecuting plan: {d} branches to restack, {d} need fixes\n\n", .{
            plan.stack.branches.len,
            branches_to_fix,
        });
    }

    // Create worktree
    std.debug.print("Creating worktree at: {s}\n", .{wt_path});
    _ = process.runGit(allocator, &.{ "worktree", "add", wt_path, plan.stack.base_branch }) catch |err| {
        std.debug.print("Error: Could not create worktree: {any}\n", .{err});
        std.process.exit(1);
    };

    // Initialize state
    const timestamp = try strings.formatTimestamp(allocator);
    var state = types.ExecutionState{
        .plan_file = actual_plan_file,
        .plan_hash = "sha256:TODO",
        .worktree_path = wt_path,
        .current_step_index = 0,
        .current_commit_index = 0,
        .started_at = timestamp,
        .last_updated = timestamp,
        .status = .in_progress,
        .completed_branches = &[_][]const u8{},
        .verify_cmd = plan.verify_cmd,
        .mode = .exec,
    };

    try saveState(allocator, state);

    // Execute each branch
    var completed: std.ArrayListUnmanaged([]const u8) = .{};
    defer completed.deinit(allocator);

    for (plan.stack.branches, 0..) |branch, idx| {
        state.current_step_index = @intCast(idx);
        state.last_updated = try strings.formatTimestamp(allocator);
        try saveState(allocator, state);

        std.debug.print("\n[{d}/{d}] Processing: {s}\n", .{ idx + 1, plan.stack.branches.len, branch.name });

        const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});

        // Create the fix branch in worktree
        std.debug.print("  Creating branch: {s}\n", .{fix_branch_name});

        _ = process.runGitInDir(allocator, wt_path, &.{ "checkout", "-b", fix_branch_name }) catch |err| {
            std.debug.print("Error: Could not create branch: {any}\n", .{err});
            state.status = .failed;
            try saveState(allocator, state);
            std.process.exit(1);
        };

        // Cherry-pick commits from parent to this branch (one at a time for verification)
        if (branch.parent_branch) |parent| {
            const range = try std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, branch.name });
            defer allocator.free(range);

            // Get list of commits to cherry-pick
            const commits_output = process.runGit(allocator, &.{
                "rev-list",
                "--reverse",
                range,
            }) catch |err| {
                std.debug.print("Error: Could not list commits: {any}\n", .{err});
                state.status = .failed;
                try saveState(allocator, state);
                std.process.exit(1);
            };
            defer allocator.free(commits_output);

            var commits_iter = std.mem.splitScalar(u8, strings.trim(commits_output), '\n');
            var commit_count: u32 = 0;
            while (commits_iter.next()) |commit_sha| {
                if (commit_sha.len == 0) continue;
                commit_count += 1;

                std.debug.print("  Cherry-picking commit {d}: {s}\n", .{ commit_count, commit_sha[0..@min(7, commit_sha.len)] });

                const cherry_result = process.runGitWithStatus(allocator, &.{
                    "-C",
                    wt_path,
                    "cherry-pick",
                    commit_sha,
                }) catch |err| {
                    std.debug.print("Error: Cherry-pick failed: {any}\n", .{err});
                    state.status = .conflict;
                    try saveState(allocator, state);
                    std.process.exit(2);
                };
                defer allocator.free(cherry_result.stdout);
                defer allocator.free(cherry_result.stderr);

                if (cherry_result.exit_code != 0) {
                    std.debug.print("\n\x1b[33mConflict detected during cherry-pick.\x1b[0m\n\n", .{});
                    std.debug.print("Resolve conflicts in worktree: {s}\n\n", .{wt_path});

                    // Show future diffs to help with conflict resolution
                    showFutureDiffs(allocator, wt_path, plan, idx);

                    std.debug.print("Then run:\n", .{});
                    std.debug.print("  cd {s}\n", .{wt_path});
                    std.debug.print("  git add <resolved_files>\n", .{});
                    std.debug.print("  git cherry-pick --continue\n", .{});
                    std.debug.print("  cd -\n", .{});
                    std.debug.print("  git-jenga exec {s} --continue\n\n", .{actual_plan_file});
                    std.debug.print("Or abort:\n", .{});
                    std.debug.print("  git-jenga exec {s} --abort\n", .{actual_plan_file});

                    state.status = .conflict;
                    try saveState(allocator, state);
                    std.process.exit(2);
                }
            }
            if (commit_count > 0) {
                std.debug.print("  Cherry-picked {d} commits\n", .{commit_count});
            }
        }

        // Apply fixes if needed
        if (branch.needs_fix) {
            if (branch.fix) |fix| {
                std.debug.print("  Applying {d} file fixes\n", .{fix.files.len});

                for (fix.files) |file| {
                    // Write diff to temp file
                    const tmp_path = try std.fmt.allocPrint(allocator, "{s}/.git-jenga-patch", .{wt_path});
                    defer allocator.free(tmp_path);

                    const tmp_file = try std.fs.cwd().createFile(tmp_path, .{});
                    try tmp_file.writeAll(file.diff);
                    tmp_file.close();

                    // Apply the patch
                    const apply_result = process.runGitWithStatus(allocator, &.{
                        "-C",
                        wt_path,
                        "apply",
                        "--allow-empty",
                        tmp_path,
                    }) catch {
                        std.debug.print("    Warning: Could not apply patch for {s}\n", .{file.path});
                        continue;
                    };
                    defer allocator.free(apply_result.stdout);
                    defer allocator.free(apply_result.stderr);

                    if (apply_result.exit_code != 0) {
                        std.debug.print("    Warning: Patch apply failed for {s}: {s}\n", .{ file.path, apply_result.stderr });
                    } else {
                        std.debug.print("    Applied: {s}\n", .{file.path});
                    }

                    // Clean up temp file
                    std.fs.cwd().deleteFile(tmp_path) catch {};
                }

                // Stage and commit
                _ = process.runGitInDir(allocator, wt_path, &.{ "add", "-A" }) catch {};

                const commit_result = process.runGitWithStatus(allocator, &.{
                    "-C",
                    wt_path,
                    "commit",
                    "-m",
                    fix.commit_message,
                    "--allow-empty",
                }) catch {
                    std.debug.print("    Warning: Commit failed\n", .{});
                    continue;
                };
                defer allocator.free(commit_result.stdout);
                defer allocator.free(commit_result.stderr);

                if (commit_result.exit_code == 0) {
                    std.debug.print("  Committed fix\n", .{});
                }
            }
        }

        // Run verification once at branch completion if verify_cmd is set
        if (plan.verify_cmd) |cmd| {
            if (!runVerification(allocator, wt_path, cmd)) {
                std.debug.print("\n\x1b[31mVerification failed for branch {s}!\x1b[0m\n", .{branch.name});
                std.debug.print("Fix the issue in: {s}\n", .{wt_path});
                std.debug.print("Then amend the commit and run: git-jenga exec --continue\n", .{});
                state.status = .conflict;
                try saveState(allocator, state);
                std.process.exit(4);
            }
        }

        try completed.append(allocator, fix_branch_name);
        std.debug.print("  \x1b[32m✓\x1b[0m {s} complete\n", .{fix_branch_name});
    }

    // Complete - first execution path done
    state.status = .completed;
    state.completed_branches = try completed.toOwnedSlice(allocator);
    state.last_updated = try strings.formatTimestamp(allocator);

    std.debug.print("\n\x1b[32m══════════════════════════════════════════\x1b[0m\n", .{});
    std.debug.print("\x1b[32m Execution complete! \x1b[0m\n", .{});
    std.debug.print("\x1b[32m══════════════════════════════════════════\x1b[0m\n\n", .{});

    std.debug.print("Worktree: {s}\n\n", .{wt_path});
    std.debug.print("Created branches:\n", .{});

    for (state.completed_branches) |branch| {
        std.debug.print("  • {s}\n", .{branch});
    }

    std.debug.print("\nNext steps:\n", .{});
    std.debug.print("  1. Review the -fix branches in {s}\n", .{wt_path});
    std.debug.print("  2. If satisfied, push the -fix branches\n", .{});
    std.debug.print("  3. Update PRs to point to the -fix branches\n", .{});
    std.debug.print("  4. Clean up with: git worktree remove {s}\n", .{wt_path});

    // Clean up state file
    cleanupState();
}

fn handleAbort(allocator: std.mem.Allocator) !void {
    if (!stateExists()) {
        std.debug.print("Error: No execution in progress.\n", .{});
        std.process.exit(1);
    }

    const state_content = std.fs.cwd().readFileAlloc(allocator, STATE_FILE, 1024 * 1024) catch {
        std.debug.print("Error: Could not read state file.\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(state_content);

    const state = parser.parseState(allocator, state_content) catch {
        std.debug.print("Error: Could not parse state file.\n", .{});
        std.process.exit(1);
    };

    std.debug.print("Aborting execution...\n", .{});
    std.debug.print("Removing worktree: {s}\n", .{state.worktree_path});

    // Remove worktree
    _ = process.runGit(allocator, &.{ "worktree", "remove", state.worktree_path, "--force" }) catch {};

    // Clean up state
    cleanupState();

    std.debug.print("\x1b[32mAborted.\x1b[0m Original branches unchanged.\n", .{});
}

fn handleContinue(allocator: std.mem.Allocator) !void {
    if (!stateExists()) {
        std.debug.print("Error: No execution in progress.\n", .{});
        std.process.exit(1);
    }

    // Load state
    const state_content = std.fs.cwd().readFileAlloc(allocator, STATE_FILE, 1024 * 1024) catch {
        std.debug.print("Error: Could not read state file.\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(state_content);

    var state = parser.parseState(allocator, state_content) catch {
        std.debug.print("Error: Could not parse state file.\n", .{});
        std.process.exit(1);
    };

    std.debug.print("Resuming execution from step {d}...\n", .{state.current_step_index + 1});
    std.debug.print("Worktree: {s}\n", .{state.worktree_path});

    // Load plan
    const plan_content = std.fs.cwd().readFileAlloc(allocator, state.plan_file, 10 * 1024 * 1024) catch {
        std.debug.print("Error: Could not read plan file '{s}'\n", .{state.plan_file});
        std.process.exit(1);
    };
    defer allocator.free(plan_content);

    const plan = parser.parsePlan(allocator, plan_content) catch {
        std.debug.print("Error: Could not parse plan file.\n", .{});
        std.process.exit(1);
    };

    const wt_path = state.worktree_path;

    // Check worktree still exists
    std.fs.cwd().access(wt_path, .{}) catch {
        std.debug.print("Error: Worktree '{s}' no longer exists.\n", .{wt_path});
        std.debug.print("Run --abort to clean up state.\n", .{});
        std.process.exit(1);
    };

    // Build completed branches list from state
    var completed: std.ArrayListUnmanaged([]const u8) = .{};
    defer completed.deinit(allocator);
    for (state.completed_branches) |branch| {
        try completed.append(allocator, branch);
    }

    // Resume from current_step_index
    // The conflict happened during cherry-pick, which the user has now resolved
    // We need to:
    // 1. Apply fixes for the current branch (if any)
    // 2. Continue with remaining branches

    const start_idx = state.current_step_index;

    for (plan.stack.branches[start_idx..], start_idx..) |branch, idx| {
        const is_first = idx == start_idx;

        state.current_step_index = @intCast(idx);
        state.last_updated = try strings.formatTimestamp(allocator);
        state.status = .in_progress;
        try saveState(allocator, state);

        std.debug.print("\n[{d}/{d}] Processing: {s}\n", .{ idx + 1, plan.stack.branches.len, branch.name });

        const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});

        // For the first (resumed) branch, we're already on it after cherry-pick --continue
        // For subsequent branches, we need to create and cherry-pick
        if (!is_first) {
            // Create the fix branch in worktree
            std.debug.print("  Creating branch: {s}\n", .{fix_branch_name});

            _ = process.runGitInDir(allocator, wt_path, &.{ "checkout", "-b", fix_branch_name }) catch |err| {
                std.debug.print("Error: Could not create branch: {any}\n", .{err});
                state.status = .failed;
                try saveState(allocator, state);
                std.process.exit(1);
            };

            // Cherry-pick commits from parent to this branch (one at a time for verification)
            if (branch.parent_branch) |parent| {
                const range = try std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, branch.name });
                defer allocator.free(range);

                // Get list of commits to cherry-pick
                const commits_output = process.runGit(allocator, &.{
                    "rev-list",
                    "--reverse",
                    range,
                }) catch |err| {
                    std.debug.print("Error: Could not list commits: {any}\n", .{err});
                    state.status = .failed;
                    try saveState(allocator, state);
                    std.process.exit(1);
                };
                defer allocator.free(commits_output);

                var commits_iter = std.mem.splitScalar(u8, strings.trim(commits_output), '\n');
                var commit_count: u32 = 0;
                while (commits_iter.next()) |commit_sha| {
                    if (commit_sha.len == 0) continue;
                    commit_count += 1;

                    std.debug.print("  Cherry-picking commit {d}: {s}\n", .{ commit_count, commit_sha[0..@min(7, commit_sha.len)] });

                    const cherry_result = process.runGitWithStatus(allocator, &.{
                        "-C",
                        wt_path,
                        "cherry-pick",
                        commit_sha,
                    }) catch |err| {
                        std.debug.print("Error: Cherry-pick failed: {any}\n", .{err});
                        state.status = .conflict;
                        try saveState(allocator, state);
                        std.process.exit(2);
                    };
                    defer allocator.free(cherry_result.stdout);
                    defer allocator.free(cherry_result.stderr);

                    if (cherry_result.exit_code != 0) {
                        std.debug.print("\n\x1b[33mConflict detected during cherry-pick.\x1b[0m\n\n", .{});
                        std.debug.print("Resolve conflicts in worktree: {s}\n\n", .{wt_path});

                        // Show future diffs to help with conflict resolution
                        showFutureDiffs(allocator, wt_path, plan, idx);

                        std.debug.print("Then run:\n", .{});
                        std.debug.print("  cd {s}\n", .{wt_path});
                        std.debug.print("  git add <resolved_files>\n", .{});
                        std.debug.print("  git cherry-pick --continue\n", .{});
                        std.debug.print("  cd -\n", .{});
                        std.debug.print("  git-jenga exec --continue\n\n", .{});
                        std.debug.print("Or abort:\n", .{});
                        std.debug.print("  git-jenga exec --abort\n", .{});

                        state.status = .conflict;
                        try saveState(allocator, state);
                        std.process.exit(2);
                    }
                }
                if (commit_count > 0) {
                    std.debug.print("  Cherry-picked {d} commits\n", .{commit_count});
                }
            }
        } else {
            std.debug.print("  Resumed after conflict resolution\n", .{});
        }

        // Apply fixes if needed
        if (branch.needs_fix) {
            if (branch.fix) |fix| {
                std.debug.print("  Applying {d} file fixes\n", .{fix.files.len});

                for (fix.files) |file| {
                    // Write diff to temp file
                    const tmp_path = try std.fmt.allocPrint(allocator, "{s}/.git-jenga-patch", .{wt_path});
                    defer allocator.free(tmp_path);

                    const tmp_file = try std.fs.cwd().createFile(tmp_path, .{});
                    try tmp_file.writeAll(file.diff);
                    tmp_file.close();

                    // Apply the patch
                    const apply_result = process.runGitWithStatus(allocator, &.{
                        "-C",
                        wt_path,
                        "apply",
                        "--allow-empty",
                        tmp_path,
                    }) catch {
                        std.debug.print("    Warning: Could not apply patch for {s}\n", .{file.path});
                        continue;
                    };
                    defer allocator.free(apply_result.stdout);
                    defer allocator.free(apply_result.stderr);

                    if (apply_result.exit_code != 0) {
                        std.debug.print("    Warning: Patch apply failed for {s}: {s}\n", .{ file.path, apply_result.stderr });
                    } else {
                        std.debug.print("    Applied: {s}\n", .{file.path});
                    }

                    // Clean up temp file
                    std.fs.cwd().deleteFile(tmp_path) catch {};
                }

                // Stage and commit
                _ = process.runGitInDir(allocator, wt_path, &.{ "add", "-A" }) catch {};

                const commit_result = process.runGitWithStatus(allocator, &.{
                    "-C",
                    wt_path,
                    "commit",
                    "-m",
                    fix.commit_message,
                    "--allow-empty",
                }) catch {
                    std.debug.print("    Warning: Commit failed\n", .{});
                    continue;
                };
                defer allocator.free(commit_result.stdout);
                defer allocator.free(commit_result.stderr);

                if (commit_result.exit_code == 0) {
                    std.debug.print("  Committed fix\n", .{});
                }
            }
        }

        // Run verification once at branch completion if verify_cmd is set
        if (state.verify_cmd) |cmd| {
            if (!runVerification(allocator, wt_path, cmd)) {
                std.debug.print("\n\x1b[31mVerification failed for branch {s}!\x1b[0m\n", .{branch.name});
                std.debug.print("Fix the issue in: {s}\n", .{wt_path});
                std.debug.print("Then amend the commit and run: git-jenga exec --continue\n", .{});
                state.status = .conflict;
                try saveState(allocator, state);
                std.process.exit(4);
            }
        }

        try completed.append(allocator, fix_branch_name);
        std.debug.print("  \x1b[32m✓\x1b[0m {s} complete\n", .{fix_branch_name});

        // If mode is step, process only one branch then exit
        if (state.mode == .step) {
            state.current_step_index = @intCast(idx + 1);
            state.current_commit_index = 0;
            state.last_updated = try strings.formatTimestamp(allocator);

            if (state.current_step_index >= plan.stack.branches.len) {
                state.status = .completed;
                try saveState(allocator, state);
                std.debug.print("\n\x1b[32mAll {d} branches complete!\x1b[0m\n", .{plan.stack.branches.len});
                std.debug.print("Run: git-jenga apply\n", .{});
                cleanupState();
            } else {
                try saveState(allocator, state);
                std.debug.print("\nStep {d}/{d} done. Run 'git-jenga step' for next.\n", .{ state.current_step_index, plan.stack.branches.len });
            }
            return;
        }
    }

    // Complete - continue path done (exec mode)
    state.status = .completed;
    state.completed_branches = try completed.toOwnedSlice(allocator);
    state.last_updated = try strings.formatTimestamp(allocator);

    std.debug.print("\n\x1b[32m══════════════════════════════════════════\x1b[0m\n", .{});
    std.debug.print("\x1b[32m Execution complete! \x1b[0m\n", .{});
    std.debug.print("\x1b[32m══════════════════════════════════════════\x1b[0m\n\n", .{});

    std.debug.print("Worktree: {s}\n\n", .{wt_path});
    std.debug.print("Created branches:\n", .{});

    for (state.completed_branches) |branch| {
        std.debug.print("  • {s}\n", .{branch});
    }

    std.debug.print("\nNext steps:\n", .{});
    std.debug.print("  1. Review the -fix branches in {s}\n", .{wt_path});
    std.debug.print("  2. If satisfied, push the -fix branches\n", .{});
    std.debug.print("  3. Update PRs to point to the -fix branches\n", .{});
    std.debug.print("  4. Clean up with: git worktree remove {s}\n", .{wt_path});

    // Clean up state file
    cleanupState();
}

fn stateExists() bool {
    std.fs.cwd().access(STATE_FILE, .{}) catch return false;
    return true;
}

fn saveState(allocator: std.mem.Allocator, state: types.ExecutionState) !void {
    // Ensure directory exists
    std.fs.cwd().makePath(STATE_DIR) catch {};

    const json = try emitter.emitState(allocator, state);
    defer allocator.free(json);

    const file = try std.fs.cwd().createFile(STATE_FILE, .{});
    defer file.close();
    try file.writeAll(json);
}

fn cleanupState() void {
    std.fs.cwd().deleteFile(STATE_FILE) catch {};
    std.fs.cwd().deleteDir(STATE_DIR) catch {};
}

/// Run verification command in worktree directory
/// Returns true if verification passed, false if failed
fn runVerification(allocator: std.mem.Allocator, wt_path: []const u8, verify_cmd: []const u8) bool {
    std.debug.print("  Running verification: {s}\n", .{verify_cmd});

    // Run the command via sh -c in the worktree directory
    // Use large max_output_bytes to handle verbose test output
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "sh", "-c", verify_cmd },
        .cwd = wt_path,
        .max_output_bytes = 50 * 1024 * 1024, // 50MB should be enough for any test output
    }) catch |err| {
        std.debug.print("    \x1b[31m✗\x1b[0m Verification failed to run: {any}\n", .{err});
        return false;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited == 0) {
        std.debug.print("    \x1b[32m✓\x1b[0m Verification passed\n", .{});
        return true;
    } else {
        std.debug.print("    \x1b[31m✗\x1b[0m Verification failed (exit code: {d})\n", .{result.term.Exited});
        if (result.stderr.len > 0) {
            // Only show last 2000 bytes of stderr to avoid flooding
            const start = if (result.stderr.len > 2000) result.stderr.len - 2000 else 0;
            std.debug.print("{s}\n", .{result.stderr[start..]});
        }
        return false;
    }
}

/// Show future diffs for conflicted files to help with conflict resolution
fn showFutureDiffs(allocator: std.mem.Allocator, wt_path: []const u8, plan: types.Plan, current_branch_idx: usize) void {
    // Get list of conflicted files
    const status_result = process.runGitWithStatus(allocator, &.{
        "-C",
        wt_path,
        "diff",
        "--name-only",
        "--diff-filter=U",
    }) catch return;
    defer allocator.free(status_result.stdout);
    defer allocator.free(status_result.stderr);

    if (status_result.exit_code != 0 or status_result.stdout.len == 0) return;

    const conflicted_files = strings.trim(status_result.stdout);
    if (conflicted_files.len == 0) return;

    std.debug.print("\n\x1b[36m════════════════════════════════════════════════════════════════\x1b[0m\n", .{});
    std.debug.print("\x1b[36m FUTURE CHANGES - How these files will be modified in later commits\x1b[0m\n", .{});
    std.debug.print("\x1b[36m════════════════════════════════════════════════════════════════\x1b[0m\n", .{});

    var file_iter = std.mem.splitScalar(u8, conflicted_files, '\n');
    while (file_iter.next()) |file_path| {
        if (file_path.len == 0) continue;

        var found_future_changes = false;

        // Look through remaining branches for changes to this file
        for (plan.stack.branches[current_branch_idx + 1 ..], current_branch_idx + 1..) |future_branch, future_idx| {
            _ = future_idx;

            // Check commits in this branch for changes to the file
            if (future_branch.parent_branch) |parent| {
                const range = std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, future_branch.name }) catch continue;
                defer allocator.free(range);

                // Get diff for this file in this branch's commits
                const diff_result = process.runGitWithStatus(allocator, &.{
                    "diff",
                    range,
                    "--",
                    file_path,
                }) catch continue;
                defer allocator.free(diff_result.stdout);
                defer allocator.free(diff_result.stderr);

                if (diff_result.exit_code == 0 and diff_result.stdout.len > 0) {
                    if (!found_future_changes) {
                        std.debug.print("\n\x1b[33m┌─ {s}\x1b[0m\n", .{file_path});
                        found_future_changes = true;
                    }
                    std.debug.print("\x1b[33m│\x1b[0m In \x1b[1m{s}\x1b[0m:\n", .{future_branch.name});

                    // Print the diff with indentation
                    var line_iter = std.mem.splitScalar(u8, strings.trim(diff_result.stdout), '\n');
                    while (line_iter.next()) |line| {
                        if (line.len > 0) {
                            if (line[0] == '+' and (line.len < 2 or line[1] != '+')) {
                                std.debug.print("\x1b[33m│\x1b[0m   \x1b[32m{s}\x1b[0m\n", .{line});
                            } else if (line[0] == '-' and (line.len < 2 or line[1] != '-')) {
                                std.debug.print("\x1b[33m│\x1b[0m   \x1b[31m{s}\x1b[0m\n", .{line});
                            } else if (line[0] == '@') {
                                std.debug.print("\x1b[33m│\x1b[0m   \x1b[36m{s}\x1b[0m\n", .{line});
                            } else {
                                std.debug.print("\x1b[33m│\x1b[0m   {s}\n", .{line});
                            }
                        }
                    }
                    std.debug.print("\x1b[33m│\x1b[0m\n", .{});
                }
            }

            // Also check if this branch has a fix that modifies this file
            if (future_branch.fix) |fix| {
                for (fix.files) |fix_file| {
                    if (std.mem.eql(u8, fix_file.path, file_path)) {
                        if (!found_future_changes) {
                            std.debug.print("\n\x1b[33m┌─ {s}\x1b[0m\n", .{file_path});
                            found_future_changes = true;
                        }
                        std.debug.print("\x1b[33m│\x1b[0m \x1b[1mUncommitted fix in {s}\x1b[0m:\n", .{future_branch.name});

                        var line_iter = std.mem.splitScalar(u8, strings.trim(fix_file.diff), '\n');
                        while (line_iter.next()) |line| {
                            if (line.len > 0) {
                                if (line[0] == '+' and (line.len < 2 or line[1] != '+')) {
                                    std.debug.print("\x1b[33m│\x1b[0m   \x1b[32m{s}\x1b[0m\n", .{line});
                                } else if (line[0] == '-' and (line.len < 2 or line[1] != '-')) {
                                    std.debug.print("\x1b[33m│\x1b[0m   \x1b[31m{s}\x1b[0m\n", .{line});
                                } else if (line[0] == '@') {
                                    std.debug.print("\x1b[33m│\x1b[0m   \x1b[36m{s}\x1b[0m\n", .{line});
                                } else {
                                    std.debug.print("\x1b[33m│\x1b[0m   {s}\n", .{line});
                                }
                            }
                        }
                        std.debug.print("\x1b[33m│\x1b[0m\n", .{});
                    }
                }
            }
        }

        if (found_future_changes) {
            std.debug.print("\x1b[33m└─────────────────────────────────────────────────────────────────\x1b[0m\n", .{});
        }
    }

    std.debug.print("\n", .{});
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-jenga exec [plan.yml] [OPTIONS]
        \\
        \\Executes a restacking plan.
        \\Refuses to run if plan contains non-empty 'errors' block.
        \\
        \\If the plan contains a verify_cmd, it will be run after each commit
        \\(both cherry-picks and fix commits). Use 'git-jenga plan --verify <cmd>'
        \\to set the verification command when generating the plan.
        \\
        \\Arguments:
        \\  plan.yml                 Plan file (default: .git/git-jenga/plan.yml)
        \\
        \\Options:
        \\  --worktree-path <path>   Where to create worktree (default: ../<repo>-jenga)
        \\  --continue               Continue after resolving conflicts
        \\  --abort                  Abort execution and clean up
        \\  -f, --force              Force remove existing worktree
        \\  -h, --help               Show this help message
        \\
        \\Exit Codes:
        \\  0 - Success
        \\  1 - General error
        \\  2 - Conflict during cherry-pick (resume with --continue)
        \\  3 - Validation error (plan has errors)
        \\  4 - Verification command failed
        \\
    , .{});
}
