const std = @import("std");
const types = @import("../types.zig");
const parser = @import("../yaml/parser.zig");
const emitter = @import("../yaml/emitter.zig");
const strings = @import("../utils/strings.zig");
const process = @import("../utils/process.zig");

const STATE_DIR = ".git/git-jenga";
const STATE_FILE = ".git/git-jenga/state.json";

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

    // Normal execution - need a plan file
    if (plan_file == null) {
        std.debug.print("Error: No plan file specified.\n", .{});
        std.debug.print("Usage: git-jenga exec <plan.yml>\n", .{});
        std.process.exit(1);
    }

    // Read and parse plan
    const plan_content = std.fs.cwd().readFileAlloc(allocator, plan_file.?, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error: Could not read plan file '{s}': {any}\n", .{ plan_file.?, err });
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

        std.debug.print("Fix these in {s} first, then run: git-jenga exec {s}\n", .{ plan_file.?, plan_file.? });
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

    if (branches_to_fix == 0) {
        std.debug.print("No branches need fixing. Nothing to do.\n", .{});
        return;
    }

    std.debug.print("\nExecuting plan: {d} branches to restack, {d} need fixes\n\n", .{
        plan.stack.branches.len,
        branches_to_fix,
    });

    // Create worktree
    std.debug.print("Creating worktree at: {s}\n", .{wt_path});
    _ = process.runGit(allocator, &.{ "worktree", "add", wt_path, plan.stack.base_branch }) catch |err| {
        std.debug.print("Error: Could not create worktree: {any}\n", .{err});
        std.process.exit(1);
    };

    // Initialize state
    const timestamp = try strings.formatTimestamp(allocator);
    var state = types.ExecutionState{
        .plan_file = plan_file.?,
        .plan_hash = "sha256:TODO",
        .worktree_path = wt_path,
        .current_step_index = 0,
        .started_at = timestamp,
        .last_updated = timestamp,
        .status = .in_progress,
        .completed_branches = &[_][]const u8{},
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

        // Cherry-pick commits from parent to this branch
        if (branch.parent_branch) |parent| {
            const range = try std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, branch.name });
            std.debug.print("  Cherry-picking: {s}\n", .{range});

            const cherry_result = process.runGitWithStatus(allocator, &.{
                "-C",
                wt_path,
                "cherry-pick",
                range,
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
                std.debug.print("Then run:\n", .{});
                std.debug.print("  cd {s}\n", .{wt_path});
                std.debug.print("  git add <resolved_files>\n", .{});
                std.debug.print("  git cherry-pick --continue\n", .{});
                std.debug.print("  cd -\n", .{});
                std.debug.print("  git-jenga exec {s} --continue\n\n", .{plan_file.?});
                std.debug.print("Or abort:\n", .{});
                std.debug.print("  git-jenga exec {s} --abort\n", .{plan_file.?});

                state.status = .conflict;
                try saveState(allocator, state);
                std.process.exit(2);
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

        try completed.append(allocator, fix_branch_name);
        std.debug.print("  \x1b[32m✓\x1b[0m {s} complete\n", .{fix_branch_name});
    }

    // Complete
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
    _ = allocator;
    if (!stateExists()) {
        std.debug.print("Error: No execution in progress.\n", .{});
        std.process.exit(1);
    }

    // TODO: Implement proper continue logic
    std.debug.print("Continue not fully implemented yet.\n", .{});
    std.debug.print("Please complete the cherry-pick manually in the worktree, then re-run exec.\n", .{});
    std.process.exit(1);
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

fn printHelp() void {
    std.debug.print(
        \\Usage: git-jenga exec <plan.yml> [OPTIONS]
        \\
        \\Executes a restacking plan.
        \\Refuses to run if plan.yml contains non-empty 'errors' block.
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
        \\
    , .{});
}
