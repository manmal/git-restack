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

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--worktree-path")) {
            if (i + 1 < args.len) {
                i += 1;
                worktree_path = args[i];
            }
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            plan_file = arg;
        }
    }

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
        std.debug.print("\x1b[31mError:\x1b[0m Plan has {d} unresolved errors.\n", .{plan.errors.len});
        std.process.exit(3);
    }

    // Determine worktree path
    const cwd = try process.getCwd(allocator);
    defer allocator.free(cwd);
    const repo_name = std.fs.path.basename(cwd);
    const default_worktree = try std.fmt.allocPrint(allocator, "../{s}-jenga", .{repo_name});
    defer allocator.free(default_worktree);
    const wt_path = worktree_path orelse default_worktree;

    // Auto-detect: state exists = continue, no state = start fresh
    if (stateExists()) {
        try continueStep(allocator, wt_path, plan);
    } else {
        try startFresh(allocator, wt_path, plan, actual_plan_file);
    }
}

fn startFresh(allocator: std.mem.Allocator, wt_path: []const u8, plan: types.Plan, plan_file: []const u8) !void {
    // Check if worktree path exists
    const wt_exists = blk: {
        std.fs.cwd().access(wt_path, .{}) catch break :blk false;
        break :blk true;
    };

    if (wt_exists) {
        std.debug.print("Error: Worktree path '{s}' already exists but no state file found.\n", .{wt_path});
        std.debug.print("Run 'git-jenga cleanup' first, then try again.\n", .{});
        std.process.exit(1);
    }

    // Create worktree
    std.debug.print("Creating worktree at: {s}\n", .{wt_path});
    const wt_result = process.runGit(allocator, &.{ "worktree", "add", wt_path, plan.stack.base_branch }) catch |err| {
        std.debug.print("Error: Could not create worktree: {any}\n", .{err});
        std.process.exit(1);
    };
    allocator.free(wt_result);

    // Initialize state at step 0, commit 0
    const timestamp = try strings.formatTimestamp(allocator);
    const state = types.ExecutionState{
        .plan_file = plan_file,
        .plan_hash = "sha256:TODO",
        .worktree_path = wt_path,
        .current_step_index = 0,
        .current_commit_index = 0,
        .started_at = timestamp,
        .last_updated = timestamp,
        .status = .in_progress,
        .completed_branches = &[_][]const u8{},
        .verify_cmd = plan.verify_cmd,
        .mode = .step,
    };

    try saveState(allocator, state);

    std.debug.print("\nInitialized execution state. Starting step 1/{d}\n", .{plan.stack.branches.len});

    // Execute first step
    try executeStep(allocator, wt_path, plan, state);
}

fn continueStep(allocator: std.mem.Allocator, wt_path: []const u8, plan: types.Plan) !void {
    // Load state
    const state_content = std.fs.cwd().readFileAlloc(allocator, STATE_FILE, 1024 * 1024) catch {
        std.debug.print("Error: Could not read state file.\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(state_content);

    const state = parser.parseState(allocator, state_content) catch {
        std.debug.print("Error: Could not parse state file.\n", .{});
        std.process.exit(1);
    };

    // Check if there's a cherry-pick in progress that needs to be finished
    const cherry_pick_in_progress = blk: {
        const cp_head_path = try std.fmt.allocPrint(allocator, "{s}/.git/CHERRY_PICK_HEAD", .{wt_path});
        defer allocator.free(cp_head_path);
        std.fs.cwd().access(cp_head_path, .{}) catch break :blk false;
        break :blk true;
    };

    if (cherry_pick_in_progress) {
        std.debug.print("Error: Cherry-pick still in progress in worktree.\n", .{});
        std.debug.print("Resolve conflicts, then: cd {s} && git add . && git cherry-pick --continue && cd -\n", .{wt_path});
        std.debug.print("Then run: git-jenga step\n", .{});
        std.process.exit(2);
    }

    // Check if we're done
    if (state.current_step_index >= plan.stack.branches.len) {
        std.debug.print("\n\x1b[32mAll steps complete!\x1b[0m\n", .{});
        std.debug.print("Run: git-jenga apply\n", .{});
        cleanupState();
        return;
    }

    try executeStep(allocator, wt_path, plan, state);
}

fn executeStep(allocator: std.mem.Allocator, wt_path: []const u8, plan: types.Plan, initial_state: types.ExecutionState) !void {
    var state = initial_state;
    const branch_idx = state.current_step_index;
    const branch = plan.stack.branches[branch_idx];

    std.debug.print("\n[{d}/{d}] Processing: {s}\n", .{ branch_idx + 1, plan.stack.branches.len, branch.name });

    const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});
    defer allocator.free(fix_branch_name);

    // Check if this -fix branch already exists
    const branch_exists = blk: {
        const result = process.runGitInDir(allocator, wt_path, &.{ "rev-parse", "--verify", fix_branch_name }) catch break :blk false;
        allocator.free(result);
        break :blk true;
    };

    if (!branch_exists) {
        // Create the fix branch
        std.debug.print("  Creating branch: {s}\n", .{fix_branch_name});

        // First, checkout the parent -fix branch (or base branch for first)
        const parent_fix_allocated = if (branch.parent_branch) |parent| blk: {
            // If parent is the base branch, use it directly; otherwise append -fix
            if (std.mem.eql(u8, parent, plan.stack.base_branch)) {
                break :blk false;
            } else {
                break :blk true;
            }
        } else false;
        const parent_fix = if (branch.parent_branch) |parent| blk: {
            if (std.mem.eql(u8, parent, plan.stack.base_branch)) {
                break :blk plan.stack.base_branch;
            } else {
                break :blk try std.fmt.allocPrint(allocator, "{s}-fix", .{parent});
            }
        } else plan.stack.base_branch;
        defer if (parent_fix_allocated) allocator.free(parent_fix);

        const checkout_result = process.runGitInDir(allocator, wt_path, &.{ "checkout", parent_fix }) catch |err| {
            std.debug.print("Error: Could not checkout parent branch {s}: {any}\n", .{ parent_fix, err });
            state.status = .failed;
            try saveState(allocator, state);
            std.process.exit(1);
        };
        allocator.free(checkout_result);

        const create_result = process.runGitInDir(allocator, wt_path, &.{ "checkout", "-b", fix_branch_name }) catch |err| {
            std.debug.print("Error: Could not create branch: {any}\n", .{err});
            state.status = .failed;
            try saveState(allocator, state);
            std.process.exit(1);
        };
        allocator.free(create_result);

        // Reset commit index for new branch
        state.current_commit_index = 0;
    } else {
        // Branch exists, checkout it
        const co_result = process.runGitInDir(allocator, wt_path, &.{ "checkout", fix_branch_name }) catch null;
        if (co_result) |r| allocator.free(r);
    }

    // Get commits to cherry-pick
    if (branch.parent_branch) |parent| {
        const range = try std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, branch.name });
        defer allocator.free(range);

        const commits_output = process.runGit(allocator, &.{ "rev-list", "--reverse", range }) catch |err| {
            std.debug.print("Error: Could not list commits: {any}\n", .{err});
            state.status = .failed;
            try saveState(allocator, state);
            std.process.exit(1);
        };
        defer allocator.free(commits_output);

        var commits: std.ArrayListUnmanaged([]const u8) = .{};
        defer {
            for (commits.items) |c| allocator.free(c);
            commits.deinit(allocator);
        }

        var commits_iter = std.mem.splitScalar(u8, strings.trim(commits_output), '\n');
        while (commits_iter.next()) |commit_sha| {
            if (commit_sha.len > 0) {
                try commits.append(allocator, try allocator.dupe(u8, commit_sha));
            }
        }

        const total_commits = commits.items.len;
        var commit_idx = state.current_commit_index;

        // Cherry-pick one commit at a time
        while (commit_idx < total_commits) {
            const commit_sha = commits.items[commit_idx];
            std.debug.print("  Cherry-picking commit {d}/{d}: {s}\n", .{ commit_idx + 1, total_commits, commit_sha[0..@min(7, commit_sha.len)] });

            const cherry_result = process.runGitWithStatus(allocator, &.{
                "-C",
                wt_path,
                "cherry-pick",
                commit_sha,
            }) catch |err| {
                std.debug.print("Error: Cherry-pick command failed: {any}\n", .{err});
                state.status = .conflict;
                state.current_commit_index = commit_idx;
                try saveState(allocator, state);
                std.process.exit(2);
            };
            defer allocator.free(cherry_result.stdout);
            defer allocator.free(cherry_result.stderr);

            if (cherry_result.exit_code != 0) {
                std.debug.print("\n\x1b[33mConflict during cherry-pick.\x1b[0m\n", .{});
                std.debug.print("Resolve in: {s}\n", .{wt_path});
                std.debug.print("Then: cd {s} && git add . && git cherry-pick --continue && cd - && git-jenga step\n", .{wt_path});

                state.status = .conflict;
                state.current_commit_index = commit_idx;
                try saveState(allocator, state);
                std.process.exit(2);
            }

            commit_idx += 1;
            state.current_commit_index = commit_idx;
            try saveState(allocator, state);
        }

        std.debug.print("  Cherry-picked {d} commits\n", .{total_commits});
    }

    // Apply fixes if needed
    if (branch.needs_fix) {
        if (branch.fix) |fix| {
            std.debug.print("  Applying {d} file fixes\n", .{fix.files.len});

            for (fix.files) |file| {
                const tmp_path = try std.fmt.allocPrint(allocator, "{s}/.git-jenga-patch", .{wt_path});
                defer allocator.free(tmp_path);

                const tmp_file = try std.fs.cwd().createFile(tmp_path, .{});
                try tmp_file.writeAll(file.diff);
                tmp_file.close();

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
                    std.debug.print("    Warning: Patch failed for {s}: {s}\n", .{ file.path, apply_result.stderr });
                } else {
                    std.debug.print("    Applied: {s}\n", .{file.path});
                }

                std.fs.cwd().deleteFile(tmp_path) catch {};
            }

            // Stage and commit
            const add_result = process.runGitInDir(allocator, wt_path, &.{ "add", "-A" }) catch null;
            if (add_result) |r| allocator.free(r);

            const commit_result = process.runGitWithStatus(allocator, &.{
                "-C",
                wt_path,
                "commit",
                "-m",
                fix.commit_message,
                "--allow-empty",
            }) catch |err| {
                std.debug.print("    Warning: Commit failed: {any}\n", .{err});
                return;
            };
            defer allocator.free(commit_result.stdout);
            defer allocator.free(commit_result.stderr);

            if (commit_result.exit_code == 0) {
                std.debug.print("  Committed fix\n", .{});
            } else {
                std.debug.print("  Fix commit failed: {s}\n", .{commit_result.stderr});
            }
        }
    }

    // Run verification if configured
    if (plan.verify_cmd) |cmd| {
        std.debug.print("  Running verification: {s}\n", .{cmd});
        const result = std.process.Child.run(.{
            .allocator = allocator,
            .argv = &.{ "sh", "-c", cmd },
            .cwd = wt_path,
            .max_output_bytes = 50 * 1024 * 1024,
        }) catch |err| {
            std.debug.print("    \x1b[31m✗\x1b[0m Verification failed: {any}\n", .{err});
            state.status = .conflict;
            try saveState(allocator, state);
            std.process.exit(4);
        };
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        if (result.term.Exited != 0) {
            std.debug.print("    \x1b[31m✗\x1b[0m Verification failed (exit {d})\n", .{result.term.Exited});
            state.status = .conflict;
            try saveState(allocator, state);
            std.process.exit(4);
        }
        std.debug.print("    \x1b[32m✓\x1b[0m Verification passed\n", .{});
    }

    std.debug.print("  \x1b[32m✓\x1b[0m {s} complete\n", .{fix_branch_name});

    // Move to next step
    state.current_step_index += 1;
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
}

fn stateExists() bool {
    std.fs.cwd().access(STATE_FILE, .{}) catch return false;
    return true;
}

fn saveState(allocator: std.mem.Allocator, state: types.ExecutionState) !void {
    std.fs.cwd().makePath(STATE_DIR) catch {};

    const json = try emitter.emitState(allocator, state);
    defer allocator.free(json);

    const file = try std.fs.cwd().createFile(STATE_FILE, .{});
    defer file.close();
    try file.writeAll(json);
}

fn cleanupState() void {
    std.fs.cwd().deleteFile(STATE_FILE) catch {};
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-jenga step [plan.yml] [OPTIONS]
        \\
        \\Executes ONE step of the restacking plan.
        \\Auto-detects whether to start fresh or continue from state.
        \\Run repeatedly until all branches are processed.
        \\
        \\Arguments:
        \\  plan.yml                 Plan file (default: .git/git-jenga/plan.yml)
        \\
        \\Options:
        \\  --worktree-path <path>   Where to create worktree (default: ../<repo>-jenga)
        \\  -h, --help               Show this help message
        \\
        \\Exit Codes:
        \\  0 - Step completed (or all done)
        \\  1 - General error
        \\  2 - Conflict during cherry-pick (resolve, then run step again)
        \\  3 - Validation error
        \\  4 - Verification failed
        \\
        \\Workflow:
        \\  git-jenga plan                    # Generate plan
        \\  git-jenga step                    # Start + first step
        \\  git-jenga step                    # Next step (auto-continues)
        \\  ... repeat until done ...
        \\  git-jenga apply                   # Apply to original branches
        \\
    , .{});
}
