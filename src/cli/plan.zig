const std = @import("std");
const types = @import("../types.zig");
const stack_mod = @import("../git/stack.zig");
const diff_mod = @import("../git/diff.zig");
const emitter = @import("../yaml/emitter.zig");
const strings = @import("../utils/strings.zig");
const process = @import("../utils/process.zig");

const STATE_DIR = ".git/git-restack";
const DEFAULT_PLAN_FILE = ".git/git-restack/plan.yml";
const DEFAULT_PLAN_WORKTREE_SUFFIX = "-restack-plan";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var output_file: []const u8 = DEFAULT_PLAN_FILE;
    var verify_cmd: ?[]const u8 = null;
    var staged_only = false;
    var unstaged_only = false;
    var verify_only = false;
    var plan_mode: types.PlanMode = .worktree;
    var worktree_path: ?[]const u8 = null;
    var mergetool: ?[]const u8 = null;
    var force = false;
    var base_branch_override: ?[]const u8 = null;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            if (i + 1 < args.len) {
                i += 1;
                output_file = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--verify")) {
            if (i + 1 < args.len) {
                i += 1;
                verify_cmd = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--verify-only")) {
            if (i + 1 < args.len) {
                i += 1;
                verify_cmd = args[i];
                verify_only = true;
            }
        } else if (std.mem.eql(u8, arg, "--staged-only")) {
            staged_only = true;
        } else if (std.mem.eql(u8, arg, "--unstaged-only")) {
            unstaged_only = true;
        } else if (std.mem.eql(u8, arg, "--mode")) {
            if (i + 1 < args.len) {
                i += 1;
                plan_mode = parsePlanMode(args[i]);
            }
        } else if (std.mem.eql(u8, arg, "--direct")) {
            plan_mode = .direct;
        } else if (std.mem.eql(u8, arg, "--worktree-path")) {
            if (i + 1 < args.len) {
                i += 1;
                worktree_path = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--base") or std.mem.eql(u8, arg, "--onto")) {
            if (i + 1 < args.len) {
                i += 1;
                base_branch_override = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--mergetool")) {
            if (i + 1 < args.len) {
                i += 1;
                mergetool = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
    }

    // 1. Analyze stack
    std.debug.print("Analyzing branch stack...\n", .{});
    var stack = stack_mod.analyzeStack(allocator, base_branch_override) catch |err| {
        switch (err) {
            types.RestackError.GitCommandFailed => {
                std.debug.print("Error: Git command failed. Are you in a git repository?\n", .{});
            },
            types.RestackError.BaseBranchNotFound => {
                if (base_branch_override) |base| {
                    std.debug.print("Error: Base branch '{s}' not found.\n", .{base});
                } else {
                    std.debug.print("Error: Could not find 'develop' or 'main' base branch.\n", .{});
                }
            },
            else => {
                std.debug.print("Error: {any}\n", .{err});
            },
        }
        std.process.exit(1);
    };
    defer stack.deinit(allocator);

    std.debug.print("Found {d} branches in stack\n", .{stack.branches.len});

    // 2. Collect changed files (skip if verify-only mode)
    var all_files: std.ArrayListUnmanaged(diff_mod.ChangedFile) = .{};
    defer {
        for (all_files.items) |*f| f.deinit(allocator);
        all_files.deinit(allocator);
    }

    if (verify_only) {
        std.debug.print("Verify-only mode: skipping change collection\n", .{});
    } else {
        std.debug.print("Collecting changed files...\n", .{});

        if (!unstaged_only) {
            const staged = try diff_mod.getStagedFiles(allocator);
            defer allocator.free(staged);
            for (staged) |file| {
                try all_files.append(allocator, file);
            }
        }

        if (!staged_only) {
            const unstaged = try diff_mod.getUnstagedFiles(allocator);
            defer allocator.free(unstaged);
            for (unstaged) |file| {
                // Check for duplicates (file might be both staged and unstaged)
                var exists = false;
                for (all_files.items) |existing| {
                    if (std.mem.eql(u8, existing.path, file.path)) {
                        exists = true;
                        break;
                    }
                }
                if (!exists) {
                    try all_files.append(allocator, file);
                } else {
                    // Free the duplicate path since we're not using it
                    allocator.free(file.path);
                }
            }
        }

        std.debug.print("Found {d} changed files\n", .{all_files.items.len});
    }

    // 3. Map files to branches
    var errors: std.ArrayListUnmanaged(types.PlanError) = .{};
    defer errors.deinit(allocator);

    // Create a mutable copy of branches with fix info
    var branches_with_fixes: std.ArrayListUnmanaged(types.StackBranch) = .{};
    defer branches_with_fixes.deinit(allocator);

    for (stack.branches) |branch| {
        try branches_with_fixes.append(allocator, .{
            .name = try strings.copy(allocator, branch.name),
            .commit_sha = try strings.copy(allocator, branch.commit_sha),
            .parent_branch = if (branch.parent_branch) |pb| try strings.copy(allocator, pb) else null,
            .commits_from_parent = branch.commits_from_parent,
            .needs_fix = false,
            .fix = null,
            .step_status = .pending,
        });
    }

    // Map each file to a branch
    for (all_files.items) |file| {
        std.debug.print("  Mapping: {s}\n", .{file.path});

        // Find the last commit that touched this file
        const last_commit = try diff_mod.getLastCommitForFile(allocator, file.path);
        defer if (last_commit) |lc| allocator.free(lc);

        if (last_commit == null) {
            // New file - unmapped
            try errors.append(allocator, .{
                .error_type = .unmapped_file,
                .path = try strings.copy(allocator, file.path),
                .message = try strings.copy(allocator, "New file detected. Assign to a branch fix block manually."),
            });
            continue;
        }

        // Check if the commit is in our ancestry
        const in_ancestry = try diff_mod.isCommitInAncestry(allocator, last_commit.?, stack.base_commit);
        if (!in_ancestry) {
            // Check if it's the base commit itself
            if (!std.mem.eql(u8, last_commit.?, stack.base_commit)) {
                try errors.append(allocator, .{
                    .error_type = .outside_ancestry,
                    .path = try strings.copy(allocator, file.path),
                    .message = try strings.copy(allocator, "File modified from commit outside stack ancestry."),
                });
                continue;
            }
        }

        // Find which branch the commit belongs to
        const target_branch = try diff_mod.findBranchForCommit(allocator, last_commit.?, stack);
        defer if (target_branch) |tb| allocator.free(tb);

        if (target_branch == null) {
            // Couldn't map to a specific branch
            try errors.append(allocator, .{
                .error_type = .ambiguous_branch,
                .path = try strings.copy(allocator, file.path),
                .message = try strings.copy(allocator, "Could not determine target branch for this file."),
            });
            continue;
        }

        // Get the diff for this file
        const diff_content = try diff_mod.getFileDiff(allocator, file.path, file.staged);

        // Find the branch and add the file to its fix
        for (branches_with_fixes.items, 0..) |*branch, idx| {
            if (std.mem.eql(u8, branch.name, target_branch.?)) {
                branch.needs_fix = true;

                // Initialize fix if needed
                if (branch.fix == null) {
                    const jira_prefix = strings.extractJiraPrefix(branch.name) orelse "FIX";
                    const commit_msg = try std.fmt.allocPrint(allocator, "fix: Apply changes\n\nJira: {s}", .{jira_prefix});

                    branch.fix = types.Fix{
                        .commit_message = commit_msg,
                        .files = &[_]types.FileChange{},
                    };
                }

                // Add file to fix
                var files_list: std.ArrayListUnmanaged(types.FileChange) = .{};
                if (branch.fix) |fix| {
                    for (fix.files) |f| {
                        try files_list.append(allocator, f);
                    }
                    // Free the old array (but not the items, we just moved them)
                    allocator.free(fix.files);
                }
                try files_list.append(allocator, .{
                    .path = try strings.copy(allocator, file.path),
                    .change_type = file.change_type,
                    .diff = diff_content,
                    .staged = file.staged,
                });

                branches_with_fixes.items[idx].fix = types.Fix{
                    .commit_message = branch.fix.?.commit_message,
                    .files = try files_list.toOwnedSlice(allocator),
                };

                break;
            }
        }
    }

    // 4. Detect conflicts and resolve them in simulation
    var simulation: ?types.PlanSimulation = null;
    var conflicts: []types.PlanConflict = try allocator.alloc(types.PlanConflict, 0);
    var owns_conflicts = true;
    defer {
        if (owns_conflicts) {
            for (conflicts) |*conflict| conflict.deinit(allocator);
            allocator.free(conflicts);
            if (simulation) |*sim| sim.deinit(allocator);
        }
    }

    if (stack.branches.len > 0) {
        const outcome = simulateConflicts(
            allocator,
            stack,
            branches_with_fixes.items,
            .{
                .mode = plan_mode,
                .worktree_path = worktree_path,
                .mergetool = mergetool,
                .force = force,
            },
        ) catch |err| {
            switch (err) {
                types.RestackError.WorktreeExists => {
                    std.debug.print("Error: Plan worktree already exists. Use --force or cleanup first.\n", .{});
                },
                types.RestackError.ConflictDetected => {
                    std.debug.print("Error: Conflict resolution aborted.\n", .{});
                },
                else => {
                    std.debug.print("Error: {any}\n", .{err});
                },
            }
            std.process.exit(1);
        };
        simulation = outcome.simulation;
        allocator.free(conflicts);
        conflicts = outcome.conflicts;
    }

    // 5. Generate plan
    const cwd = try process.getCwd(allocator);
    const timestamp = try strings.formatTimestamp(allocator);

    var plan = types.Plan{
        .version = 2,
        .generated = timestamp,
        .repository = cwd,
        .verify_cmd = if (verify_cmd) |cmd| try strings.copy(allocator, cmd) else null,
        .simulation = simulation,
        .errors = try errors.toOwnedSlice(allocator),
        .conflicts = conflicts,
        .stack = .{
            .branches = try branches_with_fixes.toOwnedSlice(allocator),
            .base_branch = try strings.copy(allocator, stack.base_branch),
            .base_commit = try strings.copy(allocator, stack.base_commit),
            .base_tip = try strings.copy(allocator, stack.base_tip),
            .head_branch = try strings.copy(allocator, stack.head_branch),
            .head_commit = try strings.copy(allocator, stack.head_commit),
        },
    };
    defer plan.deinit(allocator);
    owns_conflicts = false;

    // 6. Write YAML output
    const yaml = try emitter.emitPlan(allocator, plan);
    defer allocator.free(yaml);

    // Ensure state directory exists
    std.fs.cwd().makePath(STATE_DIR) catch {};

    const file = try std.fs.cwd().createFile(output_file, .{});
    defer file.close();
    try file.writeAll(yaml);

    std.debug.print("\nPlan written to: {s}\n", .{output_file});

    // 7. Report status
    if (plan.conflicts.len > 0) {
        std.debug.print("\n\x1b[32mResolved:\x1b[0m {d} conflicts captured in plan.\n", .{plan.conflicts.len});
    }
    if (plan.errors.len > 0) {
        std.debug.print("\n\x1b[33mWarning:\x1b[0m Plan has {d} unresolved errors.\n\n", .{plan.errors.len});

        for (plan.errors, 1..) |err, num| {
            std.debug.print("  {d}. [{s}] {s}\n", .{ num, err.error_type.toString(), err.path });
            std.debug.print("     {s}\n\n", .{err.message});
        }

        std.debug.print("Fix these in {s}, then run: git-restack exec {s}\n", .{ output_file, output_file });
        std.process.exit(1);
    } else {
        var fix_count: usize = 0;
        for (plan.stack.branches) |branch| {
            if (branch.needs_fix) fix_count += 1;
        }

        if (fix_count == 0) {
            std.debug.print("\nNo changes to apply. All files already in correct branches.\n", .{});
        } else {
            std.debug.print("\n\x1b[32mSuccess:\x1b[0m {d} branches will receive fixes.\n", .{fix_count});
            std.debug.print("Review the plan and run: git-restack exec {s}\n", .{output_file});
        }
    }
}

const SimulationOptions = struct {
    mode: types.PlanMode,
    worktree_path: ?[]const u8,
    mergetool: ?[]const u8,
    force: bool,
};

const SimulationOutcome = struct {
    simulation: types.PlanSimulation,
    conflicts: []types.PlanConflict,
};

fn simulateConflicts(
    allocator: std.mem.Allocator,
    stack: types.Stack,
    branches: []types.StackBranch,
    options: SimulationOptions,
) !SimulationOutcome {
    var conflicts: std.ArrayListUnmanaged(types.PlanConflict) = .{};
    errdefer {
        for (conflicts.items) |*conflict| conflict.deinit(allocator);
        conflicts.deinit(allocator);
    }

    var simulation = types.PlanSimulation{
        .mode = options.mode,
        .worktree_path = null,
        .plan_branch = null,
        .backup_branches = try allocator.alloc(types.BackupBranch, 0),
    };
    errdefer simulation.deinit(allocator);

    const timestamp = std.time.milliTimestamp();
    const plan_branch = try std.fmt.allocPrint(allocator, "git-restack-plan-{d}", .{timestamp});
    simulation.plan_branch = plan_branch;

    const cwd = try process.getCwd(allocator);
    defer allocator.free(cwd);

    var sim_path: []const u8 = cwd;
    var created_worktree = false;
    var created_plan_branch = false;
    var stashed = false;
    var original_branch: ?[]const u8 = null;

    var backups: std.ArrayListUnmanaged(types.BackupBranch) = .{};
    errdefer {
        for (backups.items) |*backup| backup.deinit(allocator);
        backups.deinit(allocator);
    }

    defer {
        if (original_branch) |branch| allocator.free(branch);
    }
    defer {
        cleanupSimulation(allocator, .{
            .mode = options.mode,
            .sim_path = sim_path,
            .plan_branch = plan_branch,
            .created_worktree = created_worktree,
            .created_plan_branch = created_plan_branch,
            .original_branch = original_branch,
            .stashed = stashed,
            .base_branch = stack.base_branch,
        });
    }

    if (options.mode == .worktree) {
        var default_path: ?[]const u8 = null;
        const wt_path = options.worktree_path orelse blk: {
            default_path = try defaultPlanWorktreePath(allocator, cwd);
            break :blk default_path.?;
        };
        defer if (default_path) |path| allocator.free(path);

        if (worktreeExists(wt_path)) {
            if (!options.force) {
                return types.RestackError.WorktreeExists;
            }
            if (process.runGit(allocator, &.{ "worktree", "remove", wt_path, "--force" })) |output| {
                allocator.free(output);
            } else |_| {}
        }

        const worktree_out = process.runGit(allocator, &.{ "worktree", "add", "--detach", wt_path, stack.base_tip }) catch |err| {
            std.debug.print("Error: Could not create plan worktree: {any}\n", .{err});
            return err;
        };
        allocator.free(worktree_out);
        created_worktree = true;
        simulation.worktree_path = try strings.copy(allocator, wt_path);
        sim_path = simulation.worktree_path.?;
    } else {
        original_branch = try strings.copy(allocator, stack.head_branch);
        const status = process.runGitWithStatus(allocator, &.{ "status", "--porcelain" }) catch null;
        if (status) |result| {
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);
            if (result.stdout.len > 0) {
                if (process.runGit(allocator, &.{ "stash", "push", "-u", "-m", "git-restack-plan" })) |output| {
                    allocator.free(output);
                } else |_| {}
                stashed = true;
            }
        }

        for (branches) |branch| {
            const backup_name = try std.fmt.allocPrint(allocator, "{s}-restack-backup-{d}", .{ branch.name, timestamp });
            const branch_out = process.runGit(allocator, &.{ "branch", backup_name, branch.name }) catch |err| {
                allocator.free(backup_name);
                return err;
            };
            allocator.free(branch_out);
            try backups.append(allocator, .{
                .source = try strings.copy(allocator, branch.name),
                .backup = backup_name,
            });
        }
        allocator.free(simulation.backup_branches);
        simulation.backup_branches = try backups.toOwnedSlice(allocator);
        backups.deinit(allocator);
    }

    // Create plan branch
    if (options.mode == .direct) {
        const checkout_out = process.runGit(allocator, &.{ "checkout", stack.base_branch }) catch |err| {
            std.debug.print("Error: Could not checkout base branch: {any}\n", .{err});
            return err;
        };
        allocator.free(checkout_out);
    }
    const checkout_result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "checkout",
        "-b",
        plan_branch,
    }) catch |err| {
        std.debug.print("Error: Could not create plan branch: {any}\n", .{err});
        return err;
    };
    allocator.free(checkout_result.stdout);
    allocator.free(checkout_result.stderr);
    created_plan_branch = true;

    // Replay each branch in order, resolving conflicts as they appear.
    for (branches) |branch| {
        if (branch.parent_branch) |parent| {
            const range = try std.fmt.allocPrint(allocator, "{s}..{s}", .{ parent, branch.name });
            defer allocator.free(range);

            const commits_output = process.runGit(allocator, &.{
                "-C",
                sim_path,
                "rev-list",
                "--reverse",
                range,
            }) catch |err| {
                std.debug.print("Error: Could not list commits: {any}\n", .{err});
                return err;
            };
            defer allocator.free(commits_output);

            var commits_iter = std.mem.splitScalar(u8, strings.trim(commits_output), '\n');
            while (commits_iter.next()) |commit_sha| {
                if (commit_sha.len == 0) continue;

                const cherry_result = process.runGitWithStatusNoRerere(allocator, &.{
                    "-C",
                    sim_path,
                    "cherry-pick",
                    commit_sha,
                }) catch |err| {
                    std.debug.print("Error: Cherry-pick failed: {any}\n", .{err});
                    return err;
                };
                defer allocator.free(cherry_result.stdout);
                defer allocator.free(cherry_result.stderr);

                if (cherry_result.exit_code != 0) {
                    const subject = try getCommitSubject(allocator, sim_path, commit_sha);
                    defer if (subject) |s| allocator.free(s);
                    const conflict = try resolveConflict(allocator, .{
                        .sim_path = sim_path,
                        .kind = .cherry_pick,
                        .branch = branch.name,
                        .commit = commit_sha,
                        .subject = subject,
                        .mergetool = options.mergetool,
                    });
                    try conflicts.append(allocator, conflict);

                    const continue_result = process.runGitWithStatusNoRerere(allocator, &.{
                        "-C",
                        sim_path,
                        "cherry-pick",
                        "--continue",
                        "--no-edit",
                    }) catch |err| {
                        std.debug.print("Error: Could not continue cherry-pick: {any}\n", .{err});
                        return err;
                    };
                    defer allocator.free(continue_result.stdout);
                    defer allocator.free(continue_result.stderr);

                    if (continue_result.exit_code != 0) {
                        if (isEmptyCherryPickOutput(continue_result.stdout) or isEmptyCherryPickOutput(continue_result.stderr)) {
                            const skip_result = process.runGitWithStatusNoRerere(allocator, &.{
                                "-C",
                                sim_path,
                                "cherry-pick",
                                "--skip",
                            }) catch |err| {
                                std.debug.print("Error: Could not skip empty cherry-pick: {any}\n", .{err});
                                return err;
                            };
                            defer allocator.free(skip_result.stdout);
                            defer allocator.free(skip_result.stderr);
                            if (skip_result.exit_code != 0) {
                                std.debug.print("Error: Cherry-pick --skip failed.\n", .{});
                                return types.RestackError.ConflictDetected;
                            }
                        } else {
                            std.debug.print("Error: Cherry-pick --continue failed.\n", .{});
                            return types.RestackError.ConflictDetected;
                        }
                    }
                }
            }
        }

        if (branch.needs_fix) {
            if (branch.fix) |fix| {
                for (fix.files) |file| {
                    const tmp_path = try std.fmt.allocPrint(allocator, "{s}/.git-restack-patch", .{sim_path});
                    defer allocator.free(tmp_path);

                    const tmp_file = try std.fs.cwd().createFile(tmp_path, .{});
                    try tmp_file.writeAll(file.diff);
                    tmp_file.close();

                    const apply_result = process.runGitWithStatusNoRerere(allocator, &.{
                        "-C",
                        sim_path,
                        "apply",
                        "--3way",
                        "--allow-empty",
                        tmp_path,
                    }) catch |err| {
                        std.debug.print("Error: Patch apply failed: {any}\n", .{err});
                        return err;
                    };
                    defer allocator.free(apply_result.stdout);
                    defer allocator.free(apply_result.stderr);

                    if (apply_result.exit_code != 0) {
                        const conflict = try resolveConflict(allocator, .{
                            .sim_path = sim_path,
                            .kind = .fix_apply,
                            .branch = branch.name,
                            .commit = null,
                            .subject = null,
                            .mergetool = options.mergetool,
                        });
                        try conflicts.append(allocator, conflict);
                    }

                    std.fs.cwd().deleteFile(tmp_path) catch {};
                }

                _ = process.runGitInDir(allocator, sim_path, &.{ "add", "-A" }) catch {};
                if (process.runGitWithStatus(allocator, &.{
                    "-C",
                    sim_path,
                    "commit",
                    "-m",
                    fix.commit_message,
                    "--allow-empty",
                })) |commit_result| {
                    allocator.free(commit_result.stdout);
                    allocator.free(commit_result.stderr);
                } else |_| {}
            }
        }
    }

    return SimulationOutcome{
        .simulation = simulation,
        .conflicts = try conflicts.toOwnedSlice(allocator),
    };
}

const ConflictContext = struct {
    sim_path: []const u8,
    kind: types.ConflictKind,
    branch: []const u8,
    commit: ?[]const u8,
    subject: ?[]const u8,
    mergetool: ?[]const u8,
};

fn resolveConflict(allocator: std.mem.Allocator, ctx: ConflictContext) !types.PlanConflict {
    std.debug.print("\n\x1b[33mConflict detected during {s}.\x1b[0m\n", .{ctx.kind.toString()});
    std.debug.print("Resolve in: {s}\n", .{ctx.sim_path});
    if (ctx.commit) |commit| {
        std.debug.print("Commit: {s}\n", .{commit[0..@min(7, commit.len)]});
    }
    std.debug.print("Branch: {s}\n\n", .{ctx.branch});

    var conflict_files: std.ArrayListUnmanaged(types.ConflictFile) = .{};
    errdefer {
        for (conflict_files.items) |*file| file.deinit(allocator);
        conflict_files.deinit(allocator);
    }

    const conflict_paths = try diff_mod.getConflictedFiles(allocator, ctx.sim_path);
    defer {
        for (conflict_paths) |path| allocator.free(path);
        allocator.free(conflict_paths);
    }

    if (conflict_paths.len == 0) {
        return types.RestackError.ConflictDetected;
    }

    for (conflict_paths) |path| {
        const conflict_diff = try getConflictDiff(allocator, ctx.sim_path, path);
        const prefer_gitlink = isGitlinkConflict(allocator, ctx.sim_path, path);
        const resolution = types.ConflictResolution{
            .present = true,
            .encoding = if (prefer_gitlink) .gitlink else .text,
            .content = try strings.copy(allocator, ""),
        };
        try conflict_files.append(allocator, .{
            .path = try strings.copy(allocator, path),
            .conflict_diff = conflict_diff,
            .resolution = resolution,
        });
    }

    _ = runMergetool(allocator, ctx.sim_path, ctx.mergetool);

    var attempted_auto = false;
    while (true) {
        const remaining = try diff_mod.getConflictedFiles(allocator, ctx.sim_path);
        defer {
            for (remaining) |path| allocator.free(path);
            allocator.free(remaining);
        }
        if (remaining.len == 0) break;

        if (!attempted_auto and try autoResolveRemaining(allocator, ctx.sim_path, remaining, ctx.mergetool)) {
            attempted_auto = true;
            continue;
        }

        std.debug.print("Unresolved files:\n", .{});
        for (remaining) |path| {
            std.debug.print("  - {s}\n", .{path});
        }
        std.debug.print("\nResolve conflicts, then press Enter to continue (or type 'abort').\n", .{});

        const input = try readLine(allocator);
        defer allocator.free(input);
        if (input.len == 0 and !std.fs.File.stdin().isTty()) {
            return types.RestackError.ConflictDetected;
        }
        if (std.mem.eql(u8, strings.trim(input), "abort")) {
            return types.RestackError.ConflictDetected;
        }
    }

    // Capture the resolved content from the simulation worktree for deterministic replay.
    for (conflict_files.items) |*file| {
        const prefer_gitlink = file.resolution.encoding == .gitlink;
        file.resolution.deinit(allocator);
        file.resolution = try captureResolution(allocator, ctx.sim_path, file.path, prefer_gitlink);
    }

    _ = process.runGitInDir(allocator, ctx.sim_path, &.{ "add", "-A" }) catch {};

    return types.PlanConflict{
        .kind = ctx.kind,
        .branch = try strings.copy(allocator, ctx.branch),
        .commit = if (ctx.commit) |commit| try strings.copy(allocator, commit) else null,
        .subject = if (ctx.subject) |subject| try strings.copy(allocator, subject) else null,
        .files = try conflict_files.toOwnedSlice(allocator),
    };
}

fn autoResolveRemaining(
    allocator: std.mem.Allocator,
    sim_path: []const u8,
    remaining: []const []const u8,
    mergetool: ?[]const u8,
) !bool {
    const side = detectMergetoolSide(mergetool) orelse return false;

    var handled = try resolveGitlinkConflicts(allocator, sim_path, remaining, side);
    handled = (try resolveDistinctTypeConflicts(allocator, sim_path, remaining, side)) or handled;

    for (remaining) |path| {
        if (conflictBackupBase(path) != null) {
            handled = true;
            continue;
        }
        if ((try getGitlinkStageHash(allocator, sim_path, path, if (std.mem.eql(u8, side, "theirs")) "3" else "2")) != null) {
            handled = true;
            continue;
        }

        var args: std.ArrayListUnmanaged([]const u8) = .{};
        defer args.deinit(allocator);

        try args.append(allocator, "-C");
        try args.append(allocator, sim_path);
        try args.append(allocator, "checkout");
        if (std.mem.eql(u8, side, "theirs")) {
            try args.append(allocator, "--theirs");
        } else {
            try args.append(allocator, "--ours");
        }
        try args.append(allocator, "--");
        try args.append(allocator, path);

        const result = process.runGitWithStatus(allocator, args.items) catch return false;
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);
        if (result.exit_code != 0) {
            var rm_args: std.ArrayListUnmanaged([]const u8) = .{};
            defer rm_args.deinit(allocator);

            try rm_args.append(allocator, "-C");
            try rm_args.append(allocator, sim_path);
            try rm_args.append(allocator, "rm");
            try rm_args.append(allocator, "-f");
            try rm_args.append(allocator, "-r");
            try rm_args.append(allocator, "--");
            try rm_args.append(allocator, path);

            const rm_result = process.runGitWithStatus(allocator, rm_args.items) catch return false;
            defer allocator.free(rm_result.stdout);
            defer allocator.free(rm_result.stderr);
            if (rm_result.exit_code != 0) {
                return false;
            }
        }

        handled = true;
    }

    _ = process.runGitInDir(allocator, sim_path, &.{ "add", "-A" }) catch {};
    return handled;
}

fn resolveGitlinkConflicts(
    allocator: std.mem.Allocator,
    sim_path: []const u8,
    remaining: []const []const u8,
    side: []const u8,
) !bool {
    var handled = false;
    const stage = if (std.mem.eql(u8, side, "theirs")) "3" else "2";

    for (remaining) |path| {
        const hash = try getGitlinkStageHash(allocator, sim_path, path, stage) orelse continue;
        defer allocator.free(hash);
        handled = true;

        const result = process.runGitWithStatus(allocator, &.{
            "-C",
            sim_path,
            "update-index",
            "--cacheinfo",
            "160000",
            hash,
            path,
        }) catch return false;
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);
        if (result.exit_code != 0) {
            return false;
        }
    }

    return handled;
}

fn resolveDistinctTypeConflicts(
    allocator: std.mem.Allocator,
    sim_path: []const u8,
    remaining: []const []const u8,
    side: []const u8,
) !bool {
    var handled = false;
    for (remaining) |path| {
        const base_name = conflictBackupBase(path) orelse continue;
        handled = true;

        const base_path = try std.fs.path.join(allocator, &.{ sim_path, base_name });
        defer allocator.free(base_path);
        const head_path = try std.fs.path.join(allocator, &.{ sim_path, path });
        defer allocator.free(head_path);

        if (std.mem.eql(u8, side, "ours")) {
            removePath(base_path);
            std.fs.cwd().rename(head_path, base_path) catch {};
        } else {
            removePath(head_path);
        }
    }

    if (!handled) return false;

    _ = process.runGitInDir(allocator, sim_path, &.{ "add", "-A" }) catch {};
    return true;
}

fn removePath(path: []const u8) void {
    if (std.fs.cwd().statFile(path)) |stat| {
        if (stat.kind == .directory) {
            std.fs.cwd().deleteTree(path) catch {};
        } else {
            std.fs.cwd().deleteFile(path) catch {};
        }
    } else |_| {}
}

fn conflictBackupBase(path: []const u8) ?[]const u8 {
    const idx = std.mem.lastIndexOfScalar(u8, path, '~') orelse return null;
    if (idx == 0 or idx + 1 >= path.len) return null;
    const suffix = path[idx + 1 ..];
    if (std.mem.startsWith(u8, suffix, "HEAD")) {
        return path[0..idx];
    }
    if (suffix.len < 6) return null;
    for (suffix[0..6]) |c| {
        if (!std.ascii.isHex(c)) return null;
    }
    return path[0..idx];
}

fn isEmptyCherryPickOutput(output: []const u8) bool {
    return std.mem.indexOf(u8, output, "previous cherry-pick is now empty") != null or
        std.mem.indexOf(u8, output, "nothing to commit") != null;
}

fn detectMergetoolSide(mergetool: ?[]const u8) ?[]const u8 {
    const tool = mergetool orelse return null;
    if (containsIgnoreCase(tool, "theirs")) return "theirs";
    if (containsIgnoreCase(tool, "ours")) return "ours";
    return null;
}

fn containsIgnoreCase(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0 or haystack.len < needle.len) return false;
    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        var matched = true;
        for (needle, 0..) |n, j| {
            const h = std.ascii.toLower(haystack[i + j]);
            if (h != std.ascii.toLower(n)) {
                matched = false;
                break;
            }
        }
        if (matched) return true;
    }
    return false;
}

fn getConflictDiff(allocator: std.mem.Allocator, sim_path: []const u8, file_path: []const u8) ![]const u8 {
    const result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "diff",
        "--binary",
        "--",
        file_path,
    }) catch {
        return strings.copy(allocator, "");
    };
    defer allocator.free(result.stderr);

    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return strings.copy(allocator, "");
    }

    return result.stdout;
}

fn captureResolution(
    allocator: std.mem.Allocator,
    sim_path: []const u8,
    file_path: []const u8,
    prefer_gitlink: bool,
) !types.ConflictResolution {
    if (try getGitlinkHash(allocator, sim_path, file_path)) |hash| {
        return .{
            .present = true,
            .encoding = .gitlink,
            .content = hash,
        };
    }

    if (prefer_gitlink) {
        return .{
            .present = false,
            .encoding = .gitlink,
            .content = try strings.copy(allocator, ""),
        };
    }

    const full_path = try std.fs.path.join(allocator, &.{ sim_path, file_path });
    defer allocator.free(full_path);

    if (std.fs.cwd().access(full_path, .{})) |_| {
        const stat = std.fs.cwd().statFile(full_path) catch return .{
            .present = false,
            .encoding = .text,
            .content = try strings.copy(allocator, ""),
        };

        if (stat.kind == .directory) {
            return .{
                .present = false,
                .encoding = .text,
                .content = try strings.copy(allocator, ""),
            };
        }

        const content = try std.fs.cwd().readFileAlloc(allocator, full_path, 100 * 1024 * 1024);
        if (isBinary(content)) {
            const encoded = try encodeBase64(allocator, content);
            allocator.free(content);
            return .{
                .present = true,
                .encoding = .base64,
                .content = encoded,
            };
        }

        return .{
            .present = true,
            .encoding = .text,
            .content = content,
        };
    } else |_| {
        return .{
            .present = false,
            .encoding = .text,
            .content = try strings.copy(allocator, ""),
        };
    }
}

fn getGitlinkHash(allocator: std.mem.Allocator, sim_path: []const u8, file_path: []const u8) !?[]const u8 {
    const result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "ls-files",
        "--stage",
        "--",
        file_path,
    }) catch return null;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.exit_code != 0 or result.stdout.len == 0) return null;

    var iter = std.mem.splitScalar(u8, strings.trim(result.stdout), '\n');
    while (iter.next()) |line| {
        const trimmed = strings.trim(line);
        if (trimmed.len == 0) continue;
        var tokens = std.mem.tokenizeAny(u8, trimmed, " \t");
        const mode = tokens.next() orelse continue;
        const hash = tokens.next() orelse continue;
        const stage = tokens.next() orelse continue;
        if (!std.mem.eql(u8, mode, "160000")) continue;
        if (!std.mem.eql(u8, stage, "0")) continue;
        return try strings.copy(allocator, hash);
    }

    return null;
}

fn isGitlinkConflict(allocator: std.mem.Allocator, sim_path: []const u8, file_path: []const u8) bool {
    const result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "ls-files",
        "-u",
        "--",
        file_path,
    }) catch return false;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.exit_code != 0 or result.stdout.len == 0) return false;

    var iter = std.mem.splitScalar(u8, strings.trim(result.stdout), '\n');
    while (iter.next()) |line| {
        const trimmed = strings.trim(line);
        if (trimmed.len == 0) continue;
        var tokens = std.mem.tokenizeAny(u8, trimmed, " \t");
        const mode = tokens.next() orelse continue;
        if (std.mem.eql(u8, mode, "160000")) return true;
    }

    return false;
}

fn getGitlinkStageHash(
    allocator: std.mem.Allocator,
    sim_path: []const u8,
    file_path: []const u8,
    stage: []const u8,
) !?[]const u8 {
    const result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "ls-files",
        "-u",
        "--",
        file_path,
    }) catch return null;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.exit_code != 0 or result.stdout.len == 0) return null;

    var iter = std.mem.splitScalar(u8, strings.trim(result.stdout), '\n');
    while (iter.next()) |line| {
        const trimmed = strings.trim(line);
        if (trimmed.len == 0) continue;
        var tokens = std.mem.tokenizeAny(u8, trimmed, " \t");
        const mode = tokens.next() orelse continue;
        const hash = tokens.next() orelse continue;
        const found_stage = tokens.next() orelse continue;
        if (!std.mem.eql(u8, mode, "160000")) continue;
        if (!std.mem.eql(u8, found_stage, stage)) continue;
        return try strings.copy(allocator, hash);
    }

    return null;
}

fn runMergetool(allocator: std.mem.Allocator, sim_path: []const u8, tool: ?[]const u8) bool {
    var args: std.ArrayListUnmanaged([]const u8) = .{};
    defer args.deinit(allocator);

    args.append(allocator, "-C") catch return false;
    args.append(allocator, sim_path) catch return false;
    args.append(allocator, "mergetool") catch return false;
    args.append(allocator, "--no-prompt") catch return false;
    if (tool) |name| {
        args.append(allocator, "--tool") catch return false;
        args.append(allocator, name) catch return false;
    }

    const result = process.runGitWithStatus(allocator, args.items) catch return false;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    return result.exit_code == 0;
}

fn readLine(allocator: std.mem.Allocator) ![]const u8 {
    var stdin_file = std.fs.File.stdin();
    var buffer: [4096]u8 = undefined;
    var stdin_reader = stdin_file.reader(&buffer);
    const line = try stdin_reader.interface.takeDelimiter('\n');
    if (line) |slice| {
        return strings.copy(allocator, slice);
    }
    return strings.copy(allocator, "");
}

fn isBinary(content: []const u8) bool {
    for (content) |c| {
        if (c == 0) return true;
    }
    return false;
}

fn encodeBase64(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    const encoded_len = std.base64.standard.Encoder.calcSize(content.len);
    const encoded = try allocator.alloc(u8, encoded_len);
    _ = std.base64.standard.Encoder.encode(encoded, content);
    return encoded;
}

fn getCommitSubject(allocator: std.mem.Allocator, sim_path: []const u8, commit: []const u8) !?[]const u8 {
    const result = process.runGitWithStatus(allocator, &.{
        "-C",
        sim_path,
        "show",
        "-s",
        "--format=%s",
        commit,
    }) catch return null;
    defer allocator.free(result.stderr);

    if (result.exit_code != 0 or result.stdout.len == 0) {
        allocator.free(result.stdout);
        return null;
    }

    const subject = try strings.copy(allocator, strings.trim(result.stdout));
    allocator.free(result.stdout);
    return subject;
}

const CleanupContext = struct {
    mode: types.PlanMode,
    sim_path: []const u8,
    plan_branch: []const u8,
    created_worktree: bool,
    created_plan_branch: bool,
    original_branch: ?[]const u8,
    stashed: bool,
    base_branch: []const u8,
};

fn cleanupSimulation(allocator: std.mem.Allocator, ctx: CleanupContext) void {
    if (ctx.mode == .worktree) {
        if (ctx.created_plan_branch) {
            if (process.runGitWithStatus(allocator, &.{ "-C", ctx.sim_path, "checkout", ctx.base_branch })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
            if (process.runGitWithStatus(allocator, &.{ "branch", "-D", ctx.plan_branch })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
        }
        if (ctx.created_worktree) {
            if (process.runGitWithStatus(allocator, &.{ "worktree", "remove", ctx.sim_path, "--force" })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
            if (process.runGit(allocator, &.{ "worktree", "prune" })) |output| {
                allocator.free(output);
            } else |_| {}
        }
    } else {
        if (ctx.original_branch) |branch| {
            if (process.runGitWithStatus(allocator, &.{ "checkout", branch })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
        }
        if (ctx.created_plan_branch) {
            if (process.runGitWithStatus(allocator, &.{ "branch", "-D", ctx.plan_branch })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
        }
        if (ctx.stashed) {
            if (process.runGitWithStatus(allocator, &.{ "stash", "pop" })) |result| {
                allocator.free(result.stdout);
                allocator.free(result.stderr);
            } else |_| {}
        }
    }
}

fn worktreeExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

fn defaultPlanWorktreePath(allocator: std.mem.Allocator, cwd: []const u8) ![]const u8 {
    const repo_name = std.fs.path.basename(cwd);
    return std.fmt.allocPrint(allocator, "../{s}{s}", .{ repo_name, DEFAULT_PLAN_WORKTREE_SUFFIX });
}

fn parsePlanMode(value: []const u8) types.PlanMode {
    return types.PlanMode.fromString(value) orelse .worktree;
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack plan [OPTIONS]
        \\
        \\Analyzes staged and unstaged changes, maps them to target branches.
        \\
        \\Options:
        \\  -o, --output <file>      Write plan to file (default: .git/git-restack/plan.yml)
        \\  --verify <cmd>           Command to run after each commit during exec
        \\  --verify-only <cmd>      Restack without changes, only run verification
        \\  --staged-only            Only analyze staged changes
        \\  --unstaged-only          Only analyze unstaged changes
        \\  --mode <worktree|direct> Conflict detection mode (default: worktree)
        \\  --direct                 Alias for --mode direct
        \\  --base, --onto <branch>  Override base branch (default: develop or main)
        \\  --worktree-path <path>   Where to create plan worktree (worktree mode)
        \\  --mergetool <tool>       Merge tool to launch on conflicts (uses git config if omitted)
        \\  -f, --force              Remove existing plan worktree if present
        \\  -h, --help               Show this help message
        \\
        \\Exit Codes:
        \\  0 - Success, all files mapped
        \\  1 - Unmapped files detected. Plan generated with 'errors' block.
        \\
        \\Examples:
        \\  git-restack plan --verify "swift build"
        \\  git-restack plan --verify "npm test"
        \\  git-restack plan --verify-only "make lint && make test"
        \\  git-restack plan --base release/1.2.0
        \\
    , .{});
}
