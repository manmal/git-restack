const std = @import("std");
const types = @import("../types.zig");
const stack_mod = @import("../git/stack.zig");
const diff_mod = @import("../git/diff.zig");
const emitter = @import("../yaml/emitter.zig");
const strings = @import("../utils/strings.zig");
const process = @import("../utils/process.zig");

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var output_file: []const u8 = "jenga-plan.yml";
    var staged_only = false;
    var unstaged_only = false;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            if (i + 1 < args.len) {
                i += 1;
                output_file = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--staged-only")) {
            staged_only = true;
        } else if (std.mem.eql(u8, arg, "--unstaged-only")) {
            unstaged_only = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
    }

    // 1. Analyze stack
    std.debug.print("Analyzing branch stack...\n", .{});
    const stack = stack_mod.analyzeStack(allocator) catch |err| {
        switch (err) {
            types.JengaError.GitCommandFailed => {
                std.debug.print("Error: Git command failed. Are you in a git repository?\n", .{});
            },
            types.JengaError.BaseBranchNotFound => {
                std.debug.print("Error: Could not find 'develop' or 'main' base branch.\n", .{});
            },
            else => {
                std.debug.print("Error: {any}\n", .{err});
            },
        }
        std.process.exit(1);
    };

    std.debug.print("Found {d} branches in stack\n", .{stack.branches.len});

    // 2. Collect changed files
    std.debug.print("Collecting changed files...\n", .{});

    var all_files: std.ArrayListUnmanaged(diff_mod.ChangedFile) = .{};
    defer all_files.deinit(allocator);

    if (!unstaged_only) {
        const staged = try diff_mod.getStagedFiles(allocator);
        for (staged) |file| {
            try all_files.append(allocator, file);
        }
    }

    if (!staged_only) {
        const unstaged = try diff_mod.getUnstagedFiles(allocator);
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
            }
        }
    }

    std.debug.print("Found {d} changed files\n", .{all_files.items.len});

    // 3. Map files to branches
    var errors: std.ArrayListUnmanaged(types.PlanError) = .{};
    defer errors.deinit(allocator);

    // Create a mutable copy of branches with fix info
    var branches_with_fixes: std.ArrayListUnmanaged(types.StackBranch) = .{};
    defer branches_with_fixes.deinit(allocator);

    for (stack.branches) |branch| {
        try branches_with_fixes.append(allocator, .{
            .name = branch.name,
            .commit_sha = branch.commit_sha,
            .parent_branch = branch.parent_branch,
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

        if (last_commit == null) {
            // New file - unmapped
            try errors.append(allocator, .{
                .error_type = .unmapped_file,
                .path = file.path,
                .message = "New file detected. Assign to a branch fix block manually.",
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
                    .path = file.path,
                    .message = "File modified from commit outside stack ancestry.",
                });
                continue;
            }
        }

        // Find which branch the commit belongs to
        const target_branch = try diff_mod.findBranchForCommit(allocator, last_commit.?, stack);

        if (target_branch == null) {
            // Couldn't map to a specific branch
            try errors.append(allocator, .{
                .error_type = .ambiguous_branch,
                .path = file.path,
                .message = "Could not determine target branch for this file.",
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
                }
                try files_list.append(allocator, .{
                    .path = file.path,
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

    // 4. Generate plan
    const cwd = try process.getCwd(allocator);
    const timestamp = try strings.formatTimestamp(allocator);

    const plan = types.Plan{
        .version = 1,
        .generated = timestamp,
        .repository = cwd,
        .errors = try errors.toOwnedSlice(allocator),
        .stack = .{
            .branches = try branches_with_fixes.toOwnedSlice(allocator),
            .base_branch = stack.base_branch,
            .base_commit = stack.base_commit,
            .head_branch = stack.head_branch,
            .head_commit = stack.head_commit,
        },
    };

    // 5. Write YAML output
    const yaml = try emitter.emitPlan(allocator, plan);

    const file = try std.fs.cwd().createFile(output_file, .{});
    defer file.close();
    try file.writeAll(yaml);

    std.debug.print("\nPlan written to: {s}\n", .{output_file});

    // 6. Report status
    if (plan.errors.len > 0) {
        std.debug.print("\n\x1b[33mWarning:\x1b[0m Plan has {d} unresolved errors.\n\n", .{plan.errors.len});

        for (plan.errors, 1..) |err, num| {
            std.debug.print("  {d}. [{s}] {s}\n", .{ num, err.error_type.toString(), err.path });
            std.debug.print("     {s}\n\n", .{err.message});
        }

        std.debug.print("Fix these in {s}, then run: git-jenga exec {s}\n", .{ output_file, output_file });
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
            std.debug.print("Review the plan and run: git-jenga exec {s}\n", .{output_file});
        }
    }
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-jenga plan [OPTIONS]
        \\
        \\Analyzes staged and unstaged changes, maps them to target branches.
        \\
        \\Options:
        \\  -o, --output <file>   Write plan to YAML file (default: jenga-plan.yml)
        \\  --staged-only         Only analyze staged changes
        \\  --unstaged-only       Only analyze unstaged changes
        \\  -h, --help            Show this help message
        \\
        \\Exit Codes:
        \\  0 - Success, all files mapped
        \\  1 - Unmapped files detected. Plan generated with 'errors' block.
        \\
    , .{});
}
