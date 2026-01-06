const std = @import("std");
const process = @import("../utils/process.zig");
const strings = @import("../utils/strings.zig");
const restack = @import("../utils/restack.zig");
const parser = @import("../yaml/parser.zig");

const STATE_DIR = ".git/git-restack";
const STATE_FILE = ".git/git-restack/state.json";
const PLAN_FILE = ".git/git-restack/plan.yml";
const PLAN_WORKTREE_SUFFIX = "-restack-plan";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var force = false;
    var keep_branches = false;

    // Parse options
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-f") or std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--keep-branches")) {
            keep_branches = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
    }

    // Check if there's anything to nuke
    const has_state_dir = blk: {
        std.fs.cwd().access(STATE_DIR, .{}) catch break :blk false;
        break :blk true;
    };

    // Determine worktree path
    const cwd = try process.getCwd(allocator);
    defer allocator.free(cwd);
    const repo_name = std.fs.path.basename(cwd);
    const worktree_path = try std.fmt.allocPrint(allocator, "../{s}-restack", .{repo_name});
    defer allocator.free(worktree_path);

    const has_worktree = blk: {
        std.fs.cwd().access(worktree_path, .{}) catch break :blk false;
        break :blk true;
    };

    // Find fix branches
    var fix_branches: std.ArrayListUnmanaged([]const u8) = .{};
    var backup_branches: std.ArrayListUnmanaged([]const u8) = .{};
    var plan_branches: std.ArrayListUnmanaged([]const u8) = .{};
    var plan_worktree_path: ?[]const u8 = null;
    defer {
        for (fix_branches.items) |b| allocator.free(b);
        fix_branches.deinit(allocator);
        for (backup_branches.items) |b| allocator.free(b);
        backup_branches.deinit(allocator);
        for (plan_branches.items) |b| allocator.free(b);
        plan_branches.deinit(allocator);
        if (plan_worktree_path) |path| allocator.free(path);
    }

    const branches_result = process.runGitWithStatus(allocator, &.{
        "branch",
        "--list",
        "git-restack/fix/*",
    }) catch null;

    if (branches_result) |result| {
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        if (result.exit_code == 0) {
            var lines = std.mem.splitScalar(u8, result.stdout, '\n');
            while (lines.next()) |line| {
                const trimmed = strings.trim(line);
                // Remove leading "* " or "  " from branch name
                const branch_name = if (trimmed.len > 2 and (trimmed[0] == '*' or trimmed[0] == ' '))
                    strings.trim(trimmed[2..])
                else
                    trimmed;
                
                if (branch_name.len > 0 and restack.isFixBranch(branch_name)) {
                    fix_branches.append(allocator, strings.copy(allocator, branch_name) catch continue) catch continue;
                }
            }
        }
    }

    if (std.fs.cwd().readFileAlloc(allocator, PLAN_FILE, 10 * 1024 * 1024)) |plan_content| {
        defer allocator.free(plan_content);
        if (parser.parsePlan(allocator, plan_content)) |plan_value| {
            var plan = plan_value;
            defer plan.deinit(allocator);
            if (plan.simulation) |sim| {
                if (sim.worktree_path) |path| {
                    plan_worktree_path = strings.copy(allocator, path) catch null;
                }
                if (sim.plan_branch) |branch_name| {
                    if (strings.copy(allocator, branch_name)) |copied| {
                        plan_branches.append(allocator, copied) catch allocator.free(copied);
                    } else |_| {}
                }
                for (sim.backup_branches) |backup| {
                    if (strings.copy(allocator, backup.backup)) |copied| {
                        backup_branches.append(allocator, copied) catch allocator.free(copied);
                    } else |_| {}
                }
            }
        } else |_| {}
    } else |_| {}

    // Fallback patterns if plan file is missing
    if (plan_worktree_path == null) {
        const default_plan_path = try std.fmt.allocPrint(allocator, "../{s}{s}", .{ repo_name, PLAN_WORKTREE_SUFFIX });
        defer allocator.free(default_plan_path);
        if (std.fs.cwd().access(default_plan_path, .{})) |_| {
            plan_worktree_path = try strings.copy(allocator, default_plan_path);
        } else |_| {}
    }

    if (backup_branches.items.len == 0) {
        const backup_result = process.runGitWithStatus(allocator, &.{
            "branch",
            "--list",
            "*-restack-backup-*",
        }) catch null;

        if (backup_result) |result| {
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            if (result.exit_code == 0) {
                var lines = std.mem.splitScalar(u8, result.stdout, '\n');
                while (lines.next()) |line| {
                    const trimmed = strings.trim(line);
                    const branch_name = if (trimmed.len > 2 and (trimmed[0] == '*' or trimmed[0] == ' '))
                        strings.trim(trimmed[2..])
                    else
                        trimmed;

                    if (branch_name.len > 0 and std.mem.indexOf(u8, branch_name, "restack-backup") != null) {
                        backup_branches.append(allocator, strings.copy(allocator, branch_name) catch continue) catch continue;
                    }
                }
            }
        }
    }

    if (plan_branches.items.len == 0) {
        const plan_branch_result = process.runGitWithStatus(allocator, &.{
            "branch",
            "--list",
            "git-restack-plan-*",
        }) catch null;

        if (plan_branch_result) |result| {
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            if (result.exit_code == 0) {
                var lines = std.mem.splitScalar(u8, result.stdout, '\n');
                while (lines.next()) |line| {
                    const trimmed = strings.trim(line);
                    const branch_name = if (trimmed.len > 2 and (trimmed[0] == '*' or trimmed[0] == ' '))
                        strings.trim(trimmed[2..])
                    else
                        trimmed;

                    if (branch_name.len > 0 and std.mem.startsWith(u8, branch_name, "git-restack-plan-")) {
                        plan_branches.append(allocator, strings.copy(allocator, branch_name) catch continue) catch continue;
                    }
                }
            }
        }
    }

    const has_plan_worktree = if (plan_worktree_path) |path| blk: {
        std.fs.cwd().access(path, .{}) catch break :blk false;
        break :blk true;
    } else false;

    if (!has_state_dir and !has_worktree and !has_plan_worktree and fix_branches.items.len == 0 and backup_branches.items.len == 0 and plan_branches.items.len == 0) {
        std.debug.print("Nothing to clean up. No git-restack state found.\n", .{});
        return;
    }

    // Show what will be deleted
    std.debug.print("\n\x1b[33mThis will delete:\x1b[0m\n\n", .{});

    if (has_state_dir) {
        std.debug.print("  • State directory: {s}\n", .{STATE_DIR});
        
        // Check for plan file
        const has_plan = blk: {
            std.fs.cwd().access(PLAN_FILE, .{}) catch break :blk false;
            break :blk true;
        };
        if (has_plan) {
            std.debug.print("    └── Plan file: {s}\n", .{PLAN_FILE});
        }
    }

    if (has_worktree) {
        std.debug.print("  • Worktree: {s}\n", .{worktree_path});
    }

    if (fix_branches.items.len > 0 and !keep_branches) {
        std.debug.print("  • {d} git-restack/fix branches:\n", .{fix_branches.items.len});
        for (fix_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    if (has_plan_worktree) {
        std.debug.print("  • Plan worktree: {s}\n", .{plan_worktree_path.?});
    }

    if (backup_branches.items.len > 0 and !keep_branches) {
        std.debug.print("  • {d} backup branches:\n", .{backup_branches.items.len});
        for (backup_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    if (plan_branches.items.len > 0 and !keep_branches) {
        std.debug.print("  • {d} plan branches:\n", .{plan_branches.items.len});
        for (plan_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    std.debug.print("\n", .{});

    // Require --force for non-interactive use
    if (!force) {
        std.debug.print("Are you sure? This cannot be undone.\n", .{});
        std.debug.print("Run with --force to confirm.\n", .{});
        return;
    }

    std.debug.print("\nCleaning up...\n\n", .{});

    // Delete worktree first (before deleting branches it might reference)
    if (has_worktree) {
        const result = process.runGitWithStatus(allocator, &.{
            "worktree",
            "remove",
            worktree_path,
            "--force",
        }) catch null;

        if (result) |r| {
            defer allocator.free(r.stdout);
            defer allocator.free(r.stderr);
            
            if (r.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Removed worktree: {s}\n", .{worktree_path});
            } else {
                // Try manual removal
                std.fs.cwd().deleteTree(worktree_path) catch {};
                std.debug.print("  \x1b[32m✓\x1b[0m Removed worktree (forced): {s}\n", .{worktree_path});
            }
        }
        
        // Prune worktree references
        _ = process.runGit(allocator, &.{ "worktree", "prune" }) catch {};
    }

    if (has_plan_worktree and (plan_worktree_path == null or !std.mem.eql(u8, plan_worktree_path.?, worktree_path))) {
        const plan_path = plan_worktree_path.?;
        const result = process.runGitWithStatus(allocator, &.{
            "worktree",
            "remove",
            plan_path,
            "--force",
        }) catch null;

        if (result) |r| {
            defer allocator.free(r.stdout);
            defer allocator.free(r.stderr);

            if (r.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Removed plan worktree: {s}\n", .{plan_path});
            } else {
                std.fs.cwd().deleteTree(plan_path) catch {};
                std.debug.print("  \x1b[32m✓\x1b[0m Removed plan worktree (forced): {s}\n", .{plan_path});
            }
        }

        _ = process.runGit(allocator, &.{ "worktree", "prune" }) catch {};
    }

    // Delete fix branches
    if (!keep_branches) {
        for (fix_branches.items) |branch| {
            const result = process.runGitWithStatus(allocator, &.{
                "branch",
                "-D",
                branch,
            }) catch continue;
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            if (result.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Deleted branch: {s}\n", .{branch});
            } else {
                std.debug.print("  \x1b[33m⚠\x1b[0m Could not delete branch: {s}\n", .{branch});
            }
        }

        for (backup_branches.items) |branch| {
            const result = process.runGitWithStatus(allocator, &.{
                "branch",
                "-D",
                branch,
            }) catch continue;
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            if (result.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Deleted backup: {s}\n", .{branch});
            } else {
                std.debug.print("  \x1b[33m⚠\x1b[0m Could not delete backup: {s}\n", .{branch});
            }
        }

        for (plan_branches.items) |branch| {
            const result = process.runGitWithStatus(allocator, &.{
                "branch",
                "-D",
                branch,
            }) catch continue;
            defer allocator.free(result.stdout);
            defer allocator.free(result.stderr);

            if (result.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Deleted plan branch: {s}\n", .{branch});
            } else {
                std.debug.print("  \x1b[33m⚠\x1b[0m Could not delete plan branch: {s}\n", .{branch});
            }
        }
    }

    // Delete state directory
    if (has_state_dir) {
        std.fs.cwd().deleteTree(STATE_DIR) catch |err| {
            std.debug.print("  \x1b[33m⚠\x1b[0m Could not delete state directory: {any}\n", .{err});
        };
        std.debug.print("  \x1b[32m✓\x1b[0m Removed state directory\n", .{});
    }

    std.debug.print("\n\x1b[32mDone!\x1b[0m All git-restack state has been removed.\n", .{});
    std.debug.print("Your original branches are unchanged.\n", .{});
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack nuke [OPTIONS]
        \\
        \\Removes all git-restack state from the repository:
        \\  - The .git/git-restack directory (plan and state files)
        \\  - The worktree (../<repo>-restack)
        \\  - The plan worktree (../<repo>-restack-plan)
        \\  - All git-restack/fix/* branches
        \\  - Any git-restack plan/backup branches
        \\
        \\Your original branches are NOT affected.
        \\
        \\Options:
        \\  -f, --force         Skip confirmation prompt
        \\  --keep-branches     Don't delete git-restack/fix/* branches
        \\  -h, --help          Show this help message
        \\
        \\Examples:
        \\  git-restack nuke              # Interactive cleanup
        \\  git-restack nuke --force      # Non-interactive cleanup
        \\  git-restack nuke --keep-branches  # Keep git-restack/fix/* branches for review
        \\
    , .{});
}
