const std = @import("std");
const process = @import("../utils/process.zig");
const strings = @import("../utils/strings.zig");
const restack = @import("../utils/restack.zig");
const parser = @import("../yaml/parser.zig");

const STATE_DIR = ".git/git-restack";
const STATE_FILE = ".git/git-restack/state.json";
const PLAN_FILE = ".git/git-restack/plan.yml";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var plan_file: ?[]const u8 = null;
    var worktree_path: ?[]const u8 = null;
    var force = false;
    var keep_plan = false;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--worktree-path")) {
            if (i + 1 < args.len) {
                i += 1;
                worktree_path = args[i];
            }
        } else if (std.mem.eql(u8, arg, "-f") or std.mem.eql(u8, arg, "--force")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--keep-plan")) {
            keep_plan = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            plan_file = arg;
        }
    }

    // Determine worktree path
    const cwd = try process.getCwd(allocator);
    defer allocator.free(cwd);
    const repo_name = std.fs.path.basename(cwd);
    const default_worktree = try std.fmt.allocPrint(allocator, "../{s}-restack", .{repo_name});
    defer allocator.free(default_worktree);
    const wt_path = worktree_path orelse default_worktree;

    // Check if worktree exists
    const has_worktree = blk: {
        std.fs.cwd().access(wt_path, .{}) catch break :blk false;
        break :blk true;
    };

    // Try to load plan to find fix/backup/plan branches
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

    // Try default plan location, then explicit plan file
    const plan_path = plan_file orelse PLAN_FILE;
    
    if (std.fs.cwd().readFileAlloc(allocator, plan_path, 10 * 1024 * 1024)) |plan_content| {
        defer allocator.free(plan_content);

        if (parser.parsePlan(allocator, plan_content)) |plan_value| {
            var plan = plan_value;
            defer plan.deinit(allocator);

            for (plan.stack.branches) |branch| {
                const fix_name = restack.makeFixBranchName(allocator, branch.name) catch continue;
                fix_branches.append(allocator, fix_name) catch continue;
            }

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
    } else |_| {
        // No plan file, find fix branches by pattern
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

    if (!has_worktree and !has_plan_worktree and fix_branches.items.len == 0 and backup_branches.items.len == 0 and plan_branches.items.len == 0) {
        std.debug.print("No git-restack artifacts found.\n", .{});
        return;
    }

    // Show what will be deleted
    std.debug.print("\nCleanup will remove:\n\n", .{});
    if (has_worktree) {
        std.debug.print("  • Worktree: {s}\n", .{wt_path});
    }
    if (has_plan_worktree) {
        std.debug.print("  • Plan worktree: {s}\n", .{plan_worktree_path.?});
    }
    
    if (fix_branches.items.len > 0) {
        std.debug.print("  • {d} git-restack/fix branches:\n", .{fix_branches.items.len});
        for (fix_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    if (backup_branches.items.len > 0) {
        std.debug.print("  • {d} backup branches:\n", .{backup_branches.items.len});
        for (backup_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    if (plan_branches.items.len > 0) {
        std.debug.print("  • {d} plan branches:\n", .{plan_branches.items.len});
        for (plan_branches.items) |branch| {
            std.debug.print("    └── {s}\n", .{branch});
        }
    }

    if (!keep_plan) {
        const has_plan = blk: {
            std.fs.cwd().access(PLAN_FILE, .{}) catch break :blk false;
            break :blk true;
        };
        if (has_plan) {
            std.debug.print("  • Plan file: {s}\n", .{PLAN_FILE});
        }
        
        const has_state = blk: {
            std.fs.cwd().access(STATE_FILE, .{}) catch break :blk false;
            break :blk true;
        };
        if (has_state) {
            std.debug.print("  • State file: {s}\n", .{STATE_FILE});
        }
    }

    std.debug.print("\n", .{});

    // Require --force for non-interactive use
    if (!force) {
        std.debug.print("Run with --force to confirm cleanup.\n", .{});
        return;
    }

    std.debug.print("\nCleaning up...\n\n", .{});

    if (has_worktree) {
        const wt_result = process.runGitWithStatus(allocator, &.{
            "worktree",
            "remove",
            wt_path,
            "--force",
        }) catch null;

        if (wt_result) |r| {
            defer allocator.free(r.stdout);
            defer allocator.free(r.stderr);

            if (r.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Removed worktree\n", .{});
            } else {
                std.fs.cwd().deleteTree(wt_path) catch {};
                std.debug.print("  \x1b[32m✓\x1b[0m Removed worktree (forced)\n", .{});
            }
        }
    }

    if (has_plan_worktree and (plan_worktree_path == null or !std.mem.eql(u8, plan_worktree_path.?, wt_path))) {
        const plan_wt_path = plan_worktree_path.?;
        const plan_result = process.runGitWithStatus(allocator, &.{
            "worktree",
            "remove",
            plan_wt_path,
            "--force",
        }) catch null;

        if (plan_result) |r| {
            defer allocator.free(r.stdout);
            defer allocator.free(r.stderr);

            if (r.exit_code == 0) {
                std.debug.print("  \x1b[32m✓\x1b[0m Removed plan worktree\n", .{});
            } else {
                std.fs.cwd().deleteTree(plan_wt_path) catch {};
                std.debug.print("  \x1b[32m✓\x1b[0m Removed plan worktree (forced)\n", .{});
            }
        }
    }

    _ = process.runGit(allocator, &.{ "worktree", "prune" }) catch {};

    // Delete fix branches
    for (fix_branches.items) |branch| {
        const result = process.runGitWithStatus(allocator, &.{
            "branch",
            "-D",
            branch,
        }) catch continue;
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        if (result.exit_code == 0) {
            std.debug.print("  \x1b[32m✓\x1b[0m Deleted: {s}\n", .{branch});
        } else {
            std.debug.print("  \x1b[33m⚠\x1b[0m Could not delete: {s}\n", .{branch});
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

    // Delete state files
    if (!keep_plan) {
        std.fs.cwd().deleteFile(STATE_FILE) catch {};
        std.fs.cwd().deleteFile(PLAN_FILE) catch {};
        std.fs.cwd().deleteDir(STATE_DIR) catch {};
        std.debug.print("  \x1b[32m✓\x1b[0m Removed state files\n", .{});
    }

    std.debug.print("\n\x1b[32mDone!\x1b[0m Cleanup complete.\n", .{});
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack cleanup [plan.yml] [OPTIONS]
        \\
        \\Removes git-restack worktrees and temporary branches after you're done reviewing.
        \\Run this after 'git-restack apply' when you're satisfied with the results.
        \\
        \\This removes:
        \\  - The worktree (../<repo>-restack)
        \\  - The plan worktree (../<repo>-restack-plan)
        \\  - All git-restack/fix/* branches created by 'exec'
        \\  - Any git-restack plan/backup branches
        \\  - The plan and state files (unless --keep-plan)
        \\
        \\Options:
        \\  --worktree-path <path>   Path to worktree (default: ../<repo>-restack)
        \\  --keep-plan              Don't delete the plan file
        \\  -f, --force              Skip confirmation prompt
        \\  -h, --help               Show this help message
        \\
        \\Examples:
        \\  git-restack cleanup                 # Clean up default locations
        \\  git-restack cleanup --force         # Skip confirmation
        \\  git-restack cleanup --keep-plan     # Keep plan for reference
        \\
    , .{});
}
