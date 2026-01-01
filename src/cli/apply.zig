const std = @import("std");
const types = @import("../types.zig");
const parser = @import("../yaml/parser.zig");
const strings = @import("../utils/strings.zig");
const process = @import("../utils/process.zig");

const DEFAULT_PLAN_FILE = ".git/git-restack/plan.yml";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var plan_file: ?[]const u8 = null;
    var worktree_path: ?[]const u8 = null;
    var force = false;
    var cleanup = false;
    var dry_run = false;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--worktree-path")) {
            if (i + 1 < args.len) {
                i += 1;
                worktree_path = args[i];
            }
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            force = true;
        } else if (std.mem.eql(u8, arg, "--cleanup")) {
            cleanup = true;
        } else if (std.mem.eql(u8, arg, "--dry-run") or std.mem.eql(u8, arg, "-n")) {
            dry_run = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            plan_file = arg;
        }
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

    // Determine worktree path
    const cwd = try process.getCwd(allocator);
    const repo_name = std.fs.path.basename(cwd);
    const default_worktree = try std.fmt.allocPrint(allocator, "../{s}-restack", .{repo_name});
    const wt_path = worktree_path orelse default_worktree;

    // Check if worktree exists
    std.fs.cwd().access(wt_path, .{}) catch {
        std.debug.print("Error: Worktree '{s}' does not exist.\n", .{wt_path});
        std.debug.print("Did you run 'git-restack exec {s}' first?\n", .{actual_plan_file});
        std.process.exit(1);
    };

    std.debug.print("\nVerifying plan against current repository state...\n\n", .{});

    // Step 1: Verify no divergence
    var diverged = false;
    var divergence_details: std.ArrayListUnmanaged(u8) = .{};
    defer divergence_details.deinit(allocator);

    for (plan.stack.branches) |branch| {
        // Get current commit of original branch
        const current_commit_raw = process.runGitWithStatus(allocator, &.{
            "rev-parse",
            branch.name,
        }) catch {
            std.debug.print("Error: Could not get commit for branch '{s}'\n", .{branch.name});
            std.process.exit(1);
        };
        defer allocator.free(current_commit_raw.stdout);
        defer allocator.free(current_commit_raw.stderr);

        if (current_commit_raw.exit_code != 0) {
            std.debug.print("Error: Branch '{s}' no longer exists.\n", .{branch.name});
            diverged = true;
            continue;
        }

        const current_commit = strings.trim(current_commit_raw.stdout);

        // Compare with plan's recorded commit
        if (!std.mem.eql(u8, current_commit, branch.commit_sha)) {
            diverged = true;
            const line1 = std.fmt.allocPrint(allocator, "  {s}:\n", .{branch.name}) catch continue;
            defer allocator.free(line1);
            const line2 = std.fmt.allocPrint(allocator, "    Plan:    {s}\n", .{branch.commit_sha}) catch continue;
            defer allocator.free(line2);
            const line3 = std.fmt.allocPrint(allocator, "    Current: {s}\n", .{current_commit}) catch continue;
            defer allocator.free(line3);
            divergence_details.appendSlice(allocator, line1) catch continue;
            divergence_details.appendSlice(allocator, line2) catch continue;
            divergence_details.appendSlice(allocator, line3) catch continue;
        }
    }

    if (diverged and !force) {
        std.debug.print("\x1b[31mError:\x1b[0m Repository has diverged from the plan!\n\n", .{});
        std.debug.print("The following branches have changed since the plan was created:\n", .{});
        std.debug.print("{s}\n", .{divergence_details.items});
        std.debug.print("This could mean:\n", .{});
        std.debug.print("  - Someone pushed new commits to these branches\n", .{});
        std.debug.print("  - You created the plan from a different repository state\n", .{});
        std.debug.print("\nOptions:\n", .{});
        std.debug.print("  1. Re-run 'git-restack plan' to create a new plan\n", .{});
        std.debug.print("  2. Use --force to apply anyway (dangerous!)\n", .{});
        std.process.exit(1);
    }

    if (diverged and force) {
        std.debug.print("\x1b[33mWarning:\x1b[0m Applying despite divergence (--force)\n\n", .{});
    } else {
        std.debug.print("\x1b[32m✓\x1b[0m All branches match the plan\n\n", .{});
    }

    // Step 2: Verify -fix branches exist in worktree
    std.debug.print("Verifying -fix branches in worktree...\n\n", .{});

    for (plan.stack.branches) |branch| {
        const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});
        defer allocator.free(fix_branch_name);

        // Check if -fix branch exists
        const check_result = process.runGitWithStatus(allocator, &.{
            "-C",
            wt_path,
            "rev-parse",
            "--verify",
            fix_branch_name,
        }) catch {
            std.debug.print("Error: Could not verify branch '{s}'\n", .{fix_branch_name});
            std.process.exit(1);
        };
        defer allocator.free(check_result.stdout);
        defer allocator.free(check_result.stderr);

        if (check_result.exit_code != 0) {
            std.debug.print("Error: Branch '{s}' not found in worktree.\n", .{fix_branch_name});
            std.debug.print("Did 'git-restack exec' complete successfully?\n", .{});
            std.process.exit(1);
        }

        std.debug.print("  \x1b[32m✓\x1b[0m {s}\n", .{fix_branch_name});
    }

    std.debug.print("\n", .{});

    // Step 3: Reset original branches to -fix branch tips
    if (dry_run) {
        std.debug.print("\x1b[33mDry run:\x1b[0m The following changes would be made:\n\n", .{});
    } else {
        std.debug.print("Applying changes...\n\n", .{});
    }

    for (plan.stack.branches) |branch| {
        const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});
        defer allocator.free(fix_branch_name);

        // Get the commit SHA of the -fix branch
        const fix_commit_raw = process.runGitWithStatus(allocator, &.{
            "-C",
            wt_path,
            "rev-parse",
            fix_branch_name,
        }) catch {
            std.debug.print("Error: Could not get commit for '{s}'\n", .{fix_branch_name});
            std.process.exit(1);
        };
        defer allocator.free(fix_commit_raw.stdout);
        defer allocator.free(fix_commit_raw.stderr);

        const fix_commit = strings.trim(fix_commit_raw.stdout);

        if (dry_run) {
            std.debug.print("  {s} → {s}\n", .{ branch.name, fix_commit[0..@min(7, fix_commit.len)] });
        } else {
            // Force update the original branch to point to the -fix commit
            const update_result = process.runGitWithStatus(allocator, &.{
                "branch",
                "-f",
                branch.name,
                fix_commit,
            }) catch {
                std.debug.print("Error: Could not update branch '{s}'\n", .{branch.name});
                std.process.exit(1);
            };
            defer allocator.free(update_result.stdout);
            defer allocator.free(update_result.stderr);

            if (update_result.exit_code != 0) {
                std.debug.print("Error: Failed to update '{s}': {s}\n", .{ branch.name, update_result.stderr });
                std.process.exit(1);
            }

            std.debug.print("  \x1b[32m✓\x1b[0m {s} → {s}\n", .{ branch.name, fix_commit[0..@min(7, fix_commit.len)] });
        }
    }

    if (dry_run) {
        std.debug.print("\nRun without --dry-run to apply these changes.\n", .{});
        return;
    }

    // Step 4: Optional cleanup
    if (cleanup) {
        std.debug.print("\nCleaning up...\n", .{});

        // Delete -fix branches from worktree
        for (plan.stack.branches) |branch| {
            const fix_branch_name = try std.fmt.allocPrint(allocator, "{s}-fix", .{branch.name});
            defer allocator.free(fix_branch_name);

            _ = process.runGitWithStatus(allocator, &.{
                "-C",
                wt_path,
                "branch",
                "-D",
                fix_branch_name,
            }) catch {};
        }

        // Remove worktree
        _ = process.runGit(allocator, &.{ "worktree", "remove", wt_path, "--force" }) catch {};

        std.debug.print("  \x1b[32m✓\x1b[0m Removed worktree: {s}\n", .{wt_path});
    }

    std.debug.print("\n\x1b[32m══════════════════════════════════════════\x1b[0m\n", .{});
    std.debug.print("\x1b[32m Apply complete! \x1b[0m\n", .{});
    std.debug.print("\x1b[32m══════════════════════════════════════════\x1b[0m\n\n", .{});

    std.debug.print("Updated branches:\n", .{});
    for (plan.stack.branches) |branch| {
        std.debug.print("  • {s}\n", .{branch.name});
    }

    std.debug.print("\nNext steps:\n", .{});
    std.debug.print("  1. Verify the changes: git log --oneline --graph\n", .{});
    std.debug.print("  2. Force-push the updated branches:\n", .{});
    for (plan.stack.branches) |branch| {
        std.debug.print("       git push --force-with-lease origin {s}\n", .{branch.name});
    }
    
    if (!cleanup) {
        std.debug.print("  3. Clean up worktree when satisfied:\n", .{});
        std.debug.print("       git worktree remove {s}\n", .{wt_path});
    }

    // Discard local changes (they're now in the commits)
    std.debug.print("\n\x1b[33mNote:\x1b[0m Your staged/unstaged changes are now in the commits.\n", .{});
    std.debug.print("Run 'git checkout .' to discard the working tree changes.\n", .{});
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack apply [plan.yml] [OPTIONS]
        \\
        \\Applies the executed plan by resetting original branches to their -fix counterparts.
        \\
        \\This command:
        \\  1. Verifies no branches have diverged since the plan was created
        \\  2. Verifies all -fix branches exist in the worktree
        \\  3. Force-updates each original branch to its -fix branch's commit
        \\  4. Optionally cleans up the worktree and -fix branches
        \\
        \\Arguments:
        \\  plan.yml                 Plan file (default: .git/git-restack/plan.yml)
        \\
        \\Options:
        \\  --worktree-path <path>   Path to worktree (default: ../<repo>-restack)
        \\  -n, --dry-run            Show what would be done without making changes
        \\  -f, --force              Apply even if branches have diverged (dangerous!)
        \\  --cleanup                Remove worktree and -fix branches after applying
        \\  -h, --help               Show this help message
        \\
        \\Safety:
        \\  This command will REFUSE to run if any branch has diverged from the
        \\  commit recorded in the plan. This protects against overwriting work
        \\  that happened after the plan was created.
        \\
        \\  Use --force to override this check (use with caution!).
        \\
        \\Examples:
        \\  git-restack apply --dry-run    # Preview changes
        \\  git-restack apply              # Apply the changes
        \\  git-restack apply --cleanup    # Apply and clean up
        \\
    , .{});
}
