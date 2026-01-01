const std = @import("std");
const types = @import("../types.zig");
const parser = @import("../yaml/parser.zig");

const STATE_FILE = ".git/git-restack/state.json";

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var plan_file: ?[]const u8 = null;

    // Parse options
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            plan_file = arg;
        }
    }

    // Try to read state file
    const state_content = std.fs.cwd().readFileAlloc(allocator, STATE_FILE, 1024 * 1024) catch {
        // No state file - check if plan file was provided
        if (plan_file) |pf| {
            try showPlanStatus(allocator, pf);
        } else {
            std.debug.print("No execution in progress.\n", .{});
            std.debug.print("Run 'git-restack plan' to generate a plan.\n", .{});
        }
        return;
    };
    defer allocator.free(state_content);

    const state = parser.parseState(allocator, state_content) catch {
        std.debug.print("Error: Could not parse state file.\n", .{});
        std.process.exit(1);
    };

    // Show state
    std.debug.print("\n\x1b[1mExecution Status\x1b[0m\n", .{});
    std.debug.print("────────────────────────────────────\n\n", .{});

    std.debug.print("Plan file:    {s}\n", .{state.plan_file});
    std.debug.print("Worktree:     {s}\n", .{state.worktree_path});
    std.debug.print("Started:      {s}\n", .{state.started_at});
    std.debug.print("Last updated: {s}\n", .{state.last_updated});

    const status_color: []const u8 = switch (state.status) {
        .completed => "\x1b[32m",
        .conflict, .failed => "\x1b[31m",
        .in_progress => "\x1b[33m",
        else => "",
    };
    std.debug.print("Status:       {s}{s}\x1b[0m\n", .{ status_color, state.status.toString() });
    std.debug.print("Progress:     {d} branches completed\n", .{state.completed_branches.len});

    if (state.completed_branches.len > 0) {
        std.debug.print("\nCompleted branches:\n", .{});
        for (state.completed_branches) |branch| {
            std.debug.print("  ✓ {s}\n", .{branch});
        }
    }

    std.debug.print("\n", .{});

    // Show next action based on status
    switch (state.status) {
        .conflict => {
            std.debug.print("\x1b[33mAction required:\x1b[0m Resolve conflicts in worktree.\n", .{});
            std.debug.print("  cd {s}\n", .{state.worktree_path});
            std.debug.print("  git add <resolved_files>\n", .{});
            std.debug.print("  git cherry-pick --continue\n", .{});
            std.debug.print("  cd -\n", .{});
            std.debug.print("  git-restack exec {s} --continue\n", .{state.plan_file});
        },
        .in_progress => {
            std.debug.print("Execution in progress.\n", .{});
            std.debug.print("Resume with: git-restack exec {s} --continue\n", .{state.plan_file});
            std.debug.print("Or abort:    git-restack exec {s} --abort\n", .{state.plan_file});
        },
        .completed => {
            std.debug.print("\x1b[32mExecution completed successfully!\x1b[0m\n", .{});
        },
        .failed => {
            std.debug.print("\x1b[31mExecution failed.\x1b[0m\n", .{});
            std.debug.print("Abort with: git-restack exec {s} --abort\n", .{state.plan_file});
        },
        else => {},
    }
}

fn showPlanStatus(allocator: std.mem.Allocator, plan_file: []const u8) !void {
    const plan_content = std.fs.cwd().readFileAlloc(allocator, plan_file, 10 * 1024 * 1024) catch {
        std.debug.print("Error: Could not read plan file '{s}'\n", .{plan_file});
        std.process.exit(1);
    };
    defer allocator.free(plan_content);

    const plan = parser.parsePlan(allocator, plan_content) catch {
        std.debug.print("Error: Could not parse plan file.\n", .{});
        std.process.exit(1);
    };

    std.debug.print("\n\x1b[1mPlan Status\x1b[0m\n", .{});
    std.debug.print("────────────────────────────────────\n\n", .{});

    std.debug.print("Plan file:  {s}\n", .{plan_file});
    std.debug.print("Generated:  {s}\n", .{plan.generated});
    std.debug.print("Repository: {s}\n", .{plan.repository});
    std.debug.print("\n", .{});

    // Errors
    if (plan.errors.len > 0) {
        std.debug.print("\x1b[31mErrors: {d}\x1b[0m\n", .{plan.errors.len});
        for (plan.errors) |err| {
            std.debug.print("  • [{s}] {s}\n", .{ err.error_type.toString(), err.path });
        }
        std.debug.print("\n", .{});
    }

    // Stack summary
    std.debug.print("Branches: {d}\n", .{plan.stack.branches.len});
    std.debug.print("Conflicts: {d}\n", .{plan.conflicts.len});

    var needs_fix_count: usize = 0;
    var total_files: usize = 0;

    for (plan.stack.branches) |branch| {
        if (branch.needs_fix) {
            needs_fix_count += 1;
            if (branch.fix) |fix| {
                total_files += fix.files.len;
            }
        }
    }

    std.debug.print("Need fixes: {d}\n", .{needs_fix_count});
    std.debug.print("Total files: {d}\n", .{total_files});

    std.debug.print("\nBranch details:\n", .{});
    for (plan.stack.branches) |branch| {
        const status_icon: []const u8 = if (branch.needs_fix) "📝" else "✓";
        const file_count: usize = if (branch.fix) |fix| fix.files.len else 0;

        if (branch.needs_fix) {
            std.debug.print("  {s} {s} ({d} files)\n", .{ status_icon, branch.name, file_count });
        } else {
            std.debug.print("  {s} {s}\n", .{ status_icon, branch.name });
        }
    }

    std.debug.print("\n", .{});

    if (plan.errors.len > 0) {
        std.debug.print("\x1b[33mFix errors in plan before executing.\x1b[0m\n", .{});
    } else if (needs_fix_count > 0) {
        std.debug.print("Ready to execute: git-restack exec {s}\n", .{plan_file});
    } else {
        std.debug.print("No changes needed.\n", .{});
    }
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack status [plan.yml]
        \\
        \\Shows execution status. Reads from .git/git-restack/state.json if no plan specified.
        \\
        \\Options:
        \\  -h, --help    Show this help message
        \\
    , .{});
}
