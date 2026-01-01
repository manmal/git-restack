const std = @import("std");
const cli_stack = @import("cli/stack.zig");
const cli_plan = @import("cli/plan.zig");
const cli_exec = @import("cli/exec.zig");
const cli_step = @import("cli/step.zig");
const cli_apply = @import("cli/apply.zig");
const cli_cleanup = @import("cli/cleanup.zig");
const cli_nuke = @import("cli/nuke.zig");
const cli_status = @import("cli/status.zig");

const VERSION = "0.1.0";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const command = args[1];

    // Global options
    if (std.mem.eql(u8, command, "-h") or std.mem.eql(u8, command, "--help")) {
        printUsage();
        return;
    }

    if (std.mem.eql(u8, command, "-v") or std.mem.eql(u8, command, "--version")) {
        std.debug.print("git-restack {s}\n", .{VERSION});
        return;
    }

    // Commands
    const sub_args = if (args.len > 2) args[2..] else &[_][]const u8{};

    if (std.mem.eql(u8, command, "stack")) {
        try cli_stack.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "plan")) {
        try cli_plan.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "exec")) {
        try cli_exec.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "step")) {
        try cli_step.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "apply")) {
        try cli_apply.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "cleanup")) {
        try cli_cleanup.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "nuke")) {
        try cli_nuke.run(allocator, sub_args);
    } else if (std.mem.eql(u8, command, "status")) {
        try cli_status.run(allocator, sub_args);
    } else {
        std.debug.print("Unknown command: {s}\n\n", .{command});
        printUsage();
        std.process.exit(1);
    }
}

fn printUsage() void {
    std.debug.print(
        \\git-restack - Strict stacked branch restacking tool
        \\
        \\Usage: git-restack <command> [options]
        \\
        \\Commands:
        \\  stack     Show the stacked branch hierarchy
        \\  plan      Generate a restacking plan from staged/unstaged changes
        \\  exec      Execute the plan (creates -fix branches in worktree)
        \\  step      Execute ONE step of the plan (for debugging/manual control)
        \\  apply     Apply changes (reset original branches to -fix branches)
        \\  cleanup   Remove worktree and -fix branches
        \\  nuke      Remove ALL git-restack state (emergency reset)
        \\  status    Show current execution status
        \\
        \\Options:
        \\  -h, --help      Show help
        \\  -v, --version   Show version
        \\
        \\Examples:
        \\  git-restack stack                              # Show branch hierarchy
        \\  git-restack plan                               # Generate plan from changes
        \\  git-restack plan --verify "make test"          # Plan with verification
        \\  git-restack plan --verify-only "make test"     # Verify-only (no changes)
        \\  git-restack exec --force                       # Execute the plan
        \\  git-restack step --force                       # Execute one step at a time
        \\  git-restack apply                              # Apply to original branches
        \\  git-restack apply --cleanup                    # Apply and clean up worktree
        \\
        \\For command-specific help:
        \\  git-restack <command> --help
        \\
    , .{});
}

// Include modules for compilation
comptime {
    _ = @import("types.zig");
    _ = @import("utils/strings.zig");
    _ = @import("utils/process.zig");
    _ = @import("git/stack.zig");
    _ = @import("git/diff.zig");
    _ = @import("yaml/emitter.zig");
    _ = @import("yaml/parser.zig");
    _ = @import("cli/stack.zig");
    _ = @import("cli/plan.zig");
    _ = @import("cli/exec.zig");
    _ = @import("cli/step.zig");
    _ = @import("cli/apply.zig");
    _ = @import("cli/cleanup.zig");
    _ = @import("cli/nuke.zig");
    _ = @import("cli/status.zig");
}

test {
    @import("std").testing.refAllDecls(@This());
}
