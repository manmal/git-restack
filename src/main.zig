const std = @import("std");
const cli_stack = @import("cli/stack.zig");
const cli_plan = @import("cli/plan.zig");
const cli_exec = @import("cli/exec.zig");
const cli_status = @import("cli/status.zig");

const VERSION = "0.1.0";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
        std.debug.print("git-jenga {s}\n", .{VERSION});
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
        \\git-jenga - Strict stacked branch restacking tool
        \\
        \\Usage: git-jenga <command> [options]
        \\
        \\Commands:
        \\  stack     Print the stacked branch hierarchy
        \\  plan      Generate a restacking plan from staged/unstaged changes
        \\  exec      Execute a restacking plan
        \\  status    Show current execution status of a plan
        \\
        \\Options:
        \\  -h, --help      Show help
        \\  -v, --version   Show version
        \\
        \\Examples:
        \\  git-jenga stack                    # Show branch hierarchy
        \\  git-jenga plan                     # Generate plan from changes
        \\  git-jenga exec jenga-plan.yml      # Execute the plan
        \\  git-jenga exec plan.yml --abort    # Abort and clean up
        \\
        \\For command-specific help:
        \\  git-jenga <command> --help
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
    _ = @import("cli/status.zig");
}

test {
    @import("std").testing.refAllDecls(@This());
}
