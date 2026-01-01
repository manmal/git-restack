const std = @import("std");
const types = @import("../types.zig");
const stack_mod = @import("../git/stack.zig");

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var json_output = false;

    // Parse options
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
    }

    var stack = stack_mod.analyzeStack(allocator) catch |err| {
        switch (err) {
            types.RestackError.GitCommandFailed => {
                std.debug.print("Error: Git command failed. Are you in a git repository?\n", .{});
            },
            types.RestackError.BaseBranchNotFound => {
                std.debug.print("Error: Could not find 'develop' or 'main' base branch.\n", .{});
            },
            else => {
                std.debug.print("Error: {any}\n", .{err});
            },
        }
        std.process.exit(1);
    };
    defer stack.deinit(allocator);

    if (json_output) {
        try stack_mod.printJson(allocator, stack);
    } else {
        stack_mod.printTree(stack);
    }
}

fn printHelp() void {
    std.debug.print(
        \\Usage: git-restack stack [OPTIONS]
        \\
        \\Print the stacked branch hierarchy from HEAD to develop/main.
        \\
        \\Options:
        \\  --json    Output as JSON instead of tree format
        \\  -h, --help    Show this help message
        \\
        \\Exit Codes:
        \\  0 - Success
        \\  1 - Not on a feature branch or invalid stack
        \\
    , .{});
}
