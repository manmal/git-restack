const std = @import("std");
const types = @import("../types.zig");
const stack_mod = @import("../git/stack.zig");

pub fn run(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var json_output = false;
    var base_branch_override: ?[]const u8 = null;

    // Parse options
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--json")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "--base") or std.mem.eql(u8, arg, "--onto")) {
            if (i + 1 < args.len) {
                i += 1;
                base_branch_override = args[i];
            }
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
    }

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
        \\Print the stacked branch hierarchy from HEAD to the base branch (develop/main by default).
        \\
        \\Options:
        \\  --json    Output as JSON instead of tree format
        \\  --base, --onto <branch>  Override base branch (default: develop or main)
        \\  -h, --help    Show this help message
        \\
        \\Exit Codes:
        \\  0 - Success
        \\  1 - Not on a feature branch or invalid stack
        \\
    , .{});
}
