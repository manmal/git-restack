const std = @import("std");
const types = @import("../types.zig");

/// Result of running a git command
pub const GitResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
};

/// Runs a git command and returns stdout. Caller owns the memory.
/// Fails strictly if git returns non-zero exit code.
pub fn runGit(allocator: std.mem.Allocator, args: []const []const u8) ![]const u8 {
    const result = try runGitWithStatus(allocator, args);
    defer allocator.free(result.stderr);

    if (result.exit_code != 0) {
        if (result.stderr.len > 0) {
            std.debug.print("git error: {s}\n", .{result.stderr});
        }
        allocator.free(result.stdout);
        return types.JengaError.GitCommandFailed;
    }

    return result.stdout;
}

/// Runs a git command and returns full result including exit code.
/// Caller owns both stdout and stderr memory.
pub fn runGitWithStatus(allocator: std.mem.Allocator, args: []const []const u8) !GitResult {
    // Build args array: ["git"] + args
    const full_args = try allocator.alloc([]const u8, args.len + 1);
    defer allocator.free(full_args);

    full_args[0] = "git";
    for (args, 0..) |arg, i| {
        full_args[i + 1] = arg;
    }

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = full_args,
        .max_output_bytes = 10 * 1024 * 1024,
    }) catch |err| {
        std.debug.print("Failed to run git: {any}\n", .{err});
        return types.JengaError.ProcessError;
    };

    const exit_code: u8 = switch (result.term) {
        .Exited => |code| code,
        else => 1,
    };

    return GitResult{
        .stdout = result.stdout,
        .stderr = result.stderr,
        .exit_code = exit_code,
    };
}

/// Run a git command in a specific directory
pub fn runGitInDir(allocator: std.mem.Allocator, dir: []const u8, args: []const []const u8) ![]const u8 {
    // Build args array: ["git", "-C", dir] + args
    const full_args = try allocator.alloc([]const u8, args.len + 3);
    defer allocator.free(full_args);

    full_args[0] = "git";
    full_args[1] = "-C";
    full_args[2] = dir;
    for (args, 0..) |arg, i| {
        full_args[i + 3] = arg;
    }

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = full_args,
        .max_output_bytes = 10 * 1024 * 1024,
    }) catch |err| {
        std.debug.print("Failed to run git: {any}\n", .{err});
        return types.JengaError.ProcessError;
    };
    defer allocator.free(result.stderr);

    const exit_code: u8 = switch (result.term) {
        .Exited => |code| code,
        else => 1,
    };

    if (exit_code != 0) {
        if (result.stderr.len > 0) {
            std.debug.print("git error: {s}\n", .{result.stderr});
        }
        allocator.free(result.stdout);
        return types.JengaError.GitCommandFailed;
    }

    return result.stdout;
}

/// Get current working directory
pub fn getCwd(allocator: std.mem.Allocator) ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = try std.fs.cwd().realpath(".", &buf);
    return allocator.dupe(u8, cwd);
}
