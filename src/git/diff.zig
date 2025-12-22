const std = @import("std");
const types = @import("../types.zig");
const process = @import("../utils/process.zig");
const strings = @import("../utils/strings.zig");

/// Information about a changed file
pub const ChangedFile = struct {
    path: []const u8,
    change_type: types.ChangeType,
    staged: bool,
    /// The commit that last touched this file (if known)
    last_commit: ?[]const u8,
};

/// Get all staged files
pub fn getStagedFiles(allocator: std.mem.Allocator) ![]ChangedFile {
    var files: std.ArrayListUnmanaged(ChangedFile) = .{};
    errdefer files.deinit(allocator);

    // Get staged modified/deleted files
    const diff_result = process.runGitWithStatus(allocator, &.{
        "diff",
        "--cached",
        "--name-status",
    }) catch return files.toOwnedSlice(allocator);
    defer allocator.free(diff_result.stdout);
    defer allocator.free(diff_result.stderr);

    if (diff_result.exit_code != 0) {
        return files.toOwnedSlice(allocator);
    }

    var lines = std.mem.splitScalar(u8, strings.trim(diff_result.stdout), '\n');
    while (lines.next()) |line| {
        if (line.len < 2) continue;

        const status = line[0];
        const path_start: usize = if (line.len > 1 and line[1] == '\t') 2 else 1;
        var path = strings.trim(line[path_start..]);

        // Handle tab-separated format
        if (std.mem.indexOfScalar(u8, path, '\t')) |tab_idx| {
            path = strings.trim(path[0..tab_idx]);
        }

        if (path.len == 0) continue;

        const change_type: types.ChangeType = switch (status) {
            'A' => .added,
            'D' => .deleted,
            'M' => .modified,
            else => .modified,
        };

        try files.append(allocator, .{
            .path = try strings.copy(allocator, path),
            .change_type = change_type,
            .staged = true,
            .last_commit = null,
        });
    }

    return files.toOwnedSlice(allocator);
}

/// Get all unstaged files (modified/deleted in working tree)
pub fn getUnstagedFiles(allocator: std.mem.Allocator) ![]ChangedFile {
    var files: std.ArrayListUnmanaged(ChangedFile) = .{};
    errdefer files.deinit(allocator);

    const diff_result = process.runGitWithStatus(allocator, &.{
        "diff",
        "--name-status",
    }) catch return files.toOwnedSlice(allocator);
    defer allocator.free(diff_result.stdout);
    defer allocator.free(diff_result.stderr);

    if (diff_result.exit_code != 0) {
        return files.toOwnedSlice(allocator);
    }

    var lines = std.mem.splitScalar(u8, strings.trim(diff_result.stdout), '\n');
    while (lines.next()) |line| {
        if (line.len < 2) continue;

        const status = line[0];
        const path_start: usize = if (line.len > 1 and line[1] == '\t') 2 else 1;
        var path = strings.trim(line[path_start..]);

        if (std.mem.indexOfScalar(u8, path, '\t')) |tab_idx| {
            path = strings.trim(path[0..tab_idx]);
        }

        if (path.len == 0) continue;

        const change_type: types.ChangeType = switch (status) {
            'A' => .added,
            'D' => .deleted,
            'M' => .modified,
            else => .modified,
        };

        try files.append(allocator, .{
            .path = try strings.copy(allocator, path),
            .change_type = change_type,
            .staged = false,
            .last_commit = null,
        });
    }

    return files.toOwnedSlice(allocator);
}

/// Get the diff content for a specific file
pub fn getFileDiff(allocator: std.mem.Allocator, path: []const u8, staged: bool) ![]const u8 {
    var args: std.ArrayListUnmanaged([]const u8) = .{};
    defer args.deinit(allocator);

    try args.append(allocator, "diff");
    if (staged) {
        try args.append(allocator, "--cached");
    }
    try args.append(allocator, "--");
    try args.append(allocator, path);

    const result = process.runGitWithStatus(allocator, args.items) catch {
        return strings.copy(allocator, "");
    };
    defer allocator.free(result.stderr);

    if (result.exit_code != 0) {
        allocator.free(result.stdout);
        return strings.copy(allocator, "");
    }

    return result.stdout;
}

/// Find the last commit that touched a file
pub fn getLastCommitForFile(allocator: std.mem.Allocator, path: []const u8) !?[]const u8 {
    const result = process.runGitWithStatus(allocator, &.{
        "log",
        "-1",
        "--format=%H",
        "--",
        path,
    }) catch return null;
    defer allocator.free(result.stderr);

    if (result.exit_code != 0 or result.stdout.len == 0) {
        allocator.free(result.stdout);
        return null;
    }

    const trimmed = strings.trim(result.stdout);
    if (trimmed.len == 0) {
        allocator.free(result.stdout);
        return null;
    }

    const commit = try strings.copy(allocator, trimmed);
    allocator.free(result.stdout);
    return commit;
}

/// Check if a commit is in the ancestry path between base and HEAD
pub fn isCommitInAncestry(allocator: std.mem.Allocator, commit: []const u8, base_commit: []const u8) !bool {
    const range = try std.fmt.allocPrint(allocator, "{s}..HEAD", .{base_commit});
    defer allocator.free(range);

    const result = process.runGitWithStatus(allocator, &.{
        "rev-list",
        "--ancestry-path",
        range,
    }) catch return false;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.exit_code != 0) {
        return false;
    }

    var lines = std.mem.splitScalar(u8, result.stdout, '\n');
    while (lines.next()) |line| {
        if (std.mem.eql(u8, strings.trim(line), commit)) {
            return true;
        }
    }

    return false;
}

/// Find which branch in the stack a commit belongs to
pub fn findBranchForCommit(allocator: std.mem.Allocator, commit: []const u8, stack: types.Stack) !?[]const u8 {
    // Check each branch to see if the commit is an ancestor of it
    // We go from the bottom of the stack up
    for (stack.branches) |branch| {
        // Check if commit is an ancestor of or equal to this branch's commit
        const result = process.runGitWithStatus(allocator, &.{
            "merge-base",
            "--is-ancestor",
            commit,
            branch.commit_sha,
        }) catch continue;
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        if (result.exit_code == 0) {
            // commit is an ancestor of branch, so it belongs to this branch or an earlier one
            // Check if it's after the parent
            if (branch.parent_branch) |parent| {
                // Get parent's commit
                const parent_commit = getParentCommit(allocator, parent, stack) catch continue;
                defer if (parent_commit) |pc| allocator.free(pc);

                if (parent_commit) |pc| {
                    const is_after_parent = process.runGitWithStatus(allocator, &.{
                        "merge-base",
                        "--is-ancestor",
                        pc,
                        commit,
                    }) catch continue;
                    defer allocator.free(is_after_parent.stdout);
                    defer allocator.free(is_after_parent.stderr);

                    if (is_after_parent.exit_code == 0) {
                        // commit is after parent, so it belongs to this branch
                        return try strings.copy(allocator, branch.name);
                    }
                }
            } else {
                // No parent, so this is the first branch after base
                return try strings.copy(allocator, branch.name);
            }
        }
    }

    return null;
}

fn getParentCommit(allocator: std.mem.Allocator, parent_name: []const u8, stack: types.Stack) !?[]const u8 {
    // Check if it's the base branch
    if (std.mem.eql(u8, parent_name, stack.base_branch)) {
        return try strings.copy(allocator, stack.base_commit);
    }

    // Find in stack
    for (stack.branches) |branch| {
        if (std.mem.eql(u8, branch.name, parent_name)) {
            return try strings.copy(allocator, branch.commit_sha);
        }
    }

    return null;
}
