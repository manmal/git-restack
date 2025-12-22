const std = @import("std");
const types = @import("../types.zig");
const process = @import("../utils/process.zig");
const strings = @import("../utils/strings.zig");

pub fn analyzeStack(allocator: std.mem.Allocator) !types.Stack {
    // 1. Get current HEAD branch
    const head_raw = try process.runGit(allocator, &.{ "symbolic-ref", "--short", "HEAD" });
    defer allocator.free(head_raw);
    const head_branch = try strings.copy(allocator, strings.trim(head_raw));

    // 2. Get HEAD commit
    const head_commit_raw = try process.runGit(allocator, &.{ "rev-parse", "HEAD" });
    defer allocator.free(head_commit_raw);
    const head_commit = try strings.copy(allocator, strings.trim(head_commit_raw));

    // 3. Find base (try develop, then main)
    var base_branch: []const u8 = undefined;
    if (checkBranchExists(allocator, "develop")) {
        base_branch = try strings.copy(allocator, "develop");
    } else if (checkBranchExists(allocator, "main")) {
        base_branch = try strings.copy(allocator, "main");
    } else {
        return types.JengaError.BaseBranchNotFound;
    }

    // 4. Get merge-base commit
    const merge_base_raw = try process.runGit(allocator, &.{ "merge-base", "HEAD", base_branch });
    defer allocator.free(merge_base_raw);
    const base_commit = try strings.copy(allocator, strings.trim(merge_base_raw));

    // 5. Get all commits in ancestry path (Base..HEAD)
    // --reverse so we go from Base -> HEAD
    const range = try std.fmt.allocPrint(allocator, "{s}..HEAD", .{base_commit});
    defer allocator.free(range);

    const rev_list_raw = try process.runGit(allocator, &.{
        "rev-list",
        "--ancestry-path",
        "--reverse",
        range,
    });
    defer allocator.free(rev_list_raw);

    var branches = std.ArrayListUnmanaged(types.StackBranch){};
    errdefer branches.deinit(allocator);

    const trimmed = strings.trim(rev_list_raw);
    if (trimmed.len == 0) {
        // No commits between base and HEAD - HEAD is at base
        return types.Stack{
            .branches = try branches.toOwnedSlice(allocator),
            .base_branch = base_branch,
            .base_commit = base_commit,
            .head_branch = head_branch,
            .head_commit = head_commit,
        };
    }

    var commit_iter = std.mem.splitScalar(u8, trimmed, '\n');

    var current_parent_branch: ?[]const u8 = try strings.copy(allocator, base_branch);
    var commits_since_last_branch: u32 = 0;

    // Iterate through every commit from base to head
    while (commit_iter.next()) |commit_sha| {
        if (commit_sha.len == 0) continue;

        commits_since_last_branch += 1;

        // Check if a branch points to this commit
        if (try getBranchAtCommit(allocator, commit_sha)) |branch_name| {
            // Found a branch in the stack!
            try branches.append(allocator, .{
                .name = branch_name,
                .commit_sha = try strings.copy(allocator, commit_sha),
                .parent_branch = current_parent_branch,
                .commits_from_parent = commits_since_last_branch,
            });

            // This branch becomes the parent for the next segment
            current_parent_branch = branch_name;
            commits_since_last_branch = 0;
        }
    }

    return types.Stack{
        .branches = try branches.toOwnedSlice(allocator),
        .base_branch = base_branch,
        .base_commit = base_commit,
        .head_branch = head_branch,
        .head_commit = head_commit,
    };
}

fn checkBranchExists(allocator: std.mem.Allocator, branch: []const u8) bool {
    const result = process.runGit(allocator, &.{ "rev-parse", "--verify", branch }) catch return false;
    allocator.free(result);
    return true;
}

/// Returns the branch name if one exists at this commit.
/// Filters out 'develop', 'main', and HEAD.
fn getBranchAtCommit(allocator: std.mem.Allocator, commit_sha: []const u8) !?[]const u8 {
    const raw = try process.runGit(allocator, &.{ "branch", "--points-at", commit_sha, "--format=%(refname:short)" });
    defer allocator.free(raw);

    var iter = std.mem.splitScalar(u8, strings.trim(raw), '\n');
    while (iter.next()) |b| {
        const branch = strings.trim(b);
        if (branch.len == 0) continue;
        if (std.mem.eql(u8, branch, "develop")) continue;
        if (std.mem.eql(u8, branch, "main")) continue;
        if (std.mem.eql(u8, branch, "HEAD")) continue;

        // Return the first valid feature branch found
        return try strings.copy(allocator, branch);
    }
    return null;
}

/// Print stack as a tree to stdout
pub fn printTree(stack: types.Stack) void {
    const bold = "\x1b[1m";
    const reset = "\x1b[0m";
    const green = "\x1b[32m";
    const blue = "\x1b[34m";
    const dim = "\x1b[2m";

    // Print base
    std.debug.print("{s}{s}{s} {s}({s}){s}\n", .{
        bold,
        stack.base_branch,
        reset,
        dim,
        stack.base_commit[0..@min(7, stack.base_commit.len)],
        reset,
    });

    // Print branches
    for (stack.branches, 0..) |branch, i| {
        const is_last = i == stack.branches.len - 1;
        const prefix: []const u8 = if (is_last) "└── " else "├── ";

        std.debug.print("{s}{s}{s}{s} {s}({s}){s} +{d} commits\n", .{
            prefix,
            green,
            branch.name,
            reset,
            blue,
            branch.commit_sha[0..@min(7, branch.commit_sha.len)],
            reset,
            branch.commits_from_parent,
        });
    }
}

/// Print stack as JSON to stdout
pub fn printJson(allocator: std.mem.Allocator, stack: types.Stack) !void {
    _ = allocator;

    std.debug.print("{{\n", .{});
    std.debug.print("  \"base_branch\": \"{s}\",\n", .{stack.base_branch});
    std.debug.print("  \"base_commit\": \"{s}\",\n", .{stack.base_commit});
    std.debug.print("  \"head_branch\": \"{s}\",\n", .{stack.head_branch});
    std.debug.print("  \"head_commit\": \"{s}\",\n", .{stack.head_commit});
    std.debug.print("  \"branches\": [\n", .{});

    for (stack.branches, 0..) |branch, i| {
        std.debug.print("    {{\n", .{});
        std.debug.print("      \"name\": \"{s}\",\n", .{branch.name});
        std.debug.print("      \"commit\": \"{s}\",\n", .{branch.commit_sha});
        if (branch.parent_branch) |parent| {
            std.debug.print("      \"parent_branch\": \"{s}\",\n", .{parent});
        } else {
            std.debug.print("      \"parent_branch\": null,\n", .{});
        }
        std.debug.print("      \"commits_from_parent\": {d}\n", .{branch.commits_from_parent});

        if (i < stack.branches.len - 1) {
            std.debug.print("    }},\n", .{});
        } else {
            std.debug.print("    }}\n", .{});
        }
    }

    std.debug.print("  ]\n", .{});
    std.debug.print("}}\n", .{});
}
