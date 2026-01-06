const std = @import("std");

pub const fix_branch_prefix = "git-restack/fix/";

pub fn makeFixBranchName(allocator: std.mem.Allocator, original: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "{s}{s}", .{ fix_branch_prefix, original });
}

pub fn isFixBranch(name: []const u8) bool {
    return std.mem.startsWith(u8, name, fix_branch_prefix);
}
