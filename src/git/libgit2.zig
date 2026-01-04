const std = @import("std");

pub const c = @cImport({
    @cInclude("git2.h");
});

var git_once = std.once(initGit);

fn initGit() void {
    _ = c.git_libgit2_init();
}

pub fn ensureInit() void {
    git_once.call();
}

pub fn shutdown() void {
    _ = c.git_libgit2_shutdown();
}

pub fn toCString(allocator: std.mem.Allocator, value: []const u8) ![:0]const u8 {
    return std.mem.concatWithSentinel(allocator, u8, &[_][]const u8{value}, 0);
}

pub fn lastErrorMessage(allocator: std.mem.Allocator) ![]const u8 {
    const err = c.git_error_last() orelse return allocator.alloc(u8, 0);
    const msg = err.*.message orelse return allocator.alloc(u8, 0);
    return allocator.dupe(u8, std.mem.span(msg));
}

pub fn oidToHex(allocator: std.mem.Allocator, oid: *const c.git_oid) ![]const u8 {
    const raw = c.git_oid_tostr_s(oid) orelse return allocator.alloc(u8, 0);
    return allocator.dupe(u8, std.mem.span(raw));
}
