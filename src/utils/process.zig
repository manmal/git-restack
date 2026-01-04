const std = @import("std");
const types = @import("../types.zig");
const git = @import("../git/libgit2.zig");

/// Result of running a git command
pub const GitResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    exit_code: u8,
};

const GitCommand = struct {
    repo_path: ?[]const u8,
    command: []const u8,
    args: []const []const u8,
    owned: []const []const u8,

    pub fn deinit(self: *GitCommand, allocator: std.mem.Allocator) void {
        allocator.free(self.owned);
    }
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
        return types.RestackError.GitCommandFailed;
    }

    return result.stdout;
}

/// Runs a git command and returns full result including exit code.
/// Caller owns both stdout and stderr memory.
pub fn runGitWithStatus(allocator: std.mem.Allocator, args: []const []const u8) !GitResult {
    var cmd = try parseGitArgs(allocator, args);
    defer cmd.deinit(allocator);

    return runGitCommand(allocator, cmd);
}

/// Runs a git command with rerere explicitly disabled.
pub fn runGitWithStatusNoRerere(allocator: std.mem.Allocator, args: []const []const u8) !GitResult {
    const full_args = try allocator.alloc([]const u8, args.len + 4);
    defer allocator.free(full_args);

    full_args[0] = "-c";
    full_args[1] = "rerere.enabled=false";
    full_args[2] = "-c";
    full_args[3] = "rerere.autoUpdate=false";
    for (args, 0..) |arg, i| {
        full_args[i + 4] = arg;
    }

    return runGitWithStatus(allocator, full_args);
}

/// Run a git command in a specific directory
pub fn runGitInDir(allocator: std.mem.Allocator, dir: []const u8, args: []const []const u8) ![]const u8 {
    const full_args = try allocator.alloc([]const u8, args.len + 2);
    defer allocator.free(full_args);

    full_args[0] = "-C";
    full_args[1] = dir;
    for (args, 0..) |arg, i| {
        full_args[i + 2] = arg;
    }

    return runGit(allocator, full_args);
}

/// Get current working directory
pub fn getCwd(allocator: std.mem.Allocator) ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = try std.fs.cwd().realpath(".", &buf);
    return allocator.dupe(u8, cwd);
}

fn parseGitArgs(allocator: std.mem.Allocator, args: []const []const u8) !GitCommand {
    var repo_path: ?[]const u8 = null;
    var cleaned: std.ArrayListUnmanaged([]const u8) = .{};
    defer cleaned.deinit(allocator);

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-C")) {
            if (i + 1 >= args.len) return types.RestackError.ParseError;
            i += 1;
            repo_path = args[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "-c")) {
            i += 1;
            if (i >= args.len) return types.RestackError.ParseError;
            continue;
        }
        try cleaned.append(allocator, arg);
    }

    if (cleaned.items.len == 0) return types.RestackError.ParseError;

    const owned = try cleaned.toOwnedSlice(allocator);
    return GitCommand{
        .repo_path = repo_path,
        .command = owned[0],
        .args = if (owned.len > 1) owned[1..] else &[_][]const u8{},
        .owned = owned,
    };
}

fn runGitCommand(allocator: std.mem.Allocator, cmd: GitCommand) !GitResult {
    const repo = try openRepository(allocator, cmd.repo_path);
    defer git.c.git_repository_free(repo);

    const workdir_ptr = git.c.git_repository_workdir(repo);
    const workdir = if (workdir_ptr == null) null else std.mem.span(workdir_ptr);

    if (std.mem.eql(u8, cmd.command, "symbolic-ref")) {
        return cmdSymbolicRef(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "rev-parse")) {
        return cmdRevParse(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "merge-base")) {
        return cmdMergeBase(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "rev-list")) {
        return cmdRevList(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "branch")) {
        return cmdBranch(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "worktree")) {
        return cmdWorktree(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "checkout")) {
        return cmdCheckout(allocator, repo, workdir, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "status")) {
        return cmdStatus(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "diff")) {
        return cmdDiff(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "log")) {
        return cmdLog(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "show")) {
        return cmdShow(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "cherry-pick")) {
        return cmdCherryPick(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "apply")) {
        return cmdApply(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "add")) {
        return cmdAdd(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "commit")) {
        return cmdCommit(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "rm")) {
        return cmdRm(allocator, repo, workdir, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "update-index")) {
        return cmdUpdateIndex(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "ls-files")) {
        return cmdLsFiles(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "stash")) {
        return cmdStash(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "for-each-ref")) {
        return cmdForEachRef(allocator, repo, cmd.args);
    } else if (std.mem.eql(u8, cmd.command, "mergetool")) {
        return cmdMergetool(allocator, repo, workdir, cmd.args);
    }

    return types.RestackError.GitCommandFailed;
}

fn openRepository(allocator: std.mem.Allocator, path: ?[]const u8) !*git.c.git_repository {
    git.ensureInit();

    var repo: ?*git.c.git_repository = null;
    if (path) |p| {
        const c_path = try git.toCString(allocator, p);
        defer allocator.free(c_path);
        if (git.c.git_repository_open_ext(&repo, c_path, 0, null) < 0 or repo == null) {
            return types.RestackError.GitCommandFailed;
        }
        return repo.?;
    }

    if (git.c.git_repository_open_ext(&repo, null, git.c.GIT_REPOSITORY_OPEN_FROM_ENV, null) < 0 or repo == null) {
        return types.RestackError.GitCommandFailed;
    }
    return repo.?;
}

fn allocEmpty(allocator: std.mem.Allocator) ![]const u8 {
    return allocator.alloc(u8, 0);
}

fn gitErrorResult(allocator: std.mem.Allocator) !GitResult {
    return .{
        .stdout = try allocEmpty(allocator),
        .stderr = try git.lastErrorMessage(allocator),
        .exit_code = 1,
    };
}

fn okResult(allocator: std.mem.Allocator, stdout: []const u8) !GitResult {
    return .{
        .stdout = stdout,
        .stderr = try allocEmpty(allocator),
        .exit_code = 0,
    };
}

fn oidFromSpec(repo: *git.c.git_repository, allocator: std.mem.Allocator, spec: []const u8) !git.c.git_oid {
    const c_spec = try git.toCString(allocator, spec);
    defer allocator.free(c_spec);

    var obj: ?*git.c.git_object = null;
    if (git.c.git_revparse_single(&obj, repo, c_spec) < 0 or obj == null) {
        return types.RestackError.GitCommandFailed;
    }
    defer git.c.git_object_free(obj.?);

    const oid = git.c.git_object_id(obj.?) orelse return types.RestackError.GitCommandFailed;
    return oid.*;
}

fn commitFromSpec(repo: *git.c.git_repository, allocator: std.mem.Allocator, spec: []const u8) !*git.c.git_commit {
    const oid = try oidFromSpec(repo, allocator, spec);
    var commit: ?*git.c.git_commit = null;
    if (git.c.git_commit_lookup(&commit, repo, &oid) < 0 or commit == null) {
        return types.RestackError.GitCommandFailed;
    }
    return commit.?;
}

fn signatureForRepo(repo: *git.c.git_repository) !*git.c.git_signature {
    var sig: ?*git.c.git_signature = null;
    if (git.c.git_signature_default(&sig, repo) == 0 and sig != null) {
        return sig.?;
    }
    if (git.c.git_signature_now(&sig, "git-restack", "git-restack@local") == 0 and sig != null) {
        return sig.?;
    }
    return types.RestackError.GitCommandFailed;
}

fn cmdSymbolicRef(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    _ = args;
    var head_ref: ?*git.c.git_reference = null;
    if (git.c.git_repository_head(&head_ref, repo) < 0 or head_ref == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_reference_free(head_ref.?);

    if (git.c.git_reference_type(head_ref.?) != git.c.GIT_REFERENCE_SYMBOLIC) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocator.dupe(u8, "HEAD is detached"), .exit_code = 1 };
    }

    const name_ptr = git.c.git_reference_shorthand(head_ref.?) orelse return gitErrorResult(allocator);
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, std.mem.span(name_ptr));
    try out.append(allocator, '\n');
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdRevParse(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var spec: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--verify")) continue;
        if (spec == null) spec = arg;
    }
    if (spec == null) return types.RestackError.ParseError;

    const c_spec = try git.toCString(allocator, spec.?);
    defer allocator.free(c_spec);

    var obj: ?*git.c.git_object = null;
    if (git.c.git_revparse_single(&obj, repo, c_spec) < 0 or obj == null) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try git.lastErrorMessage(allocator), .exit_code = 1 };
    }
    defer git.c.git_object_free(obj.?);

    const oid = git.c.git_object_id(obj.?) orelse return gitErrorResult(allocator);
    const hex = try git.oidToHex(allocator, oid);
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, hex);
    try out.append(allocator, '\n');
    allocator.free(hex);
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdMergeBase(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len >= 3 and std.mem.eql(u8, args[0], "--is-ancestor")) {
        const ancestor = oidFromSpec(repo, allocator, args[1]) catch return gitErrorResult(allocator);
        const descendant = oidFromSpec(repo, allocator, args[2]) catch return gitErrorResult(allocator);
        if (git.c.git_oid_equal(&ancestor, &descendant) != 0) {
            return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
        }
        const rc = git.c.git_graph_descendant_of(repo, &descendant, &ancestor);
        if (rc < 0) return gitErrorResult(allocator);
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = if (rc == 1) 0 else 1 };
    }

    if (args.len < 2) return types.RestackError.ParseError;
    const a = oidFromSpec(repo, allocator, args[0]) catch return gitErrorResult(allocator);
    const b = oidFromSpec(repo, allocator, args[1]) catch return gitErrorResult(allocator);

    var base_oid: git.c.git_oid = undefined;
    if (git.c.git_merge_base(&base_oid, repo, &a, &b) < 0) {
        return gitErrorResult(allocator);
    }
    const hex = try git.oidToHex(allocator, &base_oid);
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, hex);
    try out.append(allocator, '\n');
    allocator.free(hex);
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdRevList(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var reverse = false;
    var ancestry = false;
    var range: ?[]const u8 = null;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--reverse")) {
            reverse = true;
        } else if (std.mem.eql(u8, arg, "--ancestry-path")) {
            ancestry = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            range = arg;
        }
    }

    if (range == null) return types.RestackError.ParseError;

    const c_range = try git.toCString(allocator, range.?);
    defer allocator.free(c_range);

    var revspec: git.c.git_revspec = undefined;
    if (git.c.git_revparse(&revspec, repo, c_range) < 0) {
        return gitErrorResult(allocator);
    }
    defer {
        if (revspec.from) |from| git.c.git_object_free(from);
        if (revspec.to) |to| git.c.git_object_free(to);
    }

    if (revspec.from == null or revspec.to == null) {
        return gitErrorResult(allocator);
    }

    const from_oid = git.c.git_object_id(revspec.from.?).*;
    const to_oid = git.c.git_object_id(revspec.to.?).*;

    var walk: ?*git.c.git_revwalk = null;
    if (git.c.git_revwalk_new(&walk, repo) < 0 or walk == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_revwalk_free(walk.?);

    var sort_flags: c_uint = git.c.GIT_SORT_TOPOLOGICAL;
    if (reverse) sort_flags |= git.c.GIT_SORT_REVERSE;
    _ = git.c.git_revwalk_sorting(walk.?, sort_flags);
    _ = git.c.git_revwalk_push(walk.?, &to_oid);
    _ = git.c.git_revwalk_hide(walk.?, &from_oid);

    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    var oid: git.c.git_oid = undefined;
    while (git.c.git_revwalk_next(&oid, walk.?) == 0) {
        if (ancestry) {
            const rc = git.c.git_graph_descendant_of(repo, &oid, &from_oid);
            if (rc != 1) continue;
        }

        const hex = try git.oidToHex(allocator, &oid);
        try out.appendSlice(allocator, hex);
        try out.append(allocator, '\n');
        allocator.free(hex);
    }

    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdBranch(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len >= 2 and std.mem.eql(u8, args[0], "--points-at")) {
        const target_oid = oidFromSpec(repo, allocator, args[1]) catch return gitErrorResult(allocator);

        var iter: ?*git.c.git_branch_iterator = null;
        if (git.c.git_branch_iterator_new(&iter, repo, git.c.GIT_BRANCH_LOCAL) < 0 or iter == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_branch_iterator_free(iter.?);

        var out: std.ArrayListUnmanaged(u8) = .{};
        defer out.deinit(allocator);

        while (true) {
            var ref: ?*git.c.git_reference = null;
            var branch_type: git.c.git_branch_t = undefined;
            const rc = git.c.git_branch_next(&ref, &branch_type, iter.?);
            if (rc == git.c.GIT_ITEROVER) break;
            if (rc < 0 or ref == null) return gitErrorResult(allocator);

            const oid_ptr = git.c.git_reference_target(ref.?);
            if (oid_ptr != null and git.c.git_oid_equal(oid_ptr, &target_oid) != 0) {
                const name_ptr = git.c.git_reference_shorthand(ref.?) orelse {
                    git.c.git_reference_free(ref.?);
                    continue;
                };
                try out.appendSlice(allocator, std.mem.span(name_ptr));
                try out.append(allocator, '\n');
            }
            git.c.git_reference_free(ref.?);
        }

        return okResult(allocator, try out.toOwnedSlice(allocator));
    }

    if (args.len >= 1 and std.mem.eql(u8, args[0], "--list")) {
        const pattern = if (args.len >= 2) args[1] else "*";
        const head_name = headBranchName(allocator, repo) catch null;
        defer if (head_name) |name| allocator.free(name);

        var iter: ?*git.c.git_branch_iterator = null;
        if (git.c.git_branch_iterator_new(&iter, repo, git.c.GIT_BRANCH_LOCAL) < 0 or iter == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_branch_iterator_free(iter.?);

        var out: std.ArrayListUnmanaged(u8) = .{};
        defer out.deinit(allocator);

        while (true) {
            var ref: ?*git.c.git_reference = null;
            var branch_type: git.c.git_branch_t = undefined;
            const rc = git.c.git_branch_next(&ref, &branch_type, iter.?);
            if (rc == git.c.GIT_ITEROVER) break;
            if (rc < 0 or ref == null) return gitErrorResult(allocator);

            const name_ptr = git.c.git_reference_shorthand(ref.?) orelse {
                git.c.git_reference_free(ref.?);
                continue;
            };
            const name = std.mem.span(name_ptr);
            if (!globMatch(pattern, name)) {
                git.c.git_reference_free(ref.?);
                continue;
            }

            const is_head = if (head_name) |head| std.mem.eql(u8, head, name) else false;
            if (is_head) {
                try out.appendSlice(allocator, "* ");
            } else {
                try out.appendSlice(allocator, "  ");
            }
            try out.appendSlice(allocator, name);
            try out.append(allocator, '\n');
            git.c.git_reference_free(ref.?);
        }

        return okResult(allocator, try out.toOwnedSlice(allocator));
    }

    if (args.len >= 3 and std.mem.eql(u8, args[0], "-f")) {
        const new_branch = args[1];
        const source = args[2];

        const commit = commitFromSpec(repo, allocator, source) catch return gitErrorResult(allocator);
        defer git.c.git_commit_free(commit);

        const c_new = try git.toCString(allocator, new_branch);
        defer allocator.free(c_new);

        var out_ref: ?*git.c.git_reference = null;
        if (git.c.git_branch_create(&out_ref, repo, c_new, commit, 1) < 0 or out_ref == null) {
            return gitErrorResult(allocator);
        }
        git.c.git_reference_free(out_ref.?);

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (args.len >= 2 and std.mem.eql(u8, args[0], "-D")) {
        const branch = args[1];
        const c_branch = try git.toCString(allocator, branch);
        defer allocator.free(c_branch);

        var ref: ?*git.c.git_reference = null;
        if (git.c.git_branch_lookup(&ref, repo, c_branch, git.c.GIT_BRANCH_LOCAL) < 0 or ref == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_reference_free(ref.?);

        if (git.c.git_branch_delete(ref.?) < 0) {
            return gitErrorResult(allocator);
        }

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (args.len >= 2) {
        const new_branch = args[0];
        const source = args[1];

        const commit = commitFromSpec(repo, allocator, source) catch return gitErrorResult(allocator);
        defer git.c.git_commit_free(commit);

        const c_new = try git.toCString(allocator, new_branch);
        defer allocator.free(c_new);

        var out_ref: ?*git.c.git_reference = null;
        if (git.c.git_branch_create(&out_ref, repo, c_new, commit, 0) < 0 or out_ref == null) {
            return gitErrorResult(allocator);
        }
        git.c.git_reference_free(out_ref.?);

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    return types.RestackError.ParseError;
}

fn headBranchName(allocator: std.mem.Allocator, repo: *git.c.git_repository) !?[]const u8 {
    var head_ref: ?*git.c.git_reference = null;
    if (git.c.git_repository_head(&head_ref, repo) < 0 or head_ref == null) {
        return null;
    }
    defer git.c.git_reference_free(head_ref.?);

    if (git.c.git_reference_type(head_ref.?) != git.c.GIT_REFERENCE_SYMBOLIC) {
        return null;
    }

    const name_ptr = git.c.git_reference_shorthand(head_ref.?) orelse return null;
    return allocator.dupe(u8, std.mem.span(name_ptr));
}

fn globMatch(pattern: []const u8, text: []const u8) bool {
    var p: usize = 0;
    var t: usize = 0;
    var star: ?usize = null;
    var match: usize = 0;

    while (t < text.len) {
        if (p < pattern.len and (pattern[p] == text[t] or pattern[p] == '?')) {
            p += 1;
            t += 1;
            continue;
        }
        if (p < pattern.len and pattern[p] == '*') {
            star = p;
            match = t;
            p += 1;
            continue;
        }
        if (star) |s| {
            p = s + 1;
            match += 1;
            t = match;
            continue;
        }
        return false;
    }

    while (p < pattern.len and pattern[p] == '*') : (p += 1) {}
    return p == pattern.len;
}

fn cmdForEachRef(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var refspec: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "--format=")) continue;
        if (refspec == null) refspec = arg;
    }
    if (refspec == null) return types.RestackError.ParseError;

    const c_ref = try git.toCString(allocator, refspec.?);
    defer allocator.free(c_ref);

    var ref: ?*git.c.git_reference = null;
    if (git.c.git_reference_lookup(&ref, repo, c_ref) < 0 or ref == null) {
        return okResult(allocator, try allocEmpty(allocator));
    }
    defer git.c.git_reference_free(ref.?);

    const name_ptr = git.c.git_reference_shorthand(ref.?) orelse return okResult(allocator, try allocEmpty(allocator));
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, std.mem.span(name_ptr));
    try out.append(allocator, '\n');
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdWorktree(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len == 0) return types.RestackError.ParseError;

    if (std.mem.eql(u8, args[0], "add")) {
        if (args.len < 3) return types.RestackError.ParseError;
        const path = args[1];
        const branch = args[2];

        const c_branch = try git.toCString(allocator, branch);
        defer allocator.free(c_branch);

        var ref: ?*git.c.git_reference = null;
        if (git.c.git_branch_lookup(&ref, repo, c_branch, git.c.GIT_BRANCH_LOCAL) < 0 or ref == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_reference_free(ref.?);

        const name = std.fs.path.basename(path);
        const c_name = try git.toCString(allocator, name);
        defer allocator.free(c_name);

        const c_path = try git.toCString(allocator, path);
        defer allocator.free(c_path);

        var opts: git.c.git_worktree_add_options = undefined;
        if (git.c.git_worktree_add_options_init(&opts, git.c.GIT_WORKTREE_ADD_OPTIONS_VERSION) < 0) {
            return gitErrorResult(allocator);
        }
        opts.ref = ref.?;

        var out_wt: ?*git.c.git_worktree = null;
        if (git.c.git_worktree_add(&out_wt, repo, c_name, c_path, &opts) < 0 or out_wt == null) {
            return gitErrorResult(allocator);
        }
        git.c.git_worktree_free(out_wt.?);

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (std.mem.eql(u8, args[0], "remove")) {
        if (args.len < 2) return types.RestackError.ParseError;
        const path = args[1];
        const force = args.len > 2 and std.mem.eql(u8, args[2], "--force");

        var list: git.c.git_strarray = undefined;
        if (git.c.git_worktree_list(&list, repo) < 0) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_strarray_dispose(&list);

        var matched = false;
        for (list.strings[0..list.count]) |name_ptr| {
            const name = std.mem.span(name_ptr);
            var wt: ?*git.c.git_worktree = null;
            const c_name = try git.toCString(allocator, name);
            defer allocator.free(c_name);

            if (git.c.git_worktree_lookup(&wt, repo, c_name) < 0 or wt == null) continue;
            defer git.c.git_worktree_free(wt.?);

            const wt_path_ptr = git.c.git_worktree_path(wt.?);
            if (wt_path_ptr == null) continue;
            const wt_path = std.mem.span(wt_path_ptr);
            if (!pathsEqual(path, wt_path)) continue;
            matched = true;

            var prune_opts: git.c.git_worktree_prune_options = undefined;
            if (git.c.git_worktree_prune_options_init(&prune_opts, git.c.GIT_WORKTREE_PRUNE_OPTIONS_VERSION) < 0) {
                return gitErrorResult(allocator);
            }
            if (force) {
                prune_opts.flags = git.c.GIT_WORKTREE_PRUNE_VALID | git.c.GIT_WORKTREE_PRUNE_LOCKED | git.c.GIT_WORKTREE_PRUNE_WORKING_TREE;
            }
            if (git.c.git_worktree_prune(wt.?, &prune_opts) < 0) {
                return gitErrorResult(allocator);
            }
        }

        if (!matched) {
            return gitErrorResult(allocator);
        }

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (std.mem.eql(u8, args[0], "prune")) {
        var list: git.c.git_strarray = undefined;
        if (git.c.git_worktree_list(&list, repo) < 0) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_strarray_dispose(&list);

        for (list.strings[0..list.count]) |name_ptr| {
            const name = std.mem.span(name_ptr);
            var wt: ?*git.c.git_worktree = null;
            const c_name = try git.toCString(allocator, name);
            defer allocator.free(c_name);

            if (git.c.git_worktree_lookup(&wt, repo, c_name) < 0 or wt == null) continue;
            defer git.c.git_worktree_free(wt.?);

            _ = git.c.git_worktree_prune(wt.?, null);
        }

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    return types.RestackError.ParseError;
}

fn cmdCheckout(allocator: std.mem.Allocator, repo: *git.c.git_repository, workdir: ?[]const u8, args: []const []const u8) !GitResult {
    if (args.len == 0) return types.RestackError.ParseError;

    if (std.mem.eql(u8, args[0], "-b")) {
        if (args.len < 2) return types.RestackError.ParseError;
        const branch = args[1];

        var head_ref: ?*git.c.git_reference = null;
        if (git.c.git_repository_head(&head_ref, repo) < 0 or head_ref == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_reference_free(head_ref.?);

        var head_commit: ?*git.c.git_commit = null;
        if (git.c.git_reference_peel(@ptrCast(&head_commit), head_ref.?, git.c.GIT_OBJECT_COMMIT) < 0 or head_commit == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_commit_free(head_commit.?);

        const c_branch = try git.toCString(allocator, branch);
        defer allocator.free(c_branch);

        var new_ref: ?*git.c.git_reference = null;
        if (git.c.git_branch_create(&new_ref, repo, c_branch, head_commit.?, 0) < 0 or new_ref == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_reference_free(new_ref.?);

        const ref_name = git.c.git_reference_name(new_ref.?) orelse return gitErrorResult(allocator);
        if (git.c.git_repository_set_head(repo, ref_name) < 0) {
            return gitErrorResult(allocator);
        }

        var opts: git.c.git_checkout_options = undefined;
        if (git.c.git_checkout_options_init(&opts, git.c.GIT_CHECKOUT_OPTIONS_VERSION) < 0) {
            return gitErrorResult(allocator);
        }
        opts.checkout_strategy = git.c.GIT_CHECKOUT_SAFE;
        if (git.c.git_checkout_head(repo, &opts) < 0) {
            return gitErrorResult(allocator);
        }

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (std.mem.eql(u8, args[0], "--ours") or std.mem.eql(u8, args[0], "--theirs")) {
        if (args.len < 3) return types.RestackError.ParseError;
        if (!std.mem.eql(u8, args[1], "--")) return types.RestackError.ParseError;
        const path = args[2];
        _ = workdir;

        var index: ?*git.c.git_index = null;
        if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_index_free(index.?);

        var opts: git.c.git_checkout_options = undefined;
        if (git.c.git_checkout_options_init(&opts, git.c.GIT_CHECKOUT_OPTIONS_VERSION) < 0) {
            return gitErrorResult(allocator);
        }
        const side_flag: c_uint = if (std.mem.eql(u8, args[0], "--ours"))
            @as(c_uint, git.c.GIT_CHECKOUT_USE_OURS)
        else
            @as(c_uint, git.c.GIT_CHECKOUT_USE_THEIRS);
        opts.checkout_strategy = @as(c_uint, git.c.GIT_CHECKOUT_FORCE) | side_flag;

        const c_path = try git.toCString(allocator, path);
        defer allocator.free(c_path);
        var paths = [1][*:0]const u8{c_path.ptr};
        opts.paths = .{ .strings = @ptrCast(&paths), .count = 1 };

        if (git.c.git_checkout_index(repo, index.?, &opts) < 0) {
            return gitErrorResult(allocator);
        }

        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    const branch = args[0];
    const c_branch = try git.toCString(allocator, branch);
    defer allocator.free(c_branch);

    var ref: ?*git.c.git_reference = null;
    if (git.c.git_branch_lookup(&ref, repo, c_branch, git.c.GIT_BRANCH_LOCAL) < 0 or ref == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_reference_free(ref.?);

    const ref_name = git.c.git_reference_name(ref.?) orelse return gitErrorResult(allocator);
    if (git.c.git_repository_set_head(repo, ref_name) < 0) {
        return gitErrorResult(allocator);
    }

    var opts: git.c.git_checkout_options = undefined;
    if (git.c.git_checkout_options_init(&opts, git.c.GIT_CHECKOUT_OPTIONS_VERSION) < 0) {
        return gitErrorResult(allocator);
    }
    opts.checkout_strategy = git.c.GIT_CHECKOUT_SAFE;
    if (git.c.git_checkout_head(repo, &opts) < 0) {
        return gitErrorResult(allocator);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdStatus(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    _ = args;
    var opts: git.c.git_status_options = undefined;
    if (git.c.git_status_options_init(&opts, git.c.GIT_STATUS_OPTIONS_VERSION) < 0) {
        return gitErrorResult(allocator);
    }
    opts.show = git.c.GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
    opts.flags = git.c.GIT_STATUS_OPT_INCLUDE_UNTRACKED | git.c.GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX | git.c.GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR;

    var list: ?*git.c.git_status_list = null;
    if (git.c.git_status_list_new(&list, repo, &opts) < 0 or list == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_status_list_free(list.?);

    const count = git.c.git_status_list_entrycount(list.?);
    if (count == 0) {
        return okResult(allocator, try allocEmpty(allocator));
    }

    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    var i: usize = 0;
    while (i < count) : (i += 1) {
        const entry = git.c.git_status_byindex(list.?, i) orelse continue;
        const path = if (entry.*.head_to_index != null and entry.*.head_to_index.*.new_file.path != null)
            std.mem.span(entry.*.head_to_index.*.new_file.path)
        else if (entry.*.index_to_workdir != null and entry.*.index_to_workdir.*.new_file.path != null)
            std.mem.span(entry.*.index_to_workdir.*.new_file.path)
        else
            null;
        if (path) |p| {
            try out.appendSlice(allocator, "M ");
            try out.appendSlice(allocator, p);
            try out.append(allocator, '\n');
        }
    }

    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdDiff(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var cached = false;
    var name_only = false;
    var name_status = false;
    var diff_filter_unmerged = false;
    var binary = false;
    var pathspec: ?[]const u8 = null;
    var seen_sep = false;

    for (args, 0..) |arg, idx| {
        _ = idx;
        if (seen_sep) {
            pathspec = arg;
            break;
        }
        if (std.mem.eql(u8, arg, "--cached")) {
            cached = true;
        } else if (std.mem.eql(u8, arg, "--name-only")) {
            name_only = true;
        } else if (std.mem.eql(u8, arg, "--name-status")) {
            name_status = true;
        } else if (std.mem.eql(u8, arg, "--binary")) {
            binary = true;
        } else if (std.mem.eql(u8, arg, "--diff-filter=U")) {
            diff_filter_unmerged = true;
        } else if (std.mem.eql(u8, arg, "--")) {
            seen_sep = true;
        }
    }

    if (diff_filter_unmerged and name_only) {
        return listConflictedFiles(allocator, repo);
    }

    var diff: ?*git.c.git_diff = null;
    var diff_opts: git.c.git_diff_options = undefined;
    if (git.c.git_diff_options_init(&diff_opts, git.c.GIT_DIFF_OPTIONS_VERSION) < 0) {
        return gitErrorResult(allocator);
    }
    if (binary) diff_opts.flags |= git.c.GIT_DIFF_SHOW_BINARY;

    var path_buf: ?[:0]const u8 = null;
    var path_array: [1][*:0]const u8 = undefined;
    if (pathspec) |path| {
        path_buf = try git.toCString(allocator, path);
        path_array[0] = path_buf.?.ptr;
        diff_opts.pathspec = .{ .strings = @ptrCast(&path_array), .count = 1 };
    }
    defer if (path_buf) |buf| allocator.free(buf);

    if (cached) {
        var head_ref: ?*git.c.git_reference = null;
        var head_commit: ?*git.c.git_commit = null;
        var head_tree: ?*git.c.git_tree = null;

        if (git.c.git_repository_head(&head_ref, repo) == 0 and head_ref != null) {
            if (git.c.git_reference_peel(@ptrCast(&head_commit), head_ref.?, git.c.GIT_OBJECT_COMMIT) == 0 and head_commit != null) {
                _ = git.c.git_commit_tree(&head_tree, head_commit.?);
            }
        }
        if (head_ref) |ref| git.c.git_reference_free(ref);
        if (head_commit) |commit| git.c.git_commit_free(commit);

        var index: ?*git.c.git_index = null;
        if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
            if (head_tree) |tree| git.c.git_tree_free(tree);
            return gitErrorResult(allocator);
        }
        defer git.c.git_index_free(index.?);

        if (git.c.git_diff_tree_to_index(&diff, repo, head_tree, index.?, &diff_opts) < 0 or diff == null) {
            if (head_tree) |tree| git.c.git_tree_free(tree);
            return gitErrorResult(allocator);
        }
        if (head_tree) |tree| git.c.git_tree_free(tree);
    } else {
        var index: ?*git.c.git_index = null;
        if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_index_free(index.?);

        if (git.c.git_diff_index_to_workdir(&diff, repo, index.?, &diff_opts) < 0 or diff == null) {
            return gitErrorResult(allocator);
        }
    }
    defer git.c.git_diff_free(diff.?);

    const format: git.c.git_diff_format_t = if (name_only)
        git.c.GIT_DIFF_FORMAT_NAME_ONLY
    else if (name_status)
        git.c.GIT_DIFF_FORMAT_NAME_STATUS
    else
        git.c.GIT_DIFF_FORMAT_PATCH;

    var buf: git.c.git_buf = undefined;
    if (git.c.git_diff_to_buf(&buf, diff.?, format) < 0) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_buf_dispose(&buf);

    const stdout = if (buf.size == 0 or buf.ptr == null)
        try allocEmpty(allocator)
    else
        try allocator.dupe(u8, buf.ptr[0..buf.size]);
    return okResult(allocator, stdout);
}

fn cmdLog(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var path: ?[]const u8 = null;
    var seen_sep = false;

    for (args) |arg| {
        if (seen_sep) {
            path = arg;
            break;
        }
        if (std.mem.eql(u8, arg, "--")) seen_sep = true;
    }

    if (path == null) return types.RestackError.ParseError;

    const commit_oid = lastCommitForPath(repo, allocator, path.?) catch return gitErrorResult(allocator);
    if (commit_oid == null) {
        return okResult(allocator, try allocEmpty(allocator));
    }

    const hex = try git.oidToHex(allocator, &commit_oid.?);
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, hex);
    try out.append(allocator, '\n');
    allocator.free(hex);
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdShow(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var spec: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-s")) continue;
        if (std.mem.startsWith(u8, arg, "--format=")) continue;
        if (spec == null) spec = arg;
    }
    if (spec == null) return types.RestackError.ParseError;

    const commit = commitFromSpec(repo, allocator, spec.?) catch return gitErrorResult(allocator);
    defer git.c.git_commit_free(commit);

    const summary_ptr = git.c.git_commit_summary(commit);
    const summary = if (summary_ptr == null) "" else std.mem.span(summary_ptr);
    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    try out.appendSlice(allocator, summary);
    try out.append(allocator, '\n');
    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdCherryPick(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len == 0) return types.RestackError.ParseError;

    if (std.mem.eql(u8, args[0], "--continue")) {
        return cherryPickContinue(allocator, repo);
    }
    if (std.mem.eql(u8, args[0], "--skip")) {
        return cherryPickSkip(allocator, repo);
    }

    const commit_spec = args[0];
    const commit = commitFromSpec(repo, allocator, commit_spec) catch return gitErrorResult(allocator);
    defer git.c.git_commit_free(commit);

    var opts: git.c.git_cherrypick_options = undefined;
    if (git.c.git_cherrypick_options_init(&opts, git.c.GIT_CHERRYPICK_OPTIONS_VERSION) < 0) {
        return gitErrorResult(allocator);
    }
    opts.checkout_opts.checkout_strategy = git.c.GIT_CHECKOUT_SAFE;

    if (git.c.git_cherrypick(repo, commit, &opts) < 0) {
        return gitErrorResult(allocator);
    }

    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    if (git.c.git_index_has_conflicts(index.?) != 0) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 1 };
    }

    if (indexMatchesHead(repo) catch false) {
        _ = git.c.git_repository_state_cleanup(repo);
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    const message_ptr = git.c.git_commit_message(commit);
    const message = if (message_ptr == null) "" else std.mem.span(message_ptr);
    if (!try commitFromIndex(repo, allocator, message)) {
        return gitErrorResult(allocator);
    }

    _ = git.c.git_repository_state_cleanup(repo);
    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cherryPickContinue(allocator: std.mem.Allocator, repo: *git.c.git_repository) !GitResult {
    if (git.c.git_repository_state(repo) != git.c.GIT_REPOSITORY_STATE_CHERRYPICK) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocator.dupe(u8, "no cherry-pick in progress"), .exit_code = 1 };
    }

    const head_oid = oidFromSpec(repo, allocator, "HEAD") catch return gitErrorResult(allocator);

    const pick_oid = readCherryPickHead(repo, allocator) catch return gitErrorResult(allocator);
    if (pick_oid == null) {
        return gitErrorResult(allocator);
    }

    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    if (git.c.git_index_has_conflicts(index.?) != 0) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 1 };
    }

    var head_tree_oid: git.c.git_oid = undefined;
    if (git.c.git_index_write_tree_to(&head_tree_oid, index.?, repo) < 0) {
        return gitErrorResult(allocator);
    }

    var head_commit: ?*git.c.git_commit = null;
    if (git.c.git_commit_lookup(&head_commit, repo, &head_oid) < 0 or head_commit == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_commit_free(head_commit.?);

    var head_tree: ?*git.c.git_tree = null;
    if (git.c.git_commit_tree(&head_tree, head_commit.?) < 0 or head_tree == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_tree_free(head_tree.?);

    if (git.c.git_oid_equal(&head_tree_oid, git.c.git_tree_id(head_tree.?)) != 0) {
        const msg = try allocator.dupe(u8, "previous cherry-pick is now empty");
        return .{ .stdout = msg, .stderr = try allocEmpty(allocator), .exit_code = 1 };
    }

    var pick_commit: ?*git.c.git_commit = null;
    if (git.c.git_commit_lookup(&pick_commit, repo, &pick_oid.?) < 0 or pick_commit == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_commit_free(pick_commit.?);

    const message_ptr = git.c.git_commit_message(pick_commit.?);
    const message = if (message_ptr == null) "" else std.mem.span(message_ptr);
    if (!try commitFromIndex(repo, allocator, message)) {
        return gitErrorResult(allocator);
    }

    _ = git.c.git_repository_state_cleanup(repo);
    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cherryPickSkip(allocator: std.mem.Allocator, repo: *git.c.git_repository) !GitResult {
    var head_commit: ?*git.c.git_commit = null;
    if (git.c.git_revparse_single(@ptrCast(&head_commit), repo, "HEAD") == 0 and head_commit != null) {
        var opts: git.c.git_checkout_options = undefined;
        if (git.c.git_checkout_options_init(&opts, git.c.GIT_CHECKOUT_OPTIONS_VERSION) < 0) {
            return gitErrorResult(allocator);
        }
        opts.checkout_strategy = git.c.GIT_CHECKOUT_FORCE;
        _ = git.c.git_reset(repo, @ptrCast(head_commit.?), git.c.GIT_RESET_HARD, &opts);
        git.c.git_commit_free(head_commit.?);
    }

    _ = git.c.git_repository_state_cleanup(repo);
    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdApply(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len == 0) return types.RestackError.ParseError;

    var path: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "-")) continue;
        path = arg;
    }

    if (path == null) return types.RestackError.ParseError;

    const content = std.fs.cwd().readFileAlloc(allocator, path.?, 50 * 1024 * 1024) catch {
        return gitErrorResult(allocator);
    };
    defer allocator.free(content);

    var diff: ?*git.c.git_diff = null;
    if (git.c.git_diff_from_buffer(&diff, content.ptr, content.len) < 0 or diff == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_diff_free(diff.?);

    if (git.c.git_apply(repo, diff.?, git.c.GIT_APPLY_LOCATION_WORKDIR, null) < 0) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try git.lastErrorMessage(allocator), .exit_code = 1 };
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdAdd(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    _ = args;
    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    if (git.c.git_index_add_all(index.?, null, git.c.GIT_INDEX_ADD_DEFAULT, null, null) < 0) {
        return gitErrorResult(allocator);
    }
    _ = git.c.git_index_update_all(index.?, null, null, null);

    if (git.c.git_index_write(index.?) < 0) {
        return gitErrorResult(allocator);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdCommit(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var message: ?[]const u8 = null;
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-m")) {
            if (i + 1 < args.len) {
                message = args[i + 1];
                i += 1;
            }
        }
    }

    if (message == null) return types.RestackError.ParseError;

    if (!try commitFromIndex(repo, allocator, message.?)) {
        return gitErrorResult(allocator);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdRm(allocator: std.mem.Allocator, repo: *git.c.git_repository, workdir: ?[]const u8, args: []const []const u8) !GitResult {
    var cached = false;
    var recursive = false;
    var path: ?[]const u8 = null;
    var seen_sep = false;

    for (args) |arg| {
        if (seen_sep) {
            path = arg;
            break;
        }
        if (std.mem.eql(u8, arg, "--cached")) cached = true;
        if (std.mem.eql(u8, arg, "-r")) recursive = true;
        if (std.mem.eql(u8, arg, "--")) seen_sep = true;
    }

    if (path == null) return types.RestackError.ParseError;

    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    const c_path = try git.toCString(allocator, path.?);
    defer allocator.free(c_path);

    if (recursive) {
        if (git.c.git_index_remove_directory(index.?, c_path, git.c.GIT_INDEX_STAGE_ANY) < 0) {
            return gitErrorResult(allocator);
        }
    } else if (git.c.git_index_remove(index.?, c_path, git.c.GIT_INDEX_STAGE_ANY) < 0) {
        return gitErrorResult(allocator);
    }

    if (git.c.git_index_write(index.?) < 0) {
        return gitErrorResult(allocator);
    }

    if (!cached and workdir != null) {
        const full_path = try std.fs.path.join(allocator, &.{ workdir.?, path.? });
        defer allocator.free(full_path);
        removePath(full_path);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdUpdateIndex(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len < 4 or !std.mem.eql(u8, args[0], "--cacheinfo")) return types.RestackError.ParseError;

    const mode_str = args[1];
    const hash = args[2];
    const path = args[3];

    const mode = std.fmt.parseInt(u32, mode_str, 8) catch return types.RestackError.ParseError;
    var oid: git.c.git_oid = undefined;
    const c_hash = try git.toCString(allocator, hash);
    defer allocator.free(c_hash);
    if (git.c.git_oid_fromstr(&oid, c_hash) < 0) return gitErrorResult(allocator);

    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    const c_path = try git.toCString(allocator, path);
    defer allocator.free(c_path);

    var entry: git.c.git_index_entry = std.mem.zeroes(git.c.git_index_entry);
    entry.mode = mode;
    entry.id = oid;
    entry.path = c_path;

    if (git.c.git_index_add(index.?, &entry) < 0) {
        return gitErrorResult(allocator);
    }
    if (git.c.git_index_write(index.?) < 0) {
        return gitErrorResult(allocator);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn cmdLsFiles(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    var stage = false;
    var unmerged = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--stage")) stage = true;
        if (std.mem.eql(u8, arg, "-u")) unmerged = true;
    }

    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return gitErrorResult(allocator);
    }
    defer git.c.git_index_free(index.?);

    _ = git.c.git_index_read(index.?, 1);

    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);

    if (unmerged) {
        var iter: ?*git.c.git_index_conflict_iterator = null;
        if (git.c.git_index_conflict_iterator_new(&iter, index.?) < 0 or iter == null) {
            return gitErrorResult(allocator);
        }
        defer git.c.git_index_conflict_iterator_free(iter.?);

        while (true) {
            var ancestor: ?*const git.c.git_index_entry = null;
            var ours: ?*const git.c.git_index_entry = null;
            var theirs: ?*const git.c.git_index_entry = null;
            const rc = git.c.git_index_conflict_next(&ancestor, &ours, &theirs, iter.?);
            if (rc == git.c.GIT_ITEROVER) break;
            if (rc < 0) return gitErrorResult(allocator);

            if (ancestor) |entry| try appendIndexEntry(allocator, &out, entry, 1);
            if (ours) |entry| try appendIndexEntry(allocator, &out, entry, 2);
            if (theirs) |entry| try appendIndexEntry(allocator, &out, entry, 3);
        }
    } else if (stage) {
        const count = git.c.git_index_entrycount(index.?);
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const entry = git.c.git_index_get_byindex(index.?, i) orelse continue;
            if (git.c.GIT_INDEX_ENTRY_STAGE(entry) != 0) continue;
            try appendIndexEntry(allocator, &out, entry, 0);
        }
    }

    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn cmdStash(allocator: std.mem.Allocator, repo: *git.c.git_repository, args: []const []const u8) !GitResult {
    if (args.len == 0) return types.RestackError.ParseError;

    if (std.mem.eql(u8, args[0], "push")) {
        var message: ?[]const u8 = null;
        var include_untracked = false;

        var i: usize = 1;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "-u")) include_untracked = true;
            if (std.mem.eql(u8, args[i], "-m") and i + 1 < args.len) {
                message = args[i + 1];
                i += 1;
            }
        }

        const sig = signatureForRepo(repo) catch return gitErrorResult(allocator);
        defer git.c.git_signature_free(sig);

        var c_message: ?[:0]const u8 = null;
        if (message) |m| {
            c_message = try git.toCString(allocator, m);
        }
        defer if (c_message) |msg| allocator.free(msg);

        var oid: git.c.git_oid = undefined;
        const rc = git.c.git_stash_save(&oid, repo, sig, if (c_message) |msg| msg.ptr else null, if (include_untracked) git.c.GIT_STASH_INCLUDE_UNTRACKED else git.c.GIT_STASH_DEFAULT);
        if (rc < 0 and rc != git.c.GIT_ENOTFOUND) {
            return gitErrorResult(allocator);
        }
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    if (std.mem.eql(u8, args[0], "pop")) {
        const rc = git.c.git_stash_pop(repo, 0, null);
        if (rc < 0 and rc != git.c.GIT_ENOTFOUND) {
            return gitErrorResult(allocator);
        }
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    return types.RestackError.ParseError;
}

fn cmdMergetool(allocator: std.mem.Allocator, repo: *git.c.git_repository, workdir: ?[]const u8, args: []const []const u8) !GitResult {
    const tool = blk: {
        var i: usize = 0;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "--tool") and i + 1 < args.len) {
                break :blk args[i + 1];
            }
        }
        break :blk null;
    };

    if (tool == null or workdir == null) {
        return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
    }

    const conflicts = try collectConflictedPaths(allocator, repo);
    defer {
        for (conflicts) |path| allocator.free(path);
        allocator.free(conflicts);
    }

    for (conflicts) |path| {
        const full_path = try std.fs.path.join(allocator, &.{ workdir.?, path });
        defer allocator.free(full_path);

        const result = std.process.Child.run(.{
            .allocator = allocator,
            .argv = &.{ tool.?, full_path },
            .cwd = workdir.?,
            .max_output_bytes = 1024 * 1024,
        }) catch return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 1 };
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }

    return .{ .stdout = try allocEmpty(allocator), .stderr = try allocEmpty(allocator), .exit_code = 0 };
}

fn listConflictedFiles(allocator: std.mem.Allocator, repo: *git.c.git_repository) !GitResult {
    const paths = try collectConflictedPaths(allocator, repo);
    defer {
        for (paths) |path| allocator.free(path);
        allocator.free(paths);
    }

    var out: std.ArrayListUnmanaged(u8) = .{};
    defer out.deinit(allocator);
    for (paths) |path| {
        try out.appendSlice(allocator, path);
        try out.append(allocator, '\n');
    }

    return okResult(allocator, try out.toOwnedSlice(allocator));
}

fn collectConflictedPaths(allocator: std.mem.Allocator, repo: *git.c.git_repository) ![]([]const u8) {
    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return types.RestackError.GitCommandFailed;
    }
    defer git.c.git_index_free(index.?);

    _ = git.c.git_index_read(index.?, 1);

    var iter: ?*git.c.git_index_conflict_iterator = null;
    if (git.c.git_index_conflict_iterator_new(&iter, index.?) < 0 or iter == null) {
        return types.RestackError.GitCommandFailed;
    }
    defer git.c.git_index_conflict_iterator_free(iter.?);

    var paths = std.ArrayListUnmanaged([]const u8){};
    errdefer {
        for (paths.items) |p| allocator.free(p);
        paths.deinit(allocator);
    }

    while (true) {
        var ancestor: ?*const git.c.git_index_entry = null;
        var ours: ?*const git.c.git_index_entry = null;
        var theirs: ?*const git.c.git_index_entry = null;
        const rc = git.c.git_index_conflict_next(&ancestor, &ours, &theirs, iter.?);
        if (rc == git.c.GIT_ITEROVER) break;
        if (rc < 0) return types.RestackError.GitCommandFailed;

        const entry = ancestor orelse ours orelse theirs orelse continue;
        const path = std.mem.span(entry.*.path);
        try paths.append(allocator, try allocator.dupe(u8, path));
    }

    return paths.toOwnedSlice(allocator);
}

fn appendIndexEntry(allocator: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), entry: *const git.c.git_index_entry, stage: u32) !void {
    var mode_buf: [8]u8 = undefined;
    const mode_slice = try std.fmt.bufPrint(&mode_buf, "{o:0>6}", .{entry.*.mode});
    try out.appendSlice(allocator, mode_slice);
    try out.append(allocator, ' ');
    const hex = try git.oidToHex(allocator, &entry.*.id);
    defer allocator.free(hex);
    try out.appendSlice(allocator, hex);
    try out.append(allocator, ' ');
    try out.writer(allocator).print("{d}\t", .{stage});
    try out.appendSlice(allocator, std.mem.span(entry.*.path));
    try out.append(allocator, '\n');
}

fn lastCommitForPath(repo: *git.c.git_repository, allocator: std.mem.Allocator, path: []const u8) !?git.c.git_oid {
    var walk: ?*git.c.git_revwalk = null;
    if (git.c.git_revwalk_new(&walk, repo) < 0 or walk == null) return null;
    defer git.c.git_revwalk_free(walk.?);

    _ = git.c.git_revwalk_sorting(walk.?, git.c.GIT_SORT_TIME);
    if (git.c.git_revwalk_push_head(walk.?) < 0) return null;

    var oid: git.c.git_oid = undefined;
    while (git.c.git_revwalk_next(&oid, walk.?) == 0) {
        var commit: ?*git.c.git_commit = null;
        if (git.c.git_commit_lookup(&commit, repo, &oid) < 0 or commit == null) continue;
        defer git.c.git_commit_free(commit.?);

        if (commitTouchesPath(repo, commit.?, allocator, path)) {
            return oid;
        }
    }

    return null;
}

fn commitTouchesPath(repo: *git.c.git_repository, commit: *git.c.git_commit, allocator: std.mem.Allocator, path: []const u8) bool {
    _ = repo;
    var tree: ?*git.c.git_tree = null;
    if (git.c.git_commit_tree(&tree, commit) < 0 or tree == null) return false;
    defer git.c.git_tree_free(tree.?);

    const c_path = git.toCString(allocator, path) catch return false;
    defer allocator.free(c_path);

    const parent_count = git.c.git_commit_parentcount(commit);
    if (parent_count == 0) {
        const entry = treeHasPath(tree.?, c_path);
        defer if (entry) |e| git.c.git_tree_entry_free(e);
        return entry != null;
    }

    var i: usize = 0;
    while (i < parent_count) : (i += 1) {
        var parent: ?*git.c.git_commit = null;
        if (git.c.git_commit_parent(&parent, commit, @intCast(i)) < 0 or parent == null) continue;
        defer git.c.git_commit_free(parent.?);

        var parent_tree: ?*git.c.git_tree = null;
        if (git.c.git_commit_tree(&parent_tree, parent.?) < 0 or parent_tree == null) continue;
        defer git.c.git_tree_free(parent_tree.?);

        const entry = treeHasPath(tree.?, c_path);
        defer if (entry) |e| git.c.git_tree_entry_free(e);
        const parent_entry = treeHasPath(parent_tree.?, c_path);
        defer if (parent_entry) |e| git.c.git_tree_entry_free(e);
        if (entry == null and parent_entry == null) continue;
        if (entry == null or parent_entry == null) return true;
        if (git.c.git_oid_equal(git.c.git_tree_entry_id(entry.?), git.c.git_tree_entry_id(parent_entry.?)) == 0) return true;
    }

    return false;
}

fn treeHasPath(tree: *git.c.git_tree, c_path: [:0]const u8) ?*git.c.git_tree_entry {
    var entry: ?*git.c.git_tree_entry = null;
    if (git.c.git_tree_entry_bypath(&entry, tree, c_path) < 0 or entry == null) return null;
    return entry.?;
}

fn indexMatchesHead(repo: *git.c.git_repository) !bool {
    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) {
        return types.RestackError.GitCommandFailed;
    }
    defer git.c.git_index_free(index.?);

    var index_tree_oid: git.c.git_oid = undefined;
    if (git.c.git_index_write_tree_to(&index_tree_oid, index.?, repo) < 0) {
        return types.RestackError.GitCommandFailed;
    }

    var head_ref: ?*git.c.git_reference = null;
    if (git.c.git_repository_head(&head_ref, repo) < 0 or head_ref == null) return false;
    defer git.c.git_reference_free(head_ref.?);

    var head_commit: ?*git.c.git_commit = null;
    if (git.c.git_reference_peel(@ptrCast(&head_commit), head_ref.?, git.c.GIT_OBJECT_COMMIT) < 0 or head_commit == null) return false;
    defer git.c.git_commit_free(head_commit.?);

    var head_tree: ?*git.c.git_tree = null;
    if (git.c.git_commit_tree(&head_tree, head_commit.?) < 0 or head_tree == null) return false;
    defer git.c.git_tree_free(head_tree.?);

    return git.c.git_oid_equal(&index_tree_oid, git.c.git_tree_id(head_tree.?)) != 0;
}

fn commitFromIndex(repo: *git.c.git_repository, allocator: std.mem.Allocator, message: []const u8) !bool {
    var index: ?*git.c.git_index = null;
    if (git.c.git_repository_index(&index, repo) < 0 or index == null) return false;
    defer git.c.git_index_free(index.?);

    if (git.c.git_index_has_conflicts(index.?) != 0) return false;

    var tree_oid: git.c.git_oid = undefined;
    if (git.c.git_index_write_tree(&tree_oid, index.?) < 0) return false;

    var tree: ?*git.c.git_tree = null;
    if (git.c.git_tree_lookup(&tree, repo, &tree_oid) < 0 or tree == null) return false;
    defer git.c.git_tree_free(tree.?);

    const sig = signatureForRepo(repo) catch return false;
    defer git.c.git_signature_free(sig);

    var parents: [1]?*const git.c.git_commit = .{null};
    var parent_count: usize = 0;

    var head_ref: ?*git.c.git_reference = null;
    if (git.c.git_repository_head(&head_ref, repo) == 0 and head_ref != null) {
        defer git.c.git_reference_free(head_ref.?);
        var head_commit: ?*git.c.git_commit = null;
        if (git.c.git_reference_peel(@ptrCast(&head_commit), head_ref.?, git.c.GIT_OBJECT_COMMIT) == 0 and head_commit != null) {
            parents[0] = head_commit.?;
            parent_count = 1;
            defer git.c.git_commit_free(head_commit.?);
        }
    }

    const c_message = try git.toCString(allocator, message);
    defer allocator.free(c_message);

    var out_oid: git.c.git_oid = undefined;
    const parents_ptr: [*c]?*const git.c.git_commit = @ptrCast(&parents);
    const rc = git.c.git_commit_create(
        &out_oid,
        repo,
        "HEAD",
        sig,
        sig,
        null,
        c_message,
        tree.?,
        parent_count,
        if (parent_count == 1) parents_ptr else null,
    );

    if (rc < 0) return false;
    return true;
}

fn readCherryPickHead(repo: *git.c.git_repository, allocator: std.mem.Allocator) !?git.c.git_oid {
    const git_dir_ptr = git.c.git_repository_path(repo) orelse return null;
    const git_dir = std.mem.span(git_dir_ptr);
    const head_path = try std.fs.path.join(allocator, &.{ git_dir, "CHERRY_PICK_HEAD" });
    defer allocator.free(head_path);

    const content = std.fs.cwd().readFileAlloc(allocator, head_path, 1024) catch return null;
    defer allocator.free(content);

    const trimmed = std.mem.trim(u8, content, "\n\r\t ");
    if (trimmed.len == 0) return null;

    const c_hash = try git.toCString(allocator, trimmed);
    defer allocator.free(c_hash);
    var oid: git.c.git_oid = undefined;
    if (git.c.git_oid_fromstr(&oid, c_hash) < 0) return null;
    return oid;
}

fn removePath(path: []const u8) void {
    if (std.fs.cwd().statFile(path)) |stat| {
        if (stat.kind == .directory) {
            std.fs.cwd().deleteTree(path) catch {};
        } else {
            std.fs.cwd().deleteFile(path) catch {};
        }
    } else |_| {}
}

fn pathsEqual(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, trimTrailingSlash(a), trimTrailingSlash(b));
}

fn trimTrailingSlash(path: []const u8) []const u8 {
    var end = path.len;
    while (end > 0 and (path[end - 1] == '/' or path[end - 1] == '\\')) : (end -= 1) {}
    return path[0..end];
}
