const std = @import("std");
const types = @import("../types.zig");
const strings = @import("../utils/strings.zig");

/// Parse a YAML plan file strictly
pub fn parsePlan(allocator: std.mem.Allocator, content: []const u8) !types.Plan {
    var parser = Parser.init(allocator, content);
    return parser.parse();
}

const Parser = struct {
    allocator: std.mem.Allocator,
    content: []const u8,
    lines: std.mem.SplitIterator(u8, .scalar),
    line_number: usize,
    current_line: ?[]const u8,

    fn init(allocator: std.mem.Allocator, content: []const u8) Parser {
        return .{
            .allocator = allocator,
            .content = content,
            .lines = std.mem.splitScalar(u8, content, '\n'),
            .line_number = 0,
            .current_line = null,
        };
    }

    fn nextLine(self: *Parser) ?[]const u8 {
        while (self.lines.next()) |line| {
            self.line_number += 1;
            const trimmed = strings.trim(line);
            if (trimmed.len == 0 or trimmed[0] == '#') {
                continue;
            }
            self.current_line = line;
            return line;
        }
        self.current_line = null;
        return null;
    }

    fn parse(self: *Parser) !types.Plan {
        var version: u32 = 1;
        var generated: []const u8 = "";
        var repository: []const u8 = "";
        var errors: std.ArrayListUnmanaged(types.PlanError) = .{};
        var branches: std.ArrayListUnmanaged(types.StackBranch) = .{};
        var head_branch: []const u8 = "";
        var head_commit: []const u8 = "";
        var base_branch: []const u8 = "";
        var base_commit: []const u8 = "";

        while (self.nextLine()) |line| {
            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "version")) {
                version = std.fmt.parseInt(u32, value, 10) catch 1;
            } else if (std.mem.eql(u8, key, "generated")) {
                generated = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "repository")) {
                repository = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "errors")) {
                errors = try self.parseErrors();
            } else if (std.mem.eql(u8, key, "source")) {
                const source = try self.parseSource();
                head_branch = source.head_branch;
                head_commit = source.head_commit;
                base_branch = source.base_branch;
                base_commit = source.base_commit;
            } else if (std.mem.eql(u8, key, "stack")) {
                branches = try self.parseStack();
            }
        }

        return types.Plan{
            .version = version,
            .generated = generated,
            .repository = repository,
            .errors = try errors.toOwnedSlice(self.allocator),
            .stack = .{
                .branches = try branches.toOwnedSlice(self.allocator),
                .base_branch = base_branch,
                .base_commit = base_commit,
                .head_branch = head_branch,
                .head_commit = head_commit,
            },
        };
    }

    fn getKey(self: *Parser, line: []const u8) ?[]const u8 {
        _ = self;
        const trimmed = strings.trim(line);
        if (std.mem.indexOfScalar(u8, trimmed, ':')) |idx| {
            return trimmed[0..idx];
        }
        return null;
    }

    fn getValue(self: *Parser, line: []const u8) []const u8 {
        _ = self;
        const trimmed = strings.trim(line);
        if (std.mem.indexOfScalar(u8, trimmed, ':')) |idx| {
            if (idx + 1 < trimmed.len) {
                return strings.trim(trimmed[idx + 1 ..]);
            }
        }
        return "";
    }

    fn parseQuotedString(self: *Parser, value: []const u8) ![]const u8 {
        var v = value;
        if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') {
            v = v[1 .. v.len - 1];
        }
        return strings.copy(self.allocator, v);
    }

    fn parseErrors(self: *Parser) !std.ArrayListUnmanaged(types.PlanError) {
        var errors: std.ArrayListUnmanaged(types.PlanError) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.line_number -= 1;
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "[]")) {
                break;
            }

            if (std.mem.startsWith(u8, trimmed, "- type:")) {
                var err = types.PlanError{
                    .error_type = .unmapped_file,
                    .path = "",
                    .message = "",
                };

                const type_str = self.getValue(line);
                if (std.mem.eql(u8, type_str, "unmapped_file")) {
                    err.error_type = .unmapped_file;
                } else if (std.mem.eql(u8, type_str, "outside_ancestry")) {
                    err.error_type = .outside_ancestry;
                } else if (std.mem.eql(u8, type_str, "ambiguous_branch")) {
                    err.error_type = .ambiguous_branch;
                }

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.line_number -= 1;
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "path")) {
                        err.path = try self.parseQuotedString(next_value);
                    } else if (std.mem.eql(u8, next_key, "message")) {
                        err.message = try self.parseQuotedString(next_value);
                    }
                }

                try errors.append(self.allocator, err);
            }
        }

        return errors;
    }

    const SourceResult = struct {
        head_branch: []const u8,
        head_commit: []const u8,
        base_branch: []const u8,
        base_commit: []const u8,
    };

    fn parseSource(self: *Parser) !SourceResult {
        var result: SourceResult = .{
            .head_branch = "",
            .head_commit = "",
            .base_branch = "",
            .base_commit = "",
        };

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.line_number -= 1;
                break;
            }

            const key = self.getKey(line) orelse continue;
            const value = self.getValue(line);

            if (std.mem.eql(u8, key, "head_branch")) {
                result.head_branch = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "head_commit")) {
                result.head_commit = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "base_branch")) {
                result.base_branch = try self.parseQuotedString(value);
            } else if (std.mem.eql(u8, key, "base_commit")) {
                result.base_commit = try self.parseQuotedString(value);
            }
        }

        return result;
    }

    fn parseStack(self: *Parser) !std.ArrayListUnmanaged(types.StackBranch) {
        var branches: std.ArrayListUnmanaged(types.StackBranch) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent == 0) {
                self.line_number -= 1;
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "- branch:")) {
                var branch = types.StackBranch{
                    .name = try self.parseQuotedString(self.getValue(line)),
                    .commit_sha = "",
                    .parent_branch = null,
                    .commits_from_parent = 0,
                    .needs_fix = false,
                    .fix = null,
                    .step_status = .pending,
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.line_number -= 1;
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "commit")) {
                        branch.commit_sha = try self.parseQuotedString(next_value);
                    } else if (std.mem.eql(u8, next_key, "parent_branch")) {
                        if (!std.mem.eql(u8, next_value, "null")) {
                            branch.parent_branch = try self.parseQuotedString(next_value);
                        }
                    } else if (std.mem.eql(u8, next_key, "commits_from_parent")) {
                        branch.commits_from_parent = std.fmt.parseInt(u32, next_value, 10) catch 0;
                    } else if (std.mem.eql(u8, next_key, "needs_fix")) {
                        branch.needs_fix = std.mem.eql(u8, next_value, "true");
                    } else if (std.mem.eql(u8, next_key, "step_status")) {
                        branch.step_status = self.parseStepStatus(next_value);
                    } else if (std.mem.eql(u8, next_key, "fix")) {
                        branch.fix = try self.parseFix(next_indent);
                    }
                }

                try branches.append(self.allocator, branch);
            }
        }

        return branches;
    }

    fn parseFix(self: *Parser, parent_indent: usize) !types.Fix {
        var commit_message: []const u8 = "";
        var files: std.ArrayListUnmanaged(types.FileChange) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.line_number -= 1;
                break;
            }

            const key = self.getKey(line) orelse continue;

            if (std.mem.eql(u8, key, "commit_message")) {
                commit_message = try self.parseLiteralBlock(indent);
            } else if (std.mem.eql(u8, key, "files")) {
                files = try self.parseFiles(indent);
            }
        }

        return types.Fix{
            .commit_message = commit_message,
            .files = try files.toOwnedSlice(self.allocator),
        };
    }

    fn parseFiles(self: *Parser, parent_indent: usize) !std.ArrayListUnmanaged(types.FileChange) {
        var files: std.ArrayListUnmanaged(types.FileChange) = .{};

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.line_number -= 1;
                break;
            }

            const trimmed = strings.trim(line);
            if (std.mem.startsWith(u8, trimmed, "- path:")) {
                var file = types.FileChange{
                    .path = try self.parseQuotedString(self.getValue(line)),
                    .change_type = .modified,
                    .diff = "",
                    .staged = true,
                };

                while (self.nextLine()) |next_line| {
                    const next_indent = self.getIndent(next_line);
                    if (next_indent <= indent) {
                        self.line_number -= 1;
                        break;
                    }

                    const next_key = self.getKey(next_line) orelse continue;
                    const next_value = self.getValue(next_line);

                    if (std.mem.eql(u8, next_key, "change_type")) {
                        file.change_type = self.parseChangeType(next_value);
                    } else if (std.mem.eql(u8, next_key, "staged")) {
                        file.staged = std.mem.eql(u8, next_value, "true");
                    } else if (std.mem.eql(u8, next_key, "diff")) {
                        file.diff = try self.parseLiteralBlock(next_indent);
                    }
                }

                try files.append(self.allocator, file);
            }
        }

        return files;
    }

    fn parseLiteralBlock(self: *Parser, parent_indent: usize) ![]const u8 {
        var buffer: std.ArrayListUnmanaged(u8) = .{};
        errdefer buffer.deinit(self.allocator);

        while (self.nextLine()) |line| {
            const indent = self.getIndent(line);
            if (indent <= parent_indent) {
                self.line_number -= 1;
                break;
            }

            const content = if (indent < line.len) line[indent..] else "";
            try buffer.appendSlice(self.allocator, content);
            try buffer.append(self.allocator, '\n');
        }

        if (buffer.items.len > 0 and buffer.items[buffer.items.len - 1] == '\n') {
            _ = buffer.pop();
        }

        return buffer.toOwnedSlice(self.allocator);
    }

    fn getIndent(self: *Parser, line: []const u8) usize {
        _ = self;
        var count: usize = 0;
        for (line) |c| {
            if (c == ' ') {
                count += 1;
            } else {
                break;
            }
        }
        return count;
    }

    fn parseStepStatus(self: *Parser, value: []const u8) types.StepStatus {
        _ = self;
        if (std.mem.eql(u8, value, "pending")) return .pending;
        if (std.mem.eql(u8, value, "cherry_picked")) return .cherry_picked;
        if (std.mem.eql(u8, value, "fixed")) return .fixed;
        if (std.mem.eql(u8, value, "verified")) return .verified;
        if (std.mem.eql(u8, value, "skipped")) return .skipped;
        return .pending;
    }

    fn parseChangeType(self: *Parser, value: []const u8) types.ChangeType {
        _ = self;
        if (std.mem.eql(u8, value, "added")) return .added;
        if (std.mem.eql(u8, value, "deleted")) return .deleted;
        return .modified;
    }
};

/// Parse execution state from JSON
pub fn parseState(allocator: std.mem.Allocator, content: []const u8) !types.ExecutionState {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, content, .{});
    defer parsed.deinit();

    const root = parsed.value.object;

    var completed: std.ArrayListUnmanaged([]const u8) = .{};
    if (root.get("completed_branches")) |branches| {
        for (branches.array.items) |item| {
            try completed.append(allocator, try strings.copy(allocator, item.string));
        }
    }

    return types.ExecutionState{
        .plan_file = try strings.copy(allocator, root.get("plan_file").?.string),
        .plan_hash = try strings.copy(allocator, root.get("plan_hash").?.string),
        .worktree_path = try strings.copy(allocator, root.get("worktree_path").?.string),
        .current_step_index = @intCast(root.get("current_step_index").?.integer),
        .started_at = try strings.copy(allocator, root.get("started_at").?.string),
        .last_updated = try strings.copy(allocator, root.get("last_updated").?.string),
        .status = parseExecutionStatus(root.get("status").?.string),
        .completed_branches = try completed.toOwnedSlice(allocator),
    };
}

fn parseExecutionStatus(value: []const u8) types.ExecutionStatus {
    if (std.mem.eql(u8, value, "pending")) return .pending;
    if (std.mem.eql(u8, value, "in_progress")) return .in_progress;
    if (std.mem.eql(u8, value, "conflict")) return .conflict;
    if (std.mem.eql(u8, value, "completed")) return .completed;
    if (std.mem.eql(u8, value, "failed")) return .failed;
    if (std.mem.eql(u8, value, "aborted")) return .aborted;
    return .pending;
}
