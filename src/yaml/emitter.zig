const std = @import("std");
const types = @import("../types.zig");

/// Emit a complete plan as YAML
pub fn emitPlan(allocator: std.mem.Allocator, plan: types.Plan) ![]const u8 {
    var buffer: std.ArrayListUnmanaged(u8) = .{};
    errdefer buffer.deinit(allocator);
    const writer = buffer.writer(allocator);

    try writer.writeAll("# git-restack restacking plan\n");
    try writer.print("version: {d}\n", .{plan.version});
    try writer.print("generated: \"{s}\"\n", .{plan.generated});
    try writer.print("repository: \"{s}\"\n", .{plan.repository});
    if (plan.verify_cmd) |cmd| {
        try writer.print("verify_cmd: \"{s}\"\n", .{cmd});
    }
    try writer.writeAll("\n");

    // Errors section
    try writer.writeAll("# STRICT VALIDATION\n");
    try writer.writeAll("# If this list is not empty, 'exec' will refuse to run.\n");
    try writer.writeAll("# User must move entries to appropriate branch fix blocks or delete files.\n");
    try writer.writeAll("errors:\n");
    if (plan.errors.len == 0) {
        try writer.writeAll("  []\n");
    } else {
        for (plan.errors) |err| {
            try writer.writeAll("  - type: ");
            try writer.writeAll(err.error_type.toString());
            try writer.writeAll("\n");
            try writer.print("    path: \"{s}\"\n", .{err.path});
            try writer.print("    message: \"{s}\"\n", .{err.message});
        }
    }
    try writer.writeAll("\n");

    // Simulation metadata
    if (plan.simulation) |sim| {
        try writer.writeAll("# Simulation metadata\n");
        try writer.writeAll("simulation:\n");
        try writer.print("  mode: {s}\n", .{sim.mode.toString()});
        if (sim.worktree_path) |path| {
            try writer.print("  worktree_path: \"{s}\"\n", .{path});
        } else {
            try writer.writeAll("  worktree_path: null\n");
        }
        if (sim.plan_branch) |name| {
            try writer.print("  plan_branch: \"{s}\"\n", .{name});
        } else {
            try writer.writeAll("  plan_branch: null\n");
        }
        try writer.writeAll("  backup_branches:\n");
        if (sim.backup_branches.len == 0) {
            try writer.writeAll("    []\n");
        } else {
            for (sim.backup_branches) |branch| {
                try writer.print("    - source: \"{s}\"\n", .{branch.source});
                try writer.print("      backup: \"{s}\"\n", .{branch.backup});
            }
        }
        try writer.writeAll("\n");
    }

    // Source state
    try writer.writeAll("# Source state snapshot\n");
    try writer.writeAll("source:\n");
    try writer.print("  head_branch: \"{s}\"\n", .{plan.stack.head_branch});
    try writer.print("  head_commit: \"{s}\"\n", .{plan.stack.head_commit});
    try writer.print("  base_branch: \"{s}\"\n", .{plan.stack.base_branch});
    try writer.print("  base_commit: \"{s}\"\n", .{plan.stack.base_commit});
    try writer.print("  base_tip: \"{s}\"\n", .{plan.stack.base_tip});
    try writer.writeAll("\n");

    // Stack
    try writer.writeAll("# Branch stack (ordered from base to HEAD)\n");
    try writer.writeAll("stack:\n");

    for (plan.stack.branches) |branch| {
        try writer.print("  - branch: \"{s}\"\n", .{branch.name});
        try writer.print("    commit: \"{s}\"\n", .{branch.commit_sha});
        if (branch.parent_branch) |parent| {
            try writer.print("    parent_branch: \"{s}\"\n", .{parent});
        } else {
            try writer.writeAll("    parent_branch: null\n");
        }
        try writer.print("    commits_from_parent: {d}\n", .{branch.commits_from_parent});
        try writer.print("    needs_fix: {}\n", .{branch.needs_fix});
        try writer.print("    step_status: {s}\n", .{branch.step_status.toString()});

        if (branch.fix) |fix| {
            try writer.writeAll("    fix:\n");
            try writer.writeAll("      commit_message: |\n");
            // Write commit message with proper indentation
            var msg_lines = std.mem.splitScalar(u8, fix.commit_message, '\n');
            while (msg_lines.next()) |line| {
                try writer.writeAll("        ");
                try writer.writeAll(line);
                try writer.writeAll("\n");
            }

            try writer.writeAll("      files:\n");
            for (fix.files) |file| {
                try writer.print("        - path: \"{s}\"\n", .{file.path});
                try writer.print("          change_type: {s}\n", .{file.change_type.toString()});
                try writer.print("          staged: {}\n", .{file.staged});
                try writer.writeAll("          diff: |\n");
                // Write diff with proper indentation
                var diff_lines = std.mem.splitScalar(u8, file.diff, '\n');
                while (diff_lines.next()) |line| {
                    try writer.writeAll("            ");
                    try writer.writeAll(line);
                    try writer.writeAll("\n");
                }
            }
        }
        try writer.writeAll("\n");
    }

    // Conflicts
    try writer.writeAll("# Conflict resolutions\n");
    try writer.writeAll("conflicts:\n");
    if (plan.conflicts.len == 0) {
        try writer.writeAll("  []\n");
    } else {
        for (plan.conflicts) |conflict| {
            try writer.writeAll("  - kind: ");
            try writer.writeAll(conflict.kind.toString());
            try writer.writeAll("\n");
            try writer.print("    branch: \"{s}\"\n", .{conflict.branch});
            if (conflict.commit) |commit| {
                try writer.print("    commit: \"{s}\"\n", .{commit});
            } else {
                try writer.writeAll("    commit: null\n");
            }
            if (conflict.subject) |subject| {
                try writer.print("    subject: \"{s}\"\n", .{subject});
            } else {
                try writer.writeAll("    subject: null\n");
            }
            try writer.writeAll("    files:\n");
            for (conflict.files) |file| {
                try writer.print("      - path: \"{s}\"\n", .{file.path});
                try writer.writeAll("        conflict_diff: |\n");
                var diff_lines = std.mem.splitScalar(u8, file.conflict_diff, '\n');
                while (diff_lines.next()) |line| {
                    try writer.writeAll("          ");
                    try writer.writeAll(line);
                    try writer.writeAll("\n");
                }
                try writer.writeAll("        resolution:\n");
                try writer.print("          present: {}\n", .{file.resolution.present});
                try writer.print("          encoding: {s}\n", .{file.resolution.encoding.toString()});
                try writer.writeAll("          content: |\n");
                var content_lines = std.mem.splitScalar(u8, file.resolution.content, '\n');
                while (content_lines.next()) |line| {
                    try writer.writeAll("            ");
                    try writer.writeAll(line);
                    try writer.writeAll("\n");
                }
            }
        }
    }
    try writer.writeAll("\n");

    return buffer.toOwnedSlice(allocator);
}

/// Emit execution state as JSON
pub fn emitState(allocator: std.mem.Allocator, state: types.ExecutionState) ![]const u8 {
    var buffer: std.ArrayListUnmanaged(u8) = .{};
    errdefer buffer.deinit(allocator);
    const writer = buffer.writer(allocator);

    try writer.writeAll("{\n");
    try writer.print("  \"plan_file\": \"{s}\",\n", .{state.plan_file});
    try writer.print("  \"plan_hash\": \"{s}\",\n", .{state.plan_hash});
    try writer.print("  \"worktree_path\": \"{s}\",\n", .{state.worktree_path});
    try writer.print("  \"current_step_index\": {d},\n", .{state.current_step_index});
    try writer.print("  \"current_commit_index\": {d},\n", .{state.current_commit_index});
    try writer.print("  \"started_at\": \"{s}\",\n", .{state.started_at});
    try writer.print("  \"last_updated\": \"{s}\",\n", .{state.last_updated});
    try writer.print("  \"status\": \"{s}\",\n", .{state.status.toString()});
    try writer.print("  \"mode\": \"{s}\",\n", .{state.mode.toString()});
    try writer.writeAll("  \"completed_branches\": [");

    for (state.completed_branches, 0..) |branch, i| {
        try writer.print("\"{s}\"", .{branch});
        if (i < state.completed_branches.len - 1) {
            try writer.writeAll(", ");
        }
    }

    try writer.writeAll("],\n");

    if (state.verify_cmd) |cmd| {
        try writer.print("  \"verify_cmd\": \"{s}\"\n", .{cmd});
    } else {
        try writer.writeAll("  \"verify_cmd\": null\n");
    }

    try writer.writeAll("}\n");

    return buffer.toOwnedSlice(allocator);
}
