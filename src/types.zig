const std = @import("std");

pub const StackBranch = struct {
    name: []const u8,
    commit_sha: []const u8,
    parent_branch: ?[]const u8,
    commits_from_parent: u32,
    needs_fix: bool = false,
    fix: ?Fix = null,
    step_status: StepStatus = .pending,
};

pub const Fix = struct {
    commit_message: []const u8,
    files: []FileChange,
};

pub const FileChange = struct {
    path: []const u8,
    change_type: ChangeType,
    diff: []const u8,
    staged: bool,
};

pub const ChangeType = enum {
    modified,
    deleted,
    added,

    pub fn toString(self: ChangeType) []const u8 {
        return switch (self) {
            .modified => "modified",
            .deleted => "deleted",
            .added => "added",
        };
    }
};

pub const StepStatus = enum {
    pending,
    cherry_picked,
    fixed,
    verified,
    skipped,

    pub fn toString(self: StepStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .cherry_picked => "cherry_picked",
            .fixed => "fixed",
            .verified => "verified",
            .skipped => "skipped",
        };
    }
};

pub const Stack = struct {
    branches: []StackBranch,
    base_branch: []const u8,
    base_commit: []const u8,
    head_branch: []const u8,
    head_commit: []const u8,
};

pub const PlanError = struct {
    error_type: ErrorType,
    path: []const u8,
    message: []const u8,
};

pub const ErrorType = enum {
    unmapped_file,
    outside_ancestry,
    ambiguous_branch,

    pub fn toString(self: ErrorType) []const u8 {
        return switch (self) {
            .unmapped_file => "unmapped_file",
            .outside_ancestry => "outside_ancestry",
            .ambiguous_branch => "ambiguous_branch",
        };
    }
};

pub const Plan = struct {
    version: u32 = 1,
    generated: []const u8,
    repository: []const u8,
    errors: []PlanError,
    stack: Stack,
};

pub const ExecutionState = struct {
    plan_file: []const u8,
    plan_hash: []const u8,
    worktree_path: []const u8,
    current_step_index: u32,
    started_at: []const u8,
    last_updated: []const u8,
    status: ExecutionStatus,
    completed_branches: [][]const u8,
};

pub const ExecutionStatus = enum {
    pending,
    in_progress,
    conflict,
    completed,
    failed,
    aborted,

    pub fn toString(self: ExecutionStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .in_progress => "in_progress",
            .conflict => "conflict",
            .completed => "completed",
            .failed => "failed",
            .aborted => "aborted",
        };
    }
};

pub const JengaError = error{
    GitCommandFailed,
    GitNotInsideRepository,
    BaseBranchNotFound,
    CurrentBranchUnknown,
    BranchAmbiguous,
    UnmappedFiles,
    ProcessError,
    ParseError,
    OutOfMemory,
    PlanHasErrors,
    StateFileCorrupted,
    WorktreeExists,
    ConflictDetected,
    PlanNotFound,
    InvalidPlan,
};
