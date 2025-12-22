const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create shared modules
    const types_mod = b.createModule(.{
        .root_source_file = b.path("src/types.zig"),
        .target = target,
        .optimize = optimize,
    });

    const strings_mod = b.createModule(.{
        .root_source_file = b.path("src/utils/strings.zig"),
        .target = target,
        .optimize = optimize,
    });

    const process_mod = b.createModule(.{
        .root_source_file = b.path("src/utils/process.zig"),
        .target = target,
        .optimize = optimize,
    });
    process_mod.addImport("types", types_mod);

    const stack_mod = b.createModule(.{
        .root_source_file = b.path("src/git/stack.zig"),
        .target = target,
        .optimize = optimize,
    });
    stack_mod.addImport("types", types_mod);
    stack_mod.addImport("process", process_mod);
    stack_mod.addImport("strings", strings_mod);

    const diff_mod = b.createModule(.{
        .root_source_file = b.path("src/git/diff.zig"),
        .target = target,
        .optimize = optimize,
    });
    diff_mod.addImport("types", types_mod);
    diff_mod.addImport("process", process_mod);
    diff_mod.addImport("strings", strings_mod);

    const emitter_mod = b.createModule(.{
        .root_source_file = b.path("src/yaml/emitter.zig"),
        .target = target,
        .optimize = optimize,
    });
    emitter_mod.addImport("types", types_mod);

    const parser_mod = b.createModule(.{
        .root_source_file = b.path("src/yaml/parser.zig"),
        .target = target,
        .optimize = optimize,
    });
    parser_mod.addImport("types", types_mod);
    parser_mod.addImport("strings", strings_mod);

    // Main executable
    const exe = b.addExecutable(.{
        .name = "git-jenga",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Unit tests (from src/main.zig)
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Integration tests
    const integration_test_mod = b.createModule(.{
        .root_source_file = b.path("test/integration_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    integration_test_mod.addImport("types", types_mod);
    integration_test_mod.addImport("strings", strings_mod);
    integration_test_mod.addImport("process", process_mod);
    integration_test_mod.addImport("stack", stack_mod);
    integration_test_mod.addImport("diff", diff_mod);
    integration_test_mod.addImport("parser", parser_mod);
    integration_test_mod.addImport("emitter", emitter_mod);

    const integration_tests = b.addTest(.{
        .root_module = integration_test_mod,
    });

    const run_integration_tests = b.addRunArtifact(integration_tests);
    const integration_test_step = b.step("test-integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);

    // All tests
    const all_tests_step = b.step("test-all", "Run all tests");
    all_tests_step.dependOn(&run_unit_tests.step);
    all_tests_step.dependOn(&run_integration_tests.step);
}
