const std = @import("std");

const LibGit2 = struct {
    include_dir: std.Build.LazyPath,
    lib_dir: std.Build.LazyPath,
    build_step: *std.Build.Step,
};

fn setupLibGit2(b: *std.Build) LibGit2 {
    const libgit2_src = b.path("deps/libgit2");
    const libgit2_include = b.path("deps/libgit2/include");
    const libgit2_build = b.path("zig-cache/libgit2");
    const libgit2_lib = libgit2_build;

    std.fs.cwd().access("deps/libgit2/CMakeLists.txt", .{}) catch {
        std.debug.panic("Missing libgit2 submodule. Run: git submodule update --init --recursive", .{});
    };

    const configure = b.addSystemCommand(&.{ "cmake", "-S" });
    configure.addFileArg(libgit2_src);
    configure.addArg("-B");
    configure.addFileArg(libgit2_build);
    configure.addArgs(&.{
        "-DBUILD_SHARED_LIBS=OFF",
        "-DBUILD_TESTS=OFF",
        "-DBUILD_CLI=OFF",
        "-DBUILD_EXAMPLES=OFF",
        "-DUSE_HTTPS=OFF",
        "-DUSE_SSH=OFF",
        "-DUSE_NTLMCLIENT=OFF",
        "-DUSE_AUTH_NEGOTIATE=OFF",
        "-DUSE_I18N=OFF",
        "-DUSE_BUNDLED_ZLIB=ON",
        "-DUSE_REGEX=builtin",
        "-DCMAKE_BUILD_TYPE=Release",
    });
    configure.addPrefixedFileArg("-DCMAKE_ARCHIVE_OUTPUT_DIRECTORY=", libgit2_lib);
    configure.has_side_effects = true;

    const libgit2_build_step = b.addSystemCommand(&.{ "cmake", "--build" });
    libgit2_build_step.addFileArg(libgit2_build);
    libgit2_build_step.addArgs(&.{ "--config", "Release" });
    libgit2_build_step.has_side_effects = true;
    libgit2_build_step.step.dependOn(&configure.step);

    return .{
        .include_dir = libgit2_include,
        .lib_dir = libgit2_lib,
        .build_step = &libgit2_build_step.step,
    };
}

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

    const libgit2 = setupLibGit2(b);

    // Main executable
    const exe = b.addExecutable(.{
        .name = "git-restack",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addIncludePath(libgit2.include_dir);
    exe.root_module.addLibraryPath(libgit2.lib_dir);
    exe.root_module.linkSystemLibrary("git2", .{
        .use_pkg_config = .no,
        .preferred_link_mode = .static,
    });
    exe.step.dependOn(libgit2.build_step);

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
    unit_tests.root_module.addIncludePath(libgit2.include_dir);
    unit_tests.root_module.addLibraryPath(libgit2.lib_dir);
    unit_tests.root_module.linkSystemLibrary("git2", .{
        .use_pkg_config = .no,
        .preferred_link_mode = .static,
    });
    unit_tests.step.dependOn(libgit2.build_step);

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
    integration_tests.root_module.addIncludePath(libgit2.include_dir);
    integration_tests.root_module.addLibraryPath(libgit2.lib_dir);
    integration_tests.root_module.linkSystemLibrary("git2", .{
        .use_pkg_config = .no,
        .preferred_link_mode = .static,
    });
    integration_tests.step.dependOn(libgit2.build_step);

    const run_integration_tests = b.addRunArtifact(integration_tests);
    const integration_test_step = b.step("test-integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);

    // All tests
    const all_tests_step = b.step("test-all", "Run all tests");
    all_tests_step.dependOn(&run_unit_tests.step);
    all_tests_step.dependOn(&run_integration_tests.step);
}
