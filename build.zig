const std = @import("std");
const builtin = @import("builtin");

const minimum_zig_version = @import("build.zig.zon").minimum_zig_version;

pub fn build(b: *std.Build) void {
    comptime if (builtin.zig_version.order(std.SemanticVersion.parse(minimum_zig_version) catch unreachable) == .lt) {
        @compileError(std.fmt.comptimePrint(
            \\Your Zig version does not meet the minimum build requirement:
            \\  required Zig version: {[minimum_zig_version]s}
            \\  actual   Zig version: {[current_version]s}
            \\
        , .{
            .current_version = builtin.zig_version_string,
            .minimum_zig_version = minimum_zig_version,
        }));
    };

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use Zig's llvm code backend");
    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match filter") orelse &[0][]const u8{};

    // -------------------------------------------------------------------------

    const mcp_module = b.addModule("blitzmcp", .{
        .root_source_file = b.path("src/mcp.zig"),
        .target = target,
        .optimize = optimize,
    });

    // -------------------------------- Autodoc --------------------------------

    const autodoc_exe = b.addObject(.{
        .name = "blitzmcp",
        .root_module = mcp_module,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc_exe.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc/blitzmcp",
    });

    const docs_step = b.step("docs", "Generate and install documentation");
    docs_step.dependOn(&install_docs.step);

    // ------------------------------- Examples --------------------------------

    const stdio_server_exe = b.addExecutable(.{
        .name = "stdio-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/stdio_server.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "blitzmcp", .module = mcp_module },
            },
        }),
        .use_llvm = use_llvm,
    });
    b.installArtifact(stdio_server_exe);

    const run_stdio_server = b.addRunArtifact(stdio_server_exe);
    run_stdio_server.addPassthruArgs();

    const run_stdio_server_step = b.step("run-stdio-server", "Run the stdio-server example");
    run_stdio_server_step.dependOn(&run_stdio_server.step);

    // --------------------------------- Tests ---------------------------------

    const mcp_tests = b.addTest(.{
        .root_module = mcp_module,
        .filters = test_filters,
        .use_llvm = use_llvm,
    });

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addRunArtifact(mcp_tests).step);

    // ----------------------------- Code Coverage -----------------------------

    const kcov_bin = b.findProgramLazy(.{
        .names = &.{"kcov"},
    });

    const kcov_merge = std.Build.Step.Run.create(b, "kcov merge coverage");
    kcov_merge.rename_step_with_output_arg = false;
    kcov_merge.addFileArg(kcov_bin);
    kcov_merge.addArg("--merge");
    const coverage_output = kcov_merge.addOutputDirectoryArg(".");

    for ([_]*std.Build.Step.Compile{mcp_tests}) |test_artifact| {
        const kcov_collect = std.Build.Step.Run.create(b, "kcov collect coverage");
        kcov_collect.addFileArg(kcov_bin);
        kcov_collect.addArg("--collect-only");
        kcov_collect.addPrefixedDirectoryArg("--include-path=", b.path("."));
        kcov_merge.addDirectoryArg(kcov_collect.addOutputDirectoryArg(test_artifact.name));
        kcov_collect.addArtifactArg(test_artifact);
        kcov_collect.enableTestRunnerMode();
    }

    const install_coverage = b.addInstallDirectory(.{
        .source_dir = coverage_output,
        .install_dir = .{ .custom = "coverage" },
        .install_subdir = "",
    });

    const coverage_step = b.step("coverage", "Generate a coverage report with kcov");
    coverage_step.dependOn(&install_coverage.step);
}
