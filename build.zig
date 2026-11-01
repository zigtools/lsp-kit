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

    // ---------------------------- End-to-end Tests ---------------------------

    // Drives the example server with the official MCP TypeScript SDK client.
    // Requires Node.js and npm.
    //
    // The test project is copied into the cache and installed there, so the
    // `npm ci` step is cached until one of the copied files changes and the
    // source tree stays clean.
    const e2e_files = b.addWriteFiles();
    _ = e2e_files.addCopyFile(b.path("e2e/package.json"), "package.json");
    _ = e2e_files.addCopyFile(b.path("e2e/package-lock.json"), "package-lock.json");
    const e2e_test_script = e2e_files.addCopyFile(b.path("e2e/stdio_server.test.mjs"), "stdio_server.test.mjs");

    const npm_ci = std.Build.Step.Run.create(b, "npm ci");
    npm_ci.addFileArg(b.findProgramLazy(.{ .names = &.{"npm"} }));
    npm_ci.addArgs(&.{ "ci", "--no-audit", "--no-fund" });
    npm_ci.setCwd(e2e_files.getDirectory());
    // Capturing the output makes the step cacheable.
    _ = npm_ci.captureStdOut(.{});

    const run_e2e = std.Build.Step.Run.create(b, "e2e stdio-server");
    run_e2e.addFileArg(b.findProgramLazy(.{ .names = &.{"node"} }));
    run_e2e.addFileArg(e2e_test_script);
    run_e2e.addArtifactArg(stdio_server_exe);
    run_e2e.has_side_effects = true;
    run_e2e.step.dependOn(&npm_ci.step);

    const e2e_step = b.step("e2e", "Run the end-to-end tests against the example server (requires Node.js)");
    e2e_step.dependOn(&run_e2e.step);

    // ----------------------------------- CI ----------------------------------

    const fmt_check = b.addSystemCommand(&.{ b.graph.zig_exe, "fmt", "--check", "--ast-check" });
    fmt_check.setName("zig fmt --check --ast-check");
    for ([_][]const u8{ "src", "examples", "build.zig", "build.zig.zon" }) |path| {
        fmt_check.addFileArg(b.path(path));
    }

    const ci_step = b.step("ci", "Run everything that CI runs");
    ci_step.dependOn(&fmt_check.step);
    ci_step.dependOn(test_step);
    ci_step.dependOn(b.getInstallStep());
    ci_step.dependOn(docs_step);
    ci_step.dependOn(e2e_step);
}
