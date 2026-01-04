//! Implements a LSP client to explain the language server protocol (LSP).
//!
//! This is NOT meant to be a blueprint on how to design an LSP client.
//! Instead it meant to showcase the various low-level utilities provided by
//! this library to LSP client authors.
//!
//! This library will take care of the tedious boilerplate (stdio, JSON-RPC,
//! LSP data types) while allowing authors to freely decide on how to
//! architect their LSP client.
//!
//!
//! The run this example program with the build system, use the following command:
//! ```
//! zig build run-hello-client -- path/to/unformatted/file.zig /path/to/zls
//! ```
//!
//! Omitting the arguments to the language server will use `./hello_server.zig` as the language server:
//! ```
//! zig build run-hello-client -- path/to/unformatted/file.zig
//! ```
//!
//! See the `usage` below for more information.
//!

const std = @import("std");
const builtin = @import("builtin");
const lsp = @import("lsp");

pub const std_options: std.Options = .{
    .log_level = .info,
};

/// Be aware that the output doesn't clearly show the source (client or server) of the output
const show_langauge_server_stderr: bool = false;

const usage =
    \\hello-client
    \\
    \\Give me a document an I will ask the language server whether it has any suggested changes to format the document.
    \\
    \\Usage:   hello-client /path/to/unformatted/file.zig <language server arguments>
    \\
    \\Example: hello-client file.zig     /path/to/zls
    \\         hello-client ../file.zig  zls
    \\
    \\
;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const gpa = init.gpa;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);

    if (args.len < 3) fatalWithUsage("expected at least 2 arguments but got {d}", .{args.len - 1});

    const input_file = std.Io.Dir.cwd().readFileAlloc(io, args[1], gpa, .limited(std.math.maxInt(u32))) catch |err|
        fatal("failed to read file '{s}': {}", .{ args[1], err });
    defer gpa.free(input_file);

    // Spawn the language server as a child process.
    var child_process = std.process.spawn(io, .{
        .argv = args[2..],
        .stdin = .pipe,
        .stdout = .pipe,
        .stderr = .pipe,
    }) catch |err| fatal("child process could not be created: {}", .{err});

    // Language servers can support multiple communication channels (e.g. stdio, pipes, sockets).
    // See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#implementationConsiderations
    //
    // The `lsp.Transport.Stdio` implements the necessary logic to read and write messages over stdio.
    var read_buffer: [256]u8 = undefined;
    var stdio_transport: lsp.Transport.Stdio = .init(io, &read_buffer, .{ .handle = child_process.stdout.?.handle }, child_process.stdin.?);
    const transport: *lsp.Transport = &stdio_transport.transport;

    // The order of exchanged messages will look similar to this:
    //
    // 1. send `initialize` request and receive response
    // 2. send `initialized` notification
    // 3. send various requests like `textDocument/formatting`
    // 4. send `shutdown` request and receive response
    // 5. send `exit` notification

    std.log.debug("sending 'initialize' request to server", .{});
    try transport.writeRequest(
        gpa,
        .{ .number = 0 },
        "initialize", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
        lsp.types.InitializeParams,
        .{ .capabilities = .{} }, // the client capabilities tell the server what "features" the client supports
        .{ .emit_null_optional_fields = false },
    );

    // Wait for the response from the server
    // For the sake of simplicity, we will block here and read messages until the response to our request has been found. All other messages will be ignored.
    // A more sophisticated client implementation will need to handle messages asynchronously.
    const initialize_response = try readAndIgnoreUntilResponse(gpa, transport, .{ .number = 0 }, "initialize");
    defer initialize_response.deinit();

    const initialize_result: lsp.types.InitializeResult = initialize_response.value;
    _ = initialize_result.capabilities; // the server capabilities tell the client what "features" the server supports

    const serverName = if (initialize_result.serverInfo) |serverInfo| serverInfo.name else "unknown";
    std.log.info("Good morning Mx. {s}.", .{serverName});

    // Check whether the server supports the `textDocument/formatting` request
    const supports_formatting = blk: {
        const documentFormattingProvider = initialize_result.capabilities.documentFormattingProvider orelse break :blk false;
        switch (documentFormattingProvider) {
            .bool => |supported| break :blk supported,
            .document_formatting_options => break :blk true,
        }
    };

    if (!supports_formatting) {
        std.log.err("It seems like you're not qualified for this task. Get out!", .{});
        std.process.exit(1);
    }

    std.log.debug("sending 'initialized' notification to server", .{});
    try transport.writeNotification(
        gpa,
        "initialized", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
        lsp.types.InitializedParams,
        .{},
        .{ .emit_null_optional_fields = false },
    );

    // ----------------

    std.log.info("This document recently came in by the CLI.", .{});
    std.log.debug("sending 'textDocument/didOpen' notification to server", .{});
    try transport.writeNotification(
        gpa,
        "textDocument/didOpen", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_didOpen
        lsp.types.TextDocument.DidOpenParams,
        .{
            .textDocument = .{
                .uri = "untitled:Document", // Usually a file system uri will be provided like 'file:///path/to/main.zig'
                .languageId = .{ .custom_value = "" },
                .text = input_file,
                .version = 0,
            },
        },
        .{ .emit_null_optional_fields = false },
    );

    std.log.info("Just to double check, could you verify that it is formatted correctly?", .{});
    std.log.debug("sending 'textDocument/formatting' request to server", .{});
    try transport.writeRequest(
        gpa,
        .{ .number = 1 },
        "textDocument/formatting", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_formatting
        lsp.types.document_formatting.Params,
        .{
            .textDocument = .{ .uri = "untitled:Document" },
            .options = .{ .tabSize = 4, .insertSpaces = true },
        },
        .{ .emit_null_optional_fields = false },
    );

    const formatting_response = try readAndIgnoreUntilResponse(gpa, transport, .{ .number = 1 }, "textDocument/formatting");
    defer formatting_response.deinit();

    const text_edits = formatting_response.value orelse &.{};
    if (text_edits.len == 0) {
        std.log.info("{s}: I have no comments.", .{serverName});
    } else {
        std.log.info("{s}: I have identified {d} non-compliance(s) with my formatting specification", .{ serverName, text_edits.len });
    }

    // ----------------

    std.log.info("Well, thanks for your insight on this. Now get out!", .{});

    // Even though this is a request, we do not wait for a response because we are going to close the server anyway.
    std.log.debug("sending 'shutdown' request to server", .{});
    try transport.writeRequest(
        gpa,
        .{ .number = 2 },
        "shutdown", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
        void,
        {},
        .{ .emit_null_optional_fields = false },
    );

    std.log.debug("sending 'exit' notification to server", .{});
    try transport.writeNotification(
        gpa,
        "exit", // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
        void,
        {},
        .{ .emit_null_optional_fields = false },
    );

    // The "exit" notification will ask the server to exit its process. Ideally we should wait with a timeout in case the server is not behaving correctly.
    _ = try child_process.wait(io);
}

fn fatalWithUsage(comptime format: []const u8, args: anytype) noreturn {
    {
        const stderr = std.debug.lockStderr(&.{}).terminal();
        defer std.debug.unlockStderr();
        stderr.writer.writeAll(usage) catch {};
    }
    std.log.err(format, args);
    std.process.exit(1);
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    std.process.exit(1);
}

/// Do not use such a function in an actual implementation.
fn readAndIgnoreUntilResponse(
    allocator: std.mem.Allocator,
    transport: *lsp.Transport,
    id: lsp.JsonRPCMessage.ID,
    comptime method: []const u8,
) !std.json.Parsed(lsp.ResultType(method)) {
    while (true) {
        // read the unparsed JSON-RPC message
        const json_message = try transport.readJsonMessage(allocator);
        defer allocator.free(json_message);
        std.log.debug("received message from server: {s}", .{json_message});

        // try to find the "id" field
        const parsed_message = try std.json.parseFromSlice(
            struct { id: ?lsp.JsonRPCMessage.ID = null },
            allocator,
            json_message,
            .{ .ignore_unknown_fields = true },
        );
        defer parsed_message.deinit();

        const actual_id = parsed_message.value.id orelse {
            // std.log.info("received message from server while waiting for '{}'. Ignoring...", .{std.json.fmt(id, .{})});
            continue;
        };

        if (!id.eql(actual_id)) {
            // std.log.info("received message from server while waiting for '{}'. Ignoring...", .{std.json.fmt(id, .{})});
            continue;
        }

        // parse the "result" field to the expected type
        const parsed_response = try std.json.parseFromSlice(
            struct { result: lsp.ResultType(method) },
            allocator,
            json_message,
            .{ .ignore_unknown_fields = true, .allocate = .alloc_always },
        );

        return .{
            .arena = parsed_response.arena,
            .value = parsed_response.value.result,
        };
    }
}
