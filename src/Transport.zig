//! A type-erased transport for JSON-RPC messages.
//!
//! The vtable only deals in complete JSON messages, framing is an
//! implementation detail of each transport. This allows swapping between
//! `Stdio`, an in-memory `Stream` for testing or a custom (e.g. HTTP) transport
//! without changing any server logic.
//!
//! The MCP stdio transport frames messages as newline-delimited JSON:
//! https://modelcontextprotocol.io/specification/2025-11-25/basic/transports#stdio

const std = @import("std");
const json_rpc = @import("json_rpc.zig");

const Transport = @This();

vtable: *const VTable,

pub const VTable = struct {
    /// Returns the next JSON message. The caller owns the returned memory.
    /// Returns `error.EndOfStream` when there are no more messages.
    readJsonMessage: *const fn (transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ReadError![]u8,
    /// `json_message` must not contain any newline characters. This is
    /// guaranteed when serializing with `.whitespace = .minified`.
    writeJsonMessage: *const fn (transport: *Transport, io: std.Io, json_message: []const u8) WriteError!void,
};

pub const ReadError = std.Io.File.Reader.Error || error{
    EndOfStream,
    /// The message exceeded the transport's size limit. It has been skipped,
    /// so reading can continue with the next message.
    MessageTooLong,
} || std.mem.Allocator.Error;

/// The default maximum size of a single message in bytes.
pub const default_max_message_len: usize = 16 * 1024 * 1024;
pub const WriteError = std.Io.File.Writer.Error;

/// Reads from and writes to a pair of files (usually stdin and stdout) using
/// newline-delimited JSON.
pub const Stdio = struct {
    transport: Transport,
    reader: std.Io.Reader,
    read_from: std.Io.File,
    write_to: std.Io.File,
    /// Longer messages are skipped and reported as `error.MessageTooLong`.
    max_message_len: usize = default_max_message_len,

    pub fn init(
        /// Must not be empty. Messages may be longer than the buffer.
        read_buffer: []u8,
        read_from: std.Io.File,
        write_to: std.Io.File,
    ) Stdio {
        std.debug.assert(read_buffer.len != 0);
        return .{
            .transport = .{
                .vtable = &.{
                    .readJsonMessage = &Stdio.readJsonMessage,
                    .writeJsonMessage = &Stdio.writeJsonMessage,
                },
            },
            .reader = std.Io.File.Reader.initInterface(read_buffer),
            .read_from = read_from,
            .write_to = write_to,
        };
    }

    fn readJsonMessage(transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ReadError![]u8 {
        const stdio: *Stdio = @fieldParentPtr("transport", transport);
        var file_reader: std.Io.File.Reader = .initStreaming(stdio.read_from, io, stdio.reader.buffer);
        file_reader.interface = stdio.reader;
        defer stdio.reader = file_reader.interface;
        return Transport.readNdjsonMessage(&file_reader.interface, allocator, stdio.max_message_len) catch |err| switch (err) {
            error.ReadFailed => return file_reader.err.?,
            else => |e| return e,
        };
    }

    fn writeJsonMessage(transport: *Transport, io: std.Io, json_message: []const u8) WriteError!void {
        const stdio: *Stdio = @fieldParentPtr("transport", transport);
        var file_writer: std.Io.File.Writer = .initStreaming(stdio.write_to, io, &.{});
        Transport.writeNdjsonMessage(&file_writer.interface, json_message) catch |err| switch (err) {
            error.WriteFailed => return file_writer.err.?,
        };
    }
};

/// Reads from and writes to arbitrary `std.Io.Reader`/`std.Io.Writer`
/// interfaces using newline-delimited JSON. Useful for testing with
/// `std.Io.Reader.fixed` and `std.Io.Writer.Allocating`.
pub const Stream = struct {
    transport: Transport,
    reader: *std.Io.Reader,
    writer: *std.Io.Writer,
    /// Longer messages are skipped and reported as `error.MessageTooLong`.
    max_message_len: usize = default_max_message_len,

    pub fn init(reader: *std.Io.Reader, writer: *std.Io.Writer) Stream {
        return .{
            .transport = .{
                .vtable = &.{
                    .readJsonMessage = &Stream.readJsonMessage,
                    .writeJsonMessage = &Stream.writeJsonMessage,
                },
            },
            .reader = reader,
            .writer = writer,
        };
    }

    fn readJsonMessage(transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ReadError![]u8 {
        _ = io;
        const stream: *Stream = @fieldParentPtr("transport", transport);
        return Transport.readNdjsonMessage(stream.reader, allocator, stream.max_message_len) catch |err| switch (err) {
            error.ReadFailed => return error.Unexpected,
            else => |e| return e,
        };
    }

    fn writeJsonMessage(transport: *Transport, io: std.Io, json_message: []const u8) WriteError!void {
        _ = io;
        const stream: *Stream = @fieldParentPtr("transport", transport);
        Transport.writeNdjsonMessage(stream.writer, json_message) catch |err| switch (err) {
            error.WriteFailed => return error.Unexpected,
        };
    }
};

/// Consider using `readJsonMessageUncancelable` to avoid `error.Canceled` being returned.
pub fn readJsonMessage(transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ReadError![]u8 {
    return try transport.vtable.readJsonMessage(transport, io, allocator);
}

pub fn readJsonMessageUncancelable(transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ![]u8 {
    const old_cancel_protect = io.swapCancelProtection(.blocked);
    defer _ = io.swapCancelProtection(old_cancel_protect);
    return Transport.readJsonMessage(transport, io, allocator) catch |err| switch (err) {
        error.Canceled => unreachable,
        else => |e| return e,
    };
}

/// Consider using `writeJsonMessageUncancelable` to avoid `error.Canceled` being returned.
pub fn writeJsonMessage(transport: *Transport, io: std.Io, json_message: []const u8) WriteError!void {
    return try transport.vtable.writeJsonMessage(transport, io, json_message);
}

pub fn writeJsonMessageUncancelable(transport: *Transport, io: std.Io, json_message: []const u8) !void {
    const old_cancel_protect = io.swapCancelProtection(.blocked);
    defer _ = io.swapCancelProtection(old_cancel_protect);
    Transport.writeJsonMessage(transport, io, json_message) catch |err| switch (err) {
        error.Canceled => unreachable,
        else => |e| return e,
    };
}

pub fn writeRequest(
    transport: *Transport,
    io: std.Io,
    allocator: std.mem.Allocator,
    id: json_rpc.ID,
    method: []const u8,
    comptime Params: type,
    params: Params,
    options: std.json.Stringify.Options,
) (WriteError || std.mem.Allocator.Error)!void {
    const request: json_rpc.TypedJsonRPCRequest(Params) = .{
        .id = id,
        .method = method,
        .params = params,
    };
    try transport.writeValue(io, allocator, request, options);
}

pub fn writeNotification(
    transport: *Transport,
    io: std.Io,
    allocator: std.mem.Allocator,
    method: []const u8,
    comptime Params: type,
    params: Params,
    options: std.json.Stringify.Options,
) (WriteError || std.mem.Allocator.Error)!void {
    const notification: json_rpc.TypedJsonRPCNotification(Params) = .{
        .method = method,
        .params = params,
    };
    try transport.writeValue(io, allocator, notification, options);
}

pub fn writeResponse(
    transport: *Transport,
    io: std.Io,
    allocator: std.mem.Allocator,
    id: ?json_rpc.ID,
    comptime Result: type,
    result: Result,
    options: std.json.Stringify.Options,
) (WriteError || std.mem.Allocator.Error)!void {
    const response: json_rpc.TypedJsonRPCResponse(Result) = .{
        .id = id,
        .result_or_error = .{ .result = result },
    };
    try transport.writeValue(io, allocator, response, options);
}

pub fn writeErrorResponse(
    transport: *Transport,
    io: std.Io,
    allocator: std.mem.Allocator,
    id: ?json_rpc.ID,
    err: json_rpc.JsonRPCMessage.Response.Error,
    options: std.json.Stringify.Options,
) (WriteError || std.mem.Allocator.Error)!void {
    const response: json_rpc.TypedJsonRPCResponse(void) = .{
        .id = id,
        .result_or_error = .{ .@"error" = err },
    };
    try transport.writeValue(io, allocator, response, options);
}

/// Serializes `value` and writes it as a single message.
/// `options.whitespace` is ignored, messages are always minified.
fn writeValue(
    transport: *Transport,
    io: std.Io,
    allocator: std.mem.Allocator,
    value: anytype,
    options: std.json.Stringify.Options,
) (WriteError || std.mem.Allocator.Error)!void {
    var minified_options = options;
    minified_options.whitespace = .minified;
    const json_message = try std.json.Stringify.valueAlloc(allocator, value, minified_options);
    defer allocator.free(json_message);
    try transport.writeJsonMessage(io, json_message);
}

pub const ThreadSafeConfig = struct {
    /// Makes `readJsonMessage` thread-safe.
    thread_safe_read: bool,
    /// Makes `writeJsonMessage` thread-safe.
    thread_safe_write: bool,
    MutexType: type = std.Io.Mutex,
};

/// Wraps a non-thread-safe transport and makes it thread-safe.
pub fn ThreadSafe(config: ThreadSafeConfig) type {
    return struct {
        transport: Transport,
        child_transport: *Transport,
        in_mutex: @TypeOf(in_mutex_init) = in_mutex_init,
        out_mutex: @TypeOf(out_mutex_init) = out_mutex_init,

        const Self = @This();

        pub fn init(child_transport: *Transport) Self {
            return .{
                .transport = .{
                    .vtable = &.{
                        .readJsonMessage = Self.readJsonMessage,
                        .writeJsonMessage = Self.writeJsonMessage,
                    },
                },
                .child_transport = child_transport,
            };
        }

        pub fn readJsonMessage(transport: *Transport, io: std.Io, allocator: std.mem.Allocator) ReadError![]u8 {
            const self: *Self = @fieldParentPtr("transport", transport);

            try self.in_mutex.lock(io);
            defer self.in_mutex.unlock(io);

            return try self.child_transport.readJsonMessage(io, allocator);
        }

        pub fn writeJsonMessage(transport: *Transport, io: std.Io, json_message: []const u8) WriteError!void {
            const self: *Self = @fieldParentPtr("transport", transport);

            try self.out_mutex.lock(io);
            defer self.out_mutex.unlock(io);

            return try self.child_transport.writeJsonMessage(io, json_message);
        }

        const in_mutex_init = if (config.thread_safe_read)
            config.MutexType.init
        else
            DummyMutex{};

        const out_mutex_init = if (config.thread_safe_write)
            config.MutexType.init
        else
            DummyMutex{};

        const DummyMutex = struct {
            pub fn lock(_: *DummyMutex, _: std.Io) !void {}
            pub fn unlock(_: *DummyMutex, _: std.Io) void {}
        };
    };
}

/// Reads the next newline-delimited JSON message. Empty lines are skipped and
/// a trailing `\r` is stripped. A final message without a trailing newline is
/// accepted. The message length is not limited by the reader's buffer size.
///
/// A line longer than `max_len` bytes is discarded without buffering it and
/// `error.MessageTooLong` is returned. The next call continues with the
/// following line.
///
/// The caller owns the returned memory.
pub fn readNdjsonMessage(
    reader: *std.Io.Reader,
    allocator: std.mem.Allocator,
    max_len: usize,
) (std.Io.Reader.Error || std.mem.Allocator.Error || error{MessageTooLong})![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    while (true) {
        aw.clearRetainingCapacity();
        _ = reader.streamDelimiterLimit(&aw.writer, '\n', .limited(max_len +| 1)) catch |err| switch (err) {
            error.WriteFailed => return error.OutOfMemory,
            error.ReadFailed => return error.ReadFailed,
            error.StreamTooLong => {
                _ = reader.discardDelimiterExclusive('\n') catch |e| switch (e) {
                    error.ReadFailed => return error.ReadFailed,
                };
                if (reader.bufferedLen() != 0) reader.toss(1); // the '\n' delimiter
                return error.MessageTooLong;
            },
        };
        const at_end = reader.bufferedLen() == 0;
        if (!at_end) reader.toss(1); // the '\n' delimiter

        const line = std.mem.trimEnd(u8, aw.written(), "\r");
        if (std.mem.trim(u8, line, " \t").len == 0) {
            if (at_end) return error.EndOfStream;
            continue;
        }

        aw.shrinkRetainingCapacity(line.len);
        return try aw.toOwnedSlice();
    }
}

test readNdjsonMessage {
    var reader: std.Io.Reader = .fixed("{}\n\r\n\n{\"a\":1}\r\n  \n[]");

    const first = try readNdjsonMessage(&reader, std.testing.allocator, default_max_message_len);
    defer std.testing.allocator.free(first);
    try std.testing.expectEqualStrings("{}", first);

    const second = try readNdjsonMessage(&reader, std.testing.allocator, default_max_message_len);
    defer std.testing.allocator.free(second);
    try std.testing.expectEqualStrings("{\"a\":1}", second);

    const third = try readNdjsonMessage(&reader, std.testing.allocator, default_max_message_len);
    defer std.testing.allocator.free(third);
    try std.testing.expectEqualStrings("[]", third);

    try std.testing.expectError(error.EndOfStream, readNdjsonMessage(&reader, std.testing.allocator, default_max_message_len));
}

test "readNdjsonMessage - message larger than reader buffer" {
    const message = "{\"text\":\"" ++ @as([300]u8, @splat('x')) ++ "\"}";
    var input: std.Io.Reader = .fixed(message ++ "\n");
    var buffer: [16]u8 = undefined;
    var limited = input.limited(.unlimited, &buffer);

    const result = try readNdjsonMessage(&limited.interface, std.testing.allocator, default_max_message_len);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(message, result);

    try std.testing.expectError(error.EndOfStream, readNdjsonMessage(&limited.interface, std.testing.allocator, default_max_message_len));
}

test "readNdjsonMessage - message too long" {
    var input: std.Io.Reader = .fixed("12345\n123456\n" ++ @as([300]u8, @splat('x')) ++ "\n{}\n1234567");
    var buffer: [4]u8 = undefined;
    var limited = input.limited(.unlimited, &buffer);
    const reader = &limited.interface;

    const exact = try readNdjsonMessage(reader, std.testing.allocator, 5);
    defer std.testing.allocator.free(exact);
    try std.testing.expectEqualStrings("12345", exact);

    try std.testing.expectError(error.MessageTooLong, readNdjsonMessage(reader, std.testing.allocator, 5));
    try std.testing.expectError(error.MessageTooLong, readNdjsonMessage(reader, std.testing.allocator, 5));

    const next = try readNdjsonMessage(reader, std.testing.allocator, 5);
    defer std.testing.allocator.free(next);
    try std.testing.expectEqualStrings("{}", next);

    try std.testing.expectError(error.MessageTooLong, readNdjsonMessage(reader, std.testing.allocator, 5));
    try std.testing.expectError(error.EndOfStream, readNdjsonMessage(reader, std.testing.allocator, 5));
}

/// Writes `json_message` followed by a newline and flushes the writer.
/// Asserts that `json_message` contains no newline characters.
pub fn writeNdjsonMessage(writer: *std.Io.Writer, json_message: []const u8) std.Io.Writer.Error!void {
    std.debug.assert(std.mem.findScalar(u8, json_message, '\n') == null);
    var data: [2][]const u8 = .{ json_message, "\n" };
    try writer.writeVecAll(&data);
    try writer.flush();
}

test writeNdjsonMessage {
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();

    try writeNdjsonMessage(&aw.writer, "{}");
    try writeNdjsonMessage(&aw.writer, "[]");
    try std.testing.expectEqualStrings("{}\n[]\n", aw.written());
}

test "Stream - write helpers emit minified messages" {
    var input: std.Io.Reader = .fixed("");
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();

    var stream: Stream = .init(&input, &aw.writer);
    const transport = &stream.transport;
    const io = std.testing.io;

    try transport.writeRequest(io, std.testing.allocator, .{ .number = 0 }, "ping", void, {}, .{ .whitespace = .indent_2 });
    try transport.writeNotification(io, std.testing.allocator, "notifications/initialized", void, {}, .{ .emit_null_optional_fields = false });
    try transport.writeResponse(io, std.testing.allocator, .{ .string = "a" }, struct {}, .{}, .{});
    try transport.writeErrorResponse(io, std.testing.allocator, null, .{ .code = .method_not_found, .message = "nope" }, .{ .emit_null_optional_fields = false });

    try std.testing.expectEqualStrings(
        \\{"jsonrpc":"2.0","id":0,"method":"ping"}
        \\{"jsonrpc":"2.0","method":"notifications/initialized"}
        \\{"jsonrpc":"2.0","id":"a","result":{}}
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32601,"message":"nope"}}
        \\
    , aw.written());

    try std.testing.expectError(error.EndOfStream, transport.readJsonMessage(io, std.testing.allocator));
}

test "Stream - read and write uncancelable" {
    var input: std.Io.Reader = .fixed("{\"a\":1}\n");
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();

    var stream: Stream = .init(&input, &aw.writer);
    const transport = &stream.transport;
    const io = std.testing.io;

    const message = try transport.readJsonMessageUncancelable(io, std.testing.allocator);
    defer std.testing.allocator.free(message);
    try std.testing.expectEqualStrings("{\"a\":1}", message);
    try std.testing.expectError(error.EndOfStream, transport.readJsonMessageUncancelable(io, std.testing.allocator));

    try transport.writeJsonMessageUncancelable(io, message);
    try std.testing.expectEqualStrings("{\"a\":1}\n", aw.written());
}
