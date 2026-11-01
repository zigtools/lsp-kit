//! A small framework that implements the core loop of an MCP server.
//!
//! Dispatch happens on two levels:
//!
//! 1. Protocol level: JSON-RPC methods like `initialize`, `ping`,
//!    `tools/list` or `resources/read`. Most are implemented by the server
//!    itself. Any of them can be implemented (or overridden) by adding a public
//!    function to the `Handler` whose name is the method name, e.g.
//!    `pub fn @"resources/read"(...)`. See `types.client_requests` and
//!    `types.client_notifications` for the available methods.
//! 2. Primitive level: tools and prompts are registered at comptime in the
//!    server config. `tools/list` and `prompts/list` are generated from the
//!    config (including JSON Schemas) and `tools/call`/`prompts/get` are routed
//!    to the matching function based on `params.name`.
//!
//! Handler functions declare which arguments they want by type. Each
//! parameter may be one of:
//!
//! - `Handler`, `*Handler` or `*const Handler`: the handler
//! - `std.mem.Allocator`: an arena that is freed after the message has been handled
//! - `*mcp.Context`: request context for sending progress, log messages and notifications
//! - any other type: the parsed params (protocol level) or arguments (tools/prompts)
//!
//! Also see `examples/stdio_server.zig`.

const std = @import("std");
const json_rpc = @import("json_rpc.zig");
const types = @import("types.zig");
const schema = @import("schema.zig");
const Transport = @import("Transport.zig");

const log = std.log.scoped(.mcp);

const stringify_options: std.json.Stringify.Options = .{ .emit_null_optional_fields = false };

/// Zig errors that are translated into specific JSON-RPC error codes when
/// returned by a protocol level handler function. Any other error is reported
/// as an `internal_error`.
pub const Error = error{
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    ResourceNotFound,
};

fn errorCode(err: anyerror) json_rpc.JsonRPCMessage.Response.Error.Code {
    return switch (err) {
        error.ParseError => .parse_error,
        error.InvalidRequest => .invalid_request,
        error.MethodNotFound => .method_not_found,
        error.InvalidParams => .invalid_params,
        error.ResourceNotFound => .resource_not_found,
        else => .internal_error,
    };
}

/// Passed to handler functions that request a `*Context` parameter.
pub const Context = struct {
    io: std.Io,
    /// Freed after the current message has been handled.
    arena: std.mem.Allocator,
    transport: *Transport,
    /// `null` when handling a notification.
    request_id: ?json_rpc.ID,
    /// Set if the client requested progress notifications for this request.
    progress_token: ?types.ProgressToken,
    /// The minimum log level requested by the client via `logging/setLevel`.
    /// `null` if the client has not set a level.
    log_level: ?types.LoggingLevel,
    /// Set by `fail`. Replaces the error name in the reported message if the
    /// handler returns `error_message_for`.
    error_message: ?[]const u8 = null,
    error_message_for: ?anyerror = null,

    pub const SendError = Transport.WriteError || std.mem.Allocator.Error || JsonValueError;

    pub fn sendNotification(ctx: *Context, method: []const u8, comptime Params: type, params: Params) SendError!void {
        try checkJson(params);
        try ctx.transport.writeNotification(ctx.io, ctx.arena, method, Params, params, stringify_options);
    }

    /// Sends a `notifications/progress` notification. Does nothing if the
    /// client did not request progress notifications.
    pub fn sendProgress(ctx: *Context, progress: f64, total: ?f64, message: ?[]const u8) SendError!void {
        const token = ctx.progress_token orelse return;
        try ctx.sendNotification("notifications/progress", types.ProgressNotificationParams, .{
            .progressToken = token,
            .progress = progress,
            .total = total,
            .message = message,
        });
    }

    /// Sends a `notifications/message` notification if `level` is at least the
    /// level requested by the client. `data` may be any JSON serializable value.
    pub fn log(ctx: *Context, level: types.LoggingLevel, logger: ?[]const u8, data: anytype) SendError!void {
        if (ctx.log_level) |min_level| {
            if (@backingInt(level) < @backingInt(min_level)) return;
        }
        const Params = struct {
            level: types.LoggingLevel,
            logger: ?[]const u8,
            data: @TypeOf(data),
        };
        try ctx.sendNotification("notifications/message", Params, .{ .level = level, .logger = logger, .data = data });
    }

    /// Like `log` with a formatted string as `data`.
    pub fn logf(ctx: *Context, level: types.LoggingLevel, comptime fmt: []const u8, args: anytype) SendError!void {
        try ctx.log(level, null, try std.fmt.allocPrint(ctx.arena, fmt, args));
    }

    /// Returns `err` and reports the formatted message instead of the error
    /// name if the handler returns `err`.
    pub fn fail(ctx: *Context, err: anyerror, comptime fmt: []const u8, args: anytype) anyerror {
        ctx.error_message = std.fmt.allocPrint(ctx.arena, fmt, args) catch null;
        ctx.error_message_for = err;
        return err;
    }

    /// The message to report for `err`. Invalid UTF-8 is replaced with U+FFFD.
    fn errorMessage(ctx: *Context, err: anyerror) std.mem.Allocator.Error![]const u8 {
        const matches = if (ctx.error_message_for) |failed| failed == err else false;
        const message = if (matches) ctx.error_message orelse @errorName(err) else @errorName(err);
        return sanitizeUtf8(ctx.arena, message);
    }
};

/// Returns a server type for the given `Handler` and comptime `config`.
///
/// `config` is a struct literal with the following fields:
///
/// - `.name: []const u8` (required)
/// - `.version: []const u8` (required)
/// - `.title: []const u8`
/// - `.description: []const u8`
/// - `.instructions: []const u8`
/// - `.tools`: a tuple of tool definitions:
///   - `.name: []const u8` (required)
///   - `.call: fn` (required) — the arguments struct is inferred from the
///     function signature and its JSON Schema is generated with `schema.generateObject`.
///     The function may return `types.CallToolResult`, `[]const u8` (text
///     content), `types.ContentBlock`, `[]const types.ContentBlock`, `void`, or
///     any other JSON serializable type. Structs are returned as structured
///     content and also produce an `outputSchema`. Errors are reported as tool
///     errors (`isError = true`).
///   - `.title: []const u8`
///   - `.description: []const u8`
///   - `.annotations`: fields of `types.ToolAnnotations`
///   - `.Args: type` — optional, must match the inferred arguments type
/// - `.prompts`: a tuple of prompt definitions:
///   - `.name: []const u8` (required)
///   - `.call: fn` (required) — the arguments struct may only contain string
///     fields (`[]const u8` or `?[]const u8`). The function may return
///     `types.GetPromptResult`, `[]const types.PromptMessage` or `[]const u8`
///     (a single user message).
///   - `.title: []const u8`
///   - `.description: []const u8`
pub fn Server(comptime Handler: type, comptime config: anytype) type {
    const Config = @TypeOf(config);
    const tool_defs = if (@hasField(Config, "tools")) config.tools else .{};
    const prompt_defs = if (@hasField(Config, "prompts")) config.prompts else .{};

    comptime validateDefinitions(Handler, tool_defs, "tool");
    comptime validateDefinitions(Handler, prompt_defs, "prompt");

    return struct {
        handler: *Handler,
        /// The minimum log level requested by the client via `logging/setLevel`.
        log_level: ?types.LoggingLevel = null,

        const Self = @This();

        pub const server_info: types.Implementation = .{
            .name = config.name,
            .version = config.version,
            .title = optionalField(config, "title"),
            .description = optionalField(config, "description"),
        };

        pub const instructions: ?[]const u8 = optionalField(config, "instructions");

        /// The result of `tools/list`, generated at comptime.
        pub const tools: []const types.Tool = &tool_list;
        const tool_list: [tool_defs.len]types.Tool = blk: {
            var list: [tool_defs.len]types.Tool = undefined;
            for (&list, tool_defs) |*tool, def| {
                const Result = PayloadType(FunctionReturnType(def.call));
                tool.* = .{
                    .name = def.name,
                    .title = optionalField(def, "title"),
                    .description = optionalField(def, "description"),
                    .inputSchema = .{ .raw = schema.generateObject(ArgsType(Handler, def)) },
                    .outputSchema = if (hasStructuredOutput(Result)) .{ .raw = schema.generateObject(Result) } else null,
                    .annotations = if (@hasField(@TypeOf(def), "annotations")) coerceStruct(types.ToolAnnotations, def.annotations) else null,
                };
            }
            break :blk list;
        };

        /// The result of `prompts/list`, generated at comptime.
        pub const prompts: []const types.Prompt = &prompt_list;
        const prompt_list: [prompt_defs.len]types.Prompt = blk: {
            var list: [prompt_defs.len]types.Prompt = undefined;
            for (&list, prompt_defs) |*prompt, def| {
                const Args = ArgsType(Handler, def);
                prompt.* = .{
                    .name = def.name,
                    .title = optionalField(def, "title"),
                    .description = optionalField(def, "description"),
                    .arguments = if (Args == void) null else promptArguments(Args),
                };
            }
            break :blk list;
        };

        pub const capabilities: types.ServerCapabilities = .{
            .logging = .{},
            .completions = if (@hasDecl(Handler, "completion/complete")) .{} else null,
            .prompts = if (prompt_defs.len != 0 or @hasDecl(Handler, "prompts/list")) .{} else null,
            .resources = if (@hasDecl(Handler, "resources/list") or
                @hasDecl(Handler, "resources/read") or
                @hasDecl(Handler, "resources/templates/list"))
                .{ .subscribe = if (@hasDecl(Handler, "resources/subscribe")) true else null }
            else
                null,
            .tools = if (tool_defs.len != 0 or @hasDecl(Handler, "tools/list")) .{} else null,
        };

        pub fn init(handler: *Handler) Self {
            return .{ .handler = handler };
        }

        /// The response to `initialize` that is sent unless the `Handler`
        /// implements `initialize` itself. Can be used as a starting point in
        /// a custom `initialize` implementation.
        pub fn defaultInitializeResult(params: types.InitializeRequestParams) types.InitializeResult {
            return .{
                .protocolVersion = negotiateProtocolVersion(params.protocolVersion),
                .capabilities = capabilities,
                .serverInfo = server_info,
                .instructions = instructions,
            };
        }

        /// If the server supports the requested protocol version, it responds
        /// with the same version. Otherwise, it responds with the latest version
        /// it supports.
        pub fn negotiateProtocolVersion(requested: []const u8) []const u8 {
            return if (types.isSupportedProtocolVersion(requested)) requested else types.latest_protocol_version;
        }

        /// Reads and handles messages until the transport reaches the end of
        /// the stream (e.g. stdin has been closed).
        pub fn run(self: *Self, io: std.Io, gpa: std.mem.Allocator, transport: *Transport) !void {
            while (true) {
                const json_message = transport.readJsonMessage(io, gpa) catch |err| switch (err) {
                    error.EndOfStream => return,
                    error.MessageTooLong => {
                        log.warn("skipped a message that exceeds the transport's size limit", .{});
                        try writeFallbackError(io, transport, null, .invalid_request, "Message too long");
                        continue;
                    },
                    else => |e| return e,
                };
                defer gpa.free(json_message);

                try self.handleMessage(io, gpa, transport, json_message);
            }
        }

        /// Handles a single JSON-RPC message. Responses and notifications are
        /// written to `transport`. This function is independent of the
        /// framing so that it can be reused by other transports (e.g. HTTP).
        ///
        /// Running out of memory while handling a request is reported to the
        /// client as an `internal_error` instead of being returned.
        pub fn handleMessage(
            self: *Self,
            io: std.Io,
            gpa: std.mem.Allocator,
            transport: *Transport,
            json_message: []const u8,
        ) Transport.WriteError!void {
            var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
            defer arena_allocator.deinit();

            var kind: MessageKind = .unknown;
            self.handleMessageInner(io, arena_allocator.allocator(), transport, json_message, &kind) catch |err| switch (err) {
                error.OutOfMemory => {
                    log.warn("ran out of memory while handling a message", .{});
                    switch (kind) {
                        // Like a parse error: the id cannot be determined.
                        .unknown => try writeFallbackError(io, transport, null, .internal_error, "OutOfMemory"),
                        .request => |id| try writeFallbackError(io, transport, id, .internal_error, "OutOfMemory"),
                        // Notifications and responses must not be answered.
                        .other => {},
                    }
                },
                else => |e| return e,
            };
        }

        fn handleMessageInner(
            self: *Self,
            io: std.Io,
            arena: std.mem.Allocator,
            transport: *Transport,
            json_message: []const u8,
            kind: *MessageKind,
        ) (Transport.WriteError || std.mem.Allocator.Error)!void {
            const value = std.json.parseFromSliceLeaky(std.json.Value, arena, json_message, .{}) catch |err| {
                if (err == error.OutOfMemory) return error.OutOfMemory;
                log.debug("failed to parse message: {}", .{err});
                return transport.writeErrorResponse(io, arena, null, .{ .code = .parse_error, .message = "Parse error" }, stringify_options);
            };
            kind.* = if (recoverId(value)) |id| .{ .request = id } else .other;

            const message = std.json.parseFromValueLeaky(json_rpc.JsonRPCMessage, arena, value, .{ .ignore_unknown_fields = true }) catch |err| {
                if (err == error.OutOfMemory) return error.OutOfMemory;
                log.debug("received an invalid message: {}", .{err});
                return transport.writeErrorResponse(io, arena, recoverId(value), .{ .code = .invalid_request, .message = "Invalid Request" }, stringify_options);
            };

            var ctx: Context = .{
                .io = io,
                .arena = arena,
                .transport = transport,
                .request_id = null,
                .progress_token = null,
                .log_level = self.log_level,
            };

            switch (message) {
                .request => |request| {
                    ctx.request_id = request.id;
                    ctx.progress_token = extractProgressToken(request.params);
                    try self.handleRequest(&ctx, request);
                },
                .notification => |notification| self.handleNotification(&ctx, notification),
                .response => |response| {
                    if (@hasDecl(Handler, "onResponse")) {
                        toErrorUnion(callInjected(self.handler, &ctx, Handler.onResponse, response)) catch |err| {
                            log.err("failed to handle response: {}", .{err});
                        };
                    }
                },
            }
        }

        fn handleRequest(self: *Self, ctx: *Context, request: json_rpc.JsonRPCMessage.Request) !void {
            inline for (comptime std.meta.declarations(types.client_requests)) |method| {
                if ((comptime isImplemented(method)) and std.mem.eql(u8, request.method, method)) {
                    return self.handleKnownRequest(method, ctx, request);
                }
            }
            const message = try std.fmt.allocPrint(ctx.arena, "Method not found: {s}", .{request.method});
            try ctx.transport.writeErrorResponse(ctx.io, ctx.arena, request.id, .{ .code = .method_not_found, .message = message }, stringify_options);
        }

        fn handleKnownRequest(self: *Self, comptime method: []const u8, ctx: *Context, request: json_rpc.JsonRPCMessage.Request) !void {
            const M = @field(types.client_requests, method);

            const params: M.Params = if (M.Params == void) {} else std.json.parseFromValueLeaky(
                M.Params,
                ctx.arena,
                paramsOrEmpty(request.params),
                .{ .ignore_unknown_fields = true },
            ) catch |err| {
                const message = try std.fmt.allocPrint(ctx.arena, "Invalid params: {s}", .{@errorName(err)});
                return ctx.transport.writeErrorResponse(ctx.io, ctx.arena, request.id, .{ .code = .invalid_params, .message = message }, stringify_options);
            };

            const result_or_error = if (@hasDecl(Handler, method)) blk: {
                const result = callInjected(self.handler, ctx, @field(Handler, method), params);
                if (PayloadType(@TypeOf(result)) != M.Result) {
                    @compileError(std.fmt.comptimePrint(
                        \\The '{}.{f}' function has an unexpected result type:
                        \\
                        \\Expected: {}
                        \\Actual:   {}
                    , .{ Handler, std.zig.fmtId(method), M.Result, PayloadType(@TypeOf(result)) }));
                }
                const result_or_error = toErrorUnion(result);
                // Keep the server state in sync when the Handler overrides a stateful method.
                if (comptime std.mem.eql(u8, method, "logging/setLevel")) {
                    if (result_or_error) |_| self.log_level = params.level else |_| {}
                }
                break :blk result_or_error;
            } else self.builtin(method, ctx, params);

            if (result_or_error) |result| {
                checkJson(result) catch |err| {
                    log.warn("invalid result for '{s}' request: {s}", .{ method, jsonValueErrorMessage(err) });
                    return ctx.transport.writeErrorResponse(ctx.io, ctx.arena, request.id, .{
                        .code = .internal_error,
                        .message = try std.fmt.allocPrint(ctx.arena, "Invalid result: {s}", .{jsonValueErrorMessage(err)}),
                    }, stringify_options);
                };
                try ctx.transport.writeResponse(ctx.io, ctx.arena, request.id, M.Result, result, stringify_options);
            } else |err| {
                if (err == error.OutOfMemory) return error.OutOfMemory;
                const code = errorCode(err);
                if (code == .internal_error) {
                    log.err("failed to handle '{s}' request: {}", .{ method, err });
                } else {
                    log.debug("failed to handle '{s}' request: {}", .{ method, err });
                }
                try ctx.transport.writeErrorResponse(ctx.io, ctx.arena, request.id, .{
                    .code = code,
                    .message = try ctx.errorMessage(err),
                }, stringify_options);
            }
        }

        /// Whether the request `method` is handled by the `Handler` or the server itself.
        pub fn isImplemented(comptime method: []const u8) bool {
            if (@hasDecl(Handler, method)) return true;
            const eql = std.mem.eql;
            if (eql(u8, method, "initialize") or eql(u8, method, "ping") or eql(u8, method, "logging/setLevel")) return true;
            if (eql(u8, method, "tools/list") or eql(u8, method, "tools/call")) return tool_defs.len != 0;
            if (eql(u8, method, "prompts/list") or eql(u8, method, "prompts/get")) return prompt_defs.len != 0;
            return false;
        }

        fn builtin(
            self: *Self,
            comptime method: []const u8,
            ctx: *Context,
            params: @field(types.client_requests, method).Params,
        ) anyerror!@field(types.client_requests, method).Result {
            if (comptime std.mem.eql(u8, method, "initialize")) return defaultInitializeResult(params);
            if (comptime std.mem.eql(u8, method, "ping")) return .{};
            if (comptime std.mem.eql(u8, method, "logging/setLevel")) {
                self.log_level = params.level;
                return .{};
            }
            if (comptime std.mem.eql(u8, method, "tools/list")) return .{ .tools = tools };
            if (comptime std.mem.eql(u8, method, "tools/call")) return self.callTool(ctx, params);
            if (comptime std.mem.eql(u8, method, "prompts/list")) return .{ .prompts = prompts };
            if (comptime std.mem.eql(u8, method, "prompts/get")) return self.getPrompt(ctx, params);
            comptime unreachable; // see `isImplemented`
        }

        fn callTool(self: *Self, ctx: *Context, params: types.CallToolRequestParams) anyerror!types.CallToolResult {
            inline for (tool_defs) |def| {
                if (std.mem.eql(u8, params.name, def.name)) {
                    return self.invokeTool(def, ctx, params.arguments);
                }
            }
            return ctx.fail(error.InvalidParams, "Unknown tool: {s}", .{params.name});
        }

        fn invokeTool(self: *Self, comptime def: anytype, ctx: *Context, arguments: ?std.json.Value) std.mem.Allocator.Error!types.CallToolResult {
            const Args = ArgsType(Handler, def);
            const args: Args = if (Args == void) {} else std.json.parseFromValueLeaky(
                Args,
                ctx.arena,
                paramsOrEmpty(arguments),
                .{ .ignore_unknown_fields = true },
            ) catch |err| {
                return toolError(ctx.arena, "Invalid arguments for tool '{s}': {s}", .{ def.name, @errorName(err) });
            };

            const result = toErrorUnion(callInjected(self.handler, ctx, def.call, args)) catch |err| {
                if (err == error.OutOfMemory) return error.OutOfMemory;
                return toolError(ctx.arena, "{s}", .{try ctx.errorMessage(err)});
            };
            checkJson(result) catch |err| return toolError(ctx.arena, "Invalid tool result: {s}", .{jsonValueErrorMessage(err)});
            return try toCallToolResult(ctx.arena, result);
        }

        fn getPrompt(self: *Self, ctx: *Context, params: types.GetPromptRequestParams) anyerror!types.GetPromptResult {
            inline for (prompt_defs) |def| {
                if (std.mem.eql(u8, params.name, def.name)) {
                    const Args = ArgsType(Handler, def);
                    const args: Args = if (Args == void) {} else std.json.parseFromValueLeaky(
                        Args,
                        ctx.arena,
                        paramsOrEmpty(params.arguments),
                        .{ .ignore_unknown_fields = true },
                    ) catch |err| {
                        return ctx.fail(error.InvalidParams, "Invalid arguments for prompt '{s}': {s}", .{ def.name, @errorName(err) });
                    };
                    const result = try toErrorUnion(callInjected(self.handler, ctx, def.call, args));
                    return toGetPromptResult(ctx.arena, result);
                }
            }
            return ctx.fail(error.InvalidParams, "Unknown prompt: {s}", .{params.name});
        }

        fn handleNotification(self: *Self, ctx: *Context, notification: json_rpc.JsonRPCMessage.Notification) void {
            inline for (comptime std.meta.declarations(types.client_notifications)) |method| {
                if (@hasDecl(Handler, method) and std.mem.eql(u8, notification.method, method)) {
                    const M = @field(types.client_notifications, method);
                    const params: M.Params = if (M.Params == void) {} else std.json.parseFromValueLeaky(
                        M.Params,
                        ctx.arena,
                        paramsOrEmpty(notification.params),
                        .{ .ignore_unknown_fields = true },
                    ) catch |err| {
                        log.err("invalid params for '{s}' notification: {}", .{ method, err });
                        return;
                    };
                    const result = callInjected(self.handler, ctx, @field(Handler, method), params);
                    if (PayloadType(@TypeOf(result)) != void) {
                        @compileError(std.fmt.comptimePrint("The '{}.{f}' function must return void", .{ Handler, std.zig.fmtId(method) }));
                    }
                    toErrorUnion(result) catch |err| {
                        log.err("failed to handle '{s}' notification: {}", .{ method, err });
                    };
                    return;
                }
            }
        }
    };
}

fn validateDefinitions(comptime Handler: type, comptime defs: anytype, comptime kind: []const u8) void {
    const info = @typeInfo(@TypeOf(defs));
    if (info != .@"struct" or !info.@"struct".is_tuple) {
        @compileError("expected the " ++ kind ++ "s to be a tuple, e.g. `.{ .{ .name = ..., .call = ... } }`");
    }
    for (defs, 0..) |def, i| {
        if (!@hasField(@TypeOf(def), "name")) @compileError(std.fmt.comptimePrint("{s} #{d} is missing the `.name` field", .{ kind, i }));
        if (!@hasField(@TypeOf(def), "call")) @compileError(kind ++ " '" ++ def.name ++ "' is missing the `.call` field");
        for (defs, 0..) |other, j| {
            if (j < i and std.mem.eql(u8, other.name, def.name)) @compileError("duplicate " ++ kind ++ " name '" ++ def.name ++ "'");
        }
        _ = ArgsType(Handler, def);
    }
}

fn optionalField(comptime value: anytype, comptime name: []const u8) ?[]const u8 {
    return if (@hasField(@TypeOf(value), name)) @field(value, name) else null;
}

/// Copies the fields of the struct literal `value` into a `T`.
fn coerceStruct(comptime T: type, comptime value: anytype) T {
    if (@TypeOf(value) == T) return value;
    var result: T = .{};
    for (@typeInfo(@TypeOf(value)).@"struct".field_names) |name| {
        @field(result, name) = @field(value, name);
    }
    return result;
}

fn promptArguments(comptime Args: type) []const types.PromptArgument {
    const info = @typeInfo(Args).@"struct";
    var list: [info.field_names.len]types.PromptArgument = undefined;
    for (&list, info.field_names, info.field_types, info.field_attrs) |*argument, name, FieldType, attrs| {
        if (FieldType != []const u8 and FieldType != ?[]const u8) {
            @compileError("prompt argument '" ++ name ++ "' of " ++ @typeName(Args) ++ " must be of type `[]const u8` or `?[]const u8`");
        }
        const description: ?[]const u8 = if (@hasDecl(Args, "descriptions") and @hasField(@TypeOf(Args.descriptions), name))
            @field(Args.descriptions, name)
        else
            null;
        argument.* = .{
            .name = name,
            .description = description,
            .required = FieldType == []const u8 and attrs.default_value_ptr == null,
        };
    }
    const final = list;
    return &final;
}

pub const JsonValueError = error{
    /// `inf` and `nan` cannot be represented in JSON.
    NonFiniteNumber,
    /// `std.json` serializes byte strings that are not valid UTF-8 as arrays of numbers.
    InvalidUtf8,
};

fn jsonValueErrorMessage(err: JsonValueError) []const u8 {
    return switch (err) {
        error.NonFiniteNumber => "contains a non-finite number (inf or nan), which cannot be represented in JSON",
        error.InvalidUtf8 => "contains a string that is not valid UTF-8",
    };
}

/// Verifies that `value` serializes to the JSON its schema promises.
fn checkJson(value: anytype) JsonValueError!void {
    const T = @TypeOf(value);
    if (T == std.json.Value) {
        switch (value) {
            .float => |float| if (!std.math.isFinite(float)) return error.NonFiniteNumber,
            .string, .number_string => |string| try checkString(string),
            .array => |array| for (array.items) |item| try checkJson(item),
            .object => |object| for (object.keys(), object.values()) |key, item| {
                try checkString(key);
                try checkJson(item);
            },
            .null, .bool, .integer => {},
        }
        return;
    }
    switch (@typeInfo(T)) {
        .float => if (!std.math.isFinite(value)) return error.NonFiniteNumber,
        .comptime_float => if (!std.math.isFinite(@as(f64, value))) return error.NonFiniteNumber,
        .optional => if (value) |payload| try checkJson(payload),
        .array => |info| if (info.child == u8) try checkString(&value) else for (value) |item| try checkJson(item),
        .pointer => |info| switch (info.size) {
            .slice => if (info.child == u8) try checkString(value) else for (value) |item| try checkJson(item),
            .one => switch (@typeInfo(info.child)) {
                .@"opaque" => {},
                else => try checkJson(value.*),
            },
            .many, .c => {},
        },
        .@"struct" => |info| {
            if (!checksContents(T)) return;
            inline for (info.field_names) |name| try checkJson(@field(value, name));
        },
        .@"union" => |info| if (info.tag_type != null and checksContents(T)) switch (value) {
            inline else => |payload| try checkJson(payload),
        },
        else => {},
    }
}

/// The serialization of a type with a custom `jsonStringify` is unknown, so its
/// contents are only checked if it declares `json_check_payload`, i.e. that it
/// serializes its fields (or active payload) unchanged.
fn checksContents(comptime T: type) bool {
    return !std.meta.hasFn(T, "jsonStringify") or @hasDecl(T, "json_check_payload");
}

fn checkString(string: []const u8) JsonValueError!void {
    if (!std.unicode.utf8ValidateSlice(string)) return error.InvalidUtf8;
}

test checkJson {
    try checkJson(.{ .a = 1.5, .b = "text", .c = @as(?f64, null) });
    try checkJson(types.ContentBlock.initText("héllo"));

    try std.testing.expectError(error.NonFiniteNumber, checkJson(std.math.inf(f64)));
    try std.testing.expectError(error.NonFiniteNumber, checkJson(.{ .xs = &[_]f32{ 1, std.math.nan(f32) } }));
    try std.testing.expectError(error.InvalidUtf8, checkJson(types.ContentBlock.initText("caf\xe9")));

    var object: std.json.ObjectMap = .empty;
    defer object.deinit(std.testing.allocator);
    try object.put(std.testing.allocator, "k", .{ .float = std.math.inf(f64) });
    try std.testing.expectError(error.NonFiniteNumber, checkJson(std.json.Value{ .object = object }));
}

/// What is known about a message, used to decide whether it can be answered.
const MessageKind = union(enum) {
    /// The message could not be parsed yet.
    unknown,
    request: json_rpc.ID,
    /// A notification or response.
    other,
};

/// Writes an error response without using the (possibly exhausted) heap.
fn writeFallbackError(
    io: std.Io,
    transport: *Transport,
    id: ?json_rpc.ID,
    code: json_rpc.JsonRPCMessage.Response.Error.Code,
    message: []const u8,
) Transport.WriteError!void {
    var buffer: [4096]u8 = undefined;
    var fba: std.heap.FixedBufferAllocator = .init(&buffer);
    transport.writeErrorResponse(io, fba.allocator(), id, .{ .code = code, .message = message }, stringify_options) catch |err| switch (err) {
        error.OutOfMemory => log.err("failed to report an error: the request id is too long", .{}),
        else => |e| return e,
    };
}

fn toolError(arena: std.mem.Allocator, comptime fmt: []const u8, args: anytype) std.mem.Allocator.Error!types.CallToolResult {
    const content = try arena.alloc(types.ContentBlock, 1);
    content[0] = .initText(try std.fmt.allocPrint(arena, fmt, args));
    return .{ .content = content, .isError = true };
}

fn isString(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .pointer => |info| switch (info.size) {
            .slice => info.child == u8,
            .one => switch (@typeInfo(info.child)) {
                .array => |array| array.child == u8,
                else => false,
            },
            else => false,
        },
        else => false,
    };
}

/// Whether a tool result of type `T` is returned as `structuredContent`.
fn hasStructuredOutput(comptime T: type) bool {
    if (T == types.CallToolResult or T == types.ContentBlock or T == void) return false;
    return switch (@typeInfo(T)) {
        .@"struct" => |info| !info.is_tuple,
        else => false,
    };
}

fn toCallToolResult(arena: std.mem.Allocator, result: anytype) std.mem.Allocator.Error!types.CallToolResult {
    const T = @TypeOf(result);
    if (T == types.CallToolResult) return result;
    if (T == void) return .{ .content = &.{} };
    if (T == types.ContentBlock) {
        const content = try arena.alloc(types.ContentBlock, 1);
        content[0] = result;
        return .{ .content = content };
    }
    if (T == []const types.ContentBlock or T == []types.ContentBlock) return .{ .content = result };
    if (comptime isString(T)) {
        const content = try arena.alloc(types.ContentBlock, 1);
        content[0] = .initText(result);
        return .{ .content = content };
    }

    // Serialize everything else as JSON. The serialized JSON is also returned
    // as text content for backwards compatibility.
    const json = try std.json.Stringify.valueAlloc(arena, result, stringify_options);
    const content = try arena.alloc(types.ContentBlock, 1);
    content[0] = .initText(json);
    return .{
        .content = content,
        .structuredContent = if (comptime hasStructuredOutput(T)) .{ .raw = json } else null,
    };
}

fn toGetPromptResult(arena: std.mem.Allocator, result: anytype) std.mem.Allocator.Error!types.GetPromptResult {
    const T = @TypeOf(result);
    if (T == types.GetPromptResult) return result;
    if (T == []const types.PromptMessage or T == []types.PromptMessage) return .{ .messages = result };
    if (comptime isString(T)) {
        const messages = try arena.alloc(types.PromptMessage, 1);
        messages[0] = .{ .role = .user, .content = .initText(result) };
        return .{ .messages = messages };
    }
    @compileError("unsupported prompt result type " ++ @typeName(T));
}

/// Treats missing and `null` params (or arguments) as an empty object.
fn paramsOrEmpty(params: ?std.json.Value) std.json.Value {
    const value = params orelse return .{ .object = .empty };
    return if (value == .null) .{ .object = .empty } else value;
}

/// Extracts `params._meta.progressToken`.
fn extractProgressToken(params: ?std.json.Value) ?types.ProgressToken {
    const p = params orelse return null;
    if (p != .object) return null;
    const meta = p.object.get("_meta") orelse return null;
    if (meta != .object) return null;
    const token = meta.object.get("progressToken") orelse return null;
    return parseId(token);
}

/// Tries to find the request id of an invalid message so that the error
/// response can be associated with it. Messages without a `"method"` are
/// responses whose id belongs to the server's own requests, so no id is
/// recovered for them.
fn recoverId(value: std.json.Value) ?json_rpc.ID {
    if (value != .object) return null;
    if (value.object.get("method") == null) return null;
    return parseId(value.object.get("id") orelse return null);
}

fn parseId(value: std.json.Value) ?json_rpc.ID {
    // `ID.jsonParseFromValue` neither allocates nor reads the options.
    return json_rpc.ID.jsonParseFromValue(undefined, value, .{}) catch null;
}

test recoverId {
    const parse = struct {
        fn parse(arena: std.mem.Allocator, json: []const u8) !std.json.Value {
            return std.json.parseFromSliceLeaky(std.json.Value, arena, json, .{});
        }
    }.parse;
    var arena_allocator: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    try std.testing.expectEqual(json_rpc.ID{ .number = 3 }, recoverId(try parse(arena, "{\"id\":3,\"method\":5}")).?);
    try std.testing.expectEqualStrings("a", recoverId(try parse(arena, "{\"id\":\"a\",\"method\":\"x\"}")).?.string);
    try std.testing.expectEqual(null, recoverId(try parse(arena, "{\"id\":3,\"result\":1,\"error\":{}}")));
    try std.testing.expectEqual(null, recoverId(try parse(arena, "{\"id\":true,\"method\":\"x\"}")));
}

/// Replaces each byte of an invalid UTF-8 sequence with U+FFFD so that `std.json` serializes
/// the text as a string.
fn sanitizeUtf8(arena: std.mem.Allocator, text: []const u8) std.mem.Allocator.Error![]const u8 {
    if (std.unicode.utf8ValidateSlice(text)) return text;
    var result: std.ArrayList(u8) = .empty;
    var i: usize = 0;
    while (i < text.len) {
        const len = std.unicode.utf8ByteSequenceLength(text[i]) catch 0;
        if (len != 0 and i + len <= text.len and std.unicode.utf8ValidateSlice(text[i..][0..len])) {
            try result.appendSlice(arena, text[i..][0..len]);
            i += len;
        } else {
            try result.appendSlice(arena, "\u{FFFD}");
            i += 1;
        }
    }
    return result.items;
}

test sanitizeUtf8 {
    var arena_allocator: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();
    try std.testing.expectEqualStrings("ok", try sanitizeUtf8(arena, "ok"));
    try std.testing.expectEqualStrings("caf\u{FFFD}!\u{FFFD}\u{FFFD}", try sanitizeUtf8(arena, "caf\xe9!\xf0\x9f"));
}

// ----------------------------------------------------------------------------
// Handler function introspection
// ----------------------------------------------------------------------------

fn FnInfo(comptime func: anytype) std.builtin.Type.Fn {
    return switch (@typeInfo(@TypeOf(func))) {
        .@"fn" => |info| info,
        .pointer => |info| switch (@typeInfo(info.child)) {
            .@"fn" => |fn_info| fn_info,
            else => @compileError("expected a function but got " ++ @typeName(@TypeOf(func))),
        },
        else => @compileError("expected a function but got " ++ @typeName(@TypeOf(func))),
    };
}

fn FunctionReturnType(comptime func: anytype) type {
    return FnInfo(func).return_type orelse @compileError("generic return types are not supported");
}

/// Strips the error set of an error union.
fn PayloadType(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .error_union => |info| info.payload,
        else => T,
    };
}

fn ErrorUnionOf(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .error_union => T,
        else => error{}!T,
    };
}

fn toErrorUnion(value: anytype) ErrorUnionOf(@TypeOf(value)) {
    return value;
}

const ParamKind = enum { handler_value, handler_ptr, arena, context, args };

fn paramKind(comptime Handler: type, comptime P: type) ParamKind {
    if (P == Handler) return .handler_value;
    if (P == *Handler or P == *const Handler) return .handler_ptr;
    if (P == std.mem.Allocator) return .arena;
    if (P == *Context) return .context;
    return .args;
}

/// Returns the type of the arguments/params parameter of a definition's
/// `call` function or `void` if there is none.
fn ArgsType(comptime Handler: type, comptime def: anytype) type {
    const info = FnInfo(def.call);
    var Args: type = void;
    var found = false;
    for (info.param_types) |maybe_P| {
        const P = maybe_P orelse @compileError("'" ++ def.name ++ "': generic parameters are not supported");
        if (paramKind(Handler, P) != .args) continue;
        if (found) @compileError("'" ++ def.name ++ "' has more than one arguments parameter");
        Args = P;
        found = true;
    }
    if (schema.findOptionalWithoutDefault(Args)) |field| {
        @compileError("'" ++ def.name ++ "': the optional field `" ++ field ++ "` must have a default value (e.g. `= null`), " ++
            "otherwise parsing fails when the client omits it");
    }
    if (@hasField(@TypeOf(def), "Args") and def.Args != Args) {
        @compileError(std.fmt.comptimePrint("'{s}': `.Args = {}` does not match the arguments parameter type {}", .{ def.name, def.Args, Args }));
    }
    return Args;
}

/// Calls `func` with each parameter resolved by its type. See the module documentation.
fn callInjected(
    handler: anytype,
    ctx: *Context,
    comptime func: anytype,
    args: anytype,
) FunctionReturnType(func) {
    const Handler = @TypeOf(handler.*);
    const info = FnInfo(func);
    var call_args: std.meta.ArgsTuple(@TypeOf(func)) = undefined;
    inline for (info.param_types, 0..) |P, i| {
        call_args[i] = switch (comptime paramKind(Handler, P.?)) {
            .handler_value => handler.*,
            .handler_ptr => handler,
            .arena => ctx.arena,
            .context => ctx,
            .args => args,
        };
    }
    return @call(.auto, func, call_args);
}

// ----------------------------------------------------------------------------
// Tests
// ----------------------------------------------------------------------------

const TestHandler = struct {
    calls: u32 = 0,

    const SearchArgs = struct {
        query: []const u8,
        limit: u32 = 10,

        pub const descriptions = .{ .query = "What to search for" };
    };

    const SearchResult = struct {
        matches: []const []const u8,
    };

    fn search(handler: *TestHandler, arena: std.mem.Allocator, args: SearchArgs) !SearchResult {
        handler.calls += 1;
        const matches = try arena.alloc([]const u8, 1);
        matches[0] = try std.fmt.allocPrint(arena, "{s}:{d}", .{ args.query, args.limit });
        return .{ .matches = matches };
    }

    fn echo(args: struct { text: []const u8 }) []const u8 {
        return args.text;
    }

    fn fail(ctx: *Context) !void {
        try ctx.sendProgress(0.5, 1, null);
        return error.FileNotFound;
    }

    fn greet(args: struct { name: []const u8, greeting: ?[]const u8 = null }, arena: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(arena, "{s}, {s}!", .{ args.greeting orelse "Hello", args.name });
    }

    pub fn @"resources/read"(_: *TestHandler, params: types.ReadResourceRequestParams) !types.ReadResourceResult {
        if (!std.mem.eql(u8, params.uri, "file:///hello.txt")) return error.ResourceNotFound;
        return .{ .contents = &.{.{ .text = .{ .uri = "file:///hello.txt", .text = "hello" } }} };
    }
};

const TestServer = Server(TestHandler, .{
    .name = "test-server",
    .version = "1.2.3",
    .instructions = "Use the tools.",
    .tools = .{
        .{
            .name = "search",
            .description = "Search for things",
            .Args = TestHandler.SearchArgs,
            .annotations = .{ .readOnlyHint = true },
            .call = TestHandler.search,
        },
        .{ .name = "echo", .call = TestHandler.echo },
        .{ .name = "fail", .call = TestHandler.fail },
    },
    .prompts = .{
        .{ .name = "greet", .description = "Greet someone", .call = TestHandler.greet },
    },
});

fn expectExchange(handler: *TestHandler, input: []const u8, expected_output: []const u8) !void {
    try expectServerExchange(TestServer, handler, input, expected_output);
}

fn expectServerExchange(comptime S: type, handler: anytype, input: []const u8, expected_output: []const u8) !void {
    var reader: std.Io.Reader = .fixed(input);
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();

    var stream: Transport.Stream = .init(&reader, &aw.writer);
    var server: S = .init(handler);
    try server.run(std.testing.io, std.testing.allocator, &stream.transport);

    try std.testing.expectEqualStrings(expected_output, aw.written());
}

test "initialize" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"c","version":"0"}}}
        \\{"jsonrpc":"2.0","method":"notifications/initialized"}
        \\{"jsonrpc":"2.0","id":2,"method":"initialize","params":{"protocolVersion":"1999-01-01","capabilities":{},"clientInfo":{"name":"c","version":"0"}}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18","capabilities":{"logging":{},"prompts":{},"resources":{},"tools":{}},"serverInfo":{"name":"test-server","version":"1.2.3"},"instructions":"Use the tools."}}
        \\{"jsonrpc":"2.0","id":2,"result":{"protocolVersion":"2025-11-25","capabilities":{"logging":{},"prompts":{},"resources":{},"tools":{}},"serverInfo":{"name":"test-server","version":"1.2.3"},"instructions":"Use the tools."}}
        \\
    );
}

test "tools/list" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":"a","method":"tools/list"}
    ,
        \\{"jsonrpc":"2.0","id":"a","result":{"tools":[{"name":"search","description":"Search for things","inputSchema":{"type":"object","properties":{"query":{"type":"string","description":"What to search for"},"limit":{"type":"integer","minimum":0,"maximum":4294967295}},"required":["query"]},"outputSchema":{"type":"object","properties":{"matches":{"type":"array","items":{"type":"string"}}},"required":["matches"]},"annotations":{"readOnlyHint":true}},{"name":"echo","inputSchema":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}},{"name":"fail","inputSchema":{"type":"object"}}]}}
        \\
    );
}

test "tools/call" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"search","arguments":{"query":"zig"}}}
        \\{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hi"}}}
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"echo","arguments":{}}}
        \\{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"fail","_meta":{"progressToken":"p"}}}
        \\{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"nope"}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"{\"matches\":[\"zig:10\"]}"}],"structuredContent":{"matches":["zig:10"]}}}
        \\{"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"hi"}]}}
        \\{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"Invalid arguments for tool 'echo': MissingField"}],"isError":true}}
        \\{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":"p","progress":0.5,"total":1}}
        \\{"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"FileNotFound"}],"isError":true}}
        \\{"jsonrpc":"2.0","id":5,"error":{"code":-32602,"message":"Unknown tool: nope"}}
        \\
    );
    try std.testing.expectEqual(1, handler.calls);
}

test "prompts" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"prompts/list"}
        \\{"jsonrpc":"2.0","id":2,"method":"prompts/get","params":{"name":"greet","arguments":{"name":"Zig"}}}
        \\{"jsonrpc":"2.0","id":3,"method":"prompts/get","params":{"name":"greet","arguments":{}}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{"prompts":[{"name":"greet","description":"Greet someone","arguments":[{"name":"name","required":true},{"name":"greeting","required":false}]}]}}
        \\{"jsonrpc":"2.0","id":2,"result":{"messages":[{"role":"user","content":{"type":"text","text":"Hello, Zig!"}}]}}
        \\{"jsonrpc":"2.0","id":3,"error":{"code":-32602,"message":"Invalid arguments for prompt 'greet': MissingField"}}
        \\
    );
}

test "protocol level handler" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"file:///hello.txt"}}
        \\{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"file:///missing.txt"}}
        \\{"jsonrpc":"2.0","id":3,"method":"resources/read","params":{}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{"contents":[{"uri":"file:///hello.txt","text":"hello"}]}}
        \\{"jsonrpc":"2.0","id":2,"error":{"code":-32002,"message":"ResourceNotFound"}}
        \\{"jsonrpc":"2.0","id":3,"error":{"code":-32602,"message":"Invalid params: MissingField"}}
        \\
    );
}

test "errors and misc" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"ping"}
        \\{"jsonrpc":"2.0","id":2,"method":"unknown/method"}
        \\{"jsonrpc":"2.0","id":3,"method":"completion/complete","params":{}}
        \\not json
        \\{"jsonrpc":"2.0","id":null,"method":"ping"}
        \\{"jsonrpc":"1.0","id":4,"method":"ping"}
        \\{"jsonrpc":"2.0","method":"some/notification"}
        \\{"jsonrpc":"2.0","id":5,"method":"logging/setLevel","params":{"level":"warning"}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{}}
        \\{"jsonrpc":"2.0","id":2,"error":{"code":-32601,"message":"Method not found: unknown/method"}}
        \\{"jsonrpc":"2.0","id":3,"error":{"code":-32601,"message":"Method not found: completion/complete"}}
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error"}}
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Invalid Request"}}
        \\{"jsonrpc":"2.0","id":4,"error":{"code":-32600,"message":"Invalid Request"}}
        \\{"jsonrpc":"2.0","id":5,"result":{}}
        \\
    );
}

test "results that cannot be represented in JSON" {
    const Handler = struct {
        fn ratio() struct { ratio: f64 } {
            return .{ .ratio = std.math.inf(f64) };
        }
        fn latin1() []const u8 {
            return "caf\xe9";
        }
        fn progress(ctx: *Context) !void {
            try ctx.sendProgress(1, std.math.nan(f64), null);
        }
        pub fn @"resources/read"() types.ReadResourceResult {
            return .{ .contents = &.{.{ .text = .{ .uri = "file:///a", .text = "caf\xe9" } }} };
        }
    };
    const S = Server(Handler, .{
        .name = "s",
        .version = "0",
        .tools = .{
            .{ .name = "ratio", .call = Handler.ratio },
            .{ .name = "latin1", .call = Handler.latin1 },
            .{ .name = "progress", .call = Handler.progress },
        },
    });
    var handler: Handler = .{};
    try expectServerExchange(S, &handler,
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"ratio"}}
        \\{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"latin1"}}
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"progress","_meta":{"progressToken":1}}}
        \\{"jsonrpc":"2.0","id":4,"method":"resources/read","params":{"uri":"file:///a"}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"Invalid tool result: contains a non-finite number (inf or nan), which cannot be represented in JSON"}],"isError":true}}
        \\{"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"Invalid tool result: contains a string that is not valid UTF-8"}],"isError":true}}
        \\{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"NonFiniteNumber"}],"isError":true}}
        \\{"jsonrpc":"2.0","id":4,"error":{"code":-32603,"message":"Invalid result: contains a string that is not valid UTF-8"}}
        \\
    );
}

test "error handling" {
    const Handler = struct {
        levels_set: u32 = 0,

        fn latin1Error(ctx: *Context) !void {
            return ctx.fail(error.FileNotFound, "cannot open caf\xe9", .{});
        }
        fn staleMessage(ctx: *Context) !void {
            fallback(ctx) catch {};
            return error.AccessDenied;
        }
        fn fallback(ctx: *Context) !void {
            return ctx.fail(error.FileNotFound, "x missing", .{});
        }
        fn huge() error{OutOfMemory}![]const u8 {
            return error.OutOfMemory;
        }
        fn debugLog(ctx: *Context) !void {
            try ctx.log(.debug, null, "hidden");
            try ctx.log(.@"error", null, "shown");
        }
        pub fn @"logging/setLevel"(handler: *@This()) types.EmptyResult {
            handler.levels_set += 1;
            return .{};
        }
    };
    const S = Server(Handler, .{
        .name = "s",
        .version = "0",
        .tools = .{
            .{ .name = "latin1Error", .call = Handler.latin1Error },
            .{ .name = "staleMessage", .call = Handler.staleMessage },
            .{ .name = "huge", .call = Handler.huge },
            .{ .name = "debugLog", .call = Handler.debugLog },
        },
    });

    var handler: Handler = .{};
    try expectServerExchange(S, &handler,
        \\{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"latin1Error"}}
        \\{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"staleMessage"}}
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"huge"}}
        \\{"jsonrpc":"2.0","id":4,"method":"logging/setLevel","params":{"level":"warning"}}
        \\{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"debugLog"}}
        \\{"jsonrpc":"2.0","id":6,"result":{},"error":{"code":1,"message":"x"}}
        \\
    , "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"cannot open caf\u{FFFD}\"}],\"isError\":true}}\n" ++
        \\{"jsonrpc":"2.0","id":2,"result":{"content":[{"type":"text","text":"AccessDenied"}],"isError":true}}
        \\{"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"OutOfMemory"}}
        \\{"jsonrpc":"2.0","id":4,"result":{}}
        \\{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"error","data":"shown"}}
        \\{"jsonrpc":"2.0","id":5,"result":{"content":[]}}
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Invalid Request"}}
        \\
    );
    try std.testing.expectEqual(1, handler.levels_set);
}

test "message too long" {
    var handler: TestHandler = .{};
    var reader: std.Io.Reader = .fixed(
        \\{"jsonrpc":"2.0","id":1,"method":"ping","params":{"padding":"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}}
        \\{"jsonrpc":"2.0","id":2,"method":"ping"}
        \\
    );
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();

    var stream: Transport.Stream = .init(&reader, &aw.writer);
    stream.max_message_len = 64;
    var server: TestServer = .init(&handler);
    try server.run(std.testing.io, std.testing.allocator, &stream.transport);

    try std.testing.expectEqualStrings(
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Message too long"}}
        \\{"jsonrpc":"2.0","id":2,"result":{}}
        \\
    , aw.written());
}

test "null params are treated as missing" {
    var handler: TestHandler = .{};
    try expectExchange(&handler,
        \\{"jsonrpc":"2.0","id":1,"method":"ping","params":null}
        \\{"jsonrpc":"2.0","id":2,"method":"prompts/list","params":null}
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"fail","arguments":null}}
        \\
    ,
        \\{"jsonrpc":"2.0","id":1,"result":{}}
        \\{"jsonrpc":"2.0","id":2,"result":{"prompts":[{"name":"greet","description":"Greet someone","arguments":[{"name":"name","required":true},{"name":"greeting","required":false}]}]}}
        \\{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"FileNotFound"}],"isError":true}}
        \\
    );
}

test "out of memory never answers notifications or responses" {
    const messages = [_][]const u8{
        \\{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":1,"progress":0.5}}
        ,
        \\{"jsonrpc":"2.0","id":7,"result":{"content":[]}}
        ,
    };
    // The only reply allowed is the one for a message that could not be
    // parsed at all, where the kind of message is unknown.
    const unknown_reply =
        \\{"jsonrpc":"2.0","id":null,"error":{"code":-32603,"message":"OutOfMemory"}}
        \\
    ;
    for (messages) |message| {
        var induced_failures: usize = 0;
        var fail_index: usize = 0;
        while (fail_index < 64) : (fail_index += 1) {
            var failing: std.testing.FailingAllocator = .init(std.testing.allocator, .{ .fail_index = fail_index });
            var reader: std.Io.Reader = .fixed("");
            var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
            defer aw.deinit();
            var stream: Transport.Stream = .init(&reader, &aw.writer);
            var handler: TestHandler = .{};
            var server: TestServer = .init(&handler);
            try server.handleMessage(std.testing.io, failing.allocator(), &stream.transport, message);
            if (aw.written().len != 0) try std.testing.expectEqualStrings(unknown_reply, aw.written());
            if (!failing.has_induced_failure) {
                // Without a failure, the message must not be answered at all.
                try std.testing.expectEqualStrings("", aw.written());
                break;
            }
            induced_failures += 1;
        }
        try std.testing.expect(induced_failures > 0);
    }
}

test "checkJson skips custom serialization unless it serializes its payload" {
    const Opaque = union(enum) {
        bytes: []const u8,
        pub fn jsonStringify(_: @This(), stream: anytype) !void {
            try stream.write("base64...");
        }
    };
    const Transparent = union(enum) {
        bytes: []const u8,
        pub const json_check_payload = {};
        pub fn jsonStringify(self: @This(), stream: anytype) !void {
            try stream.write(self.bytes);
        }
    };
    try checkJson(Opaque{ .bytes = "\xff" });
    try std.testing.expectError(error.InvalidUtf8, checkJson(Transparent{ .bytes = "\xff" }));
}
