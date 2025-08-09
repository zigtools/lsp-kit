//! A tiny 'framework' to implement the core loop of a language server.
//! Handles the core loop, parsing, and message dispatch, while being
//! customizable with a user provided `Handler`.
//!
//! If you need fine-grained control over this logic, consider implementing the
//! core loop yourself instead. There are many parts of the LSP specification
//! that this abstraction does not handle.
//!
//! Also see `examples/my_first_server.zig`.

const std = @import("std");
const lsp = @import("lsp.zig");
const offsets = lsp.offsets;
const types = lsp.types;

pub fn run(
    allocator: std.mem.Allocator,
    transport: *lsp.Transport,
    /// Must be a pointer to a container type (e.g. `struct`) that implements
    /// the desired LSP methods.
    ///
    /// Each method is implemented by adding a public function with the
    /// following signature:
    ///
    /// ```
    /// pub fn methodName(
    ///     handler: *Handler,
    ///     arena: std.mem.Allocator,
    ///     params: lsp.ParamsType(method),
    /// ) lsp.ResultType(method) {}
    /// ```
    ///
    /// Here is a concrete example:
    ///
    /// ```
    /// pub fn @"textDocument/formatting"(
    ///     handler: *Handler,
    ///     arena: std.mem.Allocator,
    ///     params: types.DocumentFormattingParams,
    /// ) ?[]const types.TextEdit {}
    /// ```
    ///
    /// The function may optionally return an error.
    ///
    /// The `handler` parameter is optional and may be one of the following:
    ///
    /// - `Handler`
    /// - `*Handler`
    /// - `*const Handler`
    ///
    handler_ptr: anytype,
    /// - `std.log.err`
    /// - `std.log.scoped(.my_scope).err`
    comptime logErr: ?fn (comptime fmt: []const u8, args: anytype) void,
) !void {
    const Handler = @TypeOf(handler_ptr.*); // The handler must be passed as a pointer
    const Message = MessageType(Handler);

    comptime std.debug.assert(@hasDecl(Handler, "initialize") or @hasField(Handler, "initialize")); // The 'initialize' function cannot be omitted.

    while (true) {
        const json_message = try transport.readJsonMessage(allocator);
        defer allocator.free(json_message);

        var arena_allocator: std.heap.ArenaAllocator = .init(allocator);
        defer arena_allocator.deinit();

        const arena = arena_allocator.allocator();

        const message = Message.parseFromSliceLeaky(
            arena,
            json_message,
            .{ .ignore_unknown_fields = true, .max_value_len = null },
        ) catch |err| {
            if (logErr) |log| log("Failed to handle message: {}", .{err});
            try transport.writeErrorResponse(
                allocator,
                null,
                .{ .code = .parse_error, .message = @errorName(err) },
                .{ .emit_null_optional_fields = false },
            );
            continue;
        };

        switch (message) {
            .request => |request| switch (request.params) {
                inline else => |params, method_tag| {
                    const method: []const u8 = @tagName(method_tag);
                    if (callHandler(handler_ptr, method, .{ arena, params })) |result| {
                        if (@TypeOf(result) != lsp.ResultType(method)) {
                            @compileError(std.fmt.comptimePrint(
                                \\The '{}.{f}' function has an unexpected result type:
                                \\
                                \\Expected: {}
                                \\Actual:   {}
                            , .{ Handler, std.zig.fmtId(method), lsp.ResultType(method), @TypeOf(result) }));
                        }
                        try transport.writeResponse(
                            allocator,
                            request.id,
                            lsp.ResultType(method),
                            result,
                            .{ .emit_null_optional_fields = false },
                        );
                    } else |err| {
                        if (logErr) |log| log("Failed to handle '{s}' request: {}", .{ method, err });
                        var code: lsp.JsonRPCMessage.Response.Error.Code = .internal_error;
                        for (
                            [_]Error{
                                error.ParseError,
                                error.InvalidRequest,
                                error.MethodNotFound,
                                error.InvalidParams,
                                error.InternalError,
                                error.ServerNotInitialized,
                                error.RequestFailed,
                                error.ServerCancelled,
                                error.ContentModified,
                                error.RequestCancelled,
                            },
                            [_]lsp.JsonRPCMessage.Response.Error.Code{
                                .parse_error,
                                .invalid_request,
                                .method_not_found,
                                .invalid_params,
                                .internal_error,
                                @enumFromInt(@intFromEnum(types.ErrorCodes.ServerNotInitialized)),
                                @enumFromInt(@intFromEnum(types.LSPErrorCodes.RequestFailed)),
                                @enumFromInt(@intFromEnum(types.LSPErrorCodes.ServerCancelled)),
                                @enumFromInt(@intFromEnum(types.LSPErrorCodes.ContentModified)),
                                @enumFromInt(@intFromEnum(types.LSPErrorCodes.RequestCancelled)),
                            },
                        ) |zig_err, new_code| {
                            if (err == zig_err) {
                                code = new_code;
                                break;
                            }
                        }

                        try transport.writeErrorResponse(
                            allocator,
                            request.id,
                            .{ .code = code, .message = @errorName(err) },
                            .{ .emit_null_optional_fields = false },
                        );
                    }
                },
                .other => {
                    try transport.writeResponse(
                        allocator,
                        request.id,
                        ?void,
                        null,
                        .{},
                    );
                },
            },
            .notification => |notification| switch (notification.params) {
                inline else => |params, method_tag| {
                    const method: []const u8 = @tagName(method_tag);
                    if (callHandler(handler_ptr, method, .{ arena, params })) |result| {
                        if (@TypeOf(result) != void) {
                            @compileError(std.fmt.comptimePrint(
                                \\The '{}.{f}' function has an unexpected result type:
                                \\
                                \\Expected: void
                                \\Actual:   {}
                            , .{ Handler, std.zig.fmtId(method), @TypeOf(result) }));
                        }
                    } else |err| {
                        if (logErr) |log| log("Failed to handle '{s}' notification: {}", .{ method, err });
                    }
                    if (std.mem.eql(u8, method, "exit")) break;
                },
                .other => {},
            },
            .response => |response| try callHandler(handler_ptr, "onResponse", .{ arena, response }),
        }
    }
}

pub const Error = error{
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    /// Error code indicating that a server received a notification or
    /// request before the server has received the `initialize` request.
    ServerNotInitialized,
    /// A request failed but it was syntactically correct, e.g the
    /// method name was known and the parameters were valid. The error
    /// message should contain human readable information about why
    /// the request failed.
    ///
    /// @since 3.17.0
    RequestFailed,
    /// The server cancelled the request. This error code should
    /// only be used for requests that explicitly support being
    /// server cancellable.
    ///
    /// @since 3.17.0
    ServerCancelled,
    /// The server detected that the content of a document got
    /// modified outside normal conditions. A server should
    /// NOT send this error code if it detects a content change
    /// in it unprocessed messages. The result even computed
    /// on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore
    /// the client should cancel the request.
    ContentModified,
    /// The client has canceled a request and a server as detected
    /// the cancel.
    RequestCancelled,
};

/// Compare the implemented methods in `Handler` and compare them to the server capabilities to identify mismatches.
pub fn validateServerCapabilities(comptime Handler: type, capabilities: types.ServerCapabilities) void {
    const Status = enum { disabled, allowed, expected };

    var text_document_did_open: Status = .disabled;
    var text_document_did_close: Status = .disabled;
    var text_document_did_change: Status = .disabled;
    var text_document_will_save: Status = .disabled;
    var text_document_will_save_wait_until: Status = .disabled;
    var text_document_did_save: Status = .disabled;

    if (capabilities.textDocumentSync) |text_document_sync| {
        switch (text_document_sync) {
            .TextDocumentSyncOptions => |options| {
                if (options.openClose orelse false) text_document_did_open = .allowed;
                if (options.openClose orelse false) text_document_did_close = .allowed;
                switch (options.change orelse .None) {
                    .None => {},
                    else => text_document_did_change = .expected,
                }
                if (options.willSave orelse false) text_document_will_save = .expected;
                if (options.willSaveWaitUntil orelse false) text_document_will_save_wait_until = .expected;
                if (options.save) |save| switch (save) {
                    .bool => |b| if (b) {
                        text_document_did_save = .expected;
                    },
                    .SaveOptions => text_document_did_save = .expected,
                };
            },
            .TextDocumentSyncKind => |sync_kind| switch (sync_kind) {
                else => {
                    text_document_did_open = .allowed;
                    text_document_did_close = .allowed;
                    text_document_did_change = .allowed;
                    text_document_will_save = .allowed;
                    text_document_will_save_wait_until = .allowed;
                    text_document_did_save = .allowed;
                },
                .Full, .Incremental => {
                    text_document_did_open = .allowed;
                    text_document_did_close = .allowed;
                    text_document_did_change = .expected; // just guessing
                    text_document_will_save = .allowed;
                    text_document_will_save_wait_until = .allowed;
                    text_document_did_save = .allowed;
                },
            },
        }
    }

    var notebook_document_did_open: Status = .disabled;
    var notebook_document_did_change: Status = .disabled;
    var notebook_document_did_save: Status = .disabled;
    var notebook_document_did_close: Status = .disabled;
    if (capabilities.notebookDocumentSync) |notebook_document_sync| {
        notebook_document_did_open = .allowed;
        notebook_document_did_change = .allowed;
        notebook_document_did_save = .allowed;
        notebook_document_did_close = .allowed;
        switch (notebook_document_sync) {
            inline .NotebookDocumentSyncOptions, .NotebookDocumentSyncRegistrationOptions => |options| {
                notebook_document_did_save = if (options.save orelse false) .expected else .disabled;
            },
        }
    }

    const completion: Status = if (capabilities.completionProvider != null) .expected else .disabled;

    const hover: Status = if (capabilities.hoverProvider) |hover| switch (hover) {
        .bool => |b| if (b) .expected else .disabled,
        .HoverOptions => .expected,
    } else .disabled;

    const signature_help: Status = if (capabilities.signatureHelpProvider != null) .expected else .disabled;

    const declaration: Status = if (capabilities.declarationProvider) |declaration| switch (declaration) {
        .bool => |b| if (b) .expected else .disabled,
        .DeclarationOptions, .DeclarationRegistrationOptions => .expected,
    } else .disabled;

    const definition: Status = if (capabilities.definitionProvider) |definition| switch (definition) {
        .bool => |b| if (b) .expected else .disabled,
        .DefinitionOptions => .expected,
    } else .disabled;

    const type_definition: Status = if (capabilities.typeDefinitionProvider) |type_definition| switch (type_definition) {
        .bool => |b| if (b) .expected else .disabled,
        .TypeDefinitionOptions, .TypeDefinitionRegistrationOptions => .expected,
    } else .disabled;

    const implementation: Status = if (capabilities.implementationProvider) |implementation| switch (implementation) {
        .bool => |b| if (b) .expected else .disabled,
        .ImplementationOptions, .ImplementationRegistrationOptions => .expected,
    } else .disabled;

    const references: Status = if (capabilities.referencesProvider) |references| switch (references) {
        .bool => |b| if (b) .expected else .disabled,
        .ReferenceOptions => .expected,
    } else .disabled;

    const document_highlight: Status = if (capabilities.documentHighlightProvider) |document_highlight| switch (document_highlight) {
        .bool => |b| if (b) .expected else .disabled,
        .DocumentHighlightOptions => .expected,
    } else .disabled;

    const document_symbol: Status = if (capabilities.documentSymbolProvider) |document_symbol| switch (document_symbol) {
        .bool => |b| if (b) .expected else .disabled,
        .DocumentSymbolOptions => .expected,
    } else .disabled;

    const code_action: Status = if (capabilities.codeActionProvider) |code_action| switch (code_action) {
        .bool => |b| if (b) .expected else .disabled,
        .CodeActionOptions => .expected,
    } else .disabled;

    const code_lens: Status, const codeLensResolve: Status = if (capabilities.codeLensProvider) |code_lens| .{
        .expected,
        if (code_lens.resolveProvider orelse false) .expected else .disabled,
    } else .{ .disabled, .disabled };

    const document_link: Status, const document_link_resolve: Status = if (capabilities.documentLinkProvider) |document_link| .{
        .expected,
        if (document_link.resolveProvider orelse false) .expected else .disabled,
    } else .{ .disabled, .disabled };

    const document_color: Status = if (capabilities.colorProvider) |color| switch (color) {
        .bool => |b| if (b) .expected else .disabled,
        .DocumentColorOptions, .DocumentColorRegistrationOptions => .expected,
    } else .disabled;

    const workspace_symbol: Status, const workspace_symbol_resolve: Status = if (capabilities.workspaceSymbolProvider) |workspace_symbol| switch (workspace_symbol) {
        .bool => |b| .{
            if (b) .expected else .disabled,
            .disabled,
        },
        .WorkspaceSymbolOptions => |options| .{
            .expected,
            if (options.resolveProvider orelse false) .expected else .disabled,
        },
    } else .{ .disabled, .disabled };

    const formatting: Status = if (capabilities.documentFormattingProvider) |document_formatting| switch (document_formatting) {
        .bool => |b| if (b) .expected else .disabled,
        .DocumentFormattingOptions => .expected,
    } else .disabled;

    const range_formatting: Status = if (capabilities.documentRangeFormattingProvider) |document_range_formatting| switch (document_range_formatting) {
        .bool => |b| if (b) .expected else .disabled,
        .DocumentRangeFormattingOptions => .expected,
    } else .disabled;

    const on_type_formatting: Status = if (capabilities.documentOnTypeFormattingProvider != null) .expected else .disabled;

    const rename: Status = if (capabilities.renameProvider) |rename| switch (rename) {
        .bool => |b| if (b) .expected else .disabled,
        .RenameOptions => .expected,
    } else .disabled;

    const folding_range: Status = if (capabilities.foldingRangeProvider) |folding_range| switch (folding_range) {
        .bool => |b| if (b) .expected else .disabled,
        .FoldingRangeOptions, .FoldingRangeRegistrationOptions => .expected,
    } else .disabled;

    const selection_range: Status = if (capabilities.selectionRangeProvider) |selection_range| switch (selection_range) {
        .bool => |b| if (b) .expected else .disabled,
        .SelectionRangeOptions, .SelectionRangeRegistrationOptions => .expected,
    } else .disabled;

    const workspace_execute_command: Status = if (capabilities.executeCommandProvider != null) .expected else .disabled;

    var prepare_call_hierarchy: Status = .disabled;
    var call_hierarchy_incomingCalls: Status = .disabled;
    var call_hierarchy_outgoing_calls: Status = .disabled;
    if (capabilities.callHierarchyProvider) |call_hierarchy| switch (call_hierarchy) {
        .bool => |b| if (b) {
            prepare_call_hierarchy = .expected;
            call_hierarchy_incomingCalls = .expected;
            call_hierarchy_outgoing_calls = .expected;
        },
        .CallHierarchyOptions, .CallHierarchyRegistrationOptions => {
            prepare_call_hierarchy = .expected;
            call_hierarchy_incomingCalls = .expected;
            call_hierarchy_outgoing_calls = .expected;
        },
    };

    const linked_editing_range: Status = if (capabilities.linkedEditingRangeProvider) |linked_editing_range| switch (linked_editing_range) {
        .bool => |b| if (b) .expected else .disabled,
        .LinkedEditingRangeOptions, .LinkedEditingRangeRegistrationOptions => .expected,
    } else .disabled;

    var semantic_tokens_range: Status = .disabled;
    var semantic_tokens_full: Status = .disabled;
    var semantic_tokens_full_delta: Status = .disabled;
    if (capabilities.semanticTokensProvider) |semantic_tokens| switch (semantic_tokens) {
        inline else => |options| {
            semantic_tokens_range = if (options.range) |range| switch (range) {
                .bool => |b| if (b) .expected else .disabled,
                .literal_1 => .expected,
            } else .disabled;
            semantic_tokens_full, semantic_tokens_full_delta = if (options.full) |full| switch (full) {
                .bool => |b| .{
                    if (b) .expected else .disabled,
                    .disabled,
                },
                .literal_1 => |full_options| .{
                    .expected,
                    if (full_options.delta orelse false) .expected else .disabled,
                },
            } else .{ .disabled, .disabled };
        },
    };

    const moniker: Status = if (capabilities.monikerProvider) |moniker| switch (moniker) {
        .bool => |b| if (b) .expected else .disabled,
        .MonikerOptions, .MonikerRegistrationOptions => .expected,
    } else .disabled;

    const type_hierarchy: Status = if (capabilities.typeHierarchyProvider) |type_hierarchy| switch (type_hierarchy) {
        .bool => |b| if (b) .expected else .disabled,
        .TypeHierarchyOptions, .TypeHierarchyRegistrationOptions => .expected,
    } else .disabled;

    const inline_value: Status = if (capabilities.inlineValueProvider) |inline_value| switch (inline_value) {
        .bool => |b| if (b) .expected else .disabled,
        .InlineValueOptions, .InlineValueRegistrationOptions => .expected,
    } else .disabled;

    const inlay_hint: Status, const inlay_hint_resolve: Status = if (capabilities.inlayHintProvider) |inlay_hint| switch (inlay_hint) {
        .bool => |b| if (b) .{ .expected, .expected } else .{ .disabled, .disabled },
        inline .InlayHintOptions, .InlayHintRegistrationOptions => |options| .{
            .expected,
            if (options.resolveProvider orelse false) .expected else .disabled,
        },
    } else .{ .disabled, .disabled };

    const diagnostic: Status = if (capabilities.diagnosticProvider != null) .expected else .disabled;

    const inline_completion: Status = if (capabilities.inlineCompletionProvider) |inline_completion| switch (inline_completion) {
        .bool => |b| if (b) .expected else .disabled,
        .InlineCompletionOptions => .expected,
    } else .disabled;

    inline for ([_][]const u8{
        "textDocument/didOpen",
        "textDocument/didClose",
        "textDocument/didChange",
        "textDocument/willSave",
        "textDocument/willSaveWaitUntil",
        "textDocument/didSave",
        "notebookDocument/didOpen",
        "notebookDocument/didChange",
        "notebookDocument/didSave",
        "notebookDocument/didClose",
        "textDocument/completion",
        "textDocument/hover",
        "textDocument/signatureHelp",
        "textDocument/declaration",
        "textDocument/definition",
        "textDocument/typeDefinition",
        "textDocument/implementation",
        "textDocument/references",
        "textDocument/documentHighlight",
        "textDocument/documentSymbol",
        "textDocument/codeAction",
        "textDocument/codeLens",
        "codeLens/resolve",
        "textDocument/documentLink",
        "documentLink/resolve",
        "textDocument/documentColor",
        "workspace/symbol",
        "workspaceSymbol/resolve",
        "textDocument/formatting",
        "textDocument/rangeFormatting",
        "textDocument/onTypeFormatting",
        "textDocument/rename",
        "textDocument/foldingRange",
        "textDocument/selectionRange",
        "workspace/executeCommand",
        "textDocument/prepareCallHierarchy",
        "callHierarchy/incomingCalls",
        "callHierarchy/outgoingCalls",
        "textDocument/linkedEditingRange",
        "textDocument/semanticTokens/range",
        "textDocument/semanticTokens/full",
        "textDocument/semanticTokens/full/delta",
        "textDocument/moniker",
        "textDocument/prepareTypeHierarchy",
        "typeHierarchy/supertypes",
        "typeHierarchy/subtypes",
        "textDocument/inlineValue",
        "textDocument/inlayHint",
        "inlayHint/resolve",
        "textDocument/diagnostic",
        "textDocument/inlineCompletion",
    }, [_]Status{
        text_document_did_open,
        text_document_did_close,
        text_document_did_change,
        text_document_will_save,
        text_document_will_save_wait_until,
        text_document_did_save,
        notebook_document_did_open,
        notebook_document_did_change,
        notebook_document_did_save,
        notebook_document_did_close,
        completion,
        hover,
        signature_help,
        declaration,
        definition,
        type_definition,
        implementation,
        references,
        document_highlight,
        document_symbol,
        code_action,
        code_lens,
        codeLensResolve,
        document_link,
        document_link_resolve,
        document_color,
        workspace_symbol,
        workspace_symbol_resolve,
        formatting,
        range_formatting,
        on_type_formatting,
        rename,
        folding_range,
        selection_range,
        workspace_execute_command,
        prepare_call_hierarchy,
        call_hierarchy_incomingCalls,
        call_hierarchy_outgoing_calls,
        linked_editing_range,
        semantic_tokens_range,
        semantic_tokens_full,
        semantic_tokens_full_delta,
        moniker,
        type_hierarchy,
        type_hierarchy,
        type_hierarchy,
        inline_value,
        inlay_hint,
        inlay_hint_resolve,
        diagnostic,
        inline_completion,
    }) |method_name, status| {
        switch (status) {
            .disabled => {
                if (@hasDecl(Handler, method_name) or @hasField(Handler, method_name)) {
                    std.debug.panic(
                        \\The LSP method {}.{f} has been implemented but the server capabilities still indicate that the method hasn't been implemented.
                        \\
                        \\Ensure that the server capabilities which are returned during `initialize` match the implemented method.
                    , .{
                        Handler,
                        std.zig.fmtId(method_name),
                    });
                }
            },
            .allowed => {},
            .expected => {
                if (!@hasDecl(Handler, method_name) and !@hasField(Handler, method_name)) {
                    std.debug.panic(
                        \\The server capabilities indicate that {f} is implemented in `{}` but the expected function could not be found.
                        \\
                        \\Make sure that a public function with the following signature has been added:
                        \\pub fn {f}(
                        \\    handler: *Handler,
                        \\    arena: std.mem.Allocator,
                        \\    params: {},
                        \\) {} {{}}
                        \\ 
                    , .{
                        std.zig.fmtId(method_name),
                        Handler,
                        std.zig.fmtId(method_name),
                        lsp.ParamsType(method_name),
                        lsp.ResultType(method_name),
                    });
                }
            },
        }
    }
}

fn MessageType(comptime Handler: type) type {
    var RequestParams: type = undefined;
    var NotificationParams: type = undefined;

    for (
        &.{ lsp.isRequestMethod, lsp.isNotificationMethod },
        &.{ &RequestParams, &NotificationParams },
    ) |isMethod, Params| {
        var methods: []const [:0]const u8 = &.{};

        for (std.meta.declarations(Handler)) |decl| {
            if (isMethod(decl.name)) {
                methods = methods ++ [1][:0]const u8{decl.name};
            }
        }

        var enum_fields: [methods.len + 1]std.builtin.Type.EnumField = undefined;
        for (enum_fields[0 .. enum_fields.len - 1], methods, 0..) |*field, method, i| field.* = .{ .name = method, .value = i };
        enum_fields[methods.len] = .{ .name = "other", .value = methods.len };

        const MethodEnum = @Type(.{ .@"enum" = .{
            .tag_type = std.math.IntFittingRange(0, methods.len),
            .fields = &enum_fields,
            .decls = &.{},
            .is_exhaustive = true,
        } });

        var union_fields: [methods.len + 1]std.builtin.Type.UnionField = undefined;
        for (union_fields[0 .. union_fields.len - 1], methods) |*field, method| {
            const field_type = lsp.ParamsType(method);
            field.* = .{ .name = method, .type = field_type, .alignment = @alignOf(field_type) };
        }
        union_fields[methods.len] = .{ .name = "other", .type = lsp.MethodWithParams, .alignment = @alignOf(lsp.MethodWithParams) };

        Params.* = @Type(.{ .@"union" = .{
            .layout = .auto,
            .tag_type = MethodEnum,
            .fields = &union_fields,
            .decls = &.{},
        } });
    }

    return lsp.Message(RequestParams, NotificationParams, .{});
}

fn CallHandlerReturnType(comptime Handler: type, comptime fn_name: []const u8) type {
    if (!@hasDecl(Handler, fn_name) and !@hasField(Handler, fn_name)) {
        @compileError(std.fmt.comptimePrint("Could not find '{}.{f}'", .{ Handler, std.zig.fmtId(fn_name) }));
    }
    const func = @field(Handler, fn_name);
    const FuncUnwrapped = switch (@typeInfo(@TypeOf(func))) {
        .pointer => |info| switch (info.size) {
            .one => info.child,
            .many, .slice, .c => @TypeOf(func),
        },
        else => @TypeOf(func),
    };
    const ReturnType = switch (@typeInfo(FuncUnwrapped)) {
        .@"fn" => |info| info.return_type.?,
        else => @compileError(std.fmt.comptimePrint("Expected '{}.{f}' to be a function but was '{}'", .{ Handler, std.zig.fmtId(fn_name), FuncUnwrapped })),
    };
    return switch (@typeInfo(ReturnType)) {
        .error_union, .error_set => ReturnType,
        else => error{}!ReturnType,
    };
}

fn callHandler(
    handler_ptr: anytype,
    comptime fn_name: []const u8,
    args: anytype,
) CallHandlerReturnType(@TypeOf(handler_ptr.*), fn_name) {
    const Handler = @TypeOf(handler_ptr.*);
    const func = @field(Handler, fn_name);

    const fn_info = switch (@typeInfo(@TypeOf(func))) {
        .pointer => |info| switch (info.size) {
            .one => @typeInfo(info.child).@"fn",
            .many, .slice, .c => comptime unreachable,
        },
        .@"fn" => |info| info,
        else => comptime unreachable,
    };

    if (fn_info.params.len >= 1) switch (fn_info.params[0].type.?) {
        Handler, *Handler, *const Handler => {},
        else => return @call(.auto, func, args),
    } else return @call(.auto, func, args);

    return switch (fn_info.params[0].type.?) {
        Handler => @call(.auto, func, .{handler_ptr.*} ++ args),
        *Handler, *const Handler => @call(.auto, func, .{handler_ptr} ++ args),
        else => comptime unreachable,
    };
}
