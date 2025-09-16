const std = @import("std");
const MetaModel = @import("MetaModel.zig");

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit();

    const gpa = debug_allocator.allocator();

    var arg_it: std.process.ArgIterator = try .initWithAllocator(gpa);
    defer arg_it.deinit();

    _ = arg_it.skip(); // skip self exe

    const out_file_path = try gpa.dupe(u8, arg_it.next() orelse std.process.fatal("second argument must be the output path to the generated zig code", .{}));
    defer gpa.free(out_file_path);

    const parsed_meta_model = try std.json.parseFromSlice(MetaModel, gpa, @embedFile("meta-model"), .{});
    defer parsed_meta_model.deinit();

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    @setEvalBranchQuota(100_000);
    writeMetaModel(&aw.writer, parsed_meta_model.value) catch return error.OutOfMemory;

    const source = try aw.toOwnedSliceSentinel(0);
    defer gpa.free(source);

    var zig_tree: std.zig.Ast = try .parse(gpa, source, .zig);
    defer zig_tree.deinit(gpa);

    std.fs.cwd().makePath(std.fs.path.dirname(out_file_path) orelse ".") catch {};

    var out_file = try std.fs.cwd().createFile(out_file_path, .{});
    defer out_file.close();

    if (zig_tree.errors.len != 0) {
        std.log.warn("generated file contains syntax errors! (cannot format file)", .{});
        try out_file.writeAll(source);
    } else {
        var buf: [1024]u8 = undefined;
        var out = out_file.writer(&buf);
        const w = &out.interface;
        try zig_tree.render(gpa, w, .{});
        try w.flush();
    }
}

const FormatDocs = struct {
    text: []const u8,
    comment_kind: CommentKind,

    const CommentKind = enum {
        normal,
        doc,
        top_level,
    };
};

fn renderDocs(ctx: FormatDocs, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const prefix = switch (ctx.comment_kind) {
        .normal => "// ",
        .doc => "/// ",
        .top_level => "//! ",
    };
    var iterator = std.mem.splitScalar(u8, ctx.text, '\n');
    while (iterator.next()) |line| try writer.print("{s}{s}\n", .{ prefix, line });
}

fn fmtDocs(text: []const u8, comment_kind: FormatDocs.CommentKind) std.fmt.Alt(FormatDocs, renderDocs) {
    return .{ .data = .{ .text = text, .comment_kind = comment_kind } };
}

fn messageDirectionName(message_direction: MetaModel.MessageDirection) []const u8 {
    return switch (message_direction) {
        .clientToServer => "client_to_server",
        .serverToClient => "server_to_client",
        .both => "both",
    };
}

fn guessFieldName(meta_model: MetaModel, writer: *std.Io.Writer, typ: MetaModel.Type, i: usize) std.Io.Writer.Error!void {
    switch (typ) {
        .base => |base| switch (base.name) {
            .URI => try writer.writeAll("uri"),
            .DocumentUri => try writer.writeAll("document_uri"),
            .integer => try writer.writeAll("integer"),
            .uinteger => try writer.writeAll("uinteger"),
            .decimal => try writer.writeAll("decimal"),
            .RegExp => try writer.writeAll("regexp"),
            .string => try writer.writeAll("string"),
            .boolean => try writer.writeAll("bool"),
            .null => try writer.writeAll("@\"null\""),
        },
        .reference => |ref| try writer.print("{f}", .{std.zig.fmtId(ref.name)}),
        .array => |arr| {
            try writer.writeAll("array_of_");
            try guessFieldName(meta_model, writer, arr.element.*, 0);
        },
        .map => try writer.print("map_{d}", .{i}),
        .@"and" => try writer.print("and_{d}", .{i}),
        .@"or" => try writer.print("or_{d}", .{i}),
        .tuple => try writer.print("tuple_{d}", .{i}),
        .literal,
        .stringLiteral,
        .integerLiteral,
        .booleanLiteral,
        => try writer.print("literal_{d}", .{i}),
    }
}

fn isNull(ty: MetaModel.Type) bool {
    return ty == .base and ty.base.name == .null;
}

fn unwrapOptional(ty: MetaModel.Type) ?MetaModel.Type {
    if (ty != .@"or") return null;
    const or_ty = ty.@"or";
    if (or_ty.items.len == 2 and isNull(or_ty.items[1])) return or_ty.items[0];
    return null;
}

fn isEnum(ty: MetaModel.Type) bool {
    if (ty != .@"or") return false;
    const or_ty = ty.@"or";
    for (or_ty.items) |t| {
        if (t != .stringLiteral) return false;
    }
    return true;
}

fn isNullable(ty: MetaModel.Type) bool {
    if (ty != .@"or") return false;
    const or_ty = ty.@"or";
    return (or_ty.items.len == 2 and isNull(or_ty.items[1])) or isNull(or_ty.items[or_ty.items.len - 1]);
}

const FormatType = struct {
    meta_model: *const MetaModel,
    ty: MetaModel.Type,
};

fn renderType(ctx: FormatType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (ctx.ty) {
        .base => |base| switch (base.name) {
            .URI => try writer.writeAll("URI"),
            .DocumentUri => try writer.writeAll("DocumentUri"),
            .integer => try writer.writeAll("i32"),
            .uinteger => try writer.writeAll("u32"),
            .decimal => try writer.writeAll("f32"),
            .RegExp => try writer.writeAll("RegExp"),
            .string => try writer.writeAll("[]const u8"),
            .boolean => try writer.writeAll("bool"),
            .null => try writer.writeAll("?void"),
        },
        .reference => |ref| try writer.print("{f}", .{std.zig.fmtId(ref.name)}),
        .array => |arr| try writer.print("[]const {f}", .{fmtType(arr.element.*, ctx.meta_model)}),
        .map => |map| {
            try writer.writeAll("parser.Map(");
            switch (map.key) {
                .base => |base| try switch (base.name) {
                    .Uri => writer.writeAll("Uri"),
                    .DocumentUri => writer.writeAll("DocumentUri"),
                    .integer => writer.writeAll("i32"),
                    .string => writer.writeAll("[]const u8"),
                },
                .reference => |ref| try writer.print("{f}", .{fmtType(.{ .reference = ref }, ctx.meta_model)}),
            }
            try writer.print(", {f})", .{fmtType(map.value.*, ctx.meta_model)});
        },
        .@"and" => |andt| {
            try writer.writeAll("struct {\n");
            for (andt.items) |item| {
                if (item != .reference) @panic("Unimplemented and subject encountered!");
                try writer.print("// And {s}\n{f}\n\n", .{
                    item.reference.name,
                    fmtReference(item.reference, null, ctx.meta_model),
                });
            }
            try writer.writeAll("}");
        },
        .@"or" => |ort| {
            if (unwrapOptional(ctx.ty)) |child_type| {
                try writer.print("?{f}", .{fmtType(child_type, ctx.meta_model)});
            } else if (isEnum(ctx.ty)) {
                try writer.writeAll("enum {");
                for (ort.items) |sub_type| {
                    try writer.print("{s},\n", .{sub_type.stringLiteral.value});
                }
                try writer.writeByte('}');
            } else {
                const has_null = isNull(ort.items[ort.items.len - 1]);

                if (has_null) try writer.writeByte('?');

                try writer.writeAll("union(enum) {\n");
                for (ort.items[0 .. ort.items.len - @intFromBool(has_null)], 0..) |sub_type, i| {
                    try guessFieldName(ctx.meta_model.*, writer, sub_type, i);
                    try writer.print(": {f},\n", .{fmtType(sub_type, ctx.meta_model)});
                }
                try writer.writeAll(
                    \\pub const jsonParse = parser.UnionParser(@This()).jsonParse;
                    \\pub const jsonParseFromValue = parser.UnionParser(@This()).jsonParseFromValue;
                    \\pub const jsonStringify = parser.UnionParser(@This()).jsonStringify;
                    \\}
                );
            }
        },
        .tuple => |tup| {
            try writer.writeAll("struct {");
            for (tup.items, 0..) |ty, i| {
                if (i != 0) try writer.writeByte(',');
                try writer.print(" {f}", .{fmtType(ty, ctx.meta_model)});
            }
            try writer.writeAll(" }");
        },
        .literal => |lit| {
            try writer.writeAll("struct {");
            if (lit.value.properties.len != 0) {
                for (lit.value.properties) |property| {
                    try writer.print("\n{f}", .{fmtProperty(property, ctx.meta_model)});
                }
                try writer.writeByte('\n');
            }
            try writer.writeByte('}');
        },
        .stringLiteral => |lit| try writer.print("[]const u8 = \"{f}\"", .{std.zig.fmtString(lit.value)}),
        .integerLiteral => @panic("unsupported"),
        .booleanLiteral => @panic("unsupported"),
    }
}

fn fmtType(ty: MetaModel.Type, meta_model: *const MetaModel) std.fmt.Alt(FormatType, renderType) {
    return .{ .data = .{ .meta_model = meta_model, .ty = ty } };
}

const FormatProperty = struct {
    meta_model: *const MetaModel,
    property: MetaModel.Property,
};

fn renderProperty(ctx: FormatProperty, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const is_undefinedable = ctx.property.optional orelse false;
    const is_nullable = isNullable(ctx.property.type);
    // WORKAROUND: recursive SelectionRange
    const is_selection_range = ctx.property.type == .reference and std.mem.eql(u8, ctx.property.type.reference.name, "SelectionRange");

    if (ctx.property.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .doc)});

    try writer.print("{f}: {s}{f}{s},", .{
        std.zig.fmtIdPU(ctx.property.name),
        if (is_selection_range) "?*" else if (is_undefinedable and !is_nullable) "?" else "",
        fmtType(ctx.property.type, ctx.meta_model),
        if (is_nullable or is_undefinedable) " = null" else "",
    });
}

fn fmtProperty(property: MetaModel.Property, meta_model: *const MetaModel) std.fmt.Alt(FormatProperty, renderProperty) {
    return .{ .data = .{ .meta_model = meta_model, .property = property } };
}

const FormatProperties = struct {
    meta_model: *const MetaModel,
    structure: MetaModel.Structure,
    maybe_extender: ?MetaModel.Structure,
};

fn renderProperties(ctx: FormatProperties, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const properties: []MetaModel.Property = ctx.structure.properties;
    const extends: []MetaModel.Type = ctx.structure.extends orelse &.{};
    const mixins: []MetaModel.Type = ctx.structure.mixins orelse &.{};

    var has_properties = false;

    skip: for (properties) |property| {
        if (ctx.maybe_extender) |ext| {
            for (ext.properties) |ext_property| {
                if (std.mem.eql(u8, property.name, ext_property.name)) {
                    // std.log.info("Skipping implemented field emission: {s}", .{property.name});
                    continue :skip;
                }
            }
        }
        try writer.print("{f}\n", .{fmtProperty(property, ctx.meta_model)});
        has_properties = true;
    }

    if (has_properties and (extends.len != 0 or mixins.len != 0)) try writer.writeByte('\n');

    for (extends) |ext| {
        if (ext != .reference) @panic("Expected reference for extends!");
        try writer.print("// Extends `{s}`\n{f}\n", .{
            ext.reference.name,
            fmtReference(ext.reference, ctx.structure, ctx.meta_model),
        });
    }

    for (mixins) |ext| {
        if (ext != .reference) @panic("Expected reference for mixin!");
        try writer.print("// Uses mixin `{s}`\n{f}\n", .{
            ext.reference.name,
            fmtReference(ext.reference, ctx.structure, ctx.meta_model),
        });
    }
}

fn fmtProperties(
    structure: MetaModel.Structure,
    maybe_extender: ?MetaModel.Structure,
    meta_model: *const MetaModel,
) std.fmt.Alt(FormatProperties, renderProperties) {
    return .{ .data = .{ .meta_model = meta_model, .structure = structure, .maybe_extender = maybe_extender } };
}

const FormatReference = struct {
    meta_model: *const MetaModel,
    reference: MetaModel.ReferenceType,
    maybe_extender: ?MetaModel.Structure,
};

fn renderReference(ctx: FormatReference, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    for (ctx.meta_model.structures) |s| {
        if (std.mem.eql(u8, s.name, ctx.reference.name)) {
            try writer.print("{f}", .{fmtProperties(s, ctx.maybe_extender, ctx.meta_model)});
            return;
        }
    }
}

fn fmtReference(
    reference: MetaModel.ReferenceType,
    maybe_extender: ?MetaModel.Structure,
    meta_model: *const MetaModel,
) std.fmt.Alt(FormatReference, renderReference) {
    return .{ .data = .{ .meta_model = meta_model, .reference = reference, .maybe_extender = maybe_extender } };
}

fn writeRequest(writer: *std.Io.Writer, meta_model: MetaModel, request: MetaModel.Request) std.Io.Writer.Error!void {
    if (request.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .normal)});

    try writer.print(
        \\.{{
        \\    "{f}",
        \\    RequestMetadata{{
        \\        .method = {f},
        \\        .documentation = {?f},
        \\        .direction = .{s},
        \\        .Params = {?f},
        \\        .Result = {f},
        \\        .PartialResult = {?f},
        \\        .ErrorData = {?f},
        \\        .registration = .{{ .method = {?f}, .Options = {?f} }},
        \\    }},
        \\}},
        \\
    , .{
        std.zig.fmtString(request.method),
        std.json.fmt(request.method, .{}),
        if (request.documentation) |documentation| std.json.fmt(documentation, .{}) else null,
        messageDirectionName(request.messageDirection),
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        if (request.params) |params| fmtType(params.Type, &meta_model) else null,
        fmtType(request.result, &meta_model),
        if (request.partialResult) |ty| fmtType(ty, &meta_model) else null,
        if (request.errorData) |ty| fmtType(ty, &meta_model) else null,
        if (request.registrationMethod) |method| std.json.fmt(method, .{}) else null,
        if (request.registrationOptions) |ty| fmtType(ty, &meta_model) else null,
    });
}

fn writeNotification(writer: *std.Io.Writer, meta_model: MetaModel, notification: MetaModel.Notification) std.Io.Writer.Error!void {
    if (notification.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .normal)});

    try writer.print(
        \\.{{
        \\    "{f}",
        \\    NotificationMetadata{{
        \\        .method = {f},
        \\        .documentation = {?f},
        \\        .direction = .{s},
        \\        .Params = {?f},
        \\        .registration = .{{ .method = {?f}, .Options = {?f} }},
        \\    }},
        \\}},
        \\
    , .{
        std.zig.fmtString(notification.method),
        std.json.fmt(notification.method, .{}),
        if (notification.documentation) |documentation| std.json.fmt(documentation, .{}) else null,
        messageDirectionName(notification.messageDirection),
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        if (notification.params) |params| fmtType(params.Type, &meta_model) else null,
        if (notification.registrationMethod) |method| std.json.fmt(method, .{}) else null,
        if (notification.registrationOptions) |ty| fmtType(ty, &meta_model) else null,
    });
}

fn writeStructure(writer: *std.Io.Writer, meta_model: MetaModel, structure: MetaModel.Structure) std.Io.Writer.Error!void {
    if (std.mem.eql(u8, structure.name, "LSPObject")) return;

    if (structure.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .doc)});
    try writer.print("pub const {f} = struct {{\n{f}}};\n\n", .{
        std.zig.fmtId(structure.name),
        fmtProperties(structure, null, &meta_model),
    });
}

fn writeEnumeration(writer: *std.Io.Writer, meta_model: MetaModel, enumeration: MetaModel.Enumeration) std.Io.Writer.Error!void {
    _ = meta_model;

    if (enumeration.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .doc)});

    const container_kind = switch (enumeration.type.name) {
        .string => "union(enum)",
        .integer => "enum(i32)",
        .uinteger => "enum(u32)",
    };
    try writer.print("pub const {f} = {s} {{\n", .{ std.zig.fmtId(enumeration.name), container_kind });

    // WORKAROUND: the enumeration value `pascal` appears twice in LanguageKind
    var found_pascal = false;

    var contains_empty_enum = false;
    for (enumeration.values) |entry| {
        if (entry.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .doc)});
        switch (entry.value) {
            .string => |value| {
                if (std.mem.eql(u8, value, "pascal")) {
                    if (found_pascal) continue;
                    found_pascal = true;
                }
                if (value.len == 0) contains_empty_enum = true;
                const name = if (value.len == 0) "empty" else value;
                try writer.print("{f},\n", .{std.zig.fmtIdP(name)});
            },
            .number => |value| try writer.print("{f} = {d},\n", .{ std.zig.fmtIdP(entry.name), value }),
        }
    }

    const supportsCustomValues = enumeration.supportsCustomValues orelse false;

    const field_name, const docs = if (supportsCustomValues) .{ "custom_value", "Custom Value" } else .{ "unknown_value", "Unknown Value" };
    switch (enumeration.type.name) {
        .string => {
            try writer.print(
                \\{s}: []const u8,
                \\pub const eql = parser.EnumCustomStringValues(@This(), {1}).eql;
                \\pub const jsonParse = parser.EnumCustomStringValues(@This(), {1}).jsonParse;
                \\pub const jsonParseFromValue = parser.EnumCustomStringValues(@This(), {1}).jsonParseFromValue;
                \\pub const jsonStringify = parser.EnumCustomStringValues(@This(), {1}).jsonStringify;
                \\
            , .{ field_name, contains_empty_enum });
        },
        .integer, .uinteger => {
            try writer.print(
                \\/// {s}
                \\_,
                \\pub const jsonStringify = parser.EnumStringifyAsInt(@This()).jsonStringify;
                \\
            , .{docs});
        },
    }

    try writer.writeAll("};\n\n");
}

fn writeTypeAlias(writer: *std.Io.Writer, meta_model: MetaModel, type_alias: MetaModel.TypeAlias) std.Io.Writer.Error!void {
    if (std.mem.startsWith(u8, type_alias.name, "LSP")) return;

    if (type_alias.documentation) |docs| try writer.print("{f}", .{fmtDocs(docs, .doc)});
    try writer.print("pub const {f} = {f};\n\n", .{ std.zig.fmtId(type_alias.name), fmtType(type_alias.type, &meta_model) });
}

fn writeMetaModel(writer: *std.Io.Writer, meta_model: MetaModel) std.Io.Writer.Error!void {
    try writer.writeAll(@embedFile("lsp_types_base.zig") ++ "\n");

    try writer.writeAll("// Type Aliases\n\n");
    for (meta_model.typeAliases) |type_alias| {
        try writeTypeAlias(writer, meta_model, type_alias);
    }

    try writer.writeAll("// Enumerations\n\n");
    for (meta_model.enumerations) |enumeration| {
        try writeEnumeration(writer, meta_model, enumeration);
    }

    try writer.writeAll("// Structures\n\n");
    for (meta_model.structures) |structure| {
        try writeStructure(writer, meta_model, structure);
    }

    try writer.writeAll("const notifications_generated: std.StaticStringMap(NotificationMetadata) = .initComptime(&.{\n");
    for (meta_model.notifications) |notification| {
        try writeNotification(writer, meta_model, notification);
    }
    try writer.writeAll("\n});");

    try writer.writeAll("const requests_generated: std.StaticStringMap(RequestMetadata) = .initComptime(&.{\n");
    for (meta_model.requests) |request| {
        try writeRequest(writer, meta_model, request);
    }
    try writer.writeAll("\n});");
}
