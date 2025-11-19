//! This program is executed from the build system to generate type bindings for the LSP specification.

const std = @import("std");
const MetaModel = @import("MetaModel.zig");

pub fn main() !u8 {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit();

    const gpa = debug_allocator.allocator();

    var arg_it: std.process.ArgIterator = try .initWithAllocator(gpa);
    defer arg_it.deinit();

    _ = arg_it.skip(); // skip self exe

    const out_file_path = try gpa.dupe(u8, arg_it.next() orelse std.process.fatal("second argument must be the output path to the generated zig code", .{}));
    defer gpa.free(out_file_path);

    var parsed_meta_model = try std.json.parseFromSlice(MetaModel, gpa, @embedFile("meta-model"), .{});
    defer parsed_meta_model.deinit();

    const source = try renderMetaModel(gpa, &parsed_meta_model.value);
    defer gpa.free(source);

    var zig_tree: std.zig.Ast = try .parse(gpa, source, .zig);
    defer zig_tree.deinit(gpa);

    std.fs.cwd().makePath(std.fs.path.dirname(out_file_path) orelse ".") catch {};

    var out_file = try std.fs.cwd().createFile(out_file_path, .{});
    defer out_file.close();

    if (zig_tree.errors.len != 0) {
        std.log.warn("generated file contains syntax errors! (cannot format file)", .{});
        try out_file.writeAll(source);
        return 1;
    } else {
        var buf: [1024]u8 = undefined;
        var out = out_file.writer(&buf);
        const w = &out.interface;
        try zig_tree.render(gpa, w, .{});
        try w.flush();
        return 0;
    }
}

const FormatToSnakeCase = struct {
    text: []const u8,

    pub fn format(ctx: FormatToSnakeCase, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        for (ctx.text, 0..) |c, i| {
            if (std.ascii.isUpper(c)) {
                const isNextUpper = i + 1 < ctx.text.len and std.ascii.isUpper(ctx.text[i + 1]);
                if (i != 0 and !isNextUpper) try writer.writeByte('_');
                try writer.writeByte(std.ascii.toLower(c));
            } else {
                try writer.writeByte(c);
            }
        }
    }
};

fn fmtToSnakeCase(text: []const u8) std.fmt.Alt(FormatToSnakeCase, FormatToSnakeCase.format) {
    return .{ .data = .{ .text = text } };
}

const FormatDocs = struct {
    text: []const u8,
    comment_kind: CommentKind,

    const CommentKind = enum {
        normal,
        doc,
        top_level,
    };

    pub fn format(ctx: FormatDocs, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        const prefix = switch (ctx.comment_kind) {
            .normal => "// ",
            .doc => "/// ",
            .top_level => "//! ",
        };
        var iterator = std.mem.splitScalar(u8, ctx.text, '\n');
        while (iterator.next()) |line| try writer.print("{s}{s}\n", .{ prefix, line });
    }
};

fn fmtDocs(text: []const u8, comment_kind: FormatDocs.CommentKind) std.fmt.Alt(FormatDocs, FormatDocs.format) {
    return .{ .data = .{ .text = text, .comment_kind = comment_kind } };
}

fn guessFieldName(meta_model: *const MetaModel, writer: *std.Io.Writer, typ: MetaModel.Type, i: usize) std.Io.Writer.Error!void {
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
        .reference => |ref| try writer.print("{f}", .{fmtToSnakeCase(ref.name)}),
        .array => |arr| {
            try guessFieldName(meta_model, writer, arr.element.*, 0);
            try writer.writeByte('s');
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

fn unwrapOptionalUnion(ty: MetaModel.Type) ?MetaModel.Type {
    if (ty != .@"or") return null;
    const or_ty = ty.@"or";
    if (unwrapOptional(ty) != null) return null;
    if (isEnum(ty)) return null;
    if (!isNull(or_ty.items[or_ty.items.len - 1])) return null;
    return .{ .@"or" = .{ .items = or_ty.items[0 .. or_ty.items.len - 1] } };
}

fn createOptional(arena: std.mem.Allocator, child_type: MetaModel.Type) error{OutOfMemory}!MetaModel.Type {
    return .{ .@"or" = .{
        .items = try arena.dupe(MetaModel.Type, &.{
            child_type,
            .{ .base = .{ .name = .null } },
        }),
    } };
}

/// Some LSP types will be rendered to a distinct Zig type (anonymous struct/container type).
fn findInnerDistinctType(ty: *MetaModel.Type, meta_model: *const MetaModel) ?*MetaModel.Type {
    return switch (ty.*) {
        .base => null,
        .reference => null,
        .array => |arr| findInnerDistinctType(arr.element, meta_model),
        .map => |map| findInnerDistinctType(map.value, meta_model),
        .@"and" => ty,
        .@"or" => |or_ty| {
            if (unwrapOptional(ty.*)) |_| {
                return findInnerDistinctType(&or_ty.items[0], meta_model);
            } else {
                return ty;
            }
        },
        .tuple => |tup| {
            for (tup.items) |*sub_type| {
                if (findInnerDistinctType(sub_type, meta_model)) |reason| return reason;
            }
            return null;
        },
        .literal => ty,
        .stringLiteral => null,
        .integerLiteral => @panic("unsupported"),
        .booleanLiteral => @panic("unsupported"),
    };
}

const Symbol = union(enum) {
    namespace,
    decl: struct {
        type: MetaModel.Type,
        original_name: ?[]const u8,
        documentation: ?[]const u8,
    },
    structure: MetaModel.Structure,
    enumeration: MetaModel.Enumeration,
};

const SymbolTree = struct {
    root: std.StringArrayHashMapUnmanaged(Node) = .empty,

    const Node = struct {
        symbol: Symbol,
        children: std.StringArrayHashMapUnmanaged(Node) = .empty,

        fn deinit(
            node: *Node,
            gpa: std.mem.Allocator,
        ) void {
            for (node.children.values()) |*child_node| child_node.deinit(gpa);
            node.children.deinit(gpa);
            node.* = undefined;
        }
    };

    fn deinit(
        tree: *SymbolTree,
        gpa: std.mem.Allocator,
    ) void {
        for (tree.root.values()) |*node| node.deinit(gpa);
        tree.root.deinit(gpa);
        tree.* = undefined;
    }

    fn insert(
        tree: *SymbolTree,
        gpa: std.mem.Allocator,
        name: []const u8,
        symbol: Symbol,
    ) error{OutOfMemory}!void {
        var name_it = std.mem.splitScalar(u8, name, '.');
        var current_node: ?*Node = null;
        while (name_it.next()) |name_component| {
            const children = if (current_node) |node| &node.children else &tree.root;
            const gop = try children.getOrPutValue(gpa, name_component, .{ .symbol = .namespace });
            current_node = gop.value_ptr;
        }
        const node = current_node.?;
        if (node.symbol != .namespace) std.debug.panic("symbol collision: {s}", .{name[0 .. name_it.index orelse name.len]});
        node.symbol = symbol;
    }

    /// Useful for debugging
    fn dump(tree: *const SymbolTree) error{WriteFailed}!void {
        const dumpNode = struct {
            fn dumpNode(
                children: std.StringArrayHashMapUnmanaged(Node),
                writer: *std.Io.Writer,
                indent: usize,
            ) error{WriteFailed}!void {
                for (children.keys(), children.values(), 0..) |name, child_node, i| {
                    const is_last = i + 1 == children.count();
                    try writer.splatBytesAll("│   ", indent);
                    try writer.print("{s}── {s} {s}\n", .{ @as([]const u8, if (is_last) "└" else "├"), name });
                    try dumpNode(child_node.children, writer, indent + 1);
                }
            }
        }.dumpNode;

        var buffer: [4096]u8 = undefined;
        const writer = std.debug.lockStderrWriter(&buffer);
        defer std.debug.unlockStderrWriter();

        try dumpNode(tree.root, writer, 0);
    }
};

const Renderer = struct {
    scope_stack: std.ArrayList(Scope),
    meta_model: *const MetaModel,
    w: *std.Io.Writer,

    const Scope = struct {
        name: ?[]const u8,
        symbols: std.StringArrayHashMapUnmanaged(SymbolTree.Node),
    };

    fn renderNode(r: *Renderer, node: *const SymbolTree.Node, name: []const u8) error{WriteFailed}!void {
        r.scope_stack.appendAssumeCapacity(.{ .name = name, .symbols = node.children });
        defer r.scope_stack.items.len -= 1;

        switch (node.symbol) {
            .namespace => {
                try r.w.print("pub const {f} = struct {{\n", .{std.zig.fmtId(name)});
                for (node.children.keys(), node.children.values()) |child_name, *child_node| {
                    try r.renderNode(child_node, child_name);
                }
                try r.w.writeAll("};\n\n");
            },
            .decl => |payload| {
                if (payload.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .doc)});
                if (payload.original_name) |original_name| {
                    if (!std.mem.eql(u8, name, original_name)) {
                        if (payload.documentation != null) try r.w.writeAll("///\n");
                        try r.w.print("/// LSP Specification name: `{s}`\n", .{original_name});
                    }
                }
                try r.w.print("pub const {f} = ", .{std.zig.fmtId(name)});
                try r.renderType(payload.type, node.children);
                try r.w.writeAll(";\n\n");
            },
            .structure => |*structure| {
                if (structure.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .doc)});
                if (!std.mem.eql(u8, name, structure.name)) {
                    if (structure.documentation != null) try r.w.writeAll("///\n");
                    try r.w.print("/// LSP Specification name: `{s}`\n", .{structure.name});
                }

                try r.w.print("pub const {f} = struct {{\n", .{std.zig.fmtId(name)});
                try r.renderProperties(structure, null);
                if (node.children.count() != 0) try r.w.writeAll("\n\n");
                for (node.children.keys(), node.children.values()) |child_name, *child_node| {
                    try r.renderNode(child_node, child_name);
                }
                try r.w.writeAll("};\n\n");
            },
            .enumeration => |enumeration| {
                const container_kind = switch (enumeration.type.name) {
                    .string => "union(enum)",
                    .integer => "enum(i32)",
                    .uinteger => "enum(u32)",
                };
                if (enumeration.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .doc)});
                if (!std.mem.eql(u8, name, enumeration.name)) {
                    if (enumeration.documentation != null) try r.w.writeAll("///\n");
                    try r.w.print("/// LSP Specification name: `{s}`\n", .{enumeration.name});
                }
                try r.w.print("pub const {f} = {s} {{\n", .{ std.zig.fmtId(name), container_kind });

                var contains_empty_enum = false;
                for (enumeration.values) |entry| {
                    if (entry.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .doc)});
                    switch (entry.value) {
                        .string => |value| {
                            var value_name = if (value.len == 0) "empty" else value;
                            if (value.len == 0) contains_empty_enum = true;
                            // WORKAROUND: the enumeration value `pascal` appears twice in LanguageKind
                            if (std.mem.eql(u8, entry.name, "Delphi")) value_name = "delphi";
                            try r.w.print("{f},\n", .{std.zig.fmtIdP(value_name)});
                        },
                        .number => |value| try r.w.print("{f} = {d},\n", .{ std.zig.fmtIdP(entry.name), value }),
                    }
                }

                const supportsCustomValues = enumeration.supportsCustomValues orelse false;

                const field_name, const docs = if (supportsCustomValues) .{ "custom_value", "Custom Value" } else .{ "unknown_value", "Unknown Value" };
                switch (enumeration.type.name) {
                    .string => try r.w.print("{s}: []const u8,", .{field_name}),
                    .integer, .uinteger => try r.w.print("/// {s}\n_,", .{docs}),
                }

                if (node.children.count() != 0) try r.w.writeAll("\n\n");
                for (node.children.keys(), node.children.values()) |child_name, *child_node| {
                    try r.renderNode(child_node, child_name);
                }
                try r.w.writeAll("\n\n");

                switch (enumeration.type.name) {
                    .string => {
                        try r.w.print(
                            \\pub const eql = parser.EnumCustomStringValues(@This(), {0}).eql;
                            \\pub const jsonParse = parser.EnumCustomStringValues(@This(), {0}).jsonParse;
                            \\pub const jsonParseFromValue = parser.EnumCustomStringValues(@This(), {0}).jsonParseFromValue;
                            \\pub const jsonStringify = parser.EnumCustomStringValues(@This(), {0}).jsonStringify;
                            \\
                        , .{contains_empty_enum});
                    },
                    .integer, .uinteger => {
                        try r.w.writeAll(
                            \\pub const jsonStringify = parser.EnumStringifyAsInt(@This()).jsonStringify;
                            \\
                        );
                    },
                }

                try r.w.writeAll("};\n\n");
            },
        }
    }

    fn renderReference(r: *Renderer, reference_name: []const u8) error{WriteFailed}!void {
        switch (symbol_map.get(reference_name).?) {
            .remove => try r.w.writeAll(reference_name), // keep the old name
            .rename, .replace_with => |namespaced_name| try r.renderNamespacedName(namespaced_name),
        }
    }

    fn renderNamespacedName(r: *Renderer, namespaced_name: []const u8) error{WriteFailed}!void {
        if (r.scope_stack.items.len <= 1) {
            try r.w.writeAll(namespaced_name);
            return;
        }

        var namespace_it: SymbolNamespaceIterator = .init(namespaced_name, .reverse);
        next: while (true) {
            const symbol_name = namespace_it.next() orelse break;
            var reference_count: usize = 0;
            for (r.scope_stack.items) |scope| {
                const is_referencing = scope.symbols.contains(symbol_name);
                if (is_referencing and !r.isRenderingNamespace(namespaced_name[0..namespace_it.index])) continue :next;
                reference_count += @intFromBool(is_referencing);
                if (reference_count > 1) continue :next;
            }
            switch (reference_count) {
                0 => continue,
                1 => {},
                else => unreachable,
            }
            const trimmed_name = namespaced_name[if (namespace_it.index == 0) 0 else namespace_it.index + 1..];
            try r.w.writeAll(trimmed_name);
            return;
        }
        try r.w.writeAll("types.");
        try r.w.writeAll(namespaced_name);
    }

    fn isRenderingName(r: *Renderer, name: []const u8) bool {
        var it: SymbolNamespaceIterator = .init(name, .forward);
        var i: usize = 1;
        while (it.next()) |a| : (i += 1) {
            if (i >= r.scope_stack.items.len) return false;
            const b = r.scope_stack.items[i].name.?;
            if (!std.mem.eql(u8, a, b)) return false;
        }
        if (i != r.scope_stack.items.len) return false;
        return true;
    }

    fn isRenderingNamespace(r: *Renderer, namespace_name: []const u8) bool {
        var it: SymbolNamespaceIterator = .init(namespace_name, .forward);
        var i: usize = 1;
        while (it.next()) |a| : (i += 1) {
            if (i >= r.scope_stack.items.len) return false;
            const b = r.scope_stack.items[i].name.?;
            if (!std.mem.eql(u8, a, b)) return false;
        }
        return true;
    }

    fn renderType(
        r: *Renderer,
        ty: MetaModel.Type,
        children: std.StringArrayHashMapUnmanaged(SymbolTree.Node),
    ) error{WriteFailed}!void {
        switch (ty) {
            .@"and", .@"or" => {},
            else => if (children.count() != 0) {
                std.debug.print("scope:\n", .{});
                for (r.scope_stack.items, 0..) |scope, i| {
                    std.debug.print("{d}: {?s}\n", .{ i, scope.name });
                }
                std.debug.print("children:\n", .{});
                for (children.keys(), 0..) |child_name, i| {
                    std.debug.print("{d}: {s}\n", .{ i, child_name });
                }
                std.debug.panic("renderType on a '{t}' type with children is unsupported", .{ty});
            },
        }

        switch (ty) {
            .base => |base| switch (base.name) {
                .URI => try r.w.writeAll("URI"),
                .DocumentUri => try r.w.writeAll("DocumentUri"),
                .integer => try r.w.writeAll("i32"),
                .uinteger => try r.w.writeAll("u32"),
                .decimal => try r.w.writeAll("f32"),
                .RegExp => try r.w.writeAll("RegExp"),
                .string => try r.w.writeAll("[]const u8"),
                .boolean => try r.w.writeAll("bool"),
                .null => try r.w.writeAll("?void"),
            },
            .reference => |ref| try r.renderReference(ref.name),
            .array => |arr| {
                try r.w.writeAll("[]const ");
                try r.renderType(arr.element.*, children);
            },
            .map => |map| {
                try r.w.writeAll("parser.Map(");
                switch (map.key) {
                    .base => |base| try switch (base.name) {
                        .Uri => r.w.writeAll("Uri"),
                        .DocumentUri => r.w.writeAll("DocumentUri"),
                        .integer => r.w.writeAll("i32"),
                        .string => r.w.writeAll("[]const u8"),
                    },
                    .reference => |ref| try r.renderType(.{ .reference = ref }, children),
                }
                try r.w.writeAll(", ");
                try r.renderType(map.value.*, children);
                try r.w.writeByte(')');
            },
            .@"and" => |andt| {
                try r.w.writeAll("struct {\n");
                for (andt.items) |item| {
                    if (item != .reference) @panic("Unimplemented and subject encountered!");
                    try r.w.print("// And {s}\n", .{item.reference.name});
                    try r.renderProperties(lookupStructure(r.meta_model, item.reference.name), null);
                    try r.w.writeAll("\n\n");
                }
                for (children.keys(), children.values()) |child_name, *child_node| {
                    try r.renderNode(child_node, child_name);
                }
                try r.w.writeAll("}");
            },
            .@"or" => |ort| {
                if (unwrapOptional(ty)) |child_type| {
                    if (children.count() != 0) @panic("unsupported");
                    try r.w.writeByte('?');
                    try r.renderType(child_type, children);
                } else if (isEnum(ty)) {
                    try r.w.writeAll("enum {");
                    for (ort.items) |sub_type| {
                        try r.w.print("{s},\n", .{sub_type.stringLiteral.value});
                    }
                    if (children.count() != 0) try r.w.writeAll("\n\n");
                    for (children.keys(), children.values()) |child_name, *child_node| {
                        try r.renderNode(child_node, child_name);
                    }
                    try r.w.writeByte('}');
                } else {
                    try r.w.writeAll("union(enum) {\n");
                    for (ort.items, 0..) |sub_type, i| {
                        std.debug.assert(!isNull(sub_type));
                        try guessFieldName(r.meta_model, r.w, sub_type, i);
                        try r.w.writeAll(": ");
                        try r.renderType(sub_type, .empty);
                        try r.w.writeAll(",\n");
                    }
                    if (children.count() != 0) try r.w.writeAll("\n\n");
                    for (children.keys(), children.values()) |child_name, *child_node| {
                        try r.renderNode(child_node, child_name);
                    }
                    try r.w.writeAll("\n\n");
                    try r.w.writeAll(
                        \\pub const jsonParse = parser.UnionParser(@This()).jsonParse;
                        \\pub const jsonParseFromValue = parser.UnionParser(@This()).jsonParseFromValue;
                        \\pub const jsonStringify = parser.UnionParser(@This()).jsonStringify;
                        \\}
                    );
                }
            },
            .tuple => |tup| {
                try r.w.writeAll("struct {");
                for (tup.items, 0..) |field_ty, i| {
                    if (i != 0) try r.w.writeByte(',');
                    try r.w.writeByte(' ');
                    try r.renderType(field_ty, children);
                }
                try r.w.writeAll(" }");
            },
            .literal => |lit| {
                try r.w.writeAll("struct {");
                if (lit.value.properties.len != 0) {
                    for (lit.value.properties) |property| {
                        try r.w.writeByte('\n');
                        try r.renderProperty(property);
                    }
                    try r.w.writeByte('\n');
                }
                try r.w.writeByte('}');
            },
            .stringLiteral => |lit| try r.w.print("[]const u8 = \"{f}\"", .{std.zig.fmtString(lit.value)}),
            .integerLiteral => @panic("unsupported"),
            .booleanLiteral => @panic("unsupported"),
        }
    }

    fn renderProperty(r: *Renderer, property: MetaModel.Property) error{WriteFailed}!void {
        std.debug.assert(property.optional == null);
        if (property.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .doc)});

        try r.w.print("{f}", .{std.zig.fmtIdPU(property.name)});
        try r.w.writeAll(": ");
        if (unwrapOptional(property.type)) |child_ty| {
            // WORKAROUND: recursive SelectionRange
            if (child_ty == .reference and std.mem.eql(u8, child_ty.reference.name, "SelectionRange")) {
                try r.w.writeAll("?*const ");
                try r.renderType(child_ty, .empty);
            } else {
                try r.renderType(property.type, .empty);
            }
            try r.w.writeAll(" = null");
        } else {
            try r.renderType(property.type, .empty);
        }
        try r.w.writeByte(',');
    }

    fn renderProperties(
        r: *Renderer,
        structure: *const MetaModel.Structure,
        maybe_extender: ?*const MetaModel.Structure,
    ) error{WriteFailed}!void {
        const properties: []MetaModel.Property = structure.properties;
        const extends: []MetaModel.Type = structure.extends orelse &.{};
        const mixins: []MetaModel.Type = structure.mixins orelse &.{};

        var has_properties = false;

        skip: for (properties) |property| {
            std.debug.assert(property.optional == null);
            if (maybe_extender) |ext| {
                for (ext.properties) |ext_property| {
                    if (std.mem.eql(u8, property.name, ext_property.name)) {
                        // std.log.info("Skipping implemented field emission: {s}", .{property.name});
                        continue :skip;
                    }
                }
            }
            try r.renderProperty(property);
            try r.w.writeByte('\n');
            has_properties = true;
        }

        if (has_properties and (extends.len != 0 or mixins.len != 0)) try r.w.writeByte('\n');

        for (extends) |ext| {
            if (ext != .reference) @panic("Expected reference for extends!");
            try r.w.print("// Extends `{s}`\n", .{ext.reference.name});
            try r.renderProperties(lookupStructure(r.meta_model, ext.reference.name), structure);
            try r.w.writeByte('\n');
        }

        for (mixins) |ext| {
            if (ext != .reference) @panic("Expected reference for mixin!");
            try r.w.print("// Uses mixin `{s}`\n", .{ext.reference.name});
            try r.renderProperties(lookupStructure(r.meta_model, ext.reference.name), structure);
            try r.w.writeByte('\n');
        }
    }

    const FormatType = struct {
        r: *Renderer,
        ty: MetaModel.Type,
        children: std.StringArrayHashMapUnmanaged(SymbolTree.Node) = .empty,

        pub fn format(
            ctx: FormatType,
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            std.debug.assert(ctx.r.w == writer);
            try ctx.r.renderType(ctx.ty, ctx.children);
        }
    };

    fn renderRequest(r: *Renderer, request: MetaModel.Request) error{WriteFailed}!void {
        if (request.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .normal)});

        try r.w.print(
            \\.{{
            \\    "{f}",
            \\    RequestMetadata{{
            \\        .method = {f},
            \\        .documentation = {?f},
            \\        .direction = .{f},
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
            fmtToSnakeCase(@tagName(request.messageDirection)),
            // Multiparams not used here, so we dont have to implement them :)
            if (request.params) |params| FormatType{ .r = r, .ty = params.one } else null,
            FormatType{ .r = r, .ty = request.result },
            if (request.partialResult) |ty| FormatType{ .r = r, .ty = ty } else null,
            if (request.errorData) |ty| FormatType{ .r = r, .ty = ty } else null,
            if (request.registrationMethod) |method| std.json.fmt(method, .{}) else null,
            if (request.registrationOptions) |ty| FormatType{ .r = r, .ty = ty } else null,
        });
    }

    fn renderNotification(r: *Renderer, notification: MetaModel.Notification) error{WriteFailed}!void {
        if (notification.documentation) |docs| try r.w.print("{f}", .{fmtDocs(docs, .normal)});

        try r.w.print(
            \\.{{
            \\    "{f}",
            \\    NotificationMetadata{{
            \\        .method = {f},
            \\        .documentation = {?f},
            \\        .direction = .{f},
            \\        .Params = {?f},
            \\        .registration = .{{ .method = {?f}, .Options = {?f} }},
            \\    }},
            \\}},
            \\
        , .{
            std.zig.fmtString(notification.method),
            std.json.fmt(notification.method, .{}),
            if (notification.documentation) |documentation| std.json.fmt(documentation, .{}) else null,
            fmtToSnakeCase(@tagName(notification.messageDirection)),
            // Multiparams not used here, so we dont have to implement them :)
            if (notification.params) |params| FormatType{ .r = r, .ty = params.one } else null,
            if (notification.registrationMethod) |method| std.json.fmt(method, .{}) else null,
            if (notification.registrationOptions) |ty| FormatType{ .r = r, .ty = ty } else null,
        });
    }
};

const SymbolNamespaceIterator = struct {
    name: []const u8,
    index: usize,
    direction: Direction,

    const Direction = enum {
        forward,
        reverse,
    };

    fn init(name: []const u8, direction: Direction) SymbolNamespaceIterator {
        return .{
            .name = name,
            .index = switch (direction) {
                .forward => 0,
                .reverse => name.len,
            },
            .direction = direction,
        };
    }

    fn next(it: *SymbolNamespaceIterator) ?[]const u8 {
        switch (it.direction) {
            .forward => {
                if (it.index == it.name.len) return null;
                const new_index = std.mem.indexOfScalarPos(u8, it.name, it.index, '.') orelse {
                    defer it.index = it.name.len;
                    return it.name[it.index..];
                };
                defer it.index = new_index + 1;
                return it.name[it.index..new_index];
            },
            .reverse => {
                if (it.index == 0) return null;
                const name = it.name[0..it.index];
                const new_index = std.mem.lastIndexOfScalar(u8, name, '.') orelse {
                    it.index = 0;
                    return name;
                };
                it.index = new_index;
                return name[it.index + 1 ..];
            },
        }
    }
};

test SymbolNamespaceIterator {
    var it: SymbolNamespaceIterator = .init("foo.bar.baz", .forward);
    try std.testing.expectEqualStrings("foo", it.next().?);
    try std.testing.expectEqualStrings("bar", it.next().?);
    try std.testing.expectEqualStrings("baz", it.next().?);
    try std.testing.expect(it.next() == null);

    it = .init("foo", .forward);
    try std.testing.expectEqualStrings("foo", it.next().?);
    try std.testing.expect(it.next() == null);

    it = .init("foo.bar.baz", .reverse);
    try std.testing.expectEqualStrings("baz", it.next().?);
    try std.testing.expectEqualStrings("bar", it.next().?);
    try std.testing.expectEqualStrings("foo", it.next().?);
    try std.testing.expect(it.next() == null);

    it = .init("foo", .reverse);
    try std.testing.expectEqualStrings("foo", it.next().?);
    try std.testing.expect(it.next() == null);
}

fn constructSymbolTree(
    gpa: std.mem.Allocator,
    arena: std.mem.Allocator,
    meta_model: *const MetaModel,
) error{OutOfMemory}!SymbolTree {
    var symbols: std.StringArrayHashMapUnmanaged(Symbol) = .empty;
    defer symbols.deinit(gpa);

    try symbols.ensureTotalCapacity(
        gpa,
        meta_model.structures.len + meta_model.enumerations.len + meta_model.typeAliases.len + config.symbolize.len,
    );

    for (meta_model.structures) |structure| {
        symbols.putAssumeCapacityNoClobber(structure.name, .{ .structure = structure });
    }
    for (meta_model.enumerations) |enumeration| {
        symbols.putAssumeCapacityNoClobber(enumeration.name, .{ .enumeration = enumeration });
    }
    for (meta_model.typeAliases) |type_alias| {
        symbols.putAssumeCapacityNoClobber(type_alias.name, .{ .decl = .{
            .type = type_alias.type,
            .original_name = type_alias.name,
            .documentation = type_alias.documentation,
        } });
    }

    for (config.symbolize) |property_name| {
        const expect_optional = std.mem.endsWith(u8, property_name, ".?");
        const trimmed_property_name = property_name[0 .. property_name.len - @as(usize, if (expect_optional) 2 else 0)];
        const property = lookupProperty(trimmed_property_name, meta_model);
        std.debug.assert(property.optional == null);

        if (expect_optional) property.type = unwrapOptional(property.type).?;

        symbols.putAssumeCapacityNoClobber(property_name, .{ .decl = .{
            .type = property.type,
            .original_name = null,
            .documentation = property.documentation,
        } });

        const reference_type: MetaModel.Type = .{ .reference = .{ .name = property_name } };
        property.type = if (expect_optional) try createOptional(arena, reference_type) else reference_type;
    }

    // Add symbols for the Result types i.e. return types of various requests.
    for (meta_model.requests) |*request| {
        const distinct_result_type = findInnerDistinctType(&request.result, meta_model) orelse continue;

        const result_name = try std.fmt.allocPrint(arena, "{s}.Result", .{request.method});
        try symbols.putNoClobber(gpa, result_name, .{ .decl = .{
            .type = distinct_result_type.*,
            .original_name = null,
            .documentation = null,
        } });
        defer distinct_result_type.* = .{ .reference = .{ .name = result_name } };

        const distinct_partial_result_type = findInnerDistinctType(if (request.partialResult) |*ty| ty else continue, meta_model) orelse continue;
        if (distinct_partial_result_type.eql(distinct_result_type.*)) {
            distinct_partial_result_type.* = .{ .reference = .{ .name = result_name } };
        } else {
            const partial_result_name = try std.fmt.allocPrint(arena, "{s}.PartialResult", .{request.method});
            try symbols.putNoClobber(gpa, partial_result_name, .{ .decl = .{
                .type = distinct_partial_result_type.*,
                .original_name = null,
                .documentation = null,
            } });
            distinct_partial_result_type.* = .{ .reference = .{ .name = partial_result_name } };
        }
    }

    var symbol_tree: SymbolTree = .{};
    errdefer symbol_tree.deinit(gpa);

    for (config.symbols) |item| {
        const name, const action = item;
        switch (action) {
            .remove => {
                if (!symbols.swapRemove(name)) {
                    std.debug.panic("invalid symbol '{s}' in @import(\"config.zig\").symbols", .{name});
                }
            },
            .rename => |new_name| {
                const kv = symbols.fetchSwapRemove(name) orelse
                    std.debug.panic("invalid symbol '{s}' in @import(\"config.zig\").symbols", .{name});
                try symbol_tree.insert(gpa, new_name, kv.value);
            },
            .replace_with => {
                if (!symbols.swapRemove(name)) {
                    std.debug.panic("invalid symbol '{s}' in @import(\"config.zig\").symbols", .{name});
                }
            },
        }
    }

    if (symbols.count() > 0) {
        for (symbols.keys()) |missing| {
            std.log.warn("The LSP type '{s}' is missing in @import(\"config.zig\").symbols", .{missing});
        }
    }

    return symbol_tree;
}

fn lookupStructure(meta_model: *const MetaModel, name: []const u8) *const MetaModel.Structure {
    for (meta_model.structures) |*s| {
        if (std.mem.eql(u8, s.name, name)) return s;
    }
    std.debug.panic("could not resolve reference to '{s}'", .{name});
}

fn lookupProperty(name: []const u8, meta_model: *const MetaModel) *MetaModel.Property {
    var name_it: SymbolNamespaceIterator = .init(name, .forward);
    const structure_name = name_it.next().?;
    const first_property_name = name_it.next().?;
    std.debug.assert(name_it.next() == null);

    const structure = lookupStructure(meta_model, structure_name);
    for (structure.properties) |*property| {
        if (std.mem.eql(u8, property.name, first_property_name)) return property;
    }
    for (structure.extends orelse &.{}) |ty| {
        const s = lookupStructure(meta_model, ty.reference.name);
        for (s.properties) |*property| {
            if (std.mem.eql(u8, property.name, first_property_name)) return property;
        }
    }
    std.debug.panic("could not find field '{s}' in {s}", .{ first_property_name, structure.name });
}

/// - normalize representation of an optional union type
/// - normalize representation of optional properties
fn normalizeMetaModel(arena: std.mem.Allocator, meta_model: *MetaModel) error{OutOfMemory}!void {
    const ctx = struct {
        fn onType(ally: std.mem.Allocator, ty: *MetaModel.Type) error{OutOfMemory}!void {
            // normalize the representation of an optional union type
            if (unwrapOptionalUnion(ty.*)) |inner| {
                ty.* = try createOptional(ally, inner);
                return;
            }
            // TODO consider flattening nested or types (e.g `Definition.Result`)

            switch (ty.*) {
                .array => |*array| try onType(ally, array.element),
                .map => |map| try onType(ally, map.value),
                .@"and" => |and_ty| for (and_ty.items) |*inner_ty| try onType(ally, inner_ty),
                .@"or" => |or_ty| for (or_ty.items) |*inner_ty| try onType(ally, inner_ty),
                .tuple => |tuple| for (tuple.items) |*inner_ty| try onType(ally, inner_ty),
                .literal => |literal| for (literal.value.properties) |*property| try onProperty(ally, property),
                .base, .reference, .stringLiteral, .integerLiteral, .booleanLiteral => {},
            }
        }
        fn onProperty(ally: std.mem.Allocator, property: *MetaModel.Property) error{OutOfMemory}!void {
            try onType(ally, &property.type);
            if (property.optional orelse false) {
                if (unwrapOptional(property.type) == null) {
                    property.type = try createOptional(ally, property.type);
                }
                property.optional = null;
            }
        }
    };
    const onType = ctx.onType;
    const onProperty = ctx.onProperty;
    for (meta_model.requests) |*request| {
        if (request.params) |*params| switch (params.*) {
            .one => |*ty| try onType(arena, ty),
            .multiple => |types| for (types) |*ty| try onType(arena, ty),
        };
        try onType(arena, &request.result);
        if (request.partialResult) |*ty| try onType(arena, ty);
        if (request.errorData) |*ty| try onType(arena, ty);
        if (request.registrationOptions) |*ty| try onType(arena, ty);
    }
    for (meta_model.notifications) |*notification| {
        if (notification.params) |*params| switch (params.*) {
            .one => |*ty| try onType(arena, ty),
            .multiple => |types| for (types) |*ty| try onType(arena, ty),
        };
        if (notification.registrationOptions) |*ty| try onType(arena, ty);
    }
    for (meta_model.structures) |*structure| {
        if (structure.extends) |extends| for (extends) |*ty| try onType(arena, ty);
        if (structure.mixins) |mixins| for (mixins) |*ty| try onType(arena, ty);
        for (structure.properties) |*property| try onProperty(arena, property);
    }
    // for (meta_model.enumerations) |*enumeration| {}
    for (meta_model.typeAliases) |*type_alias| {
        try onType(arena, &type_alias.type);
    }
}

fn renderMetaModel(gpa: std.mem.Allocator, meta_model: *MetaModel) error{ OutOfMemory, WriteFailed }![:0]u8 {
    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    try normalizeMetaModel(arena.allocator(), meta_model);

    var symbol_tree = try constructSymbolTree(gpa, arena.allocator(), meta_model);
    defer symbol_tree.deinit(gpa);

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    var scope_stack: [8]Renderer.Scope = undefined;
    var renderer: Renderer = .{
        .scope_stack = .initBuffer(&scope_stack),
        .meta_model = meta_model,
        .w = &aw.writer,
    };
    renderer.scope_stack.appendAssumeCapacity(.{ .name = null, .symbols = symbol_tree.root });

    // try symbol_tree.dump();
    // std.log.info("root symbol count: {d}", .{symbol_tree.root.count()});

    try renderer.w.writeAll(@embedFile("lsp_types_base.zig") ++ "\n");

    for (symbol_tree.root.keys(), symbol_tree.root.values()) |name, *symbol| {
        try renderer.renderNode(symbol, name);
    }

    {
        var original_symbol_names: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer original_symbol_names.deinit(gpa);
        try original_symbol_names.ensureTotalCapacity(gpa, meta_model.structures.len + meta_model.enumerations.len + meta_model.typeAliases.len);
        for (meta_model.structures) |structure| original_symbol_names.putAssumeCapacityNoClobber(structure.name, {});
        for (meta_model.enumerations) |enumeration| original_symbol_names.putAssumeCapacityNoClobber(enumeration.name, {});
        for (meta_model.typeAliases) |type_alias| original_symbol_names.putAssumeCapacityNoClobber(type_alias.name, {});

        original_symbol_names.sort(struct {
            names: []const []const u8,
            pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
                const a_name = ctx.names[a_index];
                const b_name = ctx.names[b_index];
                return symbol_map.getIndex(a_name).? < symbol_map.getIndex(b_name).?;
            }
        }{ .names = original_symbol_names.keys() });

        try renderer.w.writeAll(
            \\/// A flat namespace that aliases all LSP types under their original name from the official specification.
            \\pub const flat = struct {
            \\  pub const parser = types.parser;
            \\  pub const URI = types.URI;
            \\  pub const DocumentUri = types.DocumentUri;
            \\  pub const RegExp = types.RegExp;
            \\  pub const LSPAny = types.LSPAny;
            \\  pub const LSPArray = types.LSPArray;
            \\  pub const LSPObject = types.LSPObject;
            \\  pub const ID = types.ID;
            \\  pub const MessageDirection = types.MessageDirection;
            \\  pub const RegistrationMetadata = types.RegistrationMetadata;
            \\  pub const NotificationMetadata = types.NotificationMetadata;
            \\  pub const RequestMetadata = types.RequestMetadata;
            \\  pub const requests = types.requests;
            \\  pub const notifications = types.notifications;
            \\
            \\
        );

        for (original_symbol_names.keys()) |name| {
            switch (symbol_map.get(name).?) {
                .remove => continue,
                .rename, .replace_with => |new_name| try renderer.w.print("  pub const {s} = types.{s};\n", .{ name, new_name }),
            }
        }
        try renderer.w.writeAll("};\n\n");
    }

    {
        try renderer.w.writeAll("const notifications_generated: std.StaticStringMap(NotificationMetadata) = .initComptime(&.{\n");
        for (meta_model.notifications) |notification| {
            try renderer.renderNotification(notification);
        }
        try renderer.w.writeAll("\n});\n\n");

        try renderer.w.writeAll("const requests_generated: std.StaticStringMap(RequestMetadata) = .initComptime(&.{\n");
        for (meta_model.requests) |request| {
            try renderer.renderRequest(request);
        }
        try renderer.w.writeAll("});\n");
    }

    return try aw.toOwnedSliceSentinel(0);
}

const Config = struct {
    symbols: []const struct { []const u8, Action },
    /// List of expressions that should be refactored i.e symbolized into a new
    /// declaration to avoid generating anonymous types.
    /// The new name of the declaration will be looked up in `symbols`.
    symbolize: []const []const u8,

    const Action = union(enum) {
        /// Remove the given symbol.
        remove,
        /// Rename a symbol to the given name.
        rename: []const u8,
        /// Replace every reference of a symbol with a different one.
        replace_with: []const u8,
    };
};

const config: Config = @import("config.zon");
const symbol_map: std.StaticStringMap(Config.Action) = .initComptime(config.symbols);
