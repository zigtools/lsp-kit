//! Custom `std.json` parser functions.

const std = @import("std");

pub fn Map(comptime Key: type, comptime Value: type) type {
    if (Key != []const u8) @compileError("TODO support non string Key's");
    return std.json.ArrayHashMap(Value);
}

pub fn UnionParser(comptime T: type) type {
    return struct {
        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            // TODO this implementation is incredibly naive, surely it can be improved somewhat
            const json_value = try std.json.Value.jsonParse(allocator, source, options);
            return try jsonParseFromValue(allocator, json_value, options);
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            inline for (std.meta.fields(T)) |field| {
                if (std.json.parseFromValueLeaky(field.type, allocator, source, options)) |result| {
                    return @unionInit(T, field.name, result);
                } else |_| {}
            }
            return error.UnexpectedToken;
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            switch (self) {
                inline else => |value| try stream.write(value),
            }
        }
    };
}

test UnionParser {
    {
        const U = union(enum) {
            number: u32,
            string: []const u8,
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .number = 3 }, "3");
        try expectParseEqual(U, U{ .string = "foo" }, "\"foo\"");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        // same as above but the fields are in reverse order
        const U = union(enum) {
            string: []const u8,
            number: u32,
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .number = 3 }, "3");
        try expectParseEqual(U, U{ .string = "foo" }, "\"foo\"");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        const U = union(enum) {
            foo: struct {},
            bar: struct { member: u32 },
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .foo = .{} }, "{}");
        try expectParseEqual(U, U{ .bar = .{ .member = 5 } }, "{\"member\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        // same as above but the fields are in reverse order
        const U = union(enum) {
            bar: struct { member: u32 },
            foo: struct {},
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .foo = .{} }, "{}");
        try expectParseEqual(U, U{ .bar = .{ .member = 5 } }, "{\"member\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "{\"mumber\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }
}

test "UnionParser.jsonStringify" {
    const U = union(enum) {
        number: u32,
        string: []const u8,
        pub const jsonStringify = UnionParser(@This()).jsonStringify;
    };

    try std.testing.expectFmt("5", "{f}", .{std.json.fmt(U{ .number = 5 }, .{})});
    try std.testing.expectFmt("\"foo\"", "{f}", .{std.json.fmt(U{ .string = "foo" }, .{})});
}

pub fn EnumCustomStringValues(comptime T: type, comptime contains_empty_enum: bool) type {
    return struct {
        comptime {
            if (contains_empty_enum) std.debug.assert(@hasField(T, "empty"));
            std.debug.assert(@hasField(T, "custom_value") or @hasField(T, "unknown_value"));
        }
        const special_value_indicator: std.meta.Tag(T) = if (@hasField(T, "custom_value")) T.custom_value else T.unknown_value;
        const special_value_field_name: []const u8 = if (@hasField(T, "custom_value")) "custom_value" else "unknown_value";

        const kvs = build_kvs: {
            const KV = struct { []const u8, T };
            const fields = std.meta.fields(T);
            var kvs_array: [fields.len - 1]KV = undefined;
            for (fields[0 .. fields.len - 1], &kvs_array) |field, *kv| {
                if (contains_empty_enum and std.mem.eql(u8, field.name, "empty")) {
                    kv.* = .{ "", T.empty };
                } else {
                    kv.* = .{ field.name, @field(T, field.name) };
                }
            }
            break :build_kvs kvs_array;
        };

        const enum_from_string_map: std.StaticStringMap(T) = .initComptime(kvs);

        pub fn eql(a: T, b: T) bool {
            const tag_a = std.meta.activeTag(a);
            const tag_b = std.meta.activeTag(b);

            const is_a_empty = (contains_empty_enum and a == .empty) or (tag_a == special_value_indicator and @field(a, special_value_field_name).len == 0);
            const is_b_empty = (contains_empty_enum and b == .empty) or (tag_b == special_value_indicator and @field(b, special_value_field_name).len == 0);
            if (is_a_empty and is_b_empty) return true;

            if (tag_a != tag_b) return false;

            if (tag_a == special_value_indicator) {
                return std.mem.eql(u8, @field(a, special_value_field_name), @field(b, special_value_field_name));
            } else {
                return true;
            }
        }

        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            const slice = try std.json.innerParse([]const u8, allocator, source, options);
            return enum_from_string_map.get(slice) orelse
                return @unionInit(T, special_value_field_name, slice);
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            const slice = try std.json.parseFromValueLeaky([]const u8, allocator, source, options);
            return enum_from_string_map.get(slice) orelse
                return @unionInit(T, special_value_field_name, slice);
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            if (contains_empty_enum and self == .empty) {
                try stream.write("");
                return;
            }
            switch (self) {
                special_value_indicator => |str| try stream.write(str),
                else => |val| try stream.write(@tagName(val)),
            }
        }
    };
}

test EnumCustomStringValues {
    {
        const E = union(enum) {
            foo,
            bar,
            baz,
            custom_value: []const u8,
            pub const eql = EnumCustomStringValues(@This(), false).eql;
            pub const jsonParse = EnumCustomStringValues(@This(), false).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), false).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), false).jsonStringify;
        };

        try std.testing.expectFmt("\"foo\"", "{f}", .{std.json.fmt(E{ .foo = {} }, .{})});
        try std.testing.expectFmt("\"bar\"", "{f}", .{std.json.fmt(E{ .bar = {} }, .{})});
        try std.testing.expectFmt("\"baz\"", "{f}", .{std.json.fmt(E{ .baz = {} }, .{})});
        try std.testing.expectFmt("\"\"", "{f}", .{std.json.fmt(E{ .custom_value = "" }, .{})});
        try std.testing.expectFmt("\"boo\"", "{f}", .{std.json.fmt(E{ .custom_value = "boo" }, .{})});

        try expectParseEqual(E, E.foo, "\"foo\"");
        try expectParseEqual(E, E.bar, "\"bar\"");
        try expectParseEqual(E, E.baz, "\"baz\"");

        try expectParseEqual(E, E{ .custom_value = "boo" }, "\"boo\"");
        try expectParseEqual(E, E{ .custom_value = "" }, "\"\"");
    }

    {
        const E = union(enum) {
            foo,
            bar,
            baz,
            empty,
            unknown_value: []const u8,
            pub const eql = EnumCustomStringValues(@This(), true).eql;
            pub const jsonParse = EnumCustomStringValues(@This(), true).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), true).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), true).jsonStringify;
        };

        const foo: E = .foo;
        const bar: E = .bar;
        const baz: E = .baz;
        const empty: E = .empty;
        const custom_empty: E = .{ .unknown_value = "" };
        const boo: E = .{ .unknown_value = "boo" };
        const zoo: E = .{ .unknown_value = "zoo" };

        try std.testing.expectFmt("\"foo\"", "{f}", .{std.json.fmt(foo, .{})});
        try std.testing.expectFmt("\"bar\"", "{f}", .{std.json.fmt(bar, .{})});
        try std.testing.expectFmt("\"baz\"", "{f}", .{std.json.fmt(baz, .{})});
        try std.testing.expectFmt("\"\"", "{f}", .{std.json.fmt(empty, .{})});
        try std.testing.expectFmt("\"\"", "{f}", .{std.json.fmt(custom_empty, .{})});
        try std.testing.expectFmt("\"boo\"", "{f}", .{std.json.fmt(boo, .{})});
        try std.testing.expectFmt("\"zoo\"", "{f}", .{std.json.fmt(zoo, .{})});

        try expectParseEqual(E, foo, "\"foo\"");
        try expectParseEqual(E, bar, "\"bar\"");
        try expectParseEqual(E, baz, "\"baz\"");
        try expectParseEqual(E, empty, "\"\"");
        try expectParseEqual(E, boo, "\"boo\"");
        try expectParseEqual(E, zoo, "\"zoo\"");

        try std.testing.expect(foo.eql(foo));
        try std.testing.expect(!foo.eql(bar));
        try std.testing.expect(foo.eql(foo));
        try std.testing.expect(!foo.eql(zoo));

        try std.testing.expect(empty.eql(empty));
        try std.testing.expect(!empty.eql(foo));
        try std.testing.expect(!empty.eql(boo));

        try std.testing.expect(custom_empty.eql(custom_empty));
        try std.testing.expect(!custom_empty.eql(foo));
        try std.testing.expect(!custom_empty.eql(boo));

        try std.testing.expect(empty.eql(custom_empty));
        try std.testing.expect(custom_empty.eql(empty));
    }

    {
        const E = union(enum) {
            foo,
            empty,
            custom_value: []const u8,
            pub const eql = EnumCustomStringValues(@This(), false).eql;
            pub const jsonParse = EnumCustomStringValues(@This(), false).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), false).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), false).jsonStringify;
        };

        const foo: E = .foo;
        const empty: E = .empty;
        const true_empty: E = .{ .custom_value = "" };
        const boo: E = .{ .custom_value = "boo" };

        try std.testing.expectFmt("\"foo\"", "{f}", .{std.json.fmt(foo, .{})});
        try std.testing.expectFmt("\"empty\"", "{f}", .{std.json.fmt(empty, .{})});
        try std.testing.expectFmt("\"\"", "{f}", .{std.json.fmt(true_empty, .{})});
        try std.testing.expectFmt("\"boo\"", "{f}", .{std.json.fmt(boo, .{})});

        try expectParseEqual(E, foo, "\"foo\"");
        try expectParseEqual(E, empty, "\"empty\"");
        try expectParseEqual(E, true_empty, "\"\"");
        try expectParseEqual(E, boo, "\"boo\"");

        try std.testing.expect(empty.eql(empty));
        try std.testing.expect(!empty.eql(foo));
        try std.testing.expect(!empty.eql(boo));

        try std.testing.expect(true_empty.eql(true_empty));
        try std.testing.expect(!true_empty.eql(foo));
        try std.testing.expect(!true_empty.eql(boo));

        try std.testing.expect(!true_empty.eql(empty));
        try std.testing.expect(!empty.eql(true_empty));
    }
}

pub fn EnumStringifyAsInt(comptime T: type) type {
    return struct {
        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            try stream.write(@intFromEnum(self));
        }
    };
}

test EnumStringifyAsInt {
    {
        const E = enum {
            foo,
            bar,
            baz,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("0", "{f}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("1", "{f}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("2", "{f}", .{std.json.fmt(E.baz, .{})});
    }

    {
        const E = enum(u8) {
            foo = 2,
            bar,
            baz = 5,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("2", "{f}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("3", "{f}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("5", "{f}", .{std.json.fmt(E.baz, .{})});
    }

    {
        const E = enum(u8) {
            foo,
            bar = 3,
            baz,
            _,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("0", "{f}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("3", "{f}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("4", "{f}", .{std.json.fmt(E.baz, .{})});
        try std.testing.expectFmt("7", "{f}", .{std.json.fmt(@as(E, @enumFromInt(7)), .{})});
    }
}

fn expectParseEqual(comptime T: type, comptime expected: anytype, s: []const u8) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    if (@typeInfo(@TypeOf(expected)) != .error_set) {
        const actual_from_slice = try std.json.parseFromSliceLeaky(T, arena, s, .{});
        try std.testing.expectEqualDeep(@as(T, expected), actual_from_slice);

        const value = try std.json.parseFromSliceLeaky(std.json.Value, arena, s, .{});
        const actual_from_value = std.json.parseFromValueLeaky(T, arena, value, .{});
        try std.testing.expectEqualDeep(@as(T, expected), actual_from_value);
    } else {
        try std.testing.expectError(expected, std.json.parseFromSliceLeaky(T, arena, s, .{}));
        const value = try std.json.parseFromSliceLeaky(std.json.Value, arena, s, .{});
        try std.testing.expectError(expected, std.json.parseFromValueLeaky(T, arena, value, .{}));
    }
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
