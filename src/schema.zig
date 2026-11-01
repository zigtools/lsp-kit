//! Generates JSON Schemas from Zig types at compile time.
//!
//! The generated schema matches how `std.json` (de)serializes the type:
//!
//! | Zig type                          | JSON Schema                                     |
//! |-----------------------------------|-------------------------------------------------|
//! | `bool`                            | `{"type":"boolean"}`                            |
//! | integers                          | `{"type":"integer"}` with the type's `minimum`/`maximum` |
//! | floats                            | `{"type":"number"}`                             |
//! | `[]const u8`, `[N]u8`             | `{"type":"string"}`                             |
//! | slices and arrays                 | `{"type":"array","items":...}`                  |
//! | `enum`                            | `{"type":"string","enum":[...]}`                |
//! | `?T` (struct field)               | schema of `T`, the field is not required        |
//! | `?T` (elsewhere, e.g. array item) | `{"anyOf":[<T>,{"type":"null"}]}`               |
//! | `struct`                          | `{"type":"object","properties":...,"required":...}` |
//! | `std.json.Value`                  | `{}` (any value)                                |
//!
//! A struct field is required unless it is optional or has a default value.
//!
//! Types can customize their schema by declaring:
//!
//! - `pub const json_schema: []const u8` to replace the generated schema
//!   entirely. Must be valid minified JSON. Required for types with a custom
//!   `jsonStringify`, `jsonParse` or `jsonParseFromValue`, whose JSON does not
//!   follow their fields.
//! - `pub const descriptions` as a struct literal that maps field names to
//!   human readable descriptions, e.g. `.{ .query = "The text to search for" }`.

const std = @import("std");

/// Returns the minified JSON Schema of `T`.
pub fn generate(comptime T: type) []const u8 {
    @setEvalBranchQuota(100_000);
    return comptime schemaOf(T, null);
}

/// Like `generate` but always emits an object schema. MCP requires the input
/// schema of a tool to be of type `"object"`. `void` produces an object
/// schema without properties.
pub fn generateObject(comptime T: type) []const u8 {
    if (T == void) return "{\"type\":\"object\"}";
    switch (@typeInfo(T)) {
        .@"struct" => |info| if (info.is_tuple) @compileError("expected a struct type but got the tuple " ++ @typeName(T)),
        else => @compileError("expected a struct type but got " ++ @typeName(T)),
    }
    const result = comptime generate(T);
    if (comptime !std.mem.startsWith(u8, result, "{\"type\":\"object\"")) {
        @compileError(@typeName(T) ++ ".json_schema must describe an object and start with `{\"type\":\"object\"`");
    }
    return result;
}

fn schemaOf(comptime T: type, comptime description: ?[]const u8) []const u8 {
    const is_container = switch (@typeInfo(T)) {
        .@"struct", .@"enum", .@"union", .@"opaque" => true,
        else => false,
    };
    if (is_container and @hasDecl(T, "json_schema")) {
        const custom: []const u8 = T.json_schema;
        if (std.mem.findScalar(u8, custom, '\n') != null) {
            @compileError(@typeName(T) ++ ".json_schema must be minified JSON without newlines");
        }
        if (description != null) return withDescription(custom, description.?);
        return custom;
    }

    const desc = if (description) |d| ",\"description\":" ++ jsonString(d) else "";

    if (T == std.json.Value) {
        if (description) |d| return "{\"description\":" ++ jsonString(d) ++ "}";
        return "{}";
    }

    if (is_container) assertDefaultSerialization(T);

    return switch (@typeInfo(T)) {
        .bool => "{\"type\":\"boolean\"" ++ desc ++ "}",
        .int => |info| if (info.bits <= max_bounded_int_bits)
            "{\"type\":\"integer\",\"minimum\":" ++ intString(std.math.minInt(T)) ++
                ",\"maximum\":" ++ intString(std.math.maxInt(T)) ++ desc ++ "}"
        else switch (info.signedness) {
            // Wider bounds are not exactly representable as doubles, which most
            // JSON Schema validators use, and would mostly add noise.
            .signed => "{\"type\":\"integer\"" ++ desc ++ "}",
            .unsigned => "{\"type\":\"integer\",\"minimum\":0" ++ desc ++ "}",
        },
        .comptime_int => "{\"type\":\"integer\"" ++ desc ++ "}",
        .float, .comptime_float => "{\"type\":\"number\"" ++ desc ++ "}",
        // Struct fields unwrap the optional themselves because `null` fields are omitted when serializing.
        .optional => |info| "{\"anyOf\":[" ++ schemaOf(info.child, null) ++ ",{\"type\":\"null\"}]" ++ desc ++ "}",
        .@"enum" => |info| blk: {
            var values: []const u8 = "";
            for (info.field_names) |name| values = listAppend(values, jsonString(name));
            const named = "{\"type\":\"string\",\"enum\":[" ++ values ++ "]";
            break :blk switch (info.mode) {
                .exhaustive => named ++ desc ++ "}",
                // `std.json` serializes unnamed values as integers.
                .nonexhaustive => "{\"anyOf\":[" ++ named ++ "}," ++ schemaOf(info.tag_type, null) ++ "]" ++ desc ++ "}",
            };
        },
        .pointer => |info| switch (info.size) {
            .slice => if (info.child == u8)
                "{\"type\":\"string\"" ++ desc ++ "}"
            else
                "{\"type\":\"array\",\"items\":" ++ schemaOf(info.child, null) ++ desc ++ "}",
            .one => switch (@typeInfo(info.child)) {
                .array => |array_info| schemaOf([]const array_info.child, description),
                else => schemaOf(info.child, description),
            },
            .many, .c => @compileError("unable to generate a JSON schema for " ++ @typeName(T)),
        },
        // `std.json` requires exactly `len` bytes, which JSON Schema cannot express
        // (`minLength`/`maxLength` count characters).
        .array => |info| if (info.child == u8)
            "{\"type\":\"string\"" ++ desc ++ "}"
        else
            "{\"type\":\"array\",\"items\":" ++ schemaOf(info.child, null) ++
                ",\"minItems\":" ++ intString(info.len) ++ ",\"maxItems\":" ++ intString(info.len) ++ desc ++ "}",
        .@"struct" => structSchema(T, "", "", desc),
        .@"union" => @compileError("unable to generate a JSON schema for the union " ++ @typeName(T) ++ "; declare `pub const json_schema` on it"),
        else => @compileError("unable to generate a JSON schema for " ++ @typeName(T)),
    };
}

/// Generates an object schema for the struct `T` with additional leading
/// `properties` and `required` entries (comma separated, may be empty).
fn structSchema(
    comptime T: type,
    comptime extra_properties: []const u8,
    comptime extra_required: []const u8,
    comptime desc: []const u8,
) []const u8 {
    const info = @typeInfo(T).@"struct";
    if (info.is_tuple) @compileError("unable to generate a JSON schema for the tuple " ++ @typeName(T));
    var properties: []const u8 = extra_properties;
    var required: []const u8 = extra_required;
    for (info.field_names, info.field_types, info.field_attrs) |name, FieldType, attrs| {
        if (attrs.@"comptime") continue;
        const field_description = fieldDescription(T, name);
        const is_optional = @typeInfo(FieldType) == .optional;
        const PropertyType = if (is_optional) @typeInfo(FieldType).optional.child else FieldType;
        properties = listAppend(properties, jsonString(name) ++ ":" ++ schemaOf(PropertyType, field_description));
        if (!is_optional and attrs.default_value_ptr == null) {
            required = listAppend(required, jsonString(name));
        }
    }
    const required_part = if (required.len == 0) "" else ",\"required\":[" ++ required ++ "]";
    return "{\"type\":\"object\",\"properties\":{" ++ properties ++ "}" ++ required_part ++ desc ++ "}";
}

/// Returns the schema of a tagged union whose payloads are structs and whose
/// active field is stored in the `tag_field` property, like
/// `types.TypeDiscriminated` serializes it.
pub fn generateDiscriminated(comptime T: type, comptime tag_field: []const u8) []const u8 {
    @setEvalBranchQuota(100_000);
    return comptime blk: {
        const info = @typeInfo(T).@"union";
        var variants: []const u8 = "";
        for (info.field_names, info.field_types) |name, Payload| {
            if (@typeInfo(Payload) != .@"struct") @compileError(@typeName(T) ++ "." ++ name ++ " must be a struct");
            if (@hasField(Payload, tag_field)) {
                @compileError(@typeName(Payload) ++ " has a field named '" ++ tag_field ++ "', which clashes with the discriminator of " ++ @typeName(T));
            }
            const tag_property = jsonString(tag_field) ++ ":{\"const\":" ++ jsonString(name) ++ "}";
            const variant = if (@hasDecl(Payload, "json_schema"))
                "{\"allOf\":[{\"type\":\"object\",\"properties\":{" ++ tag_property ++ "},\"required\":[" ++ jsonString(tag_field) ++ "]}," ++ schemaOf(Payload, null) ++ "]}"
            else variant: {
                assertDefaultSerialization(Payload);
                break :variant structSchema(Payload, tag_property, jsonString(tag_field), "");
            };
            variants = listAppend(variants, variant);
        }
        break :blk "{\"anyOf\":[" ++ variants ++ "]}";
    };
}

/// Returns `{"anyOf":[...]}` with the schema of each payload of the union `T`.
/// For unions that serialize their active payload directly.
pub fn generateAnyOfPayloads(comptime T: type) []const u8 {
    @setEvalBranchQuota(100_000);
    return comptime blk: {
        var variants: []const u8 = "";
        for (@typeInfo(T).@"union".field_types) |Payload| variants = listAppend(variants, schemaOf(Payload, null));
        break :blk "{\"anyOf\":[" ++ variants ++ "]}";
    };
}

fn assertDefaultSerialization(comptime T: type) void {
    if (std.meta.hasFn(T, "jsonStringify") or std.meta.hasFn(T, "jsonParse") or std.meta.hasFn(T, "jsonParseFromValue")) {
        @compileError(@typeName(T) ++ " has custom JSON (de)serialization, so its schema cannot be derived from its fields; declare `pub const json_schema`");
    }
}

/// Integer types up to this size get `minimum`/`maximum` bounds.
const max_bounded_int_bits = 32;

/// Appends `item` to a comma separated list.
fn listAppend(comptime list: []const u8, comptime item: []const u8) []const u8 {
    return if (list.len == 0) item else list ++ "," ++ item;
}

/// Returns the path of the first optional struct field inside `T` that has no
/// default value, or `null` if there is none.
///
/// `std.json` fails to parse a struct if such a field is missing, even though
/// the generated schema does not list it as required. Types that are parsed from
/// client input (e.g. tool arguments) should give every optional field a default.
pub fn findOptionalWithoutDefault(comptime T: type) ?[]const u8 {
    @setEvalBranchQuota(100_000);
    return comptime findOptionalWithoutDefaultInner(T, @typeName(T), &.{});
}

fn findOptionalWithoutDefaultInner(comptime T: type, comptime path: []const u8, comptime visited: []const type) ?[]const u8 {
    for (visited) |V| if (V == T) return null;
    const next_visited = visited ++ [1]type{T};
    switch (@typeInfo(T)) {
        .optional => |info| return findOptionalWithoutDefaultInner(info.child, path, next_visited),
        .pointer => |info| return findOptionalWithoutDefaultInner(info.child, path, next_visited),
        .array => |info| return findOptionalWithoutDefaultInner(info.child, path, next_visited),
        .@"struct" => |info| {
            if (info.is_tuple or @hasDecl(T, "json_schema")) return null;
            for (info.field_names, info.field_types, info.field_attrs) |name, FieldType, attrs| {
                if (attrs.@"comptime") continue;
                const field_path = path ++ "." ++ name;
                if (@typeInfo(FieldType) == .optional and attrs.default_value_ptr == null) return field_path;
                if (findOptionalWithoutDefaultInner(FieldType, field_path, next_visited)) |found| return found;
            }
            return null;
        },
        else => return null,
    }
}

test findOptionalWithoutDefault {
    const Ok = struct {
        a: []const u8,
        b: ?u32 = null,
        nested: []const struct { c: ?bool = null },
    };
    try std.testing.expectEqual(null, comptime findOptionalWithoutDefault(Ok));

    const Inner = struct { c: ?bool };
    const Bad = struct {
        a: []const u8,
        nested: ?[]const Inner = null,
    };
    try std.testing.expectEqualStrings(@typeName(Bad) ++ ".nested.c", comptime findOptionalWithoutDefault(Bad).?);

    const Recursive = struct {
        children: []const @This(),
        value: ?u32 = null,
    };
    try std.testing.expectEqual(null, comptime findOptionalWithoutDefault(Recursive));
}

fn fieldDescription(comptime T: type, comptime field_name: []const u8) ?[]const u8 {
    if (!@hasDecl(T, "descriptions")) return null;
    const descriptions = T.descriptions;
    if (!@hasField(@TypeOf(descriptions), field_name)) return null;
    return @field(descriptions, field_name);
}

/// Inserts a `"description"` into an existing object schema.
fn withDescription(comptime schema: []const u8, comptime description: []const u8) []const u8 {
    const is_object = schema.len >= 2 and schema[0] == '{' and schema[schema.len - 1] == '}';
    // Boolean schemas (`true`/`false`) cannot have keywords.
    if (!is_object) return "{\"allOf\":[" ++ schema ++ "],\"description\":" ++ jsonString(description) ++ "}";
    const separator = if (schema.len == 2) "" else ",";
    return schema[0 .. schema.len - 1] ++ separator ++ "\"description\":" ++ jsonString(description) ++ "}";
}

fn intString(comptime value: comptime_int) []const u8 {
    return std.fmt.comptimePrint("{d}", .{value});
}

/// Encodes a string as a JSON string literal at comptime.
fn jsonString(comptime s: []const u8) []const u8 {
    var result: []const u8 = "\"";
    for (s) |c| {
        result = result ++ switch (c) {
            '"' => "\\\"",
            '\\' => "\\\\",
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            0x00...0x08, 0x0B, 0x0C, 0x0E...0x1F => std.fmt.comptimePrint("\\u{x:0>4}", .{c}),
            else => &[1]u8{c},
        };
    }
    return result ++ "\"";
}

test jsonString {
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\nd\\u0001\"", comptime jsonString("a\"b\\c\nd\x01"));
}

test generate {
    try std.testing.expectEqualStrings("{\"type\":\"boolean\"}", generate(bool));
    try std.testing.expectEqualStrings("{\"type\":\"integer\",\"minimum\":-2147483648,\"maximum\":2147483647}", generate(i32));
    try std.testing.expectEqualStrings("{\"type\":\"integer\",\"minimum\":0,\"maximum\":255}", generate(u8));
    try std.testing.expectEqualStrings("{\"type\":\"integer\",\"minimum\":0}", generate(u64));
    try std.testing.expectEqualStrings("{\"type\":\"integer\"}", generate(i64));
    try std.testing.expectEqualStrings(
        "{\"anyOf\":[{\"type\":\"string\",\"enum\":[\"a\"]},{\"type\":\"integer\",\"minimum\":0,\"maximum\":255}]}",
        generate(enum(u8) { a, _ }),
    );
    try std.testing.expectEqualStrings("{\"type\":\"string\"}", generate([4]u8));
    try std.testing.expectEqualStrings("{\"type\":\"number\"}", generate(f64));
    try std.testing.expectEqualStrings("{\"type\":\"string\"}", generate([]const u8));
    try std.testing.expectEqualStrings("{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}", generate(?[]const u8));
    try std.testing.expectEqualStrings(
        "{\"type\":\"array\",\"items\":{\"anyOf\":[{\"type\":\"integer\",\"minimum\":0,\"maximum\":4294967295},{\"type\":\"null\"}]}}",
        generate([]const ?u32),
    );
    try std.testing.expectEqualStrings("{\"type\":\"array\",\"items\":{\"type\":\"string\"}}", generate([]const []const u8));
    try std.testing.expectEqualStrings("{\"type\":\"string\",\"enum\":[\"a\",\"b\"]}", generate(enum { a, b }));
    try std.testing.expectEqualStrings("{}", generate(std.json.Value));
}

test "generate - struct" {
    const Args = struct {
        query: []const u8,
        limit: u32 = 10,
        case_sensitive: ?bool = null,
        mode: enum { fast, exact },

        pub const descriptions = .{
            .query = "The \"text\" to search for",
            .limit = "Maximum number of results",
        };
    };

    try std.testing.expectEqualStrings(
        \\{"type":"object","properties":{"query":{"type":"string","description":"The \"text\" to search for"},"limit":{"type":"integer","minimum":0,"maximum":4294967295,"description":"Maximum number of results"},"case_sensitive":{"type":"boolean"},"mode":{"type":"string","enum":["fast","exact"]}},"required":["query","mode"]}
    , generateObject(Args));

    // the generated schema must be valid JSON
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, generateObject(Args), .{});
    defer parsed.deinit();
}

test "generate - nested and custom" {
    const Point = struct {
        x: f32,
        y: f32,
    };
    const Custom = struct {
        pub const json_schema = "{\"type\":\"string\",\"format\":\"uri\"}";
    };
    const Args = struct {
        points: []const Point,
        link: Custom,

        pub const descriptions = .{ .link = "A link" };
    };

    try std.testing.expectEqualStrings(
        \\{"type":"object","properties":{"points":{"type":"array","items":{"type":"object","properties":{"x":{"type":"number"},"y":{"type":"number"}},"required":["x","y"]}},"link":{"type":"string","format":"uri","description":"A link"}},"required":["points","link"]}
    , generateObject(Args));
}

test "generate - descriptions on boolean schemas" {
    const Anything = struct {
        pub const json_schema = "true";
    };
    const Args = struct {
        value: Anything,
        pub const descriptions = .{ .value = "Any value" };
    };
    try std.testing.expectEqualStrings(
        \\{"type":"object","properties":{"value":{"allOf":[true],"description":"Any value"}},"required":["value"]}
    , generateObject(Args));
}

test generateDiscriminated {
    const Custom = struct {
        pub const json_schema = "{\"type\":\"object\",\"properties\":{\"n\":{\"type\":\"number\"}}}";
    };
    const U = union(enum) {
        plain: struct { x: bool },
        custom: Custom,
    };
    try std.testing.expectEqualStrings(
        \\{"anyOf":[{"type":"object","properties":{"kind":{"const":"plain"},"x":{"type":"boolean"}},"required":["kind","x"]},{"allOf":[{"type":"object","properties":{"kind":{"const":"custom"}},"required":["kind"]},{"type":"object","properties":{"n":{"type":"number"}}}]}]}
    , generateDiscriminated(U, "kind"));
}

test generateAnyOfPayloads {
    const U = union(enum) { a: bool, b: []const u8 };
    try std.testing.expectEqualStrings("{\"anyOf\":[{\"type\":\"boolean\"},{\"type\":\"string\"}]}", generateAnyOfPayloads(U));
}

test generateObject {
    try std.testing.expectEqualStrings("{\"type\":\"object\"}", generateObject(void));
    try std.testing.expectEqualStrings("{\"type\":\"object\",\"properties\":{}}", generateObject(struct {}));
}
