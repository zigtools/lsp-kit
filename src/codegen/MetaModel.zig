//! Metamodel schema
//! specification taken from:
//! https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/metaModel/metaModel.ts

const std = @import("std");

/// Additional meta data.
metaData: MetaData,
/// The requests.
requests: []Request,
/// The notifications.
notifications: []Notification,
/// The structures.
structures: []Structure,
/// The enumerations.
enumerations: []Enumeration,
/// The type aliases.
typeAliases: []TypeAlias,

pub const BaseTypes = enum {
    URI,
    DocumentUri,
    integer,
    uinteger,
    decimal,
    RegExp,
    string,
    boolean,
    null,
};

pub const TypeKind = enum {
    base,
    reference,
    array,
    map,
    @"and",
    @"or",
    tuple,
    literal,
    stringLiteral,
    integerLiteral,
    booleanLiteral,
};

/// Indicates in which direction a message is sent in the protocol.
pub const MessageDirection = enum {
    clientToServer,
    serverToClient,
    both,
};

/// Represents a base type like `string` or `DocumentUri`.
pub const BaseType = struct {
    kind: []const u8 = "base",
    name: BaseTypes,
};

/// Represents a reference to another type (e.g. `TextDocument`).
/// This is either a `Structure`, a `Enumeration` or a `TypeAlias`
/// in the same meta model.
pub const ReferenceType = struct {
    kind: []const u8 = "reference",
    name: []const u8,
};

/// Represents an array type (e.g. `TextDocument[]`).
pub const ArrayType = struct {
    kind: []const u8 = "array",
    element: *Type,
};

/// Represents a type that can be used as a key in a
/// map type. If a reference type is used then the
/// type must either resolve to a `string` or `integer`
/// type. (e.g. `type ChangeAnnotationIdentifier === string`).
pub const MapKeyType = union(enum) {
    base: struct {
        kind: []const u8 = "base",
        name: enum {
            Uri,
            DocumentUri,
            string,
            integer,
        },
    },
    reference: ReferenceType,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!@This() {
        const result = try std.json.innerParse(struct {
            kind: []const u8,
            name: []const u8,
        }, allocator, source, options);
        const NameEnum = @FieldType(@FieldType(@This(), "base"), "name");
        if (std.mem.eql(u8, result.kind, "base")) {
            return .{ .reference = .{ .kind = result.kind, .name = std.meta.stringToEnum(NameEnum, result.name) orelse return error.InvalidEnumTag } };
        } else if (std.mem.eql(u8, result.kind, "reference")) {
            return .{ .reference = .{ .kind = result.kind, .name = result.name } };
        }
        return error.UnexpectedToken;
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!@This() {
        if (source != .object) return error.UnexpectedToken;

        const kind = source.object.get("kind") orelse return error.MissingField;
        if (kind != .string) return error.UnexpectedToken;

        if (std.mem.eql(u8, kind.string, "base")) {
            return .{ .base = try std.json.parseFromValueLeaky(@FieldType(@This(), "base"), allocator, source, options) };
        } else if (std.mem.eql(u8, kind.string, "reference")) {
            return .{ .reference = try std.json.parseFromValueLeaky(ReferenceType, allocator, source, options) };
        }
        return error.UnexpectedToken;
    }

    pub fn jsonStringify(self: @This(), options: std.json.StringifyOptions, out_stream: anytype) @TypeOf(out_stream.*).Error!void {
        switch (self) {
            inline else => |value| try std.json.stringify(value, options, out_stream),
        }
    }
};

/// Represents a JSON object map
/// (e.g. `interface Map<K extends string | integer, V> { [key: K] => V; }`).
pub const MapType = struct {
    kind: []const u8 = "map",
    key: MapKeyType,
    value: *Type,
};

/// Represents an `and`type
/// (e.g. TextDocumentParams & WorkDoneProgressParams`).
pub const AndType = struct {
    kind: []const u8 = "and",
    items: []Type,
};

/// Represents an `or` type
/// (e.g. `Location | LocationLink`).
pub const OrType = struct {
    kind: []const u8 = "or",
    items: []Type,
};

/// Represents a `tuple` type
/// (e.g. `[integer, integer]`).
pub const TupleType = struct {
    kind: []const u8 = "tuple",
    items: []Type,
};

/// Represents a literal structure
/// (e.g. `property: { start: uinteger; end: uinteger; }`).
pub const StructureLiteralType = struct {
    kind: []const u8 = "literal",
    value: StructureLiteral,
};

/// Represents a string literal type
/// (e.g. `kind: 'rename'`).
pub const StringLiteralType = struct {
    kind: []const u8 = "stringLiteral",
    value: []const u8,
};

/// Represents an integer literal type
/// (e.g. `kind: 1`).
pub const IntegerLiteralType = struct {
    kind: []const u8 = "integerLiteral",
    value: f64,
};

/// Represents a boolean literal type
/// (e.g. `kind: true`).
pub const BooleanLiteralType = struct {
    kind: []const u8 = "booleanLiteral",
    value: bool,
};

pub const Type = union(TypeKind) {
    base: BaseType,
    reference: ReferenceType,
    array: ArrayType,
    map: MapType,
    @"and": AndType,
    @"or": OrType,
    tuple: TupleType,
    literal: StructureLiteralType,
    stringLiteral: StringLiteralType,
    integerLiteral: IntegerLiteralType,
    booleanLiteral: BooleanLiteralType,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!@This() {
        const json_value = try std.json.innerParse(std.json.Value, allocator, source, options);
        return try jsonParseFromValue(allocator, json_value, options);
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!Type {
        if (source != .object) return error.UnexpectedToken;
        const kind = source.object.get("kind") orelse return error.MissingField;
        if (kind != .string) return error.UnexpectedToken;

        inline for (std.meta.fields(Type)) |field| {
            if (std.mem.eql(u8, kind.string, field.name)) {
                return @unionInit(Type, field.name, try std.json.parseFromValueLeaky(field.type, allocator, source, options));
            }
        }
        return error.UnexpectedToken;
    }

    pub fn jsonStringify(self: @This(), options: std.json.StringifyOptions, out_stream: anytype) @TypeOf(out_stream.*).Error!void {
        switch (self) {
            inline else => |value| try std.json.stringify(value, options, out_stream),
        }
    }
};

/// Represents a LSP request
pub const Request = struct {
    /// The request's method name.
    method: []const u8,
    /// The parameter type(s) if any.
    params: ?Params = null,
    /// The result type.
    result: Type,
    /// Optional partial result type if the request
    /// supports partial result reporting.
    partialResult: ?Type = null,
    /// An optional error data type.
    errorData: ?Type = null,
    /// Optional a dynamic registration method if it
    /// different from the request's method.
    registrationMethod: ?[]const u8 = null,
    /// Optional registration options if the request
    /// supports dynamic registration.
    registrationOptions: ?Type = null,
    /// The direction in which this request is sent
    /// in the protocol.
    messageDirection: MessageDirection,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this request is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed feature. If omitted
    /// the feature is final.
    proposed: ?bool = null,
    /// Whether the request is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Represents a LSP notification
pub const Notification = struct {
    /// The notification's method name.
    method: []const u8,
    /// The parameter type(s) if any.
    params: ?Params = null,
    /// Optional a dynamic registration method if it
    /// different from the notification's method.
    registrationMethod: ?[]const u8 = null,
    /// Optional registration options if the notification
    /// supports dynamic registration.
    registrationOptions: ?Type = null,
    /// The direction in which this notification is sent
    /// in the protocol.
    messageDirection: MessageDirection,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this notification is
    /// available. Is null if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed feature. If omitted
    /// the feature is final.
    proposed: ?bool = null,
    /// Whether the notification is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

pub const Params = union(enum) {
    Type: Type,
    array_of_Type: []Type,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!@This() {
        switch (try source.peekNextTokenType()) {
            .object_begin => return .{ .Type = try std.json.innerParse(Type, allocator, source, options) },
            .array_begin => return .{ .array_of_Type = try std.json.innerParse([]Type, allocator, source, options) },
            else => return error.UnexpectedToken,
        }
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!@This() {
        switch (source) {
            .object => return .{ .Type = try std.json.parseFromValueLeaky(Type, allocator, source, options) },
            .array => return .{ .array_of_Type = try std.json.parseFromValueLeaky([]Type, allocator, source, options) },
            else => return error.UnexpectedToken,
        }
    }

    pub fn jsonStringify(self: @This(), options: std.json.StringifyOptions, out_stream: anytype) @TypeOf(out_stream.*).Error!void {
        switch (self) {
            inline else => |value| try std.json.stringify(value, options, out_stream),
        }
    }
};

/// Represents an object property.
pub const Property = struct {
    /// The property name
    name: []const u8,
    /// The type of the property
    type: Type,
    /// Whether the property is optional. If
    /// omitted, the property is mandatory.
    optional: ?bool = null,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this property is
    /// available. Is null if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed property. If omitted
    /// the structure is final.
    proposed: ?bool = null,
    /// Whether the property is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Defines the structure of an object literal.
pub const Structure = struct {
    /// The name of the structure.
    name: []const u8,
    /// Structures extended from. This structures form
    /// a polymorphic type hierarchy.
    extends: ?[]Type = null,
    /// Structures to mix in. The properties of these
    /// structures are `copied` into this structure.
    /// Mixins don't form a polymorphic type hierarchy in
    /// LSP.
    mixins: ?[]Type = null,
    /// The properties.
    properties: []Property,
    /// An optional documentation;
    documentation: ?[]const u8 = null,
    /// Since when (release number) this structure is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed structure. If omitted,
    /// the structure is final.
    proposed: ?bool = null,
    /// Whether the structure is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Defines an unnamed structure of an object literal.
pub const StructureLiteral = struct {
    /// The properties.
    properties: []Property,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this structure is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed structure. If omitted,
    /// the structure is final.
    proposed: ?bool = null,
    /// Whether the structure is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Defines a type alias.
/// (e.g. `type Definition = Location | LocationLink`)
pub const TypeAlias = struct {
    /// The name of the type alias.
    name: []const u8,
    /// The aliased type.
    type: Type,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this type alias is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed type alias. If omitted,
    /// the type alias is final.
    proposed: ?bool = null,
    /// Whether the type alias is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Defines an enumeration entry.
pub const EnumerationEntry = struct {
    /// The name of the enum item.
    name: []const u8,
    /// The value.
    value: Value,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this enumeration entry is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed enumeration entry. If omitted,
    /// the enumeration entry is final.
    proposed: ?bool = null,
    /// Whether the enumeration entry is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,

    pub const Value = union(enum) {
        number: f64,
        string: []const u8,

        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!@This() {
            switch (try source.peekNextTokenType()) {
                .string => return .{ .string = try std.json.innerParse([]const u8, allocator, source, options) },
                .number => return .{ .number = try std.json.innerParse(f64, allocator, source, options) },
                else => return error.UnexpectedToken,
            }
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!@This() {
            switch (source) {
                .string => |s| return .{ .string = s },
                .float, .integer => return .{ .number = try std.json.parseFromValueLeaky(f64, allocator, source, options) },
                else => return error.UnexpectedToken,
            }
        }

        pub fn jsonStringify(self: @This(), options: std.json.StringifyOptions, out_stream: anytype) @TypeOf(out_stream.*).Error!void {
            switch (self) {
                .number => |f| try std.json.stringify(f, options, out_stream),
                .string => |s| try std.json.stringify(s, options, out_stream),
            }
        }
    };
};

pub const EnumerationType = struct {
    kind: []const u8 = "base",
    name: enum {
        string,
        integer,
        uinteger,
    },
};

/// Defines an enumeration.
pub const Enumeration = struct {
    /// The name of the enumeration.
    name: []const u8,
    /// The type of the elements.
    values: []EnumerationEntry,
    /// The enum values.
    type: EnumerationType,
    /// Whether the enumeration supports custom values (e.g. values which are not
    /// part of the set defined in `values`). If omitted no custom values are
    /// supported.
    supportsCustomValues: ?bool = null,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this enumeration entry is
    /// available. Is undefined if not known.
    since: ?[]const u8 = null,
    /// Whether this is a proposed enumeration. If omitted,
    /// the enumeration is final.
    proposed: ?bool = null,
    /// Whether the enumeration is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

pub const MetaData = struct {
    /// The protocol version.
    version: []const u8,
};
