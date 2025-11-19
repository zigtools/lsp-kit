//! Metamodel schema
//! specification taken from:
//! https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/metaModel/metaModel.ts

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

    pub fn hash(ty: MapKeyType) u64 {
        var hasher: std.hash.Wyhash = .init(0);
        ty.hashWithHasher(&hasher);
        return hasher.final();
    }

    pub fn hashWithHasher(ty: MapKeyType, hasher: anytype) void {
        std.hash.autoHash(hasher, std.meta.activeTag(ty));
        switch (ty) {
            .base => |base| std.hash.autoHash(hasher, base.name),
            .reference => |reference| hasher.update(reference.name),
        }
    }

    pub fn eql(a: MapKeyType, b: MapKeyType) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .base => a.base.name == b.base.name,
            .reference => std.mem.eql(u8, a.reference.name, b.reference.name),
        };
    }

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

    pub fn jsonStringify(ty: MapKeyType, jw: *std.json.Stringify) std.json.Stringify.Error!void {
        switch (ty) {
            inline else => |value| try jw.write(value),
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

    pub fn hash(ty: Type) u64 {
        var hasher: std.hash.Wyhash = .init(0);
        ty.hashWithHasher(&hasher);
        return hasher.final();
    }

    pub fn hashWithHasher(ty: Type, hasher: anytype) void {
        std.hash.autoHash(hasher, @as(TypeKind, ty));
        switch (ty) {
            .base => |base| std.hash.autoHash(hasher, base.name),
            .reference => |reference| hasher.update(reference.name),
            .array => |array| array.element.hashWithHasher(hasher),
            .map => |map| {
                map.key.hashWithHasher(hasher);
                map.value.hashWithHasher(hasher);
            },
            inline .@"and", .@"or", .tuple => |payload| {
                for (payload.items) |item| {
                    item.hashWithHasher(hasher);
                }
                std.hash.autoHash(hasher, payload.items.len);
            },
            .literal => |literal| {
                for (literal.value.properties) |property| {
                    property.hashWithHasher(hasher);
                }
                std.hash.autoHash(hasher, literal.value.properties.len);
                // ignores documentation
                // ignores since
                // ignores proposed
                // ignores deprecated
            },
            .stringLiteral => |stringLiteral| hasher.update(stringLiteral.value),
            .integerLiteral => |integerLiteral| std.hash.autoHash(hasher, @as(u64, @bitCast(integerLiteral.value))), // good enough :)
            .booleanLiteral => |booleanLiteral| std.hash.autoHash(hasher, booleanLiteral.value),
        }
    }

    pub fn eql(a: Type, b: Type) bool {
        if (@as(TypeKind, a) != @as(TypeKind, b)) return false;
        return switch (a) {
            .base => a.base.name == b.base.name,
            .reference => std.mem.eql(u8, a.reference.name, b.reference.name),
            .array => std.mem.eql(u8, a.array.kind, b.array.kind) and eql(a.array.element.*, b.array.element.*),
            .map => a.map.key.eql(b.map.key) and eql(a.map.value.*, b.map.value.*),
            inline .@"and", .@"or", .tuple => |_, tag| {
                const a_items = @field(a, @tagName(tag)).items;
                const b_items = @field(b, @tagName(tag)).items;
                if (a_items.len != b_items.len) return false;
                for (a_items, b_items) |a_item, b_item| {
                    if (!eql(a_item, b_item)) return false;
                }
                return true;
            },
            .literal => {
                const a_properties = a.literal.value.properties;
                const b_properties = b.literal.value.properties;
                if (a_properties.len != b_properties.len) return false;
                for (a_properties, b_properties) |a_property, b_property| {
                    if (!a_property.eql(b_property)) return false;
                }
                // ignores documentation
                // ignores since
                // ignores proposed
                // ignores deprecated
                return true;
            },
            .stringLiteral => std.mem.eql(u8, a.stringLiteral.value, b.stringLiteral.value),
            .integerLiteral => a.integerLiteral.value == b.integerLiteral.value,
            .booleanLiteral => a.booleanLiteral.value == b.booleanLiteral.value,
        };
    }

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

    pub fn jsonStringify(ty: Type, jw: *std.json.Stringify) std.json.Stringify.Error!void {
        switch (ty) {
            inline else => |value| try jw.write(value),
        }
    }
};

/// Represents a LSP request
pub const Request = struct {
    /// The request's method name.
    method: []const u8,
    /// The type name of the notifications if any.
    typeName: ?[]const u8 = null,
    /// The client capability property path if any.
    clientCapability: ?[]const u8 = null,
    /// The server capability property path if any.
    serverCapability: ?[]const u8 = null,
    /// The parameter type(s) if any.
    params: ?Params = null,
    /// The result type.
    result: Type,
    /// Optional partial result type if the supports partial result reporting.
    partialResult: ?Type = null,
    /// An optional error data type.
    errorData: ?Type = null,
    /// Optional a dynamic registration method if it
    /// different from the request's method.
    registrationMethod: ?[]const u8 = null,
    /// Optional registration options if the request
    /// supports dynamic registration.
    registrationOptions: ?Type = null,
    /// The direction in which this request is sent in the protocol.
    messageDirection: MessageDirection,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this request is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed feature. If omitted, the feature is final.
    proposed: ?bool = null,
    /// Whether the request is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

/// Represents a LSP notification
pub const Notification = struct {
    /// The notification's method name.
    method: []const u8,
    /// The type name of the notifications if any.
    typeName: ?[]const u8 = null,
    /// The client capability property path if any.
    clientCapability: ?[]const u8 = null,
    /// The server capability property path if any.
    serverCapability: ?[]const u8 = null,
    /// The parameter type(s) if any.
    params: ?Params = null,
    /// Optional a dynamic registration method if it
    /// different from the notification's method.
    registrationMethod: ?[]const u8 = null,
    /// Optional registration options if the notification
    /// supports dynamic registration.
    registrationOptions: ?Type = null,
    /// The direction in which this notification is sent in the protocol.
    messageDirection: MessageDirection,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this notification is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed feature. If omitted, the feature is final.
    proposed: ?bool = null,
    /// Whether the notification is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

pub const Params = union(enum) {
    one: Type,
    multiple: []Type,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!@This() {
        switch (try source.peekNextTokenType()) {
            .object_begin => return .{ .one = try std.json.innerParse(Type, allocator, source, options) },
            .array_begin => return .{ .multiple = try std.json.innerParse([]Type, allocator, source, options) },
            else => return error.UnexpectedToken,
        }
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!@This() {
        switch (source) {
            .object => return .{ .one = try std.json.parseFromValueLeaky(Type, allocator, source, options) },
            .array => return .{ .multiple = try std.json.parseFromValueLeaky([]Type, allocator, source, options) },
            else => return error.UnexpectedToken,
        }
    }

    pub fn jsonStringify(params: Params, jw: *std.json.Stringify) std.json.Stringify.Error!void {
        switch (params) {
            inline else => |value| try jw.write(value),
        }
    }
};

/// Represents an object property.
pub const Property = struct {
    /// The property name
    name: []const u8,
    /// The type of the property
    type: Type,
    /// Whether the property is optional. If omitted, the property is mandatory.
    optional: ?bool = null,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this property is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed property. If omitted, the structure is final.
    proposed: ?bool = null,
    /// Whether the property is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,

    pub fn hash(property: Property) u64 {
        var hasher: std.hash.Wyhash = .init(0);
        property.hashWithHasher(&hasher);
        return hasher.final();
    }

    pub fn hashWithHasher(property: Property, hasher: anytype) void {
        hasher.update(property.name);
        property.type.hashWithHasher(hasher);
        std.hash.autoHash(hasher, property.optional);
        // ignores documentation
        // ignores since
        // ignores proposed
        // ignores deprecated
    }

    pub fn eql(a: Property, b: Property) bool {
        if (!std.mem.eql(u8, a.name, b.name)) return false;
        if (!a.type.eql(b.type)) return false;
        if (a.optional != b.optional) return false;
        // ignores documentation
        // ignores since
        // ignores proposed
        // ignores deprecated
        return true;
    }
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
    /// Since when (release number) this structure is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed structure. If omitted, the structure is final.
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
    /// Since when (release number) this structure is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed structure. If omitted, the structure is final.
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
    /// Since when (release number) this type alias is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed type alias. If omitted, the type alias is final.
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
    /// Since when (release number) this enumeration entry is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed enumeration entry. If omitted, the enumeration entry is final.
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

        pub fn jsonStringify(value: Value, jw: *std.json.Stringify) std.json.Stringify.Error!void {
            switch (value) {
                .number => |f| try jw.write(f),
                .string => |s| try jw.write(s),
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
    /// part of the set defined in `values`). If omitted, no custom values are supported.
    supportsCustomValues: ?bool = null,
    /// An optional documentation.
    documentation: ?[]const u8 = null,
    /// Since when (release number) this enumeration entry is available. Is null if not known.
    since: ?[]const u8 = null,
    /// All since tags in case there was more than one tag. Is null if not known.
    sinceTags: ?[]const []const u8 = null,
    /// Whether this is a proposed enumeration. If omitted, the enumeration is final.
    proposed: ?bool = null,
    /// Whether the enumeration is deprecated or not. If deprecated
    /// the property contains the deprecation message.
    deprecated: ?[]const u8 = null,
};

pub const MetaData = struct {
    /// The protocol version.
    version: []const u8,
};
