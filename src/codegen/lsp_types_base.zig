//! Type definitions of the Language Server Protocol.

const std = @import("std");

const types = @This();
const parser = @import("parser");

/// A normal non document URI.
///
/// The URI’s format is defined in https://tools.ietf.org/html/rfc3986
pub const URI = []const u8;

/// The URI of a document.
///
/// The URI’s format is defined in https://tools.ietf.org/html/rfc3986
pub const DocumentUri = []const u8;

/// A JavaScript regular expression; never used
pub const RegExp = []const u8;

pub const LSPAny = std.json.Value;
pub const LSPArray = []LSPAny;
pub const LSPObject = std.json.ArrayHashMap(std.json.Value);

/// Indicates in which direction a message is sent in the protocol.
pub const MessageDirection = enum {
    client_to_server,
    server_to_client,
    both,
};

test MessageDirection {
    try std.testing.expectEqual(MessageDirection.server_to_client, requests.get("workspace/configuration").?.direction);
    try std.testing.expectEqual(MessageDirection.client_to_server, notifications.get("textDocument/didOpen").?.direction);
    try std.testing.expectEqual(MessageDirection.both, notifications.get("$/cancelRequest").?.direction);
}

pub const getRequestMetadata = @compileError("Removed; Use `requests.get(method)` instead.");
pub const getNotificationMetadata = @compileError("Removed; Use `notifications.get(method)` instead.");

pub const RegistrationMetadata = struct {
    /// A dynamic registration method if it different from the request's method.
    method: ?[]const u8,
    /// registration options if the request supports dynamic registration.
    Options: ?type,
};

/// Represents a LSP notification
pub const NotificationMetadata = struct {
    /// The notification's method name.
    method: []const u8,
    documentation: ?[]const u8,
    /// The direction in which this notification is sent in the protocol.
    direction: MessageDirection,
    /// The parameter type if any.
    Params: ?type,
    registration: RegistrationMetadata,
};

/// Represents a LSP request
pub const RequestMetadata = struct {
    /// The request's method name.
    method: []const u8,
    documentation: ?[]const u8,
    /// The direction in which this request is sent in the protocol.
    direction: MessageDirection,
    /// The parameter type if any.
    Params: ?type,
    /// The result type.
    Result: type,
    /// Partial result type if the request supports partial result reporting.
    PartialResult: ?type,
    /// An optional error data type.
    ErrorData: ?type,
    registration: RegistrationMetadata,
};

pub const request_metadata = @compileError("Removed; Use `requests.values()` instead.");
pub const notification_metadata = @compileError("Removed; Use `notifications.values()` instead.");

/// A set of Request with comptime-known metadata about them.
pub const requests: std.StaticStringMap(RequestMetadata) = types.requests_generated;

/// A set of Notification with comptime-known metadata about them.
pub const notifications: std.StaticStringMap(NotificationMetadata) = types.notifications_generated;

fn testType(comptime T: type) void {
    if (T == void) return;
    if (T == ?void) return;

    const S = struct {
        fn parseFromValue() void {
            _ = std.json.parseFromValue(T, undefined, undefined, undefined) catch unreachable;
        }
        fn innerParse() void {
            var source: std.json.Scanner = undefined;
            _ = std.json.innerParse(T, undefined, &source, undefined) catch unreachable;
        }
        fn stringify() void {
            const value: T = undefined;
            _ = std.json.stringify(value, undefined, std.io.null_writer) catch unreachable;
        }
    };
    _ = &S.parseFromValue;
    _ = &S.innerParse;
    _ = &S.stringify;
}

test {
    for (types.notifications.values()) |metadata| {
        if (metadata.Params) |Params| {
            testType(Params);
        }
    }
    for (types.requests.values()) |metadata| {
        if (metadata.Params) |Params| {
            testType(Params);
        }
        testType(metadata.Result);
        if (metadata.PartialResult) |PartialResult| {
            testType(PartialResult);
        }
        if (metadata.ErrorData) |ErrorData| {
            testType(ErrorData);
        }
    }
}
