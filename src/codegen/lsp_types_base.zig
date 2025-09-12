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
    try std.testing.expectEqual(MessageDirection.server_to_client, getRequestMetadata("workspace/configuration").?.direction);
    try std.testing.expectEqual(MessageDirection.client_to_server, getNotificationMetadata("textDocument/didOpen").?.direction);
    try std.testing.expectEqual(MessageDirection.both, getNotificationMetadata("$/cancelRequest").?.direction);
}

/// Returns comptime-known metadata about them a Request.
pub fn getRequestMetadata(comptime method: []const u8) ?RequestMetadata {
    @setEvalBranchQuota(10_000);
    for (request_metadata) |meta| {
        if (std.mem.eql(u8, method, meta.method)) {
            return meta;
        }
    }
    return null;
}

/// Returns comptime-known metadata about them a Notification.
pub fn getNotificationMetadata(comptime method: []const u8) ?NotificationMetadata {
    @setEvalBranchQuota(10_000);
    for (notification_metadata) |meta| {
        if (std.mem.eql(u8, method, meta.method)) {
            return meta;
        }
    }
    return null;
}

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

/// A list of Request with comptime-known metadata about them.
pub const request_metadata: []const RequestMetadata = &types.request_metadata_generated;

/// A list of Notification with comptime-known metadata about them.
pub const notification_metadata: []const NotificationMetadata = &types.notification_metadata_generated;

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
    for (types.notification_metadata) |metadata| {
        if (metadata.Params) |Params| {
            testType(Params);
        }
    }
    for (types.request_metadata) |metadata| {
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
