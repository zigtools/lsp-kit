//! Building blocks for implementing Model Context Protocol servers in Zig.
//!
//! - `json_rpc`: JSON-RPC 2.0 message envelope types.
//! - `Transport`: A type-erased message transport with a newline-delimited
//!   JSON stdio implementation.
//! - `types`: MCP protocol types.
//! - `schema`: Comptime JSON Schema generation from Zig types.
//! - `Server`: A server loop with comptime dispatch to protocol methods,
//!   tools and prompts.
//!
//! See https://modelcontextprotocol.io/specification/2025-11-25

const std = @import("std");

pub const json_rpc = @import("json_rpc.zig");
pub const Transport = @import("Transport.zig");
pub const types = @import("types.zig");
pub const schema = @import("schema.zig");

const server = @import("server.zig");
pub const Server = server.Server;
pub const Context = server.Context;
pub const Error = server.Error;

pub const JsonRPCMessage = json_rpc.JsonRPCMessage;
pub const ID = json_rpc.ID;

test {
    std.testing.refAllDecls(@This());
    _ = json_rpc;
    _ = Transport;
    _ = types;
    _ = schema;
    _ = server;
}
