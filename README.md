[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# blitzmcp

Building blocks for [Model Context Protocol](https://modelcontextprotocol.io/specification/2025-11-25) servers in Zig.

Derived from [zigtools/lsp-kit](https://github.com/zigtools/lsp-kit). The JSON-RPC envelope, the `Transport` abstraction and the comptime dispatch pattern were kept; everything specific to the Language Server Protocol was removed.

| Module                         | Purpose                                                                                      |
| ------------------------------ | -------------------------------------------------------------------------------------------- |
| `json_rpc`                     | JSON-RPC 2.0 messages (`JsonRPCMessage`, `ID`, typed request/notification/response writers) |
| `Transport`                    | Type-erased message transport. `Transport.Stdio` uses newline-delimited JSON, `Transport.Stream` wraps any `std.Io.Reader`/`std.Io.Writer` |
| `types`                        | Hand-written MCP protocol types (lifecycle, tools, resources, prompts, logging, sampling, elicitation, …) and method tables |
| `schema`                       | Comptime JSON Schema generation from Zig types                                               |
| `Server` / `Context`           | Server loop with comptime two-level dispatch                                                 |

# Installation

> [!NOTE]
> Requires Zig `0.17.0` or later.

```bash
zig fetch --save git+https://github.com/StaffinityAI/blitzmcp.git
```

```zig
const blitzmcp = b.dependency("blitzmcp", .{}).module("blitzmcp");
exe.root_module.addImport("blitzmcp", blitzmcp);
```

# Usage

```zig
const std = @import("std");
const mcp = @import("blitzmcp");

const Server = mcp.Server(Handler, .{
    .name = "my-zig-mcp",
    .version = "0.1.0",
    .tools = .{
        .{
            .name = "search_files",
            .description = "Search files in workspace",
            .annotations = .{ .readOnlyHint = true },
            .call = Handler.searchFiles,
        },
    },
});

const Handler = struct {
    const SearchArgs = struct {
        query: []const u8,
        limit: u32 = 20,

        pub const descriptions = .{ .query = "The text to search for" };
    };

    // The input schema is generated from `SearchArgs` at comptime.
    fn searchFiles(handler: *Handler, ctx: *mcp.Context, args: SearchArgs) ![]const u8 {
        try ctx.sendProgress(0, null, "searching");
        ...
    }

    // Protocol level methods are implemented by declaring a function with the method name.
    pub fn @"resources/read"(handler: *Handler, arena: std.mem.Allocator, params: mcp.types.ReadResourceRequestParams) !mcp.types.ReadResourceResult {
        ...
    }
};

pub fn main(init: std.process.Init) !void {
    var read_buffer: [4096]u8 = undefined;
    var stdio: mcp.Transport.Stdio = .init(&read_buffer, .stdin(), .stdout());

    var handler: Handler = .{};
    var server: Server = .init(&handler);
    try server.run(init.io, init.gpa, &stdio.transport);
}
```

- `initialize`, `ping`, `logging/setLevel`, `tools/list`, `tools/call`, `prompts/list` and `prompts/get` are handled by the server. Server capabilities are derived from the config and the methods the `Handler` declares.
- Handler functions request their arguments by type: the handler (`*Handler`), an arena (`std.mem.Allocator`), the request context (`*mcp.Context`) and the parsed params/arguments.
- Tool errors (including invalid arguments) are returned as a `CallToolResult` with `isError = true`. Errors from protocol level methods become JSON-RPC errors (`error.InvalidParams` → `-32602`, `error.ResourceNotFound` → `-32002`, …).
- Tools that return a struct produce `structuredContent` and an `outputSchema`.
- `Server.handleMessage` is independent of the framing so that it can be reused by other transports.

See [`examples/stdio_server.zig`](examples/stdio_server.zig) for a complete server. Try it with the MCP Inspector:

```bash
zig build
npx @modelcontextprotocol/inspector zig-out/bin/stdio-server
```
