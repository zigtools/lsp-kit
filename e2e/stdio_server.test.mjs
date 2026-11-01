// End-to-end tests for `examples/stdio_server.zig` using the official MCP
// TypeScript SDK as the reference client.
//
// Usage: node stdio_server.test.mjs <path to stdio-server executable>
//
// Normally invoked through `zig build e2e` or `zig build ci`.

import { test, before, after } from "node:test";
import assert from "node:assert/strict";

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import {
  ErrorCode,
  LATEST_PROTOCOL_VERSION,
  LoggingMessageNotificationSchema,
  McpError,
} from "@modelcontextprotocol/sdk/types.js";

const serverPath = process.argv[2];
if (!serverPath) {
  console.error("usage: node stdio_server.test.mjs <path to stdio-server>");
  process.exit(2);
}

/** @type {Client} */
let client;
/** @type {StdioClientTransport} */
let transport;
const logMessages = [];
let negotiatedProtocolVersion;

before(async () => {
  transport = new StdioClientTransport({ command: serverPath, stderr: "inherit" });
  // `Client` reports the negotiated version to transports that support it.
  transport.setProtocolVersion = (version) => {
    negotiatedProtocolVersion = version;
  };
  client = new Client({ name: "blitzmcp-e2e", version: "0.0.0" });
  client.setNotificationHandler(LoggingMessageNotificationSchema, (notification) => {
    logMessages.push(notification.params);
  });
  await client.connect(transport);
});

after(async () => {
  await client?.close();
});

test("initialize", () => {
  assert.deepEqual(client.getServerVersion(), {
    name: "blitz-example",
    title: "Blitz MCP Example",
    version: "0.1.0",
  });
  assert.deepEqual(client.getServerCapabilities(), {
    logging: {},
    prompts: {},
    resources: {},
    tools: {},
  });
  assert.match(client.getInstructions() ?? "", /demo server/);
});

test("protocol version is negotiated to the client's latest version", () => {
  assert.equal(negotiatedProtocolVersion, LATEST_PROTOCOL_VERSION);
});

test("ping", async () => {
  assert.deepEqual(await client.ping(), {});
});

test("tools/list", async () => {
  const { tools } = await client.listTools();
  assert.deepEqual(
    tools.map((tool) => tool.name),
    ["increment", "count_words"],
  );

  const [increment, countWords] = tools;
  assert.deepEqual(increment.inputSchema, {
    type: "object",
    properties: {
      by: { type: "integer", description: "The amount to add to the counter." },
    },
  });
  assert.deepEqual(increment.outputSchema, {
    type: "object",
    properties: { value: { type: "integer" } },
    required: ["value"],
  });

  assert.equal(countWords.title, "Count Words");
  assert.deepEqual(countWords.inputSchema.required, ["text"]);
  assert.deepEqual(countWords.annotations, { readOnlyHint: true, openWorldHint: false });
});

test("tools/call with structured output", async () => {
  // The SDK validates `structuredContent` against the advertised `outputSchema`.
  const first = await client.callTool({ name: "increment", arguments: { by: 5 } });
  assert.deepEqual(first.structuredContent, { value: 5 });
  assert.deepEqual(first.content, [{ type: "text", text: '{"value":5}' }]);

  const second = await client.callTool({ name: "increment", arguments: {} });
  assert.deepEqual(second.structuredContent, { value: 6 });
});

test("tools/call with logging and progress", async () => {
  await client.setLoggingLevel("debug");

  const progress = [];
  const result = await client.callTool(
    { name: "count_words", arguments: { text: "one two  three\nfour" } },
    undefined,
    { onprogress: (p) => progress.push(p) },
  );

  assert.equal(result.isError, undefined);
  assert.deepEqual(result.content, [{ type: "text", text: "4" }]);
  assert.deepEqual(progress, [{ progress: 1, total: 1, message: "done" }]);
  assert.deepEqual(logMessages.at(-1), { level: "info", data: "counted 4 words" });
});

test("logging/setLevel filters log messages", async () => {
  await client.setLoggingLevel("error");
  const before = logMessages.length;
  await client.callTool({ name: "count_words", arguments: { text: "a b" } });
  assert.equal(logMessages.length, before);
  await client.setLoggingLevel("debug");
});

test("tools/call reports tool errors as results", async () => {
  const failed = await client.callTool({ name: "count_words", arguments: { text: "" } });
  assert.equal(failed.isError, true);
  assert.deepEqual(failed.content, [{ type: "text", text: "The text must not be empty" }]);

  const invalid = await client.callTool({ name: "count_words", arguments: { txt: "typo" } });
  assert.equal(invalid.isError, true);
  assert.match(invalid.content[0].text, /Invalid arguments for tool 'count_words'/);
});

test("tools/call with an unknown tool is a protocol error", async () => {
  await assert.rejects(client.callTool({ name: "does_not_exist", arguments: {} }), (err) => {
    assert.ok(err instanceof McpError);
    assert.equal(err.code, ErrorCode.InvalidParams);
    assert.match(err.message, /Unknown tool: does_not_exist/);
    return true;
  });
});

test("resources", async () => {
  const { resources } = await client.listResources();
  assert.deepEqual(resources, [{ uri: "counter://value", name: "counter", mimeType: "text/plain" }]);

  const { contents } = await client.readResource({ uri: "counter://value" });
  assert.equal(contents.length, 1);
  assert.equal(contents[0].uri, "counter://value");
  assert.match(contents[0].text, /^\d+$/);

  await assert.rejects(client.readResource({ uri: "counter://missing" }), (err) => {
    assert.ok(err instanceof McpError);
    assert.equal(err.code, -32002);
    return true;
  });
});

test("prompts", async () => {
  const { prompts } = await client.listPrompts();
  assert.deepEqual(prompts, [
    {
      name: "greet",
      description: "Writes a greeting.",
      arguments: [
        { name: "name", description: "Who to greet.", required: true },
        { name: "style", description: "The style of the greeting, e.g. 'formal'.", required: false },
      ],
    },
  ]);

  const result = await client.getPrompt({ name: "greet", arguments: { name: "Zig", style: "formal" } });
  assert.deepEqual(result.messages, [
    { role: "user", content: { type: "text", text: "Write a formal greeting for Zig." } },
  ]);

  await assert.rejects(client.getPrompt({ name: "greet", arguments: {} }), (err) => {
    assert.ok(err instanceof McpError);
    assert.equal(err.code, ErrorCode.InvalidParams);
    return true;
  });
});

test("unsupported methods", async () => {
  await assert.rejects(
    client.complete({ ref: { type: "ref/prompt", name: "greet" }, argument: { name: "name", value: "Z" } }),
    (err) => {
      assert.ok(err instanceof McpError);
      assert.equal(err.code, ErrorCode.MethodNotFound);
      return true;
    },
  );
});
