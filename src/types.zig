//! Type definitions of the Model Context Protocol.
//!
//! Field names follow the specification (camelCase) so that the types can be
//! (de)serialized with `std.json` directly. Optional fields default to `null`
//! and should be serialized with `.emit_null_optional_fields = false`.
//!
//! See https://modelcontextprotocol.io/specification/2025-11-25/schema

const std = @import("std");
const json_rpc = @import("json_rpc.zig");

/// The protocol version a server built with this library prefers.
pub const latest_protocol_version = "2025-11-25";

/// Protocol versions that are accepted during version negotiation, newest first.
pub const supported_protocol_versions: []const []const u8 = &.{
    "2025-11-25",
    "2025-06-18",
    "2025-03-26",
    "2024-11-05",
};

pub fn isSupportedProtocolVersion(version: []const u8) bool {
    for (supported_protocol_versions) |supported| {
        if (std.mem.eql(u8, supported, version)) return true;
    }
    return false;
}

pub const RequestId = json_rpc.ID;
/// A progress token, used to associate progress notifications with the original request.
pub const ProgressToken = json_rpc.ID;
/// An opaque token used to represent a cursor for pagination.
pub const Cursor = []const u8;
/// Reserved by MCP to allow clients and servers to attach additional metadata.
pub const Meta = std.json.Value;

/// The sender or recipient of messages and data in a conversation.
pub const Role = enum { user, assistant };

/// Optional annotations for the client. The client can use annotations to
/// inform how objects are used or displayed.
pub const Annotations = struct {
    /// Describes who the intended audience of this object or data is.
    audience: ?[]const Role = null,
    /// Describes how important this data is for operating the server, from 0 (least) to 1 (most).
    priority: ?f64 = null,
    /// The moment the resource was last modified, as an ISO 8601 formatted string.
    lastModified: ?[]const u8 = null,
};

/// An optionally-sized icon that can be displayed in a user interface.
pub const Icon = struct {
    /// A standard URI pointing to an icon resource. May be an HTTP/HTTPS URL or a `data:` URI.
    src: []const u8,
    mimeType: ?[]const u8 = null,
    /// Sizes at which the icon can be used, e.g. `"48x48"` or `"any"`.
    sizes: ?[]const []const u8 = null,
    theme: ?enum { light, dark } = null,
};

/// Describes the MCP implementation of a client or server.
pub const Implementation = struct {
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    version: []const u8,
    description: ?[]const u8 = null,
    websiteUrl: ?[]const u8 = null,
    icons: ?[]const Icon = null,
};

/// A response that indicates success but carries no data.
pub const EmptyResult = struct {
    _meta: ?Meta = null,
};

pub const PaginatedRequestParams = struct {
    /// An opaque token representing the current pagination position.
    cursor: ?Cursor = null,
    _meta: ?Meta = null,
};

/// Serializes as an empty JSON object (`{}`). Used for capability flags.
pub const EmptyObject = struct {};

// ----------------------------------------------------------------------------
// Lifecycle
// ----------------------------------------------------------------------------

pub const InitializeRequestParams = struct {
    /// The latest version of the Model Context Protocol that the client supports.
    protocolVersion: []const u8,
    capabilities: ClientCapabilities,
    clientInfo: Implementation,
    _meta: ?Meta = null,
};

pub const InitializeResult = struct {
    /// The version of the Model Context Protocol that the server wants to use.
    protocolVersion: []const u8,
    capabilities: ServerCapabilities,
    serverInfo: Implementation,
    /// Instructions describing how to use the server and its features.
    instructions: ?[]const u8 = null,
    _meta: ?Meta = null,
};

/// Capabilities a client may support.
pub const ClientCapabilities = struct {
    /// Experimental, non-standard capabilities that the client supports.
    experimental: ?std.json.Value = null,
    /// Present if the client supports listing roots.
    roots: ?struct {
        /// Whether the client supports notifications for changes to the roots list.
        listChanged: ?bool = null,
    } = null,
    /// Present if the client supports sampling from an LLM.
    sampling: ?std.json.Value = null,
    /// Present if the client supports elicitation from the server.
    elicitation: ?std.json.Value = null,
    /// Present if the client supports task-augmented requests.
    tasks: ?std.json.Value = null,
};

/// Capabilities that a server may support.
pub const ServerCapabilities = struct {
    /// Experimental, non-standard capabilities that the server supports.
    experimental: ?std.json.Value = null,
    /// Present if the server supports sending log messages to the client.
    logging: ?EmptyObject = null,
    /// Present if the server supports argument autocompletion suggestions.
    completions: ?EmptyObject = null,
    /// Present if the server offers any prompt templates.
    prompts: ?struct {
        /// Whether this server supports notifications for changes to the prompt list.
        listChanged: ?bool = null,
    } = null,
    /// Present if the server offers any resources to read.
    resources: ?struct {
        /// Whether this server supports subscribing to resource updates.
        subscribe: ?bool = null,
        /// Whether this server supports notifications for changes to the resource list.
        listChanged: ?bool = null,
    } = null,
    /// Present if the server offers any tools to call.
    tools: ?struct {
        /// Whether this server supports notifications for changes to the tool list.
        listChanged: ?bool = null,
    } = null,
    /// Present if the server supports task-augmented requests.
    tasks: ?std.json.Value = null,
};

// ----------------------------------------------------------------------------
// Notifications
// ----------------------------------------------------------------------------

pub const CancelledNotificationParams = struct {
    /// The ID of the request to cancel.
    requestId: ?RequestId = null,
    /// An optional string describing the reason for the cancellation.
    reason: ?[]const u8 = null,
    _meta: ?Meta = null,
};

pub const ProgressNotificationParams = struct {
    /// The progress token which was given in the initial request.
    progressToken: ProgressToken,
    /// The progress thus far. This should increase every time progress is made.
    progress: f64,
    /// Total number of items to process (or total progress required), if known.
    total: ?f64 = null,
    /// An optional message describing the current progress.
    message: ?[]const u8 = null,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Content
// ----------------------------------------------------------------------------

/// Text provided to or from an LLM.
pub const TextContent = struct {
    text: []const u8,
    annotations: ?Annotations = null,
    _meta: ?Meta = null,
};

/// An image provided to or from an LLM.
pub const ImageContent = struct {
    /// The base64-encoded image data.
    data: []const u8,
    /// The MIME type of the image.
    mimeType: []const u8,
    annotations: ?Annotations = null,
    _meta: ?Meta = null,
};

/// Audio provided to or from an LLM.
pub const AudioContent = struct {
    /// The base64-encoded audio data.
    data: []const u8,
    /// The MIME type of the audio.
    mimeType: []const u8,
    annotations: ?Annotations = null,
    _meta: ?Meta = null,
};

/// A resource that the server is capable of reading, included in a prompt or tool call result.
pub const ResourceLink = Resource;

/// The contents of a resource, embedded into a prompt or tool call result.
pub const EmbeddedResource = struct {
    resource: ResourceContents,
    annotations: ?Annotations = null,
    _meta: ?Meta = null,
};

/// Discriminated by the `"type"` field.
pub const ContentBlock = union(enum) {
    text: TextContent,
    image: ImageContent,
    audio: AudioContent,
    resource_link: ResourceLink,
    resource: EmbeddedResource,

    pub const jsonParse = TypeDiscriminated(@This()).jsonParse;
    pub const jsonParseFromValue = TypeDiscriminated(@This()).jsonParseFromValue;
    pub const jsonStringify = TypeDiscriminated(@This()).jsonStringify;

    pub fn initText(text: []const u8) ContentBlock {
        return .{ .text = .{ .text = text } };
    }
};

/// Either `TextResourceContents` or `BlobResourceContents`. Distinguished by
/// the presence of the `"text"` or `"blob"` field.
pub const ResourceContents = union(enum) {
    text: TextResourceContents,
    blob: BlobResourceContents,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!ResourceContents {
        const value = try std.json.Value.jsonParse(allocator, source, options);
        return try jsonParseFromValue(allocator, value, options);
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!ResourceContents {
        if (source != .object) return error.UnexpectedToken;
        if (source.object.get("blob") != null) {
            return .{ .blob = try std.json.innerParseFromValue(BlobResourceContents, allocator, source, options) };
        }
        return .{ .text = try std.json.innerParseFromValue(TextResourceContents, allocator, source, options) };
    }

    pub fn jsonStringify(self: ResourceContents, stream: anytype) @TypeOf(stream.*).Error!void {
        switch (self) {
            inline else => |payload| try stream.write(payload),
        }
    }
};

pub const TextResourceContents = struct {
    /// The URI of this resource.
    uri: []const u8,
    mimeType: ?[]const u8 = null,
    /// The text of the item. This must only be set if the item can actually be represented as text (not binary data).
    text: []const u8,
    _meta: ?Meta = null,
};

pub const BlobResourceContents = struct {
    /// The URI of this resource.
    uri: []const u8,
    mimeType: ?[]const u8 = null,
    /// A base64-encoded string representing the binary data of the item.
    blob: []const u8,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Tools
// ----------------------------------------------------------------------------

/// A JSON value that is either pre-serialized or a dynamic `std.json.Value`.
/// Parsing always produces `.value`.
pub const Json = union(enum) {
    /// Must be valid, minified JSON (no newline characters).
    raw: []const u8,
    value: std.json.Value,

    pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!Json {
        return .{ .value = try std.json.Value.jsonParse(allocator, source, options) };
    }

    pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!Json {
        _ = allocator;
        _ = options;
        return .{ .value = source };
    }

    pub fn jsonStringify(self: Json, stream: anytype) @TypeOf(stream.*).Error!void {
        switch (self) {
            .raw => |raw| {
                try stream.beginWriteRaw();
                try stream.writer.writeAll(raw);
                stream.endWriteRaw();
            },
            .value => |value| try stream.write(value),
        }
    }
};

/// A JSON Schema object. Use `.raw` for schemas generated by `schema.generate`.
pub const JsonSchema = Json;

/// Definition for a tool the client can call.
pub const Tool = struct {
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    /// A human-readable description of the tool. Used by clients to improve the LLM's understanding of available tools.
    description: ?[]const u8 = null,
    /// A JSON Schema object defining the expected parameters for the tool. Must have `"type": "object"`.
    inputSchema: JsonSchema,
    /// An optional JSON Schema object defining the structure of the tool's output in `structuredContent`.
    outputSchema: ?JsonSchema = null,
    annotations: ?ToolAnnotations = null,
    icons: ?[]const Icon = null,
    _meta: ?Meta = null,
};

/// Additional properties describing a Tool to clients. All properties are hints.
pub const ToolAnnotations = struct {
    /// A human-readable title for the tool.
    title: ?[]const u8 = null,
    /// If true, the tool does not modify its environment. Default: false
    readOnlyHint: ?bool = null,
    /// If true, the tool may perform destructive updates to its environment. Default: true
    destructiveHint: ?bool = null,
    /// If true, calling the tool repeatedly with the same arguments has no additional effect. Default: false
    idempotentHint: ?bool = null,
    /// If true, this tool may interact with an "open world" of external entities. Default: true
    openWorldHint: ?bool = null,
};

pub const ListToolsResult = struct {
    tools: []const Tool,
    nextCursor: ?Cursor = null,
    _meta: ?Meta = null,
};

pub const CallToolRequestParams = struct {
    name: []const u8,
    /// Must be a JSON object if present.
    arguments: ?std.json.Value = null,
    _meta: ?Meta = null,
};

/// The server's response to a tool call.
pub const CallToolResult = struct {
    /// A list of content objects that represent the unstructured result of the tool call.
    content: []const ContentBlock,
    /// An optional JSON object that represents the structured result of the tool call.
    structuredContent: ?Json = null,
    /// Whether the tool call ended in an error. Tool errors are reported here
    /// (instead of as a JSON-RPC error) so that the LLM can see and handle them.
    isError: ?bool = null,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Resources
// ----------------------------------------------------------------------------

/// A known resource that the server is capable of reading.
pub const Resource = struct {
    /// The URI of this resource.
    uri: []const u8,
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    mimeType: ?[]const u8 = null,
    /// The size of the raw resource content, in bytes (before base64 encoding or tokenization), if known.
    size: ?i64 = null,
    annotations: ?Annotations = null,
    icons: ?[]const Icon = null,
    _meta: ?Meta = null,
};

/// A template description for resources available on the server.
pub const ResourceTemplate = struct {
    /// A URI template (according to RFC 6570) that can be used to construct resource URIs.
    uriTemplate: []const u8,
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    /// The MIME type for all resources that match this template, if they share one.
    mimeType: ?[]const u8 = null,
    annotations: ?Annotations = null,
    icons: ?[]const Icon = null,
    _meta: ?Meta = null,
};

pub const ListResourcesResult = struct {
    resources: []const Resource,
    nextCursor: ?Cursor = null,
    _meta: ?Meta = null,
};

pub const ListResourceTemplatesResult = struct {
    resourceTemplates: []const ResourceTemplate,
    nextCursor: ?Cursor = null,
    _meta: ?Meta = null,
};

pub const ReadResourceRequestParams = struct {
    /// The URI of the resource to read. The URI can use any protocol; it is up to the server how to interpret it.
    uri: []const u8,
    _meta: ?Meta = null,
};

pub const ReadResourceResult = struct {
    contents: []const ResourceContents,
    _meta: ?Meta = null,
};

pub const SubscribeRequestParams = struct {
    /// The URI of the resource to subscribe to.
    uri: []const u8,
    _meta: ?Meta = null,
};

pub const UnsubscribeRequestParams = struct {
    /// The URI of the resource to unsubscribe from.
    uri: []const u8,
    _meta: ?Meta = null,
};

pub const ResourceUpdatedNotificationParams = struct {
    /// The URI of the resource that has been updated. This might be a sub-resource of the one that the client actually subscribed to.
    uri: []const u8,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Prompts
// ----------------------------------------------------------------------------

/// A prompt or prompt template that the server offers.
pub const Prompt = struct {
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    /// A list of arguments to use for templating the prompt.
    arguments: ?[]const PromptArgument = null,
    icons: ?[]const Icon = null,
    _meta: ?Meta = null,
};

/// Describes an argument that a prompt can accept.
pub const PromptArgument = struct {
    /// Intended for programmatic or logical use.
    name: []const u8,
    /// Intended for UI and end-user contexts.
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    /// Whether this argument must be provided.
    required: ?bool = null,
};

pub const ListPromptsResult = struct {
    prompts: []const Prompt,
    nextCursor: ?Cursor = null,
    _meta: ?Meta = null,
};

pub const GetPromptRequestParams = struct {
    /// The name of the prompt or prompt template.
    name: []const u8,
    /// Arguments to use for templating the prompt. A JSON object with string values.
    arguments: ?std.json.Value = null,
    _meta: ?Meta = null,
};

pub const GetPromptResult = struct {
    /// An optional description for the prompt.
    description: ?[]const u8 = null,
    messages: []const PromptMessage,
    _meta: ?Meta = null,
};

/// Describes a message returned as part of a prompt.
pub const PromptMessage = struct {
    role: Role,
    content: ContentBlock,
};

// ----------------------------------------------------------------------------
// Logging
// ----------------------------------------------------------------------------

/// The severity of a log message. These map to syslog message severities (RFC 5424).
pub const LoggingLevel = enum {
    debug,
    info,
    notice,
    warning,
    @"error",
    critical,
    alert,
    emergency,
};

pub const SetLevelRequestParams = struct {
    /// The level of logging that the client wants to receive from the server.
    level: LoggingLevel,
    _meta: ?Meta = null,
};

pub const LoggingMessageNotificationParams = struct {
    /// The severity of this log message.
    level: LoggingLevel,
    /// An optional name of the logger issuing this message.
    logger: ?[]const u8 = null,
    /// The data to be logged, such as a string message or an object.
    data: std.json.Value,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Autocomplete
// ----------------------------------------------------------------------------

pub const PromptReference = struct {
    name: []const u8,
    title: ?[]const u8 = null,
};

pub const ResourceTemplateReference = struct {
    /// The URI or URI template of the resource.
    uri: []const u8,
};

/// Discriminated by the `"type"` field.
pub const CompletionReference = union(enum) {
    @"ref/prompt": PromptReference,
    @"ref/resource": ResourceTemplateReference,

    pub const jsonParse = TypeDiscriminated(@This()).jsonParse;
    pub const jsonParseFromValue = TypeDiscriminated(@This()).jsonParseFromValue;
    pub const jsonStringify = TypeDiscriminated(@This()).jsonStringify;
};

pub const CompleteRequestParams = struct {
    ref: CompletionReference,
    /// The argument's information.
    argument: struct {
        name: []const u8,
        /// The value of the argument to use for completion matching.
        value: []const u8,
    },
    context: ?struct {
        /// Previously-resolved variables in a URI template or prompt.
        arguments: ?std.json.Value = null,
    } = null,
    _meta: ?Meta = null,
};

pub const CompleteResult = struct {
    completion: struct {
        /// An array of completion values. Must not exceed 100 items.
        values: []const []const u8,
        /// The total number of completion options available.
        total: ?i64 = null,
        /// Indicates whether there are additional completion options beyond those provided.
        hasMore: ?bool = null,
    },
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Roots
// ----------------------------------------------------------------------------

/// Represents a root directory or file that the server can operate on.
pub const Root = struct {
    /// The URI identifying the root. This must start with `file://` for now.
    uri: []const u8,
    name: ?[]const u8 = null,
    _meta: ?Meta = null,
};

pub const ListRootsResult = struct {
    roots: []const Root,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Sampling
// ----------------------------------------------------------------------------

/// Describes a message issued to or received from an LLM API.
///
/// The specification also allows an array of content blocks (including
/// `tool_use`/`tool_result` blocks) which is not modeled here.
pub const SamplingMessage = struct {
    role: Role,
    content: ContentBlock,
    _meta: ?Meta = null,
};

/// The server's preferences for model selection, requested of the client during sampling.
pub const ModelPreferences = struct {
    /// Optional hints to use for model selection, evaluated in order.
    hints: ?[]const ModelHint = null,
    /// How much to prioritize cost when selecting a model, from 0 to 1.
    costPriority: ?f64 = null,
    /// How much to prioritize sampling speed (latency) when selecting a model, from 0 to 1.
    speedPriority: ?f64 = null,
    /// How much to prioritize intelligence and capabilities when selecting a model, from 0 to 1.
    intelligencePriority: ?f64 = null,
};

pub const ModelHint = struct {
    /// A hint for a model name, e.g. `"sonnet"` or `"claude"`.
    name: ?[]const u8 = null,
};

pub const CreateMessageRequestParams = struct {
    messages: []const SamplingMessage,
    modelPreferences: ?ModelPreferences = null,
    /// An optional system prompt the server wants to use for sampling.
    systemPrompt: ?[]const u8 = null,
    includeContext: ?enum { none, thisServer, allServers } = null,
    temperature: ?f64 = null,
    /// The requested maximum number of tokens to sample.
    maxTokens: i64,
    stopSequences: ?[]const []const u8 = null,
    /// Optional metadata to pass through to the LLM provider.
    metadata: ?std.json.Value = null,
    _meta: ?Meta = null,
};

pub const CreateMessageResult = struct {
    role: Role,
    content: ContentBlock,
    /// The name of the model that generated the message.
    model: []const u8,
    /// The reason why sampling stopped, if known (e.g. `"endTurn"`, `"stopSequence"`, `"maxTokens"`).
    stopReason: ?[]const u8 = null,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Elicitation
// ----------------------------------------------------------------------------

pub const ElicitRequestParams = struct {
    /// Defaults to `form` if omitted.
    mode: ?enum { form, url } = null,
    /// The message to present to the user.
    message: []const u8,
    /// Form mode: a restricted subset of JSON Schema (flat object with primitive properties).
    requestedSchema: ?JsonSchema = null,
    /// URL mode: the URL that the user should navigate to.
    url: ?[]const u8 = null,
    /// URL mode: the ID of the elicitation, which must be unique within the context of the server.
    elicitationId: ?[]const u8 = null,
    _meta: ?Meta = null,
};

pub const ElicitResult = struct {
    action: enum { accept, decline, cancel },
    /// The submitted form data, only present when `action` is `accept` in form mode.
    content: ?std.json.Value = null,
    _meta: ?Meta = null,
};

// ----------------------------------------------------------------------------
// Method tables
// ----------------------------------------------------------------------------

/// Associates a method with its params and result type.
/// A `Params` type of `void` means that the method has no params.
pub fn RequestMethod(comptime ParamsT: type, comptime ResultT: type) type {
    return struct {
        pub const Params = ParamsT;
        pub const Result = ResultT;
    };
}

/// Associates a method with its params type.
/// A `Params` type of `void` means that the method has no params.
pub fn NotificationMethod(comptime ParamsT: type) type {
    return struct {
        pub const Params = ParamsT;
    };
}

/// Requests sent from the client to the server.
pub const client_requests = struct {
    pub const initialize = RequestMethod(InitializeRequestParams, InitializeResult);
    pub const ping = RequestMethod(void, EmptyResult);
    pub const @"tools/list" = RequestMethod(PaginatedRequestParams, ListToolsResult);
    pub const @"tools/call" = RequestMethod(CallToolRequestParams, CallToolResult);
    pub const @"prompts/list" = RequestMethod(PaginatedRequestParams, ListPromptsResult);
    pub const @"prompts/get" = RequestMethod(GetPromptRequestParams, GetPromptResult);
    pub const @"resources/list" = RequestMethod(PaginatedRequestParams, ListResourcesResult);
    pub const @"resources/templates/list" = RequestMethod(PaginatedRequestParams, ListResourceTemplatesResult);
    pub const @"resources/read" = RequestMethod(ReadResourceRequestParams, ReadResourceResult);
    pub const @"resources/subscribe" = RequestMethod(SubscribeRequestParams, EmptyResult);
    pub const @"resources/unsubscribe" = RequestMethod(UnsubscribeRequestParams, EmptyResult);
    pub const @"logging/setLevel" = RequestMethod(SetLevelRequestParams, EmptyResult);
    pub const @"completion/complete" = RequestMethod(CompleteRequestParams, CompleteResult);
};

/// Notifications sent from the client to the server.
pub const client_notifications = struct {
    pub const @"notifications/initialized" = NotificationMethod(void);
    pub const @"notifications/cancelled" = NotificationMethod(CancelledNotificationParams);
    pub const @"notifications/progress" = NotificationMethod(ProgressNotificationParams);
    pub const @"notifications/roots/list_changed" = NotificationMethod(void);
};

/// Requests sent from the server to the client.
pub const server_requests = struct {
    pub const ping = RequestMethod(void, EmptyResult);
    pub const @"sampling/createMessage" = RequestMethod(CreateMessageRequestParams, CreateMessageResult);
    pub const @"elicitation/create" = RequestMethod(ElicitRequestParams, ElicitResult);
    pub const @"roots/list" = RequestMethod(void, ListRootsResult);
};

/// Notifications sent from the server to the client.
pub const server_notifications = struct {
    pub const @"notifications/cancelled" = NotificationMethod(CancelledNotificationParams);
    pub const @"notifications/progress" = NotificationMethod(ProgressNotificationParams);
    pub const @"notifications/message" = NotificationMethod(LoggingMessageNotificationParams);
    pub const @"notifications/resources/updated" = NotificationMethod(ResourceUpdatedNotificationParams);
    pub const @"notifications/resources/list_changed" = NotificationMethod(void);
    pub const @"notifications/tools/list_changed" = NotificationMethod(void);
    pub const @"notifications/prompts/list_changed" = NotificationMethod(void);
};

// ----------------------------------------------------------------------------
// JSON helpers
// ----------------------------------------------------------------------------

/// Implements `std.json` (de)serialization for a tagged union whose payloads
/// are structs and whose active field is determined by a `"type"` field. The
/// union field names are the possible values of the `"type"` field.
///
/// Unknown fields inside the payload are always ignored when parsing.
pub fn TypeDiscriminated(comptime T: type) type {
    const discriminator = "type";
    return struct {
        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            const value = try std.json.Value.jsonParse(allocator, source, options);
            return try jsonParseFromValue(allocator, value, options);
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            if (source != .object) return error.UnexpectedToken;
            const tag_value = source.object.get(discriminator) orelse return error.MissingField;
            if (tag_value != .string) return error.UnexpectedToken;

            var payload_options = options;
            payload_options.ignore_unknown_fields = true;

            const info = @typeInfo(T).@"union";
            inline for (info.field_names, info.field_types) |field_name, FieldType| {
                if (std.mem.eql(u8, field_name, tag_value.string)) {
                    return @unionInit(T, field_name, try std.json.innerParseFromValue(FieldType, allocator, source, payload_options));
                }
            }
            return error.InvalidEnumTag;
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            switch (self) {
                inline else => |payload, tag| {
                    try stream.beginObject();
                    try stream.objectField(discriminator);
                    try stream.write(@tagName(tag));
                    const S = @typeInfo(@TypeOf(payload)).@"struct";
                    inline for (S.field_names, S.field_types) |field_name, FieldType| {
                        const field_value = @field(payload, field_name);
                        const emit = if (@typeInfo(FieldType) == .optional)
                            stream.options.emit_null_optional_fields or field_value != null
                        else
                            true;
                        if (emit) {
                            try stream.objectField(field_name);
                            try stream.write(field_value);
                        }
                    }
                    try stream.endObject();
                },
            }
        }
    };
}

test ContentBlock {
    const allocator = std.testing.allocator;

    try std.testing.expectFmt(
        \\{"type":"text","text":"hello"}
    , "{f}", .{std.json.fmt(ContentBlock.initText("hello"), .{ .emit_null_optional_fields = false })});

    try std.testing.expectFmt(
        \\{"type":"resource","resource":{"uri":"file:///a","blob":"AA=="}}
    , "{f}", .{std.json.fmt(ContentBlock{ .resource = .{
        .resource = .{ .blob = .{ .uri = "file:///a", .blob = "AA==" } },
    } }, .{ .emit_null_optional_fields = false })});

    const parsed = try std.json.parseFromSlice(
        []const ContentBlock,
        allocator,
        \\[{"type":"image","data":"AA==","mimeType":"image/png"},{"text":"x","type":"text"},{"type":"resource","resource":{"uri":"u","text":"t"}}]
    ,
        .{},
    );
    defer parsed.deinit();
    try std.testing.expectEqualStrings("image/png", parsed.value[0].image.mimeType);
    try std.testing.expectEqualStrings("x", parsed.value[1].text.text);
    try std.testing.expectEqualStrings("t", parsed.value[2].resource.resource.text.text);

    try std.testing.expectError(error.InvalidEnumTag, std.json.parseFromSlice(ContentBlock, allocator,
        \\{"type":"video"}
    , .{}));
    try std.testing.expectError(error.MissingField, std.json.parseFromSlice(ContentBlock, allocator,
        \\{"text":"x"}
    , .{}));
}

test CompletionReference {
    const parsed = try std.json.parseFromSlice(CompleteRequestParams, std.testing.allocator,
        \\{"ref":{"type":"ref/prompt","name":"greet"},"argument":{"name":"who","value":"W"}}
    , .{});
    defer parsed.deinit();
    try std.testing.expectEqualStrings("greet", parsed.value.ref.@"ref/prompt".name);
}

test Json {
    const tool: Tool = .{ .name = "t", .inputSchema = .{ .raw = "{\"type\":\"object\"}" } };
    try std.testing.expectFmt(
        \\{"name":"t","inputSchema":{"type":"object"}}
    , "{f}", .{std.json.fmt(tool, .{ .emit_null_optional_fields = false })});
}

test InitializeRequestParams {
    const parsed = try std.json.parseFromSlice(InitializeRequestParams, std.testing.allocator,
        \\{"protocolVersion":"2025-11-25","capabilities":{"roots":{"listChanged":true},"sampling":{}},"clientInfo":{"name":"c","version":"1"}}
    , .{ .ignore_unknown_fields = true });
    defer parsed.deinit();
    try std.testing.expect(parsed.value.capabilities.roots.?.listChanged.?);
    try std.testing.expect(parsed.value.capabilities.sampling != null);
    try std.testing.expect(parsed.value.capabilities.elicitation == null);
}
