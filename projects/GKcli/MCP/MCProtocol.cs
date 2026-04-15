/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace GKcli.MCP;

/// <summary>
/// JSON-RPC 2.0 Request message.
/// </summary>
internal class MCPRequest
{
    [JsonPropertyName("jsonrpc")]
    public string JsonRpc { get; set; }

    [JsonPropertyName("id")]
    public JsonElement? Id { get; set; }

    [JsonPropertyName("method")]
    public string Method { get; set; }

    [JsonPropertyName("params")]
    public JsonElement? Params { get; set; }
}

/// <summary>
/// JSON-RPC 2.0 Response message.
/// </summary>
internal class MCPResponse
{
    [JsonPropertyName("jsonrpc")]
    public string JsonRpc { get; set; }

    [JsonPropertyName("id")]
    public JsonElement? Id { get; set; }

    [JsonPropertyName("result")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public object Result { get; set; }

    [JsonPropertyName("error")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public MCPError Error { get; set; }

    public MCPResponse()
    {
        JsonRpc = "2.0";
    }
}

/// <summary>
/// JSON-RPC 2.0 Error object.
/// </summary>
internal class MCPError
{
    [JsonPropertyName("code")]
    public int Code { get; set; }

    [JsonPropertyName("message")]
    public string Message { get; set; } = "";

    [JsonPropertyName("data")]
    public object Data { get; set; }

    public static MCPError FromException(int code, Exception ex)
    {
        return new MCPError {
            Code = code,
            Message = ex.Message,
            Data = new { ex.StackTrace }
        };
    }

    public static MCPError MethodNotFound()
    {
        return new MCPError {
            Code = -32601,
            Message = "Method not found"
        };
    }

    public static MCPError InvalidParams(string message)
    {
        return new MCPError {
            Code = -32602,
            Message = message
        };
    }

    public static MCPError InternalError(string message)
    {
        return new MCPError {
            Code = -32603,
            Message = message
        };
    }
}

/// <summary>
/// MCP Tools List response result.
/// </summary>
internal class MCPToolsListResult
{
    [JsonPropertyName("tools")]
    public List<MCPTool> Tools { get; set; }
}

/// <summary>
/// MCP Tool definition.
/// </summary>
internal class MCPTool
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("inputSchema")]
    public MCPToolInputSchema InputSchema { get; set; } = new();
}

/// <summary>
/// MCP Tool input schema.
/// </summary>
internal class MCPToolInputSchema
{
    [JsonIgnore]
    public static readonly MCPToolInputSchema Empty = new MCPToolInputSchema { Properties = new(), Required = new() };


    [JsonPropertyName("type")]
    public string Type { get; set; } = "object";

    [JsonPropertyName("properties")]
    public Dictionary<string, MCPToolProperty> Properties { get; set; } = new();

    [JsonPropertyName("required")]
    public List<string> Required { get; set; } = new();
}

/// <summary>
/// MCP Tool property definition.
/// </summary>
internal class MCPToolProperty
{
    [JsonPropertyName("type")]
    public string Type { get; set; } = "string";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("enum")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public List<string> Enum { get; set; }
}

/// <summary>
/// MCP Call result content item.
/// </summary>
internal class MCPContent
{
    [JsonPropertyName("type")]
    public string Type { get; set; } = "text";

    [JsonPropertyName("text")]
    public string Text { get; set; } = "";


    public static List<MCPContent> CreateSimpleContent(string text)
    {
        return new List<MCPContent> { new MCPContent { Text = text } };
    }
}

/// <summary>
/// MCP Initialize result.
/// </summary>
internal class MCPInitializeResult
{
    [JsonPropertyName("protocolVersion")]
    public string ProtocolVersion { get; set; } = "2024-11-05";

    [JsonPropertyName("capabilities")]
    public MCPCapabilities Capabilities { get; set; } = new();

    [JsonPropertyName("serverInfo")]
    public MCPServerInfo ServerInfo { get; set; } = new();
}

/// <summary>
/// MCP Capabilities.
/// </summary>
internal class MCPCapabilities
{
    [JsonPropertyName("tools")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public MCPToolsCapability Tools { get; set; }

    [JsonPropertyName("resources")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public MCPResourcesCapability Resources { get; set; }

    [JsonPropertyName("prompts")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public MCPPromptsCapability Prompts { get; set; }
}

/// <summary>
/// MCP Resources capability.
/// </summary>
internal class MCPResourcesCapability
{
    [JsonPropertyName("subscribe")]
    public bool Subscribe { get; set; }

    [JsonPropertyName("listChanged")]
    public bool ListChanged { get; set; }
}

/// <summary>
/// MCP Prompts capability.
/// </summary>
internal class MCPPromptsCapability
{
    [JsonPropertyName("listChanged")]
    public bool ListChanged { get; set; }
}

/// <summary>
/// MCP Tools capability.
/// </summary>
internal class MCPToolsCapability
{
    [JsonPropertyName("listChanged")]
    public bool ListChanged { get; set; }
}

/// <summary>
/// MCP Server info.
/// </summary>
internal class MCPServerInfo
{
    [JsonPropertyName("name")]
    public string Name { get; set; }

    [JsonPropertyName("version")]
    public string Version { get; set; }
}

// ===================== Resources =====================

/// <summary>
/// MCP Resource definition.
/// </summary>
internal class MCPResource
{
    [JsonPropertyName("uri")]
    public string Uri { get; set; } = "";

    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("title")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Title { get; set; }

    [JsonPropertyName("description")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Description { get; set; }

    [JsonPropertyName("mimeType")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string MimeType { get; set; }

    [JsonPropertyName("size")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public long? Size { get; set; }
}

/// <summary>
/// MCP Resource Template definition.
/// </summary>
internal class MCPResourceTemplate
{
    [JsonPropertyName("uriTemplate")]
    public string UriTemplate { get; set; } = "";

    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("title")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Title { get; set; }

    [JsonPropertyName("description")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Description { get; set; }

    [JsonPropertyName("mimeType")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string MimeType { get; set; }
}

/// <summary>
/// MCP Resource Contents (base type).
/// </summary>
internal class MCPResourceContents
{
    [JsonPropertyName("uri")]
    public string Uri { get; set; } = "";

    [JsonPropertyName("mimeType")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string MimeType { get; set; }

    [JsonPropertyName("text")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Text { get; set; }

    [JsonPropertyName("blob")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Blob { get; set; }
}

/// <summary>
/// MCP Resources List response result.
/// </summary>
internal class MCPResourcesListResult
{
    [JsonPropertyName("resources")]
    public List<MCPResource> Resources { get; set; } = new();

    [JsonPropertyName("nextCursor")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string NextCursor { get; set; }
}

/// <summary>
/// MCP Resource Templates List response result.
/// </summary>
internal class MCPResourceTemplatesListResult
{
    [JsonPropertyName("resourceTemplates")]
    public List<MCPResourceTemplate> ResourceTemplates { get; set; } = new();

    [JsonPropertyName("nextCursor")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string NextCursor { get; set; }
}

/// <summary>
/// MCP Read Resource response result.
/// </summary>
internal class MCPReadResourceResult
{
    [JsonPropertyName("contents")]
    public List<MCPResourceContents> Contents { get; set; } = new();
}

// ===================== Prompts =====================

/// <summary>
/// MCP Prompt Argument definition.
/// </summary>
internal class MCPPromptArgument
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("description")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Description { get; set; }

    [JsonPropertyName("required")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public bool? Required { get; set; }
}

/// <summary>
/// MCP Prompt definition.
/// </summary>
internal class MCPPrompt
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";

    [JsonPropertyName("title")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Title { get; set; }

    [JsonPropertyName("description")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Description { get; set; }

    [JsonPropertyName("arguments")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public List<MCPPromptArgument> Arguments { get; set; }
}

/// <summary>
/// MCP Prompt Content (text, image, audio, or resource).
/// </summary>
internal class MCPPromptContent
{
    [JsonPropertyName("type")]
    public string Type { get; set; } = "text";

    [JsonPropertyName("text")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Text { get; set; }

    [JsonPropertyName("data")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Data { get; set; }

    [JsonPropertyName("mimeType")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string MimeType { get; set; }

    [JsonPropertyName("resource")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public MCPResourceContents Resource { get; set; }
}

/// <summary>
/// MCP Prompt Message.
/// </summary>
internal class MCPPromptMessage
{
    [JsonPropertyName("role")]
    public string Role { get; set; } = "user"; // "user" or "assistant"

    [JsonPropertyName("content")]
    public MCPPromptContent Content { get; set; } = new();
}

/// <summary>
/// MCP Prompts List response result.
/// </summary>
internal class MCPPromptsListResult
{
    [JsonPropertyName("prompts")]
    public List<MCPPrompt> Prompts { get; set; } = new();

    [JsonPropertyName("nextCursor")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string NextCursor { get; set; }
}

/// <summary>
/// MCP Get Prompt response result.
/// </summary>
internal class MCPPromptGetResult
{
    [JsonPropertyName("description")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Description { get; set; }

    [JsonPropertyName("messages")]
    public List<MCPPromptMessage> Messages { get; set; } = new();
}
