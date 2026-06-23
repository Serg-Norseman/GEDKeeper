/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace GKCortex.Protocols;


public class ChatMessage
{
    /// <summary>
    /// Supported values are system, user, assistant, or tool.
    /// </summary>
    [JsonPropertyName("role")]
    public string Role { get; set; }

    /// <summary>
    /// It is only allowed to be null if tool_calls or a refusal are present.
    /// </summary>
    [JsonPropertyName("content")]
    public string Content { get; set; }

    /// <summary>
    /// Populated automatically by the model instead of content if it rejects your prompt due to safety filters or structural violations.
    /// </summary>
    [JsonPropertyName("refusal")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string Refusal { get; set; } = null;

    [JsonPropertyName("tool_calls")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public List<ToolCall> ToolCalls { get; set; } = null;

    public ChatMessage()
    {
    }

    public ChatMessage(string role, string content)
    {
        Role = role;
        Content = content;
    }
}


public class ToolDef
{
    [JsonPropertyName("type")]
    public string Type { get; set; } = "function";

    [JsonPropertyName("function")]
    public FunctionDef Function { get; set; }

    public ToolDef()
    {
    }

    public ToolDef(MCPTool mcpTool)
    {
        Function = new FunctionDef() {
            Name = mcpTool.Name,
            Description = mcpTool.Description,
            Parameters = mcpTool.InputSchema
        };
    }
}


public class FunctionDef
{
    [JsonPropertyName("name")]
    public string Name { get; set; }

    [JsonPropertyName("description")]
    public string Description { get; set; }

    [JsonPropertyName("parameters")]
    public MCPToolInputSchema Parameters { get; set; }

    [JsonPropertyName("strict")]
    public bool Strict { get; set; } = true;
}


public class ToolCall
{
    [JsonPropertyName("id")]
    public string Id { get; set; }

    [JsonPropertyName("type")]
    public string Type { get; set; } = "function";

    [JsonPropertyName("function")]
    public FunctionCall Function { get; set; }
}


public class FunctionCall
{
    [JsonPropertyName("name")]
    public string Name { get; set; }

    [JsonPropertyName("arguments")]
    public string Arguments { get; set; }
}


public class ChatRequest
{
    [JsonPropertyName("model")]
    public string Model { get; set; }

    [JsonPropertyName("messages")]
    public List<ChatMessage> Messages { get; set; }


    [JsonPropertyName("stream")]
    public bool Stream { get; set; } = false;

    [JsonPropertyName("max_tokens")]
    public int MaxTokens { get; set; } = 2048;


    [JsonPropertyName("temperature")]
    public double Temperature { get; set; } = 0.35;

    [JsonPropertyName("top_p")]
    public double TopP { get; set; } = 0.8;


    [JsonPropertyName("presence_penalty")]
    public double PresencePenalty { get; set; } = 0.0;

    [JsonPropertyName("frequency_penalty")]
    public double FrequencyPenalty { get; set; } = 0.0;


    [JsonPropertyName("tools")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public List<ToolDef> Tools { get; set; } = null;


    public ChatRequest()
    {
    }

    public ChatRequest(string model, List<ChatMessage> messages, bool stream)
    {
        Model = model;
        Messages = messages;
        Stream = stream;
    }
}


public class ChatResponse
{
    [JsonPropertyName("choices")]
    public List<ChatChoice> Choices { get; set; }
}


public class ChatChoice
{
    [JsonPropertyName("message")]
    public ChatMessage Message { get; set; }
}


public class ChatStreamResponse
{
    [JsonPropertyName("id")]
    public string Id { get; set; } = string.Empty;

    [JsonPropertyName("object")]
    public string ObjectType { get; set; } = string.Empty;

    [JsonPropertyName("created")]
    public long Created { get; set; }

    [JsonPropertyName("model")]
    public string Model { get; set; } = string.Empty;

    [JsonPropertyName("choices")]
    public List<ChatStreamChoice> Choices { get; set; }
}


public class ChatStreamChoice
{
    [JsonPropertyName("index")]
    public int Index { get; set; }

    [JsonPropertyName("delta")]
    public ChatStreamDelta Delta { get; set; }

    [JsonPropertyName("finish_reason")]
    public string? FinishReason { get; set; }
}


public class ChatStreamDelta
{
    [JsonPropertyName("role")]
    public string Role { get; set; }


    [JsonPropertyName("content")]
    public string Content { get; set; }

    [JsonPropertyName("tool_calls")]
    public List<ChatStreamToolCall> ToolCalls { get; set; }
}


public class ChatStreamToolCall
{
    [JsonPropertyName("index")]
    public int Index { get; set; }

    [JsonPropertyName("id")]
    public string Id { get; set; }

    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("function")]
    public ChatStreamFunction Function { get; set; }
}


public class ChatStreamFunction
{
    [JsonPropertyName("name")]
    public string Name { get; set; }

    [JsonPropertyName("arguments")]
    public string Arguments { get; set; }
}


public class ModelsResponse
{
    [JsonPropertyName("data")]
    public List<ModelData> Data { get; set; }
}


public class ModelData
{
    [JsonPropertyName("id")]
    public string Id { get; set; }
}
