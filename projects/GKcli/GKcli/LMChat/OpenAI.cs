using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace GKcli.LMChat.OpenAI;


public class ChatMessage
{
    [JsonPropertyName("role")]
    public string Role { get; set; }

    [JsonPropertyName("content")]
    public string Content { get; set; }

    [JsonPropertyName("tool_calls")]
    public List<ToolCall> ToolCalls { get; set; }

    public ChatMessage()
    {
    }

    public ChatMessage(string role, string content)
    {
        Role = role;
        Content = content;
    }
}


public class ToolCall
{
    public string Id { get; set; }
    public string Type { get; set; } = "function";
    public FunctionCall Function { get; set; }
}


public class FunctionCall
{
    public string Name { get; set; }
    public string Arguments { get; set; }
}


public class ChatRequest
{
    [JsonPropertyName("model")]
    public string Model { get; set; }

    [JsonPropertyName("messages")]
    public List<ChatMessage> Messages { get; set; }

    [JsonPropertyName("stream")]
    public bool Stream { get; set; }

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
    [JsonPropertyName("choices")]
    public List<ChatStreamChoice> Choices { get; set; }
}


public class ChatStreamChoice
{
    [JsonPropertyName("delta")]
    public ChatDelta Delta { get; set; }
}


public class ChatDelta
{
    [JsonPropertyName("content")]
    public string Content { get; set; }
}
