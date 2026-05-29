using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace GKcli.LMChat.OpenAI;

public record ChatMessage(
    [property: JsonPropertyName("role")] string Role,
    [property: JsonPropertyName("content")] string Content
);

internal record ChatRequest(
    [property: JsonPropertyName("model")] string Model,
    [property: JsonPropertyName("messages")] List<ChatMessage> Messages,
    [property: JsonPropertyName("stream")] bool Stream
);

internal record ChatResponse(
    [property: JsonPropertyName("choices")] List<ChatChoice> Choices
);

internal record ChatChoice(
    [property: JsonPropertyName("message")] ChatMessage Message
);

internal record ChatStreamResponse(
    [property: JsonPropertyName("choices")] List<ChatStreamChoice> Choices
);

internal record ChatStreamChoice(
    [property: JsonPropertyName("delta")] ChatDelta Delta
);

internal record ChatDelta(
    [property: JsonPropertyName("content")] string Content
);
