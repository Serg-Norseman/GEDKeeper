/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Runtime.CompilerServices;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using GKCortex.Features;
using GKCortex.Protocols;

namespace GKCortex.LMChat;


public interface ILMChatView
{
    void ShowMessage(string msg, string role);

    void StartStreamingMessage(int requestId);
    void UpdateStreamingMessage(int requestId, string content);
    void FinalizeStreamingMessage(int requestId);
}


public interface ILMChat
{
    LMHistoryStorage HistoryStorage { get; }
    LMSettings Settings { get; }

    Task<string> SendMessageSingleAsync(string role, string content, float temperature);
    Task SendMessageAsync();
}


public class LMChatClient : ILMChat
{
    private readonly HttpClient fHttpClient;
    private int fRequestId;
    private string fSystemPrompt;
    private readonly List<ToolDef> fTools;
    private readonly CancellationTokenSource fTokenSource;

    public LMHistoryStorage HistoryStorage { get; private set; }

    public LMSettings Settings { get; set; }

    public string SystemPrompt
    {
        get { return fSystemPrompt; }
        set { fSystemPrompt = value; }
    }

    public ILMChatView View { get; set; }


    public LMChatClient(LMSettings settings)
    {
        fHttpClient = new HttpClient();
        fHttpClient.Timeout = Timeout.InfiniteTimeSpan;
        fHttpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        fHttpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("text/event-stream"));

        fSystemPrompt = "You are an advanced AI assistant for the GEDKeeper genealogy program. You help analyze databases and build family trees.";

        Settings = settings;
        HistoryStorage = new LMHistoryStorage();

        var mcpTools = MCPController.GetTools();
        fTools = mcpTools.Select(mT => new ToolDef(mT)).ToList();

        fTokenSource = new CancellationTokenSource(TimeSpan.FromMinutes(5));

        NewSession();
    }

    /// <summary>
    /// Clear chat history.
    /// </summary>
    public void NewSession()
    {
        HistoryStorage.CreateNewSession("unknown", Settings, fSystemPrompt);
        fRequestId = 0;
    }

    /// <summary>
    /// Cancel current request.
    /// </summary>
    public void CancelRequest()
    {
        fTokenSource.Cancel();
        fTokenSource.TryReset();
    }

    public async Task AddHistory(string role, string content)
    {
        await HistoryStorage.AddToCurrentHistory(new ChatMessage(role, content));
    }

    private void RequireHttp()
    {
        fHttpClient.BaseAddress = new Uri(Settings.APIAddress);

        if (!string.IsNullOrEmpty(Settings.APIKey))
            fHttpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", Settings.APIKey);
    }

    public async Task<List<ModelData>> LoadModelsAsync()
    {
        RequireHttp();

        var response = await fHttpClient.GetAsync("v1/models");
        response.EnsureSuccessStatusCode();

        var data = await response.Content.ReadFromJsonAsync<ModelsResponse>();
        return data.Data;
    }

    /// <summary>
    /// Process tool calls and return result
    /// </summary>
    private async Task ProcessToolCallsAsync(List<ToolCall> toolCalls)
    {
        fTokenSource.TryReset();

        // Create new message for tool results
        var toolResults = new List<ChatMessage>();

        foreach (var toolCall in toolCalls) {
            if (toolCall.Type != "function" || toolCall.Function == null) continue;
            string funcName = toolCall.Function.Name;

            ChatMessage toolResult = null;
            try {
                View?.ShowMessage($"Attempt to call the tool '{funcName}'", "system");

                // Parse tool arguments
                JsonElement args = JsonDocument.Parse(toolCall.Function.Arguments).RootElement;

                // Execute tool via MCPController
                var contents = await MCPController.ExecuteTool(funcName, args);

                // Convert result to string
                string resultText = string.Join("\n", contents.Select(c => c.Text ?? ""));

                // Create message with tool result
                toolResult = new ChatMessage("tool", resultText);
            } catch (Exception ex) {
                // In case of error, create message with error description
                toolResult = new ChatMessage("tool", $"Error executing tool '{funcName}': {ex.Message}");
            }

            if (toolResult != null) {
                toolResults.Add(toolResult);
                // Add tool results to history
                await ProcessMessage(toolResult);
            }
        }

        // If tools were called, send a new request with results
        if (toolResults.Count > 0) {
            fTokenSource.TryReset();

            // Create new request with tool results
            var request = CreateRequest(false);
            var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, fTokenSource.Token);
            response.EnsureSuccessStatusCode();

            var result = await response.Content.ReadFromJsonAsync<ChatResponse>(fTokenSource.Token);
            var finalMessage = result?.Choices?.FirstOrDefault()?.Message;
            // Add final assistant message to history
            await ProcessMessage(finalMessage);
        }
    }

    private ChatRequest CreateRequest(bool stream)
    {
        var request = new ChatRequest(Settings.ModelId, this.HistoryStorage.CurrentHistory, stream);
        request.Temperature = Settings.Temperature;
        request.TopP = Settings.TopP;
        request.PresencePenalty = Settings.PresencePenalty;
        request.FrequencyPenalty = Settings.FrequencyPenalty;
        request.MaxTokens = Settings.MaxTokens;
        request.Tools = fTools;

        return request;
    }

    public async Task ProcessMessage(ChatMessage message)
    {
        if (message == null) return;

        await HistoryStorage.AddToCurrentHistory(message);
        View?.ShowMessage(JsonSerializer.Serialize(message.Content), message.Role);
    }

    private async Task ProcessReceivedMessage(ChatMessage message)
    {
        // Add assistant's message to history (null if tool_calls are present)
        if (!string.IsNullOrEmpty(message.Content)) {
            await ProcessMessage(message);
        }

        // Check for tool calls
        if (message.ToolCalls != null && message.ToolCalls.Count > 0) {
            // Process tool calls
            await ProcessToolCallsAsync(message.ToolCalls);
        }
    }

    /// <summary>
    /// Single request (waiting for the model's complete response).
    /// </summary>
    public async Task<string> SendMessageSingleAsync(string role, string content, float temperature)
    {
        RequireHttp();

        var messages = new List<ChatMessage>();
        messages.Add(new ChatMessage(role, content));

        var request = new ChatRequest(Settings.ModelId, messages, false);
        request.Temperature = temperature;
        request.TopP = Settings.TopP;
        request.PresencePenalty = Settings.PresencePenalty;
        request.FrequencyPenalty = Settings.FrequencyPenalty;
        request.MaxTokens = Settings.MaxTokens;

        var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, fTokenSource.Token);
        response.EnsureSuccessStatusCode();

        var result = await response.Content.ReadFromJsonAsync<ChatResponse>(fTokenSource.Token);
        var message = result?.Choices?.FirstOrDefault()?.Message;
        if (message != null && !string.IsNullOrEmpty(message.Content)) {
            return message.Content;
        }

        return null;
    }

    /// <summary>
    /// Request.
    /// </summary>
    public async Task SendMessageAsync()
    {
        RequireHttp();

        fRequestId += 1;
        fTokenSource.TryReset();

        var request = CreateRequest(Settings.StreamMode);

        var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, fTokenSource.Token);
        response.EnsureSuccessStatusCode();

        if (!Settings.StreamMode) {
            // Regular request (waiting for the model's complete response).

            var result = await response.Content.ReadFromJsonAsync<ChatResponse>(fTokenSource.Token);
            var message = result?.Choices?.FirstOrDefault()?.Message;
            if (message != null)
                await ProcessReceivedMessage(message);
        } else {
            // Streaming request (streaming tokens as they are generated).

            string fullResponse = "";
            var accumulatedToolCalls = new Dictionary<int, ToolCall>();

            try {
                View?.StartStreamingMessage(fRequestId);

                var stream = await response.Content.ReadAsStreamAsync(fTokenSource.Token);
                var tokenStream = StreamTokensAndToolCallsAsync(stream, accumulatedToolCalls, fTokenSource.Token);
                await foreach (var item in tokenStream) {
                    if (item.IsToken) {
                        fullResponse += item.Content;
                        View?.UpdateStreamingMessage(fRequestId, fullResponse);
                    } else if (item.IsToolCall) {
                        // Tool calls are being accumulated in accumulatedToolCalls list
                        // We could display tool calls as they arrive if needed
                    }
                }
            } finally {
                // Add the complete assistant message to history
                await AddHistory("assistant", fullResponse);
                View?.FinalizeStreamingMessage(fRequestId);

                // Process tool calls if any were received
                if (accumulatedToolCalls.Count > 0) {
                    // Create a message containing the tool calls
                    var toolCallMessage = new ChatMessage("assistant", "");
                    toolCallMessage.ToolCalls = accumulatedToolCalls.Values.ToList();
                    await ProcessReceivedMessage(toolCallMessage);
                }
            }
        }
    }

    private static async IAsyncEnumerable<StreamItem> StreamTokensAndToolCallsAsync(
        Stream stream,
        Dictionary<int, ToolCall> accumulatedToolCalls,
        [EnumeratorCancellation] CancellationToken cancellationToken)
    {
        using var reader = new StreamReader(stream);
        while (!reader.EndOfStream) {
            cancellationToken.ThrowIfCancellationRequested();
            var line = await reader.ReadLineAsync(cancellationToken);

            if (string.IsNullOrWhiteSpace(line) || !line.StartsWith("data: "))
                continue;

            var jsonText = line["data: ".Length..].Trim();
            if (jsonText == "[DONE]")
                break;

            ChatStreamResponse chunk = null;
            try {
                chunk = JsonSerializer.Deserialize<ChatStreamResponse>(jsonText);
            } catch { /* Ignore incomplete lines */ }

            if (chunk?.Choices?.FirstOrDefault() is { } choice) {
                // Handle content tokens
                var content = choice.Delta?.Content;
                if (!string.IsNullOrEmpty(content)) {
                    yield return new StreamItem { IsToken = true, Content = content };
                }

                // Handle tool calls
                if (choice.Delta?.ToolCalls != null) {
                    foreach (var toolCallDelta in choice.Delta.ToolCalls) {
                        // Find or create the tool call in our accumulated list
                        ToolCall existingToolCall;
                        if (!accumulatedToolCalls.TryGetValue(toolCallDelta.Index, out existingToolCall)) {
                            existingToolCall = new ToolCall();
                            accumulatedToolCalls[toolCallDelta.Index] = existingToolCall;
                        }

                        // Update the tool call with the delta information
                        if (!string.IsNullOrEmpty(toolCallDelta.Id)) {
                            existingToolCall.Id = toolCallDelta.Id;
                        }
                        if (!string.IsNullOrEmpty(toolCallDelta.Type)) {
                            existingToolCall.Type = toolCallDelta.Type;
                        }
                        if (toolCallDelta.Function != null) {
                            if (existingToolCall.Function == null) {
                                existingToolCall.Function = new FunctionCall();
                            }
                            if (!string.IsNullOrEmpty(toolCallDelta.Function.Name)) {
                                existingToolCall.Function.Name = toolCallDelta.Function.Name;
                            }
                            if (!string.IsNullOrEmpty(toolCallDelta.Function.Arguments)) {
                                existingToolCall.Function.Arguments += toolCallDelta.Function.Arguments;
                            }
                        }

                        yield return new StreamItem { IsToolCall = true, ToolCall = existingToolCall };
                    }
                }
            }
        }
    }
}

internal class StreamItem
{
    public bool IsToken { get; set; }
    public string Content { get; set; }
    public bool IsToolCall { get; set; }
    public ToolCall ToolCall { get; set; }
}
