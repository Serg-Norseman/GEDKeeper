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
using GKCortex.MCP;
using GKCortex.Protocols;

namespace GKCortex.LMChat;

public class LMChatClient
{
    private readonly HttpClient fHttpClient;
    private readonly MCPServer fMCPServer;
    private string fModelId;
    private string fSystemPrompt;
    private List<ToolDef> fTools;

    public List<ChatMessage> History { get; private set; }

    public string ModelId
    {
        get { return fModelId; }
        set { fModelId = value; }
    }

    public string SystemPrompt
    {
        get { return fSystemPrompt; }
        set { fSystemPrompt = value; }
    }

    #region Model parameters

    public double Temperature { get; set; } = 0.7;
    public double TopP { get; set; } = 0.9;
    public double PresencePenalty { get; set; } = 0.0;
    public double FrequencyPenalty { get; set; } = 0.0;
    public int MaxTokens { get; set; } = 2048;
    public bool ReasoningMode { get; set; } = true;

    #endregion

    public LMChatClient(string baseUrl, string modelId, string apiKey = null, MCPServer mcpServer = null)
    {
        fHttpClient = new HttpClient { BaseAddress = new Uri(baseUrl) };
        fModelId = modelId;
        fSystemPrompt = "You are an advanced AI assistant for the GEDKeeper genealogy program. You help analyze databases and build family trees.";

        fMCPServer = mcpServer;

        if (!string.IsNullOrEmpty(apiKey))
            fHttpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", apiKey);

        fHttpClient.Timeout = Timeout.InfiniteTimeSpan;
        fHttpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        fHttpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("text/event-stream"));

        History = new List<ChatMessage>();

        var mcpTools = MCPController.GetTools();
        fTools = mcpTools.Select(mT => new ToolDef(mT)).ToList();
    }

    public void AddHistory(string role, string content)
    {
        History.Add(new ChatMessage(role, content));
    }

    public void AddHistory(ChatMessage message)
    {
        History.Add(message);
    }

    public async Task<List<ModelData>> LoadModelsAsync()
    {
        var response = await fHttpClient.GetAsync("v1/models");
        response.EnsureSuccessStatusCode();

        var data = await response.Content.ReadFromJsonAsync<ModelsResponse>();
        return data.Data;
    }

    private ChatRequest CreateRequest(bool stream)
    {
        var request = new ChatRequest(fModelId, this.History, stream);
        request.Temperature = Temperature;
        request.TopP = TopP;
        request.PresencePenalty = PresencePenalty;
        request.FrequencyPenalty = FrequencyPenalty;
        request.MaxTokens = MaxTokens;
        request.Tools = fTools;

        return request;
    }

    /// <summary>
    /// Regular request (waiting for the model's complete response)
    /// </summary>
    public async Task<string> SendMessageAsync(CancellationToken cancellationToken = default)
    {
        // Add system prompt to the beginning of the history if it's not there yet
        if (History.Count == 0) {
            AddHistory("system", fSystemPrompt);
        }

        var request = CreateRequest(false);
        var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, cancellationToken);
        response.EnsureSuccessStatusCode();

        var result = await response.Content.ReadFromJsonAsync<ChatResponse>(cancellationToken: cancellationToken);
        var message = result?.Choices?.FirstOrDefault()?.Message;

        if (message == null)
            return string.Empty;

        // Add assistant's message to history
        AddHistory(message);

        // Check for tool calls
        if (message.ToolCalls != null && message.ToolCalls.Count > 0) {
            // Process tool calls
            return await ProcessToolCallsAsync(message.ToolCalls, cancellationToken);
        }

        // Return text content
        return message.Content ?? string.Empty;
    }

    /// <summary>
    /// Process tool calls and return result
    /// </summary>
    private async Task<string> ProcessToolCallsAsync(List<ToolCall> toolCalls, CancellationToken cancellationToken)
    {
        // Create new message for tool results
        var toolResults = new List<ChatMessage>();

        foreach (var toolCall in toolCalls) {
            if (toolCall.Type == "function" && toolCall.Function != null) {
                try {
                    // Parse tool arguments
                    JsonElement args = JsonDocument.Parse(toolCall.Function.Arguments).RootElement;

                    // Execute tool via MCPController
                    List<MCPContent> contents = MCPController.ExecuteTool(toolCall.Function.Name, args);

                    // Convert result to string
                    string resultText = string.Join("\n", contents.Select(c => c.Text ?? ""));

                    // Create message with tool result
                    var toolResultMessage = new ChatMessage {
                        Role = "tool",
                        Content = resultText
                    };

                    toolResults.Add(toolResultMessage);
                } catch (Exception ex) {
                    // In case of error, create message with error description
                    var errorResultMessage = new ChatMessage {
                        Role = "tool",
                        Content = $"Error executing tool '{toolCall.Function.Name}': {ex.Message}"
                    };

                    toolResults.Add(errorResultMessage);
                }
            }
        }

        // Add tool results to history
        foreach (var toolResult in toolResults) {
            AddHistory(toolResult);
        }

        // If tools were called, send a new request with results
        if (toolResults.Count > 0) {
            // Create new request with tool results
            var request = CreateRequest(false);
            var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, cancellationToken);
            response.EnsureSuccessStatusCode();

            var result = await response.Content.ReadFromJsonAsync<ChatResponse>(cancellationToken: cancellationToken);
            var finalMessage = result?.Choices?.FirstOrDefault()?.Message;

            if (finalMessage != null) {
                // Add final assistant message to history
                AddHistory(finalMessage);
                return finalMessage.Content ?? string.Empty;
            }
        }

        return string.Empty;
    }

    /// <summary>
    /// Streaming request (streaming tokens as they are generated)
    /// </summary>
    public async Task<IAsyncEnumerable<string>> SendMessageStreamAsync(CancellationToken cancellationToken = default)
    {
        // Add system prompt to the beginning of the history if it's not there yet
        if (History.Count == 0) {
            AddHistory("system", fSystemPrompt);
        }

        var request = CreateRequest(true);
        var response = await fHttpClient.PostAsJsonAsync("v1/chat/completions", request, cancellationToken);
        response.EnsureSuccessStatusCode();

        var stream = await response.Content.ReadAsStreamAsync(cancellationToken);
        return StreamTokensAsync(stream, cancellationToken);
    }

    private static async IAsyncEnumerable<string> StreamTokensAsync(Stream stream, [EnumeratorCancellation] CancellationToken cancellationToken)
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

            ChatStreamResponse? chunk = null;
            try {
                chunk = JsonSerializer.Deserialize<ChatStreamResponse>(jsonText);
            } catch { /* Ignore incomplete lines */ }

            var content = chunk?.Choices?.FirstOrDefault()?.Delta?.Content;
            if (!string.IsNullOrEmpty(content)) {
                yield return content;
            }
        }
    }
}
