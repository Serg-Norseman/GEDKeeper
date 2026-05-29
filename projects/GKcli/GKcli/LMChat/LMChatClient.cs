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
using GKcli.LMChat.OpenAI;

namespace GKcli.LMChat;

public class LMChatClient
{
    private readonly HttpClient _httpClient;
    private readonly string _modelId;

    public List<ChatMessage> History { get; private set; }

    public LMChatClient(string baseUrl, string modelId, string apiKey = null)
    {
        _httpClient = new HttpClient { BaseAddress = new Uri(baseUrl) };
        _modelId = modelId;

        if (!string.IsNullOrEmpty(apiKey))
            _httpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", apiKey);

        _httpClient.Timeout = Timeout.InfiniteTimeSpan;
        _httpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        _httpClient.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("text/event-stream"));

        History = new List<ChatMessage>();
    }

    public void AddHistory(string role, string content)
    {
        History.Add(new ChatMessage(role, content));
    }

    /// <summary>
    /// Regular request (waiting for the model's complete response)
    /// </summary>
    public async Task<string> SendMessageAsync(CancellationToken cancellationToken = default)
    {
        var request = new ChatRequest(_modelId, this.History, Stream: false);

        var response = await _httpClient.PostAsJsonAsync("v1/chat/completions", request, cancellationToken);
        response.EnsureSuccessStatusCode();

        var result = await response.Content.ReadFromJsonAsync<ChatResponse>(cancellationToken: cancellationToken);
        return result?.Choices?.FirstOrDefault()?.Message?.Content ?? string.Empty;
    }

    /// <summary>
    /// Streaming request (streaming tokens as they are generated)
    /// </summary>
    public async Task<IAsyncEnumerable<string>> SendMessageStreamAsync(CancellationToken cancellationToken = default)
    {
        var request = new ChatRequest(_modelId, this.History, Stream: true);
        var response = await _httpClient.PostAsJsonAsync("v1/chat/completions", request, cancellationToken);
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
