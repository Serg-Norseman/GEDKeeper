/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Concurrent;
using System.IO;
using System.Net;
using System.Text;
using System.Threading.Channels;
using System.Threading.Tasks;
using GKCore;
using GKCortex.MCP;

namespace GKMCPPlugin;

public class MCPServerController
{
    // Channel storage for sending messages to an SSE stream
    private readonly ConcurrentDictionary<string, Channel<string>> fActiveSessions = new();
    private bool fIsRunning;
    private bool fIsVerboseLogging;
    private HttpListener fListener;
    private readonly MCPServer fMCPServer;

    public bool IsRunning { get { return fIsRunning; } }

    public MCPServerController(MCPServer mcpServer)
    {
        fMCPServer = mcpServer ?? throw new ArgumentNullException(nameof(mcpServer));
    }

    public async Task StartAsync(string host, int port, bool enableCors, string allowedHosts, bool verboseLogging)
    {
        fIsVerboseLogging = verboseLogging;

        string listenHost = host == "localhost" ? "127.0.0.1" : host;
        fListener = new HttpListener();
        fListener.Prefixes.Add($"http://{listenHost}:{port}/mcp/");
        fListener.Start();

        fIsRunning = true;
        Log($"MCP server is running on http://{listenHost}:{port}/mcp/");

        _ = Task.Run(async () => {
            while (fIsRunning && fListener.IsListening) {
                try {
                    var context = await fListener.GetContextAsync();
                    _ = Task.Run(() => HandleIncomingRequestAsync(context, enableCors, allowedHosts));
                } catch (HttpListenerException) when (!fIsRunning) {
                    // Standard server shutdown; ignoring the exception
                } catch (Exception ex) {
                    Log($"❌ Error in the listening loop: {ex.Message}");
                }
            }
        });
    }

    private async Task HandleIncomingRequestAsync(HttpListenerContext context, bool enableCors, string allowedHosts)
    {
        var request = context.Request;
        var response = context.Response;

        if (enableCors) {
            string? origin = request.Headers["Origin"];
            if (!string.IsNullOrEmpty(origin) && allowedHosts.Contains(origin)) {
                response.Headers.Add("Access-Control-Allow-Origin", origin);
            } else if (string.IsNullOrEmpty(allowedHosts) || allowedHosts == "*") {
                response.Headers.Add("Access-Control-Allow-Origin", "*");
            }
            response.Headers.Add("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
            response.Headers.Add("Access-Control-Allow-Headers", "Content-Type, Authorization");

            // If it is a preflight request from the browser or LM Studio/Jan, respond with OK immediately.
            if (request.HttpMethod == "OPTIONS") {
                response.StatusCode = (int)HttpStatusCode.OK;
                response.Close();
                return;
            }
        }

        string sessionId = request.QueryString["sessionId"];
        try {
            // --- GET Processing (SSE Stream) ---
            if (request.HttpMethod == "GET") {
                sessionId ??= Guid.NewGuid().ToString("N");

                response.ContentType = "text/event-stream";
                response.Headers.Add("Cache-Control", "no-cache");
                response.Headers.Add("Connection", "keep-alive");

                var channel = Channel.CreateUnbounded<string>(new UnboundedChannelOptions {
                    SingleWriter = true,
                    SingleReader = true
                });
                fActiveSessions[sessionId] = channel;

                using var writer = new StreamWriter(response.OutputStream, new UTF8Encoding(false));

                try {
                    // Instantly send the client their endpoint in SSE format
                    await writer.WriteAsync($"event: endpoint\ndata: /mcp/?sessionId={sessionId}\n\n");
                    await writer.FlushAsync();

                    // Read messages from the channel and write to the network stream
                    await foreach (var message in channel.Reader.ReadAllAsync()) {
                        if (!fIsRunning) break;
                        await writer.WriteAsync(message);
                        await writer.FlushAsync();
                    }
                } catch (Exception ex) {
                    if (fIsVerboseLogging) Log($"ℹ️ The client disconnected or an SSE error occurred: {ex.Message}");
                } finally {
                    fActiveSessions.TryRemove(sessionId, out _);
                    channel.Writer.TryComplete();
                    response.Close();
                }
            }
            // --- POST Processing (JSON-RPC commands) ---
            else if (request.HttpMethod == "POST") {
                using var reader = new StreamReader(request.InputStream, Encoding.UTF8);
                string jsonRpcRequest = await reader.ReadToEndAsync();

                if (fIsVerboseLogging) {
                    Log($"POST received: {jsonRpcRequest[..Math.Min(150, jsonRpcRequest.Length)]}...");
                }

                string jsonRpcResponse = fMCPServer.ProcessSSERequest(jsonRpcRequest);
                bool isNotification = !jsonRpcRequest.Contains("\"id\"");

                if (!string.IsNullOrEmpty(jsonRpcResponse)) {
                    string formatted = $"data: {jsonRpcResponse.Replace("\r", "").Replace("\n", "")}\n\n";

                    if (!string.IsNullOrEmpty(sessionId) && fActiveSessions.TryGetValue(sessionId, out var channel)) {
                        await channel.Writer.WriteAsync(formatted);
                    } else if (!isNotification) {
                        Log($"⚠️ Response generated, but no active SSE session found for sessionId={sessionId}");
                    }
                    response.StatusCode = (int)HttpStatusCode.Accepted; // 202 Accepted
                } else {
                    response.StatusCode = (int)HttpStatusCode.NoContent; // 204 No Content
                }
                response.Close();
            } else {
                response.StatusCode = (int)HttpStatusCode.MethodNotAllowed;
                response.Close();
            }
        } catch (Exception ex) {
            Log($"❌ Error processing MCP request: {ex.Message}");
            response.StatusCode = (int)HttpStatusCode.InternalServerError;
            response.Close();
        }
    }

    public async Task StopAsync()
    {
        fIsRunning = false;
        if (fListener != null && fListener.IsListening) {
            Log("Stopping the MCP server...");

            foreach (var session in fActiveSessions.Values) {
                session.Writer.TryComplete();
            }
            fActiveSessions.Clear();

            fListener.Stop();
            fListener.Close();
        }
        await Task.CompletedTask;
    }

    private void Log(string message)
    {
        string line = $"[GKMCP] {message}";
        Logger.WriteInfo(line);
    }
}
