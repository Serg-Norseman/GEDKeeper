/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using GKCore;
using GKCortex.Features;
using GKCortex.Protocols;

namespace GKCortex.MCP;

/// <summary>
/// Minimal MCP server that reads JSON-RPC 2.0 messages from stdin and writes responses to stdout.
/// No external packages — only System.Text.Json from .NET 8.
/// </summary>
public class MCPServer
{
    private readonly CancellationTokenSource fCancellationToken;
    private readonly JsonSerializerOptions fJsonOptions;
    private readonly MCPToolsListResult fToolsList;
    private readonly MCPResourcesListResult fResourcesList;
    private readonly MCPPromptsListResult fPromptsList;
    private bool fIsRunning;
    private bool fIsVerboseLogging;

    public bool IsRunning { get { return fIsRunning; } }


    public MCPServer()
    {
        int pid = Environment.ProcessId;
        fCancellationToken = new CancellationTokenSource();

        // Jan, LM Studio clients terminate the process in such a way
        // that this handlers is not called.
        AppDomain.CurrentDomain.ProcessExit += (s, e) => {
            Log($"[EXIT] Process {pid} terminated. Code: {Environment.ExitCode}\n");
        };
        Console.CancelKeyPress += (s, e) => {
            e.Cancel = true;
            fCancellationToken.Cancel();
            Log($"[SIGINT] Interrupt signal received for PID: {pid}\n");
        };

        Task.Run(async () => {
            while (!fCancellationToken.IsCancellationRequested) {
                Log($"Keep-alive tick process {pid}");
                await Task.Delay(30000);
            }
            Log("Cancelled");
        });

        var utf8NoBom = new UTF8Encoding(false);
        // Important: disable buffering so that messages are sent instantly.
        Console.SetOut(new StreamWriter(Console.OpenStandardOutput(), utf8NoBom) { AutoFlush = true, NewLine = "\n" });

        Log($"Initializing MCP server ({pid})...");

        // The list is generated after registration.
        fToolsList = new MCPToolsListResult() {
            Tools = MCPController.GetTools()
        };

        // Initialize resources
        fResourcesList = new MCPResourcesListResult() {
            Resources = MCPController.GetResources().Select(x => x.CreateResource()).ToList()
        };

        // Initialize prompts (minimal stub)
        fPromptsList = new MCPPromptsListResult() {
            Prompts = new List<MCPPrompt>()
        };

        fJsonOptions = new JsonSerializerOptions {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        };
        fJsonOptions.Converters.Add(
            new JsonStringEnumConverter(JsonNamingPolicy.CamelCase)
        );
    }

    /// <summary>
    /// Main server loop: reads lines from stdin, processes JSON-RPC requests, writes responses to stdout.
    /// </summary>
    public async void Run()
    {
        Log("MCP Server started");

        // Since using such tools is a risky process,
        // we force backup of each file revision (each save).
        var prevBackups = AppHost.SetForcedBackup();

        try {
            int nullsBeforeExit = 0;
            var stdin = Console.In;
            while (!fCancellationToken.IsCancellationRequested) {
                try {
                    // If line == null -> MCP client closed the stream.
                    string line = await stdin.ReadLineAsync();
                    if (line == null) {
                        // 10 attempts to check for a possible failure,
                        // although if the stream is already closed, it's pointless.
                        nullsBeforeExit++;
                        if (nullsBeforeExit >= 10) break;
                    }

                    ProcessRequest(line);
                } catch (Exception ex) {
                    Log($"Error processing request: {ex.Message}");
                    SendResponse(new MCPResponse {
                        Error = MCPError.InternalError(ex.Message)
                    });
                }
            }
        } catch (Exception ex) {
            Log($"Run().Exception: {ex.Message}");
        } finally {
            // Restore backup options.
            AppHost.SetRegularBackup(prevBackups);

            Log("MCP Server stopped");
        }
    }

    private static bool IsNotificationRequest(MCPRequest request)
    {
        // Notifications don't have an "id" and don't require a response.
        return request?.Id == null || !request.Id.HasValue
            || request.Id.Value.ValueKind == JsonValueKind.Null || request.Id.Value.ValueKind == JsonValueKind.Undefined;
    }

    private async void SendResponse(MCPResponse response)
    {
        var json = JsonSerializer.Serialize(response, fJsonOptions);

        // It definitely doesn't work with `Content-Length` and `WriteAsync()`.
        await Console.Out.WriteLineAsync(json);
        await Console.Out.FlushAsync();

#if DEBUG
        /*string jsonStr = json;
        if (jsonStr.Length > 200) {
            jsonStr = jsonStr.Substring(0, 200);
        }
        Log($"Sending: {jsonStr}");*/
#endif
    }

    public static void Log(string message)
    {
        string line = $"[GKCortex MCP] {message}";
        Logger.WriteInfo(line);

        //Console.Error.WriteLine(line);
        //Console.Error.Flush();
    }

    private void ProcessRequest(string line)
    {
        if (string.IsNullOrEmpty(line)) return;

        Log($"Received: {line}");
        var request = JsonSerializer.Deserialize<MCPRequest>(line, fJsonOptions);

        if (request == null) {
            SendResponse(new MCPResponse {
                Error = MCPError.InvalidParams("Invalid request")
            });
            return;
        }

        if (IsNotificationRequest(request)) {
            // If the MCP-Client sends a notification, it must not be responded to,
            // otherwise it will violate the protocol and cause a session reset.
            return;
        }

        var response = request.Method switch {
            "initialize" => HandleInitialize(request),
            "tools/list" => HandleToolsList(request),
            "tools/call" => HandleToolsCall(request),
            "resources/list" => HandleResourcesList(request),
            "resources/templates/list" => HandleResourceTemplatesList(request),
            "resources/read" => HandleResourceRead(request),
            "prompts/list" => HandlePromptsList(request),
            "prompts/get" => HandlePromptsGet(request),
            _ => new MCPResponse {
                Id = request.Id,
                Error = MCPError.MethodNotFound()
            }
        };

        SendResponse(response);
    }

    public string ProcessSSERequest(string line)
    {
        if (string.IsNullOrEmpty(line)) return string.Empty;

        Log($"Received: {line}");
        var request = JsonSerializer.Deserialize<MCPRequest>(line, fJsonOptions);

        if (request == null) {
            //TODO
            /*SendResponse(new MCPResponse {
                Error = MCPError.InvalidParams("Invalid request")
            });*/
            return string.Empty;
        }

        if (IsNotificationRequest(request)) {
            // If the MCP-Client sends a notification, it must not be responded to,
            // otherwise it will violate the protocol and cause a session reset.
            return string.Empty;
        }

        var response = request.Method switch {
            "initialize" => HandleInitialize(request),
            "tools/list" => HandleToolsList(request),
            "tools/call" => HandleToolsCall(request),
            "resources/list" => HandleResourcesList(request),
            "resources/templates/list" => HandleResourceTemplatesList(request),
            "resources/read" => HandleResourceRead(request),
            "prompts/list" => HandlePromptsList(request),
            "prompts/get" => HandlePromptsGet(request),
            _ => new MCPResponse {
                Id = request.Id,
                Error = MCPError.MethodNotFound()
            }
        };

        var json = JsonSerializer.Serialize(response, fJsonOptions);
        return json;
    }

    private static MCPResponse HandleInitialize(MCPRequest request)
    {
        // Actual protocol version = "2025-11-25".
        // FIXME: The version is not simply "specified" - it is negotiated
        // during the initialization process between the client and the server!

        var result = new MCPInitializeResult {
            ProtocolVersion = "2025-11-25", //"2025-06-18", //"2024-11-05",
            Capabilities = new MCPCapabilities {
                Tools = new MCPToolsCapability { ListChanged = false },
                Resources = new MCPResourcesCapability { Subscribe = false, ListChanged = false },
                Prompts = new MCPPromptsCapability { ListChanged = false }
            },
            ServerInfo = new MCPServerInfo {
                Name = "GKCortex",
                Version = "1.0.0"
            }
        };

        return new MCPResponse { Id = request.Id, Result = result };
    }

    /// <summary>
    /// Returns the list of available MCP tools.
    /// </summary>
    private MCPResponse HandleToolsList(MCPRequest request)
    {
        // Some clients (like Jan) re-request the list of tools very frequently,
        // so it's better to have a cached list in advance.
        return new MCPResponse { Id = request.Id, Result = fToolsList };
    }

    private MCPResponse HandleToolsCall(MCPRequest request)
    {
        try {
            if (request.Params == null || request.Params.Value.ValueKind != JsonValueKind.Object) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing params")
                };
            }

            var p = request.Params.Value;
            if (!p.TryGetProperty("name", out var nameElem) || nameElem.ValueKind != JsonValueKind.String) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing tool name")
                };
            }

            string toolName = nameElem.GetString()!;
            p.TryGetProperty("arguments", out var arguments);

            // Execute an MCP tool call by name and arguments.
            var content = MCPController.ExecuteTool(toolName, arguments);

            return new MCPResponse {
                Id = request.Id,
                Result = new { Content = content, IsError = false }
            };
        } catch (Exception ex) {
            return new MCPResponse {
                Id = request.Id,
                Result = new {
                    Content = new List<MCPContent> { new MCPContent { Text = ex.Message } },
                    IsError = true
                }
            };
        }
    }

    /// <summary>
    /// Returns the list of available resources.
    /// </summary>
    private MCPResponse HandleResourcesList(MCPRequest request)
    {
        // Minimal stub: return empty list
        return new MCPResponse { Id = request.Id, Result = fResourcesList };
    }

    /// <summary>
    /// Returns the list of resource templates.
    /// </summary>
    private MCPResponse HandleResourceTemplatesList(MCPRequest request)
    {
        // Minimal stub: return empty list
        return new MCPResponse {
            Id = request.Id,
            Result = new MCPResourceTemplatesListResult {
                ResourceTemplates = new List<MCPResourceTemplate>()
            }
        };
    }

    /// <summary>
    /// Reads the content of a specific resource by URI.
    /// </summary>
    private MCPResponse HandleResourceRead(MCPRequest request)
    {
        try {
            if (request.Params == null || request.Params.Value.ValueKind != JsonValueKind.Object) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing params")
                };
            }

            var p = request.Params.Value;
            if (!p.TryGetProperty("uri", out var uriElem) || uriElem.ValueKind != JsonValueKind.String) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing uri")
                };
            }

            string uri = uriElem.GetString()!;

            // Match registered resources
            var resContent = MCPController.GetResource(uri);
            if (resContent != null) {
                Logger.WriteInfo($"Returned contents for resource {uri}");

                return new MCPResponse {
                    Id = request.Id,
                    Result = new {
                        Contents = resContent,
                    }
                };
            }

            // Minimal stub: resource not found
            return new MCPResponse {
                Id = request.Id,
                Error = MCPError.InvalidParams($"Resource not found: {uri}")
            };
        } catch (Exception ex) {
            return new MCPResponse {
                Id = request.Id,
                Error = MCPError.InternalError(ex.Message)
            };
        }
    }

    /// <summary>
    /// Returns the list of available prompts.
    /// </summary>
    private MCPResponse HandlePromptsList(MCPRequest request)
    {
        // Minimal stub: return empty list
        return new MCPResponse { Id = request.Id, Result = fPromptsList };
    }

    /// <summary>
    /// Gets a specific prompt with arguments resolved.
    /// </summary>
    private MCPResponse HandlePromptsGet(MCPRequest request)
    {
        try {
            if (request.Params == null || request.Params.Value.ValueKind != JsonValueKind.Object) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing params")
                };
            }

            var p = request.Params.Value;
            if (!p.TryGetProperty("name", out var nameElem) || nameElem.ValueKind != JsonValueKind.String) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams("Missing name")
                };
            }

            string name = nameElem.GetString()!;

            // Minimal stub: prompt not found
            return new MCPResponse {
                Id = request.Id,
                Error = MCPError.InvalidParams($"Prompt not found: {name}")
            };
        } catch (Exception ex) {
            return new MCPResponse {
                Id = request.Id,
                Error = MCPError.InternalError(ex.Message)
            };
        }
    }

    #region Streamable Http

    // Channel storage for sending messages to an SSE stream
    private readonly ConcurrentDictionary<string, Channel<string>> fActiveSessions = new();
    private HttpListener fListener;

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

                string jsonRpcResponse = ProcessSSERequest(jsonRpcRequest);
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

    #endregion
}
