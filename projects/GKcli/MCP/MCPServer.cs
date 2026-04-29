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
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using System.Threading.Tasks;
using GKcli.Features;
using GKCore;

namespace GKcli.MCP;

/// <summary>
/// Minimal MCP server that reads JSON-RPC 2.0 messages from stdin and writes responses to stdout.
/// No external packages — only System.Text.Json from .NET 8.
/// </summary>
internal class MCPServer
{
    private readonly CancellationTokenSource fCancellationToken;
    private readonly JsonSerializerOptions fJsonOptions;
    private readonly MCPToolsListResult fToolsList;
    private readonly MCPResourcesListResult fResourcesList;
    private readonly MCPPromptsListResult fPromptsList;

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
        var commands = MCPController.GetTools();
        // Register tools from commands
        fToolsList = new MCPToolsListResult() {
            Tools = commands.Select(cmd => cmd.CreateTool()).Where(tl => tl != null).ToList()
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
        var prevBackups = AppHost.Instance.SetForcedBackup();

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

                    if (string.IsNullOrEmpty(line)) continue;

                    Log($"Received: {line}");
                    var request = JsonSerializer.Deserialize<MCPRequest>(line, fJsonOptions);

                    if (request == null) {
                        SendResponse(new MCPResponse {
                            Error = MCPError.InvalidParams("Invalid request")
                        });
                        continue;
                    }

                    if (IsNotificationRequest(request)) {
                        // If the MCP-Client sends a notification, it must not be responded to,
                        // otherwise it will violate the protocol and cause a session reset.
                        continue;
                    }

                    ProcessRequest(request);
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
            AppHost.Instance.SetRegularBackup(prevBackups);

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
        string line = $"[GKcli MCP] {message}";
        Logger.WriteInfo(line);

        //Console.Error.WriteLine(line);
        //Console.Error.Flush();
    }

    private void ProcessRequest(MCPRequest request)
    {
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

    private MCPResponse HandleInitialize(MCPRequest request)
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
                Name = "GKcli",
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
}
