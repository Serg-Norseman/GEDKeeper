/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using GKCore;

namespace GKcli.MCP;

internal delegate List<MCPContent> MCPToolHandler(JsonElement args);

internal class MCPSrvTool
{
    public MCPTool Tool { get; set; }
    public MCPToolHandler Handler { get; set; }
}

/// <summary>
/// Minimal MCP server that reads JSON-RPC 2.0 messages from stdin and writes responses to stdout.
/// No external packages — only System.Text.Json from .NET 8.
/// </summary>
internal class MCPServer
{
    private readonly BaseContext fContext;
    private readonly JsonSerializerOptions fJsonOptions;
    private readonly MCPToolkit fToolkit;
    private readonly Dictionary<string, MCPSrvTool> fTools;

    public MCPServer()
    {
        int pid = Process.GetCurrentProcess().Id;

        AppDomain.CurrentDomain.ProcessExit += (s, e) => {
            Log($"[EXIT] Process {pid} terminated. Code: {Environment.ExitCode}\n");
        };

        using var cts = new CancellationTokenSource();
        Console.CancelKeyPress += (s, e) => {
            e.Cancel = true;
            cts.Cancel();
            Log($"[SIGINT] Interrupt signal received for PID: {pid}\n");
        };

        var utf8NoBom = new UTF8Encoding(false);
        // Important: disable buffering so that messages are sent instantly.
        Console.SetOut(new StreamWriter(Console.OpenStandardOutput(), utf8NoBom) { AutoFlush = true, NewLine = "\n" });

        Log($"Initializing MCP server ({pid})...");

        fContext = new BaseContext(null);
        Log("BaseContext created successfully");

        fTools = new Dictionary<string, MCPSrvTool>();
        fToolkit = new MCPToolkit(fContext, this);

        fJsonOptions = new JsonSerializerOptions {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
        };
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
            var stdin = Console.In;
            while (true) {
                // If line == null -> MCP client closed the stream.
                string line = await stdin.ReadLineAsync();
                if (line == null) break;

                try {
                    line = line.Trim();
                    if (string.IsNullOrEmpty(line)) continue;

                    Log($"Received: {line}");
                    var request = JsonSerializer.Deserialize<MCPRequest>(line, fJsonOptions);

                    if (IsNotificationRequest(request)) {
                        // If the MCP-Client sends a notification, it must not be responded to,
                        // otherwise it will violate the protocol and cause a session reset.
                        continue;
                    }

                    var response = ProcessRequest(request);
                    SendResponse(response, false);
                } catch (Exception ex) {
                    Log($"Error processing request: {ex.Message}");
                    var errorResponse = new MCPResponse {
                        Error = MCPError.InternalError(ex.Message)
                    };
                    SendResponse(errorResponse, true);
                }
            }
        } catch (Exception ex) {
            Log($"Run().Exception: {ex.Message}");
        }

        // Restore backup options.
        AppHost.Instance.SetRegularBackup(prevBackups);

        Log("MCP Server stopped");
    }

    private static bool IsNotificationRequest(MCPRequest request)
    {
        // Notifications don't have an "id" and don't require a response.
        return request?.Id == null || !request.Id.HasValue
            || request.Id.Value.ValueKind == JsonValueKind.Null || request.Id.Value.ValueKind == JsonValueKind.Undefined;
    }

    private async void SendResponse(MCPResponse response, bool isError)
    {
        var json = JsonSerializer.Serialize(response, fJsonOptions);

#if RELEASE
        if (!isError) {
            string jsonStr = json;
            if (jsonStr.Length > 1000) {
                jsonStr = jsonStr.Substring(0, 1000);
            }
            Log($"Sending: {jsonStr}");
        }
#endif

        // It definitely doesn't work with `Content-Length` and `WriteAsync()`.
        await Console.Out.WriteLineAsync(json);
        await Console.Out.FlushAsync();
    }

    public static void Log(string message)
    {
        string line = $"[GKcli MCP] {message}";
        Logger.WriteInfo(line);

        //Console.Error.WriteLine(line);
        //Console.Error.Flush();
    }

    private MCPResponse ProcessRequest(MCPRequest request)
    {
        if (request == null) {
            return new MCPResponse {
                Error = MCPError.InvalidParams("Invalid request")
            };
        }

        var response = request.Method switch {
            "initialize" => HandleInitialize(request),
            "tools/list" => HandleToolsList(request),
            "tools/call" => HandleToolsCall(request),
            _ => new MCPResponse {
                Id = request.Id,
                Error = MCPError.MethodNotFound()
            }
        };

        return response;
    }

    private MCPResponse HandleInitialize(MCPRequest request)
    {
        // Actual protocol version = "2025-11-25".
        // FIXME: The version is not simply "specified" - it is negotiated
        // during the initialization process between the client and the server!

        var result = new MCPInitializeResult {
            ProtocolVersion = "2024-11-05",
            Capabilities = new MCPCapabilities {
                Tools = new MCPToolsCapability { ListChanged = false }
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
        var result = new MCPToolsListResult();
        result.Tools = fTools.Values.Select(it => it.Tool).ToList();
        return new MCPResponse { Id = request.Id, Result = result };
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

            var content = ExecuteTool(toolName, arguments);

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
    /// Execute an MCP tool call by name and arguments.
    /// </summary>
    private List<MCPContent> ExecuteTool(string toolName, JsonElement? arguments)
    {
        if (fTools.TryGetValue(toolName, out MCPSrvTool internalTool)) {
            var args = arguments?.ValueKind == JsonValueKind.Object ? arguments.Value : default;

            return internalTool.Handler(args);
        } else {
            throw new ArgumentException($"Unknown tool: {toolName}");
        }
    }

    internal void RegisterTool(MCPTool tool, MCPToolHandler handler)
    {
        var intTool = new MCPSrvTool() { Tool = tool, Handler = handler };
        fTools.Add(tool.Name, intTool);
    }
}
