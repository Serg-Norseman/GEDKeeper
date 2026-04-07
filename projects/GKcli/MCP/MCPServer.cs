/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using GKCore;

namespace GKcli.MCP;

/// <summary>
/// Minimal MCP server that reads JSON-RPC 2.0 messages from stdin and writes responses to stdout.
/// No external packages — only System.Text.Json from .NET 8.
/// </summary>
internal class MCPServer
{
    private readonly BaseContext fContext;
    private readonly JsonSerializerOptions fJsonOptions;
    private readonly MCPToolkit fToolkit;

    public MCPServer()
    {
        Log("Initializing MCP server...");

        fContext = new BaseContext(null);
        Log("BaseContext created successfully");

        fToolkit = new MCPToolkit(fContext);

        fJsonOptions = new JsonSerializerOptions {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
        };
    }

    /// <summary>
    /// Main server loop: reads lines from stdin, processes JSON-RPC requests, writes responses to stdout.
    /// </summary>
    public void Run()
    {
        Log("MCP Server main loop started");

        var stdin = Console.In;
        string? line;
        while ((line = stdin.ReadLine()) != null) {
            line = line.Trim();
            if (string.IsNullOrEmpty(line))
                continue;

            try {
                Log($"Received: {line}");
                var request = JsonSerializer.Deserialize<MCPRequest>(line, fJsonOptions);

                // Notifications don't have an "id" and don't require a response
                bool isNotification = request?.Id == null || !request.Id.HasValue ||
                                      request.Id.Value.ValueKind == JsonValueKind.Null ||
                                      request.Id.Value.ValueKind == JsonValueKind.Undefined;

                if (isNotification && request?.Method == "notifications/initialized") {
                    Log("Client initialized notification received");
                    continue;
                }

                if (isNotification) {
                    Log($"Skipping notification: {request.Method}");
                    continue;
                }

                var response = ProcessRequest(request);
                var json = JsonSerializer.Serialize(response, fJsonOptions);
                Log($"Sending: {json}");
                Console.WriteLine(json);
                Console.Out.Flush();
            } catch (Exception ex) {
                Log($"Error processing request: {ex.Message}");
                var errorResponse = new MCPResponse {
                    Error = MCPError.InternalError(null, ex.Message)
                };
                var json = JsonSerializer.Serialize(errorResponse, fJsonOptions);
                Console.WriteLine(json);
                Console.Out.Flush();
            }
        }

        Log("MCP Server stopped (stdin closed)");
    }

    public static void Log(string message)
    {
        string line = $"[GKcli MCP] {message}";
        Logger.WriteInfo(line);

        Console.Error.WriteLine(line);
        Console.Error.Flush();
    }

    private MCPResponse ProcessRequest(MCPRequest? request)
    {
        if (request == null) {
            return new MCPResponse {
                Error = MCPError.InvalidParams(null, "Invalid request")
            };
        }

        var response = request.Method switch {
            "initialize" => HandleInitialize(request),
            "tools/list" => HandleToolsList(request),
            "tools/call" => HandleToolsCall(request),
            _ => new MCPResponse {
                Id = request.Id,
                Error = MCPError.MethodNotFound(request.Id)
            }
        };

        return response;
    }

    private MCPResponse HandleInitialize(MCPRequest request)
    {
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

    private MCPResponse HandleToolsList(MCPRequest request)
    {
        var result = fToolkit.GetToolsList();
        return new MCPResponse { Id = request.Id, Result = result };
    }

    private MCPResponse HandleToolsCall(MCPRequest request)
    {
        try {
            if (request.Params == null || request.Params.Value.ValueKind != JsonValueKind.Object) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams(request.Id, "Missing params")
                };
            }

            var p = request.Params.Value;
            if (!p.TryGetProperty("name", out var nameElem) || nameElem.ValueKind != JsonValueKind.String) {
                return new MCPResponse {
                    Id = request.Id,
                    Error = MCPError.InvalidParams(request.Id, "Missing tool name")
                };
            }

            string toolName = nameElem.GetString()!;
            p.TryGetProperty("arguments", out var arguments);

            var content = fToolkit.ExecuteTool(toolName, arguments);
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
}
