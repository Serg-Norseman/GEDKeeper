using System;
using System.Collections.Concurrent;
using System.IO;
using System.Text;
using System.Threading.Channels;
using System.Threading.Tasks;
using GKCore;
using GKCortex.Database;
using GKCortex.Features;
using GKCortex.MCP;
using GKUI.Platform;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;

namespace GKsse;

// Works: Jan (checked ok), LM Studio (fail!)
internal class Program
{
    // Channel storage for sending messages to an SSE stream
    private static readonly ConcurrentDictionary<string, Channel<string>> fActiveSessions = new();

    private static void Main(string[] args)
    {
        // TODO: refactoring!
        CLIAppHost.Startup(args);
        AppHost.InitSettings();
        AppHost.Instance.Init(args, false);
        LLMDatabase.SetAppDataPath(AppHost.GetAppDataPathStatic());
        MCPController.InitFeatures(embedded: false, pureMode: false, tdeMode: true, ragMode: true);
        Log("MCP Server started");
        AppHost.Instance.SetForcedBackup();


        var builder = WebApplication.CreateBuilder(args);
        builder.Services.AddSingleton<MCPServer>();
        var app = builder.Build();

        app.MapGet("/mcp", async (HttpContext context, string? sessionId, MCPServer mcpServer) => {
            sessionId ??= Guid.NewGuid().ToString("N");

            context.Response.ContentType = "text/event-stream";
            context.Response.Headers.CacheControl = "no-cache";
            context.Response.Headers.Connection = "keep-alive";
            context.Response.Headers.AcceptEncoding = "identity";

            var channel = Channel.CreateUnbounded<string>(new UnboundedChannelOptions {
                SingleWriter = true,
                SingleReader = true
            });
            fActiveSessions[sessionId] = channel;

            try {
                // Send the endpoint immediately
                await context.Response.WriteAsync($"event: endpoint\ndata: /mcp?sessionId={sessionId}\n\n", context.RequestAborted);
                await context.Response.Body.FlushAsync(context.RequestAborted);

                // Read from the channel and write to the response stream.
                await foreach (var message in channel.Reader.ReadAllAsync(context.RequestAborted)) {
                    await context.Response.WriteAsync(message, context.RequestAborted);
                    await context.Response.Body.FlushAsync(context.RequestAborted);
                }
            } catch (OperationCanceledException) {
                // Routine client shutdown
            } catch (Exception ex) {
                Log($"❌ SSE error: {ex.Message}");
            } finally {
                fActiveSessions.TryRemove(sessionId, out _);
                channel.Writer.TryComplete();
            }

            return Results.Empty;
        });

        // POST /mcp — receiving JSON-RPC commands from the client
        app.MapPost("/mcp", async (HttpContext context, string? sessionId, MCPServer mcpServer) => {
            using var reader = new StreamReader(context.Request.Body, Encoding.UTF8);
            string jsonRpcRequest = await reader.ReadToEndAsync();

            Log($"POST received: {jsonRpcRequest[..Math.Min(150, jsonRpcRequest.Length)]}...");

            try {
                string? jsonRpcResponse = await mcpServer.ProcessSSERequestAsync(jsonRpcRequest);

                // Check if the request is a "notification" (no "id" field)
                bool isNotification = !jsonRpcRequest.Contains("\"id\"");

                if (!string.IsNullOrEmpty(jsonRpcResponse)) {
                    // Format the response in SSE format
                    string formatted = $"data: {jsonRpcResponse.Replace("\r", "").Replace("\n", "")}\n\n";

                    // If there is an active SSE session, send it to the channel
                    if (!string.IsNullOrEmpty(sessionId) &&
                        fActiveSessions.TryGetValue(sessionId, out var channel)) {
                        await channel.Writer.WriteAsync(formatted);
                    } else if (!isNotification) {
                        // If there is no session, but this is a request (not a notification), we log a warning
                        Log($"⚠️ Response generated but no active SSE session for sessionId={sessionId}");
                    }

                    // - 202 Accepted for queries (the response will come via the SSE channel)
                    return Results.Accepted();
                } else {
                    // - 204 No Content for notifications (do not require a response)
                    return Results.NoContent();
                }
            } catch (Exception ex) {
                Log($"❌ Error processing MCP request: {ex.Message}");

                if (jsonRpcRequest.Contains("\"id\"")) {
                    // TODO: Try to extract the id for the correct error response.
                    string errorResponse = """data: {"jsonrpc":"2.0","id":null,"error":{"code":-32603,"message":"Internal error"}}""";

                    if (!string.IsNullOrEmpty(sessionId) && fActiveSessions.TryGetValue(sessionId, out var channel)) {
                        await channel.Writer.WriteAsync(errorResponse);
                    }
                }
                return Results.StatusCode(500);
            }
        });

        app.Run("http://localhost:5001");
    }

    private static async Task SendMessageAsync(Channel<string> channel, string content)
    {
        if (!string.IsNullOrEmpty(content)) {
            string formatted = $"data: {content.Replace("\r", "").Replace("\n", "")}\n\n";
            await channel.Writer.WriteAsync(formatted);
        }
    }

    public static void Log(string message)
    {
        string line = $"[GKsse MCP] {message}";
        Logger.WriteInfo(line);
    }
}
