/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKcli.Features;
using GKcli.MCP;
using GKCore;

namespace GKcli.Memory;


internal class GetContextSummaryTool : BaseTool
{
    public GetContextSummaryTool() : base("get_context_summary") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Retrieve the session context summary containing global history and current session accumulation",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["session_id"] = new MCPToolProperty { Type = "string", Description = "The unique identifier of the user session" }
                },
                Required = new List<string> { "session_id" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = true,
                DestructiveHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sessionId = MCPHelper.GetRequiredStr(args, "session_id");

        var service = new MemoryService();
        var summary = service.GetInjectedContextAsync(sessionId).GetAwaiter().GetResult();

        if (summary == null)
            return MCPContent.CreateSimpleContent("❌ History is empty. This is the first dialogue with the user.");

        return MCPContent.CreateSimpleContent(summary);
    }
}


internal class SaveChatMilestoneTool : BaseTool
{
    public SaveChatMilestoneTool() : base("save_chat_milestone") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Force-save an important dialogue milestone to the long-term session log",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["session_id"] = new MCPToolProperty { Type = "string", Description = "The unique identifier of the user session" },
                    ["user_line"] = new MCPToolProperty { Type = "string", Description = "The user's message to be recorded" },
                    ["assistant_line"] = new MCPToolProperty { Type = "string", Description = "The assistant's response to be recorded" }
                },
                Required = new List<string> { "session_id", "user_line", "assistant_line" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sessionId = MCPHelper.GetRequiredStr(args, "session_id");
        string userLine = MCPHelper.GetRequiredStr(args, "user_line");
        string assistantLine = MCPHelper.GetRequiredStr(args, "assistant_line");

        var service = new MemoryService();
        // Note: Consider async-over-sync pattern or background queue for production use
        service.AppendAndOptimizeContextAsync(sessionId, userLine, assistantLine).GetAwaiter().GetResult();

        return MCPContent.CreateSimpleContent("✅ Interaction successfully recorded to the long-term session log.");
    }
}
