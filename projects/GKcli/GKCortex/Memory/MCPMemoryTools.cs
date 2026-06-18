/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKCore;
using GKCortex.Features;
using GKCortex.MCP;
using GKCortex.Protocols;

namespace GKCortex.Memory;


internal class StoreFactTool : BaseTool
{
    public StoreFactTool() : base("store_fact") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Store a new fact in the long-term memory",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["fact"] = new MCPToolProperty { Type = "string", Description = "The fact to be stored in memory" }
                },
                Required = new List<string> { "fact" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string fact = MCPHelper.GetRequiredStr(args, "fact");

        MemoryService.StoreFact(fact).GetAwaiter().GetResult();

        return MCPContent.CreateSimpleContent("✅ The fact was successfully stored in memory.");
    }
}


internal class SearchMemoryTool : BaseTool
{
    public SearchMemoryTool() : base("search_memory") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Search the long-term memory using semantic similarity",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["query"] = new MCPToolProperty { Type = "string", Description = "The search query for finding relevant memories" },
                    ["top_k"] = new MCPToolProperty { Type = "integer", Description = "Maximum number of results to return", Default = 5 }
                },
                Required = new List<string> { "query" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = true,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string query = MCPHelper.GetRequiredStr(args, "query");
        int topK = MCPHelper.GetOptionalInt(args, "top_k", 5);

        var results = MemoryService.SearchMemory(query, topK).GetAwaiter().GetResult();

        return MCPContent.CreateSimpleContent(results);
    }
}
