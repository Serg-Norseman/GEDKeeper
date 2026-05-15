/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;
using GKcli.Features;
using GKCore;

namespace GKcli.MCP;


internal class SearchTool : BaseTool
{
    private static readonly JsonSerializerOptions fJsonOptions;

    static SearchTool()
    {
        fJsonOptions = new JsonSerializerOptions {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        };
        fJsonOptions.Converters.Add(
            new JsonStringEnumConverter(JsonNamingPolicy.CamelCase)
        );
    }

    public SearchTool() : base("search_tool") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Search available tools by keywords (English only, do not mix tools with different purposes in one query)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["query"] = new MCPToolProperty { Type = "string", Description = "Keyword query string" },
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
        var found = MCPToolDiscovery.Search(query);

        var result = JsonSerializer.Serialize(new {
            message = $"Tools found: {found.Count()}",
            tools = found
        }, fJsonOptions);

        return MCPContent.CreateSimpleContent(result);
    }
}


internal class UseTool : BaseTool
{
    public UseTool() : base("use_tool") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Calling a tool by name",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["tool_name"] = new MCPToolProperty { Type = "string", Description = "Tool name" },
                    ["arguments"] = new MCPToolProperty { Type = "object", Description = "Tool call arguments" }
                },
                Required = new List<string> { "tool_name", "arguments" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string toolName = MCPHelper.GetRequiredStr(args, "tool_name");
        var arguments = MCPHelper.GetRequiredObj(args, "arguments");

        return MCPController.ExecuteTool(toolName, arguments);
    }
}
