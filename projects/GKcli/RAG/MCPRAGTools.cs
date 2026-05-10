/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using GKcli.Features;
using GKcli.MCP;
using GKCore;

namespace GKcli.RAG;


internal class RAGSearchExamplesTool : BaseTool
{
    private static readonly JsonSerializerOptions fJsonOptions;

    static RAGSearchExamplesTool()
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

    public RAGSearchExamplesTool() : base("rag_search_examples") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Tool for searching for patterns of parsing old documents",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["input_text"] = new MCPToolProperty { Type = "string", Description = "Original census text (archaic spelling)" },
                    ["century"] = new MCPToolProperty { Type = "string", Description = "Century for pattern filtering: '17', '18', '19'" },
                    ["min_score"] = new MCPToolProperty { Type = "number", Description = "Minimum cosine similarity threshold (0.0–1.0)", Default = 0.6f },
                    ["top_k"] = new MCPToolProperty { Type = "integer", Description = "Maximum number of patterns to return", Default = 3 },
                },
                Required = new List<string> { "input_text" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string inputText = MCPHelper.GetRequiredStr(args, "input_text");
        string century = MCPHelper.GetOptionalStr(args, "century", null);
        double minScore = MCPHelper.GetOptionalDbl(args, "min_score", 0.6f); // ?!
        int topK = MCPHelper.GetOptionalInt(args, "top_k", 3);

        string result = RAGHelper.SearchExamples(inputText, century, topK);

        return MCPContent.CreateSimpleContent(result);
    }
}


internal class RAGWritePatternTool : BaseTool
{
    public RAGWritePatternTool() : base("rag_write_pattern") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Tool for writing a sample census text with a reference reading result",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["raw_text"] = new MCPToolProperty { Type = "string", Description = "Original census text (archaic spelling)" },
                    ["corrected_result"] = new MCPToolProperty { Type = "string", Description = "Ideal parsing result (JSON/structure)" },
                    ["century"] = new MCPToolProperty { Type = "string", Description = "Century: '17', '18', '19'" },
                    ["quality_score"] = new MCPToolProperty { Type = "number", Description = "User pattern quality rating" },
                },
                Required = new List<string> { "raw_text", "corrected_result", "century" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string rawText = MCPHelper.GetRequiredStr(args, "raw_text");
        string correctedResult = MCPHelper.GetRequiredStr(args, "corrected_result");
        string century = MCPHelper.GetOptionalStr(args, "century", null); // ?
        double qualityScore = MCPHelper.GetOptionalDbl(args, "quality_score", 0); // ?!

        RAGHelper.WritePattern(rawText, correctedResult, century);

        return MCPContent.CreateSimpleContent("✅ The pattern was written successfully.");
    }
}
