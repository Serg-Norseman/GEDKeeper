/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;
using GKCore;
using GKCortex.Features;
using GKCortex.MCP;
using GKCortex.Protocols;

namespace GKCortex.Memory;

internal class GetKnowledgeSubgraphTool : BaseTool
{
    public GetKnowledgeSubgraphTool() : base("get_knowledge_subgraph") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Retrieve the local knowledge subgraph centered around a specific entity",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["entity_id"] = new MCPToolProperty { Type = "string", Description = "The unique identifier of the entity" }
                },
                Required = new List<string> { "entity_id" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = true,
            }
        };
    }

    public override async Task<List<MCPContent>> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string entityId = MCPHelper.GetRequiredStr(args, "entity_id");

        var service = new MemoryService();
        string result = await service.GetLocalSubGraphAsTextAsync(entityId);

        return MCPContent.CreateSimpleContent(result);
    }
}


internal class AddKnowledgeNodeTool : BaseTool
{
    public AddKnowledgeNodeTool() : base("add_knowledge_node") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add or update a knowledge node (entity) in the graph",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["entity_id"] = new MCPToolProperty { Type = "string", Description = "Unique identifier for the entity" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Human-readable name of the entity" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Entity type/category (e.g., Person, Location, Event)" },
                    ["description"] = new MCPToolProperty { Type = "string", Description = "Detailed description of the entity" }
                },
                Required = new List<string> { "entity_id", "name", "type", "description" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = false,
            }
        };
    }

    public override async Task<List<MCPContent>> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string entityId = MCPHelper.GetRequiredStr(args, "entity_id");
        string name = MCPHelper.GetRequiredStr(args, "name");
        string type = MCPHelper.GetRequiredStr(args, "type");
        string description = MCPHelper.GetRequiredStr(args, "description");

        var service = new MemoryService();
        await service.AddEntityAsync(entityId, name, type, description);

        return MCPContent.CreateSimpleContent($"✅ Node '{name}' [{type.ToUpperInvariant()}] successfully recorded in the knowledge graph.");
    }
}


internal class ConnectKnowledgeNodesTool : BaseTool
{
    public ConnectKnowledgeNodesTool() : base("connect_knowledge_nodes") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Create a semantic relationship between two knowledge nodes with contextual notes",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["source_id"] = new MCPToolProperty { Type = "string", Description = "ID of the source entity" },
                    ["predicate"] = new MCPToolProperty { Type = "string", Description = "Relationship type (e.g., 'born_in', 'parent_of', 'worked_at')" },
                    ["target_id"] = new MCPToolProperty { Type = "string", Description = "ID of the target entity" },
                    ["context_notes"] = new MCPToolProperty { Type = "string", Description = "Additional context or evidence for this relationship" }
                },
                Required = new List<string> { "source_id", "predicate", "target_id", "context_notes" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = false,
            }
        };
    }

    public override async Task<List<MCPContent>> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sourceId = MCPHelper.GetRequiredStr(args, "source_id");
        string predicate = MCPHelper.GetRequiredStr(args, "predicate");
        string targetId = MCPHelper.GetRequiredStr(args, "target_id");
        string contextNotes = MCPHelper.GetRequiredStr(args, "context_notes");

        var service = new MemoryService();
        await service.AddRelationAsync(sourceId, predicate, targetId, contextNotes);

        return MCPContent.CreateSimpleContent($"✅ Relationship successfully created: {sourceId} --({predicate})--> {targetId}.");
    }
}
