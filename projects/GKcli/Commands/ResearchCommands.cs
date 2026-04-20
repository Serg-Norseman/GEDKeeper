/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class ResearchListCommand : BaseCommand
{
    public ResearchListCommand() : base("research_list", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class ResearchAddCommand : BaseCommand
{
    public ResearchAddCommand() : base("research_add", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();
        var statuses = RuntimeData.StatusMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a new research record to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the research item" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "Priority of the research.", Enum = priorities },
                    ["status"] = new MCPToolProperty { Type = "string", Description = "Status of the research.", Enum = statuses },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "Research start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "Research end date" },
                    ["percent"] = new MCPToolProperty { Type = "integer", Description = "Completion percentage (0-100)" },
                },
                Required = new List<string> { "title", "priority", "status" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string title = MCPHelper.GetRequiredArgument(args, "title");
        string priorityStr = MCPHelper.GetRequiredArgument(args, "priority");
        string statusStr = MCPHelper.GetRequiredArgument(args, "status");

        if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
            return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");

        if (!RuntimeData.StatusMap.TryGetValue(statusStr, out var status))
            return MCPContent.CreateSimpleContent($"Invalid status: '{statusStr}'.");

        string startDate = MCPHelper.GetStringArgument(args, "start_date", string.Empty);
        string stopDate = MCPHelper.GetStringArgument(args, "stop_date", string.Empty);
        int percent = MCPHelper.GetIntArgument(args, "percent", 0);

        var resRec = baseContext.Tree.CreateResearch();
        resRec.ResearchName = title;
        resRec.Priority = priority;
        resRec.Status = status;
        resRec.StartDate.ParseString(startDate);
        resRec.StopDate.ParseString(stopDate);
        resRec.Percent = percent;

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Research record added: {resRec.XRef} - \"{title}\"");
    }
}


internal class ResearchEditCommand : BaseCommand
{
    public ResearchEditCommand() : base("research_edit", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();
        var statuses = RuntimeData.StatusMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing research record in the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "New title/name of the research item" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "New priority of the research.", Enum = priorities },
                    ["status"] = new MCPToolProperty { Type = "string", Description = "New status of the research.", Enum = statuses },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "New research start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "New research end date" },
                    ["percent"] = new MCPToolProperty { Type = "integer", Description = "New completion percentage (0-100)" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var resRec = baseContext.Tree.FindXRef<GDMResearchRecord>(xref);
        if (resRec == null)
            return MCPContent.CreateSimpleContent($"Research record not found: '{xref}'.");

        string title = MCPHelper.GetStringArgument(args, "title", null);
        string priorityStr = MCPHelper.GetStringArgument(args, "priority", null);
        string statusStr = MCPHelper.GetStringArgument(args, "status", null);
        string startDate = MCPHelper.GetStringArgument(args, "start_date", null);
        string stopDate = MCPHelper.GetStringArgument(args, "stop_date", null);
        int percent = MCPHelper.GetIntArgument(args, "percent", -1);

        if (title != null) {
            resRec.ResearchName = title;
        }

        if (priorityStr != null) {
            if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");
            resRec.Priority = priority;
        }

        if (statusStr != null) {
            if (!RuntimeData.StatusMap.TryGetValue(statusStr, out var status))
                return MCPContent.CreateSimpleContent($"Invalid status: '{statusStr}'.");
            resRec.Status = status;
        }

        if (startDate != null) {
            resRec.StartDate.ParseString(startDate);
        }

        if (stopDate != null) {
            resRec.StopDate.ParseString(stopDate);
        }

        if (percent > -1) {
            if (percent < 0 || percent > 100)
                return MCPContent.CreateSimpleContent($"Percent value must be between 0 and 100, got: {percent}.");
            resRec.Percent = percent;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Research record updated: {resRec.XRef} - \"{resRec.ResearchName}\"");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class ResearchDeleteCommand : BaseCommand
{
    public ResearchDeleteCommand() : base("research_delete", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
