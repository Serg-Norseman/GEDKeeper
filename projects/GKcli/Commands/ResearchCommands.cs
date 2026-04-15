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

internal class ResearchListCommand : BaseCommand
{
    public ResearchListCommand() : base("research_list", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all researches in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtResearch);
        return MCPHelper.PageableTable("researches", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Name | Priority | Status | Start Date | Stop Date | Percent |\n|---|---|---|---|---|---|---|";
            } else {
                var rec = (GDMResearchRecord)recList[index];
                string name = rec.ResearchName;
                string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                string status = LangMan.LS(GKData.StatusNames[(int)rec.Status]);
                string startDate = GKUtils.GetDateDisplayString(rec.StartDate);
                string stopDate = GKUtils.GetDateDisplayString(rec.StopDate);
                string percent = rec.Percent.ToString();
                return $"|{rec.XRef}|{name}|{priority}|{status}|{startDate}|{stopDate}|{percent}|";
            }
        });
    }
}


internal class ResearchAddCommand : BaseCommand
{
    private static Dictionary<string, GDMResearchPriority> PriorityMap;
    private static Dictionary<string, GDMResearchStatus> StatusMap;

    public ResearchAddCommand() : base("research_add", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    private static void RequireMaps()
    {
        if (PriorityMap == null) {
            PriorityMap = new Dictionary<string, GDMResearchPriority>();
            for (GDMResearchPriority pt = GDMResearchPriority.rpNone; pt <= GDMResearchPriority.rpTop; pt++) {
                PriorityMap.Add(LangMan.LS(GKData.PriorityNames[(int)pt]), pt);
            }
        }

        if (StatusMap == null) {
            StatusMap = new Dictionary<string, GDMResearchStatus>();
            for (var st = GDMResearchStatus.rsDefined; st <= GDMResearchStatus.rsWithdrawn; st++) {
                StatusMap.Add(LangMan.LS(GKData.StatusNames[(int)st]), st);
            }
        }
    }

    public override MCPTool CreateTool()
    {
        RequireMaps();

        var priorities = PriorityMap.Keys.ToList();
        var statuses = StatusMap.Keys.ToList();

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

        if (!PriorityMap.TryGetValue(priorityStr, out var priority))
            return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");

        if (!StatusMap.TryGetValue(statusStr, out var status))
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


internal class ResearchDeleteCommand : BaseCommand
{
    public ResearchDeleteCommand() : base("research_delete", null, CommandCategory.Research) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a research record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'RES1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(xref);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {xref}");

        baseContext.DeleteRecord(researchRec);

        return MCPContent.CreateSimpleContent($"Research deleted: {xref}");
    }
}
