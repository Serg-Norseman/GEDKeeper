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
using GKcli.Platform;
using GKCore;

namespace GKcli.Features;


internal class ResearchUpsertTool : BaseTool
{
    public ResearchUpsertTool() : base("research_upsert") { }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();
        var statuses = RuntimeData.StatusMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add new research record or update existing. Provide 'xref' to edit; omit 'xref' to create. 'title', 'priority' and 'status' required for new records.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing research to update (omit for new)" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the research item" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "Priority of the research.", Enum = priorities },
                    ["status"] = new MCPToolProperty { Type = "string", Description = "Status of the research.", Enum = statuses },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "Research start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "Research end date" },
                    ["percent"] = new MCPToolProperty { Type = "integer", Description = "Completion percentage (0-100)" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string title = MCPHelper.GetOptionalStr(args, "title", null);
        string priorityStr = MCPHelper.GetOptionalStr(args, "priority", null);
        string statusStr = MCPHelper.GetOptionalStr(args, "status", null);
        string startDate = MCPHelper.GetOptionalStr(args, "start_date", null);
        string stopDate = MCPHelper.GetOptionalStr(args, "stop_date", null);
        int percent = MCPHelper.GetOptionalInt(args, "percent", -1);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var resRec = baseContext.Tree.FindXRef<GDMResearchRecord>(xref);
            if (resRec == null)
                return MCPContent.CreateSimpleContent($"❌ Research record not found: '{xref}'.");

            if (title != null) {
                resRec.ResearchName = title;
            }

            if (priorityStr != null) {
                if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                    return MCPContent.CreateSimpleContent($"❌ Invalid priority: '{priorityStr}'.");
                resRec.Priority = priority;
            }

            if (statusStr != null) {
                if (!RuntimeData.StatusMap.TryGetValue(statusStr, out var status))
                    return MCPContent.CreateSimpleContent($"❌ Invalid status: '{statusStr}'.");
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
                    return MCPContent.CreateSimpleContent($"❌ Percent value must be between 0 and 100, got: {percent}.");
                resRec.Percent = percent;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Research record updated: {resRec.XRef} - \"{resRec.ResearchName}\"");
        } else {
            if (string.IsNullOrEmpty(title))
                return MCPContent.CreateSimpleContent("❌ 'title' required for new research record");

            if (string.IsNullOrEmpty(priorityStr))
                return MCPContent.CreateSimpleContent("❌ 'priority' required for new research record");

            if (string.IsNullOrEmpty(statusStr))
                return MCPContent.CreateSimpleContent("❌ 'status' required for new research record");

            if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                return MCPContent.CreateSimpleContent($"❌ Invalid priority: '{priorityStr}'.");

            if (!RuntimeData.StatusMap.TryGetValue(statusStr, out var status))
                return MCPContent.CreateSimpleContent($"❌ Invalid status: '{statusStr}'.");

            var resRec = baseContext.Tree.CreateResearch();
            resRec.ResearchName = title;
            resRec.Priority = priority;
            resRec.Status = status;

            if (startDate != null) {
                resRec.StartDate.ParseString(startDate);
            }

            if (stopDate != null) {
                resRec.StopDate.ParseString(stopDate);
            }

            if (percent > -1) {
                if (percent < 0 || percent > 100)
                    return MCPContent.CreateSimpleContent($"❌ Percent value must be between 0 and 100, got: {percent}.");
                resRec.Percent = percent;
            } else {
                resRec.Percent = 0;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Research record added: {resRec.XRef} - \"{title}\"");
        }
    }
}
