/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
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
        // Empty for interactive mode
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
                string startDate = MCPHelper.GetDateValue(rec.StartDate);
                string stopDate = MCPHelper.GetDateValue(rec.StopDate);
                string percent = rec.Percent.ToString();
                return $"|{rec.XRef}|{name}|{priority}|{status}|{startDate}|{stopDate}|{percent}|";
            }
        });
    }
}
