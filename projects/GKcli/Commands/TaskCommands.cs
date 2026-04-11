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

internal class TaskListCommand : BaseCommand
{
    public TaskListCommand() : base("task_list", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all tasks in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtTask);
        return MCPHelper.PageableTable("tasks", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Goal | Priority | Start Date | Stop Date |\n|---|---|---|---|---|";
            } else {
                var rec = (GDMTaskRecord)recList[index];
                string goal = GKUtils.GetTaskGoalStr(baseContext.Tree, rec);
                string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                string startDate = MCPHelper.GetDateValue(rec.StartDate);
                string stopDate = MCPHelper.GetDateValue(rec.StopDate);
                return $"|{rec.XRef}|{goal}|{priority}|{startDate}|{stopDate}|";
            }
        });
    }
}
