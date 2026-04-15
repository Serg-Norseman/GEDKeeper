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
using GDModel.Providers.GEDCOM;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class TaskListCommand : BaseCommand
{
    public TaskListCommand() : base("task_list", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
                string startDate = GKUtils.GetDateDisplayString(rec.StartDate);
                string stopDate = GKUtils.GetDateDisplayString(rec.StopDate);
                return $"|{rec.XRef}|{goal}|{priority}|{startDate}|{stopDate}|";
            }
        });
    }
}


internal class TaskAddCommand : BaseCommand
{
    private static Dictionary<string, GDMResearchPriority> PriorityMap;

    public TaskAddCommand() : base("task_add", null, CommandCategory.Task) { }

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
    }

    public override MCPTool CreateTool()
    {
        RequireMaps();

        var priorities = PriorityMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a new task record to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["goal"] = new MCPToolProperty { Type = "string", Description = "Goal of the task item (arbitrary name or XRef identifier of the individual, family, source)" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "Priority of the task.", Enum = priorities },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "Task start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "Task end date" },
                },
                Required = new List<string> { "goal", "priority" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string goal = MCPHelper.GetRequiredArgument(args, "goal");
        string goalName;
        var record = baseContext.Tree.FindXRef<GDMRecord>(goal);
        if (record != null) {
            goal = GEDCOMUtils.EncloseXRef(goal);
            goalName = GKUtils.GetRecordName(baseContext.Tree, record, false);
        } else {
            goalName = goal;
        }

        string priorityStr = MCPHelper.GetRequiredArgument(args, "priority");
        if (!PriorityMap.TryGetValue(priorityStr, out var priority))
            return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");

        string startDate = MCPHelper.GetStringArgument(args, "start_date", string.Empty);
        string stopDate = MCPHelper.GetStringArgument(args, "stop_date", string.Empty);

        var taskRec = baseContext.Tree.CreateTask();
        taskRec.Goal = goal;
        taskRec.Priority = priority;
        taskRec.StartDate.ParseString(startDate);
        taskRec.StopDate.ParseString(stopDate);

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Task record added: {taskRec.XRef} - \"{goalName}\"");
    }
}


internal class TaskDeleteCommand : BaseCommand
{
    public TaskDeleteCommand() : base("task_delete", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a task record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the task (e.g., 'TSK1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var taskRec = baseContext.Tree.FindXRef<GDMTaskRecord>(xref);
        if (taskRec == null)
            return MCPContent.CreateSimpleContent($"Task not found with XRef: {xref}");

        baseContext.DeleteRecord(taskRec);

        return MCPContent.CreateSimpleContent($"Task deleted: {xref}");
    }
}
