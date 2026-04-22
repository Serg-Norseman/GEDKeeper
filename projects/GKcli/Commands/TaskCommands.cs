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

/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class TaskListCommand : BaseCommand
{
    public TaskListCommand() : base("task_list", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class TaskAddCommand : BaseCommand
{
    public TaskAddCommand() : base("task_add", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();

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
        string goal = MCPHelper.GetRequiredStr(args, "goal");
        string goalName;
        var record = baseContext.Tree.FindXRef<GDMRecord>(goal);
        if (record != null) {
            goal = GEDCOMUtils.EncloseXRef(goal);
            goalName = GKUtils.GetRecordName(baseContext.Tree, record, false);
        } else {
            goalName = goal;
        }

        string priorityStr = MCPHelper.GetRequiredStr(args, "priority");
        if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
            return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");

        string startDate = MCPHelper.GetOptionalStr(args, "start_date", string.Empty);
        string stopDate = MCPHelper.GetOptionalStr(args, "stop_date", string.Empty);

        var taskRec = baseContext.Tree.CreateTask();
        taskRec.Goal = goal;
        taskRec.Priority = priority;
        taskRec.StartDate.ParseString(startDate);
        taskRec.StopDate.ParseString(stopDate);

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Task record added: {taskRec.XRef} - \"{goalName}\"");
    }
}


internal class TaskEditCommand : BaseCommand
{
    public TaskEditCommand() : base("task_edit", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing task record in the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["goal"] = new MCPToolProperty { Type = "string", Description = "New goal of the task item (arbitrary name or XRef identifier of the individual, family, source)" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "New priority of the task.", Enum = priorities },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "New task start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "New task end date" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");

        var taskRec = baseContext.Tree.FindXRef<GDMTaskRecord>(xref);
        if (taskRec == null)
            return MCPContent.CreateSimpleContent($"Task not found with XRef: {xref}");

        string goal = MCPHelper.GetOptionalStr(args, "goal", null);
        if (goal != null) {
            string goalName;
            var record = baseContext.Tree.FindXRef<GDMRecord>(goal);
            if (record != null) {
                goal = GEDCOMUtils.EncloseXRef(goal);
                goalName = GKUtils.GetRecordName(baseContext.Tree, record, false);
            } else {
                goalName = goal;
            }
            taskRec.Goal = goal;
        } else {
        }

        string priorityStr = MCPHelper.GetOptionalStr(args, "priority", null);
        if (priorityStr != null) {
            if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                return MCPContent.CreateSimpleContent($"Invalid priority: '{priorityStr}'.");

            taskRec.Priority = priority;
        }

        string startDate = MCPHelper.GetOptionalStr(args, "start_date", null);
        if (startDate != null) {
            taskRec.StartDate.ParseString(startDate);
        }

        string stopDate = MCPHelper.GetOptionalStr(args, "stop_date", null);
        if (stopDate != null) {
            taskRec.StopDate.ParseString(stopDate);
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Task record updated: {taskRec.XRef}");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class TaskDeleteCommand : BaseCommand
{
    public TaskDeleteCommand() : base("task_delete", null, CommandCategory.Task) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
