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
using GKcli.Platform;
using GKCore;

namespace GKcli.Features;

internal class TaskUpsertTool : BaseTool
{
    public TaskUpsertTool() : base("task_upsert") { }

    public override MCPTool CreateTool()
    {
        var priorities = RuntimeData.PriorityMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add new task or update existing. Provide 'xref' to edit; omit 'xref' to create. 'goal' and 'priority' required for new tasks.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit (omit for new)" },
                    ["goal"] = new MCPToolProperty { Type = "string", Description = "Goal of the task item (arbitrary name or XRef identifier of the individual, family, source)" },
                    ["priority"] = new MCPToolProperty { Type = "string", Description = "Priority of the task.", Enum = priorities },
                    ["start_date"] = new MCPToolProperty { Type = "string", Description = "Task start date" },
                    ["stop_date"] = new MCPToolProperty { Type = "string", Description = "Task end date" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string goal = MCPHelper.GetOptionalStr(args, "goal", null);
        string priorityStr = MCPHelper.GetOptionalStr(args, "priority", null);
        string startDate = MCPHelper.GetOptionalStr(args, "start_date", null);
        string stopDate = MCPHelper.GetOptionalStr(args, "stop_date", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var taskRec = baseContext.Tree.FindXRef<GDMTaskRecord>(xref);
            if (taskRec == null)
                return MCPContent.CreateSimpleContent($"❌ Task not found with XRef: {xref}");

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
            }

            if (priorityStr != null) {
                if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                    return MCPContent.CreateSimpleContent($"❌ Invalid priority: '{priorityStr}'.");

                taskRec.Priority = priority;
            }

            if (startDate != null) {
                taskRec.StartDate.ParseString(startDate);
            }

            if (stopDate != null) {
                taskRec.StopDate.ParseString(stopDate);
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Task record updated: {taskRec.XRef}");
        } else {
            if (string.IsNullOrEmpty(goal))
                return MCPContent.CreateSimpleContent("❌ 'goal' required for new task");

            if (string.IsNullOrEmpty(priorityStr))
                return MCPContent.CreateSimpleContent("❌ 'priority' required for new task");

            string goalName;
            var record = baseContext.Tree.FindXRef<GDMRecord>(goal);
            if (record != null) {
                goal = GEDCOMUtils.EncloseXRef(goal);
                goalName = GKUtils.GetRecordName(baseContext.Tree, record, false);
            } else {
                goalName = goal;
            }

            if (!RuntimeData.PriorityMap.TryGetValue(priorityStr, out var priority))
                return MCPContent.CreateSimpleContent($"❌ Invalid priority: '{priorityStr}'.");

            var taskRec = baseContext.Tree.CreateTask();
            taskRec.Goal = goal;
            taskRec.Priority = priority;
            if (startDate != null) taskRec.StartDate.ParseString(startDate);
            if (stopDate != null) taskRec.StopDate.ParseString(stopDate);

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Task record added: {taskRec.XRef} - \"{goalName}\"");
        }
    }
}
