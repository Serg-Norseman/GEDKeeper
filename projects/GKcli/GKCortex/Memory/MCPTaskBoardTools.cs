/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using GKCore;
using GKCortex.Features;
using GKCortex.MCP;
using GKCortex.Protocols;

namespace GKCortex.Memory;


internal class CreateGenealogyTaskTool : BaseTool
{
    public CreateGenealogyTaskTool() : base("create_genealogy_task") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Create a new genealogy research task focused on a specific person",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["target_person"] = new MCPToolProperty { Type = "string", Description = "Name or identifier of the person being researched" },
                    ["goal_description"] = new MCPToolProperty { Type = "string", Description = "Description of the research goal or question" }
                },
                Required = new List<string> { "target_person", "goal_description" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string targetPerson = MCPHelper.GetRequiredStr(args, "target_person");
        string goalDescription = MCPHelper.GetRequiredStr(args, "goal_description");

        var service = new MemoryService();
        int newId = service.CreateTaskAsync(targetPerson, goalDescription).GetAwaiter().GetResult();

        return MCPContent.CreateSimpleContent($"✅ Created new research task #{newId}. Focus on completing this task.");
    }
}


internal class UpdateTaskProgressTool : BaseTool
{
    public UpdateTaskProgressTool() : base("update_task_progress") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Update progress on an existing research task: add checked sources and define next steps",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["task_id"] = new MCPToolProperty { Type = "integer", Description = "The unique ID of the task to update" },
                    ["add_checked_source"] = new MCPToolProperty { Type = "string", Description = "Description of a newly verified source or finding", Default = "" },
                    ["set_next_steps"] = new MCPToolProperty {
                        Type = "array",
                        Description = "List of next action steps for the task",
                        Items = new MCPToolProperty { Type = "string" }
                    }
                },
                Required = new List<string> { "task_id" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        int taskId = MCPHelper.GetRequiredInt(args, "task_id");
        string addCheckedSource = MCPHelper.GetOptionalStr(args, "add_checked_source", "");
        string[] setNextSteps = MCPHelper.GetOptionalStringArray(args, "set_next_steps", Array.Empty<string>());

        var service = new MemoryService();
        bool success = service.UpdateTaskProgressAsync(taskId, addCheckedSource, setNextSteps).GetAwaiter().GetResult();

        if (!success)
            return MCPContent.CreateSimpleContent($"❌ Task with ID {taskId} not found.");

        return MCPContent.CreateSimpleContent($"✅ Progress on task #{taskId} updated. Knowledge base has recorded the changes.");
    }
}


internal class ChangeTaskStatusTool : BaseTool
{
    public ChangeTaskStatusTool() : base("change_task_status") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Change the status of a research task (COMPLETED, PAUSED, or ACTIVE)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["task_id"] = new MCPToolProperty { Type = "integer", Description = "The unique ID of the task" },
                    ["status"] = new MCPToolProperty { Type = "string", Description = "New status value: COMPLETED, PAUSED, or ACTIVE" }
                },
                Required = new List<string> { "task_id", "status" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        int taskId = MCPHelper.GetRequiredInt(args, "task_id");
        string status = MCPHelper.GetRequiredStr(args, "status");

        var service = new MemoryService();
        bool success = service.ChangeTaskStatusAsync(taskId, status).GetAwaiter().GetResult();

        if (!success)
            return MCPContent.CreateSimpleContent($"❌ Failed to change status for task #{taskId}. Verify ID and status value (COMPLETED/PAUSED/ACTIVE).");

        return MCPContent.CreateSimpleContent($"✅ Task #{taskId} status changed to {status.ToUpperInvariant()}.");
    }
}
