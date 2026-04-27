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

namespace GKcli.Features;

internal class ResearchListTasksTool : BaseTool
{
    public ResearchListTasksTool() : base("research_list_tasks") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all tasks of a research by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1')" }
                },
                Required = new List<string> { "research_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        if (researchRec.Tasks.Count <= 0)
            return MCPContent.CreateSimpleContent($"Research '{researchXRef}' has no tasks.");

        var rows = new List<string> {
            $"Tasks of research '{researchXRef}' ({researchRec.Tasks.Count}):",
            "| Index | Task XRef | Name |",
            "|---|---|---|"
        };
        for (int i = 0; i < researchRec.Tasks.Count; i++) {
            var taskPtr = researchRec.Tasks[i];
            var taskRec = baseContext.Tree.GetPtrValue<GDMTaskRecord>(taskPtr);

            if (taskRec != null) {
                string taskName = GKUtils.GetTaskGoalStr(baseContext.Tree, taskRec);
                rows.Add($"|{i}|{taskPtr.XRef}|{taskName}|");
            } else {
                rows.Add($"|{i}|{taskPtr.XRef}|(not found)|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class ResearchAddTaskTool : BaseTool
{
    public ResearchAddTaskTool() : base("research_add_task") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a task to a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["task_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the task (e.g., 'T1', 'T2')" }
                },
                Required = new List<string> { "research_xref", "task_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string taskXRef = MCPHelper.GetRequiredStr(args, "task_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var taskRec = baseContext.Tree.FindXRef<GDMTaskRecord>(taskXRef);
        if (taskRec == null)
            return MCPContent.CreateSimpleContent($"Task not found with XRef: {taskXRef}");

        if (researchRec.IndexOfTask(taskRec) >= 0)
            return MCPContent.CreateSimpleContent($"Task {taskXRef} is already assigned to research '{researchXRef}'.");

        researchRec.AddTask(taskRec);
        baseContext.SetModified();

        string taskName = GKUtils.GetTaskGoalStr(baseContext.Tree, taskRec);
        return MCPContent.CreateSimpleContent($"Task added to research '{researchXRef}': {taskName} ({taskXRef})");
    }
}


internal class ResearchDeleteTaskTool : BaseTool
{
    public ResearchDeleteTaskTool() : base("research_delete_task") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a task from a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["task_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the task (e.g., 'T1', 'T2')" }
                },
                Required = new List<string> { "research_xref", "task_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string taskXRef = MCPHelper.GetRequiredStr(args, "task_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var taskRec = baseContext.Tree.FindXRef<GDMTaskRecord>(taskXRef);
        if (taskRec == null)
            return MCPContent.CreateSimpleContent($"Task not found with XRef: {taskXRef}");

        if (researchRec.IndexOfTask(taskRec) < 0)
            return MCPContent.CreateSimpleContent($"Task {taskXRef} is not assigned to research '{researchXRef}'.");

        researchRec.RemoveTask(taskRec);
        baseContext.SetModified();

        string taskName = GKUtils.GetTaskGoalStr(baseContext.Tree, taskRec);
        return MCPContent.CreateSimpleContent($"Task removed from research '{researchXRef}': {taskName} ({taskXRef})");
    }
}


internal class ResearchListCommunicationsTool : BaseTool
{
    public ResearchListCommunicationsTool() : base("research_list_communications") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all communications of a research by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1')" }
                },
                Required = new List<string> { "research_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        if (researchRec.Communications.Count <= 0)
            return MCPContent.CreateSimpleContent($"Research '{researchXRef}' has no communications.");

        var rows = new List<string> {
            $"Communications of research '{researchXRef}' ({researchRec.Communications.Count}):",
            "| Index | Communication XRef | Name | Date |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < researchRec.Communications.Count; i++) {
            var commPtr = researchRec.Communications[i];
            var commRec = baseContext.Tree.GetPtrValue<GDMCommunicationRecord>(commPtr);

            if (commRec != null) {
                string commName = commRec.CommName;
                string date = GKUtils.GetDateDisplayString(commRec.Date);
                rows.Add($"|{i}|{commPtr.XRef}|{commName}|{date}|");
            } else {
                rows.Add($"|{i}|{commPtr.XRef}|(not found)|-|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class ResearchAddCommunicationTool : BaseTool
{
    public ResearchAddCommunicationTool() : base("research_add_communication") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a communication to a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["communication_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the communication (e.g., 'C1', 'C2')" }
                },
                Required = new List<string> { "research_xref", "communication_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string communicationXRef = MCPHelper.GetRequiredStr(args, "communication_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var commRec = baseContext.Tree.FindXRef<GDMCommunicationRecord>(communicationXRef);
        if (commRec == null)
            return MCPContent.CreateSimpleContent($"Communication not found with XRef: {communicationXRef}");

        if (researchRec.IndexOfCommunication(commRec) >= 0)
            return MCPContent.CreateSimpleContent($"Communication {communicationXRef} is already assigned to research '{researchXRef}'.");

        researchRec.AddCommunication(commRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Communication added to research '{researchXRef}': {commRec.CommName} ({communicationXRef})");
    }
}


internal class ResearchDeleteCommunicationTool : BaseTool
{
    public ResearchDeleteCommunicationTool() : base("research_delete_communication") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a communication from a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["communication_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the communication (e.g., 'C1', 'C2')" }
                },
                Required = new List<string> { "research_xref", "communication_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string communicationXRef = MCPHelper.GetRequiredStr(args, "communication_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var commRec = baseContext.Tree.FindXRef<GDMCommunicationRecord>(communicationXRef);
        if (commRec == null)
            return MCPContent.CreateSimpleContent($"Communication not found with XRef: {communicationXRef}");

        if (researchRec.IndexOfCommunication(commRec) < 0)
            return MCPContent.CreateSimpleContent($"Communication {communicationXRef} is not assigned to research '{researchXRef}'.");

        researchRec.RemoveCommunication(commRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Communication removed from research '{researchXRef}': {commRec.CommName} ({communicationXRef})");
    }
}


internal class ResearchListGroupsTool : BaseTool
{
    public ResearchListGroupsTool() : base("research_list_groups") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all groups of a research by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1')" }
                },
                Required = new List<string> { "research_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        if (researchRec.Groups.Count <= 0)
            return MCPContent.CreateSimpleContent($"Research '{researchXRef}' has no groups.");

        var rows = new List<string> {
            $"Groups of research '{researchXRef}' ({researchRec.Groups.Count}):",
            "| Index | Group XRef | Name |",
            "|---|---|---|"
        };
        for (int i = 0; i < researchRec.Groups.Count; i++) {
            var groupPtr = researchRec.Groups[i];
            var groupRec = baseContext.Tree.GetPtrValue<GDMGroupRecord>(groupPtr);

            if (groupRec != null) {
                string groupName = groupRec.GroupName;
                rows.Add($"|{i}|{groupPtr.XRef}|{groupName}|");
            } else {
                rows.Add($"|{i}|{groupPtr.XRef}|(not found)|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class ResearchAddGroupTool : BaseTool
{
    public ResearchAddGroupTool() : base("research_add_group") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a group to a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["group_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1', 'G2')" }
                },
                Required = new List<string> { "research_xref", "group_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string groupXRef = MCPHelper.GetRequiredStr(args, "group_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(groupXRef);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {groupXRef}");

        if (researchRec.IndexOfGroup(groupRec) >= 0)
            return MCPContent.CreateSimpleContent($"Group {groupXRef} is already assigned to research '{researchXRef}'.");

        researchRec.AddGroup(groupRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Group added to research '{researchXRef}': {groupRec.GroupName} ({groupXRef})");
    }
}


internal class ResearchDeleteGroupTool : BaseTool
{
    public ResearchDeleteGroupTool() : base("research_delete_group") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a group from a research by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["research_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the research (e.g., 'R1', 'R2')" },
                    ["group_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1', 'G2')" }
                },
                Required = new List<string> { "research_xref", "group_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string researchXRef = MCPHelper.GetRequiredStr(args, "research_xref");
        string groupXRef = MCPHelper.GetRequiredStr(args, "group_xref");

        var researchRec = baseContext.Tree.FindXRef<GDMResearchRecord>(researchXRef);
        if (researchRec == null)
            return MCPContent.CreateSimpleContent($"Research not found with XRef: {researchXRef}");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(groupXRef);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {groupXRef}");

        if (researchRec.IndexOfGroup(groupRec) < 0)
            return MCPContent.CreateSimpleContent($"Group {groupXRef} is not assigned to research '{researchXRef}'.");

        researchRec.RemoveGroup(groupRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Group removed from research '{researchXRef}': {groupRec.GroupName} ({groupXRef})");
    }
}
