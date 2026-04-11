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
using GDModel;
using GKcli.MCP;
using GKCore;

namespace GKcli.Commands;

internal class GroupListCommand : BaseCommand
{
    public GroupListCommand() : base("group_list", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all groups in the database",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtGroup);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No groups in database.");

        var rows = new List<string> {
            $"Groups ({recList.Count}):",
            "| XRef | Group | Members |",
            "|---|---|---|"
        };
        foreach (var rec in recList) {
            var groupRec = (GDMGroupRecord)rec;
            int membersCount = groupRec.Members.Count;
            rows.Add($"|{rec.XRef}|{groupRec.GroupName}|{membersCount}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class GroupAddCommand : BaseCommand
{
    public GroupAddCommand() : base("group_add", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new group to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Group name" }
                },
                Required = new List<string> { "name" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredArgument(args, "name");

        var groupRec = baseContext.Tree.CreateGroup();
        groupRec.GroupName = name;
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Group added: {name}");
    }
}


internal class GroupAddMemberCommand : BaseCommand
{
    public GroupAddMemberCommand() : base("group_add_member", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an individual to a group by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["group_name"] = new MCPToolProperty { Type = "string", Description = "Name of the group" },
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1', 'I2')" }
                },
                Required = new List<string> { "group_name", "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string groupName = MCPHelper.GetRequiredArgument(args, "group_name");
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");

        var groupRecs = baseContext.Tree.GetRecords(GDMRecordType.rtGroup);
        GDMGroupRecord targetGroup = null;
        foreach (var rec in groupRecs) {
            var gr = rec as GDMGroupRecord;
            if (gr != null && string.Equals(gr.GroupName, groupName, StringComparison.OrdinalIgnoreCase)) {
                targetGroup = gr;
                break;
            }
        }

        if (targetGroup == null)
            return MCPContent.CreateSimpleContent($"Group not found: {groupName}");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (targetGroup.IndexOfMember(indiRec) >= 0)
            return MCPContent.CreateSimpleContent($"Individual {individualXRef} is already a member of group '{groupName}'.");

        targetGroup.AddMember(indiRec);
        baseContext.SetModified();

        string indiName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual added to group '{groupName}': {indiName} ({individualXRef})");
    }
}


internal class GroupDeleteCommand : BaseCommand
{
    public GroupDeleteCommand() : base("group_delete", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a group from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(xref);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {xref}");

        baseContext.DeleteRecord(groupRec);

        return MCPContent.CreateSimpleContent($"Group deleted: {xref}");
    }
}
