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

namespace GKcli.Commands;

internal class GroupListCommand : BaseCommand
{
    public GroupListCommand() : base("group_list", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all groups in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtGroup);
        return MCPHelper.PageableTable("groups", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Group | Members |\n|---|---|---|";
            } else {
                var rec = (GDMGroupRecord)recList[index];
                int membersCount = rec.Members.Count;
                return $"|{rec.XRef}|{rec.GroupName}|{membersCount}|";
            }
        });
    }
}


internal class GroupAddCommand : BaseCommand
{
    public GroupAddCommand() : base("group_add", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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

        return MCPContent.CreateSimpleContent($"Group added: {name} with XRef `{groupRec.XRef}`");
    }
}


internal class GroupDeleteCommand : BaseCommand
{
    public GroupDeleteCommand() : base("group_delete", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
