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

/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class GroupListCommand : BaseCommand
{
    public GroupListCommand() : base("group_list", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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


internal class GroupEditCommand : BaseCommand
{
    public GroupEditCommand() : base("group_edit", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing group record to the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New name of the group item" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var repoRec = baseContext.Tree.FindXRef<GDMGroupRecord>(xref);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {xref}");

        string name = MCPHelper.GetStringArgument(args, "name", null);
        if (name != null) {
            repoRec.GroupName = name;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Group record updated: {repoRec.XRef} - \"{name}\"");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class GroupDeleteCommand : BaseCommand
{
    public GroupDeleteCommand() : base("group_delete", null, CommandCategory.Group) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
