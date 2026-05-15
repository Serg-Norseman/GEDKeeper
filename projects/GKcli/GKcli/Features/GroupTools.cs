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

internal class GroupUpsertTool : BaseTool
{
    public GroupUpsertTool() : base("group_upsert") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new group or update existing. Provide 'xref' to edit; omit 'xref' to create. 'name' required for new groups.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing group to update (omit for new)" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Group name" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string name = MCPHelper.GetOptionalStr(args, "name", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(xref);
            if (groupRec == null)
                return MCPContent.CreateSimpleContent($"❌ Group not found with XRef: {xref}");

            if (name != null) {
                groupRec.GroupName = name;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Group record updated: {groupRec.XRef} - \"{groupRec.GroupName}\"");
        } else {
            if (string.IsNullOrEmpty(name))
                return MCPContent.CreateSimpleContent("❌ 'name' required for new group");

            var groupRec = baseContext.Tree.CreateGroup();
            groupRec.GroupName = name;

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Group added: {name} with XRef `{groupRec.XRef}`");
        }
    }
}
