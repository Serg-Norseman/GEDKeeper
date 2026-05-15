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

internal class GroupListMembersTool : BaseTool
{
    public GroupListMembersTool() : base("group_list_members") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all members of a group by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["group_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1')" }
                },
                Required = new List<string> { "group_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string groupXRef = MCPHelper.GetRequiredStr(args, "group_xref");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(groupXRef);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {groupXRef}");

        if (groupRec.Members.Count <= 0)
            return MCPContent.CreateSimpleContent($"Group '{groupXRef}' has no members.");

        var rows = new List<string> {
            $"Members of group '{groupXRef}' ({groupRec.Members.Count}):",
            "| Index | Member XRef | Name | Sex |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < groupRec.Members.Count; i++) {
            var memberPtr = groupRec.Members[i];
            var memberRec = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(memberPtr);

            if (memberRec != null) {
                string memberName = GKUtils.GetNameString(memberRec, false);
                string sex = GKData.SexData[(int)memberRec.Sex].Sign;
                rows.Add($"|{i}|{memberPtr.XRef}|{memberName}|{sex}|");
            } else {
                rows.Add($"|{i}|{memberPtr.XRef}|(not found)|-|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class GroupAddMemberTool : BaseTool
{
    public GroupAddMemberTool() : base("group_add_member") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an individual to a group by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["group_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1', 'G2')" },
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1', 'I2')" }
                },
                Required = new List<string> { "group_xref", "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string groupXRef = MCPHelper.GetRequiredStr(args, "group_xref");
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(groupXRef);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {groupXRef}");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (groupRec.IndexOfMember(indiRec) >= 0)
            return MCPContent.CreateSimpleContent($"Individual {individualXRef} is already a member of group '{groupXRef}'.");

        groupRec.AddMember(indiRec);
        baseContext.SetModified();

        string indiName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual added to group '{groupXRef}': {indiName} ({individualXRef})");
    }
}


internal class GroupDeleteMemberTool : BaseTool
{
    public GroupDeleteMemberTool() : base("group_delete_member") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an individual from a group by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["group_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1', 'G2')" },
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1', 'I2')" }
                },
                Required = new List<string> { "group_xref", "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string groupXRef = MCPHelper.GetRequiredStr(args, "group_xref");
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var groupRec = baseContext.Tree.FindXRef<GDMGroupRecord>(groupXRef);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {groupXRef}");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (groupRec.IndexOfMember(indiRec) < 0)
            return MCPContent.CreateSimpleContent($"Individual {individualXRef} is not a member of group '{groupXRef}'.");

        groupRec.RemoveMember(indiRec);
        baseContext.SetModified();

        string indiName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual removed from group '{groupXRef}': {indiName} ({individualXRef})");
    }
}
