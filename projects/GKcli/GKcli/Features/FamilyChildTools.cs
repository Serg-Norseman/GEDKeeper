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
using GKcli.MCP;
using GKcli.Platform;
using GKCore;

namespace GKcli.Features;

internal class FamListChildrenTool : BaseTool
{
    public FamListChildrenTool() : base("family_list_children") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all children of a family by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "family_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredStr(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        if (familyRec.Children.Count <= 0)
            return MCPContent.CreateSimpleContent($"Family '{familyXRef}' has no children.");

        var rows = new List<string> {
            $"Children of family '{familyXRef}' ({familyRec.Children.Count}):",
            "| Index | Child XRef | Name | Sex |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < familyRec.Children.Count; i++) {
            var childPtr = familyRec.Children[i];
            var childRec = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(childPtr);

            if (childRec != null) {
                string childName = GKUtils.GetNameString(childRec, false);
                string sex = GKData.SexData[(int)childRec.Sex].Sign;
                rows.Add($"|{i}|{childPtr.XRef}|{childName}|{sex}|");
            } else {
                rows.Add($"|{i}|{childPtr.XRef}|(not found)|-|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class FamAddChildTool : BaseTool
{
    public FamAddChildTool() : base("family_add_child") { }

    public override MCPTool CreateTool()
    {
        var linkageTypes = RuntimeData.LinkageTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a child to a family by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["child_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the child (e.g., 'I3')" },
                    ["linkage_type"] = new MCPToolProperty { Type = "string", Description = "Pedigree linkage type", Enum = linkageTypes },
                },
                Required = new List<string> { "family_xref", "child_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredStr(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredStr(args, "child_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        var childRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(childXRef);
        if (childRec == null)
            return MCPContent.CreateSimpleContent($"Child not found with XRef: {childXRef}");

        if (familyRec.IndexOfChild(childRec) >= 0)
            return MCPContent.CreateSimpleContent($"Child {childXRef} is already a member of family '{familyXRef}'.");

        var linkageType = GDMPedigreeLinkageType.plNone;
        string linkageTypeStr = MCPHelper.GetOptionalStr(args, "linkage_type", null);
        if (linkageTypeStr != null) {
            if (!RuntimeData.LinkageTypeMap.TryGetValue(linkageTypeStr, out linkageType))
                return MCPContent.CreateSimpleContent($"Invalid linkage type: '{linkageTypeStr}'.");
        }

        familyRec.AddChild(childRec, linkageType);
        baseContext.SetModified();

        string childName = GKUtils.GetNameString(childRec, false);
        return MCPContent.CreateSimpleContent($"Child added to family '{familyXRef}': {childName} ({childXRef})");
    }
}


internal class FamDeleteChildTool : BaseTool
{
    public FamDeleteChildTool() : base("family_delete_child") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a child from a family by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["child_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the child (e.g., 'I3')" }
                },
                Required = new List<string> { "family_xref", "child_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredStr(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredStr(args, "child_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        var childRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(childXRef);
        if (childRec == null)
            return MCPContent.CreateSimpleContent($"Child not found with XRef: {childXRef}");

        if (familyRec.IndexOfChild(childRec) < 0)
            return MCPContent.CreateSimpleContent($"Child {childXRef} is not a member of family '{familyXRef}'.");

        familyRec.RemoveChild(childRec);
        baseContext.SetModified();

        string childName = GKUtils.GetNameString(childRec, false);
        return MCPContent.CreateSimpleContent($"Child removed from family '{familyXRef}': {childName} ({childXRef})");
    }
}
