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
using GKCore.Locales;

namespace GKcli.Commands;

internal class FamMenuCommand : BaseCommand
{
    public FamMenuCommand() : base("families", LSID.RPFamilies, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Family, true, "Select a family operation");
    }
}


internal class FamListCommand : RecordCommand
{
    public FamListCommand() : base("family_list", LSID.Find, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        SelectRecord(baseContext, GDMRecordType.rtFamily, "Select a family", "Family: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all families in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtFamily);
        return MCPHelper.PageableTable("families", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Husband | Wife |\n|---|---|---|";
            } else {
                var famRec = (GDMFamilyRecord)recList[index];
                var husbandRec = baseContext.Tree.GetPtrValue(famRec.Husband);
                string husbandName = husbandRec == null ? "-" : GKUtils.GetRecordName(baseContext.Tree, husbandRec, false);
                var wifeRec = baseContext.Tree.GetPtrValue(famRec.Wife);
                string wifeName = wifeRec == null ? "-" : GKUtils.GetRecordName(baseContext.Tree, wifeRec, false);
                return $"|{famRec.XRef}|{husbandName}|{wifeName}|";
            }
        });
    }
}


internal class FamAddCommand : BaseCommand
{
    public FamAddCommand() : base("family_add", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new family record to the database with husband and wife",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["husband_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the husband (e.g., 'I1')" },
                    ["wife_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the wife (e.g., 'I2')" }
                },
                Required = new List<string> { "husband_xref", "wife_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string husbandXRef = MCPHelper.GetRequiredArgument(args, "husband_xref");
        string wifeXRef = MCPHelper.GetRequiredArgument(args, "wife_xref");

        var husbandRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(husbandXRef);
        if (husbandRec == null)
            return MCPContent.CreateSimpleContent($"Husband not found with XRef: {husbandXRef}");

        var wifeRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(wifeXRef);
        if (wifeRec == null)
            return MCPContent.CreateSimpleContent($"Wife not found with XRef: {wifeXRef}");

        var familyRec = baseContext.Tree.CreateFamily();
        familyRec.AddSpouse(husbandRec);
        familyRec.AddSpouse(wifeRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Family with XRef `{familyRec.XRef}` added: husband {husbandXRef}, wife {wifeXRef}");
    }
}


internal class FamDeleteCommand : BaseCommand
{
    public FamDeleteCommand() : base("family_delete", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a family from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(xref);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {xref}");

        baseContext.DeleteRecord(familyRec);

        return MCPContent.CreateSimpleContent($"Family deleted: {xref}");
    }
}


internal class FamAddChildCommand : BaseCommand
{
    public FamAddChildCommand() : base("family_add_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a child to a family by their XRef identifiers",
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
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredArgument(args, "child_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        var childRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(childXRef);
        if (childRec == null)
            return MCPContent.CreateSimpleContent($"Child not found with XRef: {childXRef}");

        if (familyRec.IndexOfChild(childRec) >= 0)
            return MCPContent.CreateSimpleContent($"Child {childXRef} is already a member of family '{familyXRef}'.");

        familyRec.AddChild(childRec);
        baseContext.SetModified();

        string childName = GKUtils.GetNameString(childRec, false);
        return MCPContent.CreateSimpleContent($"Child added to family '{familyXRef}': {childName} ({childXRef})");
    }
}


internal class FamDeleteChildCommand : BaseCommand
{
    public FamDeleteChildCommand() : base("family_delete_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

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
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredArgument(args, "child_xref");

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
