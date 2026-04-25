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
using GKUI.Platform;

namespace GKcli.Commands;

internal class FamMenuCommand : BaseCommand
{
    public FamMenuCommand() : base("families", LSID.RPFamilies, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Family, true, "Select a family operation");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class FamListCommand : BaseCommand
{
    public FamListCommand() : base("family_list", LSID.Find, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtFamily, "Select a family", "Family: {0}", "No records.");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="FamilyUpsertCommand"/>).
/// </summary>
internal class FamAddCommand : BaseCommand
{
    public FamAddCommand() : base("family_add", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="FamilyUpsertCommand"/>).
/// </summary>
internal class FamEditCommand : BaseCommand
{
    public FamEditCommand() : base("family_edit", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class FamDeleteCommand : BaseCommand
{
    public FamDeleteCommand() : base("family_delete", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


/// <summary>
/// For MCP use only.
/// </summary>
internal class FamilyUpsertCommand : BaseCommand
{
    public FamilyUpsertCommand() : base("family_upsert", null, CommandCategory.None) { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new family or update existing. Provide 'xref' to edit; omit 'xref' to create. 'husband_xref' and 'wife_xref' required for new families.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit (omit for new)" },
                    ["husband_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the husband (e.g., 'I1'). Pass an empty string or '-' to delete." },
                    ["wife_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the wife (e.g., 'I2'). Pass an empty string or '-' to delete." }
                },
                Required = new List<string> { }
            }
        };
    }

    // TODO: Logic for the case when one of the spouses is unknown
    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string husbandXRef = MCPHelper.GetOptionalStr(args, "husband_xref", null);
        string wifeXRef = MCPHelper.GetOptionalStr(args, "wife_xref", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(xref);
            if (familyRec == null)
                return MCPContent.CreateSimpleContent($"❌ Family not found with XRef: {xref}");

            if (husbandXRef != null) {
                if (husbandXRef == "" || husbandXRef == "-") {
                    familyRec.Husband.XRef = "";
                } else if (!string.IsNullOrEmpty(husbandXRef)) {
                    var husbandRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(husbandXRef);
                    if (husbandRec == null)
                        return MCPContent.CreateSimpleContent($"❌ Husband not found with XRef: {husbandXRef}");

                    familyRec.Husband.XRef = husbandRec.XRef;
                }
            }

            if (wifeXRef != null) {
                if (wifeXRef == "" || wifeXRef == "-") {
                    familyRec.Wife.XRef = "";
                } else if (!string.IsNullOrEmpty(wifeXRef)) {
                    var wifeRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(wifeXRef);
                    if (wifeRec == null)
                        return MCPContent.CreateSimpleContent($"❌ Wife not found with XRef: {wifeXRef}");

                    familyRec.Wife.XRef = wifeRec.XRef;
                }
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Family with XRef `{familyRec.XRef}` updated: husband {husbandXRef}, wife {wifeXRef}");
        } else {
            if (string.IsNullOrEmpty(husbandXRef))
                return MCPContent.CreateSimpleContent("❌ 'husband_xref' required for new family");

            if (string.IsNullOrEmpty(wifeXRef))
                return MCPContent.CreateSimpleContent("❌ 'wife_xref' required for new family");

            var husbandRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(husbandXRef);
            if (husbandRec == null)
                return MCPContent.CreateSimpleContent($"❌ Husband not found with XRef: {husbandXRef}");

            var wifeRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(wifeXRef);
            if (wifeRec == null)
                return MCPContent.CreateSimpleContent($"❌ Wife not found with XRef: {wifeXRef}");

            var familyRec = baseContext.Tree.CreateFamily();
            familyRec.AddSpouse(husbandRec);
            familyRec.AddSpouse(wifeRec);
            baseContext.SetModified();

            return MCPContent.CreateSimpleContent($"✅ Family with XRef `{familyRec.XRef}` added: husband {husbandXRef}, wife {wifeXRef}");
        }
    }
}
