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


internal class FamAddCommand : BaseCommand
{
    public FamAddCommand() : base("family_add", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
