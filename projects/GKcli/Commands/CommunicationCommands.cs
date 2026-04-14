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

internal class CommunicationListCommand : BaseCommand
{
    public CommunicationListCommand() : base("communication_list", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all communications in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtCommunication);
        return MCPHelper.PageableTable("communications", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Theme | Corresponder | Type | Date |\n|---|---|---|---|---|";
            } else {
                var rec = (GDMCommunicationRecord)recList[index];
                string theme = rec.CommName;
                string corresponder = GKUtils.GetCorresponderStr(baseContext.Tree, rec, false);
                string type = LangMan.LS(GKData.CommunicationNames[(int)rec.CommunicationType]);
                string date = GKUtils.GetDateDisplayString(rec.Date);
                return $"|{rec.XRef}|{theme}|{corresponder}|{type}|{date}|";
            }
        });
    }
}


internal class CommunicationDeleteCommand : BaseCommand
{
    public CommunicationDeleteCommand() : base("communication_delete", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a communication record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the communication (e.g., 'COMM1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var commRec = baseContext.Tree.FindXRef<GDMCommunicationRecord>(xref);
        if (commRec == null)
            return MCPContent.CreateSimpleContent($"Communication not found with XRef: {xref}");

        baseContext.DeleteRecord(commRec);

        return MCPContent.CreateSimpleContent($"Communication deleted: {xref}");
    }
}
